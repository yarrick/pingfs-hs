import Control.Concurrent
import Control.Concurrent.Chan
import Control.Exception
import Data.ByteString.Lazy.Char8 (unpack, pack)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import Data.Word
import qualified Data.Map as Map
import Data.IORef
import Network.Socket
import Maybe
import System
import System.Environment
import System.Fuse
import System.Posix.Files
import System.Posix.Types
import Time

import Icmp

data PingEvent = 
	IcmpData IcmpPacket | 
	AddBlock Word16 SockAddr BL.ByteString -- id, peer, data
	deriving (Eq, Show)

data PingSession = PingSession {
	pingSeqNo :: Word16,
	pingPeer :: SockAddr,
	pingTime :: ClockTime
	-- todo add length?
}

-- type alias for map with id->session info
type PingMap = Map.Map Word16 PingSession 

type IcmpState = (Socket, Chan PingEvent)

blockSize = 128

-- handle icmp reply. update seqno and timestamp if seqno and addr matches
processIcmp:: IcmpPacket -> PingSession -> ClockTime -> PingMap -> (PingMap, Maybe IcmpPacket)
processIcmp i s now m 
	| (pingSeqNo s == icmpSeqNo i) && (pingPeer s == icmpPeer i) =
		(map, Just $ requestFromReply i (pingSeqNo sess))
	| otherwise = (m, Nothing)
	where	sess = s { pingSeqNo = (pingSeqNo s) + 1, pingTime = now }
		map = Map.insert (icmpId i) sess m

-- Returns new map and optional ping to send
processEvent :: PingEvent -> ClockTime -> PingMap -> (PingMap, Maybe IcmpPacket)
-- got pong, increase seqno and send ping
processEvent (IcmpData icmp) now m =
	case sess of
		Nothing -> (m, Nothing) -- if id does not match, discard it
		Just s -> processIcmp icmp s now m
	where sess = Map.lookup (icmpId icmp) m
-- new block, add to map and send ping
processEvent (AddBlock id peer bytes) now m = 
		(map2, Just $ echoRequest peer id (pingSeqNo sess) bytes)
	where 
	sess = PingSession 1 peer now
	map2 = Map.insert id sess m

pinger :: IcmpState -> PingMap -> IO ()
pinger is@(s,c) map = do
	ev <- readChan c
	putStrLn $ show ev
	now <- getClockTime
	let (map2, icmp) = processEvent ev now map
	sendIcmp s icmp
	pinger is map2

-- icmp receiver thread
runIcmpThread :: IcmpState -> IO ()
runIcmpThread is@(sock,chan) = do
	mapM (writeChan chan) . wrapIcmp . filter isEchoReply . maybeToList =<< readIcmp sock
	runIcmpThread is
	where wrapIcmp l = map (\i -> IcmpData i) l

parseHosts :: String -> IO [SockAddr]
parseHosts file = mapM getHost . lines =<< readFile file
	where getHost x = do
		let hints = defaultHints { addrFamily = AF_INET } -- ipv4 for now
		addrs <- try $ getAddrInfo (Just hints) (Just x) Nothing
		case addrs of
			Left e -> putStrLn ("Failed to lookup host " ++ x) >> ioError e
			Right a -> return $ addrAddress $ head a

checkNotEmpty :: [a] -> String -> IO ()
checkNotEmpty [] err = putStrLn err >> showHelp >> exitFailure
checkNotEmpty a _ = return ()

showHelp :: IO ()
showHelp = mapM putStrLn [
	"Usage: pingfs mountpoint hostfile", "",
	"Mountpoint is empty directory to contain pingfs file system",
	"Hostfile is text file with one host per line (a.example.com or 10.2.2.1 etc)", "",
	"pingfs - written by Erik Ekman in 2011"
	] >> return ()

data PingHandle = PingHandle FsState String OpenMode FileOffset -- fs name mode pos
data File = File FileOffset FileMode [Word16] -- len mode blocks
type FileMap = Map.Map String File
data FsState = FsState {
		fsmap :: IORef FileMap,
		fschan :: Chan PingEvent,
		fspeer :: [SockAddr]
}

writeBlock :: PingHandle -> B.ByteString -> FileOffset -> IORef FileMap -> FileMap -> String -> File -> IO (Either Errno ByteCount)
writeBlock (PingHandle fs _ _ n) b off m mm name (File len mode actBlocks) = do
	--let (block, blockoffset) = quotRem (B.length b) blockSize
	let payload = BL.fromChunks [B.take blockSize b]
	writeChan (fschan fs) $ AddBlock 0 (head $ fspeer fs) payload
	putStrLn $ "write to file " ++ name ++ " at " ++ show off ++ " data " ++ show b
	return $ Right . fromIntegral $ BL.length payload

fileStat ctx etype link len mode = FileStat { 
	statEntryType = etype, statFileMode = mode,
	statLinkCount = link, statFileOwner = fuseCtxUserID ctx,
	statFileGroup = fuseCtxGroupID ctx, statSpecialDeviceID = 0,
	statFileSize = len, statBlocks = 1, statAccessTime = 0,
	statModificationTime = 0, statStatusChangeTime = 0 }
dirStat ctx = fileStat ctx Directory 2 4096 accessModes

pingGetFileStat :: FsState -> FilePath -> IO (Either Errno FileStat)
pingGetFileStat _ "/" = do
	ctx <- getFuseContext
	return $ Right $ dirStat ctx
pingGetFileStat fs ('/':name)= do
	ctx <- getFuseContext
	m <- readIORef $ fsmap fs
	case Map.lookup name m of
		Just (File len mode _) -> return $ Right $ fileStat ctx RegularFile 1 len mode
		Nothing -> return $ Left eNOENT

lookupFile :: FsState -> String -> (IORef FileMap -> FileMap -> String -> File -> IO a) -> a -> IO a
lookupFile fs name fun err = do
	let m = fsmap fs
	mm <- readIORef m
	case Map.lookup name mm of
		Nothing -> return err
		Just a -> fun m mm name a

updateMapIfFound :: FsState -> String -> (FileMap -> String -> File -> FileMap) -> IO Errno
updateMapIfFound fs n fun = lookupFile fs n updateMap eNOENT
	where updateMap m mm name a = writeIORef m (fun mm name a) >> return eOK

-- settime, setowner
pingNoOp _ _ _ _ = return eOK

-- open or close dir
pingDoDir :: FsState -> FilePath -> IO Errno
pingDoDir _ "/" = return eOK
pingDoDir _ _ = return eNOENT

pingReadDir :: FsState -> FilePath -> IO (Either Errno [(FilePath, FileStat)])
pingReadDir fs "/" = do
	ctx <- getFuseContext
	m <- readIORef $ fsmap fs
	return $ Right $ map (dir ctx) [".",".."] ++ map (listFile ctx) (Map.toList m) 
	where 	listFile ctx (a,File len mode _) = (a, fileStat ctx RegularFile 1 len mode)
		dir ctx x = (x, dirStat ctx)
pingReadDir _ _ = return $ Left eNOENT

pingCreateFile :: FsState -> FilePath -> EntryType -> FileMode -> DeviceID -> IO Errno
pingCreateFile fs ('/':name) RegularFile mode _ = do
	let m = fsmap fs
	mm <- readIORef m
	writeIORef m $ Map.insert name (File 0 mode []) mm
	return eOK

pingUnlink :: FsState -> FilePath -> IO Errno
pingUnlink fs ('/':name) = updateMapIfFound fs name (\m n _ -> Map.delete n m)

pingRenameFile :: FsState -> FilePath -> FilePath -> IO Errno
pingRenameFile fs ('/':from) ('/':to) = updateMapIfFound fs from
	(\m n f -> Map.insert to f $ Map.delete n m)

-- todo handle truncation (trunc flags == True)
pingOpenFile :: FsState -> FilePath -> OpenMode -> OpenFileFlags -> IO (Either Errno PingHandle)
pingOpenFile fs ('/':n) mode flags = lookupFile fs n openFile (Left eNOENT)
	where openFile m mm name a = return $ Right $ PingHandle fs name mode $ pos a flags
		where pos (File len mode _) f 
			| append f = len
			| otherwise = 0

pingWrite :: FilePath -> PingHandle -> B.ByteString -> FileOffset -> IO (Either Errno ByteCount)
pingWrite _ (PingHandle _  _ ReadOnly _) _ _ = return $ Left ePERM -- no write if readonly
pingWrite _ ph@(PingHandle fs n _ _) b off = lookupFile fs n (writeBlock ph b off) (Left eNOENT)

pingFlush :: FilePath -> PingHandle -> IO Errno
pingFlush _ _ = return eOK

pingCloseFile :: FilePath -> PingHandle -> IO ()
pingCloseFile _ _ = return ()

pingOps :: FsState -> FuseOperations PingHandle
pingOps p = defaultFuseOps {
	fuseGetFileStat = pingGetFileStat p,
	fuseCreateDevice = pingCreateFile p,
	fuseSetOwnerAndGroup = pingNoOp p,
	fuseRemoveLink = pingUnlink p,
	fuseRename = pingRenameFile p,
	fuseSetFileTimes = pingNoOp p,
	fuseOpen = pingOpenFile p,
	fuseWrite = pingWrite,
	fuseFlush = pingFlush,
	fuseRelease = pingCloseFile,
	fuseOpenDirectory = pingDoDir p,
	fuseReadDirectory = pingReadDir p,
	fuseReleaseDirectory = pingDoDir p
	}

parseArgs :: IO ([SockAddr], String)
parseArgs = do
	args <- getArgs
	checkNotEmpty args "First argument must me mount directory"
	let (mpoint : aargs) = args
	checkNotEmpty aargs "Second argument must be a file with one hostname/IP per line."
	hosts <- parseHosts $ head aargs
	return (hosts, mpoint)

main :: IO ()
main = do
	(hosts,mpoint) <- parseArgs
	checkNotEmpty hosts "Need at least one host!" 
	pingChan <- newChan
	icmpSock <- openIcmpSocket
	let state = (icmpSock, pingChan)
	forkIO $ runIcmpThread state
	forkIO $ pinger state Map.empty
	iomap <- newIORef $ Map.empty
	let fstate = FsState iomap pingChan hosts
	putStrLn $ "Mounting pingfs at " ++ mpoint ++ ", using " ++ show (length hosts) ++ " hosts"
	putStrLn "Please be kind to your network."
	withArgs (mpoint:["-f"]) $ fuseMain (pingOps fstate) defaultExceptionHandler

