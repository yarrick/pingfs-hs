import Control.Concurrent
import Control.Concurrent.Chan
import Control.Exception
import Data.ByteString.Lazy.Char8 (unpack, pack)
import qualified Data.ByteString.Lazy as BL
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
	"Hostfile is text file with one host per line (a.example.com or 10.2.2.1 and so on)", "",
	"pingfs - written by Erik Ekman in 2011"
	] >> return ()

type PingHandle = ()
data File = File FileOffset FileMode -- len mode
type FileMap = Map.Map String File
data FsState = FsState (IORef FileMap) (Chan PingEvent)

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
pingGetFileStat (FsState i c) ('/':name)= do
	ctx <- getFuseContext
	m <- readIORef i
	case Map.lookup name m of
		Just (File len mode) -> return $ Right $ fileStat ctx RegularFile 1 len mode
		Nothing -> return $ Left eNOENT

-- settime, setowner
pingNoOp _ _ _ _ = return eOK

-- open or close dir
pingDoDir :: FsState -> FilePath -> IO Errno
pingDoDir _ "/" = return eOK
pingDoDir _ _ = return eNOENT

pingReadDir :: FsState -> FilePath -> IO (Either Errno [(FilePath, FileStat)])
pingReadDir (FsState i c) "/" = do
	ctx <- getFuseContext
	m <- readIORef i
	return $ Right $ map (dir ctx) [".",".."] ++ map (listFile ctx) (Map.toList m) 
	where 	listFile ctx (a,File len mode) = (a, fileStat ctx RegularFile 1 len mode)
		dir ctx x = (x, dirStat ctx)
pingReadDir _ _ = return $ Left eNOENT

pingCreateFile :: FsState -> FilePath -> EntryType -> FileMode -> DeviceID -> IO Errno
pingCreateFile (FsState m c) ('/':name) RegularFile mode _ = do
	mm <- readIORef m
	writeIORef m $ Map.insert name (File 0 mode) mm
	return eOK

pingOps :: FsState -> FuseOperations PingHandle
pingOps p = defaultFuseOps {
	fuseGetFileStat = pingGetFileStat p,
	fuseCreateDevice = pingCreateFile p,
	fuseSetOwnerAndGroup = pingNoOp p,
	fuseSetFileTimes = pingNoOp p,
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
	let fstate = FsState iomap pingChan
	putStrLn $ "Mounting pingfs at " ++ mpoint ++ ", using " ++ show (length hosts) ++ " hosts"
	putStrLn "Please be kind to your network."
	withArgs (mpoint:["-f"]) $ fuseMain (pingOps fstate) defaultExceptionHandler

