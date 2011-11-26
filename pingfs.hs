import Control.Concurrent
import Control.Concurrent.Chan
import Control.Exception
import Data.ByteString.Lazy.Char8 (unpack, pack)
import qualified Data.ByteString.Lazy as BL
import Data.Word
import qualified Data.Map as Map
import Network.Socket
import Maybe
import System
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

pinger :: (Socket, Chan PingEvent) -> PingMap -> IO ()
pinger (s, c) map = do
	ev <- readChan c
	putStrLn $ show ev
	now <- getClockTime
	let (map2, icmp) = processEvent ev now map
	sendIcmp s icmp
	pinger (s, c) map2

-- icmp receiver thread
runIcmpThread :: (Socket, Chan PingEvent) -> IO ()
runIcmpThread (sock, chan) = do
	mapM (writeChan chan) . wrapIcmp . filter isEchoReply . maybeToList =<< readIcmp sock
	runIcmpThread (sock, chan)
	where wrapIcmp l = map (\i -> IcmpData i) l

-- read a line of text and start it as a block
reader :: (Socket, Chan PingEvent) -> Word16 -> [SockAddr] -> IO ()
reader (sock, chan) id (s:ss) = do
	writeChan chan . (\l -> AddBlock id s $ pack l) =<< getLine
	reader (sock, chan) (id + 1) ss 

parseHosts :: String -> IO [SockAddr]
parseHosts file = mapM getHost . lines =<< readFile file
	where getHost x = do
		let hints = defaultHints { addrFamily = AF_INET } -- ipv4 for now
		addrs <- try $ getAddrInfo (Just hints) (Just x) Nothing
		case addrs of
			Left e -> putStrLn ("Failed to lookup host " ++ x) >> ioError e
			Right a -> return $ addrAddress $ head a

checkNotEmpty :: [a] -> String -> ([a] -> IO ()) -> IO ()
checkNotEmpty [] err _ = putStrLn err >> exitFailure
checkNotEmpty a _ fun = fun a

parseArgs :: IO [SockAddr]
parseArgs = do
	args <- getArgs
	checkNotEmpty args "Need one argument; a file with one hostname/IP per line." (\_ -> return ())
	parseHosts $ head args

main :: IO ()
main = do
	hosts <- parseArgs
	checkNotEmpty hosts "Need at least one host!" 
		(\a -> putStrLn $ "Resolved " ++ show (length a) ++ " hosts.")
	pingChan <- newChan
	icmpSock <- openIcmpSocket
	let state = (icmpSock, pingChan)
	forkIO $ runIcmpThread state
	forkIO $ pinger state Map.empty
	putStrLn "Input data. Press newline and it will be sent as ping to one of the given hosts"
	reader state 0 $ cycle hosts

