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

import Icmp

data PingEvent = 
	IcmpData IcmpPacket | 
	AddBlock Word16 SockAddr BL.ByteString -- id, peer, data
	deriving (Eq, Show)

data PingSession = PingSession {
	pingSeqno :: Word16,
	pingPeer :: SockAddr,
	pingTime :: Int -- todo use better type
	-- todo add length?
}

-- type alias for map with id->session info
type PingMap = Map.Map Word16 PingSession 

-- Returns new map and optional ping to send
processEvent :: PingEvent -> PingMap -> (PingMap, Maybe IcmpPacket)
-- got pong, increase seqno and send ping
processEvent (IcmpData icmp) m =
	case sess of
		Nothing -> (m, Nothing) -- if id does not match, discard it
		Just s -> (map2 m s, Just $ ping icmp s) -- otherwise seq++ and resend ping
	where 
	sess = Map.lookup (icmpId icmp) m
	incSeq s = s { pingSeqno = (pingSeqno s) + 1 }
	map2 m s = Map.insert (icmpId icmp) (incSeq s) m
	ping icmp s = echoRequest (icmpPeer icmp) (icmpId icmp) (pingSeqno s) (icmpPayload icmp)
-- new block, add to map and send ping
processEvent (AddBlock id peer bytes) m = (map2, Just $ echoRequest peer id 0 bytes)
	where 
	sess = PingSession 1 peer 0
	map2 = Map.insert id sess m

pinger :: (Socket, Chan PingEvent) -> PingMap -> IO ()
pinger (s, c) map = do
	ev <- readChan c
	putStrLn $ show ev
	let (map2, icmp) = processEvent ev map
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

