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
	pingTime :: Int
}

-- type alias for map with id->session info
type PingMap = Map.Map Word16 PingSession 

processEvent :: IcmpState -> PingEvent -> PingMap -> IO PingMap
-- got pong, increase seqno and send ping
processEvent is (IcmpData icmp) m = return m
-- new block, add to map and send ping
processEvent is (AddBlock id peer bytes) m = do
	let sess = PingSession 1 peer 0
	let map = Map.insert id sess m
	echoRequest (sockState is) peer id 0 bytes
	return map

pinger :: IcmpState -> IO ()
pinger is = doPing is Map.empty

doPing :: IcmpState -> PingMap -> IO ()
doPing is map = do
	ev <- readChan $ chanState is
	putStrLn $ show ev
	map2 <- processEvent is ev map
	doPing is map2

-- icmp receiver thread

data IcmpState = IcmpState {
	chanState :: Chan PingEvent, -- event channel
	sockState :: Socket -- ICMP raw socket
}

createIcmpState :: Chan PingEvent -> IO IcmpState
createIcmpState c = withSocketsDo $ do
	sock <- openIcmpSocket
	return (IcmpState c sock)

-- Forward EchoReply if parsing went fine
sendPacket :: Maybe IcmpPacket -> Chan PingEvent -> IO ()
sendPacket Nothing _ = return ()
sendPacket (Just i) c = writeChan c $ IcmpData i

runIcmpThread :: IcmpState -> IO ()
runIcmpThread state = do
	icmpPkt <- readIcmp $ sockState state
	sendPacket icmpPkt $ chanState state
	runIcmpThread state

-- read a line of text and start it as a block
reader :: IcmpState -> Word16 -> [SockAddr] -> IO ()
reader is id (s:ss) = do
	line <- getLine
	writeChan (chanState is) $ AddBlock id s $ pack line
	reader is (id + 1) ss 

getHost :: String -> IO SockAddr
getHost x = do
	let hints = defaultHints { addrFamily = AF_INET } -- ipv4 for now
	addrs <- try $ getAddrInfo (Just hints) (Just x) Nothing
	case addrs of
		Left e -> do
			putStrLn $ "Failed to lookup host " ++ x
			ioError e
		Right a -> return $ addrAddress $ head a

parseHosts :: String -> IO [SockAddr]
parseHosts file = do
	l <- readFile file
	mapM getHost (lines l)

checkArgs :: [String] -> IO ()
checkArgs [] = do
	putStrLn "Need one argument; a file with one hostname/IP per line."
	exitFailure
checkArgs _ = return ()

parseArgs :: IO [SockAddr]
parseArgs = do
	args <- getArgs
	checkArgs args
	parseHosts $ head args

checkHostCount :: [SockAddr] -> IO ()
checkHostCount [] = do
	putStrLn "Need at least one host!"
	exitFailure
checkHostCount x = putStrLn $ "Resolved " ++ show (length x) ++ " hosts."

main :: IO ()
main = do
	hosts <- parseArgs
	checkHostCount hosts
	pingChan <- newChan
	icmpState <- createIcmpState pingChan
	forkIO $ runIcmpThread icmpState
	forkIO $ pinger icmpState
	putStrLn "Input data. Press newline and it will be sent as ping to one of the given hosts"
	reader icmpState 0 $ cycle hosts

