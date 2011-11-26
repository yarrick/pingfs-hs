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

-- Returns new map and optional ping to send encoded as string
processEvent :: IcmpState -> PingEvent -> PingMap -> (PingMap, Maybe IcmpPacket)
-- got pong, increase seqno and send ping
processEvent is (IcmpData icmp) m =
	case sess of
		Nothing -> (m, Nothing) -- if id does not match, discard it
		Just s -> (map2 m s, Just $ ping icmp s) -- otherwise increase seq and send new ping
	where 
	sess = Map.lookup (icmpId icmp) m
	incSeq s = s { pingSeqno = (pingSeqno s) + 1 }
	map2 m s = Map.insert (icmpId icmp) (incSeq s) m
	ping icmp s = echoRequest (icmpPeer icmp) (icmpId icmp) (pingSeqno s) (icmpPayload icmp)
-- new block, add to map and send ping
processEvent is (AddBlock id peer bytes) m = (map2, Just $ echoRequest peer id 0 bytes)
	where 
	sess = PingSession 1 peer 0
	map2 = Map.insert id sess m

pinger :: IcmpState -> IO ()
pinger is = doPing is Map.empty

doPing :: IcmpState -> PingMap -> IO ()
doPing is map = do
	ev <- readChan $ chanState is
	putStrLn $ show ev
	let (map2, icmp) = processEvent is ev map
	sendIcmp (sockState is) icmp
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

runIcmpThread :: IcmpState -> IO ()
runIcmpThread state = do
	icmpPkt <- readIcmp $ sockState state
	mapM (writeChan $ chanState state) $ map (\i -> IcmpData i) $ 
		filter isEchoReply $ maybeToList icmpPkt
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

