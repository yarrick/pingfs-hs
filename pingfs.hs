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

data PingEvent = IcmpData IcmpPacket | AddBlock { } | RemoveBlock { } | DataWanted { }
	deriving (Eq, Show)

data PingSession = PingSession {
	pingSeqno :: Word16,
	pingPeer :: SockAddr,
	pingTime :: Int
}

-- type alias for map with id->session info
type PingMap = Map.Map Word16 PingSession

processEvent :: PingEvent -> PingMap -> PingMap
processEvent p m = m

pinger :: Chan PingEvent -> IO ()
pinger c = doPing c Map.empty

doPing :: Chan PingEvent -> PingMap -> IO ()
doPing chan map = do
	ev <- readChan chan
	putStrLn $ show ev
	doPing chan $ processEvent ev map

-- icmp receiver thread

-- State for sender
data IcmpSender = IcmpSender {
	chan :: Chan PingEvent, -- channel to send replies
	sock :: Socket -- ICMP raw socket
}

createIcmpSender :: Chan PingEvent -> IO IcmpSender
createIcmpSender c = withSocketsDo $ do
	sock <- openIcmpSocket
	return (IcmpSender c sock)

-- Forward EchoReply if parsing went fine
sendPacket :: Maybe IcmpPacket -> Chan PingEvent -> IO ()
sendPacket Nothing _ = return ()
sendPacket (Just i) c = writeChan c $ IcmpData i

runIcmpThread :: IcmpSender -> IO ()
runIcmpThread (IcmpSender chan sock) = do
	icmpPkt <- readIcmp sock
	sendPacket icmpPkt chan
	runIcmpThread (IcmpSender chan sock)

reader :: IcmpSender -> [SockAddr] -> IO ()
reader is (s:ss) = do
	line <- getLine
	echoRequest (sock is) s 15 22 $ pack line
	reader is ss

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
	icmpSender <- createIcmpSender pingChan
	forkIO $ runIcmpThread icmpSender
	forkIO $ pinger pingChan
	putStrLn "Input data. Press newline and it will be sent as ping to one of the given hosts"
	reader icmpSender $ cycle hosts

