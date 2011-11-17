import Control.Concurrent
import Control.Concurrent.Chan
import Data.ByteString.Lazy.Char8 (unpack, pack)
import Network.Socket
import Icmp


printer :: Chan EchoReply -> IO ()
printer chan = do
	a <- readChan chan
	putStrLn $ showReply a
	printer chan

reader :: IcmpSender -> IO ()
reader is = do
	line <- getLine
	echoRequest is addr 15 22 $ pack line
	reader is
		where
		addr = SockAddrInet 0 0x0104040A -- reversed because of big endian

main :: IO ()
main = do
	replyChan <- newChan
	icmpSender <- createIcmpSender replyChan
	icmpId <- forkIO $ runIcmpThread icmpSender
	printId <- forkIO $ printer replyChan
	putStrLn $ "Started icmp: " ++ show icmpId ++ " and printer: " ++ show printId
	reader icmpSender
