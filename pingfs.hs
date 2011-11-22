import Control.Concurrent
import Control.Concurrent.Chan
import Control.Exception
import Data.ByteString.Lazy.Char8 (unpack, pack)
import Network.Socket
import Maybe
import System

import Icmp


printer :: Chan EchoReply -> IO ()
printer chan = do
	a <- readChan chan
	putStrLn $ show a
	printer chan

reader :: IcmpSender -> [SockAddr] -> IO ()
reader is (s:ss) = do
	line <- getLine
	echoRequest is s 15 22 $ pack line
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
	hosts <- mapM getHost (lines l)
	return hosts

checkArgs :: [String] -> IO ()
checkArgs [] = do
	putStrLn "Need one argument; a file with one hostname/IP per line."
	exitFailure
checkArgs _ = return ()

parseArgs :: IO [SockAddr]
parseArgs = do
	args <- getArgs
	checkArgs args
	hosts <- parseHosts $ head args
	return hosts

checkHostCount :: [SockAddr] -> IO ()
checkHostCount [] = do
	putStrLn "Need at least one host!"
	exitFailure
checkHostCount x = putStrLn $ "Resolved " ++ show (length x) ++ " hosts."

main :: IO ()
main = do
	hosts <- parseArgs
	checkHostCount hosts
	replyChan <- newChan
	icmpSender <- createIcmpSender replyChan
	icmpId <- forkIO $ runIcmpThread icmpSender
	printId <- forkIO $ printer replyChan
	putStrLn $ "Started icmp: " ++ show icmpId ++ " and printer: " ++ show printId
	reader icmpSender $ cycle hosts

