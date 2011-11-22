module Icmp (createIcmpSender, IcmpSender, runIcmpThread, echoRequest, EchoReply) where
import Control.Concurrent.Chan
import Network.Socket
import Data.Word
import Data.Bits
import Data.ByteString.Lazy.Char8 (unpack, pack)
import qualified Data.ByteString.Lazy as BL
import Data.Binary.Put
import Data.Binary.Get

-- State for sender
data IcmpSender = IcmpSender {
	chan :: Chan EchoReply, -- channel to send replies
	sock :: Socket -- ICMP raw socket
}

data IcmpPacket = IcmpPacket { 
	icmpPeer :: SockAddr,
	icmpType :: Word8, 
	icmpCode :: Word8, 
	icmpCrc :: Word16,
	icmpId :: Word16, 
	icmpSeqNo :: Word16, 
	icmpPayload :: BL.ByteString }
	deriving Eq

data EchoReply = EchoReply IcmpPacket

icmpTypeName :: IcmpPacket -> String
icmpTypeName a 
	| icmpType a == 8 = "Ping to"
	| icmpType a == 0 = "Pong from"
	| otherwise = "Junk (" ++ show (icmpType a) ++ ") from"

instance Show EchoReply where
	show (EchoReply i) = icmpTypeName i ++ 
		" peer " ++ show (icmpPeer i) ++
		" id " ++ show (icmpId i) ++
		" seq " ++ show (icmpSeqNo i) ++ 
		" data " ++ show (icmpPayload i)

createIcmpSender :: Chan EchoReply -> IO IcmpSender
createIcmpSender c = withSocketsDo $ do
	sock <- socket AF_INET Raw 1
	return (IcmpSender c sock)

-- Check if parsing went ok, and then verify ICMP checksum
sendPacket :: Maybe IcmpPacket -> Chan EchoReply -> BL.ByteString -> IO ()
sendPacket Nothing _  _= return ()
sendPacket (Just i) c p
	| icmpChecksumOk p = do writeChan c $ EchoReply i
	| otherwise = return ()

runIcmpThread :: IcmpSender -> IO ()
runIcmpThread (IcmpSender chan sock) = do
	listenSock chan sock
	where
	listenSock :: Chan EchoReply -> Socket -> IO ()
	listenSock chan sock = do
		(str,len,addr) <- recvFrom sock 2048
		let inpkt = parseIP str
		let inicmp = decodeICMP inpkt addr
		sendPacket inicmp chan (upperData inpkt)
		listenSock chan sock


data IpPacket = IpPacket { version :: Int, proto :: ProtocolNumber, 
	source :: HostAddress, dest :: HostAddress, upperData :: BL.ByteString }
	deriving (Eq, Show)

echoRequest :: IcmpSender -> SockAddr -> Word16 -> Word16 -> BL.ByteString -> IO Int
echoRequest (IcmpSender _ sock) addr id seq payload = do 
	let p = IcmpPacket addr 8 0 0 id seq payload
	sendTo sock (encodeICMP p) addr

encodeICMP :: IcmpPacket -> String
encodeICMP pkt = unpack $ addIcmpChecksum $ runPut $ encode pkt
	where
	encode :: IcmpPacket -> Put
	encode (IcmpPacket a t c cs i s p) = do
		putWord8 t
		putWord8 c
		putWord16be cs
		putWord16be i
		putWord16be s
		putLazyByteString p

icmpChecksumOk :: BL.ByteString -> Bool
icmpChecksumOk i = calcSum i == 0

decodeICMP :: IpPacket -> SockAddr -> Maybe IcmpPacket
decodeICMP (IpPacket 4 ipproto_icmp _ _ d) addr = Just $ IcmpPacket addr t c cs i s p
	where 
	(t,c,cs,i,s,p) = runGet parseICMP d
	parseICMP :: Get (Word8, Word8, Word16, Word16, Word16, BL.ByteString)
	parseICMP = do
		typ <- getWord8
		code <- getWord8
		csum <- getWord16be
		id <- getWord16be
		seq <- getWord16be
		buf <- getRemainingLazyByteString
		return (typ, code, csum, id, seq, buf)
decodeICMP ip addr = Nothing

nibbles :: Word8 -> (Int, Int)
nibbles a = (shiftR i 4, i .&. 0x0F)
	where i = fromIntegral a

parseIP :: String -> IpPacket
parseIP b = parseIPV4 $ pack b

parseIPV4 :: BL.ByteString -> IpPacket
parseIPV4 pkt = IpPacket ver p s d upper
	where 
	(ver, len) = nibbles $ BL.head pkt
	hdrlen = fromIntegral $ 4 * len
	upper = BL.drop hdrlen pkt
	(p,s,d) = runGet parseHdr $ BL.take hdrlen pkt
	parseHdr :: Get (ProtocolNumber, HostAddress, HostAddress)
	parseHdr = do
		skip 9
		proto <- getWord8
		skip 2
		src <- getWord32be
		dst <- getWord32be
		return (fromIntegral proto, src, dst)

-- calculate and insert checksum
addIcmpChecksum :: BL.ByteString -> BL.ByteString
addIcmpChecksum a = BL.append start $ BL.append sum end
	where 	
	start = BL.take 2 a
	end = BL.drop 4 a
	sum = BL.pack $ map fromIntegral [shiftR csum 8, csum .&. 0xFF]
	csum = calcSum a

calcSum :: BL.ByteString -> Int
calcSum a = 0xFFFF - (s .&. 0xFFFF + shiftR s 16)
	where 
	s = sum $ merge16 $ BL.unpack a
	merge16 :: [Word8] -> [Int]
	merge16 [] = []
	merge16 (a:[]) = merge16 $ a:[0] -- pad with zero
	merge16 (x:y:xs) = (a .|. b) : merge16 xs
		where 	a = shiftL (fromIntegral x) 8
			b = fromIntegral y

ipproto_icmp :: ProtocolNumber
ipproto_icmp = 1
