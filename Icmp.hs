module Icmp (createIcmpSender, runIcmpThread, IcmpSender, echoRequest, EchoReply, showReply) where
import Control.Concurrent.Chan
import Network.Socket
import Data.Word
import Data.Bits
import Data.ByteString.Lazy.Char8 (unpack, pack)
import qualified Data.ByteString.Lazy as BL
import Data.Binary.Put
import Data.Binary.Get

data IcmpSender = IcmpSender {
	chan :: Chan EchoReply,
	sock :: Socket
}

data EchoRequest = EchoRequest {
	dstaddr :: SockAddr
}

data EchoReply = EchoReply {
	reply :: String
}

showReply :: EchoReply -> String
showReply (EchoReply a) = a

createIcmpSender :: Chan EchoReply -> IO IcmpSender
createIcmpSender c = do
	sock <- socket AF_INET Raw 1
	return (IcmpSender c sock)

runIcmpThread :: IcmpSender -> IO ()
runIcmpThread (IcmpSender chan sock) = do
	sock <- socket AF_INET Raw 1
	listenSock chan sock

listenSock :: Chan EchoReply -> Socket -> IO ()
listenSock chan sock = do
	(str,len,addr) <- recvFrom sock 2048
	let inpkt = parseIP str
	let inicmp = decodeICMP inpkt
	writeChan chan $ EchoReply $ icmpStr inicmp
	listenSock chan sock

data IcmpPacket = IcmpPacket { icmpType :: Word8, code :: Word8, 
	chksum :: Word16, chksumGood :: Bool, 
	id :: Word16, seqNo :: Word16, payload :: BL.ByteString }
	deriving Eq

data IpPacket = IpPacket { version :: Int, proto :: ProtocolNumber, 
	source :: HostAddress, dest :: HostAddress, upperData :: BL.ByteString }
	deriving (Eq, Show)

echoRequest :: IcmpSender -> SockAddr -> Word16 -> Word16 -> BL.ByteString -> IO Int
echoRequest (IcmpSender _ sock) addr id seq payload = do 
	let p = IcmpPacket 8 0 0 True id seq payload
	sendTo sock (encodeICMP p) addr

icmpTypeName :: Word8 -> String
icmpTypeName 8 = "Ping"
icmpTypeName 0 = "Pong"

icmpStr :: IcmpPacket -> String
icmpStr (IcmpPacket t c cs cg i s p) = icmpTypeName t ++ " crc " ++
	show cs ++ " ok? " ++ show cg ++ " id " ++ show i ++ 
	" seq " ++ show s ++ " data " ++ show p

encodeICMP :: IcmpPacket -> String
encodeICMP pkt = unpack $ addIcmpChecksum $ runPut $ encode pkt
	where
	encode :: IcmpPacket -> Put
	encode (IcmpPacket t c cs cg i s p) = do
		putWord8 t
		putWord8 c
		putWord16be cs
		putWord16be i
		putWord16be s
		putLazyByteString p

icmpChecksumOk :: BL.ByteString -> Bool
icmpChecksumOk i = calcSum i == 0

decodeICMP :: IpPacket -> IcmpPacket
decodeICMP (IpPacket 4 ipproto_icmp _ _ d) = IcmpPacket t c cs cg i s p
	where 
	(t,c,cs,i,s,p) = runGet parseICMP d
	cg = icmpChecksumOk d
	parseICMP :: Get (Word8, Word8, Word16, Word16, Word16, BL.ByteString)
	parseICMP = do
		typ <- getWord8
		code <- getWord8
		csum <- getWord16be
		id <- getWord16be
		seq <- getWord16be
		buf <- getRemainingLazyByteString
		return (typ, code, csum, id, seq, buf)

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
