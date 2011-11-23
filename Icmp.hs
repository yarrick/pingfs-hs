module Icmp (openIcmpSocket, IcmpPacket, readIcmp, echoRequest) where
import Control.Concurrent.Chan
import Network.Socket
import Data.Word
import Data.Bits
import Data.ByteString.Lazy.Char8 (unpack, pack)
import qualified Data.ByteString.Lazy as BL
import Data.Binary.Put
import Data.Binary.Get

data IcmpPacket = IcmpPacket { 
	icmpPeer :: SockAddr,
	icmpType :: Word8, 
	icmpCode :: Word8, 
	icmpId :: Word16, 
	icmpSeqNo :: Word16, 
	icmpPayload :: BL.ByteString }
	deriving Eq

icmpTypeName :: IcmpPacket -> String
icmpTypeName a 
	| icmpType a == 8 = "Ping to"
	| icmpType a == 0 = "Pong from"
	| otherwise = "Junk (" ++ show (icmpType a) ++ ") from"

instance Show IcmpPacket where
	show i = icmpTypeName i ++ 
		" peer " ++ show (icmpPeer i) ++
		" id " ++ show (icmpId i) ++
		" seq " ++ show (icmpSeqNo i) ++ 
		" data " ++ show (icmpPayload i)

openIcmpSocket :: IO Socket
openIcmpSocket = socket AF_INET Raw ipproto_icmp

readIcmp :: Socket -> IO (Maybe IcmpPacket)
readIcmp sock = do
	(str,len,addr) <- recvFrom sock 2048
	let ippkt = parseIP str
	let icmppkt = decodeICMP ippkt addr
	return icmppkt

echoRequest :: Socket -> SockAddr -> Word16 -> Word16 -> BL.ByteString -> IO Int
echoRequest sock addr id seq payload = do 
	let p = IcmpPacket addr 8 0 id seq payload
	sendTo sock (encodeICMP p) addr

data IpPacket = IpPacket { version :: Int, proto :: ProtocolNumber, 
	source :: HostAddress, dest :: HostAddress, upperData :: BL.ByteString }
	deriving (Eq, Show)

encodeICMP :: IcmpPacket -> String
encodeICMP pkt = unpack $ addIcmpChecksum $ runPut $ encode pkt
	where
	encode :: IcmpPacket -> Put
	encode (IcmpPacket a t c i s p) = do
		putWord8 t
		putWord8 c
		putWord16be 0 -- checksum will be added later
		putWord16be i
		putWord16be s
		putLazyByteString p

icmpChecksumOk :: BL.ByteString -> Bool
icmpChecksumOk i = calcSum i == 0

decodeICMP :: IpPacket -> SockAddr -> Maybe IcmpPacket
decodeICMP ip addr 
	| ipv4icmp ip = case icmpChecksumOk (upperData ip) of
		True -> Just $ IcmpPacket addr t c i s p
		False -> Nothing
	| otherwise = Nothing
	where 
	ipv4icmp pkt = (version pkt == 4) && (proto pkt == ipproto_icmp)
	(t,c,i,s,p) = runGet parseICMP (upperData ip)
	parseICMP :: Get (Word8, Word8, Word16, Word16, BL.ByteString)
	parseICMP = do
		typ <- getWord8
		code <- getWord8
		skip 2 -- checksum is verified above
		id <- getWord16be
		seq <- getWord16be
		buf <- getRemainingLazyByteString
		return (typ, code, id, seq, buf)

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
