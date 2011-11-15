import Network.Socket
import Data.Word
import Data.Bits
import Data.ByteString.Lazy.Char8 (unpack, pack)
import qualified Data.ByteString.Lazy as BL
import Data.Binary.Put

data IcmpPacket =
   	IcmpPacket { icmpType :: Word8, code :: Word8, chksum :: Word16, 
   	id :: Word16, seqNo :: Word16, payload :: BL.ByteString }
	deriving Eq

echoRequest :: Word16 -> Word16 -> BL.ByteString -> String
echoRequest id seq payload = unpack $ addChecksum $ runPut $ icmpToStr icmp
	where
	icmp = IcmpPacket 8 0 0 id seq payload
	icmpToStr :: IcmpPacket -> Put
	icmpToStr (IcmpPacket t c cs i s p) = do
		putWord8 t
		putWord8 c
		putWord16be cs
		putWord16be i
		putWord16be s
		putLazyByteString p

-- calculate and insert checksum
addChecksum :: BL.ByteString -> BL.ByteString
addChecksum a = BL.append start $ BL.append sum end
	where 	
	start = BL.take 2 a
	end = BL.drop 4 a
	sum = BL.pack [fromIntegral $ shiftR csum 8, fromIntegral $ csum .&. 0xFF]
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

main :: IO ()
main = do
	sock <- socket AF_INET Raw ipproto_icmp
	len <- sendTo sock pkt addr
	putStrLn $ "send " ++ show len
	(buf,len,addr) <- recvFrom sock 1024
	putStrLn $ "got " ++ show len ++ ":"
	putStrLn $ show $ pack buf
		where 
		pkt = echoRequest 12 19 $ BL.pack [0xFF, 0xFF, 0xF0, 0x0F, 0xFF, 0xFF]
		addr = SockAddrInet 0 0x0104040A


