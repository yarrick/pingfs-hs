import Network.Socket
import Data.Word
import Data.ByteString.Lazy.Char8 (unpack)
import qualified Data.ByteString.Lazy as BL
import Data.Binary.Put

data IcmpPacket =
   	IcmpPacket { icmpType :: Word8, code :: Word8, chksum :: Word16, 
   	id :: Word16, seqNo :: Word16, payload :: BL.ByteString }
	deriving Eq

echoRequest :: Word16 -> Word16 -> BL.ByteString -> String
echoRequest id seq payload = unpack $ checksum $ runPut $ icmpToStr icmp
	where 	icmp = IcmpPacket 8 0 0 id seq payload
		icmpToStr :: IcmpPacket -> Put
		icmpToStr (IcmpPacket t c cs i s p) = do
			putWord8 t
			putWord8 c
			putWord16be cs
			putWord16be i
			putWord16be s
			putLazyByteString p
		checksum :: BL.ByteString -> BL.ByteString
		checksum a = a

ipproto_icmp :: ProtocolNumber
ipproto_icmp = 1

main :: IO ()
main = do
	sock <- socket AF_INET Raw ipproto_icmp
	len <- sendTo sock pkt addr
	putStrLn $ show len
		where 	pkt = echoRequest 12 19 $ BL.singleton 55
			addr = SockAddrInet 0 0x0104040A
