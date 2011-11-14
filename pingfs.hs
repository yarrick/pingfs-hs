import Network.Socket
import Data.Word
import Data.ByteString hiding ( putStrLn )

data Echo =
	Echo { id :: Word16, seqNo :: Word16, payload :: ByteString}

data IcmpPacket =
	IcmpPacket { icmpType :: Word8, code :: Word8, chksum :: Word16, ping :: Echo }

ipproto_icmp :: ProtocolNumber
ipproto_icmp = 1

main :: IO ()
main = do
	sock <- socket AF_INET Raw ipproto_icmp
	putStrLn "foo"
	len <- sendTo sock "FOO" (SockAddrInet 0 0x0104040A)
	putStrLn $ show len
