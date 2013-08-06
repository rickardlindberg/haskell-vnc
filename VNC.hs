import Network
import System.IO
import Data.Char

main = do
    h <- connect "localhost" 5900

    version <- readVersion h
    print $ version

    hPutStr h "RFB 003.007\n"

    securityTypes <- readSecurityTypes h
    print $ securityTypes

connect :: String -> Int -> IO Handle
connect host port = connectTo host (PortNumber (fromIntegral port))

readVersion :: Handle -> IO String
readVersion h = do
    bytes <- readBytes 12 h
    return $ map chr bytes

readSecurityTypes :: Handle -> IO [Int]
readSecurityTypes h = do
    [numberOfBytes] <- readBytes 1 h
    securityTypes <- readBytes numberOfBytes h
    return securityTypes

readBytes :: Int -> Handle -> IO [Int]
readBytes 0         handle = return []
readBytes bytesLeft handle = do char <- hGetChar handle
                                chars <- readBytes (bytesLeft - 1) handle
                                return (ord char:chars)
