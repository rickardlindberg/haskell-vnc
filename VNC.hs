import Network
import System.IO
import Data.Char
import Data.Bits

main = do
    h <- connect "localhost" 5900

    -- Read server version
    version <- readVersion h
    print $ version

    -- Use version 3.7
    hPutStr h "RFB 003.007\n"

    -- Get security types
    securityTypes <- readSecurityTypes h
    print $ securityTypes

    -- Use security type 1 (None)
    hPutChar h (chr 1)

    -- Send init message
    hPutChar h (chr 1)

    -- Read init message
    initMessage <- readInitMessage h
    print $ initMessage

connect :: String -> Int -> IO Handle
connect host port = connectTo host (PortNumber (fromIntegral port))

readVersion :: Handle -> IO String
readVersion h = do
    bytes <- readBytes 12 h
    return $ map chr bytes

readSecurityTypes :: Handle -> IO [Int]
readSecurityTypes h = do
    numberOfBytes <- readU8 h
    securityTypes <- readBytes numberOfBytes h
    return securityTypes

readInitMessage :: Handle -> IO (Int, Int, Int, String)
readInitMessage h = do
    width <- readU16 h
    height <- readU16 h
    pixelFormat <- readBytes 16 h
    nameLength <- readU32 h
    name <- readBytes nameLength h
    return (width, height, 0, map chr name)

readU8 :: Handle -> IO Int
readU8 h = do
    [byte] <- readBytes 1 h
    return byte

readU16 :: Handle -> IO Int
readU16 h = do
    [high, low] <- readBytes 2 h
    return $ (shiftL high 8) .|. low

readU32 :: Handle -> IO Int
readU32 h = do
    [high, highMiddle, lowMiddle, low] <- readBytes 4 h
    return $  (shiftL high 24)
          .|. (shiftL highMiddle 16)
          .|. (shiftL lowMiddle 8)
          .|. low

readBytes :: Int -> Handle -> IO [Int]
readBytes 0         handle = return []
readBytes bytesLeft handle = do char <- hGetChar handle
                                chars <- readBytes (bytesLeft - 1) handle
                                return (ord char:chars)
