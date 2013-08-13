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
    hFlush h

    -- Get security types
    securityTypes <- readSecurityTypes h
    print $ securityTypes

    -- Use security type 1 (None)
    hPutChar h (chr 1)
    hFlush h

    -- Send init message
    hPutChar h (chr 1)
    hFlush h

    -- Read init message
    initMessage <- readInitMessage h
    print $ initMessage

    -- Send framebuffer message
    sendFramebufferUpdateRequest h

    -- Read framebuffer message
    bufferMessage <- readFramebufferUpdate h
    print $ bufferMessage

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

readFramebufferUpdate :: Handle -> IO String
readFramebufferUpdate h = do
    messageType <- readU8 h
    case messageType of
        0 -> do
                readU8 h
                response <- readNumberOfRectangles h
                return $ "Number of rectangles: " ++ show response
        _ -> return ":-("

readNumberOfRectangles :: Handle -> IO Int
readNumberOfRectangles h = do
    readU16 h

sendFramebufferUpdateRequest :: Handle -> IO ()
sendFramebufferUpdateRequest h = do
    hPutChar h (chr 3)
    hPutChar h (chr 0)
    hPutChar h (chr 0) >> hPutChar h (chr 0)
    hPutChar h (chr 0) >> hPutChar h (chr 0)
    hPutChar h (chr 0) >> hPutChar h (chr 10)
    hPutChar h (chr 0) >> hPutChar h (chr 10)
    hFlush h

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
