import Network
import System.IO
import Data.Char
import Data.Bits
import Graphics.UI.SDL as SDL

main :: IO ()
main = do
    SDL.init [SDL.InitEverything]
    SDL.setVideoMode 640 480 32 []
    SDL.setCaption "Video Test!" "video test"
    surface <- SDL.getVideoSurface
    main2 surface
    SDL.flip surface
    eventLoop
    SDL.quit
    where
        eventLoop = SDL.waitEventBlocking >>= checkEvent
        checkEvent (KeyUp _) = return ()
        checkEvent _         = eventLoop

main2 surface = do
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
    rectangles <- readFramebufferUpdate h
    print $ rectangles
    drawRectangles rectangles surface

drawRectangles :: [Rectangle] -> SDL.Surface -> IO ()
drawRectangles [Rectangle x y width height pixels] surface = do
    mapM_ (putPixel surface) pixels

putPixel :: SDL.Surface -> OurPixel -> IO ()
putPixel surface (OurPixel x y r g b) = do
    let rect = Just $ SDL.Rect x y 1 1
    let color = SDL.Pixel ((shiftL (fromIntegral r) 16)
                       .|. (shiftL (fromIntegral g) 8)
                       .|. (fromIntegral b))
    SDL.fillRect surface rect color
    return ()

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
    print $ "pixel format: " ++ show pixelFormat
    nameLength <- readU32 h
    name <- readBytes nameLength h
    return (width, height, 0, map chr name)

readFramebufferUpdate :: Handle -> IO [Rectangle]
readFramebufferUpdate h = do
    messageType <- readU8 h
    case messageType of
        0 -> do
                readU8 h
                numberOfRectangles <- readNumberOfRectangles h
                rectangles <- readRectangles numberOfRectangles h
                return rectangles
        _ -> fail "Gaaaah!"

readNumberOfRectangles :: Handle -> IO Int
readNumberOfRectangles h = do
    readU16 h

readRectangles :: Int -> Handle -> IO [Rectangle]
readRectangles numberOfRectangles h =
    case numberOfRectangles of
        0 -> return []
        n -> do
                 rectangle <- readRectangle h
                 restRectangles <- readRectangles (n - 1) h
                 return (rectangle:restRectangles)

readRectangle :: Handle -> IO Rectangle
readRectangle h = do
    x <- readU16 h
    y <- readU16 h
    width <- readU16 h
    height <- readU16 h
    encodingType <- readU32 h -- Should be S32
    pixels <- readBytes (width * height * 4) h
    return (Rectangle x y width height (buildPixelList pixels x y x width))

buildPixelList :: [Int] -> Int -> Int -> Int -> Int -> [OurPixel]
buildPixelList [] _ _ _ _ = []
buildPixelList (a:r:g:b:rest) x y startX width =
    let (nextX, nextY) = if (x - startX + 1) >= width
                             then (startX, y+1)
                             else (x+1, y)
    in OurPixel x y r g b : buildPixelList rest nextX nextY startX width

data Rectangle = Rectangle
    { x      :: Int
    , y      :: Int
    , width  :: Int
    , height :: Int
    , pixels :: [OurPixel]
    }
    deriving (Show)

data OurPixel = OurPixel
    { px :: Int
    , py :: Int
    , r :: Int
    , g :: Int
    , b :: Int
    } deriving (Show)

sendFramebufferUpdateRequest :: Handle -> IO ()
sendFramebufferUpdateRequest h = do
    hPutChar h (chr 3)
    hPutChar h (chr 0)
    hPutChar h (chr 0) >> hPutChar h (chr 0)
    hPutChar h (chr 0) >> hPutChar h (chr 0)
    hPutChar h (chr 0) >> hPutChar h (chr 1000)
    hPutChar h (chr 0) >> hPutChar h (chr 700)
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
