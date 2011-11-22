import System.IO
import Data.Word
import qualified Data.ByteString as B
import System.Process

type RGB = (Word8, Word8, Word8)
type Row = [RGB]
type Width = Int
type Height = Int
type XPos = Int
type YPos = Int
type Posi = (XPos, YPos)
type Screen = (Width, Height)
type Region = (Posi, Screen, Width, Height, [Row])

-- beim 101. Byte geht der Binärstream los
bINARYoFFSET :: Int
bINARYoFFSET = 100

toPixel :: [Word8] -> RGB
toPixel [r, g, b] = (r, g, b)
fromPixel :: RGB -> [Word8]
fromPixel (r, g, b) = [r, g, b]

getWidth :: Region -> Int
getWidth (_, _, w, _, _) = w
getHeight :: Region -> Int
getHeight (_, _, _, h, _) = h

getScreenWidth :: Region -> Int
getScreenWidth (_, (w, _), _, _, _) = w
getScreenHeight :: Region -> Int
getScreenHeight (_, (_, h), _, _, _) = h
getScreen :: Region -> Screen
getScreen r = (getScreenWidth r, getScreenHeight r)

getPosX :: Region -> Int
getPosX ((x, _), _, _, _, _) = x
getPosY :: Region -> Int
getPosY ((_, y), _, _, _, _) = y


regionToByteString :: Region -> B.ByteString
regionToByteString (_, _, _, _, rows) = B.pack(concat(map fromPixel (concat (rows)))) -- concatMap?

regionToArray :: Region -> [Word8]
regionToArray (_, _, _, _, rows) = concat(map fromPixel (concat (rows)))


-- schreibt Header mit genügend Pufferkommentaren für spätere Größenänderung, festes Byteoffset für einfügen von Regionen
-- Kommentare nur direkt vor dem Bytestream
-- durch Benutzung von WriteMode wird alter Inhalt gelöscht
writePNMHeader :: String -> Screen -> IO ()
writePNMHeader fn (width, height) = do
                                    h <- openFile fn WriteMode
                                    let header = longheader("P6\n" ++ (show width) ++ " " ++ (show height) ++ "\n" ++ "# puffer comment\n") ++ "\n255\n"
                                    hPutStr h header
                                    hClose h
                                    where
                                    longheader s = if (length s == bINARYoFFSET-5) then s else longheader (s++"#")


-- macht Seek zur richtigen Position und schreibt Binärdaten
writeRegionToFile :: String -> Region -> IO ()
writeRegionToFile fn r = do
                         h <- openFile fn ReadWriteMode
                         let offset = toInteger (bINARYoFFSET + (getPosY r)*(getScreenWidth r)*3 + (getPosX r)*3)
                         hSeek h AbsoluteSeek offset
                         foreach h r (regionToByteLines r)
                         hClose h
                         where
                         foreach h r (x:xs) = do
                                        B.hPut h x
                                        hSeek h RelativeSeek (toInteger (distance r))
                                        if xs /= [] then foreach h r xs else hSeek h RelativeSeek 0
                         distance r = ((getScreenWidth r)-(getWidth r))*3

regionToByteLines :: Region -> [B.ByteString]
regionToByteLines (_, _, _, _, rows) = map (\ r -> B.pack(concat(map fromPixel r))) rows
-- als kompletter Stream B.pack(concat(map fromPixel (concat (rows))))

-- kopiert als PNG
saveAsPNG :: String -> IO ProcessHandle
saveAsPNG s = do
              runCommand ("pnmtopng \"" ++ s ++ "\" > \"`dirname \"" ++ s ++ "\"`/`basename \"" ++ s ++ "\" .pnm`.png\"")
              --runCommand ("./pngconvert.sh "++s)


farbigesTestBild :: Width -> Height -> RGB -> Region
farbigesTestBild w h c = ((0,0), (w,h), w, h, replicate h (replicate w c))
farbigeTestRegion :: Screen -> Posi -> Width -> Height -> RGB -> Region
farbigeTestRegion s p w h c = (p, s, w, h, replicate h (replicate w c))

testsequenz = do
 -- Erstellen von vier.png:
 writePNMHeader "vier.pnm" (100,100)
 
 let b1 = farbigeTestRegion (100,100) (0,0) 50 50 (0,40,40)
 writeRegionToFile "vier.pnm" b1
 
 let b2 = farbigeTestRegion (100,100) (50,0) 50 50 (255,255,40)
 writeRegionToFile "vier.pnm" b2
 
 let b3 = farbigeTestRegion (100,100) (0,50) 50 50 (0,255,40)
 writeRegionToFile "vier.pnm" b3
 
 let b4 = farbigeTestRegion (100,100) (50,50) 50 50 (0,255,240)
 writeRegionToFile "vier.pnm" b4
 
 saveAsPNG "vier.pnm"
 -- auch möglich:
 let b = farbigesTestBild 100 100 (55,55,55)
 writePNMHeader "graudannbunt.pnm" (getScreen b)
 let brechtsunten50quadratabmitte = farbigeTestRegion (getScreen b) (floor((fromIntegral(getWidth b))/2), floor((fromIntegral(getHeight b))/2)) 50 50 (0,255,0)
 writeRegionToFile "graudannbunt.pnm" brechtsunten50quadratabmitte
 saveAsPNG "graudannbunt.pnm"
