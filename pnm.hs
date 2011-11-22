-- @TODO: Wie kann ein IO Handle weitergereich werden?

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

farbigesTestBild :: Width -> Height -> RGB -> Region
farbigesTestBild w h c = ((0,0), (w,h), w, h, replicate h (replicate w c))

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

-- im Moment noch einfache funktion, soll später gelöscht werden
--writePicToFile_deprec :: Region -> String -> IO ()
--writePicToFile_deprec r f = do
--                            h <- openFile f ReadWriteMode
--                            hPutStrLn h "P6"
--                            hPutStrLn h ((show (getWidth r)) ++ " " ++ (show (getHeight r)))
--                            hPutStrLn h "# comment"
--                            hPutStrLn h "###########################"
--                            hPutStrLn h "255"
--                            putStrLn (show (regionToArray r))
--                            B.hPut h (regionToByteString r)
--                            hClose h


regionToByteString :: Region -> B.ByteString
regionToByteString (_, _, _, _, rows) = B.pack(concat(map fromPixel (concat (rows)))) -- concatMap?

regionToArray :: Region -> [Word8]
regionToArray (_, _, _, _, rows) = concat(map fromPixel (concat (rows)))


-- öffne Datei und gib Handle zurück
--openPNM :: String -> IO Handle
--openPNM f = do
--             h <- openFile f ReadWriteMode
--             return h

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

-- schließe Datei und gibt Erfolgsstatus zurück
--closeFile :: Handle -> IO ()
--closeFile h = do
--              hClose h

-- macht Seek zur richtigen Position und schreibt Binärdaten
writeRegionToFile :: String -> Region -> IO ()
writeRegionToFile fn r = do
                         h <- openFile fn ReadWriteMode
                         let offset = toInteger (bINARYoFFSET + (getPosY r)*(getScreenWidth r)*3 + (getPosX r)*3)
                         hSeek h AbsoluteSeek offset
                         B.hPut h (regionToByteString r)
                         hClose h

-- kopiert als PNG
saveAsPNG :: String -> IO ProcessHandle
saveAsPNG s = do
              runCommand ("pnmtopng \"" ++ s ++ "\" > \"`dirname \"" ++ s ++ "\"`/`basename \"" ++ s ++ "\" .pnm`.png\"")
              --runCommand ("./pngconvert.sh "++s)
