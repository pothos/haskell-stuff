-- magic number: P6, binary
-- P6
-- #comment: 3col 2row 255maxcolor
-- 3 2
-- 255
-- binary from here

import Data.Word
type RGB = (Word8, Word8, Word8)
type Row = [RGB]
type Width = Int
type Height = Int
type XPos = Int
type YPos = Int
type Posi = (XPos, YPos)
type Region = (Posi, Width, Height, [Row])

-- BINARY_OFFSET = xyz

farbigesTestBild :: Width -> Height -> RGB -> Region
farbigesTestBild w h c = ((0,0), w, h, replicate h (replicate w c))


-- im Moment noch einfache funktion, soll später gelöscht werden
--writePicToFile_deprec :: Region -> String -> Bool


-- schreibt Header mit genügend Pufferkommentaren für spätere Größenänderung
-- Kommentare nur direkt vor dem Bytestream
-- Seek nach BINARY_OFFSET
-- openFile :: Width -> Height -> String -> filehandle
-- schließe Datei und gibt Erfolgsstatus zurück
-- closeFile :: filehandle -> Bool

-- ändere nachträglich die Bildgröße für ein filehandle
-- changeSizeForFile :: Width -> Height -> filehandle -> Bool

-- macht Seek zur richtigen Position und schreibt Binärdaten
-- writeRegionToFile :: Region -> filehandle -> Bool
