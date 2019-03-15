--Gilbert Lam
--101044511
import Codec.BMP
import Data.ByteString
import Data.Either
import GHC.Word
import System.IO.Unsafe

loadBitmap :: FilePath -> [[(Int, Int, Int)]]
loadBitmap filename = repackAs2DList (either returnEmptyOnError processDataOnBMP (unsafePerformIO (readBMP filename)))
  
returnEmptyOnError :: Error -> ([(Int, Int, Int)], (Int, Int))
returnEmptyOnError _ = ([], (0, 0))

processDataOnBMP :: BMP -> ([(Int, Int, Int)], (Int, Int))
processDataOnBMP bmp = ((parseIntoRGBVals (convertToInts (unpack (unpackBMPToRGBA32 bmp)))), (bmpDimensions bmp))
  
convertToInts :: [Word8] -> [Int]
convertToInts [] = []
convertToInts (h:t) = (fromIntegral (toInteger h)) : (convertToInts t)

parseIntoRGBVals :: [Int] -> [(Int, Int, Int)]
parseIntoRGBVals [] = []
parseIntoRGBVals (h:i:j:_:t) = (h,i,j) : (parseIntoRGBVals t)

repackAs2DList :: ([(Int, Int, Int)], (Int, Int)) -> [[(Int, Int, Int)]]
repackAs2DList (pixels, (width, height)) = (Prelude.reverse (repackAs2DList' pixels width height))

repackAs2DList' :: [(Int, Int, Int)] -> Int -> Int -> [[(Int, Int, Int)]]
repackAs2DList' []  width  height = []
repackAs2DList' pixels width height = (Prelude.take width pixels) : (repackAs2DList' (Prelude.drop width pixels) width height)

showAsASCIIArt :: [[Char]] -> IO ()
showAsASCIIArt pixels = Prelude.putStr (unlines pixels)

--Take in RGB value and convert it to a float number that represents the grayscale value (256 = Black, 0 = White)
convertRGBToGrayscale :: (Int, Int, Int) -> Float
convertRGBToGrayscale (r,g,b) = 0.2126 * fromIntegral r + 0.7152 * fromIntegral g + 0.0722 * fromIntegral b

parseRowIntoGrayscale :: [(Int, Int, Int)] -> [Float]
parseRowIntoGrayscale [] = []
parseRowIntoGrayscale (head : tail) = convertRGBToGrayscale(head) : parseRowIntoGrayscale(tail)

parseIntoGrayscaleVals :: [[(Int,Int,Int)]] -> [[Float]]
parseIntoGrayscaleVals [] = []
parseIntoGrayscaleVals (head : tail) = parseRowIntoGrayscale(head) : parseIntoGrayscaleVals(tail)

convertToASCII :: (Float, Bool, [Char]) -> Char
convertToASCII (grayscale, white, pattern) = if white 
    then pattern !! (getLength(pattern)-1-(round(grayscale / 256 * fromIntegral(getLength pattern-1))))
    else pattern !! (round(grayscale / 256 * fromIntegral(getLength pattern - 1)))
    
getLength :: [Char] -> Int 
getLength [] = 0
getLength (head : tail) = 1 + getLength(tail)

convertRowToASCII :: ([Float], Bool, [Char]) -> [Char]
convertRowToASCII ([], white, patterns) = []
convertRowToASCII (head : tail, white, patterns) = convertToASCII(head, white, patterns) : convertRowToASCII(tail, white, patterns)

convertGrayscaleToASCII :: ([[Float]], Bool, [Char]) -> [[Char]]
convertGrayscaleToASCII ([], white, patterns)= []
convertGrayscaleToASCII (head : tail, white, patterns) = convertRowToASCII(head, white, patterns) : convertGrayscaleToASCII(tail, white, patterns)

convertPictureToASCII :: ([Char], Bool, [[(Int, Int, Int)]]) -> [[Char]]
convertPictureToASCII (patterns, white, []) = []
convertPictureToASCII (patterns, white, bitmap) = convertGrayscaleToASCII(parseIntoGrayscaleVals(bitmap), white, patterns)