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
    

convertRowToASCII :: ([Float], Bool, [Char]) -> [Char]
convertRowToASCII ([], white, patterns) = []
convertRowToASCII (head : tail, white, patterns) = convertToASCII(head, white, patterns) : convertRowToASCII(tail, white, patterns)

convertGrayscaleToASCII :: ([[Float]], Bool, [Char]) -> [[Char]]
convertGrayscaleToASCII ([], white, patterns)= []
convertGrayscaleToASCII (head : tail, white, patterns) = convertRowToASCII(head, white, patterns) : convertGrayscaleToASCII(tail, white, patterns)

getLength :: [Char] -> Int 
getLength [] = 0
getLength (head : tail) = 1 + getLength(tail)

reverseString :: [Char] -> [Char]
reverseString [] = []
reverseString [a] = [a]
reverseString (head : tail) = reverseString(tail) ++ [head]

doesRowContainRow :: ([Char],[Char]) -> Int
doesRowContainRow ([], []) = 0
doesRowContainRow ([], pattern) = -1000
doesRowContainRow (pattern, []) = 0
doesRowContainRow (bigHead : bigTail, smallHead : smallTail) = 
    if bigHead == smallHead 
        then doesRowContainRow (bigTail, smallTail)
    else 
        1 + doesRowContainRow(bigTail, smallHead:smallTail)

indexOfSubstring :: ([Char], [Char]) -> Int 
indexOfSubstring ([],[]) = 0
indexOfSubstring ([], string) = -1
indexOfSubstring (string, []) = 0
indexOfSubstring (head: string, subHead: substring) = 
    if getLength (subHead:substring) > getLength (head:string )
        then -1
    else if head == subHead
        then indexOfSubstring(string, substring)
    else 
        if indexOfSubstring(string, subHead: substring) == -1
            then -1
        else
            1 + indexOfSubstring(string, subHead: substring)
        

doesPictureContainPicture :: ([[Char]], [[Char]], Int, Int) -> [Int]
doesPictureContainPicture ([], [], x, y) = [x, y]
doesPictureContainPicture ([], pattern, x, y) = [-1,-1]
doesPictureContainPicture (pattern, [], x, y) = [x, y]
doesPictureContainPicture (searchHead : searchTail, findHead : findTail, x ,y) = 
    if doesRowContainRow(searchHead, findHead) >= 0 
        then doesPictureContainPicture(searchTail, findTail, doesRowContainRow(searchHead, findHead), y)
    else
        doesPictureContainPicture(searchTail, findHead:findTail, x, y+1)
        
findPicture :: ([[Char]], [[Char]]) -> [Int]
findPicture (search, find) = doesPictureContainPicture(search, find, 0, 0);

--main = do
   -- showAsASCIIArt(convertGrayscaleToASCII(parseIntoGrayscaleVals(loadBitmap "sample_image_to_search.bmp"), False, " .-+*#@"))
    --Prelude.put (""++doesRowContainRow("Chello","hell"))
    --findPicture(convertGrayscaleToASCII(parseIntoGrayscaleVals(loadBitmap "sample_image_to_search.bmp"), False, ".-+*#@"),(convertGrayscaleToASCII(parseIntoGrayscaleVals(loadBitmap "sample_image_to_find.bmp"), False, ".-+*#@")))
 