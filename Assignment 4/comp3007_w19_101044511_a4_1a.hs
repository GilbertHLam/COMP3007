countZerofree :: [Int] -> Int
countZerofree [] = 0
countZerofree list = countZeroFree' list 0

countZeroFree' :: [Int] -> Int -> Int
countZeroFree' [] number = number
countZeroFree' (h:t) number 
    | containsZero(getDigits(h)) = countZeroFree' t number
    | otherwise = countZeroFree' t (number+1)

containsZero :: [Int] -> Bool
containsZero [] = False
containsZero [0] = True
containsZero (h:t) = if h == 0 
    then True
    else containsZero t

getDigits :: Int -> [Int]
getDigits 0 = []
getDigits number 
    | modOperation number 10 0 == 0 = getDigits (divOperation number 10 0) ++ [0]
    | otherwise = getDigits (divOperation number 10 0) ++ [modOperation number 10 0]

modOperation :: Int -> Int -> Int -> Int
modOperation big small counter
    | small > big = big
    | big == counter + small = 0
    | big > counter + small = modOperation big small (small+counter) 
    | big < counter + small = big - counter

divOperation :: Int -> Int -> Int -> Int
divOperation big small counter
    | small == 0 = 0
    | big > small * (counter+1) = divOperation big small (counter+1)
    | otherwise = counter