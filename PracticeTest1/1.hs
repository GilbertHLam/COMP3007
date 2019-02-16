--------------------------------------------- Question 1
foo :: (Int, Int, Int) -> [Int]
foo (0,0,0) = []
foo (start, stop, step) = if start < stop
    then start : foo(start + step, stop, step)
    else []
--------------------------------------------- 

--------------------------------------------- Question 2 A
selector :: [Char] -> Char
selector [one, two, three] = three

recursive :: [Char] -> [Char]
recursive [] = []
recursive [one] = []
recursive [one,two] = []
recursive (one: two : three: tail) = selector(one:two:[three]) : recursive(tail)
---------------------------------------------
 
--------------------------------------------- Question 2 B
bar :: [Char] -> [Char]
bar [] = []
bar [one] = []
bar [one, two] = []
bar (one : two : three : tail) = three : bar(tail)
---------------------------------------------

--------------------------------------------- Question 3
three :: [(Int, Int)] -> [(Int, Int)]
three [] = []
three orderedPairs =   [(m , n) | (m,n) <- orderedPairs, m + n == 6 && m < n]
--------------------------------------------- 

--------------------------------------------- Question 4
four :: [(Int, Int)] -> [(Int, Int)]
four [] = []
four ((one, two) : tail) = if (one < two && one + two == 6)
    then (one, two) : three(tail)
    else three(tail)
--------------------------------------------- 

--------------------------------------------- Question 5
five :: [(Int, Int, Int)] -> [[(Int, Int, Int)]]
five [] = []
five list =  take(round(sqrt(fromIntegral(length list))))list : five( drop(round(sqrt(fromIntegral(length list))))list)