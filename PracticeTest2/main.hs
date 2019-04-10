------------------------------------------------------------------- Question 1
-- avrage (Cons (Cons EmptyList 1 2) 34 5)
data ListOfInts = EmptyList | Single Int | Cons ListOfInts Int Int

avrage :: ListOfInts -> Int
avrage(EmptyList) = 0.0
avrage(Single number) = number * 1.0
avrage(Cons list head tail) =  (getSum(list) + head + tail)* 1.0 / (getLength(list) + 2)* 1.0

getLength :: ListOfInts -> Int
getLength(EmptyList) = 0
getLength(Single number) = 1
getLength(Cons list head tail) = getLength(list) + 2

getSum :: ListOfInts -> Int
getSum(EmptyList) = 0
getSum(Single number) = number
getSum(Cons list head tail) = getSum(list) + head + tail
-------------------------------------------------------------------

------------------------------------------------------------------- Question 2
directors_db :: [ ( [Char] , [Char] ) ]
directors_db = [ ("Carpenter", "Assault on Precinct 13"), ("Carpenter", "The Thing"), ("Carpenter", "Big Trouble in Little China"), ("Tarantino", "Inglourious Basterds"), ("Tarantino", "Django Unchained"), ("Tarantino", "The Hateful Eight") ]

year_db :: [ ( [Char] , Int ) ]
year_db = [ ("Escape from New York", 1981), ("The Thing", 1982), ("Big Trouble in Little China", 1986), ("Pulp Fiction", 1994), ("Django Unchained", 2012), ("The Hateful Eight", 2015) ]

---------------------------------- A " getMoviesFromDirector directors_db "Carpenter" "
getMoviesFromDirector :: [([Char], [Char])] -> [Char] -> [[Char]]
getMoviesFromDirector list [] = []
getMoviesFromDirector [] search = []
getMoviesFromDirector ((director,movie):tail) (search) = if director == search 
    then movie: getMoviesFromDirector tail search
    else getMoviesFromDirector tail search
----------------------------------

---------------------------------- B " getMoviesFromDecade year_db 10 "
getMoviesFromDecade :: [([Char], Int)] -> Int-> [[Char]]
getMoviesFromDecade [] search = []
getMoviesFromDecade ((movie,decade):tail) (search)
    | search == 90 && decade < 2000 && decade > 1989 = movie: getMoviesFromDecade tail search
    | search == 80 && decade < 1990 && decade > 1979 = movie: getMoviesFromDecade tail search
    | search == 0 && decade < 2010 && decade > 1999 = movie: getMoviesFromDecade tail search
    | search == 10 && decade < 2020 && decade > 2009 = movie: getMoviesFromDecade tail search
    | otherwise = getMoviesFromDecade tail search
----------------------------------

-- ---------------------------------- C " getMoviesFromDecade year_db 10 "
-- getDirectorFromYear :: Int -> [([Char], Int)] -> [([Char], [Char])]-> Maybe [Char]
-- getDirectorFromYear [] list search = []
-- getDirectorFromYear list [] search = []
-- getDirectorFromYear [] [] search = []
-- ----------------------------------


-- ------------------------------------------------------------------- 
-- helper :: [a] -> [a]
-- helper x = foo x []

-- foo :: [a] -> [a] -> [a]
-- foo [] x = x
-- foo (h:t) x = foo t (bar h: x)


--foldr takes a list and will do operations on them and save the value to a variable
--filter takes boolean statment and list and filters the list for true values
sumEven :: [Int] -> Int
sumEven h = foldr (+) 0 (filter even h)

