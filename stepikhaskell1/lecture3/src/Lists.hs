module Lists where

{-
empty []

add to head 3 : []

let lst = 5 : 3 : []

or

[5,3]

[5,3] == lst

let cons42 = (42 : )
cons42 [1,23]




Реализуйте функцию addTwoElements, которая бы добавляла два переданных ей значения в голову переданного списка.

GHCi> addTwoElements 2 12 [85,0,6]
[2,12,85,0,6]


-}


addTwoElements :: a -> a -> [a] -> [a]
addTwoElements a b lst = a : b : lst


{-

Реализуйте функцию nTimes, которая возвращает список, состоящий из повторяющихся значений ее первого аргумента. Количество повторов определяется значением второго аргумента этой функции.

GHCi> nTimes 42 3
[42,42,42]
GHCi> nTimes 'z' 5
"zzzzz"

-}

nTimes:: a -> Int -> [a]
nTimes a n | n <=0 = []
           | otherwise = a : nTimes a (n-1)

-- nTimes = flip replicate


{-
:t head
:t tail


second :: [a] -> a
second xs = head (tail xs)
second = head . tail
-}

{-
pattern matching

let fst' ((,) x y) = x

let head' ((:) x xs) = x
let tail' (x : xs) = xs
let tail'' (_ : xs) = xs

second' (_ : xs) = head xs
second'' (_ : x : _) = x
-}


{-
length' :: [a] -> Int
length' [] = 0
length' (_ : xs) = 1 + length xs

(++) [a] -> [a] -> a
[] ++ ys = ys
(x:xs) ++ ys = x : xs ++ ys
: and ++ right associative x : (xs ++ ys)

null :: [a] -> Bool
null [] = True
null _ = False


-}

{-


Сформируйте список целых чисел, содержащий только те элементы исходного списка, значение которых нечетно.

GHCi> oddsOnly [2,5,7,10,11,12]
[5,7,11]

Для анализа четности можно использовать функции odd и even стандартной библиотеки.

-}

oddsOnly :: Integral a => [a] -> [a]
oddsOnly [] = []
oddsOnly (x : xs) | odd x = x : oddsOnly xs
                  | otherwise = oddsOnly xs


-- oddsOnly = filter odd
-- oddsOnly a = [x | x <- a, odd x]


{-
last :: [a] -> a 
last (x:[]) = x
last (_:xs) = last xs

init :: [a] -> [a]
init [_] = []
init (x:xs) = x : init xs

-}

{-
sum, product :: (Num a) => [a] -> a
maximium, minimum :: (Ord a) => [a] -> a


-}

sum1 :: (Num a) => [a] -> a
sum1 [] = 0
sum1 (x:xs) = sum1 xs + x

prod1 :: (Num a) => [a] -> a
prod1 [] = 1
prod1 (x:xs) = prod1 xs + x

minimum1 :: (Ord a) => [a] -> a
minimum1 [] = error "Error"
minimum1 [x] = x
minimum1 (x:xs) = if x < minimum1 xs then x else minimum1 xs

reverse' :: [a] -> [a]
reverse' l = rev l [] where
    rev [] a = a
    rev (x:xs) a = rev xs (x:a)


{-


Реализуйте функцию isPalindrome, которая определяет, является ли переданный ей список палиндромом.

GHCi> isPalindrome "saippuakivikauppias"
True
GHCi> isPalindrome [1]
True
GHCi> isPalindrome [1, 2]
False


-}

isPalindrome :: Eq a => [a] -> Bool
isPalindrome [] = True
isPalindrome [_] = True
isPalindrome (x:xs) = (x == last xs) && isPalindrome (init xs)

-- isPalindrome lst = reverse lst == lst

{-
zip :: [a] -> [b] -> [(a,b)]
zip [] _ = []
zip as [] = []
zip (a:as) (b:bs) = (a,b) : zip as bs

zip3 :: [a] -> [b] -> [c] -> [(a,b,c)]
zip3 (a:as) (b:bs) (c:cs) = (a, b, c) : zip3 as bs cs
zip3 _ _ _ = []

unzip :: [(a,b)] -> ([a], [b])
unzip [] = ([], [])
unzip ((x, y): xys) = 
    let (xs, ys) = unzip xys
    in (x:xs, y:ys)
-}

{-


Составьте список сумм соответствующих элементов трех заданных списков. 
Длина результирующего списка должна быть равна длине самого длинного из заданных списков, 
при этом «закончившиеся» списки не должны давать вклада в суммы.

GHCi> sum3 [1,2,3] [4,5] [6]
[11,7,3]


-}

sum3 :: Num a => [a] -> [a] -> [a] -> [a]
sum3 a b c = sum2 a (sum2 b c) where
    sum2 [] ys = ys
    sum2 xs [] = xs
    sum2 (x: xs) (y:ys) = (x+y) : sum2 xs ys


{-
sum3 [] [] [] = []
sum3 [] ys zs = sum3 [0] ys zs
sum3 xs [] zs = sum3 xs [0] zs
sum3 xs ys [] = sum3 xs ys [0]
sum3 (x:xs) (y:ys) (z:zs) = x+y+z : sum3 xs ys zs


instance (Num a) => Num [a] where
  (+) [] [] = []
  (+) xs [] = xs
  (+) [] xs = xs
  (+) (x:xs) (y:ys) = (x+y):(xs + ys)
  
sum3 :: Num a => [a] -> [a] -> [a] -> [a]
sum3 a b c = a + b + c

-}


{-
Напишите функцию groupElems которая группирует одинаковые элементы в списке (если они идут подряд) и возвращает список таких групп.

GHCi> groupElems []
[]
GHCi> groupElems [1,2]
[[1],[2]]
GHCi> groupElems [1,2,2,2,4]
[[1],[2,2,2],[4]]
GHCi> groupElems [1,2,3,2,4]
[[1],[2],[3],[2],[4]]

-}

groupElems :: Eq a => [a] -> [[a]]
groupElems [] = []
groupElems [x] = [[x]]
groupElems (x:xs) | x == head xs = 
                    let (r:rs) = groupElems xs
                    in (x : r) : rs
                  | otherwise = [x] : groupElems xs
-- groupElems a = groupHelper a [] where
--     groupHelper [] [] = []
--     groupHelper [] acc = [acc]
--     groupHelper (x:xs) [] = groupHelper xs [x]
--     groupHelper (x:xs) (a:as) | x == a = groupHelper xs (x:a:as)
--                               | otherwise = (a:as) : groupHelper xs [x]



{-
take :: Int -> [a] -> [a]
take n _ | n <= 0 = []
take _ [] = []
take n (x:xs) = x : take (n-1) xs


drop :: Int -> [a] -> [a]
drop n xs | n <= 0 = xs
drop _ [] = []
drop n (_:xs) = drop (n-1) xs

splitAt :: Int -> [a] -> ([a], [a])
splitAt n xs = (take n xs, drop n xs)

by Index

xs !! n | n < 0 = error "Prelude>!!: negative index"
[] !! _ = error "Prelude>!!: too large index"
(x:_) !! 0 = x
(_:xs) !! n = xs !! (n -1)

-}