module HOF where

import Prelude hiding (filter,takeWhile,dropWhile,span,break,map,concat,concatMap)
import Data.Char (isDigit)


filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter p (x:xs)
    | p x = x : filter p xs
    | otherwise = filter p xs
-- filter (< 3) [1,2,2,1,3,4]

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ [] = []
takeWhile p (x:xs)
    | p x = x : takeWhile p xs
    | otherwise = []

-- takeWhile (< 3) [1,2,2,1,3,4]

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile _ [] = []
dropWhile p xs@(x:xs')
    | p x = dropWhile p xs'
    | otherwise = xs
-- xs@(x:xs') local pseudonim
-- dropWhile (< 3) [1,2,2,1,3,4]

span :: (a -> Bool) -> [a] -> ([a], [a])
span p xs = (takeWhile p xs, dropWhile p xs)

break :: (a -> Bool) -> [a] -> ([a], [a])
break p = span (not . p)

{-

Напишите функцию readDigits, принимающую строку и возвращающую пару строк.
Первый элемент пары содержит цифровой префикс исходной строки, а второй - ее оставшуюся часть.

GHCi> readDigits "365ads"
("365","ads")
GHCi> readDigits "365"
("365","")

В решении вам поможет функция isDigit из модуля Data.Char.

-}


readDigits :: String -> (String, String)
readDigits = span isDigit


{-
Реализуйте функцию filterDisj, принимающую два унарных предиката и список, и возвращающую список элементов, удовлетворяющих хотя бы одному из предикатов.

GHCi> filterDisj (< 10) odd [7,8,10,11,12]
[7,8,11]
-}
filterDisj :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
filterDisj _ _ [] = []
filterDisj p1 p2 (x:xs)
    | p1 x || p2 x = x : filterDisj p1 p2 xs
    | otherwise = filterDisj p1 p2 xs
-- filterDisj p1 p2 = filter (\x -> p1 x || p2 x)

{-

Напишите реализацию функции qsort. Функция qsort должная принимать на вход список элементов и сортировать его в порядке возрастания с 
помощью сортировки Хоара: для какого-то элемента x изначального списка (обычно выбирают первый) 
делить список на элементы меньше и не меньше x, и потом запускаться рекурсивно на обеих частях.

GHCi> qsort [1,3,2,5]
[1,2,3,5]


-}

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort (filter (< x) xs) ++ [x] ++ qsort(filter (>= x) xs) 


-- qsort [] = []
-- qsort (x:xs) = let (l, r) = splitBy (<x) xs where
--                    splitBy pred = foldr f ([], []) where
--                                   f x ~(yes, no) | pred x = (x : yes, no) 
--                                                  | otherwise = (yes, x : no)
--                in qsort l ++ x : qsort r

-- (a -> b)  obschee oboznachenie funkcii
map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = f x : map f xs
--map (+10) [1,2,3,4]


concat :: [[a]] -> [a]
concat [] = []
concat (xs:xss) = xs ++ concat xss

-- concat ["Hello ", "world", "!"]

concatMap :: ( a-> [b] ) -> [a] -> [b]
concatMap f = concat . map f


-- concatMap (\x -> [x,x,x]) "ABCD"

{-
Напишите функцию squares'n'cubes, принимающую список чисел,
и возвращающую список квадратов и кубов элементов исходного списка.

GHCi> squares'n'cubes [3,4,5]
[9,27,16,64,25,125]
-}
squares'n'cubes :: Num a => [a] -> [a]
squares'n'cubes = concatMap (\x -> [x ^ 2,x ^ 3])



{-


    Let Expressions.

    Haskell's let expressions are useful whenever a nested set of bindings is required. As a simple example, consider:

    let y   = a*b
        f x = (x+y)/y
    in f c + f d

    The set of bindings created by a let expression is mutually recursive, and pattern bindings are treated as lazy patterns (i.e. they carry an implicit ~). The only kind of declarations permitted are type signatures, function bindings, and pattern bindings.

    Where Clauses.

    Sometimes it is convenient to scope bindings over several guarded equations, which requires a where clause:

    f x y  |  y>z           =  ...
           |  y==z          =  ...
           |  y<z           =  ...
         where z = x*x

    Note that this cannot be done with a let expression, which only scopes over the expression which it encloses. A where clause is only allowed at the top level of a set of equations or case expression. The same properties and constraints on bindings in let expressions apply to those in where clauses. These two forms of nested scope seem very similar, but remember that a let expression is an expression, whereas a where clause is not -- it is part of the syntax of function declarations and case expressions.

-}

{-


Воспользовавшись функциями map и concatMap, определите функцию perms, 
которая возвращает все перестановки, которые можно получить из данного списка, 
в любом порядке.

GHCi> perms [1,2,3]
[[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2],[3,2,1]]

Считайте, что все элементы в списке уникальны, 
и что для пустого списка имеется одна перестановка.

-}

-- perms :: [a] -> [[a]]
-- perms = undefined where
--     split [] = [[]]
--     split [x] = [[x]]
--     split (x:xs) = [x] : split xs

-- split [] = [[]]
-- split [x] = [[x]]
-- split (x:xs) = [x] : split xs

-- perms :: [a] -> [[a]]
-- perms = undefined where
-- move :: [a] -> [a] -> [[a]]
-- move [] [] = [[]]
-- move [x] [] = [[x]]
-- move xs [] = []
-- move xs (y:ys) = ((y:xs) ++ ys) : move (y:xs) ys ++ move (y:xs) ys


-- -- move :: [a] -> [a] -> [[a]]
-- move [] = [[]]
-- move [x] = [[x]]
-- move (x:xs) | 


 
-- swap _ [] [] = [[]]
-- swap _ xs [] = [xs]
-- swap a xs (y:ys) = (xs ++ [y] ++ [a]) : swap a (xs ++ [y]) ys
-- -- swap a xs (y:ys) = (xs ++ [y] ++ [a]) : swap a (xs ++ [y]) ys

swap [] [] = [[]]
swap [x] [y] = [[x,y], [y,x]]
swap [] [y] = [[y]]
swap [x] [] = [[x]]
swap (x:xs) (y: ys) = undefined