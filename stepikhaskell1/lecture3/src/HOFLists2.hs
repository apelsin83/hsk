module HOFLists2 where

-- import Prelude hiding (any, all, and, or)
import Data.Char

and1, or1 :: [Bool] -> Bool

and1 [] = True
and1 (x:xs) = x && and1 xs

or1 [] = True
or1 (x:xs) = x && or1 xs

all1 :: (a -> Bool) -> [a] -> Bool
all1 p = and1 . map p 

any1 :: (a -> Bool) -> [a] -> Bool
any1 p = or1 . map p 

-- unwords . map reverse . words $ "Abc is not ABC"
rev :: String -> String
rev = unwords . map reverse . words

{-


Реализуйте функцию delAllUpper, удаляющую из текста все слова, целиком состоящие из символов в верхнем регистре. 
Предполагается, что текст состоит только из символов алфавита и пробелов, знаки пунктуации, цифры и т.п. отсутствуют.

GHCi> delAllUpper "Abc IS not ABC"
"Abc not"

Постарайтесь реализовать эту функцию как цепочку композиций, аналогично revWords из предыдущего видео.

-}

-- delAllUpper :: String -> String

delAllUpper :: String -> String
delAllUpper = unwords . filter (any isLower) . words


_zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
_zipWith _ [] _ = []
_zipWith _ _ [] = []
_zipWith f (x:xs) (y:ys) = f x y : _zipWith f xs ys

_zipWith3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
_zipWith3 _ [] _ _ = []
_zipWith3 _ _ [] _ = []
_zipWith3 _ _ _ [] = []
_zipWith3 f (x:xs) (y:ys) (z:zs)= f x y z : _zipWith3 f xs ys zs


{-
zipWith (+) [1,2] [3,4]
Напишите функцию max3, которой передаются три списка одинаковой длины и которая возвращает список той же длины, содержащий на k-ой позиции наибольшее значение из величин на этой позиции в списках-аргументах.

GHCi> max3 [7,2,9] [3,6,8] [1,8,10]
[7,8,10]
GHCi> max3 "AXZ" "YDW" "MLK"
"YXZ"

-}
max3 :: Ord a => [a] -> [a] -> [a] -> [a]
max3 = zipWith3 ((max .) . max)
-- zipWith max . zipWith max x

-- max3 = zipWith3 _max
--     where _max x y z = max (max x y) z