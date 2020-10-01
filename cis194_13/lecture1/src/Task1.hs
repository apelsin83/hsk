module Task1 where

toDigits :: Integer -> [Integer]
toDigits a | a <= 0 = []
           | otherwise = toDigits (a `div` 10) ++ [a `mod` 10]


toDigitsRev :: Integer -> [Integer]
toDigitsRev a = reverse (toDigits a)
-- toDigitsRev a | a <= 0 = []
--               | otherwise = a `mod` 10 : toDigitsRev (a `div` 10)



doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther (x:(y:zs)) = x : 2 * y :doubleEveryOther zs
{-
doubleEveryOther a | null a = []
                   | even (length a) = head a * 2 : doubleEveryOther (tail a) 
                   | otherwise = head a : doubleEveryOther (tail a)
                --    | length a `mod` 2 == 0 = head a * 2 : doubleEveryOther (tail a) 
-}

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:zs) = if x > 9 
    then sumDigits (toDigits x ++ zs)
    else x + sumDigits zs

validate a = sumDigits (doubleEveryOther (toDigitsRev a)) `mod` 10 == 0

{-
a   | null a = 0
    | head a > 9 = sumDigits (tail a) + head a `div` 10 + head a `mod` 10
    | otherwise = sumDigits (tail a) + head a
-}
-- validate :: Integer -> Bool
{-
-- validate a = sumDigits (doubleEveryOther (toDigits a))

-}