module Lec1 where
x :: Int
x = 3

-- Note that normal (non-literate) comments are preceded by two hyphens
{- or enclosed
   in curly brace/hyphen pairs. -}

-- Machine-sized integers
i :: Int
i = -78

biggestInt, smallestInt :: Int
biggestInt  = maxBound
smallestInt = minBound

-- Arbitrary-precision integers
n :: Integer
n = 1234567890987654321987340982334987349872349874534

reallyBig :: Integer
reallyBig = 2^(2^(2^(2^2)))

numDigits :: Int
numDigits = length (show reallyBig)

-- For floating-point numbers, there is Double:

-- Double-precision floating point
d1, d2 :: Double
d1 = 4.5387
d2 = 6.2831e-4

-- There is also a single-precision floating point number type, Float.

-- Finally, there are booleans, characters, and strings:

-- Booleans
b1, b2 :: Bool
b1 = True
b2 = False

-- Unicode characters
c1, c2, c3 :: Char
c1 = 'x'
c2 = 'Ø'
c3 = 'ダ'

-- Strings are lists of characters with special syntax
s :: String
s = "Hello, Haskell!"

ex01 = 3 + 2
ex02 = 19 - 27
ex03 = 2.35 * 8.6
ex04 = 8.7 / 3.1


ex05 = mod 19 3
ex06 = 19 `mod` 3
ex07 = 7 ^ 222
ex08 = (-3) * (-7)

-- This is an error since / performs floating-point division only. For integer division we can use div.

ex09 = i `div` i
ex10 = 12 `div` 5

ex11 = True && False
ex12 = not (False || True)

-- Things can be compared for equality with (==) and (/=), or compared for order using (<), (>), (<=), and (>=).

ex13 = ('a' == 'a')
ex14 = (16 /= 3)
ex15 = (5 > 3) && ('p' <= 'q')
ex16 = "Haskell" > "C++"

-- Compute the sum of the integers from 1 to n.
sumtorial :: Integer -> Integer
sumtorial 0 = 0
sumtorial n = n + sumtorial (n-1)

-- Choices can also be made based on arbitrary Boolean expressions using guards. For example:

hailstone :: Integer -> Integer
hailstone n
  | n `mod` 2 == 0 = n `div` 2
  | otherwise      = 3*n + 1


-- As a more complex (but more contrived) example:

foo :: Integer -> Integer
foo 0 = 16
foo 1 
  | "Haskell" > "C++" = 3
  | otherwise         = 4
foo n
  | n < 0            = 0
  | n `mod` 17 == 2  = -43
  | otherwise        = n + 3



isEven :: Integer -> Bool
isEven n 
  | n `mod` 2 == 0 = True
  | otherwise      = False


p :: (Int, Char)
p = (3, 'x')

sumPair :: (Int,Int) -> Int
sumPair (x,y) = x + y

-- Using functions, and multiple arguments

-- To apply a function to some arguments, just list the arguments after the function, separated by spaces, like this:

f :: Int -> Int -> Int -> Int
f x y z = x + y + z
ex17 = f 3 17 8

{-
The above example applies the function f to the three arguments 3, 17, and 8. Note also the syntax for the type of a function with multiple arguments, like Arg1Type -> Arg2Type -> ... -> ResultType. This might seem strange to you (and it should!). Why all the arrows? Wouldn’t it make more sense for the type of f to be something like Int Int Int -> Int? Actually, the syntax is no accident: it is the way it is for a very deep and beautiful reason, which we’ll learn about in a few weeks; for now you just have to take my word for it!

Note that function application has higher precedence than any infix operators. So it would be incorrect to write
-}
f 3 n+1 7

-- if you intend to pass n+1 as the second argument to f, because this parses as

(f 3 n) + (1 7)

-- Instead, one must write

f 3 (n+1) 7


nums, range, range2 :: [Integer]
nums   = [1,2,3,19]
range  = [1..100]
range2 = [2,4..100]

-- Haskell (like Python) also has list comprehensions; you can read about them in LYAH.

-- Strings are just lists of characters. That is, String is just an abbreviation for [Char], and string literal syntax (text surrounded by double quotes) is just an abbreviation for a list of Char literals.

-- hello1 and hello2 are exactly the same.

hello1 :: [Char]
hello1 = ['h', 'e', 'l', 'l', 'o']

hello2 :: String
hello2 = "hello"

helloSame = hello1 == hello2


emptyList = []

-- Other lists are built up from the empty list using the cons operator, (:). Cons takes an element and a list, and produces a new list with the element prepended to the front.

ex18 = 1 : []
ex19 = 3 : (1 : [])
ex20 = 2 : 3 : 4 : []

ex21 = [2,3,4] == 2 : 3 : 4 : []

-- We can see that [2,3,4] notation is just convenient shorthand for 2 : 3 : 4 : []. Note also that these are really singly linked lists, NOT arrays.

-- Generate the sequence of hailstone iterations from a starting number.
hailstoneSeq :: Integer -> [Integer]
hailstoneSeq 1 = [1]
hailstoneSeq n = n : hailstoneSeq (hailstone n)


-- Compute the length of a list of Integers.
intListLength :: [Integer] -> Integer
intListLength []     = 0
intListLength (x:xs) = 1 + intListLength xs

sumEveryTwo :: [Integer] -> [Integer]
sumEveryTwo []         = []     -- Do nothing to the empty list
sumEveryTwo (x:[])     = [x]    -- Do nothing to lists with a single element
sumEveryTwo (x:(y:zs)) = (x + y) : sumEveryTwo zs


-- The number of hailstone steps needed to reach 1 from a starting
-- number.
hailstoneLen :: Integer -> Integer
hailstoneLen n = intListLength (hailstoneSeq n) - 1