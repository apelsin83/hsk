module Task1 where

{-
(*) Find the last element of a list.
(Note that the Lisp transcription of this problem is incorrect.)
Example in Haskell:
λ> myLast [1,2,3,4]
4
λ> myLast ['x','y','z']
'z'
-}

myLast :: [a] -> a

myLast [] = error "No end for empty lists!"
myLast [a] = a
myLast (_:xs) = myLast xs 