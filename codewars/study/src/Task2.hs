module Task2 (uniqueInOrder)  where
import Data.List
uniqueInOrder :: Eq a => [a] -> [a]
-- uniqueInOrder [] = []
-- uniqueInOrder [x] = [x]
-- uniqueInOrder (x: xs) = if x == head xs 
--                         then uniqueInOrder xs 
--                         else x : uniqueInOrder xs


uniqueInOrder = map head . group