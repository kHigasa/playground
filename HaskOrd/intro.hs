import Data.List (nub)
import qualified Data.Map as Map

--factorial :: Integer -> Integer
--factorial n = product [1..n]

--factorial :: Int -> Int
--factorial 0 = 1
--factorial n = n * factorial (n - 1)

factorial :: Int -> Int
factorial n = case n of 0 -> 1
                        n -> n * factorial (n - 1)

--calcBmis :: [(Doble, Double)] -> [Double]
--calcBmis xs = [bmi w h | (w, h) <- xs]
--    where bmi weight height = weight / height ^ 2

calcBmis :: [(Double, Double)] -> [Double]
calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]

chain :: Integer -> [Integer]
chain 1 = [1]
chain n
    | even n = n : chain (n `div` 2)
    | odd n  = n : chain (n * 3 + 1)

lochain :: Int
lochain = length (filter (\xs -> length xs > 15)
                         (map chain [1..100]))

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x : acc) [] xs

lounqnum :: (Eq a) => [a] -> Int
lounqnum = length . nub

findKey :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey key xs = foldr
                   (\(k, v) acc -> if key == k then Just v else acc)
                   Nothing xs

