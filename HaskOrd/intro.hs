factorial :: Integer -> Integer
factorial n = product [1..n]

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

factorial :: Int -> Int
factorial n = case n of 0 -> 1
                        n -> n * factorial (n - 1)

calcBmis :: [(Doble, Double)] -> [Double]
calcBmis xs = [bmi w h | (w, h) <- xs]
    where bmi weight height = weight / height ^ 2

calcBmis :: [(Doble, Double)] -> [Double]
calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]

