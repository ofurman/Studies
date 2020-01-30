map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f xs = foldr (\y ys -> (f y):ys) [] xs


sumsqeven :: [Int] -> Int
sumsqeven = foldr (\x y -> if odd x then x*x+y else 0+y) 0


isprime :: Integer -> Bool
isprime x = null [y | y<-2:[3,5..floor (sqrt (fromIntegral x))], x `mod` y == 0]

numprime :: [Integer] -> Integer
numprime = foldr (\x y -> if isprime x then 1+y else 0+y) 0


eAprox :: Enum a => Fractional a =>  a -> a
eAprox n = foldr ((+) . (1 /)) 0 (scanl (*) 1 [1..n])

sumation :: Fractional b => b -> b -> [b] -> b
sumation sum acc [] = sum
sumation sum acc (n:ns) = sumation (sum+(1/acc)) (acc*n) ns

eAprox' :: (Fractional b, Enum b) => b -> b
eAprox' n = sumation 0 1 [1..n]

