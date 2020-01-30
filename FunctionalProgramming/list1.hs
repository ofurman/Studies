-- Task 1
factorial 1 = 1
factorial n = n * factorial (n-1)

facTail 0 r = r
facTail n r = facTail (n-1) (r*n)
factorial' n = facTail n 1     

-- Task 2
rev [] = []
rev (x:xs) = rev xs ++ [x]

prepend x1 [] = x1
prepend x1 (x:xs) = prepend (x:x1) xs 
rev' x = prepend [] x

-- Task 3
fib :: Int -> Int -> Int -> Int
fib n m 1 = m
fib n m v = fib m (n + m) (v - 1)

fibn :: Int -> Int
fibn v = fib 0 1 v

-- Task 4
gcdf :: Int -> Int -> Int
gcdf n k = if k > n then gcdf k n
                    else last (filter (\x -> (k `mod` x) == 0 && (n `mod` x) == 0) [1 .. n])

eul n = filter (\x -> gcdf n x == 1) [1 .. n]

eul' :: Int -> Int
eul' n = length (filter (\x -> gcd n x == 1) [1 .. n])
-- Task 4b
sumEul n = sum (map (eul') (filter (\x -> (n `mod` x) == 0) [1 .. n]))

-- Task 5
pairs ([],xs) r = r
pairs (x:xs,ys) r = pairs (xs,x:ys) ((xs,rev (x:ys)):r)
genPairs xs = pairs (xs, []) [(xs,[])]

-- Task 6
discons :: (Eq a) => a -> [a] -> [a]
discons x [] = [x]
discons x xs
    | x == head xs = xs
    | otherwise = x:xs

ecd :: (Eq a) => [a] -> [a]
ecd [] = []
ecd ys = foldr discons [] ys


-- Task 7
pack :: Eq a => [a] -> [[a]]
pack = foldr dupcons []
  where dupcons x [] = [[x]]
        dupcons x (b@(b1:_):bs)
          | x == b1  = ((x:b):bs)
          | otherwise = [x]:b:bs

-- Task 8
foo :: Eq a => [(a,Int)] -> a -> [(a,Int)]
foo [] a = [(a, 1)]
foo ((c,i):xs) a
    | c == a   =   ((c, i + 1) : xs)
    | otherwise = ((a, 1): (c,i):xs)

encode l = rev (foldl foo [] l)

revenc :: Enum a => [(a,Int)] -> [a]
revenc l = l >>= (\(a,i) -> take i [a,a..] )

-- Task 10
permut :: Eq a => [a] -> [[a]]
permut [] = [[]]
permut l = [ a:as | a <- l, as <- (permut $ filter (\x -> x /= a) l)]


-- Task 9
sublists :: [a] -> Int -> [[a]]
sublists [] n = []
sublists a 0 = []
sublists l@(h:hs) n
    | n > length l = []
    | otherwise = ((take n l):(sublists hs n))

powerlist :: [a] -> [[a]]
powerlist l = ([]:([0..length l] >>= (\x -> sublists l x)))