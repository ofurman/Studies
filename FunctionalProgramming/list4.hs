import Data.List as List
g :: Int -> [Int]
g n = [n * 2, n * 3]

mx = [1,2,3]

validate :: Int -> Maybe Int
validate n 
    | n > 2 = Nothing
    | n < -2 = Nothing
    | otherwise = Just n


move :: Int -> Int -> Maybe Int
move m p = validate (m + p)
moves = do 
    p1 <- move 1 2
    p2 <- move 1 p1
    p3 <- move 1  p2
    p4 <- move (-1) p3
    return p4

move_list :: [Int] -> Int -> Maybe Int
move_list l sp = foldl (\a b -> a >>= move b) (Just sp) l

-- Task2
foo = do
    x1 <- [1..6]
    x2 <- [1..20]
    return (x1,x2)

foo1 = [1..6] >>= (\a -> [1..20] >>= (\b -> [(a,b)]))

-- Task3

-- move_line_upper_left :: Int -> Int -> (Int, Int) -> [(Int, Int)]
-- move_line_upper_left k n (x, y) = 
--     let line = scanl (\ (a, b) (dx, dy) -> (a+dx, b+dy) ) (x,y) (repeat (-1, 1)) in
--     tail (takeWhile (\ (a,b) -> a >= 0 && a < k && b >=0 && b < n) line)

one_step_one_dir :: Int -> Int -> (Int, Int) -> (Int,Int) -> [(Int, Int)]
one_step_one_dir k n (x, y) (v1,v2) = 
    let line = scanl (\ (a, b) (dx, dy) -> (a+dx, b+dy) ) (x,y) (repeat (v1, v2)) in
    tail (takeWhile (\ (a,b) -> a >= 0 && a < k && b >=0 && b < n) line)

one_step k n (x, y) = [(-1,1),(1,1),(1,-1),(-1,-1)] >>= one_step_one_dir k n (x, y)

three_steps k n (x,y) = List.nub (do
    first <- one_step k n (x,y)
    second <- one_step k n first
    third <- one_step k n second
    return third)

i_steps k n (x,y) 0 = [(x,y)]
i_steps k n (x,y) i = List.nub (one_step k n (x, y) >>= (\ p -> i_steps k n p (i -1)))

