-- TASK 1
data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving (Show, Read, Eq)

foldrt :: (a -> b -> b) -> b -> Tree a -> b
foldrt f z (Leaf a) = f a z
foldrt f base (Node left root right) = f root (foldrt f (foldrt f base right) left)
-- tr = Node (Node b (Leaf d)) a (Leaf c)
-- (a `f` (с `f` (b `f` (d `f` base))))
--

countLeaves :: Num b => b -> Tree a -> b
countLeaves acc (Leaf a) = acc + 1
countLeaves acc (Node left root right) = (countLeaves acc left)+(countLeaves acc right)+1

countL :: Num b => Tree a -> b
countL t = countLeaves 0 t

isTreeElem :: Eq a => a -> Tree a -> Bool
isTreeElem x (Leaf a) = x==a
isTreeElem x (Node left root right) = x==root || (isTreeElem x left) || (isTreeElem x right)

checkHeight :: (Num b, Ord b) => Tree a -> b
checkHeight (Leaf _) = 1
checkHeight (Node left root right) = 1 + (max (checkHeight left) (checkHeight right))

-- TASK 2
data MTree a = MLeaf a | MNode a [MTree a]-- deriving (Show, Read, Eq)

instance Show a => Show (MTree a) where  
    show (MLeaf x) = "<" ++ show x ++ ">"
    show (MNode root (nodes)) = show root ++ "~" ++ show nodes

instance Functor MTree where
    fmap f (MLeaf a) = (MLeaf (f a))
    fmap f (MNode root trees) = MNode (f root) (fmap (fmap f) trees)

foldrMT :: (a -> b -> b) -> b -> MTree a -> b
foldrMT f acc (MLeaf x) = f x acc
foldrMT f acc (MNode root (trees)) = f root (Prelude.foldr foo acc trees) where
    foo x ac = foldrMT f ac x

instance Foldable MTree where
    foldr = foldrMT

countMT :: Num b => MTree a -> b -> b
countMT (MLeaf a) acc = acc+1
countMT (MNode root trees) acc = foldr countMT (acc+1) trees

countMLeaves :: Num b => MTree a -> b
countMLeaves tree = countMT tree 0

isMTreeElem :: (Eq a) => a -> MTree a -> Bool
isMTreeElem x (MLeaf a) = a == x
isMTreeElem x (MNode root trees) = root == x || any (\i -> i) (map (isMTreeElem x) trees)

checkMHeight :: MTree a -> Int
checkMHeight (MLeaf a) = 1
checkMHeight (MNode root trees) = 1 + maximum (map (checkMHeight) trees)
