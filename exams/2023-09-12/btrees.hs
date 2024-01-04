data Btree a = Leaf a | Branch (Btree a) (Btree a) deriving(Show)

instance Functor Btree where
    fmap :: (a -> b) -> Btree a -> Btree b
    fmap f (Leaf x) = Leaf (f x)
    fmap f (Branch x y) = Branch (fmap f x) (fmap f y)

instance Foldable Btree where
    foldr :: (a -> b -> b) -> b -> Btree a -> b
    foldr f z (Leaf x) = f x z
    foldr f z (Branch x y) = foldr f (foldr f z y) x

btrees :: a -> [Btree a]
btrees x = Leaf x : [Branch a a  | a <- btrees x]

increments = 0 : [x + 1 | x <- increments]
functions = map (+) increments

myRes = take 5 (zipWith fmap functions (btrees 1))

treeCount (Leaf _) = 1
treeCount (Branch x y) = treeCount x + treeCount y + 1

myRes' = map treeCount (btrees 1)