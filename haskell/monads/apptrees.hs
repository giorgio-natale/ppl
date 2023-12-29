data Tree a = Empty | Leaf a | Node (Tree a) (Tree a)

instance (Show a) => Show (Tree a) where
    show Empty = ""
    show (Leaf x) = show x
    show (Node x y) = "(" ++ show x ++ "||" ++ show y ++ ")"

instance Show (a -> b) where
    show _ = "<Function>"

instance Functor Tree where
    fmap _ Empty = Empty
    fmap f (Leaf x) = Leaf (f x)
    fmap f (Node x y) = Node (fmap f x) (fmap f y)

instance Foldable Tree where
    foldr f z Empty = z
    foldr f z (Leaf x) = f x z
    foldr f z (Node x y) = foldr f (foldr f z y) x

(+++) :: Tree a -> Tree a -> Tree a
Empty +++ t = t
t +++ Empty = t
tl +++ tr = Node tl tr

tconcat :: Tree (Tree a) -> Tree a
tconcat Empty = Empty
tconcat (Leaf x) = x
tconcat (Node l r) = tconcat l +++ tconcat r


instance Applicative Tree where
    pure = Leaf
    tf <*> t = tconcat $ fmap (<$> t) tf

myTree = Node (Leaf 1) (Node (Leaf 2) (Leaf 3))
funTree = Node (Leaf (1+)) (Leaf (2*))

incrementedMyTree = fmap (1+) myTree
sumOfValues = foldr (+) 0 incrementedMyTree




