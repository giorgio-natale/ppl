{-# LANGUAGE ParallelListComp #-}
import Control.Applicative (ZipList(ZipList))
data Gtree a = Tnil | Gtree a [Gtree a] deriving (Show, Eq)

myGTree = Gtree 2 [Gtree 1 [], Gtree 4 [Gtree 3 [], Gtree 5 []]]


instance Foldable Gtree where
    foldr :: (a -> b -> b) -> b -> Gtree a -> b
    foldr _ z Tnil = z
    foldr f z (Gtree val []) = f val z
    foldr f z (Gtree val children) = f val $ foldr (\tree z' -> foldr f z' tree) z children

gtree2list :: Gtree a -> [a]
gtree2list = foldr (:) []

gtree2list' :: Gtree a -> [a]
gtree2list' Tnil = []
gtree2list' (Gtree val []) = [val]
gtree2list' (Gtree val (childTree : brothers)) = val : gtree2list' childTree ++ concatMap gtree2list' brothers

instance Functor Gtree where
    fmap :: (a -> b) -> Gtree a -> Gtree b
    fmap _ Tnil = Tnil
    fmap f (Gtree val []) = Gtree (f val) []
    fmap f (Gtree val children) = Gtree (f val) (map (fmap f) children)


tconcat :: Gtree a -> Gtree a -> Gtree a
tconcat Tnil other = other
tconcat other Tnil = other
tconcat (Gtree val children) other = Gtree val (children ++ [other])


instance Applicative Gtree where
    pure :: a -> Gtree a
    pure x = Gtree x []

    (<*>) :: Gtree (a -> b) -> Gtree a -> Gtree b
    fxs <*> xs = foldr tconcat Tnil $ fmap (\f -> fmap f xs) fxs




{-
instance Applicative Gtree where
    pure :: a -> Gtree a
    pure x = Gtree x (repeat $ pure x)

    (<*>) :: Gtree (a -> b) -> Gtree a -> Gtree b
    Tnil <*> _ = Tnil
    _ <*> Tnil = Tnil
    (Gtree f fchildren) <*> (Gtree val children) = Gtree (f val) [fc <*> c | fc <- fchildren | c <- children]
-}