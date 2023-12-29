import Data.Binary.Get (label)
data BBtree a = BBnil | BBtree (BBtree a) a a (BBtree a) deriving(Show)

instance Functor BBtree where
    fmap :: (a -> b) -> BBtree a -> BBtree b
    fmap _ BBnil = BBnil
    fmap f (BBtree l x1 x2 r) = BBtree (fmap f l) (f x1) (f x2) (fmap f r)

instance Foldable BBtree where
    foldr :: (a -> b -> b) -> b -> BBtree a ->  b
    foldr _ z BBnil = z
    foldr f z (BBtree l x1 x2 r) = foldr f x1Fold l
        where rightFold = foldr f z r
              x2Fold = f x2 rightFold
              x1Fold = f x1 x2Fold

myTree = BBtree (BBtree BBnil 3 4 BBnil) 1 2 (BBtree BBnil 5 6 BBnil)

bb2list :: BBtree a -> [a]
bb2list = foldr (:) []

infiniteTree x = BBtree (infiniteTree x) x x (infiniteTree x)

instance Applicative BBtree where
    pure x = BBtree BBnil x x BBnil

    (<*>) :: BBtree (a -> b) -> BBtree a -> BBtree b
    BBnil <*> y = BBnil
    x <*> BBnil = BBnil
    BBtree fl fx1 fx2 fr <*> BBtree l x1 x2 r = BBtree (fl <*> l) (fx1 x1) (fx2 x2) (fr <*> r)



bbmax :: (Ord a) => BBtree a -> Maybe a
bbmax t = if null (bb2list t) then Nothing else Just (maximum t)

bbmax' :: Ord a => BBtree a -> Maybe a
bbmax' BBnil = Nothing
bbmax' t = Just $ maximum t


bbmax'' :: Ord a => BBtree a -> Maybe a
bbmax'' BBnil = Nothing
bbmax'' t@(BBtree l x1 x2 r) = Just $ foldr max x1 t







