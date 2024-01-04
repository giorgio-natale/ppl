-- NOTE: cannot overload constructors. Each case must have a different constructor name (e.g. could not call D2Cons1 as D2Cons2)
data D2L a = D2Nil | D2Cons1 a (D2L a) | D2Cons2 [a] (D2L a)

instance (Show a) => Show (D2L a) where
    show :: Show a => D2L a -> String
    show D2Nil = "[]"
    show (D2Cons1 x D2Nil) = "[" ++ show x ++ "]"
    show (D2Cons1 x y) = "[" ++ show x ++ ", " ++ tail (show y)
    show (D2Cons2 x D2Nil) = "[" ++ show x ++ "]"
    show (D2Cons2 x y) = "[" ++ show x ++ ", " ++ tail (show y)

instance Functor D2L where
    fmap :: (a -> b) -> D2L a -> D2L b
    fmap _ D2Nil = D2Nil
    fmap f (D2Cons1 x y) = D2Cons1 (f x) (fmap f y)
    fmap f (D2Cons2 x y) = D2Cons2 (map f x) (fmap f y)

instance Foldable D2L where
    foldr :: (a -> b -> b) -> b -> D2L a -> b
    foldr _ z D2Nil = z
    foldr f z (D2Cons1 x y) = f x (foldr f z y)
    foldr f z (D2Cons2 x y) = foldr f (foldr f z y) x

instance Applicative D2L where
    pure :: a -> D2L a
    pure x = D2Cons1 x D2Nil

    (<*>) :: D2L (a -> b) -> D2L a -> D2L b
    x <*> y = foldr (\f -> concat' $ fmap f y) D2Nil x
    

my2dl = D2Cons1 1 (D2Cons2 [2, 3] D2Nil)

flatten :: D2L a -> [a]
flatten = foldr (:) []

concat' :: D2L a -> D2L a -> D2L a
concat' D2Nil y = y
concat' x D2Nil = x
concat' (D2Cons1 x xs) y = D2Cons1 x (concat' xs y)
concat' (D2Cons2 x xs) y = D2Cons2 x (concat' xs y)



