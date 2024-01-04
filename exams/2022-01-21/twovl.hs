data Tvtl a = Values a a | Lists [a] [a]

instance (Show a) => Show (Tvtl a) where
    show (Values x y) = "(" ++ show x ++ " | " ++ show y ++ ")"
    show (Lists x y) = "(" ++ show x ++ " | " ++ show y ++ ")"

myTvtl = Lists [1, 2, 3] [8, 9, 10]

instance Functor Tvtl where
    fmap :: (a -> b) -> Tvtl a -> Tvtl b
    fmap f (Values x y) = Values (f x) (f y)
    fmap f (Lists x y) = Lists (fmap f x) (fmap f y)

instance Foldable Tvtl where
    foldr :: (a -> b -> b) -> b -> Tvtl a -> b
    foldr f z (Values x y) = f x $ f y z
    foldr f z (Lists x y) = foldr f z $ (++) x y 

instance Applicative Tvtl where -- Probably not correct, but also the one written by the prof is not...
    pure :: a -> Tvtl a
    pure x = Values x x

    (<*>) :: Tvtl (a -> b) -> Tvtl a -> Tvtl b
    Values f1 f2 <*> Values x1 x2 = Values (f1 x1) (f2 x2)
    Values f1 f2 <*> Lists x1 x2 = Lists (map f1 x1) (map f2 x2)
    Lists f1s f2s <*> Values x1 x2 = Lists (fmap (\f -> f x1) f1s) (fmap (\f -> f x2) f2s)
    Lists f1s f2s <*> Lists x1 x2 = Lists (f1s <*> x1) (f2s <*> x2)




