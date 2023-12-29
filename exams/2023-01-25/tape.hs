data Tape a = Tape [a] a [a] a

instance (Show a) => Show (Tape a) where
    show (Tape left h right _) = show (take 5 left) ++ show h ++ show (take 5 right)


instance (Eq a) => Eq (Tape a) where
    (Tape left h right _) == (Tape left' h' right' _) = left ++ [h] ++ right == left' ++ [h'] ++ right'


myTape :: Tape Integer
myTape = Tape [1, 2, 3] 4 [5, 6, 7] 0

instance Functor Tape where
    fmap f (Tape left h right b) = Tape (map f left) (f h) (map f right) (f b)


instance Applicative Tape where
    pure x = Tape (repeat x) x (repeat x) x
    (Tape left h right b) <*> (Tape left' h' right' b') = Tape (zipApp left left') (h h') (zipApp right right') (b b')
                    where
                        zipApp fs xs = [f x | (f, x) <- zip fs xs]








{- OFFICIAL VERSION (wrong)
data Tape a = Tape [a] a [a] a

instance (Show a) => Show (Tape a) where
    show (Tape left h right _) = show left ++ show h ++ show right


instance (Eq a) => Eq (Tape a) where
    (Tape left h right _) == (Tape left' h' right' _) = left ++ [h] ++ right == left' ++ [h'] ++ right'


myTape :: Tape Integer
myTape = Tape [1, 2, 3] 4 [5, 6, 7] 0

instance Functor Tape where
    fmap f (Tape left h right b) = Tape (map f left) (f h) (map f right) (f b)

instance Applicative Tape where
    pure x = Tape [] x [] x
    (Tape left h right b) <*> (Tape left' h' right' b') = Tape (zipApp left left') (h h') (zipApp right right') (b b')
                    where
                        zipApp fs xs = [f x | (f, x) <- zip fs xs]
-}





-- Partial solution with Blank as separate type
{-data TSymbol a = TSymbol a | Blank a deriving(Eq)

data Tape a = Tape [TSymbol a] (TSymbol a) [TSymbol a]

instance (Show a) => Show (TSymbol a) where
    show :: Show a => TSymbol a -> String
    show (TSymbol x) = show x
    show (Blank x) = "*"

instance Functor TSymbol where
    fmap f (Blank x) = Blank (f x)
    fmap f (TSymbol x) = TSymbol (f x)

instance Applicative TSymbol where
    pure = TSymbol
    (Blank f) <*> x = fmap f x
    (TSymbol f) <*> x = fmap f x

instance (Show a) => Show (Tape a) where
    show (Tape left h right) = show left ++ show h ++ show right

instance (Eq a) => Eq (Tape a) where
    (Tape left h right) == (Tape left' h' right') = left ++ [h] ++ right == left' ++ [h'] ++ right'

myTape = Tape [TSymbol 1, Blank 0, TSymbol 3] (TSymbol 4) [TSymbol 5, TSymbol 6, TSymbol 7]

left :: Tape a -> Tape a
left (Tape [] h right) = Tape [] Blank (h : right)
left (Tape left h right) = Tape (init left) (last left) (h : right)

instance Functor Tape where
    fmap f (Tape left h right) = Tape (map (fmap f) left) (fmap f h) (map (fmap f) right)

instance Applicative Tape where
    pure :: a -> Tape a
    pure x = Tape (repeat Blank) (TSymbol x) (repeat Blank)

    Tape fl fh fr <*> Tape l h r = Tape (zipWith (<$>) (fmap fl) l) (fmap fh h) (zipWith (fmap fr) r)
-}