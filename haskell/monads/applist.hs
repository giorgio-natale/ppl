

-- ((+) <$>) lifts the + function from being Num a => a -> (a -> a) to being (Functor f, Num a) => f a -> f(a -> a).
-- We can think at it as if we are trying to sum values in the context of the functor type.

sumFunctor :: (Functor f, Num a) => f a -> f (a-> a)
sumFunctor = ((+)<$>)

-- If we apply it to a Functor Num we get a (Functor f, Num a) => f (a -> a).
-- But we don't know how to create a Functor from a Num, we would need a pure function, meaning we can only
-- write incrementBy1 for lists, Maybe , ... ex IncrementListBy1 = sumFunctor [1]
-- We can solve this by imposing that the f is an applicative, for which we can use the pure function
incrementBy1 :: (Applicative f, Num a) => f (a -> a)
incrementBy1 = sumFunctor (pure 1)

-- In this case we are incrementing a non deterministic Num value getting a non deterministic Num value as result
nd1 = [1, 5, 7]
nd2 = incrementBy1 <*> nd1

-- If instead of the applicative apply we use the usual function application, we get a different result (???)
nd2' = incrementBy1 nd1


-- Let's create custom functions <<*>> and pure' in order to make list applicative from scratch
(<<*>>) :: [] (a -> b) -> [] a -> [] b
-- [] <<*>> _ = []
-- (f : fs) <<*>> xs = map f xs ++ (fs <<*>> xs)
fs <<*>> xs = (concat . (map (\f -> map f xs))) fs


pure' :: a -> [] a
pure' x = [x]

myApplicativeList = pure' (+) <<*>> [1, 2, 3] <<*>> [2, 3, 4]


