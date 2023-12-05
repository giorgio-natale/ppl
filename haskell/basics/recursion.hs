

and' :: [Bool] -> Bool
and' [] = True
and' (x : xs) = x && and' xs

concat' :: [[a]] -> [a]
concat' [] = []
concat' ([]:rest) = concat' rest
concat' (x:rest) =  x ++ concat' rest

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = x : replicate' (n-1) x

selectElement :: [a] -> Int -> a
selectElement (x:xs) 0 = x
selectElement (x:xs) n = selectElement xs (n-1)

elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' x (e:es) = x == e || elem' x es

merge' :: Ord a =>[a] -> [a] -> [a]
merge' [] y  = y
merge' x [] = x
merge' (x:xs) (y:ys) | x < y = x : merge' xs (y:ys)
                     | otherwise = y : merge' (x:xs) ys

half :: [a] -> [a]
half [] = []
half [x] = [x]
half (x:xs) = x : half (init xs)


msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort l = merge' (msort (half l)) (msort (lastHalf l))
        where
            lastHalf mList = [x | x <- mList, x `notElem` half mList]


