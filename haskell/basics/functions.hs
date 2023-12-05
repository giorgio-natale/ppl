mySafeTail :: [a] -> [a]
mySafeTail l = if null l then [] else tail l

mySafeTail' l | null l = []
              | otherwise = tail l

mySafeTail'' [] = []
mySafeTail'' (_:xs) = xs

myOr :: Bool -> Bool -> Bool
False `myOr` b = b
True `myOr` _ = True

False `myOr'` False = False
_ `myOr'` _ = True



myAnd a b = if a == True then (if b == True then True else False) else False

myAnd' a b = if a == True then b else False



