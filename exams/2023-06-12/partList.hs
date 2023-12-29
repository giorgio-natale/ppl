data Part a = Part [a] a [a] deriving (Show)

checkpart :: (Ord a) => Part a -> Bool
checkpart (Part [] x (r : rs)) = (x < r) && checkpart (Part [] x rs)
checkpart (Part (l : ls) x []) = (x >= l) && checkpart (Part ls x [])
checkpart (Part (l : ls) x (r : rs)) = x >= l && x < r && checkpart (Part ls x rs)
checkpart x = True

checkpart' :: (Ord a) => Part a -> Bool
checkpart' (Part l x r) = (not . any (> x)) l && (not . any (<= x)) r

part2list :: Part a -> [a]
part2list (Part l x r) = l ++ [x] ++ r

myPart :: Part Integer
myPart = Part [3, 4, 1] 7 [11, 15, 10]
myWrongPart :: Part Integer
myWrongPart = Part [3, 4, 1] 100 [11, 15, 10]


list2part :: (Ord a) => [a] -> a -> Part a
list2part l x = Part (filter (<= x) l) x (filter (> x) l)