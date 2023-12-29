ternaryFun :: Int -> String -> Int -> Int
ternaryFun x _ z = x + z


apply' :: (a -> b) -> a -> b
apply' f = f


partialApply :: String -> Int -> Int
partialApply = apply' ternaryFun 3

