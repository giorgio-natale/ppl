data Nat = Zero | Succ Nat

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ $ int2nat (n-1)

add :: Nat -> Nat -> Nat
add Zero nat = nat
add (Succ n) nat = Succ (add n nat)

multiplies :: Nat -> Nat -> Nat
multiplies Zero nat = Zero
multiplies (Succ m) n = add n $ multiplies m n


data Expr = Val Int | Add Expr Expr | Mul Expr Expr

size :: Expr -> Int
size (Val n) = 1
size (Add x y) = size x + size y
size (Mul x y) = size x + size y

eval :: Expr -> Int
eval (Val n) = n
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y

data Tree a = Leaf a | Node (Tree a) (Tree a)


