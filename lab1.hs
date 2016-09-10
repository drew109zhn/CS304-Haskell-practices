
listlength :: [a] -> Int
listlength [] = 0
listlength (val:rest) = 1 + (listlength rest)

listsum :: [Int] -> Int
listsum [] = 0;
listsum (val:rest) = val + (listsum rest)

data Nat = Z | S Nat deriving(Read, Show, Eq)
nattoInt ::Nat -> Int
nattoInt Z = 0
nattoInt (S x) = 1 + (nattoInt x)

buildNat ::Int -> Nat
buildNat 0 = Z
buildNat x = S (buildNat (x - 1))

add :: Nat -> Nat -> Nat
add Z n =  n
add (S x)  n =  S (add x n)

--minus :: Nat -> Nat -> Nat
minus n Z =  n
minus Z n =  Z
minus (S x) (S y) = S (minus x n)

--mult :: Nat -> Nat -> Nat
mult Z x =  Z
mult (S x) y = add (mult x y) y

--factNat :: Nat -> Nat
factNat Z = 1
factNat (S x) =  mult (S x) (factNat x)

addA :: [[Char]] -> [[Char]]
addA [] = [];
addA (val:rest) = (val ++ "a"):addA (rest)

astar :: Int -> [[Char]]
astar 1 = [""]
astar n = "" : addA (astar (n - 1))



