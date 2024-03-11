module Main where

import Numeric.LinearAlgebra
import Combinatorics

matMul :: Numeric t => Matrix t -> Matrix t -> Matrix t
matMul = (Numeric.LinearAlgebra.<>)

matPow ::Int -> Matrix C -> Matrix C
matPow 0 _ = idMat
matPow n m = matMul m (matPow (n-1) m)

trace :: Matrix C -> C
trace = sum . toList . takeDiag

p:: Matrix C
p = (3><3) [ 1, 0, 0, 0, 0, 0, 0, 0, 0]

v :: Matrix C
v = (3><3) [1,-1,3,-5,3,2,-4,-8,1]

s :: Matrix C
s = (3><3) [0,0,0,0,1/(1-4),0,0,0,1/(1+3)]

idMat :: Matrix C
idMat = ident 3

intToMat :: Int -> Matrix C
intToMat 0 = matMul v p
intToMat n = (-1)^(n+1)* matMul v (matPow n s)

weakCombToMat :: [Int] -> Matrix C
weakCombToMat [] = idMat
weakCombToMat (x:xs) = matMul (intToMat x) (weakCombToMat xs)

pertCoeff :: Int -> C
pertCoeff n = trace . sum $ (map weakCombToMat (weakComps (n+1)))

main:: IO()
main = do{
  print $ map pertCoeff (take 13 [0..]);
}