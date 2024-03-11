module Main where


import Numeric.LinearAlgebra
import Combinatorics

p:: Matrix R
p = (3><3) [ 1, 2, 3, 4, 0, 5, 7, 7, 2]

matMul :: Matrix R -> Matrix R -> Matrix R
matMul = (Numeric.LinearAlgebra.<>)

v :: Matrix R
v = (3><3) [1,2,3,5,8,10,3,2,1]

s :: Matrix R
s = (3><3) [1,2,3,1,2,3,5,1,9]

idm :: Matrix R
idm = (3><3) [1,0,0,0,1,0,0,0,1]

matPow :: Int -> Matrix R -> Matrix R
matPow 0 _ = idm
matPow n m = matMul m (matPow (n-1) m)

num2mat :: Int -> Matrix R
num2mat 0 = v*p
num2mat n = (-1)^(n+1)* matMul v (matPow n s)

trace :: Matrix R -> R
trace = sum . toList . takeDiag

num2num :: Int -> R
num2num = trace . num2mat

main:: IO()
main = do{
    print $ num2num 5;
    print $ weakComps 4;
}