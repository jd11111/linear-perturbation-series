{-# LANGUAGE BangPatterns #-}
module Main where

import Combinatorics
import qualified Control.Monad.State.Strict as CMSS
import qualified Data.HashMap.Strict as DHS
import qualified Data.Hashable
import Data.Array.Unboxed
import MatMul

trace :: Matrix -> Float
trace x =let{
    a = int_data x;
    ca = cols x;
} in sum [a!(i*ca + i)| i <- range(0,ca-1)]

matPow ::  Int -> Matrix -> Matrix
matPow 1 x = x
matPow n x = matMul x (matPow (n-1) x)

memoMatPow :: Matrix -> Int -> Matrix
memoMatPow x = (map (pow x) [0 ..] !!) where { pow y 1 = y; pow y n = matMul y (memoMatPow y (n-1))}

scaMul :: Float -> Matrix -> Matrix
scaMul alpha x = let{a = int_data x; ra = rows x; ca = cols x;} in let{!z = array (li,ui) [(i,alpha*(a! i)) | i <- range (li, ui)] where (li,ui)=bounds a} in GenMat{int_data=z, rows = ra, cols = ca}

p:: Matrix
p = GenMat{int_data = listArray (0,8) [1.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0], rows=3, cols=3}

v:: Matrix
v = GenMat{int_data = listArray (0,8) [1.0,-1.0,3.0,-5.0,3.0,2.0,-4.0,-8.0,1.0], rows=3, cols=3}

s:: Matrix
s = GenMat{int_data = listArray (0,8) [0.0,0.0,0.0,0.0,1.0/(1.0-4.0),0.0,0.0,0.0,1.0/(1.0+3.0)], rows=3, cols=3}

sMemo :: Int -> Matrix
sMemo = memoMatPow s

intToMat :: Int -> Matrix
intToMat 0 = matMul v p
intToMat n = scaMul ((-1.0)^(n+1)) (matMul v (sMemo n))

memoIntToMat :: Int -> Matrix
memoIntToMat = (map intToMat [0 ..] !!)

type MyMemo a b = CMSS.State (DHS.HashMap a b) b

myMemo :: Data.Hashable.Hashable a => (a -> MyMemo a b) -> a -> MyMemo a b
myMemo f x = CMSS.gets (DHS.lookup x) >>= maybe z return where z = f x >>= (\ y -> CMSS.modify (DHS.insert x y) >> return y)

runMyMemo :: (t -> CMSS.State s a) -> s -> t -> (a, s)
runMyMemo f sta x = CMSS.runState (f x) sta

weakCombToMatMemo :: [Int] -> MyMemo [Int] Matrix
weakCombToMatMemo [x] = return (memoIntToMat x)
weakCombToMatMemo (x:xs) = myMemo weakCombToMatMemo xs >>= \y -> return $ matMul (memoIntToMat x) y
weakCombToMatMemo _ = undefined

pertCoeffNew :: [[Int]] -> (Float, DHS.HashMap [Int] Matrix)
pertCoeffNew [] = (0.0,DHS.empty)
pertCoeffNew (x:xs) = let w = pertCoeffNew xs in let z = runMyMemo weakCombToMatMemo (snd w) in (fst w + (trace . fst . z) x,snd . z $ x)

main:: IO()
main = do{
  print $ map (fst . pertCoeffNew . weakComps) (take 10 [1..]);
}