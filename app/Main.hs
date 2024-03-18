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

intToMat:: Matrix -> Matrix -> Matrix -> Int -> Matrix
intToMat v p s 0 = matMul v p
intToMat v p s n = scaMul ((-1.0)^(n+1)) (matMul v (memoMatPow s n))

memoIntToMat :: Matrix -> Matrix -> Matrix -> Int -> Matrix
memoIntToMat v p s  = ((map (intToMat v p s) [0 ..]) !!)

type MyMemo a b = CMSS.State (DHS.HashMap a b) b

myMemo :: Data.Hashable.Hashable a => (a -> MyMemo a b) -> a -> MyMemo a b
myMemo f x = CMSS.gets (DHS.lookup x) >>= maybe z return where z = f x >>= (\ y -> CMSS.modify (DHS.insert x y) >> return y)

runMyMemo :: (t -> CMSS.State s a) -> s -> t -> (a, s)
runMyMemo f sta x = CMSS.runState (f x) sta

weakCombToMatMemo :: Matrix -> Matrix -> Matrix -> [Int] -> MyMemo [Int] Matrix
weakCombToMatMemo v p s [x] = return (memoIntToMat v p s x)
weakCombToMatMemo v p s (x:xs) = myMemo (weakCombToMatMemo v p s) xs >>= \y -> return $ matMul (memoIntToMat v p s x) y
weakCombToMatMemo v p s _ = undefined

pertCoeffNew2 :: Matrix -> Matrix -> Matrix ->  DHS.HashMap [Int] Matrix-> [[Int]] -> (Float, DHS.HashMap [Int] Matrix)
pertCoeffNew2 v p s st [] = (0.0,st)
pertCoeffNew2 v p s st (x:xs) = let w = pertCoeffNew2 v p s st xs in let z = runMyMemo (weakCombToMatMemo v p s) (snd w) in (fst w + (trace . fst . z) x,snd . z $ x)

pertCoeff :: Matrix -> Matrix -> Matrix -> Int -> ([Float],DHS.HashMap [Int] Matrix)
pertCoeff v p s 1 = let z = pertCoeffNew2 v p s DHS.empty (weakComps 1) in ([fst z],snd z)
pertCoeff v p s n = let w = pertCoeff v p s (n-1) in let z = pertCoeffNew2 v p s (snd w) (weakComps n) in ((fst z):(fst w),snd z)

main:: IO()
main = do{
  let {p = GenMat{int_data = listArray (0,8) [1.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0], rows=3, cols=3};
  v = GenMat{int_data = listArray (0,8) [1.0,-1.0,3.0,-5.0,3.0,2.0,-4.0,-8.0,1.0], rows=3, cols=3};
  s = GenMat{int_data = listArray (0,8) [0.0,0.0,0.0,0.0,1.0/(1.0-4.0),0.0,0.0,0.0,1.0/(1.0+3.0)], rows=3, cols=3}}
  in print . reverse $ fst (pertCoeff v p s 6);
}