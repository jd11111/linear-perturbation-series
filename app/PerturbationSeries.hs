{-# LANGUAGE BangPatterns #-}

module PerturbationSeries where

import Combinatorics
import qualified Control.Monad.State.Strict as CMSS
import Data.Array.Unboxed
import qualified Data.HashMap.Strict as DHS
import qualified Data.Hashable
import MatMul

trace :: Matrix -> Float
trace x =
  let a = int_data x
      ca = cols x
   in sum [a ! (i * ca + i) | i <- range (0, ca - 1)]

matPow :: Int -> Matrix -> Matrix
matPow 1 x = x
matPow n x = matMul x (matPow (n - 1) x)

scaMul :: Float -> Matrix -> Matrix
scaMul alpha x = let a = int_data x; ra = rows x; ca = cols x in let !z = array (li, ui) [(i, alpha * (a ! i)) | i <- range (li, ui)] where (li, ui) = bounds a in GenMat {int_data = z, rows = ra, cols = ca}

intToMat :: Matrix -> Matrix -> Matrix -> Int -> Matrix
intToMat v p _ 0 = matMul v p
intToMat v _ s n = scaMul ((-1.0) ^ (n + 1)) (matMul v (matPow n s)) -- old: memoMatPow

type MyMemo a b = CMSS.State (DHS.HashMap a b) b

myMemo :: (Data.Hashable.Hashable a) => (a -> MyMemo a b) -> a -> MyMemo a b
myMemo f x = CMSS.gets (DHS.lookup x) >>= maybe z return where z = f x >>= (\y -> CMSS.modify (DHS.insert x y) >> return y)

runMyMemo :: (t -> CMSS.State s a) -> s -> t -> (a, s)
runMyMemo f sta x = CMSS.runState (f x) sta

weakCombToMatMemo :: [Matrix] -> [Int] -> MyMemo [Int] Matrix
weakCombToMatMemo mem [x] = return (mem !! x) -- was memoIntToMat
weakCombToMatMemo mem (x : xs) = myMemo (weakCombToMatMemo mem) xs >>= \y -> return $ matMul (mem !! x) y
weakCombToMatMemo _ _ = undefined

pertCoeffNew2 :: [Matrix] -> DHS.HashMap [Int] Matrix -> [[Int]] -> (Float, DHS.HashMap [Int] Matrix)
pertCoeffNew2 _ st [] = (0.0, st)
pertCoeffNew2 mem st (x : xs) = let w = pertCoeffNew2 mem st xs in let z = runMyMemo (weakCombToMatMemo mem) (snd w) in (fst w + (trace . fst . z) x, snd . z $ x)

memPertCoeff :: [Matrix] -> Int -> ([Float], DHS.HashMap [Int] Matrix) -- v is the perturbation, p the projection onto the eigenspace under consideration and s the reduced resolvent
memPertCoeff mem 1 = let z = pertCoeffNew2 mem DHS.empty (weakComps 1) in ([fst z], snd z)
memPertCoeff mem n = let w = memPertCoeff mem (n - 1) in let z = pertCoeffNew2 mem (snd w) (weakComps n) in (fst z : fst w, snd z)

pertCoeff :: Matrix -> Matrix -> Matrix -> Int -> ([Float], DHS.HashMap [Int] Matrix)
pertCoeff v p s n = let mem = map (intToMat v p s) [0 ..] in memPertCoeff mem n
