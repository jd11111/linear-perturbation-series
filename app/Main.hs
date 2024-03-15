module Main where

import Combinatorics
import Data.Array
import Data.List
import Control.Parallel.Strategies


import qualified Control.Monad.State.Strict as CMSS
import qualified Data.HashMap.Strict as DHS
import qualified Data.Hashable

matMul :: (Ix a, Ix b, Ix c, Num d) => Array (a,b) d -> Array (b,c) d -> Array (a,c) d
matMul x y = array resultBounds [((i,j), sum [x!(i,k) * y!(k,j) | k <- range (lj,uj)])| i <- range (li,ui), j <- range (lj',uj')]
        where ((li,lj),(ui,uj))         =  bounds x
              ((li',lj'),(ui',uj'))     =  bounds y
              resultBounds | (lj,uj)==(li',ui')    =  ((li,lj'),(ui,uj')) | otherwise = error "wrong shape"

trace :: (Ix a, Num d) => Array (a,a) d -> d
trace x = sum [x!(i,i) | i <- range(li,ui)] where ((li,_),(ui,_)) = bounds x 

matPow :: (Ix a, Num d) => Int -> Array (a,a) d -> Array (a,a) d --memoize this
matPow 1 x = x
matPow n x = matMul x (matPow (n-1) x)

memoMatPow :: (Ix b, Num d) => Array (b, b) d -> Int -> Array (b, b) d
memoMatPow x = (map (pow x) [0 ..] !!) where { pow x 1 = x; pow x n = matMul x (memoMatPow x (n-1))}

scaMul :: (Ix a, Num d) => d -> Array (a,a) d -> Array (a,a) d
scaMul alpha x = array ((li,lj),(ui,uj)) [((i,j),alpha*x!(i,j)) | i <- range (li,ui), j <- range (lj,uj)] where ((li,lj),(ui,uj)) = bounds x

matSum :: (Ix a, Num d) => Array (a,a) d -> Array (a,a) d -> Array (a,a) d
matSum x y = array ((li,lj),(ui,uj)) [((i,j),y!(i,j) + x!(i,j)) | i <- range (li,ui), j <- range (lj,uj)] where ((li,lj),(ui,uj)) = bounds x

p:: Array (Int, Int) Double
p = array ((0,0),(2,2)) [((0,0),1.0), ((0,1),0.0),((0,2),0.0), ((1,0),0.0), ((1,1),0.0), ((1,2),0.0), ((2,0),0.0), ((2,1),0.0), ((2,2),0.0)]
v:: Array (Int, Int) Double
v = array ((0,0),(2,2)) [((0,0),1.0), ((0,1),-1.0),((0,2),3.0), ((1,0),-5.0), ((1,1),3.0), ((1,2),2.0), ((2,0),-4.0), ((2,1),-8.0), ((2,2),1.0)]
s:: Array (Int, Int) Double
s = array ((0,0),(2,2)) [((0,0),0.0), ((0,1),0.0),((0,2),0.0), ((1,0),0.0), ((1,1),1.0/(1.0-4.0)), ((1,2),0.0), ((2,0),0.0), ((2,1),0.0), ((2,2),1.0/(1.0+3.0))]

type MyMemo a b = CMSS.State (DHS.HashMap a b) b


memoIntToMat :: Int -> Array (Int, Int) Double
memoIntToMat = (map intToMat [0 ..] !!)

sMemo :: Int -> Array (Int, Int) Double
sMemo = memoMatPow s

intToMat :: Int -> Array (Int, Int) Double 
intToMat 0 = matMul v p
intToMat n = scaMul ((-1.0)^(n+1)) (matMul v (sMemo n))

myMemo :: Data.Hashable.Hashable a => (a -> MyMemo a b) -> a -> MyMemo a b
myMemo f x = CMSS.gets (DHS.lookup x) >>= maybe z return where z = f x >>= (\ y -> CMSS.modify (DHS.insert x y) >> return y)

runMyMemo :: (t -> CMSS.State s a) -> s -> t -> (a, s)
runMyMemo f sta x = CMSS.runState (f x) sta 

weakCombToMatMemo :: [Int] -> MyMemo [Int] (Array (Int, Int) Double)
weakCombToMatMemo (x:[])= return (memoIntToMat x)
weakCombToMatMemo (x:xs) = myMemo weakCombToMatMemo xs >>= \y -> return $ matMul (memoIntToMat x) y

pertCoeffNew :: [[Int]] -> (Double, DHS.HashMap [Int] (Array (Int, Int) Double))
pertCoeffNew [] = (0.0,DHS.empty)
pertCoeffNew (x:xs) = (a,b) where{ a = fst (pertCoeffNew xs) + (trace . fst . runMyMemo weakCombToMatMemo (snd (pertCoeffNew xs))) x; b = snd . runMyMemo weakCombToMatMemo (snd (pertCoeffNew xs)) $ x;}

main:: IO()
main = do{
  --print $ fst ( pertCoeffNew (weakComps 9));
  print $ map (fst . pertCoeffNew . weakComps) (take 11 [0..]);
}