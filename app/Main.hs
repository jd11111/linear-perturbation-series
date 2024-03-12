module Main where

import Combinatorics
import Data.Array
import Control.Parallel.Strategies

matMul :: (Ix a, Ix b, Ix c, Num d) => Array (a,b) d -> Array (b,c) d -> Array (a,c) d
matMul x y = array resultBounds [((i,j), sum [x!(i,k) * y!(k,j) | k <- range (lj,uj)])| i <- range (li,ui), j <- range (lj',uj')]
        where ((li,lj),(ui,uj))         =  bounds x
              ((li',lj'),(ui',uj'))     =  bounds y
              resultBounds | (lj,uj)==(li',ui')    =  ((li,lj'),(ui,uj')) | otherwise = error "wrong shape"

trace :: (Ix a, Num d) => Array (a,a) d -> d
trace x = sum [x!(i,i) | i <- range(li,ui)] where ((li,_),(ui,_)) = bounds x 

matPow :: (Ix a, Num d) => Int -> Array (a,a) d -> Array (a,a) d
matPow 1 x = x
matPow n x = matMul x (matPow (n-1) x)

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


intToMat :: Int -> Array (Int, Int) Double
intToMat 0 = matMul v p
intToMat n = scaMul ((-1.0)^(n+1)) (matMul v (matPow n s))

weakCombToMat :: [Int] -> Array (Int, Int) Double
weakCombToMat (x:[])= intToMat x
weakCombToMat (x:xs) = matMul (intToMat x) (weakCombToMat xs)

--pertCoeff :: Int -> C
pertCoeff n = trace $ foldr1 matSum (map weakCombToMat (weakComps (n+1)))

main:: IO()
main = do{
  print $ parMap rdeepseq pertCoeff (take 13 [0..]);
}