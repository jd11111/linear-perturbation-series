module Main where

import MatMul 
import Data.Array.Unboxed
import PerturbationSeries

main:: IO()
main = do{
  let{
  p = GenMat{int_data = listArray (0,8) [0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,1.0], rows=3, cols=3};
  v = GenMat{int_data = listArray (0,8) [1.0,-1.0,3.0,-5.0,3.0,2.0,-4.0,-8.0,1.0], rows=3, cols=3};
  s = GenMat{int_data = listArray (0,8) [1.0/(-3.0-1.0),0.0,0.0,0.0,1.0/(-3.0-4.0),0.0,0.0,0.0,0.0], rows=3, cols=3}}
  in print . reverse $ fst (pertCoeff v p s 9);
}