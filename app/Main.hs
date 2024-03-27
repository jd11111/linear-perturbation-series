module Main where

import MatMul 
import Data.Array.Unboxed
import PerturbationSeries

main:: IO()
main = do{
  putStrLn "Enter dimension of vector space:";
  in0 <- getLine;
  putStrLn "Enter projection P in row major order:";
  in1 <- getLine;
  putStrLn "Enter perturbation T_1 in row major order:";
  in2 <- getLine;
  putStrLn "Enter reduced resolvent S in row major order:";
  in3 <- getLine;
  putStrLn "Enter desired perturbation order:";
  in4 <-getLine;
  let{
    n = read in0 :: Int;
    pList = read in1 :: [Float];
    vList = read in2 :: [Float];
    sList = read in3 :: [Float];
    order = read in4 :: Int;
    m = n*n-1;
    p = GenMat{int_data = listArray (0,m) pList, rows=n, cols=n};
    v = GenMat{int_data = listArray (0,m) vList, rows=n, cols=n};
    s = GenMat{int_data = listArray (0,m) sList, rows=n, cols=n}}
    in
    putStrLn "Calculated perturbation series coefficients:" >>
    (print . reverse $ fst (pertCoeff v p s order));
}

--[0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,1.0]
--[1.0,-1.0,3.0,-5.0,3.0,2.0,-4.0,-8.0,1.0]
--[1.0/(-3.0-1.0),0.0,0.0,0.0,1.0/(-3.0-4.0),0.0,0.0,0.0,0.0]
--[-0.25,0.0,0.0,0.0,-0.14285714285714285,0.0,0.0,0.0,0.0]
-- - 