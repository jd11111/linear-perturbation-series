import Data.Array.Unboxed
import MatMul
import PerturbationSeries

main :: IO ()
main = do
  let n = 3 :: Int
      pList = [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0] :: [Float]
      vList = [1.0, -1.0, 3.0, -5.0, 3.0, 2.0, -4.0, -8.0, 1.0] :: [Float]
      sList = [-0.25, 0.0, 0.0, 0.0, -0.14285714285714285, 0.0, 0.0, 0.0, 0.0] :: [Float]
      order = 13 :: Int
      m = n * n - 1
      p = GenMat {int_data = listArray (0, m) pList, rows = n, cols = n}
      v = GenMat {int_data = listArray (0, m) vList, rows = n, cols = n}
      s = GenMat {int_data = listArray (0, m) sList, rows = n, cols = n}
   in print . reverse $ fst (pertCoeff v p s order)
