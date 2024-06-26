import System.Random
import MatMul
import PerturbationSeries
import Data.Array.Unboxed

fac :: Integer -> Integer --factorial function
fac 0 = 1
fac 1 = 1
fac n = n * fac (n-1)

falling:: Float -> Integer -> Float
falling _ 0 = 1
falling r k = r * falling (r-1) (k-1)

binomCoeff :: Float -> Integer -> Float -- generalized binomial coefficient
binomCoeff r k = falling r k / fromInteger (fac k)

--exact solution for the coefficients in the perturbation series for
-- the eigenvalue that splits from e1:
pertCoeffExactE1 ::  Float -> Float -> Float -> Integer -> Float 
pertCoeffExactE1 e1 e2 a k = 0.5*(2.0*a)^(2*k)* binomCoeff 0.5 k /((e1 - e2)^(2*k-1))

--exact solution for the coefficients in the perturbation series for
--the eigenvalue that splits from e2:
pertCoeffExactE2 ::  Float -> Float -> Float -> Integer -> Float
pertCoeffExactE2 e1 e2 a k = (-1)*pertCoeffExactE1 e1 e2 a k

n :: Integer
n = 6

closeCheck :: Float -> Float -> Bool
closeCheck f1 f2 = abs (f1 - f2)  <= 0.000001 * (abs f1 + abs f2)

allClose :: [Float] -> [Float] -> Bool
allClose l1 l2 = foldr ((&&) . uncurry closeCheck) True (zip l1 l2)

main :: IO()
main = do{
    gen <- newStdGen;
    let {ns = randoms gen :: [Float];
        e1 = 2.0*(ns !! 1)-1.0;
        e2 = 2.0*(ns !! 2)-1.0;
        a = 2.0*(ns !! 3) -1.0;
        p1 = mkRealMat (listArray (0,3) [1.0,0.0,0.0,0.0]) 2 2 :: MyMatrix Float;
        p2 = mkRealMat (listArray (0,3) [0.0,0.0,0.0,1.0]) 2 2:: MyMatrix Float;
        v = mkRealMat (listArray (0,3) [0.0,a,a,0.0]) 2 2:: MyMatrix Float;
        s1 = mkRealMat (listArray (0,3) [0.0,0.0,0.0,1/(e1 -e2)]) 2 2:: MyMatrix Float;
        s2 = mkRealMat (listArray (0,3) [1/(e2-e1),0.0,0.0,0.0]) 2 2:: MyMatrix Float;
        x = concatMap ((\z-> 0:[z]) . pertCoeffExactE1 e1 e2 a) [1.. n];
        y = concatMap ((\z-> 0:[z]) . pertCoeffExactE2 e1 e2 a) [1..n];
        z1 = reverse $ fst (pertCoeff v p1 s1 (2* fromInteger n));
        z2 = reverse $ fst (pertCoeff v p2 s2 (2* fromInteger n));
        } in if e1 == e2 then print "e1 = e2 so no computation done" else
        print "exact E1:" >> print x
        >> print "calculated:" >> print z1
        >> print "all close?" >> print (allClose x z1)
        >> print "exact E2" >> print y
        >> print "calculated:" >> print z2
        >> print "all close?" >> print (allClose y z2)
    }
