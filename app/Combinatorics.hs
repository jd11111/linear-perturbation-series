module Combinatorics where

import Data.Maybe

data Tree = Leaf | Node Tree Tree deriving Show

cartProd :: [a] -> [b] -> [(a, b)]
cartProd l1 l2 = [(x,y) | x <- l1, y <- l2] -- cartesian product function, will be used to generate all combinations of left/right subtrees

treeComb :: ([Tree], [Tree]) -> [Tree]
treeComb (l1, l2) = map (uncurry Node) (cartProd l1 l2) -- given a tuple of list of trees, generate all combinations and create trees by taking the combinations as left/right subtree

treesWithLeafs :: Int -> [Tree]
treesWithLeafs 1 = [Leaf]
treesWithLeafs n = concatMap treeComb x where x = map (\m -> (treesWithLeafs m, treesWithLeafs (n-m)) ) [0.. n-1]

treeToWord :: Tree -> [Bool]
treeToWord Leaf = []
treeToWord (Node t1 t2) = concat [[False],treeToWord t1, [True],treeToWord t2]

accOrSplit :: (a -> a -> Bool) -> a -> [[a]] -> [[a]]
accOrSplit f x l = (x:xs):xss where
  xs:xss = case l of
    (y:_):_ | f x y -> []:l -- if the first element of l is not the empty list and f x y is true, then return []:l which will ultimately lead to the singleton [x] being prepended to l
    _ -> l -- anything else: return l, which will ultimately lead to x being prepended to the first element (list) of l

splitWhen :: (a -> a -> Bool) -> [a] -> [[a]]
splitWhen f = foldr (accOrSplit f) [[]]

whenChange :: Bool -> Bool -> Bool
whenChange True True = False
whenChange _ _ = True

splitAtChange ::  [Bool] -> [[Bool]]
splitAtChange = splitWhen whenChange

starsBarsToComp :: [[Bool]] -> [Int]
starsBarsToComp l =  catMaybes [if x== [False] && y == [False] then Just 0 else if x== [False] then Just (length y) else Nothing  |(x,y) <- zippedL] where zippedL = zip l2 (tail l2) where l2 = [False]:l++[[False]]

weakComps :: Int -> [[Int]]
weakComps n = map (starsBarsToComp . splitAtChange . treeToWord) (treesWithLeafs n)