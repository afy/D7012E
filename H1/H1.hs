-- Smallest K-List
-- Author: Hannes Furhoff, hanfur-0@student.ltu.se



import Data.List
import Data.Tuple


-- Calculate size of subset (sum of elements)
calcSum :: [Int] -> Int
calcSum [] = 0
calcSum (x:xs) 
 | (xs == []) = x
 | otherwise = x + calcSum xs


-- Qsort to insert the subset tuple according to sum (Tuple[0])
insertSubset :: (Int,[Int]) -> [ (Int,[Int]) ] -> [ (Int,[Int]) ]
insertSubset (a,b) v = [xl | xl <- v, fst xl <= a] ++ [(a,b)] ++ [xu | xu <- v, fst xu > a]


-- Iterate through original set and transform to ordered list according to calcsize
transformSubsets :: [ [Int] ] -> [ (Int,[Int]) ]
transformSubsets (x:xs) 
 | (xs == []) = [element]
 | otherwise = insertSubset element (transformSubsets xs)
 where element = (calcSum x, x)


-- Get list of all subsets
getSubsets :: [Int] -> [ [Int] ]
getSubsets v 
 | (v==[]) = []
 | otherwise = [getsublist i v | i <- [1..(length v)] ] ++ getSubsets (drop 1 v)
 where getsublist i v = take i v


getSet :: [Int] -> [ (Int,[Int]) ]
getSet = transformSubsets . getSubsets


-- Return k min sizes
getKset :: [Int] -> Int -> [ (Int,[Int]) ]
getKset v k 
 | (k <= 0) = []
 | (k == 1) = [ head ps ] 
 | otherwise = (getKset v (k-1)) ++ [(ps !! k)]
 where ps = getSet v
