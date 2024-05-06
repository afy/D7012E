-- Smallest K-List
-- Task: See labH1.pdf. No HOF allowed.
-- Author: Hannes Furhoff, hanfur-0@student.ltu.se

import Data.List
import Data.Tuple


-- Calculate size of subset (sum of elements)
calcSum :: [Int] -> Int
calcSum [] = 0
calcSum (x:xs) 
 | null xs = x
 | otherwise = x + calcSum xs


-- Qsort to insert the subset tuple according to sum (Tuple[0])
insertSubset :: (Int,[Int]) -> [ (Int,[Int]) ] -> [ (Int,[Int]) ]
insertSubset (a,b) v = [xl | xl <- v, fst xl <= a] ++ [(a,b)] ++ [xu | xu <- v, fst xu > a]


-- Iterate through original set and transform to ordered list according to calcsize
transformSubsets :: [ [Int] ] -> [ (Int,[Int]) ]
transformSubsets (x:xs) 
 | null xs = [element]
 | otherwise = insertSubset element (transformSubsets xs)
 where element = (calcSum x, x)


-- Get list of all subsets
getSubsets :: [Int] -> [ [Int] ]
getSubsets v = [ getslice v i j | i <- [0..length v - 1], j <- [i..length v - 1 ] ] 
 where getslice v i j = [ v !! vi | vi <- [i..j] ]

getSet :: [Int] -> [ (Int,[Int]) ]
getSet = transformSubsets . getSubsets


-- Return k min sizes
getKset :: [Int] -> Int -> [ (Int,[Int]) ]
getKset v k = [ ps !! vi | vi <- [0..k-1] ]
 where ps = getSet v