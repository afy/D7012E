-- Smallest K-List
-- Task: See labH1.pdf. No HOF allowed.
-- Author: Hannes Furhoff, hanfur-0@student.ltu.se

import Data.List
import Data.Tuple


-- Override for 3-tuple snd
lastElement :: (a,b,c) -> c
lastElement (_,_,c) = c 


-- Calculate size of subset (sum of elements)
calcSum :: [Int] -> Int
calcSum [] = 0
calcSum (x:xs) 
 | null xs = x
 | otherwise = x + calcSum xs


-- Get list of all subsets
getSubsets :: [Int] -> [ (Int,Int,[Int]) ]
getSubsets v = [ getslice v i j | i <- [0..length v - 1], j <- [i..length v - 1 ] ] 
 where getslice v i j = (i,j, [v !! vi| vi <- [i..j] ] )


-- Qsort to insert the subset tuple according to sum (Tuple[0])
insertSubset :: (Int,Int,[Int]) -> [ (Int,Int,[Int]) ] -> [ (Int,Int,[Int]) ]
insertSubset (i,j,x) v = [xl | xl <- v, calcSum (lastElement xl) <= calcSum x] 
                      ++ [(i,j,x)] 
                      ++ [xu | xu <- v, calcSum (lastElement xu) > calcSum x]


-- Iterate through original set and transform to ordered list according to calcsize
orderSubsets :: [ (Int,Int,[Int]) ] -> [ (Int,Int,[Int]) ]
orderSubsets ((i,j,v):xs) 
 | null xs = [element]
 | otherwise = insertSubset element (orderSubsets xs)
 where element = (i,j,v)


-- Return k min sizes
getKset :: [Int] -> Int -> [ (Int,Int,[Int]) ]
getKset v k = [ ps !! (k-vi-1) | vi <- [0..k-1] ]
 where ps = (orderSubsets . getSubsets) v


-- Final step; convert to printout format
getKstr :: [ (Int,Int,[Int]) ] -> String
getKstr ((i,j,x):xs) 
 | null(xs) = "Size\ti\tj\tList\n" ++ lineform i j x
 | otherwise = getKstr xs ++ lineform i j x
 where lineform i j x = show(calcSum x)++"\t"++show(i+1)++"\t"++show(j+1)++"\t"++show(x)++"\n"


smallestKset xs k = putStr (getKstr (getKset xs k))


-- let atest = [x*(-1)^x | x <- [1..100]]
-- smallestKset atest 15
-- smallestKset [24,-11,-34,42,-24,7,-19,21] 6
-- smallestKset  [3,2,-4,3,2,-5,-2,2,3,-3,2,-5,6,-2,2,3] 8