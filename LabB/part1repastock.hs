{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | an example of how repa and our extention can be used together
--module Part1 where
import Data.List
import qualified Data.Array.Repa as R

type IntVec = R.Array R.U R.DIM1 Int
type Index = Int
{-
runRepaStock :: Int
runRepaStock = repastock (R.fromListUnboxed 
               (R.Z R.:. (8::Int)) [0,0,2,9,8,10,1,10] :: IntVec) 0

repastock :: IntVec -> Index -> Int
repastock vec ind
  | R.size (R.extent vec) == (ind+1) = 0
  | otherwise = max (repastock vec (ind+1)) 
                    (calc vec (ind+1) (vec R.! (R.Z R.:. ind)) 0)
  where
    calc :: IntVec -> Index -> Int -> Int -> Int
    calc vec ind x y 
      | R.size (R.extent vec) == (ind+1) = max ((vec R.! (R.Z R.:. ind))-x) y
      | otherwise = max ((vec R.! (R.Z R.:. ind))-x) 
                        (calc vec (ind+1) x y)

stock::[Integer]->Integer
stock (l:[]) = 0
stock (l:ls) = max (stock ls) (calc ls l 0)
    where 
        calc::[Integer]->Integer->Integer->Integer
        calc (l:[]) x y = max (l-x) y  
        calc (l:ls) x y = max (l-x) (calc ls x y) 
-}      

main :: IO ()
main = pstock [1..30000]


--pstock::[Int]->IO ()
pstock ls = do
    let i = length ls
    let l = (tails (ls::[Int])) ::[[Int]]
    (test :: R.Array R.U R.DIM1 (Int, Int)) <- R.computeP ((R.map (runOnTails2 ls)
                (R.fromListUnboxed (R.Z R.:. (i+1::Int)) [0..i] :: IntVec))
                :: R.Array R.D R.DIM1 (Int, Int) )  
    let (x,y) = getMaxIndex $ R.toList test
    let finIn = snd x+y-1
    --let repLs = R.fromListUnboxed (R.ix1 i) l ::(R.Array R.U R.DIM1 [Int])  
    putStrLn $ show (y, finIn, fst x)

getMaxIndex xs = last $ sortBy sortGT $ zip xs [0..]

sortGT ((a1,a11), b1) ((a2,a22), b2)
  | a1 < a2 = LT
  | a1 > a2 = GT
  | a1 == a2 = compare2 b1 b2

compare2 b1 b2
  | b1 > b2 = GT
  | b1 < b2 = LT
  | b1 == b2 = EQ

runOnTails2::[Int]->Int->(Int,Int)
runOnTails2 prices index = helper (prices !! index) (drop index prices) 1 0 0

helper2 index prices i mi m = helper

{-
helper2::Int->[Int]->Int->Int->Int->(Int,Int)
helper2 _ [] _ _ _  = (0,0)
helper2 f (l:[]) i mi m | l-f > m = (l-f, i)
                       | otherwise = (m, mi)
helper2 f (l:ls) i mi m | l-f > m = helper f ls (i+1) i (l-f)
                       | otherwise = helper f ls (i+1) mi (m)
-}
runOnTails::[Int]->(Int,Int)
runOnTails [] = (0,0)
runOnTails (l:ls) = helper l ls 1 0 0 

helper::Int->[Int]->Int->Int->Int->(Int,Int)
helper _ [] _ _ _  = (0,0)
helper f (l:[]) i mi m | l-f > m = (l-f, i)
                       | otherwise = (m, mi)
helper f (l:ls) i mi m | l-f > m = helper f ls (i+1) i (l-f)
                       | otherwise = helper f ls (i+1) mi (m)
