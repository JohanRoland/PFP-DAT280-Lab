{-# LANGUAGE TypeOperators #-}
-- | an example of how repa and our extention can be used together
module Part1 where
import Data.List
import qualified Data.Array.Repa as R

type IntVec = R.Array R.U R.DIM1 Int
type Index = Int

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
        
        
pstock::[Int]->IO ()
pstock ls = do
    let i = length ls
    let l  = (tails (ls::[Int])) ::[[Int]]
    --let repLs = R.fromListUnboxed (R.ix1 i) l ::(R.Array R.U R.DIM1 [Int])  
    return ()
        

        
runOnTails::[Integer]->(Integer,Integer)
runOnTails (l:ls) = helper l ls 1 0 0

helper::Integer->[Integer]->Integer->Integer->Integer->(Integer,Integer)
helper f (l:[]) i mi m | l-f > m = (f-l, i)
                     | otherwise = (m, mi)
helper f (l:ls) i mi m | l-f > m = helper f ls (i+1) i (l-f)
                       | otherwise = helper f ls (i+1) mi (m)
