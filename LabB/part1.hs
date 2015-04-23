{-# LANGUAGE TypeOperators #-}
-- | an example of how repa and our extention can be used together
module Part1 where
import Data.List
import qualified Data.Array.Repa as R

type IntVector = R.Array R.U (R.Z R.:. Int) Integer

stock::[Integer]->Integer
stock (l:[]) = 0
stock (l:ls) = max (stock ls) (calc ls l 0)
    where 
        calc::[Integer]->Integer->Integer->Integer
        calc (l:[]) x y = max (l-x) y  
        calc (l:ls) x y = max (l-x) (calc ls x y) 
        
        
pstock::R.Array R.U R.DIM1 Int->IO ()
pstock ls = do
    
    
    
    return ()
        

        
--runOnTails::R.Array R.U R.DIM1 Int->(Integer,Integer)
--runOnTails  = helper l ls 1 0 0

helper::Integer->[Integer]->Integer->Integer->Integer->(Integer,Integer)
helper f (l:[]) i mi m | l-f > m = (f-l, i)
                     | otherwise = (m, mi)
helper f (l:ls) i mi m | l-f > m = helper f ls (i+1) i (l-f)
                       | otherwise = helper f ls (i+1) mi (m)

                       
                       
                       
                       
eqFun (a , b) (a1 ,b1) | a<a1 = LT
eqFun (a , b) (a1 ,b1) | a>a1 = GT
eqFun (a , b) (a1 ,b1) | a==a1 && b<b1 = LT
eqFun (a , b) (a1 ,b1) | a==a1 && b>b1 = GT
eqFun (a , b) (a1 ,b1) | a==a1 && b==b1 = EQ


