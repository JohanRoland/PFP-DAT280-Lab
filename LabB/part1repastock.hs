{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Data.List
import qualified Data.Array.Repa as R
import System.Random
import Data.Random.Normal
import Criterion.Main
import Control.DeepSeq

type IntVec = R.Array R.U R.DIM1 Int
type Index = Int

-- | Creates a list containing random integers
rndInts = map abs $ take 20000 (randoms (mkStdGen 211560155)) :: [Int]

-- | Creates an 'IntVec' containing random integers
rndVec :: IntVec
rndVec = R.fromListUnboxed (R.Z R.:. (20000::Int)) rndInts :: IntVec

main = defaultMain
  [ bench "buySell" (nfIO (buySell rndVec))
--  , bench "stock" (nfIO ((stock) rndInts))
  ]

--main :: IO ()
--main = do
--  t <- buySell [0,0,7,8,9,10,1,10] -- rndInts -- ..30000]
--  t <- pstock rndVec
--  putStrLn $ show $ t

-- Ta väck senare
--buySell ls = do
--  let i = length ls
--  pstock (R.fromListUnboxed (R.Z R.:. (i::Int)) ls :: IntVec)

buySell :: IntVec -> IO (Int,Int,Int)
buySell ls = do
  let i = R.size (R.extent ls)
  unboxArr :: R.Array R.U R.DIM1 (Int, Int) <- R.computeP (R.map 
    (runOnTails (R.toList ls)) (R.fromListUnboxed (R.Z R.:. (i+1::Int))
    [0..i] :: IntVec) :: R.Array R.D R.DIM1 (Int, Int))  
  let (x,y) = getMaxIndex $ R.toList unboxArr
  let finIn = snd x+y
  return (y, finIn, fst x)

-- | Returns the maximum and the index where it was located
getMaxIndex xs = last $ sortBy sortGT $ zip xs [0..]

-- | Sort ordering to get the latest buy-date instead of the earliest
sortGT ((a1,a11), b1) ((a2,a22), b2)
  | a1 < a2 = LT
  | a1 > a2 = GT
  | a1 == a2 = sortGT2 b1 b2

sortGT2 b1 b2
  | b1 > b2 = GT
  | b1 < b2 = LT
  | b1 == b2 = EQ

-- | Runs 'findMax' on every tail to get the best buy date
runOnTails :: [Int] -> Int -> (Int,Int)
runOnTails prices index = findMax (prices !! index) 
                                  (drop (index+1) prices) 1 0 0

-- | Find the best sell date
findMax :: Int -> [Int] -> Int -> Int -> Int -> (Int, Int)
findMax _ [] _ _ _  = (0,0)
findMax f (l:[]) i mi m 
  | l-f > m   = (l-f, i)
  | otherwise = (m, mi)
findMax f (l:ls) i mi m 
  | l-f > m   = findMax f ls (i+1) i (l-f)
  | otherwise = findMax f ls (i+1) mi (m)



--- Ta väck nedan senare
--- Sequential:
stock :: [Int] -> IO (Int,Int,Int)
stock ls = do
    let i = length ls
    let l = (tails (ls::[Int])) ::[[Int]]
    let (x,y) = getMaxIndex $ map runOnTails2 l
    let finIn = snd x+y
    --let repLs = R.fromListUnboxed (R.ix1 i) l ::(R.Array R.U R.DIM1 [Int])  
    return (y, finIn, fst x)

runOnTails2::[Int]->(Int,Int)
runOnTails2 [] = (0,0)
runOnTails2 (l:ls) = helper l ls 1 0 0 

helper::Int->[Int]->Int->Int->Int->(Int,Int)
helper _ [] _ _ _  = (0,0)
helper f (l:[]) i mi m | l-f > m = (l-f, i)
                       | otherwise = (m, mi)
helper f (l:ls) i mi m | l-f > m = helper f ls (i+1) i (l-f)
                       | otherwise = helper f ls (i+1) mi (m)
