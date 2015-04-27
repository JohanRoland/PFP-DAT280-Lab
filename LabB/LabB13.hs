{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : LabB13
Description : Parallel implementations of the stock market problem 
              for DAT280 2015
Copyright   : (c) Joacim Andersson, 2015
                  Johan Ekdahl, 2015

Parallel implementations of the stock market problem
-}

import Data.List
import qualified Data.Array.Repa as R
import System.Random
import Data.Random.Normal
import Criterion.Main
import Control.DeepSeq

type IntVec = R.Array R.U R.DIM1 Int
type Index = Int

-- | Main function not using criterion
main :: IO ()
main = do
  t <- buySell rndVec
  putStrLn $ show $ t

-- | Main function for criterion benchmarks
--main = defaultMain
--  [ bench "buySell" (nfIO (buySell rndVec))
--  ]

-- | Creates a list containing random integers
rndInts = map abs $ take 40000 (randoms (mkStdGen 211560155)) :: [Int]

-- | Creates an 'IntVec' containing random integers
rndVec :: IntVec
rndVec = R.fromListUnboxed (R.Z R.:. (40000::Int)) rndInts :: IntVec

-- | Algorithm to solve the stock market problem
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

