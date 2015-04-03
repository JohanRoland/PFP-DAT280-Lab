import Prelude 
import Control.Parallel
import Control.Parallel.Strategies
import Criterion.Main
import System.Random
import Control.DeepSeq

-- | Main method using criterion benchmarking
{-
main = defaultMain
  [
  bench "pscanStrat" (nf (pscanStrat op) rndInts),
  bench "pscanEval" (nf (pscanEval 15 op) rndInts),
  bench "pscanParSeq" (nf (pscanParSeq 15 op) rndInts),
  bench "scanl1" (nf (scanl1 op) rndInts)
  ]
-}

main :: IO ()
main = do 
  (pscanStrat (op) rndInts) `deepseq` return ()
--  (pscanEval 15 (op) rndInts) `deepseq` return ()
--  (pscanParSeq 15 (op) rndInts) `deepseq` return () 
--  (scanl1 (op) rndInts) `deepseq` return ()  

-- | Creates a list containing random integers
rndInts = take 1200 (randoms (mkStdGen 211570155)) :: [Integer]

-- | Operator for the scans
op :: Integer -> Integer -> Integer
op x y = (fibo 20 x) `deepseq` (x+y)

-- | Fibonacci operator, takes an extra argument to avoid Haskell's memoization
fibo :: Integer -> Integer -> Integer
fibo 0 _ = 1
fibo 1 _ = 1
fibo n x = fibo (n-2) x + fibo (n-1) x

------------------------------------------------------------------------------

-- | Implementation using a strategy
pscanStrat f xs = scanl1 f xs `using` parListChunk 10 rseq

------------------------------------------------------------------------------

-- | Implementation using the Eval monad
pscanEval :: (NFData a) => Int -> (a -> a -> a) -> [a] -> [a]
pscanEval d _ []     = []
pscanEval d f (x:xs) = pscanEval1 d f x xs

pscanEval1 0 f q ls = force $   scanl f q ls
pscanEval1 d f q ls
  | length ls > 2 = runEval $ do
                        a <- rpar part2
                        b <- rpar part1
                        return $ pmerge f b a
  | otherwise = scanl f q ls -- To prevent tail of empty list
  where
    part1 = pscanEval1 (d-1) f q (left ls)
    part2 = pscanEval1 (d-1) f ((head . right) ls) ((tail . right) ls)

------------------------------------------------------------------------------

-- | Implementation using par and seq
pscanParSeq :: (NFData a) => Int -> (a -> a -> a) -> [a] -> [a]
pscanParSeq d _ []     = []
pscanParSeq d f q@(x:xs) = pscanParSeq1 d f x xs

pscanParSeq1 0 f q ls = scanl f q ls
pscanParSeq1 d f q ls 
  | length ls > 2 = par part2 (pseq part1 (pmerge f part1 part2))
  | otherwise     = scanl f q ls -- To prevent tail of empty list
  where
    part1 = pscanParSeq1 (d-1) f q (left ls)
    part2 = pscanParSeq1 (d-1) f ((head . right) ls) ((tail . right) ls)

------------------------------------------------------------------------------

-- | Parallel merge, using parMap
pmerge f lft rgt = lft ++ mm
  where
    mm = parMap rdeepseq (`f` (last lft)) rgt 

-- | Return left half of a list
left :: [a] -> [a]
left xs  = take ((length xs) `div` 2) xs

-- | Return right half of a list
right :: [a] -> [a]
right xs = drop ((length xs) `div` 2) xs

