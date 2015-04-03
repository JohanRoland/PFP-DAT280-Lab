{-|
Module      : LabA13
Description : Parallel implementations of scan and FFT, for DAT280 2015
Copyright   : (c) Joacim Andersson, 2015
                  Johan Ekdahl, 2015

Parallel implementations of scan and FFT
-}

import Prelude
import Data.Complex
import Data.Array
import Data.Bits
import System.Random
import Data.Random.Normal
import Control.Parallel
import Control.Parallel.Strategies
import Control.Monad.Par 
import Criterion.Main
import System.Random
import Control.DeepSeq

-- Lab A 1.

-- | Main method for scan, using criterion benchmarking
main = defaultMain
  [
  bench "pscanStrat" (nf (pscanStrat op) rndInts)
  , bench "pscanEval" (nf (pscanEval 2 op) rndInts)
  , bench "pscanParSeq" (nf (pscanParSeq 2 op) rndInts)
  ]

-- | Main method for scan, normal run
--main :: IO ()
--main = do 
--  (pscanStrat (op) rndInts) `deepseq` return ()
--  (pscanEval 2 (op) rndInts) `deepseq` return ()
--  (pscanParSeq 2 (op) rndInts) `deepseq` return () 
--  (scanl1 (op) rndInts) `deepseq` return ()  

-- | Creates a list containing random integers
rndInts = take 5000 (randoms (mkStdGen 211570155)) :: [Integer]

-- | Operator for the scans
op :: Integer -> Integer -> Integer
op x y = (fibo 20 x) `deepseq` (x+y)

-- | Fibonacci operator, takes an extra argument to avoid Haskell's memoization
fibo :: Integer -> Integer -> Integer
fibo 0 = \x -> 1
fibo 1 = \x -> 1
fibo n = \x -> fibo (n-2) x + fibo (n-1) x

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
    pmerge f lft rgt = lft ++ runEval (pEvalMap (`f` (last lft)) rgt)

-- | Parallel map for Eval monad
pEvalMap :: (a -> b) -> [a] -> Eval [b]
pEvalMap f [] = return []
pEvalMap f (x:xs) = do
  y  <- rpar (f x)
  ys <- pEvalMap f xs
  return (y:ys)

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
    pmerge f lft rgt = lft ++ (parSeqMap (`f` (last lft)) rgt)

-- | Parallel map using par and pseq
parSeqMap :: (a -> b) -> [a] -> [b]
parSeqMap f [] = []
parSeqMap f (x:xs) = par fun (pseq (parSeqMap f xs) (fun : parSeqMap f xs))
  where
    fun = f x

------------------------------------------------------------------------------

-- | Return left half of a list
left :: [a] -> [a]
left xs  = take ((length xs) `div` 2) xs

-- | Return right half of a list
right :: [a] -> [a]
right xs = drop ((length xs) `div` 2) xs

------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- Lab A 2.

-- | Main method for FFT, using criterion benchmarking
{-
main = do 
  x <- sampleBench
  defaultMain
    [
    bench "parSeqfft" (nf (parSeqfft 10) x),
    bench "parfft" (nf (parfft 30) x)
    ]
-}

-- | Main method for FFT, normal run
--main = do -- Best A800M or A1200M with -O2
--  x <- sample
--  (fft x `using` parList rpar) `deepseq` return ()
--  (parSeqfft 10 x) `deepseq` return () -- ParSeq FFT
--  (parfft 30 x) `deepseq` return () -- Par Monad FFT
--  (fft x) `deepseq` return () -- Sequential FFT

-- | Samples for main methods
sampleBench = generate2DSamplesList 10000 4 5 1.4 1.5
sample = generate2DSamplesList 200000 4 5 1.4 1.5

-- generating input for FFT or DFT. Borrowed from Simon Marlow I believe.
mX, mY, sdX, sdY :: Float
mX = 0
mY = 0
sdX = 0.5
sdY = 1.5    

generate2DSamplesList :: Int           -- number of samples to generate
                  -> Float -> Float    -- X and Y mean
                  -> Float -> Float    -- X and Y standard deviations
                  -> IO [Complex Float]
generate2DSamplesList n mx my sdx sdy = do
  gen <- getStdGen
  let (genx, geny) = split gen
      xsamples = normals' (mx,sdx) genx
      ysamples = normals' (my,sdy) geny
  return $ zipWith (:+) (take n xsamples) ysamples

------------------------------------------------------------------------------
-- Task 1

-- | Implementation using par and pseq
parSeqfft :: Int -> [Complex Float] -> [Complex Float]
parSeqfft d [a] = [a]
parSeqfft 0 as = fft as
parSeqfft d as = par rs (pseq ls (interleave ls rs))
  where
    (cs,ds) = bflyS as
    ls = force $ parSeqfft (d-1) cs
    rs = force $ parSeqfft (d-1) ds

------------------------------------------------------------------------------
-- Task 2

-- | Implementation using the par monad
parfft :: Int -> [Complex Float] -> [Complex Float]
parfft d as = runPar $ parfft' d as

parfft' :: Int -> [Complex Float] -> Par [Complex Float]
parfft' d [a] = return [a]
parfft' 0 as = return $ fft as
parfft' d as = do
  v1 <- spawn ls
  v2 <- spawn rs
  a <- get v1
  b <- get v2
  return $ interleave a b
  where
    (cs,ds) = bflyS as
    ls = parfft' (d-1) cs
    rs = parfft' (d-1) ds

------------------------------------------------------------------------------
-- twiddle factors
tw :: Int -> Int -> Complex Float
tw n k = cis (-2 * pi * fromIntegral k / fromIntegral n)

dft :: [Complex Float] -> [Complex Float]
dft xs = [ sum [ xs!!j * tw n (j*k) | j <- [0..n']] | k <- [0..n']]
  where
    n = length xs
    n' = n-1

-- In case you are wondering, this is the Decimation in Frequency (DIF) 
-- radix 2 Cooley-Tukey FFT

fft :: [Complex Float] -> [Complex Float]
fft [a] = [a]
fft as = interleave ls rs
  where
    (cs,ds) = bflyS as
    ls = fft cs
    rs = fft ds

interleave [] bs = bs
interleave (a:as) bs = a : interleave bs as

bflyS :: [Complex Float] -> ([Complex Float], [Complex Float])
bflyS as = (los,rts)
  where
    (ls,rs) = halve as
    los = zipWith (+) ls rs
    ros = zipWith (-) ls rs
    rts = zipWith (*) ros [tw (length as) i | i <- [0..(length ros) - 1]]


-- missing from original file
halve as = splitAt n' as
  where
    n' = div (length as + 1) 2

