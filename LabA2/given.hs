import Data.Complex
import Data.Array
import Data.Bits
import System.Random
import Data.Random.Normal
import Criterion.Main
import Control.Parallel
import Control.Monad.Par  
import Control.Parallel.Strategies
import Control.DeepSeq

-- file given.hs for use with Lab 1 Part 1 of the Chalmers PFP Course
-- Please write your names in the file if submitting it


main = do 
  x <- sampleBench
  defaultMain
    [
    bench "parSeqfft" (nf (parSeqfft 10) x),
    bench "parfft" (nf (parfft 30) x)
    ]


--main = do -- Best A800M or A1200M with -O2
--  x <- sample
--  (fft x `using` parList rpar) `deepseq` return ()
--  (parSeqfft 10 x) `deepseq` return () -- ParSeq FFT
--  (parfft 30 x) `deepseq` return () -- Par Monad FFT
--  (fft x) `deepseq` return () -- Sequential FFT

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

