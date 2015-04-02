import Prelude hiding (scanl, scanl1)
import Control.Parallel
import Control.Parallel.Strategies
import Criterion.Main
import System.Random
import Control.DeepSeq

--main = defaultMain
--  [
--  bench "pscanEval" (nf (pscanEval op) rndInts),
--  bench "scanl1" (nf (scanl1 op) rndInts)
--  ]
fib :: Integer -> Integer -> Integer
fib 0 _ = 0
fib 1 _ = 1
fib n x = (fib n-1 x) + (fib n-2 x)

op :: Integer -> Integer -> Integer
op x y =  deepseq (fib 10 x) (x+y)

main :: IO ()
main = do
  deepseq  (pscanEval 10 (op) rndInts) return ()
  --putStrLn $ show $ last $ pscanEval (+) rndInts 
  --deepseq (scanl1 (+) rndInts) return ()  
   
rndInts = take 50000 (randoms (mkStdGen 211570155)) :: [Integer]

pscanEval :: NFData a => Int -> (a -> a -> a) -> [a] -> [a]
pscanEval d _ []     = []
pscanEval d f q@(x:xs) = pscanEval1 d f x xs

pscanEval1 0 f q ls = force $   scanl f q ls
pscanEval1 d f q ls
  | length ls > 2 = runEval $ do
                        a <- rpar part2
                        b <- rpar part1                        
                        return $  pmerge f b a
  | otherwise = scanl f q ls -- To prevent tail of empty list
  where
    part1 = force $  pscanEval1 (d-1) f q (left ls)
    part2 = force $  pscanEval1 (d-1) f ((head . right) ls) ((tail . right) ls)

{-
pscanParSeq :: Int -> (a -> a -> a) -> [a] -> [a]
pscanParSeq d _ []     = []
pscanParSeq d f q@(x:xs) = pscanParSeq1 d f x xs
-}
--pscan :: (b -> a -> b) -> b -> [a] -> [b]
pscanParSeq1 0 f q ls = scanl f q ls
pscanParSeq1 d f q ls 
  | length ls > 2 = par part2 (pseq part1 (pmerge f part1 part2))
  | otherwise     = scanl f q ls -- To prevent tail of empty list
  where
    part1 = pscanParSeq1 (d-1) f q (left ls)
    part2 = pscanParSeq1 (d-1) f ((head . right) ls) ((tail . right) ls)

pmerge f lft rgt = lft ++ mm
  where
    mm = map (`f` (last lft)) rgt `using` parListChunk 100 rdeepseq
merge f lft rgt = lft ++ (map (`f` (last lft)) rgt)

left :: [a] -> [a]
left xs  = take ((length xs) `div` 2) xs
right :: [a] -> [a]
right xs = drop ((length xs) `div` 2) xs




-- ORIGINAL:

scanl                   :: (b -> a -> b) -> b -> [a] -> [b]
scanl f q ls            =  q : (case ls of
                                []   -> []
                                x:xs -> scanl f (f q x) xs)

-- | 'scanl1' is a variant of 'scanl' that has no starting value argument:
-- 
-- > scanl1 f [x1, x2, ...] == [x1, x1 `f` x2, ...]

scanl1                  :: (a -> a -> a) -> [a] -> [a]
scanl1 f (x:xs)         =  scanl f x xs
scanl1 _ []             =  []

