import Prelude hiding (scanl, scanl1)
import Control.Parallel
import Criterion.Main
import System.Random


main = defaultMain
  [
  bench "pscan2" (nf (pscan2 op) rndInts),
  bench "scanl1" (nf (scanl1 op) rndInts)
  ]

op :: Integer -> Integer -> Integer
op x y = floor $ (((fromIntegral x)^10^10)+(fromIntegral y))**((1.0/10.0))

--main :: IO ()
--main = putStrLn $ show $ last $ pscan2 (op) rndInts

rndInts = take 100000 (randoms (mkStdGen 211570155)) :: [Integer]

--pscan :: (b -> a -> b) -> b -> [a] -> [b]
pscan f q ls 
  | length ls > 499 = par part2 (pseq part1 (merge f part1 part2))
  | otherwise      = q : (case ls of
                       [] -> []
                       x:xs -> pscan f (f q x) xs) 
  where
    part1 = pscan f q (left ls)
    part2 = pscan f ((head . right) ls) ((tail . right) ls)

pscan2 :: (a -> a -> a) -> [a] -> [a]
pscan2 _ []     = []
pscan2 f q@(x:xs) = pscan f x xs

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

