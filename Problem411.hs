module Main where

import Control.Parallel.Strategies
import Control.Parallel
import Control.Monad
import Control.Monad.ST
import Control.Monad.Primitive
import System.Environment
import Data.List
import Data.Set (toList, fromList)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV

--import qualified Graphics.Gnuplot.Simple as GP

xs :: Int -> [Int]
xs n = xs'
 where xs' = 1 : map (\x -> 2*x `mod` n) xs'

ys :: Int -> [Int]
ys n = ys'
 where ys' = 1 : map (\y -> 3*y `mod` n) ys'

stations :: Int -> V.Vector (Int, Int)
stations n = V.reverse . V.fromList . toList . fromList . ((0,0) :) . take (2*n+1) $ zip (xs n) (ys n)

process :: (PrimMonad m) => (MV.MVector (PrimState m) (Int, (Int, Int)), Int) -> (Int, Int) -> m ((MV.MVector (PrimState m) (Int, (Int, Int))), Int)
process (v, l) station = do
  --v' <- MV.unsafeGrow v 1
  ss <- V.unsafeFreeze v
  let max = maximum station ss
  MV.unsafeWrite v l (max + 1, station)
  return (v, l+1)
 where maximum s = V.foldl' (\max (curr, x) -> if (higherThan s x && curr > max) then curr else max) (-1)
       --n = MV.length v
       {-maximum = do
         foldM (\max i -> do
           (curr, x) <- MV.unsafeRead v i
           return $ if (higherThan station x && curr > max) then curr else max) (-1) [0..l-1]-}

{-
genLengths :: V.Vector (Int, Int) -> V.Vector (Int, (Int, Int))
genLengths = V.foldl' process V.empty
 where
  process ss s =
    let
        length :: Int
        length = V.foldl' (\max (curr, x) -> if (higherThan s x && curr > max) then curr else max) (-1) ss
        new = runST $ do
                v  <- V.unsafeThaw ss
                v' <- MV.unsafeGrow v 1
                MV.write v' (V.length ss) (length+1, s)
                V.unsafeFreeze v'
    in new
-}
higherThan (x1, y1) (x2, y2) = (x2 >= x1 && y2 > y1) || (x2 > x1 && y2 >= y1)

-- |
-- 
-- >>> maxPathLength 22
-- 5
-- 
-- >>> maxPathLength 123
-- 14
-- 
-- >>> maxPathLength 10000
-- 48
-- 
maxPathLength :: Int -> Int
--maxPathLength = fst . V.last . genLengths . stations
maxPathLength n = fst . V.last $ runST $ do
                  let ss = stations n
                  v <- MV.replicate (V.length ss) (0, (0, 0))
                  (mv, l) <- V.foldM' process (v, 0) ss
                  V.unsafeFreeze mv
--                  return $ MV.unsafeRead mv 0

--plotStations = GP.plotPathStyle [GP.Key Nothing] (GP.PlotStyle GP.Points $ GP.CustomStyle [GP.PointSize 5]) . map snd . stations

main = do
  args <- getArgs
  let n = read $ head args
  print $ sum . (parMap rdeepseq) maxPathLength . map (^5) $ [1,3..n] ++ [2,4..n]

