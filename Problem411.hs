{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Parallel.Strategies
import Control.Parallel
import Control.Monad
import Control.Monad.ST
import Control.Monad.Primitive
import System.Environment
import Data.List
import Data.Set (toList, fromList)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
--import qualified Data.Vector.Algorithms.Intro as AI

--import qualified Graphics.Gnuplot.Simple as GP

xs :: Int -> [Int]
xs n = xs'
 where xs' = 1 : map (\x -> 2*x `mod` n) xs'

ys :: Int -> [Int]
ys n = ys'
 where ys' = 1 : map (\y -> 3*y `mod` n) ys'

stations :: Int -> [(Int, Int)]
stations n = reverse . toList . fromList . ((0,0) :) . take (2*n+1) $ zip (xs n) (ys n)

process :: (PrimMonad m) => MV.MVector (PrimState m) (Int, (Int, Int)) -> ((Int, Int), Int) -> m ()
process v (station, l) = do
  --v' <- MV.unsafeGrow v 1
  --ss <- V.unsafeFreeze v
  max <- maximum $ l - 1
  let current = (max, station)
  MV.unsafeWrite v l current
  place v current l
  --AI.sortBy (flip compare) $ MV.unsafeDrop l v
  --return v
 where --maximum s = V.foldl' (\max (curr, x) -> if (higherThan s x && curr > max) then curr else max) (-1)
       maximum !i = do
         if i < 0
         then return 0
         else do
           (curr, x) <- MV.unsafeRead v i
           if higherThan station x
           then return $ curr + 1
           else maximum $ i - 1
       n = MV.length v
       {-maximum = do
         foldM (\max i -> do
           (curr, x) <- MV.unsafeRead v i
           return $ if (higherThan station x && curr > max) then curr else max) (-1) [0..l-1]-}

place !v !max@(val1,_) !i = place' i
 where 
  place' !i = do
    let j = i - 1
    if j < 0
    then return ()
    else do
      !curr@(!val2, _) <- MV.unsafeRead v j
      if val2 > val1
      then do
        MV.unsafeWrite v j max
        MV.unsafeWrite v i curr
        place' j
      else return ()
  n = MV.length v
{-# INLINE place #-}

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
higherThan (x1, y1) (x2, y2) = (x2 >= x1 && y2 >= y1) --- || (x2 > x1 && y2 >= y1)
{-# INLINE higherThan #-}

lengths n = runST $ do
                  let ss = stations n
                  let l = length ss
                  v <- MV.replicate l (0, (0, 0))
                  mapM_ (process v) $ zip ss $ [0..l-1]
                  V.unsafeFreeze v

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
maxPathLength = fst . V.last . lengths

--plotStations = GP.plotPathStyle [GP.Key Nothing] (GP.PlotStyle GP.Points $ GP.CustomStyle [GP.PointSize 5]) . map snd . stations

main = do
  args <- getArgs
  let n = read $ head args
  print $ sum . (parMap rdeepseq) maxPathLength . map (^5) $ [1..n]

