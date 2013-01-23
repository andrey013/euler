module Main where

import Control.Parallel.Strategies
import Control.Parallel
import Control.Monad.ST
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
stations n = V.fromList . reverse . toList . fromList . ((0,0) :) . take (2*n+1) $ zip (xs n) (ys n)

{-
stations :: Int -> [(Int, (Int, Int))]
stations n = reverse . sort . addValue . map head . group . --nub .
               sort . ((0,0) :) . take (2*n+1) $ zip (xs n) (ys n)

addValue :: [(Int, Int)] -> [(Int, (Int, Int))]
addValue ss = [ (value, current)
              | current <- ss
              , let value = length . filter (higherThan current) $ ss
              ]

genPaths :: [(Int, (Int, Int))] -> [[(Int, (Int, Int))]]
genPaths [] = [[]]
genPaths ss = [ current: path
              | let maxValue = fst . head $ ss
              , current <- takeWhile ((== maxValue) . fst) $ ss
              , let higherStations = filter (higherThan (snd current) . snd)
              , path <- genPaths $ higherStations ss
              ]

genLengths :: Int -> [(Int, (Int, Int))] -> [Int]
genLengths acc [] = [acc]
genLengths acc ss = [ length
                  | current <- ss
                  , let higherStations = filter (higherThan (snd current) . snd)
                  , let length = maximum $! genLengths (acc+1) $ higherStations ss
                  ]

genLengths :: [(Int, (Int, Int))] -> [(Int, (Int, Int))]
genLengths = foldr process []
 where
  process (_, s) ss =
    let elements = filter (higherThan s . snd) ss
        length = case elements of
                   [] -> -1
                   --_  -> maximum $ map fst elements
                   _  -> fst $ maximum elements
                   --((l,_):xs)  -> l
    in (length+1, s):ss
-}

genLengths :: V.Vector (Int, Int) -> V.Vector (Int, (Int, Int))
genLengths = V.foldl' process V.empty
 where
  process ss s =
    let --elements = filter (higherThan s) ss
        --length = case elements of
        --           [] -> -1
                   --_  -> maximum $ map fst elements
        --           _  -> fst $ maximum elements
                   --((l,_):xs)  -> l
        length :: Int
        length = V.foldl' (\max (curr, x) -> if (higherThan s x && curr > max) then curr else max) (-1) ss
        new = runST $ do
                v  <- V.unsafeThaw ss
                v' <- MV.unsafeGrow v 1
                MV.write v' (V.length ss) (length+1, s)
                V.unsafeFreeze v'
    in new
 {-V.modify (\v -> do
         v' <- MV.grow v 1
         MV.write v' (MV.length v -1) (length+1, s)
         --V.unsafeFreeze v'
       ) ss-}

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
--maxPathLength = (flip (-) 1) . maximum . map length . genPaths . stations
maxPathLength = fst . V.last . genLengths . stations

--plotStations = GP.plotPathStyle [GP.Key Nothing] (GP.PlotStyle GP.Points $ GP.CustomStyle [GP.PointSize 5]) . map snd . stations

main = do
  args <- getArgs
  let n = read $ head args
  print $ sum . (parMap rdeepseq) maxPathLength . map (^5) $ [1,3..n] ++ [2,4..n]

