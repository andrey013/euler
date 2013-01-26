{-# LANGUAGE BangPatterns, TemplateHaskell, TypeFamilies, MultiParamTypeClasses #-}

module Main where

import Control.Parallel.Strategies
import Control.Parallel

import System.Environment
import Data.List
import qualified Data.IntSet as S
import qualified Data.IntMap.Strict as M

stationsList n = iterate (\(x, y) -> (2*x `mod` n, 3*y `mod` n)) (1, 1)
{-# INLINE stationsList #-}

fillMapWithoutCycles = flip fillMap' [(0, 0)] . fillMap' M.empty
 where
  fillMap' map []     = map
  fillMap' map ((!a, !b):ss) =
    let (!newSet, oldSize) = case M.lookup a map of
                              Just oldSet -> (S.insert b oldSet, S.size oldSet)
                              Nothing     -> (S.singleton b, 0)
        newSize = S.size newSet
        newMap = M.insert a newSet map
    in if oldSize == newSize
       then map
       else fillMap' newMap ss
{-# INLINE fillMapWithoutCycles #-}

stations :: Int -> M.IntMap S.IntSet
stations n = fillMapWithoutCycles . take (2*n+1) . stationsList $ n
{-# INLINE stations #-}

process :: M.IntMap Int -> Int -> M.IntMap Int
process m y =
  let Just (_, max) = M.lookupLE y m
      newVal = max+1
      m' = M.insert y newVal m
      m'' = case M.lookupGT y m of
              Just (k, v) -> if v>newVal then m' else M.delete k m'
              Nothing     -> m'
  in m''
{-# INLINE process #-}

lengths n =
  let map = M.singleton 0 (-1) 
  in M.foldl' (S.foldl' process) map (stations n)

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
maxPathLength = snd . M.findMax . lengths

main = do
  args <- getArgs
  let n = read $ head args
  print $ sum . parMap rseq maxPathLength . map (^5) $ [1..n]
  --print $ stations $ n^5
