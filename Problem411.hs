{-# LANGUAGE BangPatterns, TemplateHaskell, TypeFamilies, MultiParamTypeClasses #-}

module Main where

import Control.Parallel.Strategies
import Control.Parallel

import System.Environment
import Data.List
import qualified Data.IntSet as S
import qualified Data.IntMap.Strict as M
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Algorithms.Intro as VA

xsGen :: Int -> [Int]
xsGen n = take (2*n+1) $ iterate (\x -> 2*x `mod` n) 1

ysGen :: Int -> [Int]
ysGen n = take (2*n+1) $ iterate (\y -> 3*y `mod` n) 1

decycle
  :: [Int] -> (Int, Int)
decycle proc = fillMap' M.empty 0 proc
 where
  fillMap' map c [] = undefined
  fillMap' map c (x:xs) =
    let newMap = if c>30 then map else (M.insert x c map)
    in case M.lookup x map of
         Just i  -> (i, c - i)
         Nothing -> fillMap' newMap (c + 1) xs

positions n = (M.fromList $ (0, 0):uniques, xsList, V.fromList yy)
 where
  (offsetx, lengthx) = decycle $ xsGen n
  (offsety, lengthy) = decycle $ ysGen n
  nou = max offsetx offsety
  uniques = take nou $ zip (xsGen n) (ysGen n)
  xx = take lengthx $ drop nou $ xsGen n
  yy = take lengthy $ drop nou $ ysGen n
  xsList = V.modify (VA.sort) $ V.fromList $ zip xx $ cycle [0..lengthy-1]

stations :: Int -> [S.IntSet]
stations n = snd $ V.foldl' (\ (map, l) x -> let (newMap, list) = (createSets x map l) in (newMap, list)) (u, []) xs
 where
  (u, xs, ys) = positions n
  interval = V.length xs
  ysCount = V.length ys

  createSets :: (Int, Int) -> M.IntMap Int -> [S.IntSet] -> (M.IntMap Int, [S.IntSet])
  createSets xx@(x, i) map acc =
    case M.lookupGE 0 map of
      Just (mx, my) -> if mx == x
                       then (M.delete mx map, (S.insert my $ createSet xx) : acc)
                       else if mx < x
                            then createSets xx (M.delete mx map) $ (S.singleton my) : acc
                            else (map, (createSet xx) : acc)
      Nothing       -> (map, (createSet xx) : acc)

  createSet (x, i) = S.fromList .
                     map (V.unsafeIndex ys) .
                     (i :) .
                     takeWhile (/= i) $
                     iterate (\i -> (i + interval) `mod` ysCount) $ (i + interval) `mod` ysCount
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
  in foldr (flip $ S.foldl' process) map (stations n)

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
