{-# LANGUAGE BangPatterns, TemplateHaskell, TypeFamilies, MultiParamTypeClasses #-}

module Main where

import Control.Parallel.Strategies
import Control.Parallel
import System.Environment
import Data.List
import Data.Set (toList, fromList)
import qualified Data.IntMap.Strict as M

data Position a = Position {-# UNPACK #-} !a {-# UNPACK #-} !a
  deriving (Eq, Ord, Show)

getY (Position _ y) = y
{-# INLINE getY #-}

xs :: Int -> [Int]
xs n = xs'
 where xs' = 1 : map (\x -> 2*x `mod` n) xs'
{-# INLINE xs #-}

ys :: Int -> [Int]
ys n = ys'
 where ys' = 1 : map (\y -> 3*y `mod` n) ys'
{-# INLINE ys #-}

stations :: Int -> [Int]
stations n = map getY . toList . fromList . ((Position 0 0) :) . take (2*n+1) $ zipWith Position (xs n) (ys n)
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
  let ss = stations n
      m = M.singleton 0 (-1)
  in foldl' process m ss

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
  print $ sum . (parMap rdeepseq) maxPathLength . map (^5) $ [1..n]

