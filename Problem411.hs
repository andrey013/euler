{-# LANGUAGE BangPatterns, TemplateHaskell, TypeFamilies, MultiParamTypeClasses #-}

module Main where

import Control.Parallel.Strategies
import Control.Parallel

import System.Environment
import Data.List
import qualified Data.Set.Unboxed as S
import qualified Data.IntMap.Strict as M

data Position = Position {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  deriving (Eq, Ord, Show)

instance S.US Position where
    data USet Position = PositionTip | PositionBin {-# UNPACK #-} !S.Size {-# UNPACK #-} !Int{-# UNPACK #-} !Int !(S.USet Position) !(S.USet Position)
    view PositionTip = S.Tip
    view (PositionBin s a b l r) = S.Bin s (Position a b) l r
    tip = PositionTip
    bin s (Position a b) = PositionBin s a b

getY (Position _ y) = y
{-# INLINE getY #-}

stationsList n = iterate (\(Position x y) -> Position (2*x `mod` n) (3*y `mod` n)) (Position 1 1)

fillSetWithoutCycles n = fillSet' S.empty 0
 where
  fillSet' set _       []     = set
  fillSet' set oldSize (x@(Position a b):xs)
    | abs (a-b) > (n `div` 4) + 1 = fillSet' set oldSize xs
    | otherwise =
    let newSet = S.insert x set
        newSize = S.size newSet
    in if oldSize == newSize
       then set
       else fillSet' newSet newSize xs
{-

fromList :: [Position] -> S.Set Position
fromList [] = S.empty
fromList [x] = S.singleton x
fromList (x0 : xs0) = fromList' (S.singleton x0) xs0
  where
    fromList' t0 xs = foldlStrict ins t0 1 xs
      where ins t x = S.insert x t
{-# INLINE fromList #-}

foldlStrict :: (S.Set Position -> Position -> S.Set Position) -> S.Set Position -> Int -> [Position] -> S.Set Position
foldlStrict f = go
 where
  go z _       []     = z
  go z oldSize (x:xs) = 
    let z' = f z x
        newSize = S.size z'
    in if oldSize == newSize
       then z
       else z' `seq` go z' newSize xs
{-# INLINE foldlStrict #-}
-}
stations :: Int -> S.USet Position
stations n = S.insert (Position 0 0) . fillSetWithoutCycles n . take (2*n+1) . stationsList $ n
{-# INLINE stations #-}

process :: M.IntMap Int -> Position -> M.IntMap Int
process m (Position _ y) =
  let Just (_, max) = M.lookupLE y m
      newVal = max+1
      m' = M.insert y newVal m
      m'' = case M.lookupGT y m of
              Just (k, v) -> if v>newVal then m' else M.delete k m'
              Nothing     -> m'
  in m''
{-# INLINE process #-}

lengths n =
  let m = M.singleton 0 (-1) 
  in S.foldl' process m (stations n)

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
  print $ sum . parMap rseq maxPathLength . map (^5) $ [n..n]

