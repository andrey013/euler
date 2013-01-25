{-# LANGUAGE BangPatterns, TemplateHaskell, TypeFamilies, MultiParamTypeClasses #-}

module Main where

import Control.Parallel.Strategies
import Control.Parallel
import Control.Monad
import Control.Monad.ST
import Control.Monad.Primitive
import System.Environment
import Data.List
import Data.Set (toList, fromList)
import qualified Data.Vector.Generic
import qualified Data.Vector.Generic.Mutable
import Data.Vector.Unboxed.Deriving
import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed (Unbox)
import qualified Data.Vector.Unboxed.Mutable as MV

data Position a = Position {-# UNPACK #-} !a {-# UNPACK #-} !a
  deriving (Eq, Ord, Show)

derivingUnbox "Position"
    [t| (Unbox a) => Position a -> (a, a) |]
    [| \ (Position x y) -> (x, y) |]
    [| \ (x, y) -> Position x y |]

data Station a = Station {-# UNPACK #-} !(Position a) {-# UNPACK #-} !Int
  deriving (Eq, Ord, Show)

derivingUnbox "Station"
    [t| (Unbox a) => Station a -> (Position a, Int) |]
    [| \ (Station x y) -> (x, y) |]
    [| \ (x, y) -> Station x y |]

data Distance a = Distance {-# UNPACK #-} !Int {-# UNPACK #-} !(Position a)
  deriving (Eq, Ord, Show)

derivingUnbox "Distance"
    [t| (Unbox a) => Distance a -> (Int, Position a) |]
    [| \ (Distance x y) -> (x, y) |]
    [| \ (x, y) -> Distance x y |]


distance (Distance dist _) = dist
{-# INLINE distance #-}

xs :: Int -> [Int]
xs n = xs'
 where xs' = 1 : map (\x -> 2*x `mod` n) xs'
{-# INLINE xs #-}

ys :: Int -> [Int]
ys n = ys'
 where ys' = 1 : map (\y -> 3*y `mod` n) ys'
{-# INLINE ys #-}

stations :: Int -> [Position Int]
stations n = reverse . toList . fromList . ((Position 0 0) :) . take (2*n+1) $ zipWith Position (xs n) (ys n)
{-# INLINE stations #-}

process :: (PrimMonad m) => MV.MVector (PrimState m) (Distance Int) -> Station Int -> m ()
process v s@(Station pos l) = do
  (maxDist, maxPos) <- findMax v s $ l - 1
  let current = Distance maxDist pos
  place v l maxPos current
{-# INLINE process #-}

findMax v (Station !pos _) i = findMax' i
 where
  findMax' !i = do
    if i < 0
    then return (0, 0)
    else do
      (Distance curr x) <- MV.unsafeRead v i
      if higherThan pos x
      then return (curr + 1, i)
      else findMax' $ i - 1
{-# INLINE findMax #-}

place :: (PrimMonad m) =>
     MV.MVector (PrimState m) (Distance Int) -> Int -> Int -> Distance Int -> m ()
place v l pos max@(Distance !val1 _) = place' pos
 where
  place' !i = do
    let len = l - i
    Distance val2 _ <- MV.read v i
    if val2 > val1
    then do
      MV.unsafeMove (MV.unsafeTake len . MV.unsafeDrop (i+1) $ v) (MV.unsafeTake len . MV.unsafeDrop (i) $ v)
      MV.write v i $! max
    else place' $ i+1
{-# INLINE place #-}

higherThan (Position x1 y1) (Position x2 y2) = {-(x2 >= x1 && -}y2 >= y1--- ) || (x2 > x1 && y2 >= y1)
{-# INLINE higherThan #-}

lengths n = runST $ do
                  let ss = stations n
                  let l = length ss
                  v <- MV.replicate (l) $ Distance (maxBound) $ Position 0 0
                  mapM_ (process v) $ zipWith Station ss $ [0..l-1]
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
maxPathLength = distance . V.last . lengths

main = do
  args <- getArgs
  let n = read $ head args
  print $ sum . (parMap rdeepseq) maxPathLength . map (^5) $ [1..n]

