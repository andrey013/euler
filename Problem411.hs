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
{-
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
-}
xs :: Int -> [Int]
xs n = xs'
 where xs' = 1 : map (\x -> 2*x `mod` n) xs'
{-# INLINE xs #-}

ys :: Int -> [Int]
ys n = ys'
 where ys' = 1 : map (\y -> 3*y `mod` n) ys'
{-# INLINE ys #-}

stations :: Int -> [Position Int]
stations n = toList . fromList . ((Position 0 0) :) . take (2*n+1) $ zipWith Position (xs n) (ys n)
{-# INLINE stations #-}

process :: (PrimMonad m) => MV.MVector (PrimState m) Int -> Position Int -> m ()
process v (Position _ y) = do
  val <- findMax v y
  MV.unsafeWrite v y (val+1)
  --updatePath v next
{-# INLINE process #-}


findMax :: (PrimMonad m) => MV.MVector (PrimState m) Int -> Int -> m Int
findMax v y = findMax' y 0
 where
  findMax' (-1) max = return max
  findMax' !i max = do
      val <- MV.unsafeRead v i
      if val > max
      then findMax' (i - 1) val
      else findMax' (i - 1) max
{-# INLINE findMax #-}

lengths n = runST $ do
                  let ss = stations n
                  v <- MV.replicate (n+1) 0
                  mapM_ (process v) ss
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
maxPathLength = (flip (-) 1) . V.maximum . lengths

main = do
  args <- getArgs
  let n = read $ head args
  print $ sum . (parMap rdeepseq) maxPathLength . map (^5) $ [1..n]

