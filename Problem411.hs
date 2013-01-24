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
  max <- findMax v s $ l - 1
  let current = Distance max pos
  MV.unsafeWrite v l $! current
  place v current l
{-# INLINE process #-}

findMax v (Station !pos _) i = findMax' i
 where
  findMax' !i = do
    if i < 0
    then return 0
    else do
      (Distance curr x) <- MV.unsafeRead v i
      if higherThan pos x
      then return $ curr + 1
      else findMax' $ i - 1
{-# INLINE findMax #-}

place :: (PrimMonad m) =>
     MV.MVector (PrimState m) (Distance Int) -> Distance Int -> Int -> m ()
place v max@(Distance !val1 _) i = place' i
 where 
  place' !i = do
    let j = i - 1
    if j < 0
    then return ()
    else do
      curr@(Distance val2 _) <- MV.unsafeRead v j
      if val2 > val1
      then do
        MV.unsafeWrite v j $! max
        MV.unsafeWrite v i $! curr
        place' j
      else return ()
  n = MV.length v
{-# INLINE place #-}

higherThan (Position x1 y1) (Position x2 y2) = (x2 >= x1 && y2 >= y1) --- || (x2 > x1 && y2 >= y1)
{-# INLINE higherThan #-}

lengths n = runST $ do
                  let ss = stations n
                  let l = length ss
                  v <- MV.replicate l $ Distance 0 $ Position 0 0
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

