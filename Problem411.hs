{-# LANGUAGE BangPatterns, TemplateHaskell, TypeFamilies, MultiParamTypeClasses #-}

module Main where

import Control.Parallel.Strategies
import Control.Parallel

import Control.Monad
import Control.Monad.ST
import Control.Monad.Primitive

import System.Environment
import Data.List
--import qualified Data.Set as S
import qualified Data.IntMap.Strict as M

import qualified Data.Vector.Generic
import qualified Data.Vector.Generic.Mutable
import Data.Vector.Unboxed.Deriving
import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed (Unbox)
import qualified Data.Vector.Unboxed.Mutable as MV

import qualified Data.Vector.Algorithms.Intro as H

data Position = Position {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  deriving (Eq, Ord, Show)

derivingUnbox "Position"
  [t| Position -> (Int, Int) |]
  [| \ (Position x y) -> (x, y) |]
  [| \ (x, y) -> Position x y |]

getY (Position _ y) = y
{-# INLINE getY #-}

stationsList = iterate (\(Position x y) -> Position (2*x `mod` n) (3*y `mod` n)) (Position 1 1)

stations :: Int -> V.Vector Position
stations n = V.iterateN (2*n+1) (\(Position x y) -> Position (2*x `mod` n) (3*y `mod` n)) (Position 1 1)
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

unsafeUnique :: (PrimMonad m) =>
  MV.MVector (PrimState m) Position -> Int -> m (MV.MVector (PrimState m) Position)
unsafeUnique v l = do
  initial <- MV.unsafeRead v 0
  unsafeUnique' 1 1 initial
 where
  unsafeUnique' !c !i !last | i == l = return $ MV.unsafeTake c v
  unsafeUnique' !c !i !last = do
    current <- MV.unsafeRead v i
    if current == last
    then unsafeUnique' c (i+1) last
    else do
      MV.unsafeWrite v c $! current
      unsafeUnique' (c+1) (i+1) current

lengths n =
  let m = M.singleton 0 (-1) 
  in V.foldl' process m $ runST $ do
                  let ss = stations n
                      l  = V.length ss
                  v  <- V.unsafeThaw ss
                  v' <- MV.unsafeGrow v 1
                  MV.unsafeWrite v' l (Position 0 0)
                  H.sort v'
                  unsafeUnique v' (l+1) >>= V.unsafeFreeze

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

