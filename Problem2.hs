module Main where

import Control.Applicative

fibs = 0:1:zipWith (+) fibs (tail fibs)

main = print . sum . filter ((== 0) . (`mod` 2)) . takeWhile (< 4000000) $ fibs
