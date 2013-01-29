module Main where

import Control.Applicative

main = print . sum . filter ((== 0) . (min <$> (`mod` 3) <*> (`mod` 5))) $ [1..999]
