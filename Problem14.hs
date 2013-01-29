module Main where

collatz n | even n = n `div` 2
          | odd  n = 3 * n + 1

collatzList n = takeWhile (> 1) $ iterate collatz n

main = print . maximum . flip zip [1..] . map ( length . collatzList) $ [1..999999]
