module Main where

factorial n = product [1..n]

latticePathsCount n = factorial (2*n) `div` ((factorial n)^2)

main = print . latticePathsCount $ 20
