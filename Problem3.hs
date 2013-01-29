module Main where


primes = primes' [2..]
primes' (n:ns) = n:(primes' $ filter ((/= 0) . (`mod` n)) ns)

factors 1  _     = []
factors n (x:xs)
  | x > (ceiling . sqrt . fromIntegral $ n) = n:factors 1 xs
  | otherwise = if (n `mod` x) == 0
                then x:(factors (n `div` x) (x:xs))
                else factors n xs

main = print . factors 600851475143 $ primes
