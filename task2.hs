import Data.List

infinitePrimes::[Int]
infinitePrimes = erathSieve [2 .. ]
erathSieve (p:xs) = p:erathSieve [x | x <- xs, x `mod` p /= 0]
-- take 21 infinitePrimes

unfoldX::Int -> [Int]
unfoldX = reverse . unfoldr (\x -> if x==0 then Nothing else Just(rem x 2, div x 2))
