
> factor4 :: Integer -> (Integer, Integer)
> factor4 n = factorFrom4 2 n 2

> factorFrom4 :: Integer -> Integer -> Integer -> (Integer, Integer)
> factorFrom4 m n s | r == 0    = (m,q)
>                   | q < m     = (n,1)
>                   | m == 2    = factorFrom4 3 n s
>                   | m == 3    = factorFrom4 5 n s
>                   | otherwise = factorFrom4 (m+s) n (6-s)
>     where (q,r) = n `divMod` m




