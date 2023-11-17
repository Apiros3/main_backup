
> factor1 :: Integer -> (Integer, Integer)
> factor1 n = factorFrom1 2 n

> factorFrom1 :: Integer -> Integer -> (Integer, Integer)
> factorFrom1 m n | r == 0    = (m,q)
>                 | n <= m*m  = (n,1)
>                 | otherwise = factorFrom1 (m+1) n
>   where (q,r) = n `divMod` m


