
> factor2 :: Integer -> (Integer, Integer)
> factor2 n = factorFrom2 2 n

> factorFrom2 :: Integer -> Integer -> (Integer, Integer)
> factorFrom2 m n | r == 0    = (m,q)
>                 | q < m  = (n,1)
>                 | otherwise = factorFrom2 (m+1) n
>   where (q,r) = n `divMod` m


