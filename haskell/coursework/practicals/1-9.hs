factor :: Integer -> (Integer, Integer)
factor n = factorFrom 2 n

factorFrom :: Integer -> Integer -> (Integer, Integer)
factorFrom m n | r == 0    = (m,q)
               | otherwise = factorFrom (m+1) n
    where (q,r) = n `divMod` m

factors :: Integer -> [Integer]
factors n = factorsFrom 2 n

factorsFrom :: Integer -> Integer -> [Integer]
factorsFrom m n | n == 1    = []
                | otherwise = p:factorsFrom p q
    where (p,q) = factorFrom m n



factor4 :: Integer -> (Integer, Integer)
factor4 n = factorFrom4 2 n 2

factorFrom4 :: Integer -> Integer -> Integer -> (Integer, Integer)
factorFrom4 m n s | r == 0    = (m,q)
                  | q < m     = (n,1)
                  | m == 2    = factorFrom4 3 n s
                  | m == 3    = factorFrom4 5 n s
                  | otherwise = factorFrom4 (m+s) n (6-s)
    where (q,r) = n `divMod` m

factors2 :: Integer -> [Integer]
factors2 n = factorsFrom2 n

factorsFrom2 :: Integer -> [Integer]
factorsFrom2 n | n == 1    = []
               | otherwise = p:factorsFrom2 q
    where (p,q) = factor4 n


