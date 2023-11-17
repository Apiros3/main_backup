
Haskell Practical Tasks 1 - 10

Exercise 1 
    factor 0    = factorFrom 2 0
                = (2,0) (where (q,r) = (0,0))
    factor 1    = factorFrom 2 1
                = factorFrom 3 1 (where (q,r) = (0,1))
                = factorFrom 4 1 (where (q,r) = (0,1))
                ...
                = factorFrom 'n' 1 (where (q,r) = (0,1))
        therefore encounters an infinite loop that is never resolved

> factor :: Integer -> (Integer, Integer)
> factor = factorFrom 2

> factorFrom :: Integer -> Integer -> (Integer, Integer)
> factorFrom m n | r == 0    = (m,q)
>                | otherwise = factorFrom (m+1) n
>     where (q,r) = n `divMod` m

Exercise 2
    Running the above functions returns the same result (nothing is returned when we ask for the value of factor 1)

Exercise 3
    We will prove this is by a proof by contradiction.
    Let a be some factor, s.t a|n and sqrt(n) < a < n be the smallest factor of n.
    Then clearly, as n/a is an integer, n/a is another factor of n.
    Rearranging our equation from line 2, 1 < n/a < sqrt(n), is a contradiction as clearly n/a < a.

> factor1 :: Integer -> (Integer, Integer)
> factor1 n = factorFrom1 2 n

> factorFrom1 :: Integer -> Integer -> (Integer, Integer)
> factorFrom1 m n | r == 0    = (m,q)
>                 | n <= m*m  = (n,1)
>                 | otherwise = factorFrom1 (m+1) n
>   where (q,r) = n `divMod` m

factor1 has a time complexity of O(sqrt n)

Exercise 4
    Replacing the test n <= m*m by q < m defines the same function, as q = floor(n/m), whereby substituting this into q shows that the two equations are clearly equivalent for integral values of n.

> factor2 :: Integer -> (Integer, Integer)
> factor2 n = factorFrom2 2 n

> factorFrom2 :: Integer -> Integer -> (Integer, Integer)
> factorFrom2 m n | r == 0    = (m,q)
>                 | q < m  = (n,1)
>                 | otherwise = factorFrom2 (m+1) n
>   where (q,r) = n `divMod` m

Exercise 5 

> factor3 :: Integer -> (Integer, Integer)
> factor3 = factorFrom3 2

> factorFrom3 :: Integer -> Integer -> (Integer, Integer)
> factorFrom3 m n | r == 0    = (m,q)
>                 | q < m     = (n,1)
>                 | m == 2    = factorFrom3 3 n
>                 | otherwise = factorFrom3 (m+2) n
>     where (q,r) = n `divMod` m

Because this function does not have to go through all the even numbers (apart from 2), it is essentially twice as fast, for suitably large values of n.

Exercise 6

Some Tests:
--------------------------------
*Main> factor3 5227226820359
(5227226820359,1)
(4.81 secs, 530,505,424 bytes)

*Main> factor3 1000000007
(1000000007,1)
(0.08 secs, 7,409,952 bytes)

*Main> factor3 998244353
(998244353,1)
(0.07 secs, 7,402,312 bytes)

*Main> factor3 (100003*100003) 
(100003,100003)
(0.23 secs, 23,275,728 bytes)
--------------------------------

Exercise 7

> factor4 :: Integer -> (Integer, Integer)
> factor4 n = factorFrom4 2 n 2

> factorFrom4 :: Integer -> Integer -> Integer -> (Integer, Integer)
> factorFrom4 m n s | r == 0    = (m,q)
>                   | q < m     = (n,1)
>                   | m == 2    = factorFrom4 3 n s
>                   | m == 3    = factorFrom4 5 n s
>                   | otherwise = factorFrom4 (m+s) n (6-s)
>     where (q,r) = n `divMod` m

Some Tests:
--------------------------------
*Main> factor4 5227226820359
(5227226820359,1)
(4.29 secs, 451,244,384 bytes)

*Main> factor4 1000000007 
(1000000007,1)
(0.08 secs, 6,314,264 bytes)

*Main> factor4 998244353      
(998244353,1)
(0.07 secs, 6,307,328 bytes)

*Main> factor4 (100003*100003)
(100003,100003)
(0.19 secs, 19,809,336 bytes)
--------------------------------

Exercise 8
To extend this idea to the prime numbers, we must find all prime numbers until sqrt n, which will take an unneccessary time, computing whether those numbers are prime where instead we could simply just test whether that number (prime or not) divides n.

Exercise 9

> factors :: Integer -> [Integer]
> factors n = factorsFrom 2 n

> factorsFrom :: Integer -> Integer -> [Integer]
> factorsFrom m n | n == 1    = []
>                 | otherwise = p:factorsFrom p q
>     where (p,q) = factorFrom m n

> factors2 :: Integer -> [Integer]
> factors2 = factorsFrom2

> factorsFrom2 :: Integer -> [Integer]
> factorsFrom2 n | n == 1    = []
>                | otherwise = p:factorsFrom2 q
>     where (p,q) = factor4 n

Exercise 9 / 10
Some Tests:
--------------------------------
*Main> factors 100003          
[100003]
(0.23 secs, 36,064,872 bytes)

*Main> factors (100003*73)
[73,100003]
(0.23 secs, 36,070,432 bytes)

*Main> factors (10000000) 
[2,2,2,2,2,2,2,5,5,5,5,5,5,5]
(0.01 secs, 108,096 bytes)

*Main> factors2 100003
[100003]
(0.01 secs, 126,968 bytes)

*Main> factors2 (100003*73)
[73,100003]
(0.01 secs, 147,000 bytes)

*Main> factors2 (100003*100003)
[100003,100003]
(0.20 secs, 19,872,952 bytes)

*Main> map factors [1..2000] == map factors2 [1..2000]
True
(1.27 secs, 192,883,864 bytes)

*Main> factors2 8616460799                            
[89681,96079]
(0.18 secs, 17,831,416 bytes)
--------------------------------

Exercise 11 / 12

> isqrt :: Integer -> Integer
> isqrt = truncate . sqrt . fromInteger

> search :: Integer -> Integer -> Integer -> Integer
> search p q n | r == 0     = p+q
>              | r < 0      = search (p+1) q n
>              | r > 0      = search p (q+1) n 
>       where r = p*p - q*q - n 

Function is guranteed to terminate, as it will pass through all possible values of p, and some q that may accompany it. Then, we must find some (p,q) where p^2 - q^2 - n = 0 at some point, as every number has a factor, specifically, when x = (n+1)/2, y = (n-1)/2
For any given p, we have that while r > 0, we will check through the possible values q can take by incrementing it, and for p = x, we will definitely reach r = 0 at q = y, thus the program must terminate.

When x^2 is strictly smaller than n, as y^2 is non-negative, we clearly have the relationship that x^2 - y^2 - n < 0.
So, we know that x has a relationship that x^2 >= n.

> fermat :: Integer -> (Integer, Integer)
> fermat n = (r, n `div` r)
>       where r = search (isqrt n) 0 n 

Exercise 13

Some Tests 
--------------------------------
*Main> fermat 8616460799
(96079,89681)
(0.02 secs, 1,840,176 bytes)

*Main> fermat 1963272347809
(8123471,241679)
(27.21 secs, 3,501,251,440 bytes)
--------------------------------

Exercise 14

> search2 :: Integer -> Integer -> Integer -> Integer 
> search2 p q r | r == 0        = p+q 
>               | r < 0         = search2 (p+1) q (r+2*p+1)
>               | r > 0         = search2 p (q+1) (r-2*q-1)

> fermat2 :: Integer -> (Integer, Integer)
> fermat2 n = (r, n `div` r)
>       where   t = isqrt n
>               r = search2 t 0 (t*t - n)

Some Tests 
--------------------------------
*Main> fermat2 8616460799
(96079,89681)
(0.03 secs, 1,736,504 bytes)

*Main> fermat2 1963272347809       
(8123471,241679)
(24.51 secs, 3,286,137,656 bytes)

*Main> (isqrt(2^105))^2 - 2^105
5545866846675497
(0.05 secs, 80,840 bytes)

*Main> isqrt(2^2048) == isqrt(2^1024)   
True
(0.02 secs, 71,536 bytes)
--------------------------------

Exercise 15 

> isqrt2 :: Integer -> Integer -> Integer
> isqrt2 k n    | k*k > n   = k-1  
>               | otherwise = isqrt2 (k+1) n 

Assuming m is a non-negative number let m <= sqrt n. Then, n-m^2 = (sqrt n + m)(sqrt n - m) >= 0 <=> sqrt n + m >= 0, which is clearly true as m >= 0 and n >= 0.
If the difference between l and r is an even value, 2k (where k is a positive integer), then (l+r)`div`2 = l+k, which clearly has the relationship l < l+k < r.
If the difference between l and r is an odd value, 2k+1 (where k is a non-negative integer), then (l+r)`div`2 = l+k, which clearly has the relationshp l < l+k < r for k > 0.
However, as k != 0 from the assumption that l+1 != r means that l < (l+r)`div`2 < r for any l < r and l+1 != r. 

Exercise 16 

> split :: (Integer, Integer) -> Integer
> split (l, r) = (l+r)`div`2 

Exercise 17

> isqrt3 :: Integer -> (Integer, Integer) -> Integer 
> isqrt3 n (l,r)    | l+1 == r      = l 
>                   | m*m <= n      = isqrt3 n (m, r)
>                   | otherwise     = isqrt3 n (l, m)
>       where m = split (l,r)

It takes approximately log2 n steps to compute the value of isqrt3 n at most, starting with (l,r) = (0,n), as each step halves the range in which the calculations needs to be done.

Exercise 18

> isqrt4 :: Integer -> Integer -> Integer 
> isqrt4 n b        | b*b >= n    = isqrt3 n (1,b)
>                   | otherwise   = isqrt4 n (2*b)

It takes approximately (log2 (sqrt n)) to find the value of b, and at most, another (log 2 b) calculations to find the value of isqrt3 n. 
However, noting that b >= sqrt n, we have that log (sqrt n) + log b is approximately log n.
Considering the extra effort put in to implement an almost-neglibile change in the speed, it does not seem to be something worth the effort to put, when it could simply be left at isqrt3 
