import System.Win32 (COORD(x))


Chapt 1.1
function multiply takes an Int and returns a function, specifically, namely "multiply a", which is a function that will take some Int b and return the value of a*b

> multiply :: Int -> (Int -> Int)
> multiply a b = a*b 

Functional application associates to the left, ie log sin x = (log sin) x
Functional application binds tighter than multiplication

Note: (f . g) x = f (g x)
So, (f . g) x y = f (g x) y != f (g x y)

> h :: Int -> Int -> Int 
> h a b = 2*a + 2*b 

> g :: Int -> Int -> Int
> g a b = a + b

> f :: Int -> Int 
> f x = 2*x 

