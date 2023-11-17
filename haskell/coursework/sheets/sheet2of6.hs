song :: Int -> IO()
song n = putStr (song1 n)

song1 :: Int -> String
song1 0 = ""
song1 n = song1 (n-1) ++ "\n" ++ verse n

verse :: Int -> String
verse n = line1 n ++ line ++ line32 n ++ line

line1 :: Int -> String
line1 1 = "One man went to mov\n"
line1 n = units!!(n-1) ++ " men went to mov\n"
line3, line32 :: Int -> String
line3 1 = "one man and his dog\n"
line3 n = unit!!(n-1) ++ " men, " ++ line3 (n-1) 
line32 1 = "One man and his dog\n"
line32 n = units!!(n-1) ++ " men, " ++ line3 (n-1)

line :: String
line = "Went to mov a meadow\n"

unit, units :: [String]
unit = ["one","two","three","four","five","six","seven","eight","nine","ten"]
units = ["One","Two","Three","Four","Five","Six","Seven","Eight","Nine","Ten"]
