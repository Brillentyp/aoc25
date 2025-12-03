
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Day3(d3p1, d3p2)
where

import Data.Char

maxNL :: [Int] -> (Int, Int)
maxNL l = mac l (0, 0) 0
  where
    mac [] _ _ = (0,0)
    mac [_] r _ = r 
    mac (x:xs) (cm, mi) i = mac xs (max cm x, if x > cm then i else mi) (i + 1)

maxAfter :: [Int] -> Int -> Int
maxAfter l index = maximum (drop (index + 1) l)

parseChar :: Char -> Int
parseChar c = (ord c) - (ord '0')

parseInput :: String -> [[Int]]
parseInput = map (map (parseChar))  . lines

handleLine :: [Int] -> Int
handleLine l = let (upper, index) = maxNL l in 10 * upper + (maxAfter l index)

d3p1 :: String -> Int
d3p1 = sum . map handleLine . filter (/= [])  . parseInput

maxLN :: [Int] -> Int -> (Int, Int)
maxLN l n = mac (take (length l - n) l) (0,0) 0
  where
    mac [] r _ = r
    mac (x:xs) (cm, mi) i = mac xs (max cm x, if x > cm then i else mi) (i + 1)

pow :: Int -> Int
pow 0 = 1
pow n = 10 * (pow (n-1))

handleLine2 :: [Int] -> Int
handleLine2 l = hla l 0 12
  where
    hla _ r 0 = r
    hla li r n = let (m, i) = maxLN li (n-1) in hla (drop (i+1) li) (r + (m * (pow (n-1)))) (n-1) 

d3p2 :: String -> Int
d3p2 = sum . map handleLine2 . filter (/= [])  . parseInput

example :: String
example = "987654321111111\n811111111111119\n234234234234278\n818181911112111\n"
