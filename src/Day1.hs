{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Day1(d1p1, d1p2)
where

parseInput :: String -> [Int]
parseInput = map parse_line . lines

parse_line :: String -> Int
parse_line ('L':xs) =  -(read xs :: Int)
parse_line ('R':xs) = read xs
parse_line [] = 0 -- just in case I somehow get an empty line
parse_line _ = error "Invalid input"

dial_start :: Int
dial_start = 50

dial_num :: Int
dial_num = 100

num_zeros :: [Int] -> Int 
num_zeros input = nz dial_start 0 input -- assumption dial_start != 0
  where
    nz acc_val acc_z (x:xs) =  nz (mod (acc_val + x) dial_num) (if acc_val == 0 then (acc_z+1) else acc_z) xs
    nz acc_val acc_z [] = acc_z + (if acc_val == 0 then 1 else 0)

zero_passes :: [Int] -> Int
zero_passes inp = zp inp dial_start 0
  where
    zp (x:xs) acc_val acc_zero = let s = acc_val + x in zp xs (mod s dial_num) (acc_zero + (zeros acc_val x))
    zp [] _ z = z
    pd100 p r = let v = ((sign r) * (mod (abs r) dial_num) + p) in if (v <= 0 || v >= 100) && p /= 0 then 1 else 0
    zeros p r = (pd100 p r) + (div (abs r) 100)
    sign x = if x < 0 then -1 else 1

d1p1 :: String -> Int
d1p1 = num_zeros . parseInput

d1p2 :: String -> Int
d1p2 = zero_passes . parseInput

example :: String
example =
  "L68\n\
   \L30\n\
   \R48\n\
   \L5\n\
   \R60\n\
   \L55\n\
   \L1\n\
   \L99\n\
   \R14\n\
   \L82\n"
