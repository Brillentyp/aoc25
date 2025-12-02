{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Day2(d2p1, d2p2)
where

import Data.List.Split (splitOn)



num_digits :: Int -> Int
num_digits d = if d <= 0 then 0 else 1 + (num_digits (div d 10))

pow :: Int -> Int
pow 0 = 1
pow x = 10 * pow (x - 1)

parseInput :: String -> [[Int]]
parseInput input = let ranges = splitOn "," input in map parseRange ranges
  where parseRange s = let se = splitOn "-" s in [(read (se !! 0)).. (read (se !! 1))]

isInvalid :: Int -> Bool
isInvalid x = let d = num_digits x
                  pd = pow (div d 2)
                    in even d && (div x pd) == (mod x pd) 

d2p1 :: String -> Int
d2p1 = sum . concatMap (filter isInvalid) . parseInput 



possiblePatternSizes :: Int -> [Int]
possiblePatternSizes x = let d = num_digits x in filter (\n -> mod d n == 0) [1..(d-1)]

multiplesOf :: Int -> [Int]
multiplesOf x = [k * x | k <- [0..]]

allSame :: [Int] -> Bool
allSame [] = True
allSame [_] = True
allSame (x:y:xs) = x == y && allSame (y:xs)

-- not sure if single digits (1-9) should be considered invalid. But none of those seem to be in my input so ¯\_(ツ)_/¯
checkForPatternOfSize :: Int -> Int -> Bool
checkForPatternOfSize x s = if s >= num_digits x then False else allSame (map (\n -> (mod (div x (pow n)) (pow s))) (takeWhile (< (num_digits x)) (multiplesOf s)) )

isInvalid2 :: Int -> Bool
isInvalid2 x = any (checkForPatternOfSize x) (possiblePatternSizes x)


d2p2 :: String -> Int
d2p2 = sum . concatMap (filter isInvalid2) . parseInput 

example :: String
example = "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124"


