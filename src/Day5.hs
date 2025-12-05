{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Day5(d5p1, d5p2)
where

import Data.Ranged.RangedSet
import Data.Ranged
import Data.List.Split (splitOn)


closedRange :: Ord v => v -> v -> Range v
closedRange a b = Range (BoundaryBelow a) (BoundaryAbove b)

parseInput :: String -> (RSet Int, [Int])
parseInput s = let l = lines s
                   rl = takeWhile (/= []) l
                   il = filter (/= []) (dropWhile (/= []) l) 
  in
    (makeRangedSet (map parseInRange rl), map read il)

parseInRange :: String -> Range Int
parseInRange s = let sl = splitOn "-" s in closedRange (read (head sl)) (read (head (tail sl)))

d5p1 :: String -> Int
d5p1 s = let (rs, l) = parseInput s
  in length (filter id (map (rSetHas rs) l))

closedRangeLen :: Range Int -> Int
closedRangeLen (Range (BoundaryBelow a) (BoundaryAbove b)) = b - a + 1
closedRangeLen _ = error "Only the case above should happen here"

d5p2 :: String -> Int
d5p2 s = let (rs, _) = parseInput s
  in sum (map closedRangeLen (rSetRanges rs))



example :: String
example = "3-5\n\
           \10-14\n\
           \16-20\n\
           \12-18\n\
           \\n\
           \1\n\
           \5\n\
           \8\n\
           \11\n\
           \17\n\
           \32"
