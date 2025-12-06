
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Day6(d6p1, d6p2)
where

import Data.Char (ord)
import Data.List.Split (splitOn)

parseChar :: Char -> Int
parseChar c = ord c - (ord '0')

parseLine :: [String] -> [[Int]] -> [[Int]]
parseLine (s:ss) [] = [read s] : parseLine ss []
parseLine [] _ = []
parseLine (s:ss) (n:ns) = ((read s) : n) : parseLine ss ns

parseInput :: String -> ([[Int]], String)
parseInput s = let l = filter (/= []) (lines s)
                   nums =  map (filter (/= []) . splitOn " ") (takeWhile (\x -> not (elem '*' x || elem '+' x)) l)
                   operators = filter (/= ' ') (head (dropWhile (\x -> not (elem '*' x || elem '+' x)) l)) -- only one line of operators
    in (foldr parseLine [] nums, operators)

doOperation :: [Int] -> Char -> Int
doOperation l '+' = sum l
doOperation l '*' = product l
doOperation _ _ = error "invalid operation"

sol :: ([[Int]], String) -> Int
sol (nums, ops) = sum (zipWith doOperation nums ops) 


d6p1 :: String -> Int
d6p1 = sol . parseInput

stuffList :: Int -> [[Int]] -> [[Int]]
stuffList s l = let lens = map length l
                    desired_len = max (length l) (maximum lens)
                    ss = repeat s 
  in zipWith (\list_elem len_elem -> (take (desired_len - len_elem) (ss)) ++ list_elem) l lens

convertNumbers :: [Int] -> [Int]
convertNumbers = id

rowsToColums :: [String] -> [String]
rowsToColums l
  | head l /= [] = map head l : rowsToColums (map tail l)
  | otherwise = []

parseInput2 :: String -> ([[Int]], String)
parseInput2 s = let l = filter (/= []) (lines s)
                    nums = splitOn [[]] (map (filter (/=' ')) (rowsToColums (takeWhile (\x -> not (elem '*' x || elem '+' x)) l)))
                    operators = filter (/= ' ') (head (dropWhile (\x -> not (elem '*' x || elem '+' x)) l)) -- only one line of operators
    in (map (map read) nums, operators)

d6p2 :: String -> Int
d6p2 = sol . parseInput2


example :: String
example = "123 328  51 64 \n\
          \ 45 64  387 23 \n\
          \  6 98  215 314\n\
          \*   +   *   + "

