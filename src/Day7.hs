{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Day7(d7p1, d7p2)
where

import qualified Data.Set as S
import qualified Data.Map as M
import Data.List (elemIndex)
import Data.Maybe (fromJust)

parseLine :: String -> S.Set Int
parseLine s = S.fromAscList (map fst (filter (\x -> (snd x) == '^') (zip [0..] s)))

parseInput :: String -> ([S.Set Int], Int)
parseInput s = let l = filter (/= []) (lines s)
                   is = fromJust (elemIndex 'S' (head l))
  in
    (map parseLine (tail l), is)

numberSplits :: Int -> [S.Set Int] -> Int
numberSplits start sets = nsac (S.singleton start) sets 0
  where
    nsac _ [] r = r
    nsac currentBeams (l:ls) split_acc = let is = S.intersection currentBeams l
                                             newElements = S.fromList (concatMap (\x -> [x-1, x+1]) (S.elems is)) -- Note that elements outside of the bounds may be added
      in nsac (S.union (S.difference currentBeams is) newElements) ls (split_acc + S.size is)


numPaths :: Int -> [S.Set Int] -> Int
numPaths start sets = descentPath start sets

levelsBelow :: [S.Set Int] -> [S.Set Int]
levelsBelow = id

descentPath :: Int -> [S.Set Int] -> Int
descentPath _ [] = 1
descentPath source (l:ls) = if S.member source l
                              then
                                descentPath (source - 1) ls + descentPath (source + 1) ls
                              else
                                 descentPath source ls
numPathsSmarter :: Int -> [S.Set Int] -> Int
numPathsSmarter start sets = snd (descentPathSmarter start (zip [0..] sets) M.empty)

descentPathSmarter :: Int -> [(Int, S.Set Int)] -> M.Map (Int, Int) Int -> (M.Map (Int, Int) Int, Int)
descentPathSmarter _ [] _ = error "This should never happen"
descentPathSmarter source [(i, _)] m = (M.insert (source, i) 1 m, 1)
descentPathSmarter source ((li, lm):ls) m  = let (dm1m, dm1v) = descentPathSmarter (source - 1) ls m
                                                 (dp1m, dp1v) = descentPathSmarter (source + 1) ls dm1m
                                                 splitSum = dm1v + dp1v
                                                 key = (source, li)
                            in
                            if M.member key m
                              then
                                (m, fromJust (M.lookup key m))
                              else
                                if S.member source lm
                                then
                                  (M.insert key splitSum dp1m, splitSum)
                                
                                else
                                   descentPathSmarter source ls m

d7p1 :: String -> Int
d7p1 s = let (maps, start) = parseInput s in numberSplits start maps

d7p2 :: String -> Int
d7p2 s = let (maps, start) = parseInput s in numPathsSmarter start maps

example :: String
example = ".......S.......\n\
          \.......|.......\n\
          \......|^|......\n\
          \......|.|......\n\
          \.....|^|^|.....\n\
          \.....|.|.|.....\n\
          \....|^|^|^|....\n\
          \....|.|.|.|....\n\
          \...|^|^|||^|...\n\
          \...|.|.|||.|...\n\
          \..|^|^|||^|^|..\n\
          \..|.|.|||.|.|..\n\
          \.|^|||^||.||^|.\n\
          \.|.|||.||.||.|.\n\
          \|^|^|^|^|^|||^|\n\
          \|.|.|.|.|.|||.|"
