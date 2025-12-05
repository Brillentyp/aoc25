
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Day4(d4p1, d4p2)
where


import qualified Data.Map as M

data Space =
  Empty |
  Paper Int deriving (Show, Eq) --Int is number of neighbouring paper
-- input model: Map with (x,y) key and Space as value

isPaper :: Space -> Bool
isPaper Empty = False
isPaper (Paper _) = True

addNeigbour :: Space -> Space
addNeigbour (Paper n) = (Paper (n+1))
addNeigbour Empty = Empty

getSurroundings :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
getSurroundings (x,y) (max_x, max_y) = filter (\(lx, ly) -> lx >= 0 && lx <= max_x && ly >= 0 && ly <= max_y) [(x-1, y-1), (x, y-1), (x+1, y-1), (x-1, y), (x + 1, y), (x-1, y+1), (x,y+1), (x+1, y+1)]

populateMap :: [(Int, Int)] -> (Int, Int) -> (M.Map (Int, Int) Space)
populateMap l (max_x, max_y) = M.union(M.fromList (zip l (repeat(Paper 0)))) (M.fromList [((lx,ly), Empty) | lx <- [0..max_x], ly <- [0..max_y]])

processNeighbours :: M.Map (Int, Int) Space -> [(Int, Int)] -> (Int, Int)  -> M.Map (Int, Int) Space
processNeighbours m [] _ = m
processNeighbours m (x:xs) bounds = processNeighbours (an m (getSurroundings x bounds)) (xs) bounds
  where
    an im [] = im
    an im (p:ps) = an (M.adjust addNeigbour p im) ps

parseLine :: String -> [((Int, Int), Space)]
parseLine line = pl line 0
  where
    pl [] _ = []
    pl ('@':xs) i = ((i, 0), (Paper 0)) : (pl xs (i+1)) 
    pl ('.':xs) i = ((i, 0), (Empty)) : (pl xs (i+1)) 
    pl _ _ = error "wtf is happening"

parseInput :: String -> ((Int, Int), M.Map (Int, Int) Space)
parseInput s = let l = filter (/= []) (lines s)
                   parsed_lines = map (parseLine) l
                   parsed_list = concat ( zipWith ((\el y -> map (\((x, _), s) -> ((x,y), s)) el)) parsed_lines [0..])
                    in ((length (head l) - 1, length l - 1), M.fromList parsed_list)
isValidPaper :: Space -> Bool
isValidPaper Empty = False
isValidPaper (Paper n) = n < 4



d4p1 :: String -> Int
d4p1 s = let (bound, m) = parseInput s
             paperList = map (\(k, _) -> k) (filter (\(_, p) -> isPaper p) (M.toList m))
             final_map = processNeighbours m paperList bound in
  length (filter (\(_, p) -> isValidPaper p ) (M.toList final_map))

paperRemover :: Space -> Space
paperRemover Empty = Empty
paperRemover (Paper n) = if n < 4 then Empty else (Paper 0) -- 0 so that we can update the neighbor count after

removePaper :: M.Map (Int, Int) Space -> M.Map (Int, Int) Space
removePaper m = let im = M.map paperRemover m
                    pl = map (\(k, _) -> k) (filter (\(_, p) -> isPaper p) (M.toList m))
  in processNeighbours im pl (fst (M.findMax im))

removeToFinish :: M.Map (Int, Int) Space -> M.Map (Int, Int) Space
removeToFinish m
  | m == nm = m
  | otherwise = removeToFinish nm
  where nm = removePaper m

d4p2 :: String -> Int
d4p2 s = let (bound, m) = parseInput s
             paperList = map (\(k, _) -> k) (filter (\(_, p) -> isPaper p) (M.toList m))
             n_start_paper = length paperList
             start_map = processNeighbours m paperList bound
             final_map = removeToFinish start_map in
  n_start_paper - length (filter (\(_, p) -> isPaper p) (M.toList final_map))


example :: String
example = "..@@.@@@@.\n\
          \@@@.@.@.@@\n\
          \@@@@@.@.@@\n\
          \@.@@@@..@.\n\
          \@@.@@@@.@@\n\
          \.@@@@@@@.@\n\
          \.@.@.@.@@@\n\
          \@.@@@.@@@@\n\
          \.@@@@@@@@.\n\
          \@.@.@@@.@."
