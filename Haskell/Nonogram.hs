module Nonogram where

import Data.Function
import Data.List
import Data.Maybe

data Cell = Fill | Blank | Unknown deriving Eq

type Line  = [Cell]
type Board = [[Cell]]

newtype NonogramBoard = NonogramBoard Board

instance Show Cell where
  show Fill = "##"
  show Blank = ".."
  show Unknown = "??"

instance Show NonogramBoard where
  show (NonogramBoard board) = dropLast $ concat (map showLine board)
    where
    showLine line = (concat (map show line)) ++ "\n"
    dropLast [] = error "Can't drop last element of empty list"
    dropLast xs = _dropLast [] xs
      where
      _dropLast xs [y] = xs
      _dropLast xs (y:ys) = _dropLast (xs ++ [y]) ys

type LineData = [Int]

data NonogramInput =
  NonogramInput {
    byRow :: [LineData],
    byCol :: [LineData]
  }

row :: NonogramInput -> Int
row input = length $ byRow input

col :: NonogramInput -> Int
col input = length $ byCol input

partitions :: Int -> Int -> [[Int]]
partitions 0 0 = [[]]
partitions n 0 | n /= 0 = []
partitions n 1 = [[n]]
partitions n k | k /= 1 = concat [map (x:) (partitions (n-x) (k-1)) | x <- [0..n]]

lineCandidates :: Int -> LineData -> [Line]
lineCandidates lineLength lineData = lines
  where
  blockNum = length lineData
  blankNum = lineLength - sum lineData

  spaceNum = blockNum + 1
  gapNum = blockNum - 1

  freeBlankNum = blankNum - gapNum

  freeBlankPartitions = partitions freeBlankNum spaceNum
  offset = 0 : (replicate gapNum 1) ++ [0]
  blankPartitions = map (zipWith (+) offset) freeBlankPartitions

  lines = map (generateLine) blankPartitions
  generateLine xs = concat $ zipWith rep (0:lineData) xs
  rep fn bn = (replicate fn Fill) ++ (replicate bn Blank)

(.|.) :: Cell -> Cell -> Cell
(.|.) x y
  | x == y    = x
  | otherwise = Unknown
-- satisfy x or y

(.&.) :: Cell -> Cell -> Maybe Cell
(.&.) Unknown x       = Just x
(.&.) x       Unknown = Just x
(.&.) x y | x == y    = Just x
(.&.) x y | x /= y    = Nothing
-- satisfy x and y

cellSatisfy :: Cell -> Cell -> Bool
cellSatisfy Unknown _     = True
cellSatisfy x y | x == y  = True
cellSatisfy x y | x /= y  = False
-- y satisfies x

solveNonogram :: NonogramInput -> [NonogramBoard]
solveNonogram input = map NonogramBoard (solve rcandss_0 ccandss_0)
  where
  r = row input
  c = col input

  rcandss_0 = map (lineCandidates c) (byRow input) :: [[Line]]
  ccandss_0 = map (lineCandidates r) (byCol input) :: [[Line]]

  solve :: [[Line]] -> [[Line]] -> [Board]
  solve rcandss ccandss =
    if is_candssOkay
      then if mergedBoardIsOkay
        then if isFinished
          then [map concat rcandss_f]
          else if isChanged
            then solve rcandss_f ccandss_f
            else concat $ map (flip solve ccandss_f) (decompose rcandss_f 0)
        else []
      else []
    where
    is_candssOkay = is_rcandssOkay && is_ccandssOkay
      where
      is_rcandssOkay = and $ map isOkay rcandss
      is_ccandssOkay = and $ map isOkay ccandss
      isOkay cands = length cands /= 0

    boardFromRow =  map (foldr1 (zipWith (.|.))) rcandss
    boardFromCol = (map (foldr1 (zipWith (.|.))) ccandss) & transpose

    maybe_mergedBoard  = zipWith (zipWith (.&.)) boardFromRow boardFromCol
    
    mergedBoardIsOkay = and $ map isJust (concat maybe_mergedBoard)

    mergedBoard  = map (map fromJust) maybe_mergedBoard
    mergedBoardT = transpose mergedBoard

    rcandss_f = zipWith filterCands mergedBoard  rcandss :: [[Line]]
    ccandss_f = zipWith filterCands mergedBoardT ccandss :: [[Line]]
    -- filtered

    filterCands :: Line -> [Line] -> [Line]
    filterCands ref cands = filter (satisfy ref) cands

    satisfy :: Line -> Line -> Bool
    satisfy ref cand = and $ zipWith cellSatisfy ref cand

    isFinished = (map length rcandss_f) == (replicate r 1)

    isChanged = is_rcandssChanged || is_ccandssChanged
      where
      is_rcandssChanged = (map length rcandss) /= (map length rcandss_f)
      is_ccandssChanged = (map length ccandss) /= (map length ccandss_f)

    decompose :: [[Line]] -> Int -> [[[Line]]]
    decompose rcandss n
      | length (rcandss !! n) == 1 = decompose rcandss (n+1)
      | otherwise = map (\cand -> front ++ [[cand]] ++ back) (rcandss !! n)
        where
        front = take  n    rcandss
        back  = drop (n+1) rcandss
