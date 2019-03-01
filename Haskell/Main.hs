import Nonogram

import System.Environment
import Data.List.Split

parseFile :: String -> NonogramInput
parseFile content = NonogramInput rdata cdata
  where
  numbers = map (map (read :: String->Int)) (map (splitOn " ") (splitOn "\n" content))
  [r, c] = head numbers
  rdata = take r (tail numbers)
  cdata = take c $ drop r (tail numbers)

main = do
  args <- getArgs
  content <- readFile (head args)
  let (NonogramInput x y)  = parseFile content
  let result = solveNonogram (NonogramInput x y)
  mapM putStr (map (\x -> show x ++ "\n\n") result)
