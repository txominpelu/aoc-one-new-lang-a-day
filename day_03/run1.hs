
-- An array of existing numbers
-- numbers = array[numbers] 

-- Map from cell to number that it belongs to 
-- cell_to_number = map[cell => number_index] 

-- Numbers that have already been touched by a symbol
-- part_numbers = set[number_index]


-- def build_numbers():
--     ongoing_number = []
--     number_index = 0
--     x = 0
--     y = 0
--     for c in chars:
--          if isEOL(c):
--              x = 0
--              y += 1
--          elif isDigit(c):
--              ongoing_number += c
--              cell_to_number[(x,y)] = number_index 
--          elif not isEmpty(ongoing_number):
--              numbers += makeNumber(ongoing_number)
--              number_index += 1
--              ongoing_number = []
--     return (numbers, cell_to_number)
-- 
-- def adjacent_cells():
--     pass
-- 
-- def part_numbers():
--     (numbers, cell_to_number) = build_numbers()
--     for row in rows:
--         for cell in row.cells:
--             if isSymbol(cell.value):
--                 for adjacent in adjacent_cells(cell.position):
--                     if cells.position in cell_to_number:
--                         part_numbers.add(cell_to_number[])
--     return part_numbers().map(numbers.get) |> multiply


{-# LANGUAGE OverloadedStrings #-}

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Char
import Data.Sequence
import Data.Foldable (toList)
import Data.List
import qualified Data.Text as T
import Data.Set (Set)
import qualified Data.Set as Set


data BuildNumbersData = BuildNumbersData
  { ongoingNumber :: [Char]
  , numberIndex :: Integer
  , numbers  :: Seq Integer
  , cellToNumber  :: Map (Integer,Integer) Integer
  , x  :: Integer
  , y  :: Integer
  , symbols :: Set (Integer, Integer)  
  } deriving Show
    
makeNumber chars = 4

validCell numRows numCols (x,y) =
    (x < numCols && x >= 0) && (y < numRows && y >= 0)

adjacentCells :: T.Text -> Map (Integer, Integer) Integer -> (Integer, Integer) -> [(Integer, Integer)]
adjacentCells matrix cellToNumber (x,y) =
    let textRows = T.splitOn "\n" matrix in
    let rows = map T.unpack textRows in
    let numRows = fromIntegral (Data.List.length rows) in
    let firstCol = Data.List.head rows in
    let numCols = fromIntegral (Data.List.length firstCol) in
    let deltas = Data.List.filter (\(x,y) -> (x,y) /= (0,0)) [(x,y) | x <- [-1, 0, 1], y <- [-1, 0, 1]] in
    let possibleCells = map (\(deltaX, deltaY) -> (x + deltaX, y + deltaY)) deltas in
    let validCells = Data.List.filter (validCell numRows numCols) possibleCells in
    Data.List.filter (\cell -> Map.member cell cellToNumber) validCells

buildNumbersStep :: BuildNumbersData -> Char -> BuildNumbersData
buildNumbersStep acc@BuildNumbersData{ ongoingNumber, numberIndex, numbers, cellToNumber, x, y, symbols } c
    | c == '\n' && ongoingNumber /= [] = acc { x = 0,  y = y + 1, ongoingNumber = [], numbers =  numbers :|> (read ongoingNumber :: Integer), numberIndex = numberIndex + 1 }
    | c == '\n' = acc { x = 0,  y = y + 1, ongoingNumber = [] }
    | isDigit c = acc { ongoingNumber = ongoingNumber ++ [c],  cellToNumber = Map.insert (x,y) numberIndex cellToNumber, x = x+1 }
    | ongoingNumber /= [] && c == '.' = acc { numbers =  numbers :|> (read ongoingNumber :: Integer), numberIndex = numberIndex + 1, ongoingNumber = [], x = x+1 }
    | c == '.' = acc { x = x+1 }
    | ongoingNumber /= [] = acc { numbers =  numbers :|> (read ongoingNumber :: Integer), numberIndex = numberIndex + 1, ongoingNumber = [], x = x+1, symbols = Set.insert (x,y) symbols }
    | otherwise = acc { x = x+1, symbols = Set.insert (x,y) symbols }

buildNumbers :: [Char] -> BuildNumbersData
buildNumbers content = foldl buildNumbersStep BuildNumbersData{ ongoingNumber = [], numberIndex = 0, numbers = Empty, cellToNumber = Map.empty, x = 0, y = 0, symbols = Set.empty } content


main = do
  content <- readFile "input.txt"
  let numbersData = buildNumbers content
  let cellsTouchingSymbols = fmap (adjacentCells (T.pack content) (cellToNumber numbersData)) (Set.toList (symbols numbersData))
  let numberIndexes = map (\cell -> Map.findWithDefault (-1 :: Integer) cell (cellToNumber numbersData)) (concat cellsTouchingSymbols)
  let myNumbers = toList (numbers numbersData)
  let maps = (map (\numberIndex -> myNumbers!!(fromIntegral numberIndex)) (Data.List.nub numberIndexes))
  print (sum maps)

