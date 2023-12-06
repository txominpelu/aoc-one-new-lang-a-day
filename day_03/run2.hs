
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
  , symbols :: Set (Integer, Integer, Char)  
  } deriving Show
    
makeNumber chars = 4

validCell numRows numCols (x,y) =
    (x < numCols && x >= 0) && (y < numRows && y >= 0)

adjacentNumbers :: T.Text -> Map (Integer, Integer) Integer -> (Integer, Integer) -> [(Integer, Integer)]
adjacentNumbers matrix cellToNumber (x,y) =
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
    | ongoingNumber /= [] = acc { numbers =  numbers :|> (read ongoingNumber :: Integer), numberIndex = numberIndex + 1, ongoingNumber = [], x = x+1, symbols = Set.insert (x,y,c) symbols }
    | otherwise = acc { x = x+1, symbols = Set.insert (x,y,c) symbols }

buildNumbers :: [Char] -> BuildNumbersData
buildNumbers content = foldl buildNumbersStep BuildNumbersData{ ongoingNumber = [], numberIndex = 0, numbers = Empty, cellToNumber = Map.empty, x = 0, y = 0, symbols = Set.empty } content


main = do
  content <- readFile "input.txt"
  let numbersData = buildNumbers content
  let myNumbers = toList (numbers numbersData)
  let stars = Data.List.filter (\(_,_,c) -> c == '*') (Set.toList (symbols numbersData))
  let cellToNumbers = cellToNumber numbersData
  let starsAndAdjacent = Data.List.map (\(x, y, c) -> (x, y, c, (adjacentNumbers (T.pack content) cellToNumbers (x,y)))) stars
  let starsAndNumbers = Data.List.map (\(x, y, c, adj) -> (x, y, c, adj, (Data.List.nub (map (\(xa,ya) -> Map.findWithDefault (-1) (xa,ya) cellToNumbers) adj)))) starsAndAdjacent
  let starsAndNumbersGears = Data.List.filter (\(x, y, c, adj, numbers) -> (Data.List.length numbers == 2)) starsAndNumbers
  print starsAndNumbersGears
  let gearsNumbersIndexes = map (\(x, y, c, adj, numbers) -> numbers) starsAndNumbersGears
  let gearsNumbers = map (\numbers -> (map (\number -> myNumbers!!(fromIntegral number)) numbers)) gearsNumbersIndexes
  let gearsNumbersTuples = map (\n -> (Data.List.head n, (Data.List.head (Data.List.tail n)))) gearsNumbers
  let gearsNumbersProducts = map (\(n1,n2) -> n1 * n2) gearsNumbersTuples
  print (sum gearsNumbersProducts)

