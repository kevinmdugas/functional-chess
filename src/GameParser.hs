module GameParser(parseGameFile) where

import System.IO
import Data.List (cycle)

import Board
import LANParser

parseGameFile :: FilePath -> IO [(Maybe ChessMove, Maybe ChessMove)]
parseGameFile filePath = do
  contents <- readFile filePath
  let moveStrings = filterNonMoves $ words contents
  return $ processMoves moveStrings turns
  -- return $ show moves

processMoves :: [String] -> [ChessColor] -> [(Maybe ChessMove, Maybe ChessMove)]
processMoves moves colors = 
  filter (/= (Nothing, Nothing)) $ -- Filter out tuples equal to (Nothing, Nothing)
  map (\(move, color) -> parseMove move color) $ -- Apply parseMove to each pair
  zip moves colors

turns :: [ChessColor]
turns = cycle [ChessWhite, ChessBlack]

-- Filters out headers (could keep them if desired), empty strings and turn numbers
filterNonMoves :: [String] -> [String]
filterNonMoves [] = []
filterNonMoves (x:xs)
  | x == ""                          = filterNonMoves xs
  | head x `elem` "KQRBNPabcdefgh0O" = x : filterNonMoves xs
  | otherwise                        = filterNonMoves xs

-- getMoveList :: [String] -> [Maybe ChessMove]
-- getMoveList = filter (/= (Nothing, Nothing)) . map parseMove

-- getGameResult

-- filterHeaders :: [String] -> [String]
-- filterHeaders [] = []
-- filterHeaders (x:xs)
--   | x == ""       = filterHeaders xs
--   | head x /= '[' = x : filterHeaders xs
--   | otherwise     = filterHeaders xs

-- filterTurnNums :: String -> String
-- filterTurnNums

-- filterResultDescription :: String -> String
-- filterResultDescription