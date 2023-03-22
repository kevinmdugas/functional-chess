module GameParser(parseGameFile) where

import System.IO
import Data.List (cycle)

import Board
import LANParser

parseGameFile :: FilePath -> IO [(Maybe ChessMove, Maybe ChessMove)]
parseGameFile filePath = do
  contents <- readFile filePath
  let moveStrings = filterNonMoves $ words contents
  -- let resultString = getGameResult $ words contents
  return $ processMoves moveStrings turns

processMoves :: [String] -> [ChessColor] -> [(Maybe ChessMove, Maybe ChessMove)]
processMoves moves colors = 
  filter (/= (Nothing, Nothing)) $ -- Filter out tuples equal to (Nothing, Nothing)
  zipWith parseMove moves colors   -- Apply parseMove to each pair of elements from moves and colors

turns :: [ChessColor]
turns = cycle [ChessWhite, ChessBlack]

-- Filters out headers (could keep them if desired), empty strings and turn numbers
filterNonMoves :: [String] -> [String]
filterNonMoves [] = []
filterNonMoves (x:xs)
  | x == ""                         = filterNonMoves xs
  | head x `elem` "KQRBNPabcdefghO" = x : filterNonMoves xs
  | otherwise                       = filterNonMoves xs
