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

getGameResult :: [String] -> String
getGameResult [] = "Error: No result given"
getGameResult (x:xs)
  | x == ""                            = getGameResult xs
  | x `elem` ["1-0", "1/2-1/2", "0-1"] = x
  | otherwise                          = getGameResult xs