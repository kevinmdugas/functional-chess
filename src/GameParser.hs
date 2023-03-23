module GameParser(parseGameFile, getGameResult) where

import Board
import LANParser

parseGameFile :: String -> [(Maybe ChessMove, Maybe ChessMove)]
parseGameFile contents = do
  let moveStrings = filterNonMoves $ words contents
  processMoves moveStrings turns

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

getGameResult :: [String] -> String
getGameResult [] = "Error: No result given"
getGameResult (x:xs)
  | x == ""                            = getGameResult xs
  | x `elem` ["1-0", "1/2-1/2", "0-1"] = x
  | otherwise                          = getGameResult xs