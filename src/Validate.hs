module Validate (validate) where

import Board
import State

import Data.Maybe

data Direction = H | V | DQ1 | DQ2 | DQ3 | DQ4 deriving (Eq)
type Vector = [Maybe Piece]

-- Validate a standard move, a castle or return false
validate :: (Maybe ChessMove, Maybe ChessMove) -> ChessColor -> GameState -> Bool
validate (Just standardMove, Nothing) player state = 
  validateStandard standardMove player state
validate (Just kingMove, Just rookMove) player state =
  validateCastle (kingMove, rookMove) player state
validate (_, _) _ _ = False

-- Verify piece at start position matches the piece returned from the parser,
-- the end position is not occupied by the current player's piece, and perform
-- piece-specific validation.
validateStandard :: ChessMove -> ChessColor -> GameState -> Bool
validateStandard (expectedStart, start, end) player state = do
  let actualStart = getPiece state start
  let actualEnd = getPiece state end
  validStartPos expectedStart actualStart &&
    validEndPos player actualEnd && 
    ( case ptype expectedStart of
        P -> validatePawn player start end state
        R -> validateHorVert start end state
        B -> validateDiag start end state
        Q -> validateDiag start end state || validateHorVert start end state
        N -> validateKnight start end
        K -> validateKing start end )

-- Make sure castling is valid given the specified two moves, the current player,
-- and gamestate.
validateCastle :: (ChessMove, ChessMove) -> ChessColor -> GameState -> Bool
validateCastle ((k, ks, ke), (r, rs, re)) player state = do
  let actualK = getPiece state ks
  let actualR = getPiece state rs
  validStartPos k actualK && firstMove actualK &&
    validStartPos r actualR && firstMove actualR &&
    validEndPos player (getPiece state ke) &&
    validEndPos player (getPiece state re)

-- Verify pawn move is in the player's direction and a valid space
validatePawn :: ChessColor -> Pos -> Pos -> GameState -> Bool
validatePawn player (sr, sc) end state 
  -- Move up one space
  | (player == ChessBlack && end == (sr+1, sc)) ||
    (player == ChessWhite && end == (sr-1, sc)) = isNothing (getPiece state end)
  -- Move up two spaces
  | (player == ChessBlack && end == (sr+2, sc)) ||
    (player == ChessWhite && end == (sr-2, sc))  =
      firstMove (getPiece state (sr, sc)) && 
      isClearPath (createVector (Just V) (sr,sc) end state)
  -- Diagonal capture
  | (player == ChessBlack && (end == (sr+1, sc+1) || end == (sr+1, sc-1))) ||
    (player == ChessWhite && (end == (sr-1, sc-1) || end == (sr-1, sc+1))) =
      isOppPiece (getPiece state end) player
  | otherwise = False

-- Verify the end position is within the knight's radius
validateKnight :: Pos -> Pos -> Bool
validateKnight start end = inPath end (knightRadius start)

-- Possible end positions relative to a knight's starting position
knightRadius :: Pos -> [Pos]
knightRadius (sr, sc) = [
    (sr-2, sc+1), (sr-1, sc+2),   -- Q1
    (sr-2, sc-1), (sr-1, sc-2),   -- Q2
    (sr+2, sc-1), (sr+1, sc-2),   -- Q3
    (sr+2, sc+1), (sr+1, sc+2)    -- Q4
  ]

-- Verify the end position is within the king's radius
validateKing :: Pos -> Pos -> Bool
validateKing start end = inPath end (kingRadius start)

-- Possible end positions relative to a king's starting position
kingRadius:: Pos -> [Pos]
kingRadius (sr, sc) = [
    (sr+1, sc), (sr-1, sc),
    (sr, sc+1), (sr, sc-1),
    (sr+1, sc-1), (sr-1, sc+1),
    (sr+1, sc+1), (sr-1, sc-1)
  ]

-- Make sure end position is either horizontal or vertical to the start
-- position, create a vector between them, and verify it is clear of all pieces
validateHorVert :: Pos -> Pos -> GameState -> Bool
validateHorVert (sr, sc) (er, ec) state 
  | sr == er = isClearPath $ createVector (Just H) (sr, sc) (er, ec) state
  | sc == ec = isClearPath $ createVector (Just V) (sr, sc) (er, ec) state
  | otherwise = False

-- Determine the quadrant of the end position relative to the start position,
-- create a vector between them, verify it is clear of all pieces
validateDiag :: Pos -> Pos -> GameState -> Bool
validateDiag start end state = do
  let quadrant = getQuadrant start end
  isClearPath $ createVector quadrant start end state

-- Determine quadrant of end position relative to the start position. Based on
-- orientation of a 2D array when printed left to right and top to bottom.
getQuadrant :: Pos -> Pos -> Maybe Direction
getQuadrant (sr, sc) end = case end of
  (er, ec) | er < sr && ec > sc -> Just DQ1
  (er, ec) | er < sr && ec < sc -> Just DQ2
  (er, ec) | er > sr && ec < sc -> Just DQ3
  (er, ec) | er > sr && ec > sc -> Just DQ4
  (_, _) -> Nothing

-- Determine if a piece belongs to the opponent
isOppPiece :: Maybe Piece -> ChessColor -> Bool
isOppPiece Nothing _ = False
isOppPiece (Just p) player = color p == oppColor player
  
-- Given a direction, determine the bounds of a vector between start and end,
-- create list of positions within this vector, get pieces at those positions
-- and determine if they are all nothing (clear path).
createVector :: Maybe Direction -> Pos -> Pos -> GameState -> Maybe Vector
createVector dir (sr, sc) (er, ec) state = case dir of
  Just V   -> do
                let (s, e) = getHVBounds sr er
                Just [getPiece state (i,sc) | i <- [s..e]]
  Just H   -> do
                let (s, e) = getHVBounds sc ec
                Just [getPiece state (sr,j) | j <- [s..e]]
  Just DQ1 -> do
                let path = zip (reverse [er..sr-1]) [sc+1..ec]
                if inPath (er,ec) path then do
                  Just [getPiece state (i, j) | (i, j) <- init path]
                else Nothing
  Just DQ2 -> do
                let path = zip (reverse [er..sr-1]) (reverse [ec..sc-1])
                if inPath (er, ec) path then do
                  Just [getPiece state (i, j) | (i, j) <- init path]
                else Nothing
  Just DQ3 -> do
                let path = zip [sr+1..er] (reverse [ec..sc-1])
                if inPath (er, ec) path then do
                  Just [getPiece state (i, j) | (i, j) <- init path]
                else Nothing
  Just DQ4 -> do
                let path = zip [sr+1..er] [sc+1..ec]
                if inPath (er, ec) path then do
                  Just [getPiece state (i, j) | (i, j) <- init path]
                else Nothing
  Nothing  -> Nothing

-- Given an end position determine if it is in the path denoted by the 
-- list of positions.
inPath :: Pos -> [Pos] -> Bool 
inPath _ [] = False
inPath end (x:xs) | end == x    = True
                  | otherwise   = inPath end xs

-- Determine the beginning and end positions of a horizontal or vertical
-- vector depending on where the end position is relative to the start.
-- (to the left, right, above or below).
getHVBounds :: Int -> Int -> Pos
getHVBounds s e | s > e     = (e+1, s-1)
                | otherwise = (s+1, e-1)

-- Iterate through a list of Maybe Pieces and propogate True if only
-- Nothings are encountered or False if a piece is encountered.
isClearPath :: Maybe Vector -> Bool
isClearPath Nothing   = False
isClearPath (Just vs) = foldr ((&&) . isNothing) True vs

-- Determine if end position is either Nothing or an opponent piece
validEndPos :: ChessColor -> Maybe Piece -> Bool
validEndPos _ Nothing = True
validEndPos c (Just p) = color p == oppColor c

-- Determine if start position contains the piece returned from
-- the parser
validStartPos :: Piece -> Maybe Piece -> Bool
validStartPos _ Nothing = False
validStartPos expected (Just actual) = expected == actual

-- Determine if a piece has moved yet
firstMove :: Maybe Piece -> Bool
firstMove Nothing = False
firstMove (Just p) = not (moved p)
