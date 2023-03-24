module State (
  ST (..),
  startState,
  getPiece,
  move,
  addCapture,
  makeMove,
  reverseMove,
  placePiece,
  removeCapture
) where

import Board

{--
  State transformer is only used after the move denoted by (Pos, Pos) has been
  proven to be valid. It returns the updated board state after the move
  and the captured piece if there is one.
--}
newtype ST = S { apply :: (Pos, Pos) -> GameState -> (Maybe Piece, GameState) }

-- Initial board position
startState :: GameState
startState = [
  [ Just (Piece ChessBlack R False), Just (Piece ChessBlack N False), 
    Just (Piece ChessBlack B False), Just (Piece ChessBlack Q False), 
    Just (Piece ChessBlack K False), Just (Piece ChessBlack B False), 
    Just (Piece ChessBlack N False), Just (Piece ChessBlack R False) ], 
  replicate 8 (Just (Piece ChessBlack P False)),
  replicate 8 Nothing,
  replicate 8 Nothing,
  replicate 8 Nothing,
  replicate 8 Nothing,
  replicate 8 (Just (Piece ChessWhite P False)),
  [ Just (Piece ChessWhite R False), Just (Piece ChessWhite N False), 
    Just (Piece ChessWhite B False), Just (Piece ChessWhite Q False), 
    Just (Piece ChessWhite K False), Just (Piece ChessWhite B False), 
    Just (Piece ChessWhite N False), Just (Piece ChessWhite R False) ]]

-- Return piece at the given position
getPiece :: GameState -> Pos -> Maybe Piece
getPiece state (x, y) =  state !! x !! y

-- Instantiate state transformer that applies a move and returns the piece
-- previously on the end position (if there was one) and the updated state but
-- only if the end position is not the same as the start
move :: ST
move = S $ \(start, end) state -> (
  case (start, end) of
    (s, e) | s == e -> (Nothing, state) 

    ((i,j), (h,k)) | inRange i && inRange j && inRange h && inRange k ->
      do
        ( getPiece state end,
          -- Update the start piece with Nothing, use that altered state to replace the
          -- end piece with the original start piece
          placePiece (getPiece state start) end (placePiece Nothing start state) )

    (_, _) -> (Nothing, state)
  ) where
    inRange :: Int -> Bool
    inRange x = x >= 0 && x <= 7

-- Add captured piece to the lists of captures for each player
addCapture :: Maybe Piece -> Captures -> ChessColor -> Captures
addCapture p (whiteCaps, blackCaps) ChessWhite = (whiteCaps ++ [p], blackCaps)
addCapture p (whiteCaps, blackCaps) ChessBlack = (whiteCaps, blackCaps ++ [p])

-- Given a now validated either standard or castle move, apply move to state transformer
-- and return any captured piece, the new state, and the move positions for displaying
makeMove :: (Maybe ChessMove, Maybe ChessMove) -> GameState -> (Maybe Piece, GameState, (Pos, Pos))
makeMove moveSet state = case moveSet of
  (Just (_, ks, ke), Just (_, rs, re)) -> do -- Castling
    let (_, newState1) = apply move (ks, ke) state
    let (_, newState2) = apply move (rs, re) newState1 
    (Nothing, newState2, (ks, ke))
  (Just (_, start, end), _) -> do
    let (captP, newState) = apply move (start, end) state
    (captP, newState, (start, end))
  (_, _) -> (Nothing, state, ((8,8),(8,8))) -- Case should never be encountered

-- Return the reverse of the current move which will be used to revert the current
-- state to the previous state
reverseMove :: (Maybe ChessMove, Maybe ChessMove) -> (Maybe ChessMove, Maybe ChessMove)
reverseMove (Just (p1, start1, end1), Just (p2, start2, end2)) =
  (Just (p1, end1, start1), Just (p2, end2, start2))
reverseMove (Just (p, start, end), Nothing) =
  (Just (p, end, start), Nothing)
reverseMove (_, _) = (Nothing, Nothing)

-- Given a piece, position and state, place piece at position within state
placePiece :: Maybe Piece -> Pos -> GameState -> GameState
placePiece p (i, j) state = 
  take i state ++ [setPiece j (state !! i)] ++ drop (i + 1) state
  where
    setPiece :: Int -> [Maybe Piece] -> [Maybe Piece]
    setPiece j' state' = take j' state' ++ [p] ++ drop (j' + 1) state'

-- Remove a captured piece from the list of captured pieces for a particular player
removeCapture :: Captures -> ChessColor -> (Captures, Maybe Piece)
removeCapture (whiteCaps, blackCaps) clr = case clr of
  ChessWhite -> case reverse whiteCaps of
    [] -> ((whiteCaps, blackCaps), Nothing)
    (p:ps) -> ((reverse ps, blackCaps), p)
  ChessBlack -> case reverse blackCaps of
    [] -> ((whiteCaps, blackCaps), Nothing)
    (p:ps) -> ((whiteCaps, reverse ps), p)