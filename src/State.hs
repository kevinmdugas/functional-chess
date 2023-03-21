module State (
  ST (..),
  startState,
  getPiece,
  move,
  addCapture,
  makeMove,
  makeRevMove,
  reverseMove,
  placePiece,
  removeCapture
) where

import Board

{--
  State object is only used after the move denoted by (Pos, Pos) has been
  proven to be valid. It returns the updated board state after the move
  and the captured piece if there is one.
--}
newtype ST = S { apply :: (Pos, Pos) -> GameState -> (Maybe Piece, GameState) }

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

getPiece :: GameState -> Pos -> Maybe Piece
getPiece state (x, y) =  state !! x !! y

getRow :: GameState -> Int -> [Maybe Piece]
getRow state i = state !! i

move :: ST
move = S $ \(start, end) state -> (
  case (start, end) of
    (start, end) | start == end -> (Nothing, state) 

    ((i,j), (h,k)) | inRange i && inRange j && inRange h && inRange k ->
      (getPiece state end, updateState (start, end) state)

    (_, _) -> (Nothing, state)
  ) where
    inRange :: Int -> Bool
    inRange x = x >= 0 && x <= 7

updateState :: (Pos, Pos) -> GameState -> GameState
updateState (start, end) state = do 
  [ updateRow (start, end) state row | row <- [0..7] ] 

updateRow :: (Pos, Pos) -> GameState -> Int -> [Maybe Piece]
updateRow ((a,b), (c,d)) state i =  case i of
  x | x == a && x == c -> do
    let row' = updatePiece (splitAt b (getRow state a)) Nothing
    updatePiece (splitAt d row') (getPiece state (a,b))
  x | x == a -> updatePiece (splitAt b (getRow state a)) Nothing
  x | x == c -> updatePiece (splitAt d (getRow state c)) (getPiece state (a,b))
  _          -> getRow state i

updatePiece :: ([Maybe Piece], [Maybe Piece]) -> Maybe Piece -> [Maybe Piece]
updatePiece (x,_:ys) piece = x ++ setMoved piece : ys
  where
    setMoved :: Maybe Piece -> Maybe Piece
    setMoved Nothing = Nothing
    setMoved (Just p) = Just (p { moved = True })

addCapture :: Maybe Piece -> Captures -> ChessColor -> Captures
addCapture p (whiteCaps, blackCaps) ChessWhite = (whiteCaps ++ [p], blackCaps)
addCapture p (whiteCaps, blackCaps) ChessBlack = (whiteCaps, blackCaps ++ [p])

makeMove :: (Maybe ChessMove, Maybe ChessMove) -> GameState -> (Maybe Piece, GameState, (Pos, Pos))
makeMove moveSet state = case moveSet of
  (Just (k, ks, ke), Just (r, rs, re)) -> do -- Castling
    let (_, newState1) = apply move (ks, ke) state
    let (_, newState2) = apply move (rs, re) newState1 
    (Nothing, newState1, (ks, ke))
  (Just (p, start, end), _) -> do
    let (captP, newState) = apply move (start, end) state
    (captP, newState, (start, end))

makeRevMove :: (Maybe ChessMove, Maybe ChessMove) -> GameState -> Maybe (Maybe Piece, GameState, (Pos, Pos))
makeRevMove moveSet state = case moveSet of
  (Just (_, start1, end1), Just (_, start2, end2)) -> do -- Castling
    let (_, newState1) = apply move (start1, end1) state
    let (_, newState2) = apply move (start2, end2) newState1
    return (Nothing, newState2, (start1, end1))
  (Just (_, start, end), Nothing) -> do -- Valid Input
    let (newP, newState) = apply move (start, end) state
    return (newP, newState, (start, end))
  (_, _) -> do -- Invalid Input
    Nothing

reverseMove :: (Maybe ChessMove, Maybe ChessMove) -> (Maybe ChessMove, Maybe ChessMove)
reverseMove (Just (p1, start1, end1), Just (p2, start2, end2)) =
  (Just (p1, end1, start1), Just (p2, end2, start2))
reverseMove (Just (p, start, end), Nothing) =
  (Just (p, end, start), Nothing)
reverseMove (_, _) = (Nothing, Nothing)

placePiece :: Maybe Piece -> Pos -> GameState -> GameState
placePiece p (i, j) state = 
  take i state ++ [setPiece j (state !! i)] ++ drop (i + 1) state
  where
    setPiece :: Int -> [Maybe Piece] -> [Maybe Piece]
    setPiece j' state' = take j' state' ++ [p] ++ drop (j' + 1) state'

removeCapture :: Captures -> ChessColor -> (Captures, Maybe Piece)
removeCapture (whiteCaps, blackCaps) clr = case clr of
  ChessWhite -> case reverse whiteCaps of
    [] -> ((whiteCaps, blackCaps), Nothing)
    (p:ps) -> ((reverse ps, blackCaps), p)
  ChessBlack -> case reverse blackCaps of
    [] -> ((whiteCaps, blackCaps), Nothing)
    (p:ps) -> ((whiteCaps, reverse ps), p)