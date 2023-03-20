module Main (main) where
  
import Board
import State
import Interface
import LANParser
import GameParser

main :: IO ()
main = do
  mapM_ putStrLn menu
  choice <- getLine
  if choice == "1" then play (([], []), startState, ChessWhite, ((8,8),(8,8)))
  else if choice == "2" then review
  else if choice == "3" then return ()
  else do
    putStrLn "Invalid input"
    main

play :: (Captures, GameState, ChessColor, (Pos, Pos)) -> IO ()
play (caps, state, player, lastMove) = do
  display (updateBoard emptyBoard state) caps player lastMove
  putStrLn "Enter move (using long algebraic notation):"
  moveStr <- getLine
  let nextPlayer = oppColor player
  let moveSet = parseMove moveStr player
  -- putStrLn (show moveSet)
  case makeMove moveSet state of
    Nothing -> putStrLn "Invalid Move" >> play (caps, state, player, lastMove)
    Just (newP, newState, newMove) -> play (addCapture newP caps player, newState, nextPlayer, newMove)

review :: IO ()
review = do
  mapM_ putStrLn reviewMenu
  putStrLn "Enter path to game file: "
  path <- getLine
  moves <- parseGameFile path
  stepLoop moves 0 (([], []), startState, ChessWhite, ((8,8),(8,8)))

stepLoop :: [(Maybe ChessMove, Maybe ChessMove)] -> Int -> (Captures, GameState, ChessColor, (Pos, Pos)) -> IO ()
stepLoop moveList n (caps, state, player, lastMove) = do
  printBoard (updateBoard emptyBoard state) player lastMove
  -- if n == length moveList then putStrLn "Result: " ++ result
  putStrLn "Options: >, <, flip, quit"
  choice <- getLine
  case choice of
    ">"    -> 
      if n + 1 >= length moveList
        then putStrLn "Invalid Move" >> stepLoop moveList n (caps, state, player, lastMove)
        else case makeMove (moveList !! n) state of
          Nothing -> 
            putStrLn "Invalid Move" >> stepLoop moveList n (caps, state, player, lastMove)
          Just (newP, newState, newMove) ->
            stepLoop moveList (n + 1) (addCapture newP caps player, newState, player, newMove)
    "<"    -> 
      if n - 1 < 0
        then putStrLn "Invalid Move" >> stepLoop moveList n (caps, state, player, lastMove)
        else case makeMove (reverseMove (moveList !! (n - 1))) state of 
          Nothing -> 
            putStrLn "Invalid Move" >> stepLoop moveList n (caps, state, player, lastMove)
          Just (_, newState, (x, y)) -> do
            let (newCaps, p) = removeCapture caps ChessWhite -- one list is sufficient here
            stepLoop moveList (n - 1) (newCaps, placePiece p x newState, player, (x, y))
    "flip" -> stepLoop moveList n (caps, state, oppColor player, lastMove)
    "quit" -> main
    _      -> putStrLn "Invalid option" >> stepLoop moveList n (caps, state, player, lastMove)

makeMove :: (Maybe ChessMove, Maybe ChessMove) -> GameState -> Maybe (Maybe Piece, GameState, (Pos, Pos))
makeMove moveSet state = case moveSet of
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

addCapture :: Maybe Piece -> Captures -> ChessColor -> Captures
addCapture p (whiteCaps, blackCaps) ChessWhite = (whiteCaps ++ [p], blackCaps)
addCapture p (whiteCaps, blackCaps) ChessBlack = (whiteCaps, blackCaps ++ [p])

removeCapture :: Captures -> ChessColor -> (Captures, Maybe Piece)
removeCapture (whiteCaps, blackCaps) clr = case clr of
  ChessWhite -> case reverse whiteCaps of
    [] -> ((whiteCaps, blackCaps), Nothing)
    (p:ps) -> ((reverse ps, blackCaps), p)
  ChessBlack -> case reverse blackCaps of
    [] -> ((whiteCaps, blackCaps), Nothing)
    (p:ps) -> ((whiteCaps, reverse ps), p)