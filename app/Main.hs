module Main (main) where
  
import Interface

main :: IO ()
main = do
  mapM_ print menu
  choice <- getLine
  if choice == "1" then play
  else if choice == "1" then review
  else do
    putStrLn "Invalid input"
    main
