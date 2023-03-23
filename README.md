# functional-chess

- High-level description of program

  - How to run the game
  - How to quit game
  - Explain LAN

    - Our program recognizes chess moves given in a specific variation of long algebraic notation (LAN). An acceptable LAN string consists of up to seven parts: an optional piece character, starting file (lowercase letter a to h), starting rank (integer 1 to 8), capture character (x for capture, - for no capture), destination file (lowercase letter a to h), destination rank (integer 1 to 8), and an optional check/checkmate character (+ for check, # for checkmate, no character if the move doesn’t put the opponent king in check or checkmate). The piece character can be P (pawn), R (rook), N (knight), B (bishop), Q (queen), K (king), or no character, in which case the piece is assumed to be a pawn.
      Examples:
      e2-e4 (pawn on e2 moves to e4)
      Nb4xd3+ (knight on b4 captures a piece on d3, checking the opponent king)
      Qc6xc7# (queen on c6 captures a piece on c7, checkmating the opponent king)
      Lastly, there is the special notation for castling: 0-0 or O-O for castling kingside, and 0-0-0 or O-O-O for castling queenside.

    - 3rd party program used to translate other chess notations to our LAN - The game review feature of our program only accepts game files written in LAN. Games recorded in this notation can be really difficult to find, luckily, there is a command-line program, called pgn-extract, capable of translating files using almost any notation into the variation of LAN that our program accepts. Simply run the pgn-extract program with the flag -Wxolalg on a game file and it will output a file that can be used in our program. There are also 10 properly formatted game files available in the games folder used for testing.
      pgn-extract: https://www.cs.kent.ac.uk/people/staff/djb/pgn-extract/

  - How to run tests

- Description of each module

  - description of module role in the overall design
  - prominent functions and datatypes
  - Description test cases if any

  - Interface
    - The `Interface` module contains all the functions that have the sole purpose of displaying info to the terminal. This includes all the menus, the functions to print the chess board and some other minor functions. The most significant piece to the module is the function `printBoard` which, with the help of the functions `printRow`, `printSquare` and `printPiece`, displays the chessboard with a black and white checkered background and colored letters representing the pieces (blue for white and red for black). The menus are just static lists of strings and the other functions display additional information such as who’s turn it is, pieces captured, etc.

- LANParser

  - The `LANParser` module contains functions to parse strings according to the chess long algebraic notation (LAN) variation described above. Strings are given to the function `parseMove` where they are first stripped of the check / checkmate character (if it exists). Then the strings are checked to see if they match a castling notation, if not, the parser then destructures the string into its character components. Each character gets run through validators to make sure they fit the notation. If everything checks out, the parser returns the relevant information to get passed on to the move validator.

- GameParser

  - The `GameParser` module functions receive the contents of a file and pull out any relevant information. The function `parseGameFile` filters out strings that don’t start with a valid LAN character, such as headers commonly added to .pgn files, turn numbers and empty strings. Then, the function passes all strings that may be valid to the `processMoves` function which feeds all the strings to the `parseMove` function from the `LANParser` module and filters out anything that comes back invalid.

- Limitations

  - Automated check/checkmate logic
  - Human vs Human games are not recorded
    - One of our initial goals was to record games played through our program by writing the moves entered into an output file. The idea being that users could play games and then enter the file of the recorded game into the review function to step through the game again. This functionality would likely be easy to implement, however, we simply didn't find the time to get to it.
  - Does not support en passante

- What we could have done differently

  - Make the state transformer a monad to support chaining moves with the bind
    operator
  - Interface changes - Adrik
    - Some time after implementing `printBoard` and its accompanying functions in the `Interface` module, I started to realize I could maybe have done it better. Once I began working on adding more information to the CLI and noticed the output often being taller than my terminal window, I thought, why not just use the space to the right of the board? However, with the way I structured the display functions, as `IO ()` outputs, I don't think I could, at least not very easily. What I wish I had done from the beginning was make all the functions in the `Interface` module return lists of strings, `[String]`, like I did for all the menus. This way I could have had the board as one list and another list that contained any additional information, such as which player’s turn it was, which end of the board represented which player, pieces captured, etc.; all the information I had to cram into the space above and below the board instead. Then, with those two lists of strings, I could have used a function like this:
    ```
    combineDisplays :: [String] -> [String] -> [String]
    combineDisplays [] _ = []
    combineDisplays _ [] = []
    combineDisplays (x:xs) (y:ys) = (x ++ y) : combineDisplays xs ys
    ```
    That way I could also swap out displays as needed. I did attempt to make this change, however, the issue was that I had no idea how to keep the color information that I had added using the module `System.Console.ANSI`. I’m sure there is a way to do it, maybe by feeding the combined displays into another function that would color the output, but I couldn’t figure it out and had to move on.
