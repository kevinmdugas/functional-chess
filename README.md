# functional-chess

This repository contains a CLI chess game implemented in Haskell in which you can play a game with 
another person or provide a transcript of a game in the format demonstrated in `games` folder.
To run the game, install [Stack](https://docs.haskellstack.org/en/stable/) and run the command
`stack run Main` from within the root directory.

You will be presented with the main menu and three options:

1. Play (Human vs Human)
2. Load game for review
3. Quit

You can quit a game or review and return to the main menu any time by entering `quit`. If playing
a game, each player will take turns providing a [LAN](#long-algebraic-notaion-lan) 
string that
denotes the move they wish to make. The game ends when either king is captured (you
must explicitly type the LAN command to capture a king).

## Table of Contents

* [Long Algebraic Notation(LAN)](#long-algebraic-notaion-lan)
  * [LAN Examples](#lan-examples)

## Long Algebraic Notaion (LAN)

Our program recognizes chess moves given in a specific variation of long algebraic notation (LAN). An 
acceptable LAN string consists of up to seven parts: 

* A piece character *(optional if moving a pawn, required otherwise)*
* Starting file: lowercase letter a to h
* Starting rank: integer 1 to 8
* Capture character: x for capture, - for no capture
* Destination file: lowercase letter a to h
* Destination rank: integer 1 to 8
* Check/checkmate character *(optional)*: + for check, # for checkmate 

The piece characters are:

* `P` (pawn)
* `R` (rook)
* `N` (knight)
* `B` (bishop)
* `Q` (queen)
* `K` (king)
* no character, in which case the piece is assumed to be a pawn.

Lastly, there is the special notation for castling:

* `0-0` or `O-O` for castling kingside
* `0-0-0` or `O-O-O` for castling queenside.

### LAN Examples

```
e2-e4     (pawn on e2 moves to e4)
Nb4xd3+   (knight on b4 captures a piece on d3, checking the opponent king)
Qc6xc7#   (queen on c6 captures a piece on c7, checkmating the opponent king)
0-0       (castle kingside)
```

### LAN Translation
    
The game review feature of our program only accepts game files written in LAN, though games 
recorded inthis notation can be really difficult to find. Luckily, there is a command-line program, 
called [pgn-extract](https://www.cs.kent.ac.uk/people/staff/djb/pgn-extract/), capable of translating 
files using almost any notation into the variation of LAN that our program accepts. 

Simply run the pgn-extract program with the flag -Wxolalg on a game file and it will output a file 
that can be used in our program. There are also 10 properly formatted game files available in the 
games folder used for testing.

## Design Overview

  - How to run tests

- Description of each module

  - description of module role in the overall design
  - prominent functions and datatypes
  - Description test cases if any

### Main

The `Main` module is the root module and is responsible for calling all other child modules as well
as taking user input and using it to navigate through menus and advance through games and reviews.

Aside from the `main` function which serves as the main menu, the main module also includes `play`
which handles the human vs human games, `review` which kicks off the game parser and starts a
review, and `stepLoop` which allows the user to move forward, backward and quit from a game review.

### Board

The `Board` module primarily contains most of the data and types that constitute a game of chess,
including `Piece` which has a color, type, and bool indicating if it's moved; `Square` which
has a color, position, and either a piece or nothing; and `Board` which is a 7x7 grid of squares.
There's also `GameState` which is a record of the current state of the board.

`emptyBoard` is a list comprehension that generates the board of empty squares and functions
to update the board with the map of pieces contained in the GameState.

### Interface

The `Interface` module contains all the functions that have the sole purpose of displaying info to the terminal. This includes all the menus, the functions to print the chess board and some other minor functions.

The most significant piece to the module is the function `printBoard` which, with the help of the functions `printRow`, `printSquare` and `printPiece`, displays the chessboard with a black and white checkered background and colored letters representing the pieces (blue for white and red for black). 

The menus are just static lists of strings and the other functions display additional information such as who’s turn it is, pieces captured, etc.

### LANParser

The `LANParser` module contains functions to parse strings according to the chess long algebraic notation (LAN) variation described above. Strings are given to the function `parseMove` where they are first stripped of the check / checkmate character (if it exists). Then the strings are checked to see if they match a castling notation, if not, the parser then destructures the string into its character components.

Each character gets run through validators to make sure they fit the notation. If everything checks out, the parser returns the relevant information to get passed on to the move validator.

### Validate

Once the LAN parser returns a move a valid move string, the `Validate` module then makes sure that
the move proposed by the player is possible given the current state of the board and the move rules
of the piece(s) involved. Ultimately, the validate module returns a boolean value that will decide
if the State module is called to carry out the move.

The `Validate` determines if the proposed move is a standard move or a castle move. If the former,
`validateStandard` performs validations common to all piece types before calling piece-specific
validator functions.

Notably, this module uses a combination of `createVector` and `isClearPath`
to define a vector of some length and direction between a move's start and end positions before
using a foldable to determine if the path between them is clear.

`validateCastle` performs castle-specific validations, including whether or not the involved
rook and king pieces of have moved.

### State

Once a move is deemed valid, the `State` module is called to actually carry out the move, altering
the GameState and returning the captured piece if there is one.

The `ST` is a state
transformer that has a single member which replaces the need to invoke the dummy constructor and
accepts a function called `move`. `move` does the work
required to alter state: it gets the piece that was at the end position before the move, updates
the GameState by placing nothing on the start position and the previous start piece on the end
position, and returns the captured piece and new GameState to the caller.

The returned GameState is displayed in the Interface module and the captured piece is used
to keep a list of the captured pieces for each player and determine a winner if a captured piece
is a king.

### GameParser

The `GameParser` module functions receive the contents of a file and pull out any relevant information. The function `parseGameFile` filters out strings that don’t start with a valid LAN character, such as headers commonly added to .pgn files, turn numbers and empty strings. Then, the function passes all strings that may be valid to the `processMoves` function which feeds all the strings to the `parseMove` function from the `LANParser` module and filters out anything that comes back invalid.

### Test

The `Test` directory contains the `Spec` and `TestData` modules which constitute our unit testing
suite. We used test driven development to validate the implementation of our major functions
before incorporating those functions in our main program. The tested functions are the `ST`
transformer, `parseMove` from the LANParser, and lists of `validate` invocations for each
of the pieces.

This allowed us to refactor and test our code quickly without needing to run through manual
scenarios by actually running the entire program itself. To run the test suite, simply run
`stack test` from the terminal.

## Limitations

The following features were stretch goals that we unfortunately did not have
enough time to implement:

* Automated check/checkmate logic
* Human vs Human games are not recorded
* Does not support en passante

## What we could have done differently

### Interface changes (Adrik)

Some time after implementing `printBoard` and its accompanying functions in the `Interface` module, I started to realize I could maybe have done it better. Once I began working on adding more information to the CLI and noticed the output often being taller than my terminal window, I thought, why not just use the space to the right of the board? However, with the way I structured the display functions, as `IO ()` outputs, I don't think I could, at least not very easily.

What I wish I had done from the beginning was make all the functions in the `Interface` module return lists of strings, `[String]`, like I did for all the menus. This way I could have had the board as one list and another list that contained any additional information, such as which player’s turn it was, which end of the board represented which player, pieces captured, etc; all the information I had to cram into the space above and below the board instead. Then, with those two lists of strings, I could have used a function like this:

```haskell
combineDisplays :: [String] -> [String] -> [String]
combineDisplays [] _ = []
combineDisplays _ [] = []
combineDisplays (x:xs) (y:ys) = (x ++ y) : combineDisplays xs ys
```

That way I could also swap out displays as needed. I did attempt to make this change, however, the issue was that I had no idea how to keep the color information that I had added using the module `System.Console.ANSI`. I’m sure there is a way to do it, maybe by feeding the combined displays into another function that would color the output, but I couldn’t figure it out and had to move on.

### Monadic state transformer (Kevin)

I knew before starting on the state module that I wanted to create a state monad
because it would be helpful to be able to bind state changes togther. For example,
castling is the only move in chess that's really two moves: you move the king and
the rook.

Our current implementation simply uses a state transformer `ST` to update the state
after moving the king and storing the output in a local variable and using the
new state to create another state transformer, thereby returning the final state
object printed to the screen:

```haskell
(Just (_, ks, ke), Just (_, rs, re)) -> do -- Castling
  let (_, newState1) = apply move (ks, ke) state
  let (_, newState2) = apply move (rs, re) newState1 
```

Given more time, I would have liked to figure out how to turn ST into a monad so
that I could chain those two function calls together, something along the lines of

```haskell
(Just (_, ks, ke), Just (_, rs, re)) -> do -- Castling
  let (_, newState) = apply move (ks, ke) state >>= 
    \capt state' -> apply move (rs, re) state'
```
