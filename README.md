# functional-chess

- High-level description of program
  - How to run the game
  - How to quit game
  - Explain LAN
    - 3rd party program used to translate other chess notations to our LAN
  - How to run tests

- Description of each module
  - description of module role in the overall design
  - prominent functions and datatypes
  - Description test cases if any

- Limitations
  - validate path strings
  - Automated check/checkmate logic
  - Human v human games are not recorded
  - Does not support en passante
  - Review doesn't indicate when the game is over

- What we could have done differently
  - Make the state transformer a monad to support chaining moves with the bind 
    operator
  - Interface changes