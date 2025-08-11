# Hangman Game

A simple hangman game in Haskell from "Haskell Programming from First Principles" exercises.

## Running

```bash
cabal run MyHangman
```

## How it works

- Randomly selects words (5-9 letters) from `data/word_list850.txt`
- Guess letters one at a time
- You lose after 7 wrong guesses
- Win by completing the word

## Key concepts practiced

- IO Monad for user input and randomness
- Maybe types for partial word discovery
- Custom data types and pattern matching
- Guards for clean conditional logic

## Attribution

Example code adapted from "Haskell Programming from first principles" by Chris Allen and Julie Moronuki. Used for non-commercial educational purposes with acknowledgment of source and authorship.