module Puzzle where

import Data.List (intersperse)

totalGuess :: Int 
totalGuess = 10

data Puzzle = Puzzle String [Maybe Char] [Char] Int

instance Show Puzzle where
 showsPrec = undefined
 show (Puzzle _ discovered guessed guessesLeft) = intersperse ' ' (fmap renderPuzzleChar discovered) ++ "\nGuessed so far: " ++ guessed ++ "\nGuesses left: " ++ show guessesLeft

freshPuzzle :: String -> Int-> Puzzle
freshPuzzle string diff = Puzzle string (map (const Nothing) string) [] (totalGuess - diff)

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle word _ _ _) char = char `elem` word

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ list _) char = char `elem` list

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing = '_'
renderPuzzleChar (Just char) = char

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s guessLeft) c = Puzzle word newFilledWord (c:s) guessLeft where
    newFilledWord = zipWith (fill c) word filledInSoFar
    fill guess correctChar Nothing = if guess == correctChar then Just correctChar else Nothing
    fill _ _ guessed = guessed