{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use when" #-}

{-
 Hangman game implementation
 Based on exercises from "Haskell Programming from first principles"
 by Chris Allen and Julie Moronuki
 Used for non-commercial educational purposes
-}

module Main where
import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO)


type WordList = [String]

minWordLength :: Int
minWordLength = 5
maxWordLength :: Int
maxWordLength = 9
totalGuess :: Int 
totalGuess = 10

validateLength :: [a] -> Bool
validateLength list = let l = length list in
    l>= minWordLength && l<= maxWordLength


allWords :: IO WordList
allWords = do
    file <- readFile "data/word_list850.txt"
    return (words file)

gameWords :: IO WordList
gameWords = do
    filter validateLength <$> allWords


randomWord :: WordList -> IO String
randomWord wl = do
    let number = length wl in
        do
     randomIndex <- randomRIO (0,number -1 )
     return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

data Puzzle = Puzzle String [Maybe Char] [Char] Int

instance Show Puzzle where
 showsPrec = undefined
 show (Puzzle _ discovered guessed guessesLeft) = intersperse ' ' (fmap renderPuzzleChar discovered) ++ " Guessed so far: " ++ guessed ++ " Guesses left: " ++ show guessesLeft

freshPuzzle :: String -> Puzzle
freshPuzzle string = Puzzle string (map (const Nothing) string) [] totalGuess

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


handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle@(Puzzle word filledInSoFar guessedChar guessLeft) guess
    | alreadyGuessed puzzle guess = do
        putStrLn "You already guessed this letter, try another one!"
        return puzzle
    | not $ charInWord puzzle guess = do
        putStrLn "Sorry, this letter is not present!"
        return $ Puzzle word filledInSoFar (guess:guessedChar) (guessLeft-1)
    | otherwise = do
        putStrLn "Yes, the character was right, filled the word!"
        return $ fillInCharacter puzzle guess



checkGameState :: Puzzle -> IO ()
checkGameState (Puzzle word filledInSoFar _ guessLeft) 
    | guessLeft <= 0 =  do 
        putStrLn "You lose!"
        putStrLn $ "The word was: " ++ word
        exitSuccess
    | all isJust filledInSoFar = do
        putStrLn "You win"
        exitSuccess
    |otherwise = return ()


runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
    checkGameState puzzle
    putStrLn $ "Current puzzle is: " ++ show puzzle
    putStr "Guess a letter: "
    guess <- getLine
    case guess of
        [c] -> handleGuess puzzle c >>= runGame
        _   -> putStrLn "Your guess must be a single character"
        

main :: IO ()
main = do
 word <- randomWord'
 let puzzle = freshPuzzle (fmap toLower word)
 runGame puzzle