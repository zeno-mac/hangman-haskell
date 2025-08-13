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
import Data.Char (toLower,isAlpha)
import Data.Maybe (isJust)
import System.Exit (exitSuccess)
import System.Random (randomRIO)
import Text.Read
import Puzzle


type WordList = [String]
data GameState = Win | Loss | InProgress

minWordLength :: Int
minWordLength = 5
maxWordLength :: Int
maxWordLength = 9

minDifficulty :: Int
minDifficulty = 0
maxDifficulty :: Int
maxDifficulty = 5

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



checkGameState :: Puzzle -> IO GameState
checkGameState (Puzzle word filledInSoFar _ guessLeft) 
    | guessLeft <= 0 =  return Loss
    | all isJust filledInSoFar = return Win
    | otherwise = return InProgress

--- TODO Finish tunGame 
runGame :: Puzzle -> IO ()
runGame puzzle@(Puzzle word _ _ _) =  do
    gameState <- checkGameState puzzle
    case gameState of
     Win -> putStrLn "You win!"
     Loss -> putStrLn $ "You lose! The word was "++word 
     InProgress -> do 
      putStrLn $ "Current puzzle is: " ++ show puzzle
      putStrLn "Guess a character: "
      guess <- getLine
      case guess of
          [c] -> if isAlpha c
              then handleGuess puzzle c >>= runGame
              else putStrLn "Your guess must be a letter!"
          _   -> putStrLn "Your guess must be a single character"

selectDifficulty :: IO Int
selectDifficulty = do
    putStrLn $  "Please select a difficulty from " ++ show minDifficulty ++ " to " ++ show maxDifficulty
    diff <-getLine
    if isValidDifficulty diff
        then return $ read diff
        else do 
            putStrLn "Invalid difficulty" 
            selectDifficulty

isValidDifficulty :: String -> Bool
isValidDifficulty number = case readMaybe number :: Maybe Int of
    Just n -> (n>= minDifficulty) && n <= maxDifficulty
    Nothing -> False


main :: IO ()
main = do
    wordList <- gameWords
    startGame wordList

startGame :: WordList -> IO ()
startGame wordList= do
 diff <- selectDifficulty
 word <- randomWord wordList
 let puzzle = freshPuzzle (fmap toLower word) diff
 runGame puzzle
 continue <- askContine
 if continue 
    then startGame wordList
    else return ()


askContine :: IO Bool
askContine = do 
    putStrLn "Do you wish do continue? [y/n]"   
    continue <-getLine
    case map toLower continue of
        "y" -> return True
        _ -> return False



 --- load words
 --- startGame
 --- difficultySelection
 --- runGame
 --- exitSelection