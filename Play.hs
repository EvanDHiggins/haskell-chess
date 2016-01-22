import Chess
import ChessIO
import System.Console.ANSI
import Data.Char
import Debug.Trace
import Data.Maybe
import System.Exit
import System.IO

main = do
        playUntilFinished $ initialBoard

playUntilFinished :: Board -> IO ()
playUntilFinished brd = do
                            putStr "Move piece from: "
                            hFlush stdout
                            from <- getPos
                            putStr "             to: "
                            hFlush stdout
                            to <- getPos
                            putStrLn ("from " ++ (show from))
                            putStrLn ("to " ++ (show to))
                            drawChessBoard initialBoard
                            playUntilFinished brd

-- Gets a valid position from the user. Invalid input throws
-- an error and retries the query. "quit" exits the program
getPos :: IO Pos
getPos = do
            input <- getLine
            if (map toLower input) == "quit"
                then exitSuccess
                else let pos = parsePos input in
                        if isNothing pos
                            then do 
                                    printInputError
                                    getPos
                            else return (fromJust pos)

-- Error on incorrect user input
printInputError :: IO ()
printInputError = putStrLn "Invalid space input. Try again"

-- Converts the string parameter into a maybe position.
-- It only considers the first two characters of the input. If a string
-- of "b4zzzzzzz" is passed it will interpret this as "Pos 3 4"
parsePos :: String -> Maybe Pos
parsePos "" = Nothing
parsePos str
        | length str < 2 = Nothing
        | (isValidPosChar (head str)) && (isValidNum (str !! 1)) = Just (Pos (toNum $ head str) (digitToInt (str !! 1)))
        | otherwise = Nothing

-- Converts a character to its corresponding position number on the 
-- chess board. [a..h] maps to [1..8]
toNum :: Char -> Int
toNum ch = (ord . toLower $ ch) - (ord 'a') + 1

-- Determines if the char is within the correct range of a chess
-- board's lettering system
isValidNum :: Char -> Bool
isValidNum n
        | isHexDigit n = (digitToInt n) `elem` [1..8]
        | otherwise = False

-- Returns true if the character argument is a valid chess board character
isValidPosChar :: Char -> Bool
isValidPosChar ch = (toLower ch) `elem` ['a'..'h']
