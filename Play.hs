import Chess
import ChessIO
import System.Console.ANSI
import Data.Char
import Debug.Trace
import Data.Maybe
import Data.Text as T

main = do
        playUntilFinished $ initialBoard

{-playUntilFinished :: Board -> IO ()-}
{-playUntilFinished gameState = do -}
                                {-drawChessBoard gameState-}
                                {-rawInput <- getLine-}
                                {-let input = map (toLower) rawInput-}
                                {-putStrLn . show $ toPos input-}
                                {-if input == "quit"-}
                                  {-then return ()-}
                                  {-else playUntilFinished (setPiece gameState (toPos input) Nothing)-}

playUntilFinished :: Board -> IO ()
playUntilFinished brd = do
                            putStrLn "Move piece from:"
                            in1 <- getLine
                            putStrLn "             to:"
                            in2 <- getLine
                            let from = parsePos in1
                            let to = parsePos in2
                            if isJust from && isJust to
                            then putStrLn "both valid"
                            else do
                                    putStrLn "Invalid space input. Try again"
                                    playUntilFinished brd

                            playUntilFinished brd

printInputError :: IO ()
printInputError = putStrLn "Invalid space input. Try again"

parsePos :: String -> Maybe Pos
parsePos "" = Nothing
parsePos str
        | length str < 2 = Nothing
        | (isValidPosChar (head str)) && (isValidNum (str !! 1)) = Just (Pos (toNum $ head str) (digitToInt (str !! 1)))
        | otherwise = Nothing

toNum :: Char -> Int
toNum ch = (ord . toLower $ ch) - (ord 'a') + 1

isValidNum :: Char -> Bool
isValidNum n
        | isHexDigit n = (digitToInt n) `elem` [1..8]
        | otherwise = False

isValidPosChar :: Char -> Bool
isValidPosChar ch = let c = toLower ch in
                    c == 'a' ||
                    c == 'b' ||
                    c == 'c' ||
                    c == 'd' ||
                    c == 'e' ||
                    c == 'f' ||
                    c == 'g' ||
                    c == 'h'

isFinished :: Board -> Bool
isFinished _ = True


-- Converts an arbitrary Pos type to it's Intern variant
toPos :: String -> Pos
toPos str
        | length str /= 2 = Pos 0 0
        | otherwise = Pos ((ord $ toLower (head str)) - (ord 'a') + 1)(digitToInt $ last str)

{-toIntern :: String -> Pos-}
{-toIntern (Intern x y) = Intern x y-}
{-toIntern (Extern c y) = Intern ((ord $ toLower c) - (ord 'a') + 1) y-}
