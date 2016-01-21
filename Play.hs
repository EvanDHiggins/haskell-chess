import Chess
import ChessIO
import System.Console.ANSI
import Data.Char
import Debug.Trace

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
                            putStrLn "Play Until Finished"
                            in1 <- getLine
                            from <- parsePos in1
                            in2 <- getLine
                            to <- parsePos in2
                            show from
                            show to
                            return ()

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
