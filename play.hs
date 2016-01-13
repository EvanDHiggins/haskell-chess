import Chess
import ChessIO
import System.Console.ANSI
import Data.Char

main = do
        playUntilFinished $ initialBoard

playUntilFinished :: Board -> IO ()
playUntilFinished gameState = do 
                                drawChessBoard gameState
                                input <- getLine
                                let linput = map (toLower) input
                                putStrLn . show $ toPos linput
                                if linput == "quit"
                                  then return ()
                                  else playUntilFinished (setPiece gameState (toPos linput) Nothing)

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
