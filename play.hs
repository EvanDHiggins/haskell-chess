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
                                if linput == "quit"
                                  then return ()
                                  else playUntilFinished gameState

isFinished :: Board -> Bool
isFinished _ = True

{-initWhiteTeam :: [[Piece]]-}
