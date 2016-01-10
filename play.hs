import Chess
import ChessIO
import System.Console.ANSI

type GameState = [Int]

main = do
        playUntilFinished $ []

playUntilFinished :: GameState -> IO ()
playUntilFinished gameState = if isFinished gameState
                                then return ()
                                else playUntilFinished gameState

isFinished :: GameState -> Bool
isFinished _ = True
