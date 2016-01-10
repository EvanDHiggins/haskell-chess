module ChessIO where
import Chess
import Data.Maybe
import System.Console.ANSI

drawChessBoard :: BoardState -> IO ()
drawChessBoard [] = return ()
drawChessBoard state = do
                        if even . length $ state
                          then drawWhiteRow (head state)
                          else drawBlackRow (head state)
                        drawChessBoard $ tail state

drawWhiteRow :: [Maybe Piece] -> IO ()
drawWhiteRow state
            | length state == 1 = do
                                    drawBlackCell $ head state
                                    putStrLn ""
                                    return ()
            | otherwise = do
                            if even . length $ state
                              then drawWhiteCell $ head state
                              else drawBlackCell $ head state
                            drawWhiteRow $ tail state

drawBlackRow :: [Maybe Piece] -> IO ()
drawBlackRow state
            | length state == 1 = do
                                    drawWhiteCell $ head state
                                    putStrLn ""
                                    return ()
            | otherwise  = do
                              if even . length $ state
                                then drawBlackCell $ head state
                                else drawWhiteCell $ head state
                              drawBlackRow $ tail state

drawWhiteCell :: Maybe Piece -> IO ()
drawWhiteCell piece = if isJust piece
                        then do
                            setSGR [SetColor Background Vivid White]
                            putStr $ " " ++ [pieceRepr (fromJust piece)] ++ " "
                        else do
                            setSGR [SetColor Background Vivid White]
                            putStr "   "

drawBlackCell :: Maybe Piece -> IO ()
drawBlackCell piece = if isJust piece
                        then do
                            setSGR [SetColor Background Vivid Black]
                            putStr $ " " ++ [pieceRepr (fromJust piece)] ++ " "
                            setSGR [SetColor Background Vivid White]
                            return ()
                        else do
                            setSGR [SetColor Background Vivid Black]
                            putStr "   "
                            setSGR [SetColor Background Vivid White]
                            return ()
