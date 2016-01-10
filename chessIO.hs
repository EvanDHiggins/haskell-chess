module ChessIO where
import Chess
import Data.Maybe
import System.Console.ANSI as ANSI

pieceRepr :: Piece -> Char
pieceRepr (Piece Chess.White piece) = whitePieceRepr piece
pieceRepr (Piece Chess.Black piece) = blackPieceRepr piece

whitePieceRepr :: PieceType -> Char
whitePieceRepr Pawn = 'P'
whitePieceRepr Rook = 'R'
whitePieceRepr King = 'K'
whitePieceRepr Queen = 'Q'
whitePieceRepr Bishop = 'B'
whitePieceRepr Knight = 'N'

blackPieceRepr :: PieceType -> Char
blackPieceRepr Pawn = 'P'
blackPieceRepr Rook = 'R'
blackPieceRepr King = 'K'
blackPieceRepr Queen = 'Q'
blackPieceRepr Bishop = 'B'
blackPieceRepr Knight = 'N'

drawChessBoard :: Board -> IO ()
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
                            setSGR [SetColor Background Vivid ANSI.White]
                            putStr $ " " ++ [pieceRepr (fromJust piece)] ++ " "
                        else do
                            setSGR [SetColor Background Vivid ANSI.White]
                            putStr "   "

drawBlackCell :: Maybe Piece -> IO ()
drawBlackCell piece = do 
                        if isJust piece
                            then do
                                setSGR [SetColor Background Vivid ANSI.Black]
                                putStr $ " " ++ [pieceRepr (fromJust piece)] ++ " "
                            else do
                                setSGR [SetColor Background Vivid ANSI.Black]
                                putStr "   "
                        setSGR [SetColor Background Vivid ANSI.White]
                        return ()
