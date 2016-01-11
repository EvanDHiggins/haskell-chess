module ChessIO where
import Chess
import Data.Maybe
import System.Console.ANSI as ANSI

pieceRepr :: Piece -> Char
pieceRepr (Piece Chess.White piece) = whitePieceRepr piece
pieceRepr (Piece Chess.Black piece) = blackPieceRepr piece

whitePieceRepr :: PieceType -> Char
whitePieceRepr Pawn = '♙'
whitePieceRepr Rook = '♖'
whitePieceRepr King = '♔'
whitePieceRepr Queen = '♕'
whitePieceRepr Bishop = '♗'
whitePieceRepr Knight = '♘'

blackPieceRepr :: PieceType -> Char
blackPieceRepr Pawn = '♟'
blackPieceRepr Rook = '♜'
blackPieceRepr King = '♚'
blackPieceRepr Queen = '♛'
blackPieceRepr Bishop = '♝'
blackPieceRepr Knight = '♞'

drawChessBoard :: Board -> IO ()
drawChessBoard [] = do 
                      drawLetters
                      return ()

drawChessBoard state
            | length state == 8 = do
                                    setSGR [SetColor Background Vivid ANSI.White]
                                    drawLetters
                                    putStr $ " " ++ (show (length state)) ++ " "
                                    drawWhiteRow $ head state
                                    putStrLn $ " " ++ (show (length state)) ++ " "
                                    drawChessBoard $ tail state
            | otherwise         = do
                                    putStr $ " " ++ (show (length state)) ++ " "
                                    if even . length $ state
                                      then drawWhiteRow $ head state
                                      else drawBlackRow $ head state
                                    putStrLn $ " " ++ (show (length state)) ++ " "
                                    drawChessBoard $ tail state

drawLetters :: IO ()
drawLetters = putStrLn "    a  b  c  d  e  f  g  h    "
                                        
drawWhiteRow :: [Maybe Piece] -> IO ()
drawWhiteRow state
            | length state == 1 = do
                                    drawBlackCell $ head state
                                    putStr ""
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
                                    putStr ""
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
