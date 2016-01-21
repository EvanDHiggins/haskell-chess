module Chess where

import Piece
import Data.Maybe
import Data.Char


data Pos = Pos Int Int deriving (Show)

type Board = [[Maybe Piece]]

-- Places piece at $from at $to and places a blank space
-- at $from.
{-movePiece :: Board -> Pos -> Pos -> Board-}
{-movePiece brd from to = let piece = pieceAt brd from in-}

setPiece :: Board -> Pos -> Maybe Piece -> Board
setPiece brd (Pos x y) piece
                    | x `elem` [1..8] && y `elem` [1..8] = reverse $ setPiece' (reverse brd) (Pos x y) piece
                    | otherwise = brd
{-setPiece brd pos piece = reverse $ setPiece' (reverse brd) pos piece-}

setPiece' :: Board -> Pos -> Maybe Piece -> Board
setPiece' [] _ _ = []
setPiece' brd (Pos x 1) piece = (setPieceRow (head brd) x piece) : (tail brd)
setPiece' brd (Pos x y) piece = (head brd) : (setPiece' (tail brd) (Pos x (y-1)) piece)

setPieceRow :: [Maybe Piece] -> Int -> Maybe Piece -> [Maybe Piece]
setPieceRow [] _ _ = []
setPieceRow row 1 piece = piece : (tail row)
setPieceRow (x:xs) idx piece = x : (setPieceRow xs (idx-1) piece)
                        
-- Returns the piece at the given position
-- Handles general case of Pos
pieceAt :: Board -> Pos -> Piece
pieceAt brd pos = aux (reverse brd) pos
          where aux brd (Pos x y)
                    | x == 1 && y == 1 = fromJust . head $ head brd
                    | y == 1           = aux ((tail $ head brd) : (tail brd)) (Pos (x - 1) y)
                    | otherwise        = aux (tail brd) (Pos x (y-1))


{--- Converts an arbitrary Pos type to it's Extern variant-}
{-toExtern :: Pos -> Pos-}
{-toExtern (Extern c y) = Extern c y-}
{-toExtern (Intern x y) = Extern (chr ((ord 'a') + x - 1)) y-}

-- Create initial board with the black team at the top of the
-- board (in relation to the screen) and the white team at the bottom
initialBoard :: Board
initialBoard = (reverse (initTeam Black)) ++ emptyRows ++ (initTeam White)
            where emptyRows = replicate 4 $ replicate 8 Nothing

-- Returns list of [[Back Row], [Pawns]] of the provided color
initTeam :: PieceColor -> [[Maybe Piece]]
initTeam color = (map (justColor) pawns) : (map (justColor) (team)) : []
        where justColor = Just . Piece color

-- The list of non-pawn pieces in starting order
team :: [PieceType]
team = [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]

-- A full row of pawns
pawns :: [PieceType]
pawns = replicate 8 Pawn
