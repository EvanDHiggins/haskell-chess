module Chess where

import Data.Maybe
import Data.Char

data PieceType = Pawn | Rook | King | Queen | Bishop | Knight deriving (Show)
data PieceColor = White | Black deriving (Show)
data Piece =  Piece PieceColor PieceType deriving (Show)

data Pos = Intern Int Int | Extern Char Int deriving (Show)

type Board = [[Maybe Piece]]

{-movePiece :: Board -> Pos -> Pos -> Board-}
{-movePiece brd from to = let piece = pieceAt brd from in-}

-- Returns the piece at the given position
-- Handles general case of Pos
pieceAt :: Board -> Pos -> Piece
pieceAt brd pos = pieceAt' (reverse brd) pos

pieceAt' :: Board -> Pos -> Piece
pieceAt' brd (Extern c y) = pieceAt' brd (toIntern $ Extern c y)
pieceAt' brd (Intern x y)
                    | x == 1 && y == 1 = fromJust . head $ head brd
                    | y == 1           = pieceAt' ((tail $ head brd) : (tail brd)) (Intern (x - 1) y)
                    | otherwise        = pieceAt' (tail brd) (Intern x (y-1))

isExtern :: Pos -> Bool
isExtern (Extern _ _) = True
isExtern _            = False

isIntern :: Pos -> Bool
isIntern pos = not $ isExtern pos

toIntern :: Pos -> Pos
toIntern (Intern x y) = Intern x y
toIntern (Extern c y) = Intern ((ord $ toLower c) - (ord 'a') + 1) y

toExtern :: Pos -> Pos
toExtern (Extern c y) = Extern c y
toExtern (Intern x y) = Extern (chr ((ord 'a') + x - 1)) y

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
