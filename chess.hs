module Chess where

import Data.Maybe

data PieceType = Pawn | Rook | King | Queen | Bishop | Knight deriving (Show)
data PieceColor = White | Black deriving (Show)
data Piece =  Piece PieceColor PieceType deriving (Show)

type Board = [[Maybe Piece]]

pieceRepr :: Piece -> Char
pieceRepr (Piece White piece) = whitePieceRepr piece
pieceRepr (Piece Black piece) = blackPieceRepr piece

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
