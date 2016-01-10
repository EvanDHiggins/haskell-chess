module Chess where

import Data.Maybe

data Piece = Pawn | Rook | King | Queen | Bishop | Knight deriving (Show)

type BoardState = [[Maybe Piece]]

data Team a b = Team Int [(Piece, Pos b)]

-- A piece is internally represented as it's 2D index
-- from the top left of the board. It is externally
-- (in IO cases) handled as a (char, int) tuple. For
-- instance 'b4', 'c5', 'g8' etc.
data Pos a = Intern (Int, Int) | Extern (Char, Int)

pieceRepr :: Piece -> Char
pieceRepr (Pawn) = 'P'
pieceRepr (Rook) = 'R'
pieceRepr (King) = 'K'
pieceRepr (Queen) = 'Q'
pieceRepr (Bishop) = 'B'
pieceRepr (Knight) = 'N'
