module Chess where

import Data.Maybe

data PieceType = Pawn | Rook | King | Queen | Bishop | Knight deriving (Show)
data PieceColor = White | Black
data Piece =  Piece PieceType PieceColor

type BoardState = [[Maybe Piece]]

pieceRepr :: Piece -> Char
pieceRepr (Piece piece White) = whitePiece piece
pieceRepr (Piece piece Black) = blackPiece piece

whitePiece :: PieceType -> Char
whitePiece Pawn = 'P'
whitePiece Rook = 'R'
whitePiece King = 'K'
whitePiece Queen = 'Q'
whitePiece Bishop = 'B'
whitePiece Knight = 'N'

blackPiece :: PieceType -> Char
blackPiece Pawn = 'P'
blackPiece Rook = 'R'
blackPiece King = 'K'
blackPiece Queen = 'Q'
blackPiece Bishop = 'B'
blackPiece Knight = 'N'
