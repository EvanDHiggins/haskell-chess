module Piece where

data PieceType = Pawn | Rook | King | Queen | Bishop | Knight deriving (Show)
data PieceColor = White | Black deriving (Show)
data Piece =  Piece PieceColor PieceType

instance Show Piece where
    show (Piece a b) = (show a) ++ " " ++ (show b)

instance Eq PieceColor where
    White == White = True
    Black == Black = True
    _ == _ = False

instance Eq PieceType where
    Pawn == Pawn = True
    Rook == Rook = True
    King == King = True
    Queen == Queen = True
    Bishop == Bishop = True
    Knight == Knight = True
    _ == _ = False

instance Eq Piece where
    (Piece a b) == (Piece c d) = a == c && b == d
