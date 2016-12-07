module Chess.Base where
import Data.Map as Map

-- Datatypes that define the pieces and board of chess
data PieceType = Pawn | Rook | Knight | Bishop | Queen | King deriving (Show, Eq)

data Color = Black | White deriving (Show, Eq)

data Piece = Piece { piece :: PieceType
                   , color :: Color } 
             | None deriving (Show, Eq)

type Board = Map.Map (Int, Int) Piece

startingBoard :: Board
startingBoard = Map.fromList
                [((1,1), Piece Rook White),
                 ((1,2), Piece Knight White),
                 ((1,3), Piece Bishop White),
                 ((1,4), Piece Queen White),
                 ((1,5), Piece King White),
                 ((1,6), Piece Bishop White),
                 ((1,7), Piece Knight White),
                 ((1,8), Piece Rook White),
                 ((2,1), Piece Pawn White),
                 ((2,2), Piece Pawn White),
                 ((2,3), Piece Pawn White),
                 ((2,4), Piece Pawn White),
                 ((2,5), Piece Pawn White),
                 ((2,6), Piece Pawn White),
                 ((2,7), Piece Pawn White),
                 ((2,8), Piece Pawn White),
                 ((7,1), Piece Pawn Black),
                 ((7,2), Piece Pawn Black),
                 ((7,3), Piece Pawn Black),
                 ((7,4), Piece Pawn Black),
                 ((7,5), Piece Pawn Black),
                 ((7,6), Piece Pawn Black),
                 ((7,7), Piece Pawn Black),
                 ((7,8), Piece Pawn Black),
                 ((8,1), Piece Rook Black),
                 ((8,2), Piece Knight Black),
                 ((8,3), Piece Bishop Black),
                 ((8,4), Piece Queen Black),
                 ((8,5), Piece King Black),
                 ((8,6), Piece Bishop Black),
                 ((8,7), Piece Knight Black),
                 ((8,8), Piece Rook Black)]
