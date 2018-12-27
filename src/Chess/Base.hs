module Chess.Base where
import Data.Map as Map

-- Datatypes that define the pieces and board of chess
data PieceType = Pawn | Rook | Knight | Bishop | Queen | King deriving (Show, Eq)

data Color = Black | White deriving (Show, Eq)

data Piece = Piece { piece :: PieceType
                   , color :: Color
                   , ts    :: Int} 
             | None deriving (Show, Eq)

type Location = (Int, Int)

type Timestamp = Int

type Board = Map.Map Location Piece

-- Increment the timestamp of the piece
pieceIncTs :: Piece -> Piece
pieceIncTs (Piece p c ts) = Piece p c (ts + 1)

startingBoard :: Board
startingBoard = Map.fromList
                [((1,1), Piece Rook White 0),
                 ((1,2), Piece Knight White 0),
                 ((1,3), Piece Bishop White 0),
                 ((1,4), Piece Queen White 0),
                 ((1,5), Piece King White 0),
                 ((1,6), Piece Bishop White 0),
                 ((1,7), Piece Knight White 0),
                 ((1,8), Piece Rook White 0),
                 ((2,1), Piece Pawn White 0),
                 ((2,2), Piece Pawn White 0),
                 ((2,3), Piece Pawn White 0),
                 ((2,4), Piece Pawn White 0),
                 ((2,5), Piece Pawn White 0),
                 ((2,6), Piece Pawn White 0),
                 ((2,7), Piece Pawn White 0),
                 ((2,8), Piece Pawn White 0),
                 ((7,1), Piece Pawn Black 0),
                 ((7,2), Piece Pawn Black 0),
                 ((7,3), Piece Pawn Black 0),
                 ((7,4), Piece Pawn Black 0),
                 ((7,5), Piece Pawn Black 0),
                 ((7,6), Piece Pawn Black 0),
                 ((7,7), Piece Pawn Black 0),
                 ((7,8), Piece Pawn Black 0),
                 ((8,1), Piece Rook Black 0),
                 ((8,2), Piece Knight Black 0),
                 ((8,3), Piece Bishop Black 0),
                 ((8,4), Piece Queen Black 0),
                 ((8,5), Piece King Black 0),
                 ((8,6), Piece Bishop Black 0),
                 ((8,7), Piece Knight Black 0),
                 ((8,8), Piece Rook Black 0)]
