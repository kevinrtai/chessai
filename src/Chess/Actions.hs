module Chess.Actions where
import Chess.Base
import qualified Data.Map as Map

-- Move a piece on the given board from one spot to another, returns Nothing if 
-- the move is invalid
movePiece :: Board -> (Int, Int) -> (Int, Int) -> Maybe Board
movePiece board fromIx toIx
  | not (inRange fromIx) || not (inRange toIx)         = Nothing
  | (target == None)                                   = Nothing
  | (taken == None) || (color target) /= (color taken) = case target of Piece King c -> moveKing board fromIx toIx
                                                                        Piece Queen c -> moveQueen board fromIx toIx
                                                                        Piece Bishop c -> moveBishop board fromIx toIx
                                                                        Piece Knight c -> moveKnight board fromIx toIx
                                                                        Piece Rook c -> moveRook board fromIx toIx
                                                                        Piece Pawn c -> movePawn board fromIx toIx
                                                                        None -> Nothing
  | otherwise = Nothing
  where target = Map.findWithDefault None fromIx board
        taken = Map.findWithDefault None toIx board


-- Piece specific move functions that moves the piece at fromIx to toIx as though it is a 
-- King, Queen, Bishop, etc. Return Nothing if the move is invalid
moveKing :: Board -> (Int, Int) -> (Int, Int) -> Maybe Board
moveKing board fromIx toIx
  | fromIx /= toIx && (abs (x0 - x1)) <= 1 && (abs (y0 - y1)) <= 1 = move board fromIx toIx
  | otherwise                                                      = Nothing
  where (x0, y0) = fromIx
        (x1, y1) = toIx

moveQueen :: Board -> (Int, Int) -> (Int, Int) -> Maybe Board
moveQueen board fromIx toIx
  | fromIx /= toIx && (abs (x0 - x1) == abs (y0 - y1) || x0 == x1 || y0 == y1) = move board fromIx toIx
  | otherwise                                                                  = Nothing
  where (x0, y0) = fromIx
        (x1, y1) = toIx

moveBishop :: Board -> (Int, Int) -> (Int, Int) -> Maybe Board
moveBishop board fromIx toIx
  | fromIx /= toIx && (abs (x0 - x1) == abs (y0 - y1)) = move board fromIx toIx
  | otherwise                                          = Nothing 
  where (x0, y0) = fromIx
        (x1, y1) = toIx

moveKnight :: Board -> (Int, Int) -> (Int, Int) -> Maybe Board
moveKnight board fromIx toIx
  | fromIx /= toIx && ((dx == 1 && dy == 2) || (dx == 2 && dy == 1)) = move board fromIx toIx
  | otherwise                                                        = Nothing
  where (x0, y0) = fromIx
        (x1, y1) = toIx
        dx       = abs (x0 - x1)
        dy       = abs (y0 - y1)

moveRook :: Board -> (Int, Int) -> (Int, Int) -> Maybe Board
moveRook board fromIx toIx
  | fromIx /= toIx && (x0 == x1 || y0 == y1) = move board fromIx toIx
  | otherwise                                = Nothing
  where (x0, y0) = fromIx
        (x1, y1) = toIx

-- TODO add en passant and diagonals
movePawn :: Board -> (Int, Int) -> (Int, Int) -> Maybe Board
movePawn board fromIx toIx
  | fromIx /= toIx && (y1 - y0) == direction = move board fromIx toIx
  | otherwise                                = Nothing
  where target    = Map.findWithDefault None fromIx board
        direction = if (color target == White)
                      then 1 else -1
        (x0, y0)  = fromIx
        (x1, y1)  = toIx

move :: Board -> (Int, Int) -> (Int, Int) -> Maybe Board
move board fromIx toIx = Just result
  where piece      = Map.findWithDefault None fromIx board
        removeFrom = Map.delete fromIx board
        removeTo   = Map.delete toIx removeFrom
        result     = Map.insert toIx piece removeTo


-- Helper functions
inRange :: (Int, Int) -> Bool
inRange (x, y) = x >= 1 && x <= 8 && y >= 1 && y <= 8
