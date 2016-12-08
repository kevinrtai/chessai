module Chess.Actions where
import Chess.Base
import qualified Data.Map as Map

-- Move a piece on the given board from one spot to another, returns Nothing if 
-- the move is invalid
movePiece :: Board -> Int -> (Int, Int) -> (Int, Int) -> Maybe Board
movePiece board time fromIx toIx
  | not (inRange fromIx) || not (inRange toIx)         = Nothing
  | (target == None)                                   = Nothing
  | (taken == None) || (color target) /= (color taken) = case target of Piece King c ts -> moveKing board time fromIx toIx
                                                                        Piece Queen c ts -> moveQueen board time fromIx toIx
                                                                        Piece Bishop c ts -> moveBishop board time fromIx toIx
                                                                        Piece Knight c ts -> moveKnight board time fromIx toIx
                                                                        Piece Rook c ts -> moveRook board time fromIx toIx
                                                                        Piece Pawn c ts -> movePawn board time fromIx toIx
                                                                        None -> Nothing
  | otherwise = Nothing
  where target = Map.findWithDefault None fromIx board
        taken = Map.findWithDefault None toIx board


-- Piece specific move functions that moves the piece at fromIx to toIx as though it is a 
-- King, Queen, Bishop, etc. Return Nothing if the move is invalid
-- TODO add castling
moveKing :: Board -> Int -> (Int, Int) -> (Int, Int) -> Maybe Board
moveKing board time fromIx toIx
  | fromIx /= toIx && (abs (x0 - x1)) <= 1 && (abs (y0 - y1)) <= 1 = move board time fromIx toIx
  | otherwise                                                      = Nothing
  where (x0, y0) = fromIx
        (x1, y1) = toIx

moveQueen :: Board -> Int -> (Int, Int) -> (Int, Int) -> Maybe Board
moveQueen board time fromIx toIx
  | fromIx /= toIx && (abs (x0 - x1) == abs (y0 - y1) || x0 == x1 || y0 == y1) = move board time fromIx toIx
  | otherwise                                                                  = Nothing
  where (x0, y0) = fromIx
        (x1, y1) = toIx

moveBishop :: Board -> Int -> (Int, Int) -> (Int, Int) -> Maybe Board
moveBishop board time fromIx toIx
  | fromIx /= toIx && (abs (x0 - x1) == abs (y0 - y1)) = move board time fromIx toIx
  | otherwise                                          = Nothing 
  where (x0, y0) = fromIx
        (x1, y1) = toIx

moveKnight :: Board -> Int -> (Int, Int) -> (Int, Int) -> Maybe Board
moveKnight board time fromIx toIx
  | fromIx /= toIx && ((dx == 1 && dy == 2) || (dx == 2 && dy == 1)) = move board time fromIx toIx
  | otherwise                                                        = Nothing
  where (x0, y0) = fromIx
        (x1, y1) = toIx
        dx       = abs (x0 - x1)
        dy       = abs (y0 - y1)

moveRook :: Board -> Int -> (Int, Int) -> (Int, Int) -> Maybe Board
moveRook board time fromIx toIx
  | fromIx /= toIx && (x0 == x1 || y0 == y1) = move board time fromIx toIx
  | otherwise                                = Nothing
  where (x0, y0) = fromIx
        (x1, y1) = toIx

movePawn :: Board -> Int -> (Int, Int) -> (Int, Int) -> Maybe Board
movePawn board time fromIx toIx
  | fromIx == toIx                                                                    = Nothing
  | dx == 1 && taken /= None && color target /= color taken && (y1 - y0) == direction = move board time fromIx toIx
  | dx == 1 && taken == None && enpassant /= None && color target /= color enpassant 
    && piece enpassant == Pawn && y0 == enemyhome - 2 * direction 
    && (y1 - y0) == direction && time - (ts enpassant) == 1                           = move takenEnBd time fromIx toIx
  | dx > 0 || taken /= None                                                           = Nothing
  | y0 == playerhome && dy == 2 && abs ((y1 - y0) + direction) == 3                   = move board time fromIx toIx
  | taken == None && (y1 - y0) == direction                                           = move board time fromIx toIx
  | otherwise                                                                         = Nothing
  where target     = Map.findWithDefault None fromIx board
        taken      = Map.findWithDefault None toIx board
        direction  = if (color target == White)
                      then 1 else -1
        playerhome = if (color target == White)
                      then 2 else 7
        enemyhome  = if (color target == White)
                      then 7 else 2
        enpassant  = Map.findWithDefault None (x1, y1 - direction) board
        takenEnBd  = Map.delete (x1, y1 - direction) board
        (x0, y0)   = fromIx
        (x1, y1)   = toIx
        dx         = abs (x0 - x1)
        dy         = abs (y0 - y1)

move :: Board -> Int -> (Int, Int) -> (Int, Int) -> Maybe Board
move board time fromIx toIx = Just result
  where Piece p c ts = Map.findWithDefault None fromIx board
        removeFrom   = Map.delete fromIx board
        removeTo     = Map.delete toIx removeFrom
        result       = Map.insert toIx (Piece p c time) removeTo


-- Helper functions
inRange :: (Int, Int) -> Bool
inRange (x, y) = x >= 1 && x <= 8 && y >= 1 && y <= 8
