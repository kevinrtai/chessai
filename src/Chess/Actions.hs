module Chess.Actions where
import Chess.Base
import Chess.Query
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
  | otherwise                                          = Nothing
  where target = Map.findWithDefault None fromIx board
        taken  = Map.findWithDefault None toIx board


-- Piece specific move functions that moves the piece at fromIx to toIx as though it is a 
-- King, Queen, Bishop, etc. Return Nothing if the move is invalid
-- TODO add castling
moveKing :: Board -> Int -> (Int, Int) -> (Int, Int) -> Maybe Board
moveKing board time fromIx toIx
  | queryKing board fromIx toIx = move board time fromIx toIx
  | otherwise                        = Nothing

moveQueen :: Board -> Int -> (Int, Int) -> (Int, Int) -> Maybe Board
moveQueen board time fromIx toIx
  | queryQueen board fromIx toIx = move board time fromIx toIx
  | otherwise                         = Nothing

moveBishop :: Board -> Int -> (Int, Int) -> (Int, Int) -> Maybe Board
moveBishop board time fromIx toIx
  | queryBishop board fromIx toIx = move board time fromIx toIx
  | otherwise                          = Nothing 

moveKnight :: Board -> Int -> (Int, Int) -> (Int, Int) -> Maybe Board
moveKnight board time fromIx toIx
  | queryKnight board fromIx toIx = move board time fromIx toIx
  | otherwise                          = Nothing

moveRook :: Board -> Int -> (Int, Int) -> (Int, Int) -> Maybe Board
moveRook board time fromIx toIx
  | queryRook board fromIx toIx = move board time fromIx toIx
  | otherwise                       = Nothing

movePawn :: Board -> Int -> (Int, Int) -> (Int, Int) -> Maybe Board
movePawn board time fromIx toIx
  | queryPawnNormal board fromIx toIx  = move board time fromIx toIx
  | queryPawnEP board time fromIx toIx = move takenEnBd time fromIx toIx
  | otherwise                          = Nothing
  where target     = Map.findWithDefault None fromIx board
        direction  = if (color target == White)
                      then 1 else -1
        takenEnBd  = Map.delete (x1, y1 - direction) board
        (x0, y0)   = fromIx
        (x1, y1)   = toIx

move :: Board -> Int -> (Int, Int) -> (Int, Int) -> Maybe Board
move board time fromIx toIx = Just result
  where Piece p c ts = Map.findWithDefault None fromIx board
        removeFrom   = Map.delete fromIx board
        removeTo     = Map.delete toIx removeFrom
        result       = Map.insert toIx (Piece p c time) removeTo

