module Chess.Query where
import Chess.Base
import qualified Data.Map as Map

-- Tests if the current space is under attack
queryCheck :: Board -> Int -> (Int, Int) -> Bool
queryCheck board time toIx = Map.foldWithKey (\k a b -> (queryPiece board time k toIx) || b) False board

-- Convenience function that looks up the type of piece at fromIx and tests if it can move to toIx
queryPiece :: Board -> Int -> (Int, Int) -> (Int, Int) -> Bool
queryPiece board time fromIx toIx
  | not (inRange fromIx) || not (inRange toIx)         = False
  | (target == None)                                   = False
  | (taken == None) || (color target) /= (color taken) = case target of Piece King c ts -> queryKing board fromIx toIx
                                                                        Piece Queen c ts -> queryQueen board fromIx toIx
                                                                        Piece Bishop c ts -> queryBishop board fromIx toIx
                                                                        Piece Knight c ts -> queryKnight board fromIx toIx
                                                                        Piece Rook c ts -> queryRook board fromIx toIx
                                                                        Piece Pawn c ts -> queryPawnNormal board fromIx toIx || queryPawnEP board time fromIx toIx
                                                                        None -> False
  | otherwise                                          = False
  where target = Map.findWithDefault None fromIx board
        taken  = Map.findWithDefault None toIx board

-- Piece specific move functions that test if the piece at fromIx can move to toIx as though it is a
-- King, Queen, Bishop, etc. True if possible, False otherwise 
-- TODO add castling
queryKing :: Board -> (Int, Int) -> (Int, Int) -> Bool
queryKing board fromIx toIx
  | fromIx /= toIx && (abs (x0 - x1)) <= 1 && (abs (y0 - y1)) <= 1 = True
  | otherwise                                                      = False
  where (x0, y0) = fromIx
        (x1, y1) = toIx

queryQueen :: Board -> (Int, Int) -> (Int, Int) -> Bool
queryQueen board fromIx toIx
  | fromIx /= toIx && (abs (x0 - x1) == abs (y0 - y1) || x0 == x1 || y0 == y1) = True
  | otherwise                                                                  = False
  where (x0, y0) = fromIx
        (x1, y1) = toIx

queryBishop :: Board -> (Int, Int) -> (Int, Int) -> Bool
queryBishop board fromIx toIx
  | fromIx /= toIx && (abs (x0 - x1) == abs (y0 - y1)) = True 
  | otherwise                                          = False
  where (x0, y0) = fromIx
        (x1, y1) = toIx

queryKnight :: Board -> (Int, Int) -> (Int, Int) -> Bool 
queryKnight board fromIx toIx
  | fromIx /= toIx && ((dx == 1 && dy == 2) || (dx == 2 && dy == 1)) = True
  | otherwise                                                        = False
  where (x0, y0) = fromIx
        (x1, y1) = toIx
        dx       = abs (x0 - x1)
        dy       = abs (y0 - y1)

queryRook :: Board -> (Int, Int) -> (Int, Int) -> Bool
queryRook board fromIx toIx
  | fromIx /= toIx && (x0 == x1 || y0 == y1) = True
  | otherwise                                = False
  where (x0, y0) = fromIx
        (x1, y1) = toIx

queryPawnNormal :: Board -> (Int, Int) -> (Int, Int) -> Bool
queryPawnNormal board fromIx toIx
  | fromIx == toIx                                                                    = False
  | dx == 1 && taken /= None && color target /= color taken && (y1 - y0) == direction = True
  | dx > 0 || taken /= None                                                           = False
  | y0 == playerhome && dy == 2 && abs ((y1 - y0) + direction) == 3                   = True
  | taken == None && (y1 - y0) == direction                                           = True
  | otherwise                                                                         = False
  where target     = Map.findWithDefault None fromIx board
        taken      = Map.findWithDefault None toIx board
        direction  = if (color target == White)
                      then 1 else -1
        playerhome = if (color target == White)
                      then 2 else 7
        (x0, y0)   = fromIx
        (x1, y1)   = toIx
        dx         = abs (x0 - x1)
        dy         = abs (y0 - y1)

queryPawnEP :: Board -> Int -> (Int, Int) -> (Int, Int) -> Bool
queryPawnEP board time fromIx toIx
  | fromIx /= toIx && dx == 1 && taken == None && enpassant /= None 
    && color target /= color enpassant && piece enpassant == Pawn 
    && y0 == enemyhome - 2 * direction && (y1 - y0) == direction 
    && time - (ts enpassant) == 1                                   = True
  | otherwise                                                       = False
  where target     = Map.findWithDefault None fromIx board
        taken      = Map.findWithDefault None toIx board
        direction  = if (color target == White)
                      then 1 else -1
        enemyhome  = if (color target == White)
                      then 7 else 2
        enpassant  = Map.findWithDefault None (x1, y1 - direction) board
        (x0, y0)   = fromIx
        (x1, y1)   = toIx
        dx         = abs (x0 - x1)
        dy         = abs (y0 - y1)
 
-- Helper functions
inRange :: (Int, Int) -> Bool
inRange (x, y) = x >= 1 && x <= 8 && y >= 1 && y <= 8
