module Chess.Query where
import Chess.Base
import qualified Data.Map as Map

-- Tests if the current space and all adjacent spaces are under attack
queryCheckmate :: Board -> Timestamp -> Location -> Color -> Bool
queryCheckmate board time toIx friend = queryCheck board time toIx friend && all queryFunc adj
  where adj = adjacent toIx
        queryFunc = \x -> queryCheck board time x friend

-- Helper function to generate all of the adjacent coordinates
adjacent :: Location -> [Location]
adjacent spot = filter (\x -> inRange x && x /= spot) possibles
  where (x, y) = spot
        possibles = [(a, b) | a <- [x-1..x+1], b <- [y-1..y+1]]

-- Tests if the current space is under attack
queryCheck :: Board -> Timestamp -> Location -> Color -> Bool
queryCheck board time toIx friend = Map.foldWithKey (\k a b -> (friend /= color a && queryPiece board time k toIx) || b) False board

-- Convenience function that looks up the type of piece at fromIx and tests if it can move to toIx
queryPiece :: Board -> Timestamp -> Location -> Location -> Bool
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
queryKing :: Board -> Location -> Location -> Bool
queryKing board fromIx toIx
  | fromIx /= toIx && (abs (x0 - x1)) <= 1 && (abs (y0 - y1)) <= 1 = True
  | otherwise                                                      = False
  where (x0, y0) = fromIx
        (x1, y1) = toIx

queryQueen :: Board -> Location -> Location -> Bool
queryQueen board fromIx toIx
  | fromIx /= toIx && (abs (x0 - x1) == abs (y0 - y1) || x0 == x1 || y0 == y1) = True
  | otherwise                                                                  = False
  where (x0, y0) = fromIx
        (x1, y1) = toIx

queryBishop :: Board -> Location -> Location -> Bool
queryBishop board fromIx toIx
  | fromIx /= toIx && (abs (x0 - x1) == abs (y0 - y1)) = True 
  | otherwise                                          = False
  where (x0, y0) = fromIx
        (x1, y1) = toIx

queryKnight :: Board -> Location -> Location -> Bool 
queryKnight board fromIx toIx
  | fromIx /= toIx && ((dx == 1 && dy == 2) || (dx == 2 && dy == 1)) = True
  | otherwise                                                        = False
  where (x0, y0) = fromIx
        (x1, y1) = toIx
        dx       = abs (x0 - x1)
        dy       = abs (y0 - y1)

queryRook :: Board -> Location -> Location -> Bool
queryRook board fromIx toIx
  | fromIx /= toIx && (x0 == x1 || y0 == y1) = True
  | otherwise                                = False
  where (x0, y0) = fromIx
        (x1, y1) = toIx

queryPawnNormal :: Board -> Location -> Location -> Bool
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

queryPawnEP :: Board -> Timestamp -> Location -> Location -> Bool
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
inRange :: Location -> Bool
inRange (x, y) = x >= 1 && x <= 8 && y >= 1 && y <= 8
