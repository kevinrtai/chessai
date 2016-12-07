module Chess.Actions where
import Chess.Base
import qualified Data.Map as Map

movePiece :: Board -> (Int, Int) -> (Int, Int) -> Maybe Board
movePiece board fromIx = case piece of Piece King c -> moveKing board c fromIx
                                       Piece Queen c -> moveQueen board c fromIx
                                       Piece Bishop c -> moveBishop board c fromIx
                                       Piece Knight c -> moveKnight board c fromIx
                                       Piece Rook c -> moveRook board c fromIx
                                       Piece Pawn c -> movePawn board c fromIx
                                       None -> \(x, y) -> Nothing
                         where piece = Map.findWithDefault None fromIx board

moveKing :: Board -> Color -> (Int, Int) -> (Int, Int) -> Maybe Board
moveKing _ _ _ _ = Nothing

moveQueen :: Board -> Color -> (Int, Int) -> (Int, Int) -> Maybe Board
moveQueen _ _ _ _ = Nothing

moveBishop :: Board -> Color -> (Int, Int) -> (Int, Int) -> Maybe Board
moveBishop _ _ _ _ = Nothing

moveKnight :: Board -> Color -> (Int, Int) -> (Int, Int) -> Maybe Board
moveKnight _ _ _ _ = Nothing

moveRook :: Board -> Color -> (Int, Int) -> (Int, Int) -> Maybe Board
moveRook _ _ _ _= Nothing

movePawn :: Board -> Color -> (Int, Int) -> (Int, Int) -> Maybe Board
movePawn _ _ _ _ = Nothing

