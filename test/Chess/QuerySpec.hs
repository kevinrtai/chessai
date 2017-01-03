module Chess.QuerySpec (spec) where

import Test.Hspec
import Test.HUnit
import Chess.Base
import Chess.Query
import Data.Map as Map
import Data.Set as Set

spec :: Spec
spec = do
  -- queryCheckmate tests
  describe "queryCheckmate" $ do
    it  "returns True for space in checkmate" $ do
        let starting = Map.fromList [((3, 3), Piece Queen White 0), ((5, 5), Piece Queen White 0)];
        let expected = True;
        let result = queryCheckmate starting 1 (4, 4) Black;
        result `shouldBe` expected
    it  "returns False for space not in checkmate" $ do
        let starting = Map.fromList [((3, 3), Piece Queen White 0), ((4, 4), Piece King Black 0)];
        let expected = False;
        let result = queryCheckmate starting 1 (4, 4) Black;
        result `shouldBe` expected

  -- adjacent tests
  describe "adjacent" $ do
    it  "returns all adjacent spaces surrounding an internal space" $ do
        let expected = Set.fromList [(3, 3), (3, 4), (3, 5), (4, 3), (4, 5), (5, 3), (5, 4), (5, 5)];
        let result = Set.fromList $ adjacent (4, 4)
        result `shouldBe` expected
    it  "returns all adjacent spaces surrounding a space on the bottom edge of the board" $ do
        let expected = Set.fromList [(3, 1), (3, 2), (4, 2), (5, 2), (5, 1)];
        let result = Set.fromList $ adjacent (4, 1);
        result `shouldBe` expected
    it  "returns all adjacent spaces surrounding a space on the left edge of the board" $ do
        let expected = Set.fromList [(1, 5), (2, 5), (2, 4), (2, 3), (1, 3)];
        let result = Set.fromList $ adjacent (1, 4);
        result `shouldBe` expected
    it  "returns all adjacent spaces surrounding a space on the top edge of the board" $ do
        let expected = Set.fromList [(3, 8), (3, 7), (4, 7), (5, 7), (5, 8)];
        let result = Set.fromList $ adjacent (4, 8);
        result `shouldBe` expected
    it  "returns all adjacent spaces surrounding a space on the right edge of the board" $ do
        let expected = Set.fromList [(8, 5), (7, 5), (7, 4), (7, 3), (8, 3)];
        let result = Set.fromList $ adjacent (8, 4);
        result `shouldBe` expected
    it  "returns all adjacent spaces surrounding the bottom right space" $ do
        let expected = Set.fromList [(7, 1), (7, 2), (8, 2)];
        let result = Set.fromList $ adjacent (8, 1);
        result `shouldBe` expected
    it  "returns all adjacent spaces surrounding the bottom left space" $ do
        let expected = Set.fromList [(2, 1), (2, 2), (1, 2)];
        let result = Set.fromList $ adjacent (1, 1);
        result `shouldBe` expected
    it  "returns all adjacent spaces surrounding the top left space" $ do
        let expected = Set.fromList [(1, 7), (2, 7), (2, 8)];
        let result = Set.fromList $ adjacent (1, 8);
        result `shouldBe` expected
    it  "returns all adjacent spaces surrounding the top right space" $ do
        let expected = Set.fromList [(7, 8), (7, 7), (8, 7)];
        let result = Set.fromList $ adjacent (8, 8);
        result `shouldBe` expected
  -- queryCheck tests
  describe "queryCheck" $ do
    it  "returns True for space next to King" $ do
        let starting = Map.fromList [((4, 4), Piece King Black 0)];
        let expected = True;
        let result = queryCheck starting 0 (4, 5) White;
        result `shouldBe` expected
    it  "returns False for space two away from King" $ do
        let starting = Map.fromList [((4, 4), Piece King Black 0)];
        let expected = False;
        let result = queryCheck starting 0 (4, 6) White;
        result `shouldBe` expected
    it  "returns False for space where the King is" $ do
        let starting = Map.fromList [((4, 4), Piece King Black 0)];
        let expected = False;
        let result = queryCheck starting 0 (4, 4) White;
        result `shouldBe` expected
    it  "returns False for space next to King of same color" $ do
        let starting = Map.fromList [((4, 4), Piece King Black 0)];
        let expected = False;
        let result = queryCheck starting 0 (4, 5) Black;
        result `shouldBe` expected
    it  "returns True for space next to King, large Map" $ do
        let starting = Map.fromList [((4, 8), Piece King White 0),
                                     ((1, 1), Piece Bishop White 0),
                                     ((4, 5), Piece Queen White 0), 
                                     ((8, 8), Piece Pawn White 0),
                                     ((7, 2), Piece Knight White 0),
                                     ((4, 4), Piece King Black 0),
                                     ((1, 8), Piece Pawn Black 0),
                                     ((2, 3), Piece Rook White 20)];
        let expected = True;
        let result = queryCheck starting 0 (4, 5) White;
        result `shouldBe` expected
    it  "returns True for Queen next to King" $ do
        let starting = Map.fromList [((3, 3), Piece Queen White 0), ((5, 5), Piece Queen White 0)];
        let expected = True;
        let result = queryCheck starting 1 (4, 4) Black;
        result `shouldBe` expected

  -- queryKing tests
  describe "queryKing" $ do
    it  "returns True for moving King north from (4, 4) to (4, 5)" $ do
        let starting = Map.fromList [((4, 4), Piece King Black 0)];
        let expected = True;
        let result = queryKing starting (4, 4) (4, 5); 
        result `shouldBe` expected
    it  "returns True for moving King northeast from (4, 4) to (5, 5)" $ do
        let starting = Map.fromList [((4, 4), Piece King Black 0)];
        let expected = True;
        let result = queryKing starting (4, 4) (5, 5); 
        result `shouldBe` expected
    it  "returns True for moving King east from (4, 4) to (5, 4)" $ do
        let starting = Map.fromList [((4, 4), Piece King Black 0)];
        let expected = True;
        let result = queryKing starting (4, 4) (5, 4); 
        result `shouldBe` expected
    it  "returns True for moving King southeast from (4, 4) to (5, 3)" $ do
        let starting = Map.fromList [((4, 4), Piece King Black 0)];
        let expected = True;
        let result = queryKing starting (4, 4) (5, 3); 
        result `shouldBe` expected
    it "returns True for moving King south from (4, 4) to (4, 3)" $ do
        let starting = Map.fromList [((4, 4), Piece King Black 0)];
        let expected = True;
        let result = queryKing starting (4, 4) (4, 3); 
        result `shouldBe` expected
    it  "returns True for moving King southwest from (4, 4) to (3, 3)" $ do
        let starting = Map.fromList [((4, 4), Piece King Black 0)];
        let expected = True;
        let result = queryKing starting (4, 4) (3, 3); 
        result `shouldBe` expected
    it  "returns True for moving  King west from (4, 4) to (3, 4)" $ do
        let starting = Map.fromList [((4, 4), Piece King Black 0)];
        let expected = True;
        let result = queryKing starting (4, 4) (3, 4); 
        expected `shouldBe` expected
    it  "returns True for moving King northwest from (4, 4) to (3, 5)" $ do
        let starting = Map.fromList [((4, 4), Piece King Black 0)];
        let expected = True;
        let result = queryKing starting (4, 4) (3, 5); 
        result `shouldBe` expected
    it  "returns False when given out of bounds move" $ do
        let starting = Map.fromList [((4, 4), Piece King Black 0)];
        let expected = False;
        let result = queryKing starting (4, 4) (6, 4); 
        result `shouldBe` expected

  -- queryQueen tests
  describe "queryQueen" $ do
    it  "returns True for moving Queen north across the board" $ do
        let starting = Map.fromList [((1, 1), Piece Queen White 0)];
        let expected = True; 
        let result   = queryQueen starting (1, 1) (1, 8);
        result `shouldBe` expected
    it  "returns True for moving Queen south across the board" $ do
        let starting = Map.fromList [((1, 8), Piece Queen White 0)];
        let expected = True;
        let result   = queryQueen starting (1, 8) (1, 1);
        result `shouldBe` expected
    it  "returns True for moving Queen east across the board" $ do
        let starting = Map.fromList [((1, 1), Piece Queen White 0)];
        let expected = True;
        let result   = queryQueen starting (1, 1) (8, 1);
        result `shouldBe` expected
    it  "returns True for moving Queen west across the board" $ do
        let starting = Map.fromList [((8, 1), Piece Queen White 0)];
        let expected = True;
        let result   = queryQueen starting (8, 1) (1, 1);
        result `shouldBe` expected
    it  "returns True for moving Queen northeast across the board" $ do
        let starting = Map.fromList [((1, 1), Piece Queen White 0)];
        let expected = True;
        let result   = queryQueen starting (1, 1) (8, 8);
        result `shouldBe` expected
    it  "returns True for moving Queen northwest across the board" $ do 
        let starting = Map.fromList [((8, 1), Piece Queen White 0)];
        let expected = True;
        let result   = queryQueen starting (8, 1) (1, 8);
        result `shouldBe` expected
    it  "returns True for moving Queen southeast across the board" $ do
        let starting = Map.fromList [((1, 8), Piece Queen White 0)];
        let expected = True;
        let result   = queryQueen starting (1, 8) (8, 1);
        result `shouldBe` expected
    it  "returns True for moving Queen southwest across the board" $ do
        let starting = Map.fromList [((8, 8), Piece Queen White 0)];
        let expected = True;
        let result   = queryQueen starting (8, 8) (1, 1);
        result `shouldBe` expected
    it  "returns False when given invalid move" $ do
        let starting = Map.fromList [((1, 1), Piece Queen White 0)];
        let expected = False;
        let result   = queryQueen starting (1, 1) (2, 8);
        result `shouldBe` expected

  -- queryBishop tests
  describe "queryBishop" $ do
    it  "returns True for moving Bishop northeast across the board" $ do
        let starting = Map.fromList [((1, 1), Piece Bishop White 0)];
        let expected = True;
        let result   = queryBishop starting (1, 1) (8, 8);
        result `shouldBe` expected
    it  "returns True for moving Bishop northwest across the board" $ do 
        let starting = Map.fromList [((8, 1), Piece Bishop White 0)];
        let expected = True;
        let result   = queryBishop starting (8, 1) (1, 8);
        result `shouldBe` expected
    it  "returns True for moving Bishop southeast across the board" $ do
        let starting = Map.fromList [((1, 8), Piece Bishop White 0)];
        let expected = True; 
        let result   = queryBishop starting (1, 8) (8, 1);
        result `shouldBe` expected
    it  "returns True for moving Bishop southwest across the board" $ do
        let starting = Map.fromList [((8, 8), Piece Bishop White 0)];
        let expected = True;
        let result   = queryBishop starting (8, 8) (1, 1);
        result `shouldBe` expected
    it  "returns False when given invalid move" $ do
        let starting = Map.fromList [((1, 1), Piece Bishop White 0)];
        let expected = False;
        let result   = queryBishop starting (1, 1) (1, 8);
        result `shouldBe` expected

  -- queryKnight tests
  describe "queryKnight" $ do
    it "returns True for moving Knight northeast from (4, 4) to (5, 6)" $ do
      let starting = Map.fromList [((4, 4), Piece Knight Black 0)];
      let expected = True;
      let result   = queryKnight starting (4, 4) (5, 6);
      result `shouldBe` expected
    it "returns True for moving Knight southeast from (4, 4) to (5, 2)" $ do
      let starting = Map.fromList [((4, 4), Piece Knight Black 0)];
      let expected = True;
      let result   = queryKnight starting (4, 4) (5, 2);
      result `shouldBe` expected
    it "returns True for moving Knight southwest from (4, 4) to (3, 2)" $ do
      let starting = Map.fromList [((4, 4), Piece Knight Black 0)];
      let expected = True;
      let result   = queryKnight starting (4, 4) (3, 2);
      result `shouldBe` expected
    it "returns True for moving Knight northwest from (4, 4) to (3, 6)" $ do
      let starting = Map.fromList [((4, 4), Piece Knight Black 0)];
      let expected = True;
      let result   = queryKnight starting (4, 4) (3, 6);
      result `shouldBe` expected
    it "returns False when given invalid move" $ do
      let starting = Map.fromList [((4, 4), Piece Knight Black 0)];
      let expected = False;
      let result   = queryKnight starting (4, 4) (5, 5);
      result `shouldBe` expected

  -- queryRook tests
  describe "queryRook" $ do
    it  "returns True for moving Rook north from (1, 1) to (1, 8)" $ do
        let starting = Map.fromList [((1, 1), Piece Rook Black 0)];
        let expected = True;
        let result   = queryRook starting (1, 1) (1, 8);
        result `shouldBe` expected
    it  "returns True for moving Rook east from (1, 1) to (8, 1)" $ do
        let starting = Map.fromList [((1, 1), Piece Rook Black 0)];
        let expected = True;
        let result   = queryRook starting (1, 1) (8, 1);
        result `shouldBe` expected
    it  "returns True for moving Rook south from (8, 8) to (8, 1)" $ do
        let starting = Map.fromList [((8, 8), Piece Rook Black 0)];
        let expected = True;
        let result   = queryRook starting (8, 8) (8, 1);
        result `shouldBe` expected
    it  "returns True for moving Rook west from (8, 8) to (1, 8)" $ do
        let starting = Map.fromList [((8, 8), Piece Rook Black 0)];
        let expected = True;
        let result   = queryRook starting (8, 8) (1, 8);
        result `shouldBe` expected
    it  "returns False when given invalid move" $ do
        let starting = Map.fromList [((1, 1), Piece Rook Black 0)];
        let expected = False;
        let result   = queryRook starting (1, 1) (8, 8);
        result `shouldBe` expected

  -- queryPawnNormal tests
  describe "queryPawnNormal" $ do
    it  "returns True for moving Black pawn down from (1, 7) to (1, 6)" $ do
        let starting = Map.fromList [((1, 7), Piece Pawn Black 0)];
        let expected = True;
        let result   = queryPawnNormal starting (1, 7) (1, 6);
        result `shouldBe` expected
    it  "returns True for moving Black pawn down from (1, 7) to (1, 5)" $ do
        let starting = Map.fromList [((1, 7), Piece Pawn Black 0)];
        let expected = True;
        let result   = queryPawnNormal starting (1, 7) (1, 5);
        result `shouldBe` expected
    it  "returns False when Black pawn is moved two spaces when not on home rank" $ do
        let starting = Map.fromList [((1, 6), Piece Pawn Black 0)];
        let expected = False;
        let result   = queryPawnNormal starting (1, 6) (1, 4);
        result `shouldBe` expected
    it  "returns False when Black pawn is moved in wrong direction" $ do
        let starting = Map.fromList [((1, 4), Piece Pawn Black 0)];
        let expected = False;
        let result   = queryPawnNormal starting (1, 4) (1, 5)
        result `shouldBe` expected
    it  "returns True for moving Black pawn diagonally to take from (4, 4) to (3, 3)" $ do
        let starting = Map.fromList [((4, 4), Piece Pawn Black 0),
                                     ((3, 3), Piece Queen White 0)];
        let expected = True;
        let result   = queryPawnNormal starting (4, 4) (3, 3);
        result `shouldBe` expected
    it  "returns True for moving Black pawn diagonally to take from (4, 4) to (5, 3)" $ do
        let starting = Map.fromList [((4, 4), Piece Pawn Black 0),
                                     ((5, 3), Piece Queen White 0)];
        let expected = True;
        let result   = queryPawnNormal starting (4, 4) (5, 3);
        result `shouldBe` expected
    it  "returns False when moving diagonally (4, 4) to (3, 3)" $ do
        let starting = Map.fromList [((4, 4), Piece Pawn Black 0)];
        let expected = False;
        let result   = queryPawnNormal starting (4, 4) (3, 3);
        result `shouldBe` expected
    it  "returns False when moving diagonally (4, 4) to (5, 3)" $ do
        let starting = Map.fromList [((4, 4), Piece Pawn Black 0)];
        let expected = False;
        let result   = queryPawnNormal starting (4, 4) (5, 3);
        result `shouldBe` expected
    it  "returns True for moving White pawn up from (1, 2) to (1, 3)" $ do
        let starting = Map.fromList [((1, 2), Piece Pawn White 0)];
        let expected = True;
        let result   = queryPawnNormal starting (1, 2) (1, 3);
        result `shouldBe` expected
    it  "returns True for moving White pawn up from (1, 2) to (1, 4)" $ do
        let starting = Map.fromList [((1, 2), Piece Pawn White 0)];
        let expected = True;
        let result   = queryPawnNormal starting (1, 2) (1, 4);
        result `shouldBe` expected
    it  "returns False when White pawn is moved two spaces when not on home rank" $ do
        let starting = Map.fromList [((1, 3), Piece Pawn White 0)];
        let expected = False;
        let result   = queryPawnNormal starting (1, 3) (1, 5);
        result `shouldBe` expected
    it  "returns False when White pawn is moved in wrong direction" $ do
        let starting = Map.fromList [((1, 4), Piece Pawn White 0)];
        let expected = False;
        let result   = queryPawnNormal starting (1, 4) (1, 3)
        result `shouldBe` expected
    it  "returns True for moving White pawn diagonally to take from (4, 4) to (3, 5)" $ do
        let starting = Map.fromList [((4, 4), Piece Pawn White 0),
                                     ((3, 5), Piece Queen Black 0)];
        let expected = True;
        let result   = queryPawnNormal starting (4, 4) (3, 5);
        result `shouldBe` expected
    it  "returns True for moving White pawn diagonally to take from (4, 4) to (5, 5)" $ do
        let starting = Map.fromList [((4, 4), Piece Pawn White 0),
                                     ((5, 5), Piece Queen Black 0)];
        let expected = True;
        let result   = queryPawnNormal starting (4, 4) (5, 5);
        result `shouldBe` expected
    it  "returns False when moving diagonally (4, 4) to (3, 5)" $ do
        let starting = Map.fromList [((4, 4), Piece Pawn White 0)];
        let expected = False;
        let result   = queryPawnNormal starting (4, 4) (3, 5);
        result `shouldBe` expected
    it  "returns False when moving diagonally (4, 4) to (5, 5)" $ do
        let starting = Map.fromList [((4, 4), Piece Pawn White 0)];
        let expected = False;
        let result   = queryPawnNormal starting (4, 4) (5, 5);
        result `shouldBe` expected

  -- queryPawnEP tests
  describe "queryPawnEP" $ do 
    it  "lets Black pawn enpassant White pawn to right" $ do
        let starting = Map.fromList [((2, 4), Piece Pawn Black 5),
                                     ((3, 4), Piece Pawn White 6)];
        let expected = True;
        let result   = queryPawnEP starting 7 (2, 4) (3, 3);
        result `shouldBe` expected
    it  "lets Black pawn enpassant White pawn to left" $ do
        let starting = Map.fromList [((2, 4), Piece Pawn Black 5),
                                     ((1, 4), Piece Pawn White 6)];
        let expected = True; 
        let result   = queryPawnEP starting 7 (2, 4) (1, 3);
        result `shouldBe` expected
    it  "doesn't allow enpassant when moved from more than one move ago" $ do
        let starting = Map.fromList [((2, 4), Piece Pawn Black 5),
                                     ((1, 4), Piece Pawn White 6)];
        let expected = False;
        let result   = queryPawnEP starting 8 (2, 4) (1, 3);
        result `shouldBe` expected
    it  "doesn't allow enpassant without taken piece" $ do
        let starting = Map.fromList [((2, 4), Piece Pawn Black 5)];
        let expected = False;
        let result   = queryPawnEP starting 7 (2, 4) (1, 3);
        result `shouldBe` expected
    it  "doesn't allow enpassant on a non-Pawn" $ do
        let starting = Map.fromList [((2, 4), Piece Pawn Black 5),
                                     ((1, 4), Piece Queen White 6)];
        let expected = False;
        let result   = queryPawnEP starting 7 (2, 4) (1, 3);
        result `shouldBe` expected
    it  "doesn't allow enpassant when pawn is away from home rank" $ do
        let starting = Map.fromList [((2, 5), Piece Pawn Black 5),
                                     ((1, 5), Piece Pawn White 6)];
        let expected = False;
        let result   = queryPawnEP starting 7 (2, 5) (1, 4);
        result `shouldBe` expected
    it  "lets White pawn enpassant Black pawn to right" $ do
        let starting = Map.fromList [((2, 5), Piece Pawn White 5),
                                     ((3, 5), Piece Pawn Black 6)];
        let expected = True;
        let result   = queryPawnEP starting 7 (2, 5) (3, 6);
        result `shouldBe` expected
    it  "lets White pawn enpassant Black pawn to left" $ do
        let starting = Map.fromList [((2, 5), Piece Pawn White 5),
                                     ((1, 5), Piece Pawn Black 6)];
        let expected = True;
        let result   = queryPawnEP starting 7 (2, 5) (1, 6);
        result `shouldBe` expected
    it  "doesn't allow enpassant when pawn moved from more than one move ago" $ do
        let starting = Map.fromList [((2, 5), Piece Pawn Black 5),
                                     ((1, 5), Piece Pawn White 6)];
        let expected = False;
        let result   = queryPawnEP starting 8 (2, 5) (1, 6);
        result `shouldBe` expected
    it  "doesn't allow enpassant without taken piece" $ do
        let starting = Map.fromList [((2, 4), Piece Pawn White 5)];
        let expected = False;
        let result   = queryPawnEP starting 7 (2, 4) (1, 3);
        result `shouldBe` expected
    it  "doesn't allow enpassant on a non-Pawn" $ do
        let starting = Map.fromList [((2, 4), Piece Pawn White 5),
                                     ((1, 4), Piece Queen Black 6)];
        let expected = False;
        let result   = queryPawnEP starting 7 (2, 4) (1, 3);
        result `shouldBe` expected
    it  "doesn't allow enpassant when pawn is away from home rank" $ do
        let starting = Map.fromList [((2, 4), Piece Pawn Black 5),
                                     ((1, 4), Piece Pawn White 6)];
        let expected = False;
        let result   = queryPawnEP starting 7 (2, 4) (1, 5);
        result `shouldBe` expected

  -- queryPiece tests
  describe "movePiece" $ do
    it  "returns false for out of bounds to coordinate" $ do
      let starting = Map.fromList [((1, 8), Piece Pawn White 0)];
      let expected = False;
      let result   = queryPiece starting 0 (1, 8) (1, 9);
      result `shouldBe` expected
    it  "returns false for out of bounds from coordinate" $ do
      let starting = Map.fromList [((9, 2), Piece Pawn White 0)];
      let expected = False;
      let result   = queryPiece starting 0 (9, 2) (1, 1);
      result `shouldBe` expected
    it  "returns false for empty space" $ do
      let starting = Map.fromList [((1, 1), Piece Pawn White 0)];
      let expected = False;
      let result   = queryPiece starting 0 (1, 2) (3, 1);
      result `shouldBe` expected
    it  "returns false for out of bounds space" $ do
      let starting = Map.fromList [((1, 1), Piece Pawn White 0)];
      let expected = False;
      let result   = queryPiece starting 0 (9, 1) (1, 1);
      result `shouldBe` expected
    it  "returns false for move to piece of same color" $ do
      let starting = Map.fromList [((2, 1), Piece Queen White 0),
                                   ((3, 2), Piece Pawn White 0)];
      let expected = False; 
      let result = queryPiece starting 0 (2, 1) (3, 2);
      result `shouldBe` expected
