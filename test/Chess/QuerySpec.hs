module Chess.QuerySpec (spec) where

import Test.Hspec
import Test.HUnit
import Chess.Base
import Chess.Query
import Data.Map as Map

spec :: Spec
spec = do
  -- queryKing tests
  describe "queryKing" $ do
    it  "returns True for moving King north from (4, 4) to (4, 5)" $ do
        let starting = Map.fromList [((4, 4), Piece King Black 0)];
        let expected = True;
        let result = queryKing starting 0 (4, 4) (4, 5); 
        result `shouldBe` expected
    it  "returns True for moving King northeast from (4, 4) to (5, 5)" $ do
        let starting = Map.fromList [((4, 4), Piece King Black 0)];
        let expected = False;
        let result = queryKing starting 0 (4, 4) (5, 5); 
        result `shouldBe` expected
    it  "returns True for moving King east from (4, 4) to (5, 4)" $ do
        let starting = Map.fromList [((4, 4), Piece King Black 0)];
        let expected = True;
        let result = moveKing starting 0 (4, 4) (5, 4); 
        result `shouldBe` expected
    it  "returns True for moving King southeast from (4, 4) to (5, 3)" $ do
        let starting = Map.fromList [((4, 4), Piece King Black 0)];
        let expected = True;
        let result = moveKing starting 0 (4, 4) (5, 3); 
        result `shouldBe` expected
    it "returns True for moving King south from (4, 4) to (4, 3)" $ do
        let starting = Map.fromList [((4, 4), Piece King Black 0)];
        let expected = True;
        let result = moveKing starting 0 (4, 4) (4, 3); 
        result `shouldBe` expected
    it  "returns True for moving King southwest from (4, 4) to (3, 3)" $ do
        let starting = Map.fromList [((4, 4), Piece King Black 0)];
        let expected = True;
        let result = moveKing starting 0 (4, 4) (3, 3); 
        result `shouldBe` expected
    it  "returns True for moving  King west from (4, 4) to (3, 4)" $ do
        let starting = Map.fromList [((4, 4), Piece King Black 0)];
        let expected = True;
        let result = moveKing starting 0 (4, 4) (3, 4); 
        expected `shouldBe` expected
    it  "returns True for moving King northwest from (4, 4) to (3, 5)" $ do
        let starting = Map.fromList [((4, 4), Piece King Black 0)];
        let expected = True;
        let result = moveKing starting 0 (4, 4) (3, 5); 
        result `shouldBe` expected
    it  "returns False when given out of bounds move" $ do
        let starting = Map.fromList [((4, 4), Piece King Black 0)];
        let expected = False;
        let result = moveKing starting 0 (4, 4) (6, 4); 
        result `shouldBe` expected

  -- moveQueen tests
  describe "moveQueen" $ do
    it  "returns True for moving Queen north across the board" $ do
        let starting = Map.fromList [((1, 1), Piece Queen White 0)];
        let expected = True; 
        let result   = moveQueen starting 0 (1, 1) (1, 8);
        result `shouldBe` expected
    it  "returns True for moving Queen south across the board" $ do
        let starting = Map.fromList [((1, 8), Piece Queen White 0)];
        let expected = Map.fromList [((1, 1), Piece Queen White 0)];
        let result   = moveQueen starting 0 (1, 8) (1, 1);
        case result of 
          Just a -> a `shouldBe` expected
          Nothing -> assertFailure "got Nothing from moveQueen"
    it  "moves Queen east across the board" $ do
        let starting = Map.fromList [((1, 1), Piece Queen White 0)];
        let expected = Map.fromList [((8, 1), Piece Queen White 0)];
        let result   = moveQueen starting 0 (1, 1) (8, 1);
        case result of
          Just a -> a `shouldBe` expected
          Nothing -> assertFailure "got Nothing from moveQueen"
    it  "moves Queen west across the board" $ do
        let starting = Map.fromList [((8, 1), Piece Queen White 0)];
        let expected = Map.fromList [((1, 1), Piece Queen White 0)];
        let result   = moveQueen starting 0 (8, 1) (1, 1);
        case result of
          Just a -> a `shouldBe` expected
          Nothing -> assertFailure "got Nothing from moveQueen"
    it  "moves Queen northeast across the board" $ do
        let starting = Map.fromList [((1, 1), Piece Queen White 0)];
        let expected = Map.fromList [((8, 8), Piece Queen White 0)];
        let result   = moveQueen starting 0 (1, 1) (8, 8);
        case result of
          Just a -> a `shouldBe` expected
          Nothing -> assertFailure "got Nothing from moveQueen"
    it  "moves Queen northwest across the board" $ do 
        let starting = Map.fromList [((8, 1), Piece Queen White 0)];
        let expected = Map.fromList [((1, 8), Piece Queen White 0)];
        let result   = moveQueen starting 0 (8, 1) (1, 8);
        case result of
          Just a -> a `shouldBe` expected
          Nothing -> assertFailure "got Nothing from moveQueen"
    it  "moves Queen southeast across the board" $ do
        let starting = Map.fromList [((1, 8), Piece Queen White 0)];
        let expected = Map.fromList [((8, 1), Piece Queen White 0)];
        let result   = moveQueen starting 0 (1, 8) (8, 1);
        case result of
          Just a -> a `shouldBe` expected
          Nothing -> assertFailure "got Nothing from moveQueen"
    it  "moves Queen southwest across the board" $ do
        let starting = Map.fromList [((8, 8), Piece Queen White 0)];
        let expected = Map.fromList [((1, 1), Piece Queen White 0)];
        let result   = moveQueen starting 0 (8, 8) (1, 1);
        case result of
          Just a -> a `shouldBe` expected
          Nothing -> assertFailure "got Nothing from moveQueen" 
    it  "returns Nothing when given invalid move" $ do
        let starting = Map.fromList [((1, 1), Piece Queen White 0)];
        let result   = moveQueen starting 0 (1, 1) (2, 8);
        case result of
          Just a -> assertFailure "moved Queen with invalid move"
          Nothing -> assertBool "shouldn't fail" True

  -- moveBishop tests
  describe "moveBishop" $ do
    it  "moves Bishop northeast across the board" $ do
        let starting = Map.fromList [((1, 1), Piece Bishop White 0)];
        let expected = Map.fromList [((8, 8), Piece Bishop White 0)];
        let result   = moveBishop starting 0 (1, 1) (8, 8);
        case result of
          Just a -> a `shouldBe` expected
          Nothing -> assertFailure "got Nothing from moveBishop"
    it  "moves Bishop northwest across the board" $ do 
        let starting = Map.fromList [((8, 1), Piece Bishop White 0)];
        let expected = Map.fromList [((1, 8), Piece Bishop White 0)];
        let result   = moveBishop starting 0 (8, 1) (1, 8);
        case result of
          Just a -> a `shouldBe` expected
          Nothing -> assertFailure "got Nothing from moveBishop"
    it  "moves Bishop southeast across the board" $ do
        let starting = Map.fromList [((1, 8), Piece Bishop White 0)];
        let expected = Map.fromList [((8, 1), Piece Bishop White 0)];
        let result   = moveBishop starting 0 (1, 8) (8, 1);
        case result of
          Just a -> a `shouldBe` expected
          Nothing -> assertFailure "got Nothing from moveBishop"
    it  "moves Bishop southwest across the board" $ do
        let starting = Map.fromList [((8, 8), Piece Bishop White 0)];
        let expected = Map.fromList [((1, 1), Piece Bishop White 0)];
        let result   = moveBishop starting 0 (8, 8) (1, 1);
        case result of
          Just a -> a `shouldBe` expected
          Nothing -> assertFailure "got Nothing from moveBishop" 
    it  "returns Nothing when given invalid move" $ do
        let starting = Map.fromList [((1, 1), Piece Bishop White 0)];
        let result   = moveBishop starting 0 (1, 1) (1, 8);
        case result of
          Just a -> assertFailure "moved Bishop with invalid move"
          Nothing -> assertBool "shouldn't fail" True

  -- move Knight tests
  describe "moveKnight" $ do
    it "moves Knight northeast from (4, 4) to (5, 6)" $ do
      let starting = Map.fromList [((4, 4), Piece Knight Black 0)];
      let expected = Map.fromList [((5, 6), Piece Knight Black 0)];
      let result   = moveKnight starting 0 (4, 4) (5, 6);
      case result of
        Just a -> a `shouldBe` expected
        Nothing -> assertFailure "got Nothing from moveKnight"
    it "moves Knight southeast from (4, 4) to (5, 2)" $ do
      let starting = Map.fromList [((4, 4), Piece Knight Black 0)];
      let expected = Map.fromList [((5, 2), Piece Knight Black 0)];
      let result   = moveKnight starting 0 (4, 4) (5, 2);
      case result of
        Just a -> a `shouldBe` expected
        Nothing -> assertFailure "got Nothing from moveKnight"
    it "moves Knight southwest from (4, 4) to (3, 2)" $ do
      let starting = Map.fromList [((4, 4), Piece Knight Black 0)];
      let expected = Map.fromList [((3, 2), Piece Knight Black 0)];
      let result   = moveKnight starting 0 (4, 4) (3, 2);
      case result of
        Just a -> a `shouldBe` expected
        Nothing -> assertFailure "got Nothing from moveKnight"
    it "moves Knight northwest from (4, 4) to (3, 6)" $ do
      let starting = Map.fromList [((4, 4), Piece Knight Black 0)];
      let expected = Map.fromList [((3, 6), Piece Knight Black 0)];
      let result   = moveKnight starting 0 (4, 4) (3, 6);
      case result of
        Just a -> a `shouldBe` expected
        Nothing -> assertFailure "got Nothing from moveKnight"
    it "returns Nothing when given invalid move" $ do
      let starting = Map.fromList [((4, 4), Piece Knight Black 0)];
      let result   = moveKnight starting 0 (4, 4) (5, 5);
      case result of
        Just a -> assertFailure "moved Knight with invalid move"
        Nothing -> assertBool "shouldn't fail" True

  -- moveRook tests
  describe "moveRook" $ do
    it  "moves Rook north from (1, 1) to (1, 8)" $ do
        let starting = Map.fromList [((1, 1), Piece Rook Black 0)];
        let expected = Map.fromList [((1, 8), Piece Rook Black 0)];
        let result   = moveRook starting 0 (1, 1) (1, 8);
        case result of
          Just a -> a `shouldBe` expected
          Nothing -> assertFailure "got Nothing from moveRook"
    it  "moves Rook east from (1, 1) to (8, 1)" $ do
        let starting = Map.fromList [((1, 1), Piece Rook Black 0)];
        let expected = Map.fromList [((8, 1), Piece Rook Black 0)];
        let result   = moveRook starting 0 (1, 1) (8, 1);
        case result of
          Just a -> a `shouldBe` expected
          Nothing -> assertFailure "got Nothing from moveRook"
    it  "moves Rook south from (8, 8) to (8, 1)" $ do
        let starting = Map.fromList [((8, 8), Piece Rook Black 0)];
        let expected = Map.fromList [((8, 1), Piece Rook Black 0)];
        let result   = moveRook starting 0 (8, 8) (8, 1);
        case result of
          Just a -> a `shouldBe` expected
          Nothing -> assertFailure "got Nothing from moveRook"
    it  "moves Rook west from (8, 8) to (1, 8)" $ do
        let starting = Map.fromList [((8, 8), Piece Rook Black 0)];
        let expected = Map.fromList [((1, 8), Piece Rook Black 0)];
        let result   = moveRook starting 0 (8, 8) (1, 8);
        case result of
          Just a -> a `shouldBe` expected
          Nothing -> assertFailure "got Nothing from moveRook"
    it  "returns Nothing when given invalid move" $ do
        let starting = Map.fromList [((1, 1), Piece Rook Black 0)];
        let result   = moveRook starting 0 (1, 1) (8, 8);
        case result of
          Just a -> assertFailure "moved Rook with invalid move"
          Nothing -> assertBool "shouldn't fail" True

  -- movePawn tests
  describe "movePawn" $ do
    it  "moves Black pawn down from (1, 7) to (1, 6)" $ do
        let starting = Map.fromList [((1, 7), Piece Pawn Black 0)];
        let expected = Map.fromList [((1, 6), Piece Pawn Black 0)];
        let result   = movePawn starting 0 (1, 7) (1, 6);
        case result of
          Just a -> a `shouldBe` expected
          Nothing -> assertFailure "got Nothing from movePawn"
    it  "moves Black pawn down from (1, 7) to (1, 5)" $ do
        let starting = Map.fromList [((1, 7), Piece Pawn Black 0)];
        let expected = Map.fromList [((1, 5), Piece Pawn Black 0)];
        let result   = movePawn starting 0 (1, 7) (1, 5);
        case result of
          Just a -> a `shouldBe` expected
          Nothing -> assertFailure "got Nothing from movePawn"
    it  "returns Nothing when Black pawn is moved two spaces when not on home rank" $ do
        let starting = Map.fromList [((1, 6), Piece Pawn Black 0)];
        let result   = movePawn starting 0 (1, 6) (1, 4);
        case result of
          Just a -> assertFailure "moved Pawn with invalid move"
          Nothing -> assertBool "shouldn't fail" True
    it  "returns Nothing when Black pawn is moved in wrong direction" $ do
        let starting = Map.fromList [((1, 4), Piece Pawn Black 0)];
        let result   = movePawn starting 0 (1, 4) (1, 5)
        case result of
          Just a -> assertFailure "moved Pawn with invalid move"
          Nothing -> assertBool "shouldn't fail" True
    it  "moves Black pawn diagonally to take from (4, 4) to (3, 3)" $ do
        let starting = Map.fromList [((4, 4), Piece Pawn Black 0),
                                     ((3, 3), Piece Queen White 0)];
        let expected = Map.fromList [((3, 3), Piece Pawn Black 0)];
        let result   = movePawn starting 0 (4, 4) (3, 3);
        case result of 
          Just a -> a `shouldBe` expected
          Nothing -> assertFailure "got Nothing from movePawn" 
    it  "moves Black pawn diagonally to take from (4, 4) to (5, 3)" $ do
        let starting = Map.fromList [((4, 4), Piece Pawn Black 0),
                                     ((5, 3), Piece Queen White 0)];
        let expected = Map.fromList [((5, 3), Piece Pawn Black 0)];
        let result   = movePawn starting 0 (4, 4) (5, 3);
        case result of 
          Just a -> a `shouldBe` expected
          Nothing -> assertFailure "got Nothing from movePawn" 
    it  "returns Nothing when moving diagonally (4, 4) to (3, 3)" $ do
        let starting = Map.fromList [((4, 4), Piece Pawn Black 0)];
        let result   = movePawn starting 0 (4, 4) (3, 3);
        case result of 
          Just a -> assertFailure "moved Pawn with invalid move"
          Nothing -> assertBool "shouldn't fail" True
    it  "returns Nothing when moving diagonally (4, 4) to (5, 3)" $ do
        let starting = Map.fromList [((4, 4), Piece Pawn Black 0)];
        let result   = movePawn starting 0 (4, 4) (5, 3);
        case result of 
          Just a -> assertFailure "moved Pawn with invalid move"
          Nothing -> assertBool "shouldn't fail" True
    it  "lets Black pawn enpassant White pawn to right" $ do
        let starting = Map.fromList [((2, 4), Piece Pawn Black 5),
                                     ((3, 4), Piece Pawn White 6)];
        let expected = Map.fromList [((3, 3), Piece Pawn Black 7)];
        let result   = movePawn starting 7 (2, 4) (3, 3);
        case result of
          Just a -> a `shouldBe` expected
          Nothing -> assertFailure "got Nothing from movePawn"
    it  "lets Black pawn enpassant White pawn to left" $ do
        let starting = Map.fromList [((2, 4), Piece Pawn Black 5),
                                     ((1, 4), Piece Pawn White 6)];
        let expected = Map.fromList [((1, 3), Piece Pawn Black 7)];
        let result   = movePawn starting 7 (2, 4) (1, 3);
        case result of
          Just a -> a `shouldBe` expected
          Nothing -> assertFailure "got Nothing from movePawn"
    it  "doesn't allow enpassant when pawn moved from more than one move ago" $ do
        let starting = Map.fromList [((2, 4), Piece Pawn Black 5),
                                     ((1, 4), Piece Pawn White 6)];
        let result   = movePawn starting 8 (2, 4) (1, 3);
        case result of
          Just a -> assertFailure "moved Pawn with invalid move"
          Nothing -> assertBool "shouldn't fail" True
    it  "doesn't allow enpassant when pawn is away from home rank" $ do
        let starting = Map.fromList [((2, 5), Piece Pawn Black 5),
                                     ((1, 5), Piece Pawn White 6)];
        let result   = movePawn starting 7 (2, 5) (1, 4);
        case result of
          Just a -> assertFailure "moved Pawn with invalid move"
          Nothing -> assertBool "shouldn't fail" True
    it  "moves White pawn up from (1, 2) to (1, 3)" $ do
        let starting = Map.fromList [((1, 2), Piece Pawn White 0)];
        let expected = Map.fromList [((1, 3), Piece Pawn White 0)];
        let result   = movePawn starting 0 (1, 2) (1, 3);
        case result of
          Just a -> a `shouldBe` expected
          Nothing -> assertFailure "got Nothing from movePawn"
    it  "moves White pawn up from (1, 2) to (1, 4)" $ do
        let starting = Map.fromList [((1, 2), Piece Pawn White 0)];
        let expected = Map.fromList [((1, 4), Piece Pawn White 0)];
        let result   = movePawn starting 0 (1, 2) (1, 4);
        case result of
          Just a -> a `shouldBe` expected
          Nothing -> assertFailure "got Nothing from movePawn"
    it  "returns Nothing when White pawn is moved two spaces when not on home rank" $ do
        let starting = Map.fromList [((1, 3), Piece Pawn White 0)];
        let result   = movePawn starting 0 (1, 3) (1, 5);
        case result of
          Just a -> assertFailure "moved Pawn with invalid move"
          Nothing -> assertBool "shouldn't fail" True
    it  "returns Nothing when White pawn is moved in wrong direction" $ do
        let starting = Map.fromList [((1, 4), Piece Pawn White 0)];
        let result   = movePawn starting 0 (1, 4) (1, 3)
        case result of
          Just a -> assertFailure "moved Pawn with invalid move"
          Nothing -> assertBool "shouldn't fail" True
    it  "moves White pawn diagonally to take from (4, 4) to (3, 5)" $ do
        let starting = Map.fromList [((4, 4), Piece Pawn White 0),
                                     ((3, 5), Piece Queen Black 0)];
        let expected = Map.fromList [((3, 5), Piece Pawn White 0)];
        let result   = movePawn starting 0 (4, 4) (3, 5);
        case result of 
          Just a -> a `shouldBe` expected
          Nothing -> assertFailure "got Nothing from movePawn" 
    it  "moves White pawn diagonally to take from (4, 4) to (5, 5)" $ do
        let starting = Map.fromList [((4, 4), Piece Pawn White 0),
                                     ((5, 5), Piece Queen Black 0)];
        let expected = Map.fromList [((5, 5), Piece Pawn White 0)];
        let result   = movePawn starting 0 (4, 4) (5, 5);
        case result of 
          Just a -> a `shouldBe` expected
          Nothing -> assertFailure "got Nothing from movePawn" 
    it  "returns Nothing when moving diagonally (4, 4) to (3, 5)" $ do
        let starting = Map.fromList [((4, 4), Piece Pawn White 0)];
        let result   = movePawn starting 0 (4, 4) (3, 5);
        case result of 
          Just a -> assertFailure "moved Pawn with invalid move"
          Nothing -> assertBool "shouldn't fail" True
    it  "returns Nothing when moving diagonally (4, 4) to (5, 5)" $ do
        let starting = Map.fromList [((4, 4), Piece Pawn White 0)];
        let result   = movePawn starting 0 (4, 4) (5, 5);
        case result of 
          Just a -> assertFailure "moved Pawn with invalid move"
          Nothing -> assertBool "shouldn't fail" True
    it  "lets White pawn enpassant Black pawn to right" $ do
        let starting = Map.fromList [((2, 5), Piece Pawn White 5),
                                     ((3, 5), Piece Pawn Black 6)];
        let expected = Map.fromList [((3, 6), Piece Pawn White 7)];
        let result   = movePawn starting 7 (2, 5) (3, 6);
        case result of
          Just a -> a `shouldBe` expected
          Nothing -> assertFailure "got Nothing from movePawn"
    it  "lets White pawn enpassant Black pawn to left" $ do
        let starting = Map.fromList [((2, 5), Piece Pawn White 5),
                                     ((1, 5), Piece Pawn Black 6)];
        let expected = Map.fromList [((1, 6), Piece Pawn White 7)];
        let result   = movePawn starting 7 (2, 5) (1, 6);
        case result of
          Just a -> a `shouldBe` expected
          Nothing -> assertFailure "got Nothing from movePawn"
    it  "doesn't allow enpassant when pawn moved from more than one move ago" $ do
        let starting = Map.fromList [((2, 5), Piece Pawn Black 5),
                                     ((1, 5), Piece Pawn White 6)];
        let result   = movePawn starting 8 (2, 5) (1, 6);
        case result of
          Just a -> assertFailure "moved Pawn with invalid move"
          Nothing -> assertBool "shouldn't fail" True
    it  "doesn't allow enpassant when pawn is away from home rank" $ do
        let starting = Map.fromList [((2, 4), Piece Pawn Black 5),
                                     ((1, 4), Piece Pawn White 6)];
        let result   = movePawn starting 7 (2, 4) (1, 5);
        case result of
          Just a -> assertFailure "moved Pawn with invalid move"
          Nothing -> assertBool "shouldn't fail" True

  -- movePiece tests
  describe "movePiece" $ do
    it  "refuses to move a piece out of bounds" $ do
      let starting = Map.fromList [((1, 8), Piece Pawn White 0)];
      let result   = movePiece starting 0 (1, 8) (1, 9);
      case result of
        Just a -> assertFailure "moved out of bounds"
        Nothing -> assertBool "shouldn't fail" True
    it  "refuses to move a piece from out of bounds" $ do
      let starting = Map.fromList [((9, 2), Piece Pawn White 0)];
      let result   = movePiece starting 0 (9, 2) (1, 1);
      case result of
        Just a -> assertFailure "moved piece from out of bounds"
        Nothing -> assertBool "shouldn't fail" True
    it  "refuses to move empty space" $ do
      let starting = Map.fromList [((1, 1), Piece Pawn White 0)];
      let result = movePiece starting 0 (1, 2) (3, 1);
      case result of
        Just a -> assertFailure "moved empty space"
        Nothing -> assertBool "shouldn't fail" True
    it  "refuses to move out of bounds space" $ do
      let starting = Map.fromList [((1, 1), Piece Pawn White 0)];
      let result = movePiece starting 0 (9, 1) (1, 1);
      case result of
        Just a -> assertFailure "moved out of bounds space"
        Nothing -> assertBool "shouldn't fail" True 
    it  "refuses to take piece of same color" $ do
      let starting = Map.fromList [((2, 1), Piece Queen White 0),
                                   ((3, 2), Piece Pawn White 0)];
      let result = movePiece starting 0 (2, 1) (3, 2);
      case result of
        Just a -> assertFailure "took piece of same color"
        Nothing -> assertBool "shouldn't fail" True
