module Chess.ActionsSpec (spec) where

import Test.Hspec
import Test.HUnit
import Chess.Base
import Chess.Actions
import Data.Map as Map

spec :: Spec
spec = do
  -- move tests
  describe "move" $ do
    it  "moves pawn from (1, 1) to (8, 2)" $ do
        let starting = Map.fromList [((1, 1), Piece Pawn Black)];
        let expected = Map.fromList [((8, 2), Piece Pawn Black)];
        let result   = move starting (1, 1) (8, 2);
        case result of
          Just a -> a `shouldBe` expected
          Nothing -> assertFailure "got Nothing from move"
    it  "moves pawn from (1, 1) to (8, 2) when piece exists at (8, 2)" $ do
        let starting = Map.fromList [((1, 1), Piece Pawn Black),
                                     ((8, 2), Piece Queen Black)];
        let expected = Map.fromList [((8, 2), Piece Pawn Black)];
        let result = move starting (1, 1) (8, 2);
        case result of
          Just a -> a `shouldBe` expected
          Nothing -> assertFailure "got Nothing from move"
    it  "moves pawn from (1, 1) to (3, 4) without touching other pieces" $ do
        let starting = Map.fromList [((1, 1), Piece Pawn Black),
                                     ((1, 2), Piece Pawn White)];
        let expected = Map.fromList [((3, 4), Piece Pawn Black),
                                     ((1, 2), Piece Pawn White)];
        let result = move starting (1, 1) (3, 4);
        case result of
          Just a -> a `shouldBe` expected
          Nothing -> assertFailure "got Nothing from move"

  -- moveKing tests
  describe "moveKing" $ do
    it  "moves King north from (4, 4) to (4, 5)" $ do
        let starting = Map.fromList [((4, 4), Piece King Black)];
        let expected = Map.fromList [((4, 5), Piece King Black)];
        let result = moveKing starting (4, 4) (4, 5); 
        case result of
          Just a -> a `shouldBe` expected
          Nothing -> assertFailure "got Nothing from moveKing"
    it  "moves King northeast from (4, 4) to (5, 5)" $ do
        let starting = Map.fromList [((4, 4), Piece King Black)];
        let expected = Map.fromList [((5, 5), Piece King Black)];
        let result = moveKing starting (4, 4) (5, 5); 
        case result of
          Just a -> a `shouldBe` expected
          Nothing -> assertFailure "got Nothing from moveKing"
    it  "moves King east from (4, 4) to (5, 4)" $ do
        let starting = Map.fromList [((4, 4), Piece King Black)];
        let expected = Map.fromList [((5, 4), Piece King Black)];
        let result = moveKing starting (4, 4) (5, 4); 
        case result of
          Just a -> a `shouldBe` expected
          Nothing -> assertFailure "got Nothing from moveKing"
    it  "moves King southeast from (4, 4) to (5, 3)" $ do
        let starting = Map.fromList [((4, 4), Piece King Black)];
        let expected = Map.fromList [((5, 3), Piece King Black)];
        let result = moveKing starting (4, 4) (5, 3); 
        case result of
          Just a -> a `shouldBe` expected
          Nothing -> assertFailure "got Nothing from moveKing"
    it "moves King south from (4, 4) to (4, 3)" $ do
        let starting = Map.fromList [((4, 4), Piece King Black)];
        let expected = Map.fromList [((4, 3), Piece King Black)];
        let result = moveKing starting (4, 4) (4, 3); 
        case result of
          Just a -> a `shouldBe` expected
          Nothing -> assertFailure "got Nothing from moveKing"
    it  "moves King southwest from (4, 4) to (3, 3)" $ do
        let starting = Map.fromList [((4, 4), Piece King Black)];
        let expected = Map.fromList [((3, 3), Piece King Black)];
        let result = moveKing starting (4, 4) (3, 3); 
        case result of
          Just a -> a `shouldBe` expected
          Nothing -> assertFailure "got Nothing from moveKing"
    it  "moves King west from (4, 4) to (3, 4)" $ do
        let starting = Map.fromList [((4, 4), Piece King Black)];
        let expected = Map.fromList [((3, 4), Piece King Black)];
        let result = moveKing starting (4, 4) (3, 4); 
        case result of
          Just a -> a `shouldBe` expected
          Nothing -> assertFailure "got Nothing from moveKing"
    it  "moves King northwest from (4, 4) to (3, 5)" $ do
        let starting = Map.fromList [((4, 4), Piece King Black)];
        let expected = Map.fromList [((3, 5), Piece King Black)];
        let result = moveKing starting (4, 4) (3, 5); 
        case result of
          Just a -> a `shouldBe` expected
          Nothing -> assertFailure "got Nothing from moveKing"
    it  "returns Nothing when given out of bounds move" $ do
        let starting = Map.fromList [((4, 4), Piece King Black)];
        let result = moveKing starting (4, 4) (6, 4); 
        case result of
          Just a -> assertFailure "moved King with invalid move" 
          Nothing -> assertBool "shouldn't fail" True

  -- moveQueen tests
  describe "moveQueen" $ do
    it  "moves Queen north across the board" $ do
        let starting = Map.fromList [((1, 1), Piece Queen White)];
        let expected = Map.fromList [((1, 8), Piece Queen White)];
        let result   = moveQueen starting (1, 1) (1, 8);
        case result of
          Just a -> a `shouldBe` expected
          Nothing -> assertFailure "got Nothing from moveQueen"
    it  "moves Queen south across the board" $ do
        let starting = Map.fromList [((1, 8), Piece Queen White)];
        let expected = Map.fromList [((1, 1), Piece Queen White)];
        let result   = moveQueen starting (1, 8) (1, 1);
        case result of 
          Just a -> a `shouldBe` expected
          Nothing -> assertFailure "got Nothing from moveQueen"
    it  "moves Queen east across the board" $ do
        let starting = Map.fromList [((1, 1), Piece Queen White)];
        let expected = Map.fromList [((8, 1), Piece Queen White)];
        let result   = moveQueen starting (1, 1) (8, 1);
        case result of
          Just a -> a `shouldBe` expected
          Nothing -> assertFailure "got Nothing from moveQueen"
    it  "moves Queen west across the board" $ do
        let starting = Map.fromList [((8, 1), Piece Queen White)];
        let expected = Map.fromList [((1, 1), Piece Queen White)];
        let result   = moveQueen starting (8, 1) (1, 1);
        case result of
          Just a -> a `shouldBe` expected
          Nothing -> assertFailure "got Nothing from moveQueen"
    it  "moves Queen northeast across the board" $ do
        let starting = Map.fromList [((1, 1), Piece Queen White)];
        let expected = Map.fromList [((8, 8), Piece Queen White)];
        let result   = moveQueen starting (1, 1) (8, 8);
        case result of
          Just a -> a `shouldBe` expected
          Nothing -> assertFailure "got Nothing from moveQueen"
    it  "moves Queen northwest across the board" $ do 
        let starting = Map.fromList [((8, 1), Piece Queen White)];
        let expected = Map.fromList [((1, 8), Piece Queen White)];
        let result   = moveQueen starting (8, 1) (1, 8);
        case result of
          Just a -> a `shouldBe` expected
          Nothing -> assertFailure "got Nothing from moveQueen"
    it  "moves Queen southeast across the board" $ do
        let starting = Map.fromList [((1, 8), Piece Queen White)];
        let expected = Map.fromList [((8, 1), Piece Queen White)];
        let result   = moveQueen starting (1, 8) (8, 1);
        case result of
          Just a -> a `shouldBe` expected
          Nothing -> assertFailure "got Nothing from moveQueen"
    it  "moves Queen southwest across the board" $ do
        let starting = Map.fromList [((8, 8), Piece Queen White)];
        let expected = Map.fromList [((1, 1), Piece Queen White)];
        let result   = moveQueen starting (8, 8) (1, 1);
        case result of
          Just a -> a `shouldBe` expected
          Nothing -> assertFailure "got Nothing from moveQueen" 
    it  "returns Nothing when given invalid move" $ do
        let starting = Map.fromList [((1, 1), Piece Queen White)];
        let result   = moveQueen starting (1, 1) (2, 8);
        case result of
          Just a -> assertFailure "moved Queen with invalid move"
          Nothing -> assertBool "shouldn't fail" True

  -- moveBishop tests
  describe "moveBishop" $ do
    it  "moves Bishop northeast across the board" $ do
        let starting = Map.fromList [((1, 1), Piece Bishop White)];
        let expected = Map.fromList [((8, 8), Piece Bishop White)];
        let result   = moveBishop starting (1, 1) (8, 8);
        case result of
          Just a -> a `shouldBe` expected
          Nothing -> assertFailure "got Nothing from moveBishop"
    it  "moves Bishop northwest across the board" $ do 
        let starting = Map.fromList [((8, 1), Piece Bishop White)];
        let expected = Map.fromList [((1, 8), Piece Bishop White)];
        let result   = moveBishop starting (8, 1) (1, 8);
        case result of
          Just a -> a `shouldBe` expected
          Nothing -> assertFailure "got Nothing from moveBishop"
    it  "moves Bishop southeast across the board" $ do
        let starting = Map.fromList [((1, 8), Piece Bishop White)];
        let expected = Map.fromList [((8, 1), Piece Bishop White)];
        let result   = moveBishop starting (1, 8) (8, 1);
        case result of
          Just a -> a `shouldBe` expected
          Nothing -> assertFailure "got Nothing from moveBishop"
    it  "moves Bishop southwest across the board" $ do
        let starting = Map.fromList [((8, 8), Piece Bishop White)];
        let expected = Map.fromList [((1, 1), Piece Bishop White)];
        let result   = moveBishop starting (8, 8) (1, 1);
        case result of
          Just a -> a `shouldBe` expected
          Nothing -> assertFailure "got Nothing from moveBishop" 
    it  "returns Nothing when given invalid move" $ do
        let starting = Map.fromList [((1, 1), Piece Bishop White)];
        let result   = moveBishop starting (1, 1) (1, 8);
        case result of
          Just a -> assertFailure "moved Bishop with invalid move"
          Nothing -> assertBool "shouldn't fail" True

  -- move Knight tests
  describe "moveKnight" $ do
    it "moves Knight northeast from (4, 4) to (5, 6)" $ do
      let starting = Map.fromList [((4, 4), Piece Knight Black)];
      let expected = Map.fromList [((5, 6), Piece Knight Black)];
      let result   = moveBishop starting (4, 4) (5, 6);
      case result of
        Just a -> a `shouldBe` expected
        Nothing -> assertFailure "got Nothing from moveKnight"
    it "moves Knight southeast from (4, 4) to (5, 2)" $ do
      let starting = Map.fromList [((4, 4), Piece Knight Black)];
      let expected = Map.fromList [((5, 2), Piece Knight Black)];
      let result   = moveBishop starting (4, 4) (5, 2);
      case result of
        Just a -> a `shouldBe` expected
        Nothing -> assertFailure "got Nothing from moveKnight"
    it "moves Knight southwest from (4, 4) to (3, 2)" $ do
      let starting = Map.fromList [((4, 4), Piece Knight Black)];
      let expected = Map.fromList [((3, 2), Piece Knight Black)];
      let result   = moveBishop starting (4, 4) (3, 2);
      case result of
        Just a -> a `shouldBe` expected
        Nothing -> assertFailure "got Nothing from moveKnight"
    it "moves Knight northwest from (4, 4) to (3, 6)" $ do
      let starting = Map.fromList [((4, 4), Piece Knight Black)];
      let expected = Map.fromList [((3, 6), Piece Knight Black)];
      let result   = moveBishop starting (4, 4) (3, 6);
      case result of
        Just a -> a `shouldBe` expected
        Nothing -> assertFailure "got Nothing from moveKnight"
    it "returns Nothing when given invalid move" $ do
      let starting = Map.fromList [((4, 4), Piece Knight Black)];
      let result   = moveBishop starting (4, 4) (5, 5);
      case result of
        Just a -> assertFailure "moved Knight with invalid move"
        Nothing -> assertBool "shouldn't fail" True

  -- moveRook tests
  describe "moveRook" $ do
    it  "moves Rook north from (1, 1) to (1, 8)" $ do
        let starting = Map.fromList [((1, 1), Piece Rook Black)];
        let expected = Map.fromList [((1, 8), Piece Rook Black)];
        let result   = moveRook starting (1, 1) (1, 8);
        case result of
          Just a -> a `shouldBe` expected
          Nothing -> assertFailure "got Nothing from moveRook"
    it  "moves Rook east from (1, 1) to (8, 1)" $ do
        let starting = Map.fromList [((1, 1), Piece Rook Black)];
        let expected = Map.fromList [((8, 1), Piece Rook Black)];
        let result   = moveRook starting (1, 1) (8, 1);
        case result of
          Just a -> a `shouldBe` expected
          Nothing -> assertFailure "got Nothing from moveRook"
    it  "moves Rook south from (8, 8) to (8, 1)" $ do
        let starting = Map.fromList [((8, 8), Piece Rook Black)];
        let expected = Map.fromList [((8, 1), Piece Rook Black)];
        let result   = moveRook starting (8, 8) (8, 1);
        case result of
          Just a -> a `shouldBe` expected
          Nothing -> assertFailure "got Nothing from moveRook"
    it  "moves Rook west from (8, 8) to (1, 8)" $ do
        let starting = Map.fromList [((8, 8), Piece Rook Black)];
        let expected = Map.fromList [((1, 8), Piece Rook Black)];
        let result   = moveRook starting (8, 8) (1, 8);
        case result of
          Just a -> a `shouldBe` expected
          Nothing -> assertFailure "got Nothing from moveRook"
    it  "returns Nothing when given invalid move" $ do
        let starting = Map.fromList [((1, 1), Piece Rook Black)];
        let result   = moveRook starting (1, 1) (8, 8);
        case result of
          Just a -> assertFailure "moved Rook with invalid move"
          Nothing -> assertBool "shouldn't fail" True

  -- movePawn tests
  describe "movePawn" $ do
    it  "moves Black pawn down from (1, 7) to (1, 6)" $ do
        let starting = Map.fromList [((1, 7), Piece Pawn Black)];
        let expected = Map.fromList [((1, 6), Piece Pawn Black)];
        let result   = movePawn starting (1, 7) (1, 6);
        case result of
          Just a -> a `shouldBe` expected
          Nothing -> assertFailure "got Nothing from movePawn"
    it  "moves Black pawn down from (1, 7) to (1, 5)" $ do
        let starting = Map.fromList [((1, 7), Piece Pawn Black)];
        let expected = Map.fromList [((1, 5), Piece Pawn Black)];
        let result   = movePawn starting (1, 7) (1, 5);
        case result of
          Just a -> a `shouldBe` expected
          Nothing -> assertFailure "got Nothing from movePawn"
    it  "returns Nothing when Black pawn is moved two spaces when not on home rank" $ do
        let starting = Map.fromList [((1, 6), Piece Pawn Black)];
        let result   = movePawn starting (1, 6) (1, 4);
        case result of
          Just a -> assertFailure "moved Pawn with invalid move"
          Nothing -> assertBool "shouldn't fail" True
    it  "returns Nothing when Black pawn is moved in wrong direction" $ do
        let starting = Map.fromList [((1, 4), Piece Pawn Black)];
        let result   = movePawn starting (1, 4) (1, 5)
        case result of
          Just a -> assertFailure "moved Pawn with invalid move"
          Nothing -> assertBool "shouldn't fail" True
    it  "moves Black pawn diagonally to take from (4, 4) to (3, 3)" $ do
        let starting = Map.fromList [((4, 4), Piece Pawn Black),
                                     ((3, 3), Piece Queen White)];
        let expected = Map.fromList [((3, 3), Piece Pawn Black)];
        let result   = movePawn starting (4, 4) (3, 3);
        case result of 
          Just a -> a `shouldBe` expected
          Nothing -> assertFailure "got Nothing from movePawn" 
    it  "moves Black pawn diagonally to take from (4, 4) to (5, 3)" $ do
        let starting = Map.fromList [((4, 4), Piece Pawn Black),
                                     ((5, 3), Piece Queen White)];
        let expected = Map.fromList [((5, 3), Piece Pawn Black)];
        let result   = movePawn starting (4, 4) (5, 3);
        case result of 
          Just a -> a `shouldBe` expected
          Nothing -> assertFailure "got Nothing from movePawn" 
    it  "returns Nothing when moving diagonally (4, 4) to (3, 3)" $ do
        let starting = Map.fromList [((4, 4), Piece Pawn Black)];
        let result   = movePawn starting (4, 4) (3, 3);
        case result of 
          Just a -> assertFailure "moved Pawn with invalid move"
          Nothing -> assertBool "shouldn't fail" True
    it  "returns Nothing when moving diagonally (4, 4) to (5, 3)" $ do
        let starting = Map.fromList [((4, 4), Piece Pawn Black)];
        let result   = movePawn starting (4, 4) (5, 3);
        case result of 
          Just a -> assertFailure "moved Pawn with invalid move"
          Nothing -> assertBool "shouldn't fail" True
    it  "moves White pawn up from (1, 2) to (1, 3)" $ do
        let starting = Map.fromList [((1, 2), Piece Pawn White)];
        let expected = Map.fromList [((1, 3), Piece Pawn White)];
        let result   = movePawn starting (1, 2) (1, 3);
        case result of
          Just a -> a `shouldBe` expected
          Nothing -> assertFailure "got Nothing from movePawn"
    it  "moves White pawn up from (1, 2) to (1, 4)" $ do
        let starting = Map.fromList [((1, 2), Piece Pawn White)];
        let expected = Map.fromList [((1, 4), Piece Pawn White)];
        let result   = movePawn starting (1, 2) (1, 4);
        case result of
          Just a -> a `shouldBe` expected
          Nothing -> assertFailure "got Nothing from movePawn"
    it  "returns Nothing when White pawn is moved two spaces when not on home rank" $ do
        let starting = Map.fromList [((1, 3), Piece Pawn White)];
        let result   = movePawn starting (1, 3) (1, 5);
        case result of
          Just a -> assertFailure "moved Pawn with invalid move"
          Nothing -> assertBool "shouldn't fail" True
    it  "returns Nothing when White pawn is moved in wrong direction" $ do
        let starting = Map.fromList [((1, 4), Piece Pawn White)];
        let result   = movePawn starting (1, 4) (1, 5)
        case result of
          Just a -> assertFailure "moved Pawn with invalid move"
          Nothing -> assertBool "shouldn't fail" True
    it  "moves White pawn diagonally to take from (4, 4) to (3, 5)" $ do
        let starting = Map.fromList [((4, 4), Piece Pawn White),
                                     ((3, 5), Piece Queen Black)];
        let expected = Map.fromList [((3, 5), Piece Pawn White)];
        let result   = movePawn starting (4, 4) (3, 5);
        case result of 
          Just a -> a `shouldBe` expected
          Nothing -> assertFailure "got Nothing from movePawn" 
    it  "moves White pawn diagonally to take from (4, 4) to (5, 5)" $ do
        let starting = Map.fromList [((4, 4), Piece Pawn White),
                                     ((5, 5), Piece Queen Black)];
        let expected = Map.fromList [((5, 5), Piece Pawn White)];
        let result   = movePawn starting (4, 4) (5, 5);
        case result of 
          Just a -> a `shouldBe` expected
          Nothing -> assertFailure "got Nothing from movePawn" 
    it  "returns Nothing when moving diagonally (4, 4) to (3, 5)" $ do
        let starting = Map.fromList [((4, 4), Piece Pawn White)];
        let result   = movePawn starting (4, 4) (3, 5);
        case result of 
          Just a -> assertFailure "moved Pawn with invalid move"
          Nothing -> assertBool "shouldn't fail" True
    it  "returns Nothing when moving diagonally (4, 4) to (5, 5)" $ do
        let starting = Map.fromList [((4, 4), Piece Pawn White)];
        let result   = movePawn starting (4, 4) (5, 5);
        case result of 
          Just a -> assertFailure "moved Pawn with invalid move"
          Nothing -> assertBool "shouldn't fail" True
