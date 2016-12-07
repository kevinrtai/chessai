import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Data.Monoid
import Chess.Base
import Chess.Actions

pieceEquivTest:: Assertion
pieceEquivTest = True @?= ((Piece Queen Black == Piece Queen Black) &&
                   (Piece Queen White /= Piece Queen Black) &&
                   (Piece Queen Black /= Piece Pawn Black) &&
                   (Piece Queen Black /= Piece Pawn White))

main :: IO ()
main = defaultMainWithOpts
       [testCase "pieceEquivTest" pieceEquivTest]
       mempty
