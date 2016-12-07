module Chess.ActionsSpec (spec) where

import Test.Hspec
import Chess.Base
import Chess.Actions

spec :: Spec
spec = do
  describe "stuff" $ do
    it "removes leading and trailing whitespace" $ do
       "foo bar" `shouldBe` "foo bar"
