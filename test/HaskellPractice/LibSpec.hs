module HaskellPractice.LibSpec (spec) where

import Test.Hspec

import HaskellPractice.Lib (greet)

spec :: Spec
spec =
  describe "greet" $
    it "greets the provided name" $
      greet "Haskell" `shouldBe` "Hello, Haskell!"
