module Tests01 (main) where

import Control.Exception (evaluate)
import Puzzle01 (calcFuel)
import Test.Hspec
import Test.QuickCheck
import Data.Monoid

main :: IO ()
main = hspec $ do
  describe "01" $ do
    it "example 1: 12 -> 2" $ do
      calcFuel 12 `shouldBe` (2 :: Sum Int)
    it "example 1: 14 -> 2" $ do
      calcFuel 14 `shouldBe` (2 :: Sum Int)
    it "example 1: 1969 -> 654" $ do
      calcFuel 1969 `shouldBe` (654 :: Sum Int)
    it "example 1: 100756 -> 33583" $ do
      calcFuel 100756 `shouldBe` (33583 :: Sum Int)
      