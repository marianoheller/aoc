module Tests02 (main) where

import Control.Exception (evaluate)
import Puzzle02 (calcFuel)
import Test.Hspec
import Test.QuickCheck
import Data.Monoid

main :: IO ()
main = hspec $ do
  describe "02" $ do
    it "example 1: 14 -> 2" $ do
      calcFuel 14 `shouldBe` (2 :: Sum Int)
    it "example 1: 1969 -> 966" $ do
      calcFuel 1969 `shouldBe` (966 :: Sum Int)
    it "example 1: 100756 -> 50346" $ do
      calcFuel 100756 `shouldBe` (50346 :: Sum Int)
      