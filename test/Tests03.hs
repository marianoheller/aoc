module Tests03 (main) where

import Control.Exception (evaluate)
import Puzzle03 (runProgram)
import Test.Hspec
import Test.QuickCheck
import Data.Monoid

main :: IO ()
main = hspec $ do
  describe "03" $ do
    it "example 1: [1,0,0,0,99] -> [2,0,0,0,99]" $ do
      rawOutput <- runProgram [1,0,0,0,99]
      let currentState = snd . snd $ rawOutput
      currentState `shouldBe` [2,0,0,0,99]
    it "example 1: [2,3,0,3,99] -> [2,3,0,6,99]" $ do
      rawOutput <- runProgram [2,3,0,3,99]
      let currentState = snd . snd $ rawOutput
      currentState `shouldBe` [2,3,0,6,99]
      