module Tests03 (main) where

import Control.Exception (evaluate)
import Puzzle03 (execProgram)
import Test.Hspec
import Test.QuickCheck
import Data.Monoid

main :: IO ()
main = hspec $ do
  describe "03" $ do
    it "example 1: [1,0,0,0,99] -> [2,0,0,0,99]" $ do
      rawOutput <- execProgram [1,0,0,0,99]
      let currentState = snd rawOutput
      currentState `shouldBe` [2,0,0,0,99]
    it "example 2: [2,3,0,3,99] -> [2,3,0,6,99]" $ do
      rawOutput <- execProgram [2,3,0,3,99]
      let currentState = snd rawOutput
      currentState `shouldBe` [2,3,0,6,99]
    it "example 3: [2,4,4,5,99,0] -> [2,4,4,5,99,9801]" $ do
      rawOutput <- execProgram [2,4,4,5,99,0]
      let currentState = snd rawOutput
      currentState `shouldBe` [2,4,4,5,99,9801]
    it "example 3: [1,1,1,4,99,5,6,0,99] -> [30,1,1,4,2,5,6,0,99]" $ do
      rawOutput <- execProgram [1,1,1,4,99,5,6,0,99]
      let currentState = snd rawOutput
      currentState `shouldBe` [30,1,1,4,2,5,6,0,99]
      