module Puzzle02
  ( solver,
    calcFuel,
  )
where

import Data.List.Extra (mconcatMap)
import Data.Monoid
import Numeric.Extra (intToFloat)
import qualified Puzzle01 as P01

type Mass = Int

type Fuel = Int

filePath :: String
filePath = "./assets/01"

readInt :: String -> Int
readInt = read

solver :: IO ()
solver = do
  content <- readFile filePath
  let masses = fmap readInt $ lines content
  let res = getSum $ mconcatMap calcFuel masses
  putStrLn . show $ res

calcFuel :: Mass -> Sum Fuel
calcFuel 0 = 0
calcFuel m = mappend fuel $ calcFuel . getSum $ fuel
  where
    fuel = max 0 $ P01.calcFuel m
