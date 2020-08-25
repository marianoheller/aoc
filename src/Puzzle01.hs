module Puzzle01
  ( solver,
    calcFuel,
  )
where

import Data.List.Extra (mconcatMap)
import Data.Monoid
import Numeric.Extra (intToFloat)

type Mass = Int

type Fuel = Int

filePath :: String
filePath = "./assets/01"

readInt :: String -> Int
readInt = read

solver :: IO ()
solver = do
  content <- readFile filePath
  let rengs = lines content
  let res = getSum $ mconcatMap (calcFuel . readInt) rengs
  putStrLn . show  $ res

calcFuel :: Mass -> Sum Int
calcFuel m = Sum (floor (m' / 3)) - 2
  where
    m' = fromIntegral m :: Float
