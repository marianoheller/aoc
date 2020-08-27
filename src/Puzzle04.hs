module Puzzle04
  ( solver,
  )
where

import Data.List.Split
import qualified Puzzle03 as P03

solver :: IO ()
solver = do
  output <- P03.execProgram [1,2,3]
  putStrLn . show $ output
