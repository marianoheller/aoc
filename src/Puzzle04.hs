module Puzzle04
  ( solver,
  )
where

import Data.List.Split
import qualified Puzzle03 as P03

filePath :: String
filePath = "./assets/03"

increaseWords :: (Int, Int) -> (Int, Int)
increaseWords (n, v) = (n', v')
  where
    n' = if (n + 1) > 99 then 0 else n + 1
    v' = if (n + 1) > 99 then v + 1 else v

execProgramUntil :: Int -> [Int] -> IO P03.ProgramState
execProgramUntil n vals = go 0 0
  where
    go noun verb = do
      let fixedVals = P03.fixVals noun verb vals
      st@(a, outVals) <- P03.execProgram fixedVals
      let zeroPosVal = outVals !! 0
      case n == zeroPosVal of
        True -> return st
        False -> go n v
          where
            (n, v) = increaseWords (noun, verb)

solver :: IO ()
solver = do
  content <- readFile filePath
  let vals = fmap (read :: String -> Int) $ splitOn "," content
  output <- execProgramUntil 19690720 vals
  putStrLn . show $ output
