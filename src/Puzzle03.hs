module Puzzle03
  ( solver,
    execProgram,
  )
where

import Control.Monad.Trans.State.Lazy
import Data.List.Split

data Op = One | Two | NineNine

type ProgramState = (Int, [Int])

filePath :: String
filePath = "./assets/03"

intToOp :: Int -> Op
intToOp n
  | n == 1 = One
  | n == 2 = Two
  | otherwise = NineNine

replace :: Int -> a -> [a] -> [a]
replace n element xs = take n xs ++ [element] ++ drop (n + 1) xs

getRegister :: Int -> [Int] -> (Int, Int, Int)
getRegister p vals =
  let pos1 = vals !! (p + 1)
      pos2 = vals !! (p + 2)
   in ( vals !! pos1,
        vals !! pos2,
        vals !! (p + 3)
      )

program :: StateT ProgramState IO ProgramState
program = do
  (p, vals) <- get
  let op = intToOp $ vals !! p
  case op of
    One -> do
      let (val1, val2, pos3) = getRegister p vals
      let result = val1 + val2
      put (p + 4, replace pos3 result vals)
      program
    Two -> do
      let (val1, val2, pos3) = getRegister p vals
      let result = val1 * val2
      put (p + 4, replace pos3 result vals)
      program
    _ -> do
      state <- get
      return state

execProgram :: [Int] -> IO ProgramState
execProgram vals = execStateT program (0, vals)

solver :: IO ()
solver = do
  content <- readFile filePath
  let vals = fmap (read :: String -> Int) $ splitOn "," content
  let fixedVals = (replace 1 12) . (replace 2 2) $ vals
  output <- execProgram fixedVals
  putStrLn . show $ output
