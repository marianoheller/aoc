module Puzzle03
  ( solver,
    execProgram,
    fixVals,
    ProgramState,
  )
where

import Control.Monad.Trans.State.Lazy
import Data.List.Safe ((!!))
import Data.List.Split
import Prelude hiding ((!!))

data Op = One | Two | NineNine

type ProgramState = (Int, [Int])

filePath :: String
filePath = "./assets/03"

intToOp :: Maybe Int -> Op
intToOp Nothing = NineNine
intToOp (Just n)
  | n == 1 = One
  | n == 2 = Two
  | otherwise = NineNine

replace :: Int -> a -> [a] -> [a]
replace n element xs = take n xs ++ [element] ++ drop (n + 1) xs

getRegister :: Int -> [Int] -> Maybe (Int, Int, Int)
getRegister p vals = do
  pos1 <- vals !! (p + 1)
  pos2 <- vals !! (p + 2)
  val1 <- vals !! pos1
  val2 <- vals !! pos2
  pos3 <- vals !! (p + 3)
  return (val1, val2, pos3)

endProgram :: StateT ProgramState IO ProgramState
endProgram = do
  state <- get
  return state

program :: StateT ProgramState IO ProgramState
program = do
  (p, vals) <- get
  let op = intToOp $ vals !! p
  let register = getRegister p vals
  case (op, register) of
    (NineNine, _) -> endProgram
    (_, Nothing) -> endProgram
    (One, Just (val1, val2, pos3)) -> do
      let result = val1 + val2
      put (p + 4, replace pos3 result vals)
      program
    (Two, Just (val1, val2, pos3)) -> do
      let result = val1 * val2
      put (p + 4, replace pos3 result vals)
      program

execProgram :: [Int] -> IO ProgramState
execProgram vals = execStateT program (0, vals)

fixVals :: Int -> Int -> [Int] -> [Int]
fixVals f s = (replace 1 f) . (replace 2 s)

solver :: IO ()
solver = do
  content <- readFile filePath
  let vals = fmap (read :: String -> Int) $ splitOn "," content
  let fixedVals = fixVals 12 2 vals
  output <- execProgram fixedVals
  putStrLn . show $ output
