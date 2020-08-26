module Puzzle03
  ( solver,
    runProgram,
  )
where

import Control.Monad.Trans.State.Lazy
import Data.List.Split

data Op = One | Two | NineNine

filePath :: String
filePath = "./assets/03"

intToOp :: Int -> Op
intToOp n
  | n == 1 = One
  | n == 2 = Two
  | otherwise = NineNine

runOp :: Op -> StateT [Int] IO ()
runOp One = do
  return ()
runOp Two = do
  return ()
runOp NineNine = do
  return ()


replace :: Int -> a -> [a] -> [a]
replace n e xs = take n xs ++ [e] ++ drop (n + 1) xs

program :: StateT (Int, [Int]) IO ()
program = do
  (p, vals) <- get
  let op = intToOp $ vals !! p
  case op of
    One -> do
      let pos1 = vals !! (p + 1)
      let pos2 = vals !! (p + 2)
      let pos3 = vals !! (p + 3)
      let val1 = vals !! pos1
      let val2 = vals !! pos2
      let suma = val1 + val2
      let newState = replace pos3 suma vals
      put (p + 4, newState)
    Two -> return ()
    _ -> return ()

runProgram :: [Int] -> IO ((), (Int, [Int]))
runProgram vals = runStateT program (0, vals)

solver :: IO ()
solver = do
  content <- readFile filePath
  let vals = fmap (read :: String -> Int) $ splitOn "," content
  output <- runProgram vals
  putStrLn "asd"
