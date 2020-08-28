module Puzzle05
  ( solver,
    execProgram
  )
where


filePath :: String
filePath = "./assets/05"


execProgram :: [String] -> [String] -> IO Int
execProgram a b = do
  return 0


solver :: IO ()
solver = do
  content <- readFile filePath
  putStrLn "solver5"
