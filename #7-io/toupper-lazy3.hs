import Data.Char (toUpper)

main = do
  inputStr <- readFile "input.txt"
  writeFile "output.txt" (map toUpper inputStr)