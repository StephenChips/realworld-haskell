import Data.Char (toUpper)
import System.IO

main :: IO ()
main = do
  inHandler <- openFile "input.txt" ReadMode
  outHandler <- openFile "output.txt" WriteMode
  mainLoop inHandler outHandler
  hClose inHandler
  hClose outHandler

mainLoop :: Handle -> Handle -> IO ()
mainLoop inHandler outHandler =
  do
    inEof <- hIsEOF inHandler
    if inEof
      then return ()
      else do
        inputStr <- hGetLine inHandler
        hPutStrLn outHandler (toUpper <$> inputStr)
        mainLoop inHandler outHandler
