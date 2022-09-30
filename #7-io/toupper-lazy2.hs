import System.IO
import Data.Char(toUpper)

main :: IO ()
main = do
    inHdlr <- openFile "input.txt" ReadMode
    outHdlr <- openFile "output.txt" WriteMode
    inputStr <- hGetContents inHdlr
    hPutStr outHdlr (map toUpper inputStr)
    hClose inHdlr
    hClose outHdlr
