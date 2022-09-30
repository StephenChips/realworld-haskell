import Control.Exception (ErrorCall (ErrorCall), catch, finally)
import GHC.IO.Exception (IOException (IOError))
import System.Directory (getTemporaryDirectory, removeFile)
import System.IO (Handle, SeekMode (AbsoluteSeek), hClose, hGetContents, hPutStrLn, hSeek, hTell, openTempFile)
import System.IO.Error (catchIOError)


{-
Run the program, and ask yourself these three quesitons:

1. Why is your position 23 (24 on Windows) after writing a line with 22 bytes?
2. Why is there an empty line after the file content display?
3. Why is there a \n at the end of the Haskell literal display?

Answers:

1. That's because we used hPutStrLn instead of hPutStr to write the data.
   hPutStrLn always terminates the line by writing a \n (\r\n on Windows) at the end, which didn't appear in tempdata.

2. We used putStrLn c to display the file contents c. Because the data was written originally with hPutStrLn,
   c ends with the newline character, and putStrLn adds a second newline character. The result is a blank line.

3. The \n is the newline character from the original hPutStrLn.

-}
main :: IO ()
main =
  withTempFile
    "mytemp.txt"
    ( \tempFilePath tempHandler -> do
        putStrLn "Welcome to tempfile.hs"
        putStrLn $ "I have a temporary file at " ++ tempFilePath

        pos <- hTell tempHandler
        putStrLn $ "My initial position is " ++ show pos

        let tempData = show [1 .. 10]
        putStrLn $
          "Writing one line containing "
            ++ show (length tempData)
            ++ " bytes: "
            ++ tempData
        hPutStrLn tempHandler tempData

        pos <- hTell tempHandler
        putStrLn $ "After writing, my new position is: " ++ show pos

        putStrLn "The file content is: "
        hSeek tempHandler AbsoluteSeek 0

        c <- hGetContents tempHandler

        putStrLn c

        putStrLn "Which chould be expressed as this Haskell literal:"
        print c
    )

withTempFile :: String -> (FilePath -> Handle -> IO b) -> IO b
withTempFile pattern func =
  do
    tempDir <- catchIOError getTemporaryDirectory (\_ -> return ".")
    (tempFile, tempHandler) <- openTempFile tempDir pattern

    -- `finally` takes two IO monads. It will evaluate the first one, then
    -- evaluate the second one, regardless whether the first one raised an
    -- exception or not.
    finally
      (func tempFile tempHandler)
      ( do
          hClose tempHandler
          removeFile tempFile
      )
