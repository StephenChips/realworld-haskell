module RecursiveContents (getRecursiveContents) where

import Control.Monad (forM, mapM_)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))

getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents topDir = do
  names <- getDirectoryContents topDir
  let properNames = filter (`notElem` [".", ".."]) names
  paths <- forM properNames $ \name -> do
    let path = topDir </> name
    isDirectory <- doesDirectoryExist path
    if isDirectory
      then getRecursiveContents path
      else return [path]
  return (concat paths)

printRecursiveContents :: FilePath -> IO ()
printRecursiveContents topDir = do
  listOfPaths <- getRecursiveContents topDir
  mapM_ putStrLn listOfPaths