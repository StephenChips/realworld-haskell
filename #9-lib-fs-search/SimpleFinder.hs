module SimpleFinder (simpleFind) where

import RecursiveContents (getRecursiveContents)
import System.FilePath (isExtensionOf)

simpleFind :: (FilePath -> Bool) -> FilePath -> IO [FilePath]
simpleFind isThisFile path = do
    subDirectories <- getRecursiveContents path
    return (filter isThisFile subDirectories)

findFilesWithGivenExtension :: String -> FilePath -> IO [FilePath]
findFilesWithGivenExtension ext = simpleFind (isExtensionOf ext)
