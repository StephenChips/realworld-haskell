module BetterControlVisit where

import ControlledVisit (Info (..), getInfo, getUsefulContents, isDirectory)
import Data.Char (toLower)
import Data.Time.Clock (UTCTime (..))
import System.Directory (Permissions (..))
import System.FilePath (pathSeparators, takeExtension, takeFileName, (</>))

data Iterate seed
  = Done {unwrap :: seed}
  | Skip {unwrap :: seed}
  | Continue {unwrap :: seed}
  deriving (Show)

type Iterator seed = seed -> Info -> Iterate seed

-- Notice: function currying is right associative

-- Need revised
foldTree :: Iterator a -> a -> FilePath -> IO a
foldTree iterator initialSeed filePath = do
  res <- fold initialSeed filePath
  return (unwrap res)
  where
    fold seed path = getUsefulContents path >>= walk seed

    walk seed (name : names) = do
      let path' = filePath </> name
      info <- getInfo path'
      case iterator seed info of
        done@(Done _) -> return done
        Skip seed' -> walk seed' names
        Continue seed'
          | isDirectory info -> do
            next <- fold seed' path'
            case next of
              done@(Done _) -> return done
              seed'' -> walk (unwrap seed'') names
          | otherwise -> walk seed' names
    walk seed _ = return (Continue seed)

-- @: gives (Done _) an alias

atMostThreePictures :: Iterator [FilePath]
atMostThreePictures paths info
  | length paths == 3 = Done paths
  | isDirectory info && takeFileName path == ".svn" = Skip paths
  | extension `elem` [".jpg", ".png"] = Continue (path : paths)
  | otherwise = Continue paths
  where
    extension = map toLower (takeExtension path)
    path = infoPath info

