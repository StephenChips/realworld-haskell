module ControlledVisit where

import Control.Exception
  ( SomeException (..),
    bracket,
    handle,
  )
import Control.Monad (forM, liftM, filterM)
import Data.Time.Clock (UTCTime (..))
import System.Directory
  ( Permissions (searchable),
    getDirectoryContents,
    getModificationTime,
    getPermissions, getFileSize,
  )
import System.FilePath ((</>))
import System.IO (IOMode (..), hClose, hFileSize, openFile)

data Info = Info
  { infoPath :: FilePath,
    infoPerms :: Maybe Permissions,
    infoSize :: Maybe Integer,
    infoModTime :: Maybe UTCTime
  } deriving (Eq, Ord, Show)

maybeIO :: IO a -> IO (Maybe a)
maybeIO action = handle (\(SomeException _) -> return Nothing) (fmap Just action)

getInfo :: FilePath -> IO Info
getInfo filePath = do
  perms <- maybeIO (getPermissions filePath)
  size <- maybeIO (getFileSize  filePath)
  modTime <- maybeIO (getModificationTime filePath)
  return (Info filePath perms size modTime)

getUsefulContents :: FilePath -> IO [FilePath]
getUsefulContents path = getDirectoryContents path >>= filterM (return . (`notElem` [".", ".."]))

traverseDirs :: ([Info] -> [Info]) -> FilePath -> IO [Info]
traverseDirs order path = do
    subDirs <- getUsefulContents path
    infos <- mapM getInfo (path : map (path </>) subDirs)
    lisfOfListOfInfo <- forM (order infos) $ \info -> do
        if isDirectory info && infoPath info /= path
            then traverseDirs order (infoPath info)
            else return [info]
    return $ concat lisfOfListOfInfo

isDirectory :: Info -> Bool
isDirectory = maybe False searchable . infoPerms

-- `liftM` is `fmap` for Monad: 
-- liftM :: (Monad m) => (a -> b) -> m a -> m b
--
-- `forM` is same as `mapM`, with params fliped
--
-- mapM :: (Traversable t, Monad m) => (a -> m b) -> t a -> m (t b)
-- forM :: (Traversable t, Monad m) => t a -> (a -> m b) -> m (t b)
--
-- The mapM/forM is "strict", think of it like an "action builder" instead of pure code
-- To form a complete action you need a complete list, instead of some of its elements
-- so (order infos) will be evaluated before they are "mapped"
--
-- Map each element of a "structure" to a monadic action,
-- evaluate these actions from left to right, and collect the results. 

