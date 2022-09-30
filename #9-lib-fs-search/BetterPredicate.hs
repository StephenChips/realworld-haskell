import Control.Exception (SomeException, bracket, handle)
import Control.Monad (filterM)
import Data.Time.Clock (UTCTime (..))
import RecursiveContents (getRecursiveContents)
import System.Directory (Permissions (..), getModificationTime, getPermissions)
import System.FilePath (takeExtension)
import System.IO (IOMode (..), hClose, hFileSize, hGetContents, openFile, withFile)

type Predicate = FilePath -> Permissions -> Maybe Integer -> UTCTime -> Bool

getFileSize :: FilePath -> IO (Maybe Integer)
getFileSize filePath = undefined

betterFind :: Predicate -> FilePath -> IO [FilePath]
betterFind predicate path = getRecursiveContents path >>= filterM check
  where
    check name = do
      perms <- getPermissions name
      size <- getFileSize name
      modTime <- getModificationTime name
      return (predicate name perms size modTime)

simpleFileSize :: FilePath -> IO (Maybe Integer)
simpleFileSize path = do
  h <- openFile path ReadMode
  size <- hFileSize h
  hClose h
  return (Just size)

-- simpleFileSize + error handling
saferFileSize :: FilePath -> IO (Maybe Integer)
saferFileSize filePath = handle
  ((\_ -> return Nothing) :: IOError -> IO (Maybe a))
  $ do
    h <- openFile filePath ReadMode
    size <- hFileSize h
    hClose h
    return (Just size)

bracketReadFile :: FilePath -> IO String
bracketReadFile filePath = bracket (openFile filePath ReadMode) hClose hGetContents

withFileReadFile :: FilePath -> IO String
withFileReadFile filePath = withFile filePath ReadMode hGetContents

directReadFileCall :: FilePath -> IO String
directReadFileCall = readFile

myTest :: (Ord a, Num a) => FilePath -> p1 -> Maybe a -> p2 -> Bool
myTest path _ (Just size) _ = takeExtension path == ".cpp" && size > 131072
myTest _ _ _ _ = False

pathP path _ _ _ = path

type InfoP a =
  FilePath -> -- path to directory entry
  Permissions -> -- permissions
  Maybe Integer -> -- file size (Nothing if not file)
  UTCTime -> -- last modified
  a

equalP :: (Eq a) => InfoP a -> a -> InfoP Bool
equalP f k w x y z = f w x y z == k

equalP' :: (Eq a) => InfoP a -> a -> InfoP Bool
equalP' f k w x y z = f w x y z == k

