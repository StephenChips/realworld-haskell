import Control.Exception (SomeException, bracket, handle)
import Control.Monad (filterM)
import Data.Time.Clock (UTCTime (..))
import GHC.IO.Exception (IOException (IOError))
import RecursiveContents (getRecursiveContents)
import System.Directory (Permissions (..), getModificationTime, getPermissions)
import System.FilePath (takeExtension)
import System.IO (IOMode (..), hClose, hFileSize, hGetContents, openFile, withFile)

type InfoP a =
  FilePath -> -- path to directory entry
  Permissions -> -- permissions
  Maybe Integer -> -- file size (Nothing if not file)
  UTCTime -> -- last modified
  a

type Predicate = InfoP Bool

getFileSize :: FilePath -> IO (Maybe Integer)
getFileSize = withFileSize

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

withFileSize :: FilePath -> IO (Maybe Integer)
withFileSize filePath = withFile filePath ReadMode actualGetFileSize
  where
    actualGetFileSize fileHandle =
      handle handleError $ do
        size <- hFileSize fileHandle
        return (Just size)
    handleError :: IOError -> IO (Maybe Integer)
    handleError e = return Nothing

bracketReadFile :: FilePath -> IO String
bracketReadFile filePath = bracket (openFile filePath ReadMode) hClose hGetContents

withFileReadFile :: FilePath -> IO String
withFileReadFile filePath = withFile filePath ReadMode hGetContents

directReadFileCall :: FilePath -> IO String
directReadFileCall = readFile

myTest :: Predicate
myTest path _ _ _ = takeExtension path == ".cpp"

pathP :: InfoP FilePath
pathP path _ _ _ = path

sizeP :: InfoP Integer
sizeP _ _ (Just size) _ = size
sizeP _ _ Nothing _ = -1

liftP :: (a -> b -> c) -> InfoP a -> b -> InfoP c
liftP q f k w x y z = f w x y z `q` k

liftP2 :: (a -> b -> c) -> InfoP a -> InfoP b -> InfoP c
liftP2 q f g a b c d = f a b c d `q` g a b c d

constP :: a -> InfoP a
constP k _ _ _ _ = k

infix 4 ==?
infix 4 >?
infix 4 <?

(==?), (>?), (<?) :: (Ord a) => InfoP a -> a -> InfoP Bool
(==?) = liftP (==)
(>?) = liftP (>)
(<?) = liftP (<)

infixr 3 &&?
infixr 2 ||?

(&&?), (||?) :: InfoP Bool -> InfoP Bool -> InfoP Bool
(&&?) = liftP2 (&&)
(||?) = liftP2 (||)

notP :: InfoP Bool -> InfoP Bool
notP f a b c d = not (f a b c d)

liftPath :: (FilePath -> a) -> InfoP a
liftPath f filePath _ _ _ = f filePath 

myTest2 :: InfoP Bool
myTest2 = liftPath takeExtension ==? ".cpp" &&? sizeP >? 131072
