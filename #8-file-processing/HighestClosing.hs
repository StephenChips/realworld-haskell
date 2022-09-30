import qualified Data.ByteString.Lazy.Char8 as L
import qualified Language.Haskell.TH.Syntax as L

closing = readPrice . (!!4) . L.split ','

readPrice :: L.ByteString -> Maybe Int
readPrice str = do
    (dollars, rest) <- L.readInt str
    (cents, more) <- L.readInt (L.tail rest)
    Just (dollars * 100 + cents)

highestClose :: L.ByteString -> Maybe Int
highestClose = maximum . (Nothing :) . map closing . L.lines

highestCloseFrom :: FilePath -> IO ()
highestCloseFrom path = do
    contents <- L.readFile path
    print (highestClose contents)