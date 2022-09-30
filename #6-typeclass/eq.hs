import Data.Int (Int8)
import Numeric (showHex)

data RGB = Red | Blue | Green deriving (Show, Eq)

data RGBValue = RGBValue Int8 Int8 Int8 deriving (Eq)

instance Show RGBValue where
  show (RGBValue a b c) = "#" ++ foldMap helper [a, b, c]
    where
        helper = padZeroLeft 2 . flip showHex ""

padZeroLeft :: Int -> String -> String
padZeroLeft 0 str = str
padZeroLeft width str
    | len >= width = str
    | otherwise = '0' : padZeroLeft (width - 1) str
    where len = length str
