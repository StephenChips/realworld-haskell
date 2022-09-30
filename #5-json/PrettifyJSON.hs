module PrettifyJSON (fromJValue, prettyPrint, compactPrint) where

import Data.Bits (shiftR, (.&.))
import Data.Char (ord)
import qualified Data.Map as Map
import Numeric (showHex)
import SimpleJSON (JValue (JArray, JBool, JNull, JNumber, JObject, JString))
import Prelude hiding ((<>))

data Doc
  = Empty
  | Char Char
  | Text String
  | Line
  | Concat Doc Doc
  | Union Doc Doc
  deriving (Show, Eq)

empty :: Doc
empty = Empty

string :: String -> Doc
string = enclose '"' '"' . hcat . map oneChar

text :: String -> Doc
text "" = Empty
text s = Text s

line :: Doc
line = Line

double :: Double -> Doc
double = Text . show

char :: Char -> Doc
char = Char

(<>) :: Doc -> Doc -> Doc
Empty <> y = y
x <> Empty = x
x <> y = Concat x y

enclose :: Char -> Char -> Doc -> Doc
enclose left right x = char left <> x <> char right

fromJValue :: JValue -> Doc
fromJValue (JBool True) = text "true"
fromJValue (JBool False) = text "false"
fromJValue JNull = text "null"
fromJValue (JNumber num) = double num
fromJValue (JString str) = string str
fromJValue (JArray ary) = series '[' ']' fromJValue ary
fromJValue (JObject entry) = series '{' '}' handleObjectEntry entry
  where
    handleObjectEntry (key, value) =
      string key
        <> text ":"
        <> fromJValue value

hcat :: [Doc] -> Doc
hcat = foldr (<>) Empty

oneChar :: Char -> Doc
oneChar c = case Map.lookup c simpleEscapes of
  Just r -> text r
  Nothing
    | mustEscape c -> hexEscape c
    | otherwise -> char c
  where
    mustEscape c = c < ' ' || c == '\x7f' || c > '\xff'

simpleEscapes :: Map.Map Char String
simpleEscapes = Map.fromList $ zipWith ch "\b\n\r\r\t\\\"/" "bnfrt\\\"/"
  where
    ch a b = (a, ['\\', b])

hexEscape :: Char -> Doc
hexEscape c
  | d < 0x10000 = smallHex d
  | otherwise = astral (d - 0x10000)
  where
    d = ord c

smallHex :: Int -> Doc
smallHex x =
  text "\\u"
    <> text (replicate (4 - length h) '0')
    <> text h
  where
    h = showHex x ""

astral :: Int -> Doc
astral n = smallHex (a + 0xd800) <> smallHex (b + 0xdc00)
  where
    a = (n `shiftR` 10) .&. 0x3ff
    b = n .&. 0x3ff

series :: Char -> Char -> (a -> Doc) -> [a] -> Doc
series open close fromData =
  enclose open close
    . fsep
    . punctuate (char ',')
    . map fromData

fsep :: [Doc] -> Doc
fsep = foldr (</>) empty

(</>) :: Doc -> Doc -> Doc
x </> y = x <> softline <> y

softline :: Doc
softline = group line

group :: Doc -> Doc
group x = Union (flatten x) x

-- replace all hard lines break with spaces
flatten :: Doc -> Doc
flatten (Concat a b) = Concat (flatten a) (flatten b)
flatten Line = Char ' '
flatten (Union x _) = flatten x
flatten other = other

-- Add punctuation between documents.
punctuate :: Doc -> [Doc] -> [Doc]
punctuate p [] = []
punctuate p [d] = [d]
punctuate p (d : ds) = (d <> p) : punctuate p ds

compactPrint :: Doc -> String
compactPrint Empty = ""
compactPrint (Text t) = t
compactPrint (Char c) = [c]
compactPrint Line = ""
compactPrint (Concat a b) = compactPrint a ++ compactPrint b
compactPrint (Union a b) = compactPrint b

prettyPrint width x = best 0 [x]
  where
    best col (d : ds) =
      case d of
        Empty -> best col ds
        Char c -> c : best (col + 1) ds
        Text s -> s ++ best (col + length s) ds
        Line -> '\n' : best 0 ds
        a `Concat` b -> best col (a : b : ds)
        a `Union` b ->
          nicest
            col
            (best col (a : ds))
            (best col (b : ds))
    best _ _ = ""

    nicest col a b
      | (width - least) `fits` a = a
      | otherwise = b
      where
        least = min width col

-- file: ch05/Prettify.hs
fits :: Int -> String -> Bool
w `fits` _ | w < 0 = False
w `fits` "" = True
w `fits` ('\n' : _) = True
w `fits` (c : cs) = (w - 1) `fits` cs