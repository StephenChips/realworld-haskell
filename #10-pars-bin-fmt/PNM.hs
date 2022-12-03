{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE InstanceSigs #-}
module PNM () where

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Char (chr, isSpace, isDigit)
import Data.Int
import Data.Word (Word8)

data Greymap = Greymap
  { greyWidth :: Int,
    greyHeight :: Int,
    greyMax :: Int,
    greyData :: L.ByteString
  }
  deriving (Eq)

instance Show Greymap where
  show :: Greymap -> String
  show (Greymap w h m _) = "Greymap " ++ show w ++ "×" ++ show h ++ " " ++ show m

matchHeader :: L8.ByteString -> L8.ByteString -> Maybe L8.ByteString
matchHeader prefix str
  | prefix `L8.isPrefixOf` str =
    Just (L8.dropWhile isSpace (L.drop (L.length prefix) str))
  | otherwise =
    Nothing

getNat :: Num a => L8.ByteString -> Maybe (a, L8.ByteString)
getNat s = case L8.readInt s of
  Nothing -> Nothing
  Just (num, rest)
    | num <= 0 -> Nothing
    | otherwise -> Just (fromIntegral num, rest)

getBytes :: Integral a => a -> L8.ByteString -> Maybe (L8.ByteString, L8.ByteString)
getBytes n str =
  let count = fromIntegral n
      both@(prefix, _) = L.splitAt count str
   in if L.length prefix < count
        then Nothing
        else Just both

parseP5 :: L.ByteString -> Maybe (Greymap, L.ByteString)
parseP5 s =
  case matchHeader (L8.pack "P5") s of
    Nothing -> Nothing
    Just s1 ->
      case getNat s1 of
        Nothing -> Nothing
        Just (width, s2) ->
          case getNat (L8.dropWhile isSpace s2) of
            Nothing -> Nothing
            Just (height, s3) ->
              case getNat (L8.dropWhile isSpace s3) of
                Nothing -> Nothing
                Just (maxGrey, s4)
                  | maxGrey > 255 -> Nothing
                  | otherwise ->
                    case getBytes 1 s4 of
                      Nothing -> Nothing
                      Just (_, s5) ->
                        case getBytes (width * height) s5 of
                          Nothing -> Nothing
                          Just (bitmap, s6) ->
                            Just (Greymap width height maxGrey bitmap, s6)

-- Reinventing the `bind` function
(>>?) :: Maybe a -> (a -> Maybe b) -> Maybe b
Nothing >>? _ = Nothing
Just v >>? f = f v

parseP5_take2 :: L.ByteString -> Maybe (Greymap, L.ByteString)
parseP5_take2 s =
  matchHeader (L8.pack "P5") s
    >>? \s ->
      skipSpace ((), s)
        >>? (getNat . snd)
        >>? skipSpace
        >>? \(width, s) ->
          getNat s
            >>? skipSpace
            >>? \(height, s) ->
              getNat s
                >>? \(maxGrey, s) ->
                  getBytes 1 s
                    >>? (getBytes (width * height) . snd)
                    >>? \(bitmap, s) -> Just (Greymap width height maxGrey bitmap, s)

skipSpace :: (a, L.ByteString) -> Maybe (a, L.ByteString)
skipSpace (a, s) = Just (a, L8.dropWhile isSpace s)

data ParseState = ParseState
  { string :: L.ByteString,
    offset :: Int64
  }
  deriving (Show)

simpleParse :: ParseState -> (a, ParseState)
simpleParse = undefined

betterParse :: ParseState -> Either String (a, ParseState)
betterParse = undefined

type ErrorMessage = String

newtype Parse a = Parse
  { runParse :: ParseState -> Either ErrorMessage (a, ParseState)
  }

identity :: a -> Parse a
identity a = Parse (\s -> Right (a, s))

parse :: Parse a -> L.ByteString -> Either ErrorMessage a
parse parser initState =
  case runParse parser (ParseState initState 0) of
    Left err -> Left err
    Right (result, _) -> Right result

modifyOffset :: ParseState -> Int64 -> ParseState
modifyOffset initState newOffset = initState {offset = newOffset}

parseByte :: Parse Word8
parseByte =
  getState ==> \initState ->
    case L.uncons (string initState) of
      Nothing ->
        bail "no more input"
      Just (byte, remainder) ->
        putState newState ==> \_ -> identity byte
        where
          newOffset = offset initState + 1
          newState = ParseState remainder newOffset

getState :: Parse ParseState
getState = Parse (\s -> Right (s, s))

putState :: ParseState -> Parse ()
putState s = Parse (\_ -> Right ((), s))

bail :: String -> Parse a
bail err = Parse $ \s ->
  Left ("byte offset " ++ show (offset s) ++ ": " ++ err)

-- re-reinventing the `bind` function again!
(==>) :: Parse a -> (a -> Parse b) -> Parse b
firstParser ==> secondParser = Parse chainParser
  where
    chainParser initialState =
      case runParse firstParser initialState of
        Left errorMessage -> Left errorMessage
        Right (firstResult, newState) ->
          runParse (secondParser firstResult) newState

instance Functor Parse where
  fmap func parser = parser ==> (identity . func)

w2c :: Word8 -> Char
w2c = chr . fromIntegral

parseChar :: Parse Char
parseChar = w2c <$> parseByte

peekByte :: Parse (Maybe Word8)
peekByte = fmap fst . L.uncons . string <$> getState

peekChar :: Parse (Maybe Char)
peekChar = fmap w2c <$> peekByte

parseWhile :: (Word8 -> Bool) -> Parse [Word8]
parseWhile predicate =
  (fmap predicate <$> peekByte) ==> \mp -> -- First we peek a byte from the source
    if mp == Just True
      then -- if the byte matches the predicate, we will parse it (parseByte)
      -- then repeat the action (parseWhile predicate)
        parseByte ==> (\b -> (b :) <$> parseWhile predicate)
      else identity []

parseWhileVerbose :: (Word8 -> Bool) -> Parse [Word8]
parseWhileVerbose p =
  peekByte ==> \case {
      Nothing -> identity [];
      Just c
        | p c ->
          parseByte ==> \b ->
            parseWhileVerbose p ==> \bs ->
              identity (b : bs)
        | otherwise -> identity []
  }   


parseRawPGM =
    parseWhileWith w2c notWhite ==> \header -> skipSpaces ==>&
    assert (header == "P5") "invalid raw header" ==>&
    parseNat ==> \width -> skipSpaces ==>&
    parseNat ==> \height -> skipSpaces ==>&
    parseNat ==> \maxGrey ->
    parseByte ==>&
    parseBytes (width * height) ==> \bitmap ->
    identity (Greymap width height maxGrey bitmap)
  where notWhite = (`notElem` " \r\n\t")

parseWhileWith :: (Word8 -> a) -> (a -> Bool) -> Parse [a]
parseWhileWith f p = fmap f <$> parseWhile (p . f)

parseNat :: Parse Int
parseNat = parseWhileWith w2c isDigit ==> \digits ->
           if null digits
           then bail "no more input"
           else let n = read digits
                in if n < 0
                   then bail "integer overflow"
                   else identity n

(==>&) :: Parse a -> Parse b -> Parse b
p ==>& f = p ==> const f

parseBytes :: Int -> Parse L.ByteString
parseBytes n =
    getState ==> \st ->
    let n' = fromIntegral n
        (h, t) = L.splitAt n' (string st)
        st' = st { offset = offset st + L.length h, string = t }
    in putState st' ==>&
       assert (L.length h == n') "end of input" ==>&
       identity h

skipSpaces :: Parse ()
skipSpaces = parseWhileWith w2c isSpace ==>& identity ()

assert :: Bool -> String -> Parse ()
assert True  _   = identity ()
assert False err = bail err

