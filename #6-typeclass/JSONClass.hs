{-# LANGUAGE FlexibleInstances #-}

module JSONClass
  ( JValue (..),
    arr,
    obj,
    num,
    nil,
    str,
    bool,
    true,
    false
  )
where

import Control.Arrow (second)

data JValue
  = JString String
  | JNumber Double
  | JBool Bool
  | JNull
  | JArray (JAry JValue) -- [(String, JValue)]
  | JObject (JObj JValue) -- was [JValue]

type JSONError = String

newtype JAry a = JAry
  { fromJary :: [a]
  }
  deriving (Eq, Ord, Show)

newtype JObj a = JObj
  { fromJObj :: [(String, a)]
  }
  deriving (Eq, Ord, Show)

arr :: [JValue] -> JValue
arr a = JArray (JAry a)

obj :: [(String, JValue)] -> JValue
obj a = JObject (JObj a)

num :: Double -> JValue
num = JNumber

nil :: JValue
nil = JNull

str :: String -> JValue
str = JString

bool :: Bool -> JValue
bool = JBool

true :: JValue
true = bool True

false :: JValue
false = bool False

doubleToJValue :: (Double -> b) -> JValue -> Either JSONError b
doubleToJValue f (JNumber v) = Right (f v)
doubleToJValue f _ = Left "not a JSON number"

jaryFromJValue :: (JSON a) => JValue -> Either JSONError (JAry a)
jaryFromJValue (JArray (JAry a)) = JAry <$> mapEithers fromJValue a
jaryFromJValue _ = Left "not a JSON array"

jaryToJValue :: (JSON a) => JAry a -> JValue
jaryToJValue = JArray . JAry . map toJValue . unbox

mapEithers :: (a -> Either b c) -> [a] -> Either b [c]
mapEithers f (x : xs) = case mapEithers f xs of
  Left b -> Left b
  Right ys -> case f x of
    Left b -> Left b
    Right y -> Right (y : ys)
mapEithers _ _ = Right []

unbox :: JAry a -> [a]
unbox (JAry a) = a

class JSON a where
  toJValue :: a -> JValue
  fromJValue :: JValue -> Either JSONError a

instance JSON JValue where
  toJValue = id
  fromJValue = Right

instance JSON Bool where
  toJValue a = JBool a
  fromJValue (JBool b) = Right b
  fromJValue _ = Left "Not a JSON boolean" -- When this clause be used?

instance JSON String where
  toJValue = JString
  fromJValue (JString a) = Right a
  fromJValue _ = Left "Not a JSON string"

instance JSON Int where
  toJValue = JNumber . realToFrac
  fromJValue = doubleToJValue round

instance JSON Integer where
  toJValue = JNumber . realToFrac
  fromJValue = doubleToJValue round

instance JSON Double where
  toJValue = JNumber
  fromJValue = doubleToJValue id

instance (JSON a) => JSON (JObj a) where
  toJValue = JObject . JObj . map (second toJValue) . fromJObj

  fromJValue (JObject (JObj o)) = JObj <$> mapEithers unwrap o
    where
      unwrap (k, v) = (,) k <$> fromJValue v
  fromJValue _ = Left "not a JSON object"

val =
  obj
    [ ("a", num 1),
      ("b", num 2),
      ("c", str "3"),
      ("dash", nil),
      ("nums", arr [num 1, num 2, num 3]),
      ("isOk", true),
      ("willUpdate", false)
    ]