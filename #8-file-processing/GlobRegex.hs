module GlobRegex (matchesGlob, SensitiveFlag(..)) where

import Data.Char (isAlpha, isLetter, isUpper, toLower, toUpper)
import System.Directory (doesDirectoryExist, doesFileExist, getCurrentDirectory, getDirectoryContents)
import Text.Regex.Posix ((=~))

data SensitiveFlag = Sensitive | Insensitive deriving (Enum, Eq)

globToRegex :: SensitiveFlag -> String -> String
globToRegex isSensitive globStr = '^' : globToRegex' isSensitive globStr ++ "$"

globToRegex' :: SensitiveFlag -> String -> String
globToRegex' _ "" = ""
globToRegex' isSensitive ('*' : cs) = ".+" ++ globToRegex' isSensitive cs
globToRegex' isSensitive ('?' : cs) = '.' : globToRegex' isSensitive cs
globToRegex' isSensitive ('[' : '!' : cs) = "[^" ++ charactersToMatch ++ "]" ++ globToRegex' isSensitive rs
  where
    (charactersToMatch, rs) = charClass isSensitive cs
globToRegex' isSensitive ('[' : cs) = '[' : charactersToMatch ++ "]" ++ globToRegex' isSensitive rs
  where
    (charactersToMatch, rs) = charClass isSensitive cs
globToRegex' Sensitive (c : cs) = c : globToRegex' Sensitive cs
globToRegex' Insensitive (c : cs)
  | isLetter c = "[" ++ [c, (if isUpper c then toLower else toUpper) c] ++ "]" ++ globToRegex' Insensitive cs
  | otherwise = c : globToRegex' Insensitive cs

escapeCharacter :: Char -> String
escapeCharacter c
  | c `elem` regexChars = '\\' : [c]
  | otherwise = [c]
  where
    regexChars = "\\+()^$.{}]|"

charClass :: SensitiveFlag -> String -> (String, String)
charClass _ "" = error "unterminated character class"
charClass _ (']' : rs) = ("", rs)
charClass isSensitive (c : rs) = case isSensitive of
  Insensitive
    | isLetter c ->
      if isUpper c
        then (c : toLower c : characters, restOfStr)
        else (c : toUpper c : characters, restOfStr)
  _ -> (c : characters, restOfStr)
  where
    (characters, restOfStr) = charClass Insensitive rs

matchesGlob :: SensitiveFlag -> String -> String -> Bool
matchesGlob isSensitive pattern name = name =~ globToRegex' isSensitive pattern
