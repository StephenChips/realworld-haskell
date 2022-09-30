name2reply :: String -> String
name2reply name =
  "Pleased to meet you, " ++ name ++ ".\n"
    ++ "Your name contains " ++ charCount ++ " characters."
  where
    charCount = show (length name)

main :: IO ()
main = do
    putStrLn "Greetings once again. What is your name?"
    outStr <- name2reply <$> getLine
    putStrLn outStr

