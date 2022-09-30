import Control.Exception (ErrorCall (ErrorCall), Exception (displayException), SomeException (SomeException), catch)

divide :: Float -> Float -> Float
divide x 0 = error "Division by 0."
divide x y = x / y

main :: IO ()
main =
  catch
    (print (unwrap Nothing :: Int))
    (\(SomeException e) -> putStrLn ("Exception type: " ++ show (typeOf e) ++ "\n###\n" ++ displayException e ++ "\n###"))

unwrap :: Maybe a -> a
unwrap (Just a) = a

