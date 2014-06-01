import System.IO
import Control.Monad.Error
import Data.Char

data JValue = JString String
            | JNumber Int
            | JArray [JValue]
            deriving (Show)

parseJson (x:xs)
    | x == '[' = Right $ JArray [JNumber 1]
    | isDigit x = Right $ JNumber $ read $ x:(takeWhile isDigit xs)
    | x == '"' = Right $ JString $ takeWhile (not . (==) '"') xs -- doesn't support escaping
    | otherwise = error "Array not found!"
    where parse (x:xs)
            | x == '[' = JArray $ [parse xs]
            | otherwise = error "Oink..."

parseJson _ = error "Empty string?!"

main = do
    handle <- openFile "example.json" ReadMode
    json <- hGetContents handle
    putStrLn $ show json
    putStrLn $ show (parseJson json :: Either String JValue)
    return ()
