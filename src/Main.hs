module Main where

import Pascal
import System.Environment
import Data.Char

main :: IO ()
main = do
    (fileName:_) <- getArgs
    putStrLn $ "Running " ++ fileName
    contents <- readFile fileName
    print $ show $ parseString $ map toLower contents
