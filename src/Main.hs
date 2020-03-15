module Main where

import           Control.Monad.State.Lazy
import           Control.Monad.Writer
import           Data.Char
import           Pascal
import           System.Environment

main :: IO ()
main = do
    (fileName:_) <- getArgs
    putStrLn $ "Running " ++ fileName
    contents <- readFile fileName

    case parseString $ map toLower contents of
        Right p -> do
            putStrLn $ interpret p
        Left err -> print err

