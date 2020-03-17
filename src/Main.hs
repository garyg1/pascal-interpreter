module Main where

import           Control.Exception
import           Data.Char
import           Pascal
import qualified Pascal.State        as S
import           System.Environment

main :: IO ()
main = do
    (fileName:_) <- getArgs
    putStrLn $ "Running " ++ fileName
    contents <- readFile fileName

    _ <- case parseString $ map toLower contents of
        Right p -> S.runApp $ interpret p
        Left err -> throw $ S.InternalError $ show err

    return ()
