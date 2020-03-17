module Main where

import           Control.Exception
import           Control.Monad.State
import           Data.Char
import           Pascal
import qualified Pascal.State        as S
import           System.Environment
import           System.IO

main :: IO ()
main = do
    (fileName:_) <- getArgs
    putStrLn $ "Running " ++ fileName
    contents <- readFile fileName

    case parseString $ map toLower contents of
        Right p -> S.runApp $ do
            interpret p
        Left err -> throw $ S.InternalError $ show err

    return ()
