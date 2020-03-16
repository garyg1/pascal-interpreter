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
            st <- get
            liftIO $ do
                print st
        Left err -> throw $ S.InternalError $ show err

    -- test monad stack
    _ <- S.runApp $ do
        S.overwrite (Id "myname") (S.StrValue "gary")
        S.pushEmpty
        S.overwrite (Id "myname") (S.StrValue "gary")
        S.pushEmpty
        liftIO $ do
            putStr "Enter name: "
            hFlush stdout
        userName <- liftIO getLine
        liftIO $ do
            putStr "Enter value: "
            hFlush stdout
        value <- liftIO getLine
        S.overwrite (Id userName) (S.StrValue value)
        result <- S.mustFind (Id userName)
        liftIO $ print result
        return ()


    return ()
