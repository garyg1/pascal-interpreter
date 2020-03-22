module Main where

import           Data.Char
import qualified Pascal
import           System.Environment
import           System.IO.Streams  (stdin, stdout)
import qualified System.IO.Streams as Streams

main :: IO ()
main = do
    (fileName : _) <- getArgs
    contents <- readFile fileName

    stdin' <- Streams.lines stdin

    _ <- case Pascal.parseString $ map toLower contents of
        Right p  -> Pascal.runApp (stdin', stdout) (Pascal.interpret p)
        Left err -> error $ show err

    return ()
