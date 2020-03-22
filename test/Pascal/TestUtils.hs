module Pascal.TestUtils where

import           Control.Exception
import qualified Pascal.State      as S
import           System.IO.Streams (stdin, stdout)

extract :: S.AppReturn a -> a
extract st = case st of
    (Right val, _) -> val
    (Left ev, _)   -> throw ev

run :: (S.AppReturn a -> b) -> S.AppState a -> IO b
run extractor fn = do
    state <- S.runApp (stdin, stdout) fn
    return $ extractor state
