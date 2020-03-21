module Pascal.TestUtils where

import           Control.Exception
import qualified Pascal.State      as S

extract :: S.AppReturn a -> a
extract st = case st of
    (Right val, _) -> val
    (Left ev, _)   -> throw ev

run :: (S.AppReturn a -> b) -> S.AppState a -> IO b
run extractor fn = do
    state <- S.runApp fn
    return $ extractor state
