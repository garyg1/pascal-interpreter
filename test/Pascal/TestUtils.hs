module Pascal.TestUtils where

import           Control.Exception
import           Data.ByteString   (ByteString)
import qualified Pascal.State      as S
import           System.IO.Streams (InputStream, OutputStream, stdin, stdout)
import qualified System.IO.Streams as Streams

extract :: S.AppReturn a -> a
extract st = case st of
    (Right val, _) -> val
    (Left ev, _)   -> throw ev

run :: (S.AppReturn a -> b) -> S.AppState a -> IO b
run extractor fn = do
    state <- S.runApp (stdin, stdout) fn
    return $ extractor state

runWithStreams :: InputStream ByteString -> OutputStream ByteString -> (S.AppReturn a -> b) -> S.AppState a -> IO b
runWithStreams istream ostream extractor fn = do
    istream' <- Streams.lines istream
    state <- S.runApp (istream', ostream) fn
    return $ extractor state
