module SystemSpec (spec) where

import           Control.Monad     (when)
import           Data.ByteString   (ByteString, pack)
import qualified Data.ByteString   as BS
import           Data.Char
import           Data.Either       (isLeft)
import           Data.Knob
import qualified Pascal
import           System.Directory  (doesFileExist, getDirectoryContents)
import           System.FilePath   (splitExtension, takeBaseName)
import           System.IO         (IOMode (WriteMode), hClose)
import           System.IO.Streams (InputStream, OutputStream)
import qualified System.IO.Streams as Streams
import           Test.Hspec

testDirName :: String
testDirName = "./test/SystemTests/"

evalTest :: String -> InputStream ByteString -> OutputStream ByteString -> IO ()
evalTest source instream outstream = do
    let parsed = Pascal.parseString $ map toLower source

    when (isLeft parsed) $
        error $ show parsed

    let (Right ast) = parsed
    _ <- Pascal.runApp (instream, outstream) $ Pascal.interpret ast
    return ()

runSystemTest :: String -> IO ()
runSystemTest testName = do
    let sourceFileName = testDirName ++ testName ++ ".pas"
        inFileName = testDirName ++ testName ++ ".in"
        outFileName = testDirName ++ testName ++ ".out"

    source <- readFile sourceFileName
    doesInFileExist <- doesFileExist inFileName
    doesOutFileExist <- doesFileExist outFileName

    knob <- newKnob (pack [])
    h <- newFileHandle knob "~Pascal-Temp-InMemory.out" WriteMode
    outstream <- Streams.handleToOutputStream h

    let run = if doesInFileExist
        then Streams.withFileAsInput inFileName \instream -> do
            instream' <- Streams.lines instream
            evalTest source instream' outstream
        else evalTest source Streams.stdin outstream

    if doesOutFileExist
        then do
            _ <- run
            hClose h
            expected <- BS.readFile outFileName
            Data.Knob.getContents knob >>= (`shouldBe` expected)
        else
            run `shouldThrow` anyException

spec :: Spec
spec =
    describe "System Tests" $ do
        dnames <- runIO (getDirectoryContents testDirName)

        let pasNames = filter ((== ".pas") . snd . splitExtension) dnames

        mapM_ (\name ->
            it ("should pass " ++ testDirName ++ name) $ runSystemTest (takeBaseName name)
            ) pasNames
