module Pascal.Wrapper where

----------------------------------------------------------------------------
import qualified Data.ByteString.Lazy.Char8 as BLC
----
import           Pascal.Lexer           (runAlex)
import           Pascal.Data            (Program)
import           Pascal.Parser          (happyParser)
----------------------------------------------------------------------------

parseString :: String -> Either String Program
parseString s = runAlex (BLC.pack s) $ happyParser
