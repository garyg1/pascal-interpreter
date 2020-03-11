module Pascal.Wrapper
  (
      parseString
  , Error(..)
  , ErrClass(..)
  )
  where

----------------------------------------------------------------------------
import qualified Data.ByteString.Lazy.Char8 as BLC
----
import           Pascal.Lexer           (runAlex)
import           Pascal.Data            (Program)
import           Pascal.Parser          (happyParser)
----------------------------------------------------------------------------

data ErrClass
    = Syntactical (Maybe String)
    | Lexical
    | Message String
    deriving (Show, Eq)

data Error = Error
    { errLine  :: Int
    , errPos   :: Int
    , errClass :: ErrClass
    } deriving (Show, Eq)

-- string version of above function for testing and running
parseString :: String -> Either String Program
parseString s = runAlex (BLC.pack s) $ happyParser