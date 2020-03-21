module Pascal.Scope where

import           Control.Exception
import           Data.Functor
import qualified Data.Map          as Map
import qualified Data.Set          as Set
import           Pascal.Data
import           GHC.Generics         (Generic)
import           Control.DeepSeq      (NFData)

data ScopeError = UnknownSymbol Id
    | CannotAssignToConst Id
    deriving (Show, Eq, NFData, Generic)
instance Exception ScopeError

data Scope a = Scope
    { vars         :: Map.Map Id a
    , constSymbols :: Set.Set Id
    }
    deriving (Show, Eq, NFData, Generic)

find :: Id -> Scope a -> Maybe a
find name = Map.lookup name . vars

empty :: Scope a
empty = Scope Map.empty Set.empty

insert :: Id -> a -> Scope a -> Scope a
insert name val (Scope vs consts) = if Set.member name consts
    then throw $ CannotAssignToConst name
    else Scope (Map.insert name val vs) consts

setConst :: Bool -> Id -> Scope a -> Scope a
setConst isConst' name (Scope vs consts) = if isConst'
    then Scope vs (Set.insert name consts)
    else Scope vs (Set.delete name consts)

-- Just Bool if exists, Nothing if not exists
isConst :: Id -> Scope a -> Maybe Bool
isConst name (Scope vs consts) = Map.lookup name vs <&> \_ -> Set.member name consts
