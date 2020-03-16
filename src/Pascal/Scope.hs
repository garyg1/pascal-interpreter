module Pascal.Scope where

import           Control.Exception
import qualified Data.Map          as Map
import qualified Data.Set          as Set
import           Pascal.Data

data ScopeError = UnknownSymbol Id
    | CannotAssignToConst
    { name :: Id
    }
    deriving (Show)
instance Exception ScopeError

data Scope a = Scope
    { vars         :: Map.Map Id a
    , constSymbols :: Set.Set Id
    }
    deriving (Show, Eq)

find :: (Scope a) -> Id -> Maybe a
find (Scope vs cs) name = case Map.lookup name vs of
    Just val -> Just val
    Nothing  -> Nothing

empty :: Scope a
empty = Scope Map.empty Set.empty

insert :: Id -> a -> Scope a -> Scope a
insert name val (Scope vs cs) = case Set.member name cs of
    False -> Scope (Map.insert name val vs) cs
    True  -> throw $ CannotAssignToConst name

setConst :: Bool -> Id -> Scope a -> Scope a
setConst isConst name (Scope vs cs) = case isConst of
    True  -> Scope vs (Set.insert name cs)
    False -> Scope vs (Set.delete name cs)
