module Pascal.Scope where

import           Control.Exception
import qualified Data.Map          as Map
import qualified Data.Set          as Set
import           Pascal.Data

data ScopeError = UnknownSymbol Id
    | CannotAssignToConst
    { ename :: Id
    }
    deriving (Show)
instance Exception ScopeError

data Scope a = Scope
    { vars         :: Map.Map Id a
    , constSymbols :: Set.Set Id
    }
    deriving (Show, Eq)

find :: (Scope a) -> Id -> Maybe a
find (Scope vs _) name = case Map.lookup name vs of
    Just val -> Just val
    Nothing  -> Nothing

empty :: Scope a
empty = Scope Map.empty Set.empty

insert :: Id -> a -> Scope a -> Scope a
insert name val (Scope vs consts) = case Set.member name consts of
    False -> Scope (Map.insert name val vs) consts
    True  -> throw $ CannotAssignToConst name

setConst :: Bool -> Id -> Scope a -> Scope a
setConst isConst' name (Scope vs consts) = case isConst' of
    True  -> Scope vs (Set.insert name consts)
    False -> Scope vs (Set.delete name consts)

isConst :: Id -> Scope a -> Maybe Bool
isConst name (Scope vs consts) = case Map.lookup name vs of
    Just _  -> Just $ Set.member name consts
    Nothing -> Nothing
