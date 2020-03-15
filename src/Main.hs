module Main where

import           Data.Char
-- import qualified Data.Map           as Map
-- import qualified Pascal.State       as State
import           Pascal
import           System.Environment
import Control.Monad.Writer
import Control.Monad.State.Lazy

main :: IO ()
main = do
    (fileName:_) <- getArgs
    putStrLn $ "Running " ++ fileName
    contents <- readFile fileName
    -- let expr = BinaryExpr "+" (BinaryExpr "*" (VarExpr $ Id "gary") (IntExpr 1)) (FltExpr 1.5)
    -- let
    --     expr = BinaryExpr "+" (IntExpr 1) (VarExpr $ Id "gary")
    --     state = State.State [(Map.insert (Id "gary") (State.IntValue 2) Map.empty)] Map.empty
    --     decls = [Decl (Id "decl") TypeBool,
    --         DeclDefn (Id "declDefn") expr,
    --         DeclTypeDefn (Id "declTypeDefn") TypeBool expr]
    --     block = Block [VarDecls decls] []
    --     in print $ evalBlock state block

    case parseString $ map toLower contents of
        Right p -> do
            putStrLn $ interpret p
        Left err -> print err

    -- print $ runWriter $ do
    --     a <- g 1
    --     b <- g a
    --     tell "done\n"
    --     return (a, b)


f :: Float -> (Float, String)
f a = let
    b = a * 3
    in (b, "multiplied by 3\n")

g :: Float -> Writer String Float
g a = writer (a * 3, "multiplied by 3\n")

bind :: (Float -> (Float, String)) -> ((Float, String) -> (Float, String))
bind f' (gx, gs) = let (fx, fs) = f gx in (fx, gs ++ fs)

unit x = (x, "")

lift f' = unit . f'

