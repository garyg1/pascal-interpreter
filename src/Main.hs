module Main where

import Pascal
import System.Environment
import Data.Char
import qualified Data.Map as Map
import qualified Pascal.State as State

main :: IO ()
main = do
    (fileName:_) <- getArgs
    putStrLn $ "Running " ++ fileName
    contents <- readFile fileName
    -- let expr = BinaryExpr "+" (BinaryExpr "*" (VarExpr $ Id "gary") (IntExpr 1)) (FltExpr 1.5)
    let
        expr = BinaryExpr "+" (IntExpr 1) (VarExpr $ Id "gary")
        state = State.State [(Map.insert (Id "gary") (State.IntValue 2) Map.empty)] Map.empty
        decls = [Decl (Id "decl") TypeBool,
            DeclDefn (Id "declDefn") expr,
            DeclTypeDefn (Id "declTypeDefn") TypeBool expr]
        block = Block [VarDecls decls] []
        in print $ evalBlock state block
    -- case parseString $ map toLower contents of
    --     Right p -> print $ interpret p
    --     Left err -> print err
