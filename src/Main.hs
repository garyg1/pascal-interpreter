module Main where

import Pascal
import System.Environment
import Data.Char
import qualified Data.Map as Map

main :: IO ()
main = do
    (fileName:_) <- getArgs
    putStrLn $ "Running " ++ fileName
    contents <- readFile fileName
    -- let expr = BinaryExpr "+" (BinaryExpr "*" (VarExpr $ Id "gary") (IntExpr 1)) (FltExpr 1.5)
    let
        expr = BinaryExpr "+" (IntExpr 1) (VarExpr $ Id "gary")
        state = Map.insert (Id "gary") (IntValue 2) Map.empty
        in print $ eval state expr
    -- case parseString $ map toLower contents of
    --     Right p -> print $ interpret p
    --     Left err -> print err
