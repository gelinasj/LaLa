module Main where
import Core_Lang
import Interpreter

main :: IO ()
main = do
    putStrLn (show (interpret
                        (Let [(Decl "x" (PrimExpr (PrimBool True))),
                             (Decl "y" (PrimExpr (PrimInt 1)))]
                        (IfElse
                            (Var "x")
                            (Var "y")
                            (PrimExpr (PrimInt 2))))
                        [] []))
