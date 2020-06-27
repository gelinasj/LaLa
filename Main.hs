module Main where
import Core_Lang
import Interpreter

main :: IO ()
main = do
    putStrLn (show (interpret
                        (Let [(Decl "1TrueElse0"
                                    (FuncExpr (Func ["x"]
                                        (IfElse (Var "x")
                                            (PrimExpr (PrimInt 1))
                                            (PrimExpr (PrimInt 0))))))]
                            (Call (Var "1TrueElse0") [(PrimExpr (PrimBool True))]))
                        [] []))
