module Main where
import Core_Lang
import Interpreter

main :: IO ()
main = do
    putStrLn (show (interpretWithPrelude
                        (Let [(Decl "fact!"
                                (FuncExpr (Func ["x"]
                                    (IfElse (Call (Var "isZero") [(Var "x")])
                                        (PrimExpr (PrimInt 1))
                                        (Call (Var "*")
                                            [(Var "x"),
                                            (Call (Var "fact!")
                                                [(Call (Var "+")
                                                    [(PrimExpr (PrimInt (-1))),
                                                    (Var "x")])])]))))),
                            (Decl "isEven"
                                (FuncExpr (Func ["x"]
                                    (IfElse (Call (Var "isZero") [(Var "x")])
                                        (PrimExpr (PrimBool True))
                                        (Call (Var "isOdd")
                                            [(Call (Var "+")
                                                [(PrimExpr (PrimInt (-1))),
                                                (Var "x")])]))))),
                            (Decl "isOdd"
                                (FuncExpr (Func ["x"]
                                    (IfElse (Call (Var "isZero") [(Var "x")])
                                        (PrimExpr (PrimBool False))
                                        (Call (Var "isEven")
                                            [(Call (Var "+")
                                                [(PrimExpr (PrimInt (-1))),
                                                (Var "x")])])))))]
                            (Call (Var "isEven") [(Call (Var "fact!") [(PrimExpr (PrimInt 4))])]))))
