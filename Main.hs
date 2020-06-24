module Main where
import Core_Lang
import Interpreter

main :: IO ()
main = do
    putStrLn (show (interpret
                        (IfElse
                            (PrimExpr (PrimInt 1))
                            (PrimExpr (PrimInt 2))
                            (PrimExpr (PrimInt 3)))
                        [] []))
