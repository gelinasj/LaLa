module Main where
import Core_Lang
import Interpreter

main :: IO ()
main = putStrLn (show (interpret (PrimExpr (PrimInt 1)) [] []))
