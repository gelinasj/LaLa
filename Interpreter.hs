module Interpreter
( interpret
) where
--import Data.String.Interpolate (i)
import Core_Lang

-- |Invokes the given callback with the given value if the value is not an error,
-- |otherwise returns the error
callIfNotError :: Either Error a -> (a -> Either Error b) -> Either Error b
callIfNotError (Left (Err t m)) _ = (Left (Err t m))
callIfNotError (Right value) func = func value

-- |Retrieves the value mapped to the first occurence of the given key
getValue :: (Eq k) => k -> Error -> [(k,v)] -> Either Error v
getValue key err [] = Left err
getValue key err ((mapKey,value):rest)
    | key == mapKey = Right value
    | otherwise = getValue key err rest

-- |Retrieves the location mapped to the first occurence of the given variable
getEnvLocation :: String -> Environment -> Either Error Location
getEnvLocation varName env = getValue varName err env
    where errorMessage = "Undeclared variable: " ++ varName
        --errorMessage = [i|Undeclared variable: '#undeclaredVar]'|]
          err = Err UNDECLARED_VARIABLE errorMessage

-- |Retrieves the value mapped to the first occurence of the given location
getStoreValue :: Location -> Store -> Either Error Value
getStoreValue location store = getValue location err store
    where errorMessage = "Invalid store access at location:" ++ (show location)
        --errorMessage = [i|Invalid store access at location: '#[location]'|]
          err = Err INVALID_STORE_ACCESS errorMessage

-- |Evaluates the given LaLa expression in the given environment with the given
-- |store
interpret :: Expression -> Environment -> Store -> (Result, Store)
interpret (Var varName) env store = (result, store)
    where location = getEnvLocation varName env
          getStoreValueCurried = (`getStoreValue` store)
          result = callIfNotError location getStoreValueCurried
interpret (FuncExpr func) env store = (Right closure, store)
    where closure = Closure { env = env, func = func }
-- interpret (IfElse {condition = condition, ifTrue = ifTrue, ifFalse = ifFalse}) env store =
--     @TODO
interpret (PrimExpr val) env store = (Right (PrimVal val), store)
