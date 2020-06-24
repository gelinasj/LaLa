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
interpret :: Expression -> Environment -> Store -> Result
interpret (Var varName) env store = result
    where location = getEnvLocation varName env
          getStoreValueCurried :: Location -> Either Error Value
          getStoreValueCurried = (`getStoreValue` store)
          maybeValue = callIfNotError location getStoreValueCurried
          result = callIfNotError maybeValue (\value -> Right (value, store))
interpret (FuncExpr func) env store = Right (closure, store)
    where closure = Closure { env = env, func = func }
interpret (IfElse condition ifTrue ifFalse) env store =
    callIfNotError maybeBool interpretIfElse
    where maybeBool = interpret condition env store
          interpretIfElse :: (Value, Store) -> Result
          interpretIfElse ((PrimVal (PrimBool isTrue)),storeNew)
              | isTrue = interpret ifTrue env storeNew
              | otherwise = interpret ifFalse env storeNew
          interpretIfElse (value, _) = Left err
              where errorMessage = "Invalid condition type: " ++ (show value)
                    err = Err INVALID_CONDITION_TYPE errorMessage
interpret (PrimExpr val) env store = (Right (PrimVal val, store))
