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
getValue :: (Eq k) => k -> Error -> [(k, v)] -> Either Error v
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

-- |Allocates a new in the store with a value of Thunk
allocLoc :: Store -> (Store, Location)
allocLoc store = ((newLoc, Thunk):store, newLoc)
    where newLoc = succ (getNewestLoc store)
          getMaxLoc :: (Location, Value) -> Location -> Location
          getMaxLoc (loc,_) newestLoc = max loc newestLoc
          getNewestLoc = foldr getMaxLoc (-1)

-- |Adds the variables to the current scope (env) and allocates a location in
-- |the store for each var with a Thunk value
initializeVars :: Environment -> Store -> [Declaration] -> (Environment, Store)
initializeVars env store decls = foldl initializeVar (env, store) decls
    where initializeVar :: (Environment, Store) -> Declaration -> (Environment, Store)
          initializeVar (env, store) (Decl var _) = (envNew, storeNew)
              where (storeNew,loc) = allocLoc store
                    envNew = (var,loc):env

-- | Evaluates the rhs of each declaration and updates its value in the store
declareVars :: Environment -> Store -> [Declaration] -> Either Error Store
declareVars env store decls = foldl declareVar (Right store) decls
    where declareVar :: Either Error Store -> Declaration -> Either Error Store
          declareVar maybeStore (Decl var rhs) =
              callIfNotError maybeStore updateVar
              where updateVar :: Store -> Either Error Store
                    updateVar storeNew =
                        callIfNotError maybeLoc updateStoreLoc
                        where maybeLoc = getEnvLocation var env
                              updateStoreLoc :: Location -> Either Error Store
                              updateStoreLoc loc =
                                  callIfNotError maybeVal updateStoreVal
                                  where maybeVal = interpret rhs env storeNew
                                        updateStoreVal :: (Value, Store) -> Either Error Store
                                        updateStoreVal (val, storeNewNew) = Right ((loc, val):storeNewNew)

-- |Evaluates the given LaLa expression in the given environment with the given
-- |store
interpret :: Expression -> Environment -> Store -> Result
interpret (PrimExpr val) env store = (Right (PrimVal val, store))
interpret (Var varName) env store = result
    where location = getEnvLocation varName env
          getStoreValueCurried :: Location -> Either Error Value
          getStoreValueCurried = (`getStoreValue` store)
          maybeValue = callIfNotError location getStoreValueCurried
          result = callIfNotError maybeValue (\value -> Right (value, store))
interpret (FuncExpr func) env store = Right (closure, store)
    where closure = Closure { env = env, func = func }
--interpret (Call callee args) = @TODO
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
interpret (Let decls letBody) env store =
    callIfNotError maybeStoreNew interpretLetBody
    where (envNew, storeTmp) = initializeVars env store decls
          maybeStoreNew = declareVars envNew storeTmp decls
          interpretLetBody :: Store -> Result
          interpretLetBody = interpret letBody envNew
