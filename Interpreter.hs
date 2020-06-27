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
initializeVars :: Environment -> Store -> [String] -> (Environment, Store)
initializeVars env store vars = foldl initializeVar (env, store) vars
    where initializeVar :: (Environment, Store) -> String -> (Environment, Store)
          initializeVar (env, store) var = (envNew, storeNew)
              where (storeNew,loc) = allocLoc store
                    envNew = (var,loc):env

updateVar :: String -> Value -> Environment -> Store -> Either Error Store
updateVar var val env store =
    callIfNotError maybeLoc updateStoreLoc
    where maybeLoc = getEnvLocation var env
          updateStoreLoc :: Location -> Either Error Store
          updateStoreLoc loc = Right ((loc, val):store)

-- | Evaluates the rhs of each declaration and updates its value in the store
declareVars :: Environment -> Store -> [Declaration] -> Either Error Store
declareVars env store decls = foldl declareVar (Right store) decls
    where declareVar :: Either Error Store -> Declaration -> Either Error Store
          declareVar maybeStore (Decl var rhs) =
              callIfNotError maybeStore updateVarWrap
              where updateVarWrap :: Store -> Either Error Store
                    updateVarWrap storeNew = callIfNotError maybeVal updateVarWrapWrap
                        where maybeVal = interpret rhs env storeNew
                              updateVarWrapWrap :: (Value, Store) -> Either Error Store
                              updateVarWrapWrap (rhsVal, storeNewNew) = updateVar var rhsVal env storeNewNew

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
interpret (Call callee args) env store = callIfNotError maybeVals interpretAfterEval
    where maybeCalleeVal = interpret callee env store
          maybeVals = callIfNotError maybeCalleeVal interpretAll
          interpretAll :: (Value, Store) -> Either Error (Value, [Value], Store)
          interpretAll (val, storeNew) = foldr interpretArgFold (Right (val, [], storeNew)) args
              where interpretArgFold :: Expression -> Either Error (Value, [Value], Store) -> Either Error (Value, [Value], Store)
                    interpretArgFold arg maybeVals = callIfNotError maybeVals interpretArg
                        where interpretArg :: (Value, [Value], Store) -> Either Error (Value, [Value], Store)
                              interpretArg (val, vals, storeNewNew) = callIfNotError maybeArgVal generateRes
                                  where maybeArgVal = interpret arg env storeNewNew
                                        generateRes :: (Value, Store) -> Either Error (Value, [Value], Store)
                                        generateRes (argVal, storeNewNewNew) = Right (val, argVal:vals, storeNewNewNew)
          interpretAfterEval :: (Value, [Value], Store) -> Result
          interpretAfterEval ((Closure closureEnv (Func vars funcBody)), argVals, store)
              | arityMatch = let varValMap = (zip vars argVals)
                                 in interpretFuncBody funcBody varValMap env store
              | otherwise = Left (Err INVALID_ARITY errorMessage)
              where expectedArity = (length vars)
                    actualArity = (length args)
                    arityMatch = expectedArity == actualArity
                    errorMessage = "Arity mismatch: expected " ++ (show expectedArity) ++ " got " ++ (show actualArity)
                    interpretFuncBody :: Expression -> [(String,Value)] -> Environment -> Store -> Result
                    interpretFuncBody body varValMap env store = callIfNotError maybestoreNewNew (interpret body envNew)
                        where (envNew, storeNew) = initializeVars env store vars
                              vars = map (\varValPair -> case varValPair of (var,_) -> var) varValMap
                              maybestoreNewNew = foldl updateStore (Right storeNew) varValMap
                              updateStore :: Either Error Store -> (String, Value) -> Either Error Store
                              updateStore maybeStore (var, val) = callIfNotError maybeStore (updateVar var val envNew)
          interpretAfterEval (val, argVals, store) = Left (Err FUNCTION_EXPECTED errorMessage)
              where errorMessage = "Function closure expected got: " ++ show val
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
    where (envNew, storeTmp) = initializeVars env store vars
          vars = map (\decl -> case decl of (Decl var _) -> var) decls
          maybeStoreNew = declareVars envNew storeTmp decls
          interpretLetBody :: Store -> Result
          interpretLetBody = interpret letBody envNew
