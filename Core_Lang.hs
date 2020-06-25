module Core_Lang
( Expression(..)
, PrimitiveValue(..)
, Store
, Environment
, Declaration(..)
, Value(..)
, ErrorType(..)
, Error(..)
, Result(..)
, Location
) where

{----------------------
    LaLa AST Constructs
-}

-- |Represents a primitive LaLa value
data PrimitiveValue = PrimInt Integer
                    | PrimBool Bool
                    | PrimString String deriving(Show)

-- |Represents a LaLa function
data Function = Func { vars :: [String]
                     , funcBody :: Expression
                     } deriving(Show)

-- |Represents a LaLa variable declaration
data Declaration = Decl { var :: String
                        , rhs :: Expression
                        } deriving(Show)

-- |Represents an untyped LaLa expression
data Expression = PrimExpr PrimitiveValue
                | Var String
                | FuncExpr Function
                | Call { callee :: Expression
                       , args :: [Expression]
                       }
                | IfElse { condition :: Expression
                         , ifTrue :: Expression
                         , ifFalse :: Expression
                         }
                | Let { decls :: [Declaration]
                      , letBody :: Expression
                      } deriving(Show)

{--------------------------
    LaLa Runtime Constructs
-}

-- |Represents a location in the store
type Location = Integer

-- |Represents the location to value mapping for variables
type Store = [(Location,Value)]

-- |Represents the variable to store loaction mapping for variables in scope for
-- |a LaLa expression
type Environment = [(String,Location)]

{-------------------------
    LaLa Return Constructs
-}

-- |Represents a LaLa value
data Value = Thunk
           | PrimVal PrimitiveValue
           | Closure { env :: Environment
                     , func :: Function
                     } deriving(Show)

-- |Represents the set of all LaLa runtime errors
data ErrorType = UNDECLARED_VARIABLE
               | FUNCTION_EXPECTED
               | INVALID_CONDITION_TYPE
               | INVALID_ARITY
               | PRIMOP_ERROR
               | INVALID_STORE_ACCESS deriving(Show) -- |Should not occur

-- |Represents a error in LaLa
data Error = Err { errType :: ErrorType
                 , message :: String
                 } deriving(Show)

-- |Represents the result of an interpreted LaLa expression
type Result = Either Error (Value,Store)
