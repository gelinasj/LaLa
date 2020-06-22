module Core_Lang
( Expression(..)
, Store
, Environment
, Value(..)
, Error(..)
, Result(..)
) where

{----------------------
    LaLa AST Constructs
-}

--|Represents a primitive LaLa value
data PrimVal = PrimInt Integer
             | PrimBool Bool
             | PrimString String

--|Represents a LaLa variable
data Variable = Var String

--|Represents a LaLa function
data Function = Func { vars :: [Symbol]
                     , body :: Expression
                     }

--|Represents a LaLa variable declaration
data Declaration = Decl { var :: Symbol
                 , rhs :: Expression
                 }

--|Represents an untyped LaLa expression
data Expression = PrimVal
          | Variable
          | Function
          | Call { func :: Expression
                 , args :: [Expression]
                 }
          | IfElse { condition :: Expression
                   , ifTrue :: Expression
                   , ifFalse :: Expression
                   }
          | Let { decls :: [Declaration]
                , body :: Expression
                }

{--------------------------
    LaLa Runtime Constructs
-}

--|Represents a location in the store
type Location = Integer

--|Represents the location to value mapping for variables
type Store = [(Location,Value)]

--|Represents the variable to store loaction mapping for variables in scope for
--|a LaLa expression
type Environment = [(Symbol,Location)]

{-------------------------
    LaLa Return Constructs
-}

--|Represents a Lala closure
data Closure = Closure { env :: Environment
                       , func :: Function
                       }

--|Represents a LaLa value
data Value = PrimVal
           | Closure

--|Represents the set of all LaLa errors
data ErrorType = UNDECLARED_VARIABLE
           | FUNCTION_EXPECTED
           | INVALID_ARITY
           | PRIMOP_ERROR
           | INVALID_STORE_ACCESS --|Should not occur

data Error = Err { type :: ErrorType
                 , message :: String
                 }

--|Represents the result of an interpreted LaLa expression
data Result = Either Error Value
