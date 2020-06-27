# Notes on Design

## AST

An `E` (expression) is one of:
* Primitive value: `P`
* Function definition: `F`
* Function call: `(call E E_1 ... E_n)`
* Conditional: `(if E E E)`
* Declaration statement: `(let ((X :: T) ... (X:: T)) E)`
* Variable: `X`
* Construct: `(cons E E)`

A `P` (primitive value) is one of:
* true
* false
* int
* string

A `F` is a function definition: `(lambda ((X :: T) ... (X:: T)) E)`

A `X` is a variable

A `R` (result) is one of:
* Value: `V`
* Error: `Err`

A `V` (value) is one of:
* Construct: `(cons V V)`
* Closure: `(closure F ENV)`
* Primitive value: `P`

An `Err` (error) is one of:
* @TODO

A `T` (type) is one of:
* Boolean: `Bool`
* Integer: `Int`
* String: `String`
* List: `List<T>`
* Function: `(T ... T -> T)`

## Primops
* List: `first`, `rest`
* Arithmetic: `+`, `-`, `*`, `/`, `^`, `%`
* Boolean: `&&`, `||`, `!`
* `print`
* Generic `==`


## Notes:

### Languages to Consider
* ocaml- functional but type inferred
* scala- types with pattern matching but close to java
* go- types but no pattern matching and imperative
* julia- multiple dispatch but no types
