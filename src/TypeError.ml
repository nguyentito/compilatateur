(** The big list of semantic errors **)

open Ast.Common

type semantic_error =

(* Most basic error of all *)
  | TypeMismatch of ctype * ctype

(* These identifiers came out of nowhere ! *)
  | UnknownVar of string
  | UnknownStruct of string 
  | UnknownUnion of string
  | UnknownStructField of string * string
  | UnknownUnionField of string * string
  | UnknownFunction of string

(* Not the right types for a primitive operator *)
  | NonNumeric of ctype
  | ArithFail (* le nom est pas terrible *)
  | NonNumericCondition of ctype (* for if/while/for *)
      
(* What you tried to do made no sense ! *)
  | InvalidLValue
  | InvalidPointer of ctype
  | InvalidArgumentList of string (* it means the arg list was not the right length *)
  | NonComposite of ctype (* composite = struct or union *)

(* Void is not a value type ! *)
  | SizeofVoid
  | InvalidReturnVoid
  | VoidVariable 

(* Uniqueness of identifiers *)
  | NonUniqueStructId of string
  | NonUniqueUnionId of string
  | NonUniqueField of string
  | NonUniqueGlobal of string
  | NonUniqueLocal of string

(* Incorrect type expressions *)
  | MalformedType of ctype
  | SelfReferentialType of ctype

let rec string_of_type  = function
  | Void -> "void"
  | Int -> "int"
  | Char -> "char"
  | Struct a -> "struct "^a
  | Union a -> "union "^a
  | TypeNull -> "TypeNull"
  | Pointer a -> (string_of_type a )^"*"


let error_message = function
  | TypeMismatch (a, b) -> "Expected type "^(string_of_type a)^
    " isn't of type "^ (string_of_type b)

  | UnknownVar a -> "Variable name "^a^" is not defined"
  | UnknownStruct a -> "Struct name "^a^" is not defined"
  | UnknownUnion a -> "Union name "^a^" is not defined"
  | UnknownStructField (a, b) -> "Field name "^a^" is not defined in struct "^b
  | UnknownUnionField  (a, b) -> "Field name "^a^" is not defined in union "^b
  | UnknownFunction a -> "Function name "^a^" is not defined"

  | NonNumeric t -> "Cannot apply numeric operation to "^string_of_type t
  | ArithFail -> "This arithmetic operation is illegal"
  | NonNumericCondition t -> "Branching requires numeric type instead of "^string_of_type t
      
  | InvalidLValue -> "This is not an lvalue"
  | InvalidPointer x -> "Type "^(string_of_type x)^" cannot be dereferenced"
  | InvalidArgumentList f -> "Function "^f^" isn't applied to the correct number of arguments"
  | NonComposite t -> "Target element of type "^(string_of_type t)^
                      " is neither a struct nor an union and has no subfield"

  | SizeofVoid ->  "Sizeof is not applied to an adequate argument"
  | InvalidReturnVoid -> "This function cannot return void"
  | VoidVariable -> "A variable cannot be of type void"

  | NonUniqueStructId a -> "Struct identifier "^a^" is already defined"
  | NonUniqueUnionId  a -> "Union identifier "^a^" is already defined"
  | NonUniqueField a -> "Field identifier "^a^" is already defined"
  | NonUniqueGlobal a -> "Global identifier "^a^" is already defined"
  | NonUniqueLocal a -> "Local variable  "^a^" is already defined"

  | MalformedType t -> "Type "^(string_of_type t)^" is not well-formed"
  | SelfReferentialType a -> "Type "^(string_of_type a)^
    " cannot contain a field of type "^string_of_type a

