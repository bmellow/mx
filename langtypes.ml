open Pgmvalues
(*open Symtbl*)
open Name
open Ast
exception Error of string

module Langtypes = 
struct


let maxAttr = 701;;


(** type-related functions
	These definitions map types defined for language being processed to LLVM types *)


(** given a type object, this function returns a tuple containing the LLVM type
	and a string describing the type respectively.  In the case of a class pointer
	type, the type is actually retrieved from the symbol table.
	@param typeObj 		the Ast type object 
	@param packContext  global variable tuple	  
	@return 			a tuple containing the following: 
						(llvm type, string representing type, size in bytes)  *)
let rec get_type typObj = 
  match typObj with
  | Ast.TypVoid ->  "void"
  | Ast.TypInt ->  "int"
  | Ast.TypBool ->  "bool"
  | Ast.TypStr ->   "string"  
  | Ast.TypPtr(t) -> "pointer"
	
  | Ast.TypClsPtr(Ast.Identifier(id)) -> id
  							  							
  | _ -> raise (Error "invalid type")
  


end;;

