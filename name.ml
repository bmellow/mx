open Ast
exception Error of string

module Name = 
struct

(** name related functions *)

(** function grabs string enclosed in passed in ID object  
	@param id		Ast.Identifier object *)
let get_string_from_identifier = function
  | Ast.Identifier(st) -> st
  | _ -> raise (Error "get_string_from_identifier: id object is required")


(** function accepts a list of Ast.Identifier objects and returns a list
	of string which were contained in those objects.  The list's order 
	is preserved. TODO: make this a name input
	@param lst list of Ast.Identifier objects *)
let name_to_string_list lst = 
	List.map (fun a -> get_string_from_identifier a) lst

let make_name_string lst = 
	String.concat "." lst

let name_to_string n = 
    make_name_string (name_to_string_list n)

end;;
