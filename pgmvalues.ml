(*open Llvm
open Symtbl
open Name
*)
exception Error of string

module Pgmvalues = 
struct



(** get functions for the package context variable that is passed to 
	all functions emiting .  This package context variable
	is a tuple  *)



(** get type symbol table from the package context object 
	@param pc		tuple which forms the package context object *)
let get_typsymtbl  (tst, cst, nl , cc) = tst

(** get type symbol table from the package context object 
    @param pc       tuple which forms the package context object *)
let get_codegensymtbl  (tst, cst, nl, cc) = cst 

(** get queue of C code 
    @param pc       tuple which forms the package context object *)
let get_c_code (tst, cst,nl, cc) = cc

(** get needs table 
    @param pc       tuple which forms the package context object *)
let get_needsTable (tst, cst,nl, cc) = nl




let add_c_code cLine packContext =
	match packContext with
		| ( tst, cc) -> 
					(tst, cc)
		
(** takes list and determines if any duplicates are contained within it
    @param lst          list to be considered
        @return                     true if list contains duplicates, false otherwise   *)
let rec doesDupExist lst  = 
    match lst with
    | h::tl -> 
          begin
            let dup = List.exists ((fun id ->
                                    let res = (id=h) in
                                    match res with
                                    | true -> true 
                                    | false -> false)) tl in
                                    match dup with
                                    | true -> let dupStr = String.concat ""  ["doesDupExist: dup found -  "; h] in
                                                print_endline dupStr; true
                                    | false ->  doesDupExist tl
          end                                                                                        
  | [] -> false

(** takes two lists and sees if they contain exactly the same elements.
    Order does not matter so this is the same as asking the question of if
		two sets are the same. Assumption is that all elements are unique.
		@param  lstA first list to be compared
		@param  lstB second list to be compared
		@param  lstALen original length of list A 
		@return true, if the list contains exactly the same elements, false otherwise *)
let rec isListSame lstA lstB lstALen =
   match lstA with 
	| h::tl ->
        begin
          let isMem = List.mem h lstB in
					match isMem with
					| true -> isListSame tl lstB lstALen 
          | false -> false
          end 
	| [] -> (lstALen = List.length lstB)


let rec create_array_list l s = 
	  match l with
		| h :: tl -> if ((List.length tl) > 0) then
										create_array_list tl (String.concat "" [s; ","; string_of_int h]) 
									else
                    create_array_list tl (String.concat "" [s; string_of_int h]) 
		| [] -> s 
	
end;;