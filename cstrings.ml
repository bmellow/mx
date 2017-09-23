open Ast
open Symtbl
open Pgmvalues


(**
{b "C Strings"} 

This module helps the generate valid C code from AST information. 
*)



module Cstrings = 
struct


(** Given an identifier, and class name,  return the C string that represents it.
		Depending on the context, the identifier may be modified.  Symbol tables are
		used to determine the context.
		
		- if a local or formal, return unmodified
		- if a direct class attribute, prepend "self->" string
		- if a class id for an upcall, prepend "self->", add "id_up"
		- if a down method call, add _bottom to 
		- if an up method call, add  _up 
        @param          co
        @return         valid c string corresponding to type   *)  
let get_prepended_identifier_not_used cgSymtbl tySymtbl id isMethCall clsName = 

  (* check to see where identifier is defined, and then *)
  (* generate appropriate code                          *) 
  try
   ignore(SymTbl.sym_get_symtbl_val cgSymtbl id); id with
   SymTblError descr ->
   begin
     if (SymTbl.sym_is_id_in_class clsName id tySymtbl) then
        (* if value is a direct class member *)
				match isMethCall with
				| false -> String.concat "" ["self->";id] (* ...and attribute , prepend with self for access *)
				| true  -> String.concat "" [clsName; "_"; id;"->methods"]
     else
        (* check to see if this is a class name, if so assume     *)
        (* that it is a dynamic method up-call, otherwise if is   *)
        (* is local or formal paramter *)
        let clsEntry = SymTbl.sym_get_symtbl_val tySymtbl id in
          match clsEntry with
          | SymTbl.STClsTyp (t,cl,al,ml,tl,il) -> String.concat "" ["self->";id;"_up "]
          | _ -> 
						   begin
							 match isMethCall with
							 | false -> id (* a local or formal parameter *)
							 | true -> String.concat "" ["self->"; id; "_bottom->methods"]
							 end
                                
          (* raise (Error "identifier not found in the symbol table")  *)
                    
          (* TODO: how do we determine how the dynamic resolution occurs.*)
          (* especially if there is more than 1 needs *)
          (* String.concat "" ["xxxx->";id] *)
   end





(*
 designHelp =       DNID of identifier
                    | DNExp of expression list
*)







(** Given a AST list of designhelper (used to form a dotted name), return the C string that represents it *)           
(*
let getCstringDottedNamexx dhl clsName isMethCall packContext = 
		let cgSymtbl = Pgmvalues.get_codegensymtbl packContext in
    let tySymtbl = Pgmvalues.get_typsymtbl packContext in
		
		let hdDhl = List.hd dhl in
		let restOfDhl = List.tl dhl in
		
		(* all but last element, used to get pointer part of dotted name for methods *) 
		(* have to check lengths - this is causing seg faults *) 
		
		let methPtDhl = List.rev (List.tl (List.rev dhl)) in
		let restOfMethPtDhl = List.tl (List.rev (List.tl dhl)) in
		
	
		
		match isMethCall with 
		| true -> 
					  (* method attribute *) 
						let methName = getMethNameString dhl in
						begin
            match hdDhl with
            | Ast.DNID(Ast.Identifier(id)) ->
							  begin
							  (* check the type of the first identifer in the list  *)
                let t = SymTbl.get_entry_type cgSymtbl tySymtbl id clsName in
								let symTblTyp = SymTbl.get_var_type_for_id t in 
								
								(* construct the pointer needed to access the method *)
								let ptStr = 
                		match symTblTyp with
                    | SymTbl.FormalID | SymTbl.LocalID  -> "sdfsd"
											(* getPointerString id restOfMethPtDhl *)
										| SymTbl.ClsID | SymTbl.ClsMemID -> "asdfasd"
											(* getPointerString (String.concat "" ["self->"; id]) restOfMethPtDhl *)
										| _ -> "srtret" 
								in
								
								(* generate the method call code  *)
								match symTblTyp with
								| SymTbl.FormalID | SymTbl.LocalID  ->  "adfsf"
							  | SymTbl.ClsID -> "adfas" 
								| SymTbl.ClsMemID -> "adfasdf"
								| _ -> "yuyuiuiy"
								end
								
(*															
								((Stack_Methods* )(s->Stack_bottom)->methods)->push	
										self->Stack_up->methods->push	
*)
            | _ -> "getCstringDottedName: invalid format"
						end
					
		| false -> 
						(* variable attribute *)
						begin
						match hdDhl with
						| Ast.DNID(Ast.Identifier(id)) ->
							  let t = SymTbl.get_entry_type cgSymtbl tySymtbl id clsName in 
                match (SymTbl.get_var_type_for_id t) with
								| SymTbl.FormalID | SymTbl.LocalID  -> (* getPointerString id restOfDhl *) "asdf"
								| SymTbl.ClsID -> "getCstringDottedName: unsupported format"
								| SymTbl.ClsMemID -> (* getPointerString (String.concat "" ["self->"; id]) restOfDhl *) "asdf"
								| _ -> "ertyrtyr"
						| _ -> "getCstringDottedName: invalid format"
						end
						
			
	(*  testing	

	  if ((List.length dhl) = 0) then
			  ""
		(* if the list is 1 element long, it must be an identifier *)
		else if ((List.length dhl) = 1) then
			begin
	  	match dhl with 
		  | [Ast.DNID(Ast.Identifier(id))] -> 
				(* do lookup to determine which identifier is a local, formal or class attribute *)
				let t = SymTbl.get_entry_type cgSymtbl tySymtbl id clsName in 
				match (SymTbl.get_var_type_for_id t) with
				(* formal or local identifier *)
				| SymTbl.FormalID | SymTbl.LocalID  -> 
											begin
											match isMethCall with
											| false -> id
											| true -> "getCstringDottedName: dotted name list length of 1, not possible for formal or local method"
											end
				(* class identifer *)
				| SymTbl.ClsID -> "getCstringDottedName: dotted name list length of 1, not possible for class"

				(* class member identifier *)
				| SymTbl.ClsMemID ->
					            begin
											match isMethCall with
											| true  ->   
												(* class method *)			
															let ptToMethTbl = 
																	String.concat "" [clsName;"_Methods*"] in
												
															let ptToImpl = 
																	String.concat "" [clsName; "_Impl*"] in
																		
																		
                              String.concat "" [ "(("; ptToMethTbl; ")(self->";
                                                 clsName ; "_bottom)"; 
																								"->methods)->"; id ] 

                			| false -> String.concat "" ["self->";id] (* ...and attribute , prepend with self for access *)
											end
					end
				
				(* get_prepended_identifier cgSymtbl tySymtbl id isMethCall clsName *)
	      (* ((Stack_Methods* ) (s->Stack_bottom)->methods)->push( (Stack_Impl* ) s->Stack_bottom, 4, &d)   *)

		
		(* if you have a list of identifers than whole list must be parsed,  *)
		else
		
		   let firstParameter = List.hd dhl in
		   let restofParameters = List.tl dhl in  
		
		   (*first value must be an identifier *)
		   (* let firstIDString = *)
		   match firstParameter with
       | Ast.DNID(Ast.Identifier(id)) ->
				 
        print_string "getCstringDottedName: first dotted name identifier found:";
				print_endline id;
				
				(* get type information of first element of the identifier list *)
				let firstType = SymTbl.get_entry_type cgSymtbl tySymtbl id clsName in
				begin
				match (SymTbl.get_var_type_for_id firstType) with
        | SymTbl.FormalID | SymTbl.LocalID  -> 
												print_endline "getCstringDottedName: formal or local:";
                        begin
                        match isMethCall with
                        | true -> 
                             begin
															print_endline "getCstringDottedName: method:";
														(* get method name *)
														 let methStr = getMethNameString restofParameters in
														
														(* get pointer string *)
														 let pStr = getPointerString "" restofParameters in
														
														(* get class type of method *)
														
														(*first, strip off method name *)
														let tempList = List.rev (List.tl (List.rev restofParameters)) in
														let memType = SymTbl.sym_get_member_type id (SymTbl.get_ast_type_for_id (firstType))  
                                                        tempList tySymtbl in 
																												
														let methodClassStr =
															begin 
															match (SymTbl.get_ast_type_for_id memType) with
                             	| Ast.TypClsPtr(Ast.Identifier(cn)) -> cn
															end
														in
														
														String.concat "" [methStr ; pStr; methodClassStr]
                             end
                        | false ->  let cStr = SymTbl.sym_get_member_type id (SymTbl.get_ast_type_for_id (firstType))  
                                                        restofParameters tySymtbl in
                                     SymTbl.get_cstring_for_desid cStr
                        end
        | SymTbl.ClsID ->
                      begin
                      match isMethCall with
                      | true -> String.concat "" [clsName; "_"; id;"->methods"]
                      | false -> id
                      end
        | SymTbl.ClsMemID -> 
                        begin
                        match isMethCall with
                        | true -> 
                             begin
                             (*  get the type of the identifier *)
                             match (SymTbl.get_ast_type_for_id firstType) with
                             | Ast.TypClsPtr(Ast.Identifier(cn)) ->
                                         String.concat "" [cn; "_"; id;"->methods"]
                             end
                        | false ->  let cStr = SymTbl.sym_get_member_type 
																									(String.concat "" ["self->";id]) 
																									(SymTbl.get_ast_type_for_id (firstType))  
                                                  restofParameters tySymtbl in
                                     SymTbl.get_cstring_for_desid cStr
                        end
				end

end testing
*)				
								
*)												
																
																				
																												
				(* if we are dealing with a method call, use last identifier, as part of naming   *)
				(* the method to be called.  Thus the type is determined without the last element *)
				(* 				
				begin 
				match isMethCall with
				| true -> ""
				| false -> 				

				end
				*)
				
																									  
				(* get_prepended_identifier cgSymtbl tySymtbl id isMethCall clsName *)
				(*
				try
        ignore(SymTbl.sym_get_symtbl_val cgSymtbl id); id with
        SymTblError descr ->
           begin
           if (SymTbl.sym_is_id_in_class clsName id tySymtbl) then
					 (* if value is a direct class member, then prepend with self for access *)
              String.concat "" ["self->";id]
           else
							(* check to see if this is a class name, if so assume     *)
							(* that it is a dynamic method up-call, otherwise if is   *)
							(* is local or formal paramter *)
						  let clsEntry = SymTbl.sym_get_symtbl_val tySymtbl id in
							match clsEntry with
							| SymTbl.STClsTyp (t,cl,al,ml,tl, il) -> String.concat "" ["self->";id;"_up "]
							| _ -> id 
								
              (* raise (Error "identifier not found in the symbol table")  *)
					
					(* TODO: how do we determine how the dynamic resolution occurs.*)
					(* especially if there is more than 1 needs *)
					  (* String.concat "" ["xxxx->";id] *)
            end
				*)
				
				
				
				(*
		  in
		  firstIDString;
	
    
    (* middle parameters either add a ->id (pointer accessor and field name *)
		(* of an expression used to access array indexes   *)
    let allButLastCString = 
						List.fold_left  (fun a b -> 
                              match b with
                              | Ast.DNID(Ast.Identifier(id)) -> 
																	print_string "getCstringDottedName: middle dotted name identifier found:";
																	print_endline id;
																	String.concat "" [ a; "->"; id ]

															| Ast.DNExp(il) ->
																    print_endline "getCstringDottedName: middle dotted name expression list found:";
																		getCstringIntList il
															| _ -> ""
																		
                            )   firstIDString middleParameters in
                                    
    (* merge with last one which as no comma, if no parameters were passed *)
    (* return an empty string *)                                
    match lastParameter with
    |  Ast.DNID(Ast.Identifier(id)) -> String.concat "" [ allButLastCString; id]
		|  Ast.DNExp(il) -> String.concat "" [allButLastCString;  getCstringIntList il]
    |  _ -> allButLastCString
			*)





end;;