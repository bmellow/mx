open Ast
(**
    This module provides a symbol table for the mx compiler's code generator and its type checker.
          
        In addition, functions are provided for other modules to work with the 
        defined symbol table and associated entries.           
    Symbol Table is implemented as a stack of hashtables where each hashtable 
    represents a scoping level in a mx program.     
        The type checking instance only holds information relevant to the type checker
        so no LLVM information is stored. On the other hand, the symbol table used at the
        code generation stage, incorporates both LLVM information along with AST information 
        needed to generate the final code. 
*)

(** raised when an exception is caught in the Symtbl module *)
exception SymTblError of string

module SymTbl = 
struct

(** the structure of an object attribute in the symbol table.  These attributes are stored in a list 
within the object structure *) 
type typSymtblClsConst = STClsConst of string * Ast.typ

(** attrname, attrindex, attr type, class name *) 
type typSymtblClsAttr = STClsAttr of string * int * Ast.typ  * string

(** method name, return type, attribute list *)
type typSymtblClsMeth = STClsMeth of string * Ast.typ * typSymtblClsAttr list


                            
(** The representation of a type checking symbol table entry.  This allows for a separate static type 
checking pass  *)
type typSymtblEntry =   STConst of Ast.typ
          | STAttr of int * Ast.typ
                (** type of class attribute *)

                    | STFormal of Ast.typ
                                (** type of method arguments *)
                                
                    | STLocal of Ast.typ
                          (** type of local variables *)
                                
                    | STClsTyp of Ast.typ  * typSymtblClsConst list * typSymtblClsAttr list * typSymtblClsMeth list * Ast.typ list * Ast.typ list
                                (** class type information - type, attribute list, method list, needs list, implements list  *)
                                    

(** represents primitive data that can be stored in the symbol table *)
type primitiveDataType = PrimObjId of int               
                        (** identifier to runtime object [stored in a STRT entry] - for testing and 
                                                will likely remove *)
                         | PrimData of int   
                        (** pointer to memory location, or LLVM data *)


(** represents data types that can be stored in the symbol table  *)
type symtblValue =  STVal of primitiveDataType              (** simple data value *)
                    | STValArray of primitiveDataType array (** array of data values  *)
                    
(** represents the type of value that an identifier represents during the code
        generation process. *)
type symtblIdType =   LocalID               (* local variable identifier *)
                                            | FormalID      (* formal variable, [used for arguments] *)
                                            | ClsMemID      (* class member variable, [used to access fields of current object] *)  
                                            | ClsID             (* class, used for upcalls      *)

(** the structure of an object attribute in the symbol table.  These attributes are stored in a list 
within the object structure *)
type symtblAttr = STAttrCG of int * string * int * int * string  * symtblValue 
    (** (object attr is associated with, object id, attr string, index, offset, 
         attr type string, symbol table entry for attribute) *)

 
(** the structure of information about program data in the symbol table.  Note this can be either a object
    or a primitive data type *)
type codegenSymtblProgData =   STObjD of int * string  * int * symtblAttr list 
                                (** object representation
                                (object identifier, object class type string, pointer to object *)

                        | STPriData of string * string * int
                                (** primitive data representation
                                (primitive data description, pointer name, pointer to data TODO: (may not use)  *)
                                
                        | STObjData of string * string * int
                                (** (classtype, pointer name, pointer to data TODO: (may not use)) *)
                                
                        | STObjAttrData of string * int * string * int 
                                (** (attr type string, index, pointer name, pointer to data TODO: (may not use)) *)
                                
                                
                            


(** The representation of a symbol table entry.  It accommediates every possible type of value
    that can be stored.  The symbol table itself is implemented as a hashtable with a key string
    pointing to entries of this type. *)
type codegenSymtblEntry =  STConstCG of string * symtblValue

                    | STFormalCG  of Ast.typ
                                (** information required to get access to method arguments *)
                                
                    | STLocalCG of Ast.typ
                                (** information required for local variables *)
                    
                    | STThisCG of codegenSymtblProgData
                                (** special symbol table value for current "this" pointer *)
                                
                    | STClsTypCG of string  * int * symtblAttr list
                                (** class type information 
                                (class name, class id, pointer type, instance size, 
                                attribute list) info *)

                    | STClsMethCG of string * symtblValue * string 
                                (**  EB TODO: will have to determine 
                                how to handle return types *) 
                    | STClsMethArgsCG of (string * string)
                                (** method argument, types encodes a method
                                arguments name and type *)


(* access attribute information in clstype structure *)
let get_attribute_name (nm, idx, ty, sVal) = nm
let get_attribute_idx (nm, idx, ty, sVal) = idx
let get_attribute_type (nm, idx, ty, sVal) = ty
let get_attribute_val (nm, idx, ty, sVal) = sVal

(* for object info structure *)

(*
let get_obj_cls_name (nm, opt, ex, fp) = nm
let get_obj_pt (nm, opt, ex, fp) = opt
let get_obj_ext_pt_list (nm, opt, ex, fp) = ex
let get_obj_func_pt(nm, opt, ex, fp) = fp
*)
let get_obj_cls_name (nm, l) = nm

(*
match nm with
                                                            | Ast.TypClsPtr(Ast.Identifier(id)) -> id
*)


(** symbol table for type checking stage *)
let typSymtblEntries:(string,typSymtblEntry) Hashtbl.t = Hashtbl.create 100 

(** symbol table for code generation stage *)
let codegenSymtblEntries:(string,codegenSymtblEntry) Hashtbl.t = Hashtbl.create 100







(** {b operation definitions} *)

(* TODO: generate an mli file so only externally visible routines can be called
    from other modules *)
    
    
    
let rec sym_print_symtbl_value_ext symtblq lev = 
    
    if (Stack.is_empty symtblq) then
        ()
    else    
        let levtbl = Stack.pop symtblq in
        Hashtbl.iter (fun k v -> 
        print_string "sym_print_symtbl_value_ext: Value : ";
        print_string k;
        print_string " Level : "; 
        print_int lev;
        print_endline "" ) levtbl;
        sym_print_symtbl_value_ext symtblq (lev+1)

let sym_print_symtbl symtblq = 
    let st = Stack.copy symtblq in
    sym_print_symtbl_value_ext st 0
    
    


let sym_add_level_to_symtbl st symtblq = 

    (* let symtblEntries: (s, ste) Hashtbl.t = Hashtbl.create 109 in *)
    print_endline "sym_add_level_to_symtbl: Level added to symbol table";
    Stack.push ( st ) symtblq 


let sym_add_level_to_typ_symtbl symtblq = 
    sym_add_level_to_symtbl typSymtblEntries symtblq


let sym_add_level_to_cg_symtbl symtblq = 
    sym_add_level_to_symtbl codegenSymtblEntries symtblq


let typSymtblEntries:(string,typSymtblEntry) Hashtbl.t = Hashtbl.create 100 

let codegenSymtblEntries:(string,codegenSymtblEntry) Hashtbl.t = Hashtbl.create 100


let sym_remove_level_from_symtbl symtblq  = Stack.pop symtblq


let sym_add_value_to_symtbl symtblq k v = 
    let stEntries = Stack.top symtblq in
        print_string "sym_add_value_to_symtbl: added to symbol table ";
        print_endline k;
        
        Hashtbl.add stEntries k v

let sym_get_symtbl_value_from_level  k l = 
        print_string "sym_get_symtbl_value_from_level: try to grab "; 
        print_string k;
        print_endline " from the symbol table";
        Hashtbl.find l k  


let rec sym_find_symbl symtblq k  = 
    if Stack.is_empty symtblq then
    raise (SymTblError "sym_find_symbl: symbol could not be found") 
    else
    
    (* print_endline "before pop";
    sym_print_symtbl symtblq;
    print_endline "------------"; *)
    
    let lev = Stack.pop symtblq in
    
    (* print_endline "after pop";
    sym_print_symtbl symtblq;
    print_endline "------------"; *)
    
    print_string "sym_find_symbl: Try to find ";
    print_string k;
    print_endline " in the symbol table";
    
    let v = try sym_get_symtbl_value_from_level k lev with
    | Not_found -> print_endline "sym_find_symbl: Not found at this level";
                    sym_find_symbl symtblq k
    in
    v

        
(** return symbol table entry value associated with key in the symbol table
    passed in.
    
    @param symtblq  symbol table that pair will be added to. 
    @param k        string key related to symbol table entry  
    @return v       symbol entry associated with key 
    @raise SymTblError if no symbol table entry is associated with key *)
let sym_get_symtbl_val symtblq k = 
    
    print_string "sym_get_symtbl_val: Try to find ";
    print_string k;
    print_endline " in the symbol table";
    
    let st = Stack.copy symtblq in
    sym_find_symbl st k

(** return if an symbol table entry value is associated with a supplied key 
    in the symbol table passed in.
    
    @param symtblq  symbol table that pair will be added to. 
    @param k        string key related to symbol table entry  
    @return         true if value is associated with key, false otherwise *)
let sym_exists_symtbl_val symtblq k = 
    
    print_string "sym_exists_symtbl_val: Try to find ";
    print_string k;
    print_endline " in the symbol table";
    try
    ignore(sym_get_symtbl_val symtblq k); true with
    SymTblError descr -> false

(** add key, symbol table entry value pair to the current level.  Pair will
    replace a pair already associated with key if one exists.
    
    @param symtblq  symbol table that pair will be added to. 
    @param k        string key related to symbol table entry  
    @param v        symbol entry added to table *)
let sym_set_symtbl_val symtblq k v = 

    print_string "sym_set_symtbl_val: Try to set ";
    print_string k;
    print_endline " in the symbol table";

    let lev = Stack.top symtblq in
    Hashtbl.add lev k v

(** remove key, symbol table entry value pair from the current level.  The function
    will do nothing if no pair exists
    
    @param symtblq  symbol table that pair will be added to. 
    @param k        string key related to symbol table entry  *)
let sym_remove_symtbl_val symtblq k = 

    print_string "sym_set_symtbl_val: Try to remove ";
    print_string k;
    print_endline " from the symbol table";

    let lev = Stack.top symtblq in
    Hashtbl.remove lev k 

(** Find class of attr identifier within the specified class.  
        The class is assumed to exist in a given type symbol table.   

    @param clsName      used to search the type symbol table
    @param memName      identifier beginning search for
    @param tySymTbl     type symbol table where the class is stored
    @return             AST type of identifier   
                  *)
let sym_find_type_attr_in_class clsName memName tySymTbl =
      let tempStr = String.concat "" ["sym_find_type_attr_in_class: find "; 
                                 memName ; " in class "; clsName] in
        print_endline tempStr;
    if (sym_exists_symtbl_val tySymTbl clsName == false) then
              begin
              print_endline (String.concat "" ["sym_find_type_attr_in_class: Class "; clsName; " is NOT in the symbol table " ]);
        Ast.TypUnk
                end
    else
        let clsST = sym_get_symtbl_val tySymTbl clsName in
                match clsST with
        | STClsTyp (t,cl,al,ml,tl, il) ->       
                  try 
                  let attr = List.find 
                (fun a ->
                 match a with
                 | STClsAttr(id, idx, typ, cn) when id=memName -> true
                 | _ -> false
                      ) al in
                  match attr with
                  | STClsAttr(id, idx, typ, cn) -> 
                                print_endline (String.concat "" ["sym_find_type_attr_in_class: "; 
                                 memName ; " is an attribute in class "; clsName]);
                                typ
                with
                Not_found -> 
                                print_endline (String.concat "" ["sym_find_type_attr_in_class: "; 
                                 memName ; " is NOT an attribute in class "; clsName]);
                                Ast.TypUnk
                
                
(** Find class of const identifier within the specified class.  
        The class is assumed to exist in a given type symbol table.   

    @param clsName      used to search the type symbol table
    @param memName      identifier beginning search for
    @param tySymTbl     type symbol table where the class is stored
    @return             AST type of identifier   
                  *)
let sym_find_type_const_in_class clsName memName tySymTbl =
    if (sym_exists_symtbl_val tySymTbl clsName == false) then
        Ast.TypUnk
    else
        let clsST = sym_get_symtbl_val tySymTbl clsName in
          match clsST with
        | STClsTyp (t,cl,al,ml,tl,il) ->       
            
          try 
          let const = List.find 
            (fun a ->
                  match a with
                  | STClsConst(id, typ) when id=memName -> true
                  | _ -> false
                      ) cl in
                    match const with
                    | STClsConst(id, typ) -> typ
          with
          Not_found -> Ast.TypUnk


(** Find class of const identifier within the specified class.  
        The class is assumed to exist in a given type symbol table.   

    @param clsName      used to search the type symbol table
    @param memName      identifier beginning search for
    @param tySymTbl     type symbol table where the class is stored
    @return             AST type of identifier   
                  *)
let sym_find_type_method_in_class clsName memName tySymTbl =
    if (sym_exists_symtbl_val tySymTbl clsName == false) then
        Ast.TypUnk
    else
        let clsST = sym_get_symtbl_val tySymTbl clsName in
                match clsST with
        | STClsTyp (t,cl,al,ml,tl,il) ->       

                try
                    let meth = List.find 
          (fun a ->
             match a with
             | STClsMeth(id, typ, al) when id=memName -> true
             | _ -> false
          ) ml in
                    match meth with
                    | STClsMeth(id, typ, al)-> typ
        with
        Not_found -> Ast.TypUnk



(** see if a string is the identifier for a const
        , method or attribute in the specified class.  The class
        is assumed to exist in a given type symbol table  

    @param clsName      used to search the type symbol table
    @param memName      identifier beginning search for
    @param symtbl           type symbol table where the class is stored
    @return                     true if clsName, is the identifier for a 
                                        class member, false otherwise
                  *)
let sym_type_of_id_in_class clsName memName tySymTbl = 
    let l = 
    [
     sym_find_type_attr_in_class clsName memName tySymTbl;
     sym_find_type_const_in_class clsName memName tySymTbl;
     sym_find_type_method_in_class clsName memName tySymTbl
    ] in
    try 
    List.find 
        (fun a ->
            match a with
            | Ast.TypUnk -> false
            | _ -> true
                ) l
    with
    | Not_found -> Ast.TypUnk
                
                
(** see if a string is the identifier for a const
        , method or attribute in the specified class.  The class
        is assumed to exist in a given type symbol table  

    @param clsName      used to search the type symbol table
    @param memName      identifier beginning search for
    @param symtbl           type symbol table where the class is stored
    @return                     true if clsName, is the identifier for a 
                                        unknown class type otherwise
                  *)
let sym_is_id_in_class clsName memName tySymTbl = 
    let res = sym_type_of_id_in_class clsName memName tySymTbl in
  match res with
    | Ast.TypUnk -> 
                                print_endline (String.concat "" ["sym_is_id_in_class: "; memName; " is NOT in "; clsName]);
                                false
    | _ ->              print_endline (String.concat "" ["sym_is_id_in_class: "; memName; " is in "; clsName]);
                                true


type memberType =   MemTypConst 
                    | MemTypAttr
                    | MemTypMeth
                                        | MemTypUnk 


(** see if a string is the identifier for a const
        , method or attribute in the specified class.  The class
        is assumed to exist in a given type symbol table  

    @param clsName      used to search the type symbol table
    @param memName      identifier beginning search for
    @param symtbl           type symbol table where the class is stored
    @return                 type of value in  
                                        unknown class type otherwise
                  *)
let sym_mem_type_of_id_in_class clsName memName tySymTbl = 

   match (sym_find_type_attr_in_class clsName memName tySymTbl) with 
     | Ast.TypUnk -> 
            begin
      match (sym_find_type_const_in_class clsName memName tySymTbl) with
            | Ast.TypUnk ->
                match (sym_find_type_method_in_class clsName memName tySymTbl) with
                | Ast.TypUnk -> print_endline (String.concat ""["sym_mem_type_of_id_in_class:";clsName;"::";memName; " does not exist"]); 
                                                MemTypUnk
                | _ -> print_endline (String.concat ""["sym_mem_type_of_id_in_class:";clsName;"::";memName; " is a method"]); 
                             MemTypMeth
            | _ -> print_endline (String.concat ""["sym_mem_type_of_id_in_class:";clsName;"::";memName; " is a constant"]); 
                            MemTypConst
            end
     | _ -> print_endline (String.concat ""["sym_mem_type_of_id_in_class:";clsName;"::";memName; " is a attr"]); 
                        MemTypAttr
 

(*          
type typSymtblClsConst = STClsConst of string * Ast.typ

attrname, attrindex, attr type, class name  
type typSymtblClsAttr = STClsAttr of string * int * Ast.typ  * string

method name, return type, attribute list 
type typSymtblClsMeth = STClsMeth of string * Ast.typ * typSymtblClsAttr list 
            
            STClsTyp of Ast.typ  * typSymtblClsConst list * typSymtblClsAttr list * typSymtblClsMeth list * Ast.typ list * Ast.typ list
    try
    ignore(sym_get_symtbl_val cgsymtbl clsName); true with
    SymTblError descr -> false
*)

(* access attribute information in type tuple for *)
let get_cstring_for_desid (cStr, astType) = cStr
let get_ast_type_for_desid (cStr, astType) = astType


(** given a class name and a list of AST designator helper elements
        determine the "type" of the dotted name component of a designator.
        
        This is needed when a designator of the following form is found.
        
        c.attr1.attr2......attrX
        
        To determine its type, one must determine the type of c. and then
        search for attr1 to determine its type.  c.attr1's type is then searched
        to find the type of attr2.  This occurs recursively until the type of the 
        whole designator is determined.
         
    @param astType      clstype to be searched for in the type symbol table
        @param cs                       c string being built
    @param memName      identifier beginning search for
        @param dl           list of AST designator helper objects
    @param symtbl       type symbol table where the class is stored
    @return             
*) 

(*                 
let rec sym_get_member_type cs astType dl tySymTbl = 

    if (List.length dl = 0) then (cs, astType)
  else 
    let curDes = List.hd dl in
    let remainingDes = List.tl dl in

    match curDes with
    | Ast.DNExp(il) -> (* an array so skip this value, as it doesn't affect typing *)
                                        let tempStr = String.concat "" [cs; "[]"] in   (* TODO: fix with correct index entries *)
                                        sym_get_member_type tempStr astType remainingDes tySymTbl
    | Ast.DNID(Ast.Identifier(id)) ->
              (* retrieve symbol table entry for class name *)
                print_endline (String.concat "" ["sym_get_member_type: find type of identifier " ; id]);
                begin
                (* get current class...class the current method is in  *)
                match astType with
                    | Ast.TypClsPtr(Ast.Identifier(astcId)) ->
                          (* determine if is the current identifier a class name, member attribute or member method *) 
                            let attrType = sym_find_type_attr_in_class astcId id tySymTbl in
                            match attrType with
                            | Ast.TypClsPtr(Ast.Identifier(cid)) ->
                                        (* identifier has been deemed to be a class name *)
                                      print_endline (String.concat "" ["sym_get_member_type: identifer is a class: class " ; 
                                                                        id; "current class:"; astcId]);

                                        let tempStr = String.concat "" [cs; "->"; id] in
                                        sym_get_member_type tempStr (Ast.TypClsPtr(Ast.Identifier(cid))) remainingDes tySymTbl
                            
                            | t  ->     
                                (* otherwise it is assumed to be a member of member...we may need more suffiticated search here *) 
                                
                                (* Langtypes.get_type t*) (String.concat "" [cs; "->"; id], attrType) 
                end
*)

(* access attribute information in type tuple for *)
let get_var_type_for_id (vType, astType) = vType
let get_ast_type_for_id (vType, astType) = astType

(** Return the variable type and AST type for an given identifier 



*)
let get_entry_type cgSymtbl tySymtbl id curClassName = 

  (* check to see where identifier is defined, and then *)
  (* generate appropriate code                          *) 
  try
    let ste = sym_get_symtbl_val cgSymtbl id in
        match ste with 
        | STFormalCG(typ)-> print_endline (String.concat "" ["get_entry_type: formal found - "; id]); 
                                                (FormalID,typ) (* found in formal list - prototype parameters *)
        | STLocalCG(typ)->  print_endline (String.concat "" ["get_entry_type: local found - "; id]);
                                                (LocalID,typ)  (* found in local variables  *)
    with                            
   SymTblError descr ->
        (* if not found it is either member variable or an upcall. We check    *)
        (* to see if it is a member variable.  If it is not, it is assumed to  *)
        (* be an upcall  *)
        begin
        if (sym_is_id_in_class curClassName id tySymtbl) then
            begin
            print_endline (String.concat "" ["get_entry_type: member of class ";curClassName ;" found - "; id]);                        
      ( ClsMemID , sym_type_of_id_in_class curClassName id tySymtbl)
            end
        else
            begin
          print_endline (String.concat "" ["get_entry_type: upcall for class "; id ;" found. Current class "; curClassName ]);                     
            (ClsID, Ast.TypClsPtr(Ast.Identifier(id)))
            end
        end
        
        
        
        
        
        
        
        
        
        
        
(** Internally, object identifiers are in "obj"integer format when acting as a search
    key in the symbol table.  This function facilitates conversion from integer to 
    this internal representation. 
    @param  objId   integer used to identify object in the system
    @return         string representation of given object id used for symtbl table key *)
let sym_make_objstring objId = 
    let objidAsString = string_of_int objId in
    String.concat "." ["obj"; objidAsString]

let sym_symtbl_type_to_type = function
                    | STClsTypCG(i,s,al) -> s                         
                                    
(** make a copy of an attribute list, inserting the supplied object pointer
    in the new list*)
let rec sym_listcopy origList newList objPt = 
    match origList with
    | h::tl -> let attr = 
                match h with 
                STAttrCG(attrObjId, attrString, idx, os, attrStrTyp,  pt) ->
                STAttrCG(attrObjId, attrString, idx, os, attrStrTyp, objPt) in
                sym_listcopy tl (attr::newList) objPt
    | [] -> List.rev newList
    

(** given a class name, return a list of its attr 
    @param clsName      class name in question
    @param symtbl       symbol table that the class name is
                        contained in *)
let sym_build_attr_list clsName objPt symtbl = 
    let objCls = sym_get_symtbl_val symtbl clsName in   
    match objCls with
    | STClsTypCG(clsName, sz, al) -> sym_listcopy al []  objPt
                            

end;;