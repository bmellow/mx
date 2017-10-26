(**
    This module will type check a given abstract symbol table passed
    into the type_check function.
    
    It uses
    - module object, where functions and global variables are stored 
    - builder object, used to generate LLVM instructions and track the location where 
        they should be inserted.
        
    The code is ultimately stored in the module object for later output. 
*)


open Symtbl
open Ast
open Pgmvalues
open Langtypes

exception Error of string


module Typecheck = 
struct
    
(** error constants *)
    let errClsDup = (-1) ;; 
    let errMemDup = (-2) ;;
  let errFrmDup = (-3) ;;
    let errClsInv = (-4) ;;
  let errMethMismatch = (-5) ;;

  let errExpInv = (-6) ;;
    let errStmtInv = (-7);;
    let errConstTypMismatch = (-8);;
  let errDesTypMismatch = (-9);;


  let errNone = (0) ;;


(** Print current state of stack for debugging purposes *)
let rec print_ast_typ t = 
    match t with
    | Ast.TypVoid -> print_string "void"
    | Ast.TypInt  -> print_string "int"
    | Ast.TypBool -> print_string "boolean"
    | Ast.TypChar -> print_string "char"
    | Ast.TypStr  -> print_string "string"
    | Ast.TypPtr(t)  -> print_string "pointer ["; print_ast_typ t; print_string "]"
    | Ast.TypClsPtr(Ast.Identifier(cn)) -> print_string "class"; print_string cn
    | Ast.TypArr(il, t) -> print_string "array of ";
                       print_ast_typ t; print_char ' '
                       (* List.iter (fun a -> print_char ' '; print_int a) il *)
        | _  -> print_string "print_ast_typ: unknown"


(** This function is used to print error messages associated
        with typecheck error
        @param errNum       error code passed into caller
        @return                 string that describes the error condition *)
let typechk_print_error v = 
    if v = errClsDup then "error: duplicate class"
  else if v = errMemDup then "error: duplicate member"
  else if v = errFrmDup then "error: duplicate formal parameter"
    else if v = errMethMismatch then "error: ill-formed method"
    else if v = errExpInv then "error: incompatiable types in expression"
    else "error: no errors found"



(** given a string and a symbol table, determine if that string is a 
        valid class name in that table.
        @param clsName  string identifying class in the symbol table 
        @param symtbl       symbol table, in the form specifed in the 
                                        Symtbl module
        @return                 true if the clsName string identifies a
                                        class in the symbol table, false otherwise *)
let is_valid_class clsName packContext  = 
      try
    let cType = SymTbl.sym_get_symtbl_val (Pgmvalues.get_typsymtbl packContext) clsName in 
    match cType with     
    | SymTbl.STClsTyp(typ, cl, al,ml, nl, il) -> errNone
        with SymTblError _ -> errClsInv 

(** given a string and a symbol table, 
        return the identifier's type in that table
        @param idName       string identifier
        @param symtbl       symbol table in form found in the Symtbl module
        @return                 the type of the identifer found in symtbl.
                                        types are defined in the Ast module *)
let get_identifier_type idName packContext  = 
    try
    let idType = SymTbl.sym_get_symtbl_val (Pgmvalues.get_typsymtbl packContext) idName in 
    match idType with 
        | SymTbl.STConst(t) -> t (* type of const *)
    | SymTbl.STAttr(i,t) -> t (* type of class attribute *) (* may not be needed *)
    | SymTbl.STFormal(t) -> t (* type of method arguments *) 
        | SymTbl.STLocal(t) -> t (* type of local variables *) 
        | SymTbl.STClsTyp(t, cl, al, ml, nl, il) -> t
         
        with SymTblError _ -> print_string "get_identifier_type: "; 
                              print_string idName;
                                                    print_string " not found.";
                                                    Ast.TypUnk 

(*
        | Symtbl.STThis(t) -> t (* type of class currently being parsed, "this" pointer *)


                    | STClsMeth of string * Ast.typ * Ast.typ list 
                                (** will probably make this a tuple list of [attr name, attr type]  *)

    | SymTbl.STClsTyp(typ, nl, il) -> errNone
        with SymTblError _ -> errClsInv 
*)



(** given a list of error codes, determine if there is an error, if so return it
        otherwise, return no error.  Error codes are represented by non-positive integers
        with no error represented by 0 - (errNone)
        
        @param lst    list of error codes passed into caller 
    @return         errorCode  *)
let get_error_in_list lst =
    try List.find (fun a -> (a < errNone)) lst 
        with Not_found -> print_endline "get_error_in_list: no errors found"; errNone 

(** given a list of ast identifiers, return a list of their associated strings 
    @param lst  list of AST identifiers
        @return      list of strings*)
let typechk_getstringlist_from_idlist lst = 
    List.map (fun a -> match a with | Ast.Identifier(a) -> a) lst

(** takes list and determines if any duplicates are contained within it
    @param lst          list to be considered
        @return                     true if list contains duplicates, false otherwise   *)
let rec typechk_dupExists lst  = 
    match lst with
    | h::tl -> 
                        begin
                        let dup = List.exists ((fun id ->
                    let res = (id=h) in
                        match res with
                        | true -> true 
                        | false -> false)) tl in
                        match dup with
                        | true -> let dupStr = String.concat ""  ["typechk_dupExists: dup found -  "; h] in
                                                print_endline dupStr; true
                        | false ->  typechk_dupExists tl
                        end                                                                                        
  | [] -> false

(** A tuple of (type, errCode).  Used when doing type checks on expressions,
designators and statements.  The functions doing these checks return the inferred
type along with an errCode (which is a non-positive integer). 0 or errNone indicates
that the value in the type field is valid and the actual type derived.  
Any other value indicates that a type error exists and the value in the type field
is not valid, ie undefined. *)
type typChk = TCRes of Ast.typ * int



(** extract type out of (type, errCode) result tuple  
    @param res  type check result value.
        @return     ast type found in the tuple *)  
let get_type_from_result  = function
    | TCRes(t,ec) -> t
 

(** extract error out of (type, errCode) tuple  
    @param res  type check result value.
    @return     error code found in the result tuple *)  
let get_error_from_result = function
    | TCRes(t,ec) -> ec

            
(** get type from   expression 
    @param e    Ast expression data structure
        @param symtbl symbol table
        @return       type check result structure (TCRes) which contains the type along 
        with the relevant error code*)                                                                                                                                                                                                                                                                                                                                                                                  
let rec get_expression_type e packContext = 
    match e with
    | Ast.ExpUnary(uOp,e) ->
          begin 
            match uOp with
            | Ast.UOpMIN | Ast.UOpNOT | Ast.UOpPLS ->
                   if (get_type_from_result (get_expression_type e packContext) = Ast.TypInt) then
                    TCRes(Ast.TypInt, errNone)
                else
                    TCRes(Ast.TypUnk, errExpInv)
            end
  | Ast.ExpBinary(bOp, e1, e2) -> 
            begin
            match bOp with
            | Ast.MOpMT | Ast.MOpDV | Ast.MOpDIV 
            | Ast.MOpMOD | Ast.AOpPL | Ast.AOpSB ->
                if (get_type_from_result (get_expression_type e1 packContext) = Ast.TypInt) && 
                     (get_type_from_result (get_expression_type e2 packContext) = Ast.TypInt) then
                                TCRes(Ast.TypInt, errNone)
                else
                                TCRes(Ast.TypUnk, errExpInv)

      | Ast.ROpLT | Ast.ROpGT | Ast.ROpLTE
      | Ast.ROpGTE | Ast.ROpE | Ast.ROpNE 

      | Ast.BoolAnd | Ast.BoolOr ->
        if (get_type_from_result (get_expression_type e1 packContext) = Ast.TypBool) && 
                     (get_type_from_result (get_expression_type e2 packContext) = Ast.TypBool) then
                    TCRes(Ast.TypBool, errNone)
        else
                TCRes(Ast.TypUnk, errExpInv)
            end
  | Ast.ExpNumber(i) -> TCRes(Ast.TypInt, errNone)
  | Ast.ExpDes(d) -> 
          begin
            let desRes = (snd (get_designator_type d packContext)) in
      match desRes with
            | Ast.TypUnk -> TCRes(Ast.TypUnk, errDesTypMismatch)
            | _ -> TCRes(desRes, errNone)
            end
  | Ast.ExpNil -> TCRes(Ast.TypVoid, errNone) 
    | Ast.ExpTrue -> TCRes(Ast.TypBool, errNone) 
    | Ast.ExpFalse -> TCRes(Ast.TypBool, errNone)
  | Ast.ExpFuncCall(d) ->
         begin 
       let desRes = (snd (get_designator_type d packContext)) in
     match desRes with
     | Ast.TypUnk -> TCRes(Ast.TypUnk, errDesTypMismatch)
     | _ -> TCRes(desRes, errNone)
     end    

  (*
    | Ast.ExpNew of (id, el, ml)  * identifier list (** class name , initial arguement list, mixin name list *)
  
    | ExpAs of designator * identifier
  | ExpHas of designator * identifier
    *)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
(** get list of member identifiers in a class.  We disallow the same names for any member
        for simplicity 
        @param    list of ast class members
        @return   list of string respresenting names of these class members*) 
and typechk_getmembername_from_cls lst = 
    let temp = 
        List.map (fun a -> match a with 
                                                | Ast.MemConst(Ast.Identifier(id),t,vT,e) -> [id]
                                                | Ast.MemAttr(Ast.Variable(idL,t,vT)) -> typechk_getstringlist_from_idlist idL 
                                                | Ast.MemMeth(Ast.Identifier(id),fl,t,c,i) -> [id]
                                                                            
                         ) lst in
                        List.concat temp

                                                                                                                                                
(** check to see if a type is valid.  
        @param  an AST defined data type
        @return errNone if t is a valid type.  A pattern matching runtime error otherwise *)                                                                                                                                                                                                                                                                                                
and is_valid_type t packContext =
  match t with
  | Ast.TypVoid | Ast.TypInt | Ast.TypBool | Ast.TypStr -> errNone
    | Ast.TypClsPtr(Ast.Identifier(id)) -> is_valid_class id packContext
  | Ast.TypArr(il, at) -> is_valid_type at packContext


(** return if all formal parameters are of the correct types 
    also add all formal parameters to the symtbl table*)
and are_formals_valid_types fl packContext =
       let symtbl = Pgmvalues.get_typsymtbl packContext in
     let forResL = List.map (fun a -> match a with 
                                    | (Ast.Identifier(id), t) -> 
                                                                            SymTbl.sym_set_symtbl_val symtbl id
                                                                    (SymTbl.STFormal(t)); 
                                                                            is_valid_type t packContext) fl  in
         get_error_in_list forResL
                                                            
            

                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
(** given a list of member we ensure that they return the correct types *)
and typechk_chk_member_list_type ast packContext = 
    let memResL = 
        List.map (fun a -> 
                    match a with 
          | Ast.MemConst(Ast.Identifier(id),t,vT,e) -> 
                            if ((get_type_from_result (get_expression_type e packContext)) <> t) then 
                            begin 
                                print_string "typechk_chk_member_list_type: const "; 
                                print_string id; 
                                print_endline " type mismatch "; 
                                errConstTypMismatch
                            end
                            else
                            begin 
                                print_string "typechk_chk_member_list_type: const ";
                                print_string id;
                                print_endline " type ok"; 
                                is_valid_type t packContext 
                            end
          | Ast.MemAttr(Ast.Variable(idL,t,vT)) ->
                        
                          (* put the variable into the type symbol table   *) 
                          let symtbl = Pgmvalues.get_typsymtbl packContext in
                            List.map (fun a -> 
                                                    match a with
                                                    | Ast.Identifier(id) ->
                                                      SymTbl.sym_set_symtbl_val symtbl id
                                      (SymTbl.STFormal(t))) idL;
                            
                            (* ensure that values are given the correct type *)
                            is_valid_type t packContext

                                                                
          | Ast.MemMeth(Ast.Identifier(id),fl,t,c,i) -> 
                            let mTypeRes = (is_valid_type t packContext) in
                            if (mTypeRes = errNone) then
                            begin
                                print_string "typechk_chk_member_list_type: method ";
                                print_string id;
                                print_endline " has valid type";
                                let formalTypeRes = are_formals_valid_types fl packContext in
                                if (formalTypeRes = errNone) then
                                begin
                                       (* must check for type agreement relative to method and type coreectness *)
                                    let stmtTypeList =  get_compound_statement_type c packContext in
                                      
                                        if ((List.length stmtTypeList) = 1) then
                                        begin
                                            if (get_type_from_result(List.hd stmtTypeList) = t) then
                                                errNone
                                            else
                                                get_error_from_result(List.hd stmtTypeList)                                         
                                        end
                                        else
                                          begin print_int (List.length stmtTypeList) ; errMethMismatch end
                                        
                                        
                                end
                                else
                                begin
                                    formalTypeRes
                                end
                            end
                            else
                            begin
                                print_endline "typechk_chk_member_list_type: method ";
                                print_string id;
                                print_endline " has invalid type"; 
                                mTypeRes
                            end                                                                     
        ) ast in
                get_error_in_list memResL



and get_dotted_name_helper clsName dl packContext =
    
        (* find the class in the symbol table *)
    let cType = SymTbl.sym_get_symtbl_val (Pgmvalues.get_typsymtbl packContext) clsName in
        
        (* get the attribute name from the ast *)
        let attrName = (List.hd dl) in
        
        match attrName with
        | Ast.DNID(Ast.Identifier(id)) ->
        try 
        match cType with     
        | SymTbl.STClsTyp(typ, cl, al, ml, nl, il) -> 
                (* find the attribute name with the class if it exists *)
                try
                let attr = List.find (fun a -> match a with
                                | SymTbl.STClsAttr(id,i, x, cN) -> true
                                | _ -> false) al in
                
                (* if it does, return related type if possible (in the case of base types)*)
                (* make a recursive call if it is a class type *)
                match attr with
                | SymTbl.STClsAttr(id, i, Ast.TypInt, cN) -> Ast.TypInt    
        | SymTbl.STClsAttr(id, i, Ast.TypBool, cN) -> Ast.TypBool
        | SymTbl.STClsAttr(id, i, Ast.TypChar, cN) -> Ast.TypChar 
        | SymTbl.STClsAttr(id, i, Ast.TypStr, cN)  ->  Ast.TypStr
        | SymTbl.STClsAttr(id, i, Ast.TypClsPtr(Ast.Identifier(clsID)), cN) -> 
                                get_dotted_name_helper clsID (List.tl dl) packContext
        | SymTbl.STClsAttr(id, i, Ast.TypArr(il,t), cN) -> Ast.TypArr(il,t)
                        (* TODO: relook at *)                                                   
(* 
          | Ast.TypArr(il,t) -> Ast.TypArr(il,t) (** array *)
          | Ast.TypUnk -> Ast.TypUnk               (** undecidable type, error condition *)                                 
          | Ast.TypPtr(t) -> Ast.TypUnk     (** memory pointer *)
*)       with Not_found -> print_string "get_dotted_name_helper: "; 
                         print_string id;
                                                 print_endline " not found";
                                                 Ast.TypUnk

            
    with SymTblError _ -> Ast.TypUnk     
    

and get_dotted_name_type dn packContext = 
    match dn with
    | Ast.DottedName(Ast.DNID(Ast.Identifier(id1))::dl) -> 
              begin
                    let idType = get_identifier_type id1 packContext in
                    match idType with
            | Ast.TypVoid -> Ast.TypUnk (* no type specified, used for methods with no return value *)
          | Ast.TypInt -> if (dl=[]) then Ast.TypInt else Ast.TypUnk  (* integer *)
          | Ast.TypBool -> if (dl=[]) then Ast.TypBool else Ast.TypUnk (* boolean *)
          | Ast.TypChar -> if (dl=[]) then Ast.TypChar else Ast.TypUnk (* character *)
          | Ast.TypStr ->  if (dl=[]) then Ast.TypStr else Ast.TypUnk (* string *)
          | Ast.TypPtr(t) -> Ast.TypUnk     (** memory pointer *)
          | Ast.TypClsPtr(Ast.Identifier(clsName)) -> 
                                                    if (dl=[]) then 
                                                            Ast.TypClsPtr(Ast.Identifier(clsName)) 
                                                    
                                                    else 
                                                            get_dotted_name_helper clsName dl packContext
                                                                                            
          | Ast.TypArr(il,t) -> Ast.TypArr(il,t) (** array *) (* think about this one *)
          | Ast.TypUnk -> print_endline "get_dotted_name_type: dotted name not found"; Ast.TypUnk               (** undecidable type, error condition *)                                
          | _ -> print_endline "get_dotted_name_type: invalid type"; Ast.TypUnk 
                    end              (** undecidable type, error condition *)                                
    | _ -> print_endline "get_dotted_name_type: runtime error"; Ast.TypUnk               (** undecidable type, error condition *)                              

    

and get_designator_type d packContext = 
    match d with
    | Ast.Designator(dn, el) -> match el with
                                                        | [] -> print_endline "get_designator_type: false - (dotted name)"; (false, get_dotted_name_type dn packContext) 
                                                        | _ ->  print_endline "get_designator_type: false - (method call - NOT DONE)";(true, Ast.TypInt)  (* TODO: method call *)    
    
    
and get_statement_type s packContext =

    match s with
    | Ast.StmtDesign(Ast.ExpDes(d),e) -> let exprType = get_type_from_result (get_expression_type e packContext) in
                           let exprError = get_error_from_result (get_expression_type e packContext) in
                                                     let desType = get_designator_type d packContext in
                                                     begin 
                                                     if ( ((fst desType) = false) && ((snd desType) = exprType) ) then
                                                        begin 
                                                                print_endline "get_statement_type: designator statement ok";
                                                              (* print_string  "get_statement_type:"; *)
                                                                [TCRes(Ast.TypVoid, errNone)]   
                                                                end                                          
                                                     else if (exprError <> errNone) then
                                                          begin print_endline "get_statement_type: designator statement error"; [TCRes(exprType, exprError)] end
                                                     else
                                                           begin print_endline "get_statement_type: designator statement invalid expression"; [TCRes(Ast.TypUnk, errExpInv)] end
                                                     end
  | Ast.StmtRaise -> [TCRes(Ast.TypVoid, errNone)]
  | Ast.StmtExtend(Ast.ExpDes(d), il) -> [TCRes(Ast.TypVoid, errNone)]
  | Ast.StmtRemove(Ast.ExpDes(d), il) -> [TCRes(Ast.TypVoid, errNone)]
  | Ast.StmtReplace(Ast.ExpDes(d),id1, id2) -> [TCRes(Ast.TypVoid, errNone)]
    | Ast.StmtLocals(ll, c) -> [TCRes(Ast.TypVoid, errNone)]
(*
  | Ast.StmtIf(e, c1, c2) ->    let exprType = get_type_from_result (get_expression_type e packContext) in
                                                            let resC1 = get_compound_statement_type c1 packContext in
                                                            let resC2 = get_compound_statement_type c2 packContext in 
                                                            if (exprType = Ast.TypBool) then    
                                                                List.append resC1 resC2
                                                            else
                                                                [TCRes(Ast.TypUnk, errExpInv)]
  
  | Ast.StmtWhile(e,c) ->  let exprType = get_expression_type e symtbl in
                                                     if (get_type_from_result (exprType) = Ast.TypBool) then    
                                                            get_compound_statement_type c symtbl
                                                     else
                              [TCRes(Ast.TypUnk, errExpInv)]    
                                                                                                                                                                    
  | Ast.StmtVar(Ast.Variable(il,t,v),c) -> 
                                                        (* add variables to the symbol table *)
                                                        print_endline "get_statement_type: variable statement ";
                                                        let symtbl = Pgmvalues.get_typsymtbl packContext in
                                                        (* add level *)
                                                        List.map (fun a -> match a with
                                                                                | Ast.Identifier(id) ->
                                                                                       let logStr = String.concat "" ["get_statement_type: add "; id; "to the symbol table"] in
                                                                                         print_endline logStr;
                                                                                       SymTbl.sym_set_symtbl_val symtbl id (SymTbl.STLocal(t)))  il;                                                            
                                                        (* check form of variable *)
                                                        get_compound_statement_type c packContext
                                                        (* remove level *)
    
  | Ast.StmtConst(i,t,e,c) -> (* check form of constant *)
                                                        (*get_compound_statement_type c symtbl *) [TCRes(Ast.TypUnk, errExpInv) ]
                                                        
                                                        
*)
  | Ast.StmtRet(e) ->  [get_expression_type e packContext]
  | Ast.StmtProcCall(d) -> [TCRes(Ast.TypVoid, errNone)] (* need designator function, ensure argument types match up *)

and get_compound_statement_type c packContext =
    match c with 
    | Ast.Compound(sl) ->
        
        (* last statement defines list's type  *)
        let revL = List.rev sl in
        let lastStmt = List.hd revL in
        let lastStmtTypes = get_statement_type lastStmt packContext in
                
        (* ensure that only one type is possible *)
        let numOfTypes = List.length lastStmtTypes in
        if ( numOfTypes = 1 )   then        
            begin
            print_endline "get_compound_statement_type: start";
                    
            let lastStmtTyp = get_type_from_result (List.hd lastStmtTypes) in
            print_string "get_compound_statement_type: last statement type "; print_ast_typ lastStmtTyp; 
            print_endline "";
                    
            (* check other statements, see if they are void or list type *)
            let otherStmts = List.tl revL in 
            let otherStmtRes = List.concat (List.map (fun a -> get_statement_type a packContext) otherStmts) in
    
            (* if all statements are consistant with list type's type return *)
      (* tha type, otherwise a type error exists and set the list type *)
      (*  to unknown                                                                 *)
            try
                 begin              
                        let errRes =
                        List.find (fun a -> 
                                          let typ = get_type_from_result a in
                                                        let e = get_error_from_result a in

                                                        print_string "get_compound_statement_type: statement type "; 
                                                        print_ast_typ typ; 
                                    print_endline "";
                            begin
                                                        if ((typ = Ast.TypVoid) && (e = errNone)) then false 
                                                        else if ((typ = lastStmtTyp) && (e = errNone)) then false 
                                                        else true
                                                        end
                                            ) otherStmtRes
                            in
                            [errRes]

            end
          with Not_found ->print_endline "get_compound_statement_type: no error"; 
                             [TCRes(lastStmtTyp, errNone)]                      
                    
            end

        else
            begin
            [TCRes(Ast.TypUnk, errStmtInv)]
            end
 
                                                                
                                                                                                                            
                                                                                                                                                                                                                       
(** assume that types are already deemed to be valid. ensure that formal parameters are unique
        and the type of statement is equal to the type of the statement *)                  
(*
let typechk_method ast symtbl = 
   let methResL = 
        List.map (fun a -> match a with 
                            | Ast.MemMeth(Ast.Identifier(id),fl,t,c) ->
                                                                    (* get all formal identifiers, and see if duplicate exists *)
                                                                let fIdL = List.map (fun f -> match f with 
                                                                                                                                | (Ast.Identifier(id),t) -> id ) fl in
                                                                    let fTypeRes = typechk_dupExists fIdL in 
                                  match fTypeRes with
                                  | true -> print_endline "typechk_method: duplicate formal parameters found"; errFrmDup
                                  | false ->
                                                                          begin 
                                                                                
                                                                            print_string "typechk_method: method name: "; print_string id; 
                                                                            print_string " return type ";
                                                                            print_ast_typ  t;
                                                                            print_endline ""; 
                                                                                
                                                                            (* check to ensure statement return type, equals method return type *)
                                                                            let stmtTyps = List.hd (get_compound_statement_type c symtbl) in
                                                                            if ( 
                                                                                    ( (get_type_from_result stmtTyps) = Ast.TypVoid)
                                                                                  || 
                                            ( (get_type_from_result stmtTyps)  = t)
                                                                                 ) then errNone
                                                                            else
                                                                                errMethMismatch
                                                                                
                                                                                
                                                                            (*      
                                                                            let compStmtTyp = List.exists (fun a ->
                                                                                print_string "typechk_method: type ";
                                                                                print_ast_typ (get_type_from_result a); 
                                                                                print_endline "";
                                                                                
                                                                                if ( (get_type_from_result a) = Ast.TypVoid) 
                                                                                || ( (get_type_from_result a)  = t) then true
                                        else false )  stmtTyps in
                                                                             
                                                                            
                                                                            
                                                                            match compStmtTyp with
                                                        | true -> print_endline "typechk_method: no type error"; errNone
                                                                | false -> print_endline "typechk_method: type mismatch error"; errMethMismatch
                                                                            *)                                                                  
                                                                            end
                                                                                                                                               
                ) ast in
        get_error_in_list methResL
    
*)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
let typechk_getclsname_from_clslist lst = 
  List.map (fun a -> match a with | Ast.Cls(Ast.Identifier(id),n,ml,nl,il, h) -> id) lst
                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
(** Cls of identifier * int * memberL * string list * string list *)
let typechk_Cls ast packContext =
  match ast with
    | Ast.Cls(id, i, Ast.MemList(ml), sln, sli, h) -> 
                let mnL = typechk_getmembername_from_cls ml in
                        (* check for duplicates names *)
                let dup = typechk_dupExists mnL in 
                    begin
                match dup with
                | true -> print_endline "typechk_Cls: duplicate member name found"; errMemDup (* raise (Error "typechk_uniList: duplicate unit name found") *)
                | false -> let resChk = typechk_chk_member_list_type ml packContext in
                                                     if (resChk = errNone) then
                                                       begin print_endline "typechk_Cls: no error"; errNone end
                                                     else 
                                                       begin print_endline "typechk_Cls: error"; resChk end                                         
                end
                                
                        
                         

            
            (* 
                        let needsDupChk = typechk_dupExists sln in
                          begin 
                            match needsDupChk with
                            | true -> raise (Error "typechk_uniCls: duplicate needs name found")
                            | false -> ()
                            end;
                        let implDupChk = typechk_dupExists sli in
                          begin
              match implDupChk with
              | true -> raise (Error "typechk_uniCls: duplicate implements name found")
              | false -> ()
                            end;
            
                      let symtbl = Pgmvalues.get_typsymtbl packContext in       
                            SymTbl.sym_add_level_to_typ_symtbl symtbl;
                            typechk_add_class_info_to_symtbl id i ml sln sli packContext; 
                            SymTbl.sym_remove_level_from_symtbl symtbl;                                                     
                        (id, sln, sli) 
                *)



let typechk_get_unitid (id, nl, il) = id 
let typechk_get_unitneeds (id, nl, il) = nl
let typechk_get_unitimplements (id, nl, il) = il 




(** Given a list of classes, this function returns a list of their names *)
let typechk_getclsname_from_clslist lst = 
    List.map (fun a -> match a with | Ast.Cls(Ast.Identifier(id),n,ml,nl,il,h) -> id) lst


let typechk_clsList ast packContext = 
  match ast with
  | Ast.ClsList(cl) -> 

            let clsIdL = typechk_getclsname_from_clslist cl in
                                                                                                     
      (* check for duplicates names *)
      let dup = typechk_dupExists clsIdL in 
      begin
      match dup with
      | true -> print_endline "typechk_clsList: duplicate class name found"; errClsDup (* raise (Error "typechk_uniList: duplicate unit name found") *)
      | false -> errNone (* () *)
      end;
            
            print_endline "typechk_clsList: no duplicate class names found";
            
            (* see if checking classes generates error *)
            let clsResL = List.map (fun c -> typechk_Cls c packContext) cl in
            try List.find (fun a -> (a < errNone)) clsResL with Not_found -> print_endline "typechk_clsList: no class error found"; errNone
                                                            


(** Entry point and the only routine that should be called from outside of the module.
        (except for unit testing). It accepts an abstract syntax tree structure for a 
        particular package and does a check to ensure that all the type rules are followed.  
    
    
    @param ast      abstract syntax tree to be processed *)
let typechk_package astAndGbl = 
  print_endline "";
    print_endline "begin type checking process...";
    let ast = fst astAndGbl in
    let packContext = snd astAndGbl in
  match ast with
  | Ast.Package (i, ul) -> print_endline "typechk_package: start"; typechk_clsList ul packContext; 
     (ast, packContext)
  | Ast.Program (i, ul, cs) -> print_endline "typechk_program: start"; typechk_clsList ul packContext; 
   (ast, packContext)
                                                     
end;;

