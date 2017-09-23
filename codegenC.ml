(**
    This module will generate C code from the abstract symbol table passed
    into the codegen_doc function. 
*)


open Symtbl
open Ast
open Pgmvalues
open Langtypes
open ObjectC
open Typesymtbl



exception Error of string


module CodegenC = 
struct
(** expressions *)

let get_cstring_for_dotted_name (cStr, pt) = cStr
let get_pt_for_dotted_name (cStr, pt) = pt


(** Given an AST formal list of parameters, return the C string that represents it *) 
          

(* expression related functions *)

(** generate code for a Ast.Primary object 
    @param objInfo      information about object currently processed method belongs to 
    @param lhsDes       AST.Designator object, (only used when new object is being 
                        created)
    @param exp          AST.Primary object to be processed
    @param packContext  global variable tuple
    @return             text representing the Ast.Expression node in C compliant *)   
let rec codegen_Expression objInfo ex packContext = 
    let clsName = SymTbl.get_obj_cls_name objInfo in 
    print_endline (String.concat "" ["codegen_Expression: current class "; clsName]);
    
    match ex with
    | Ast.ExpNumber(n) ->  print_endline "codegen_Expression: number found";
                                                 print_int n; print_endline ""; string_of_int n
    | Ast.ExpDes(Ast.Designator(Ast.DottedName(dhl), el)) ->
                                                 print_string "codegen_Expression: designator found ";                
                                                 let dnStr = get_cstring_for_dotted_name (getCstringDottedName dhl clsName false objInfo packContext) in
                                                 print_endline dnStr;
                                                 dnStr                                          
                                                (* probably not needed 
                                                 let expStr = (getCstringExpList objInfo el packContext) in 
                                String.concat "" [dnStr ; expStr]
                                                *)
    | Ast.ExpAs(des, Ast.Identifier(id))  ->
          print_endline "codegen_Expression: as found";
          let asStr =  String.concat "" ["(";id;"_Impl *) castObject(  (void *) ";
                         (codegen_Expression objInfo des packContext);",";get_method_table_name id;")"] in
          print_endline asStr;
          asStr
    |   Ast.ExpNew((t,e,el), il) -> 
        (* class name , initial arguement list, mixin name list *)
        begin
        match e with
        | Ast.ExpNone ->
            (* create a single object of the passed in type *)
            begin
            match t with
            | Ast.TypClsPtr(Ast.Identifier(cn)) ->
                
                let nStr = String.concat "" [cn;"_new()"] in
                let delStr = String.concat "" [ cn ; "_construct (" ; nStr; "," ; (getCstringExpList objInfo el packContext) ;")"] in
                nStr
            end
            
                                                        
                                                | _ ->  (* TODO: initialize array of objects *)
                                                    (* create space to store an array of object pointers *)
                                                        let naStr = String.concat "" [ "(void*)calloc(";
                                                            (codegen_Expression objInfo e packContext);
                                                            ",sizeof(void*))"] in
                                                            naStr
                                                end
                                                
                                                
                                                
                                                
                                                
                                                (* for now, do not support initializating arguements *)
                        (* String.concat "" [id ; "_new (" ; (getCstringExpList objInfo el packContext) ;");"]  *)                                          
                                                (*
                                                let t = SymTbl.get_entry_type cgSymtbl tySymtbl id clsName in
                                        let astTyp = SymTbl.get_ast_type_for_id t in
                                                
                                                match astTyp with
                                                | Ast.TypClsPtr(Ast.Identifier(cn)) -> 
                                                    begin               
                                                    let newStr = (getCstringExpList objInfo el packContext) in 
                                                        String.concat "" [cn; ]
                                                    end 
                                                
                                                | _ -> "codegen_Expression: error - bad new statement"
                                String.concat "" [dnStr ; "("; ptStr; ",";  expStr ; ");"]
                                                *)
                                                             
                                                (* Stack_Interface* s = Stack_new();    *)
                                                            
  | Ast.ExpFuncCall(Ast.Designator(Ast.DottedName(dhl), el)) ->
                         print_endline "codegen_Expression: func call found"; 
                                                let tempStr = getCstringDottedName dhl clsName true objInfo packContext in
                                                let dnStr = get_cstring_for_dotted_name tempStr in
                                                let ptStr = get_pt_for_dotted_name tempStr in
                                                
                                                begin
                                                match el with
                                                | [] -> String.concat "" [dnStr ; "()"]
                        | _ -> 
                                                        let expStr = (getCstringExpList objInfo el packContext) in 
                                String.concat "" [dnStr ; "("; ptStr; ",";  expStr ; "))"]
                                                end 

    | Ast.ExpTrue ->            print_endline "codegen_Expression: true found"; "1"
    | Ast.ExpFalse ->     print_endline "codegen_Expression: true found"; "0"
    | Ast.ExpNil ->  print_endline "codegen_Expression: nil found"; ""
    | Ast.ExpBracketExp(e) -> String.concat "" ["("; codegen_Expression objInfo e packContext ; ")"]
    | Ast.ExpUnary(uniOp , exp1) ->
                                                let uOp = 
                                                match uniOp with
                                                | Ast.UOpMIN -> "-"
                                                | Ast.UOpNOT -> "!"
                                                | Ast.UOpPLS -> "+"
                                                in
                                                String.concat "" [uOp; (codegen_Expression objInfo exp1 packContext)]
   | Ast.ExpBinary(binOp , exp1, exp2) ->
                                                let bOp = 
                                                match binOp with
                                            | Ast.MOpMT -> "*"
                            | Ast.MOpDV -> "/"
                            (* | MOpDIV (** integer divide *) *)
                            | Ast.MOpMOD -> "%"
                                    
                                                | Ast.AOpPL -> "+"
                            | Ast.AOpSB -> "-"
                                            | ROpLT -> "<"
                            | Ast.ROpGT -> ">"
                            | Ast.ROpLTE -> "<="
                            | Ast.ROpGTE -> ">="
                            | Ast.ROpE -> "=="
                            | Ast.ROpNE -> "!="
                        | Ast.BoolAnd -> "&&"
                            | Ast.BoolOr -> "||"
                                                in
                        String.concat "" [(codegen_Expression objInfo exp1 packContext); bOp; (codegen_Expression objInfo exp2 packContext) ]

(*
and codegen_Actuals objInfo act packContext = 
  match act with
   | Ast.Actuals(el) ->
     List.map (fun a ->  fst (codegen_Expression objInfo a packContext)) el 
   | Ast.ActualsEmp -> [] 
*)


and get_method_table_name clsName = 
  String.concat "" [(String.lowercase clsName); "_MethodTable"];

and getCstringExpList objInfo el packContext =
  match el with
  | [] -> ""
  | _ -> 
    if ((List.length el) = 1) then
      match (List.hd el) with 
      | a -> codegen_Expression objInfo a packContext
    else 
      let firstExp = codegen_Expression objInfo (List.hd (List.rev el)) packContext in
      let restofExps = List.tl (List.rev el) in
      let allButLastExp = 
      List.fold_left  (fun a b -> 
        String.concat " " [ a; ","; (codegen_Expression objInfo b packContext)]) "" restofExps in
        
        
      String.concat "" [firstExp ; allButLastExp]; 
      (* removed square brackets here as a test *)
 
 
 
(** return name of method, given a list of dotted name helpers
        @param  dhl             list of dotted name helpers
        @return                     string of method name
        
        In the statement, a.b[1].c(2,3,4), the string "c" is returned.
        The dotted name helper list passed to this function should be
        [ a ; b ; [1] ;c ]

*)
and getMethNameString dhl = 
        let dh = List.hd (List.rev dhl) in 
                begin
        match dh with 
        | Ast.DNID(Ast.Identifier(id)) -> id
                end
    
          
and getMethCastString clsName = String.concat "" [clsName;"_Methods*"] 
and getImplCastString clsName = String.concat "" [clsName; "_Impl*"]

and getMethodCallString ptString clsName methName isSelfCall =
    let tempStr = 
        match isSelfCall with
        | true -> "bottom"
        | false -> "up" in
    
    String.concat "" [ "(("; (getMethCastString clsName) ; ")("; 
            ptString; "->"; clsName ; "_"; tempStr ;")"; "->methods)->"; methName ] 

(*
((Stack_Methods* )(s->Stack_bottom)->methods)->push   
*)


and getCstringDottedNameHelper curClsName curTempVarNum curPtStr isSelfCall dhl objInfo packContext =
  print_endline (String.concat "" ["getCstringDottedNameHelper: class name "; curClsName; " - current string "; curPtStr]);
  let tySymtbl = Pgmvalues.get_typsymtbl packContext in
    
    if ((List.length dhl) == 0) then 
        (curPtStr, curPtStr)
    else
      
    (* get first element *)
    let hdDhl = List.hd dhl in
    let restOfDhl = List.tl dhl in
    
  match hdDhl with 
  | Ast.DNExp(el)  ->
        (* array expression *)
                begin
                let tempStr = String.concat "" [curPtStr; "["; (getCstringExpList objInfo el packContext) ; "]"] in  
                match restOfDhl with
                | [] -> (tempStr, tempStr)
                (* termining condition - emit code curPtStr + il, since we know it is an attribute *)
                | _ ->
                (* add index string to current pointer string, and continue processing list *)
                        getCstringDottedNameHelper curClsName curTempVarNum tempStr isSelfCall restOfDhl objInfo packContext
                end
    | Ast.DNID(Ast.Identifier(id)) ->
        (* identifier *)
                        
          if (SymTbl.sym_is_id_in_class curClsName id tySymtbl) then
            (* identifier is a member of the current class *)

                    match (SymTbl.sym_mem_type_of_id_in_class curClsName id tySymtbl) with 
                    | MemTypConst | MemTypAttr ->
                        begin
                        let tempStr = String.concat "" [curPtStr; "->" ; id] in
                        print_endline (String.concat "" ["getCstringDottedNameHelper: tempStr created"; tempStr]);
                        match restOfDhl with
                        | [] -> 
                            (* terminating condition - emit code *)
                            print_endline (String.concat "" ["getCstringDottedNameHelper: emitting final statement ";tempStr]);
                            (tempStr,curPtStr) 
                        | _ -> 
                            (* create temporary variable statement, add it to queue and continue processing *)
                            begin
                                print_endline (String.concat "" ["getCstringDottedNameHelper: recurse ";id ; " - "; curClsName]);
                                (* deterine the ast type of the current identfier *)
                                let curAstType = SymTbl.sym_type_of_id_in_class curClsName id tySymtbl in
                                let curAstStr = Langtypes.get_type curAstType in 
                                
                                let tempVarStr = String.concat "" ["zzz"; string_of_int (curTempVarNum)] in
                                let cLine = String.concat "" ["\t"; tempVarStr; "="; tempStr] in
                                
                                print_endline (String.concat "" ["last try: statment added to queue:"; cLine]);
                                
                                Queue.add cLine (Pgmvalues.get_c_code packContext);
                                getCstringDottedNameHelper curAstStr (curTempVarNum + 1) tempVarStr isSelfCall restOfDhl objInfo packContext
                            end
                        end
                    | MemTypMeth ->
                        begin
            match restOfDhl with
            | [] ->
                begin 
                (* terminating condition - emit code *)
                let tempStr = getMethodCallString curPtStr curClsName id isSelfCall in 
                
                (* find return type of method *)
                let methType = SymTbl.sym_find_type_method_in_class curClsName id tySymtbl in
                                             
                print_endline (String.concat "" ["getCstringDottedNameHelper: emitting final method statement ";tempStr]);
                let x = match isSelfCall with 
                | true -> "bottom"
                | false -> "up"
                in
                (String.concat "" ["(" ; tempStr ] ,String.concat "" [ curPtStr; "->"; curClsName ; "_"; x]) 
                end
            | _ -> ("last try: **** method calls in the middle of the list not supported for now *****", "") 
                        end
          | MemTypUnk -> print_endline (String.concat "" ["getCstringDottedNameHelper:unknown type returned"]);
                                                ("asdfasdfasdfadsfasdfasdf", "");
            else
                (* a local or formal *)
                begin
                print_endline (String.concat "" ["getCstringDottedNameHelper: not found  ";id ; " - "; curClsName]);
                ("lasdfsdfsdfsdf", "")
                end
             
                 


and getCstringDottedName dhl clsName isMethCall objInfo packContext = 
    let cgSymtbl = Pgmvalues.get_codegensymtbl packContext in
    let tySymtbl = Pgmvalues.get_typsymtbl packContext in
        
  let hdDhl = List.hd dhl in
  let restOfDhl = List.tl dhl in
  begin  
  match hdDhl with
  | Ast.DNID(Ast.Identifier(id)) ->
      (* check the type of the first identifer in the list  *)
     print_endline (String.concat "" ["getCstringDottedName: first identifier - "; id; " current class - "; clsName]);
     begin
       let t = SymTbl.get_entry_type cgSymtbl tySymtbl id clsName in
       let symTblTyp = SymTbl.get_var_type_for_id t in 
       match symTblTyp with
       | SymTbl.FormalID | SymTbl.LocalID  -> 
         (* formal or local variable *)
         begin
           let astType = SymTbl.get_ast_type_for_id t in
           begin
             match astType with
             | Ast.TypClsPtr(Ast.Identifier(cn)) -> 
               (* class type *)
               print_endline (String.concat "" ["getCstringDottedName: class type local/formal first identifier - "; id; " current class - "; cn; "******"]); 
               getCstringDottedNameHelper cn 0 id true restOfDhl objInfo packContext
             | Ast.TypArr(il,Ast.TypClsPtr(Ast.Identifier(cn))) ->
               (* array type *) 
                print_endline (String.concat "" ["getCstringDottedName: array type local/formal first identifier - "; id; " current class - "; cn; "******"]); 
                getCstringDottedNameHelper cn 0 id true restOfDhl objInfo packContext
             | Ast.TypArr(il,t) -> 
               (* fundamental array type - no need to continue processing, just grab array value *)
                print_endline (String.concat "" ["getCstringDottedName: fundamental type local/formal first identifier - "; id; "******"]); 
                (String.concat "" [id; "tutererr"] , "")
             | _ -> 
               (* fundamental type - no need to continue processing *)
               print_endline (String.concat "" ["getCstringDottedName: fundamental type local/formal first identifier - "; id; "******"]); 
               (id, "")
           end
         end
       | SymTbl.ClsMemID -> 
         (* class member variable *) 
         print_endline (String.concat "" ["&&&& getCstringDottedName: first identifier - "; id; " current class - "; clsName; "&&&"]);
         begin
           (* now see if the identifier is an object or a class member *)
           let astType = SymTbl.get_ast_type_for_id t in
           let selfStr = String.concat "" ["self->"; id] in 
           match astType with
           | Ast.TypClsPtr(Ast.Identifier(cn)) ->
             (* object, recurse to see if is composed with sub objects *)  
             print_endline (String.concat "" ["getCstringDottedName: class member first identifier - "; id; " current class - "; cn; "******"]); 
             getCstringDottedNameHelper cn 0 selfStr true restOfDhl objInfo packContext
           | _ ->
             (* class member *)
             (* now emit code depending on the member type *) 
             begin
             match (SymTbl.sym_mem_type_of_id_in_class clsName id tySymtbl) with 
             | MemTypConst | MemTypAttr -> getCstringDottedNameHelper clsName 0 selfStr false restOfDhl objInfo packContext (*(selfStr, "") *)
             | MemTypMeth ->
                 begin
                   let tempStr = getMethodCallString "self" clsName id true in                             
                   print_endline (String.concat "" ["getCstringDottedNameHelper: emitting final method statement ";tempStr]);
                                
                   (* find return type of method *)
                   let methType = SymTbl.sym_find_type_method_in_class clsName id tySymtbl in
                   (String.concat "" ["(" ; tempStr] ,String.concat "" ["self->"; clsName; "_bottom"])
                 end
             | MemTypUnk -> ("error condition, member unknown", "")
             end
           end
      | SymTbl.ClsID -> 
                (* class identifier *)
                begin
                let selfStr = String.concat "" ["self->"; id] in
                getCstringDottedNameHelper id 0 "self" false restOfDhl objInfo packContext
                end
               (* getPointerString (String.concat "" ["self->"; id]) restOfMethPtDhl *)
      | _ -> ("srtret", "")
            end 
  | Ast.DNExp(el) -> ("getCstringDottedName: not well formed, dotted name can't start with array indices", "") 
  end










(** Given an AST type, return the C string that represents it
        @param          ast type
        @return         valid c string corresponding to type *)     
and getCstringType astType = 
    match astType with
    | Ast.TypVoid -> "void"
    | Ast.TypInt -> "int"
    | Ast.TypBool -> "short"
    | Ast.TypChar -> "char" 
    | Ast.TypStr -> "char[]"     
    | Ast.TypPtr(typ) -> String.concat "" [ (getCstringType typ); "*" ]
    | Ast.TypClsPtr(Ast.Identifier(id)) ->  String.concat "" [id]
    | Ast.TypArr(il, typ) -> String.concat "" [getCstringType typ; "*"] 
    | Ast.TypUnk -> "getCstringType: unknown type"
        
 
    
 (** Given an AST type, return the C string that is used to declare a
     variable of that type
        @param          ast type
        @return         valid c string for declaring  type *)     
and getCstringTypeDeclaration astType = 
    match astType with
    | Ast.TypVoid -> "void"
    | Ast.TypInt | Ast.TypBool | Ast.TypChar  -> getCstringType astType
    | Ast.TypPtr(typ) -> getCstringType (Ast.TypPtr(typ))
    | Ast.TypStr -> "char[]"     
    | Ast.TypClsPtr(Ast.Identifier(id)) ->  String.concat "" [getCstringType astType ;"_Impl*"]
    | Ast.TypArr(il, typ) -> String.concat "" [getCstringType typ; "*"] 
    | Ast.TypUnk -> "getCstringType: unknown type"          
        
(** Given an AST formal list of parameters, return the C string that represents it *)           
and getCstringFormalList fl = 
    match fl with
    | [] -> ""
    | [Ast.Identifier(id), typ]  -> String.concat " " [ (getCstringType typ); id ]
    | _ ->
    let lastParameter = List.hd (List.rev fl) in
    let restofParameters = List.rev (List.tl (List.rev fl)) in 
    
    (* all but the last have commas at the end *)
    let allButLastCString = List.fold_left  (fun a b -> 
                                                    match b with
                                                    | (Ast.Identifier(id), typ) -> 
                                                        String.concat " " [ a; (getCstringType typ); id ;","]
                                    )   "" restofParameters in
                                    
    (* merge with last one which as no comma, if no parameters were passed *)
    (* return an empty string *)                                
    match lastParameter with
    |  (Ast.Identifier(id), typ) -> String.concat " " [ allButLastCString; (getCstringType typ); id]
    |  _ -> ""
    

and getCstringIntList il = 
    let lastInt = List.hd (List.rev il) in
    let restofInts = List.rev (List.tl (List.rev il)) in
    let allButLastInt = 
        List.fold_left  (fun a b -> String.concat " " [ a; ","; (string_of_int b)]) "" restofInts in                                
      
      String.concat "" ["["; allButLastInt ; string_of_int lastInt ; "]"]








(* ****************** *)



(**
{b Stmts} 

This module provides functionality to generate LLVM code for statements in the
mx language.

*)


(** This method will generate code for a designator statement of the form
    designator [":=" expression].  
    The designator is looked up in the symbol table.  It can be a local variable, 
    object or object attribute.
    @param      objInfo information about the current object    
    @param      lhsd    designator being updated
    @param      rhs     expression to be evaluated
    
*)
and codegen_StmtDesign objInfo lhsd rhs packContext = 
    let clsName = SymTbl.get_obj_cls_name objInfo in
    print_endline (String.concat "" ["codegen_StmtDesign: current class "; clsName]); 
    
  match lhsd with
  | Ast.Designator(Ast.DottedName(il), el) -> 
                
                (*      
                List.iter (fun a -> match a with
                | Ast.DNID(Ast.Identifier(firstDNStr)) -> print_string "codegen_StmtDesign: dotted name:"; print_endline firstDNStr
                | Ast.DNExp(il) -> print_string "codegen_StmtDesign: dotted name"
                ) il;
                *)
                print_endline "codegen_StmtDesign: designator statement";
            (* Codegen the rhs. *)
        let rhsExpString = codegen_Expression objInfo rhs packContext in

                (* put C code here *)
                let lhsDNCString = get_cstring_for_dotted_name 
                                        (getCstringDottedName il clsName false objInfo packContext) in
                let lhsExpLstCString = getCstringExpList objInfo el packContext in
                let lhsFinalString =                  
                   if (String.length lhsExpLstCString > 0) then
                        ( String.concat "" [ lhsDNCString;"(";lhsExpLstCString;")" ]  )
                   else
                       lhsDNCString in
                
                let cLine = String.concat "" [ "\t" ;  lhsFinalString;
                                             " = "; rhsExpString; ";"] in
                print_endline cLine;
           Queue.add cLine (Pgmvalues.get_c_code packContext);



(** This method will generate code for a extend statement of the form
    extend designator with identifier list
         
    The since a designator is an expression, code generation is handled by ExprC.codegen_Expression
        routine after ensuring that the expression is an idetnifier. 
    @param      objInfo information about the current object    
    @param      ex      designator representing object being extended
    @param      idl     list of identifiers representing objects being extended
        @param      packContext 
    
*)
and codegen_ExtendStatement objInfo ex idl packContext = 
        print_endline "codegen_ExtendStatement: extend statement found";
      match ex with
            | Ast.ExpDes(d) ->
                    begin 
                    let expC = codegen_Expression objInfo ex packContext in 
                    List.iter (fun i ->
                                   match i with
                                   | Ast.Identifier(id) ->  
                                            let cLine = String.concat "" 
                                                ["\t";id; "_extend((Object_Interface *)"; expC; ");"] in
                                            Queue.add cLine (Pgmvalues.get_c_code packContext)
                              ) idl
                    end
            | _ -> print_endline "codegen_ExtendStatement: invalid extend statement"
    

(** This method will generate code for a implement statement of the form
    implement designator with identifier list
         
    The since a designator is an expression, code generation is handled by ExprC.codegen_Expression
        routine after ensuring that the expression is an idetnifier. 
    @param      objInfo information about the current object    
    @param      ex      designator representing object being extended
    @param      idl     list of identifiers representing objects being extended
        @param      packContext 
    
*)
and codegen_ImplementStatement objInfo ex idl packContext = 
        print_endline "codegen_ImplementStatement: implement statement found";
      match ex with
            | Ast.ExpDes(d) ->
                    begin 
                    let expC = codegen_Expression objInfo ex packContext in 
                    List.iter (fun i ->
                                   match i with
                                   | Ast.Identifier(id) ->  
                                            let cLine = String.concat "" 
                                                ["\t";id; "_implement((Object_Interface *)"; expC; ");"] in
                                            Queue.add cLine (Pgmvalues.get_c_code packContext)
                              ) idl
                    end
            | _ -> print_endline "codegen_ImplementStatement: invalid extend statement"
    

    

and codegen_IfStatement  objInfo cd then_ else_  packContext =

      print_endline "codegen_IfStatement: if statement found";
                      
      let expC = codegen_Expression objInfo cd packContext in 
      let cLine = String.concat "" ["if ("; expC; ") {"] in
            Queue.add cLine (Pgmvalues.get_c_code packContext);
      print_endline "codegen_IfStatement: condition statement done"; 
      codegen_Compound objInfo then_ packContext; 

      print_endline "codegen_IfStatement: emit else value";
      Queue.add "} else {" (Pgmvalues.get_c_code packContext);
            
      codegen_Compound objInfo else_ packContext; 
      Queue.add "}" (Pgmvalues.get_c_code packContext);
            ()
             
            
and codegen_WhileStatement objInfo cd while_ packContext =

        print_endline "codegen_WhileStatement: while statement found";
                      
      let expC = codegen_Expression objInfo cd packContext in 
      let cLine = String.concat "" ["while ("; expC; ") {"] in
            Queue.add cLine (Pgmvalues.get_c_code packContext);
      print_endline "codegen_WhileStatement: condition statement done"; 
      codegen_Compound objInfo while_ packContext; 
      Queue.add "}" (Pgmvalues.get_c_code packContext);
      ()


                        
                                                                        
and codegen_ProcCallStatement objInfo dn el packContext = 
      ()
        
                
                                                
and codegen_Statement objInfo smtl packContext =
    let cgSymtbl = Pgmvalues.get_codegensymtbl packContext in
  match smtl with 
  (* core type *)
  | Ast.StmtDesign(Ast.ExpDes(dn),ex) -> 
            print_endline "codegen_statement: assignment statement";
           (codegen_StmtDesign objInfo dn ex packContext) 
                    
  | Ast.StmtIf(c,th,el) -> print_endline "codegen_statement: if statement"; 
            (codegen_IfStatement objInfo c th el packContext) 
                
  | Ast.StmtWhile(c,wh) -> print_endline "codegen_statement: while statement"; 
            (codegen_WhileStatement objInfo c wh packContext)
  
  | Ast.StmtProcCall(e) -> print_endline "codegen_statement: proc statement";
                        begin
                        match e with 
                        | Ast.ExpFuncCall(fc) ->
                                    let v = (codegen_Expression objInfo e packContext) in
                                    let cLine = String.concat "" [ "\t"; v; ";" ] in
                        Queue.add cLine (Pgmvalues.get_c_code packContext)
                        end
                        

  | Ast.StmtLocals(Ast.Locals(lvl), cd) -> print_endline "codegen_statement: local var statement ";
       (* create a declaration statement for each local variable and add it to the symbol table *)
       List.iter (fun lv ->                                        
                         match lv with
                         | Ast.VCVar(Ast.Variable(il, typ, v)) ->
                              List.map (fun a -> match a with
                                                 | Ast.Identifier(id) ->
                                                   (* put identifier in the symbol table *)   
                                                   SymTbl.sym_set_symtbl_val cgSymtbl id (SymTbl.STLocalCG(typ));
                                                                                                    
                                                   (* generate C code for declaring variable *)
                                                   let cLine = 
                                                         begin
                                                         match typ with
                                                         | Ast.TypArr(il, typ) ->
                                                             String.concat "" [ "\t"; getCstringTypeDeclaration typ; " ";
                                                                               id; ";" ] 
                                                         | _ ->  
                                                             String.concat "" [ "\t"; getCstringTypeDeclaration typ; " ";  
                                                                               id; ";" ]
                                                         end in  
                                                  Queue.add cLine (Pgmvalues.get_c_code packContext)
                                        ) il ; ()
                                                                                        
                                            )  lvl ;
      codegen_Compound objInfo cd packContext

                        
  | Ast.StmtExtend (ex, idl) -> print_endline "codegen_statement: extend statement"; 
            codegen_ExtendStatement objInfo ex idl packContext

  
  | Ast.StmtImplement(ex, il ) -> print_endline "codegen_statement: implement statement"; 
            codegen_ImplementStatement objInfo ex il packContext
            
(*
                                    
                                    
  | Ast.StmtUse (d, id, c) ->     print_endline "codegen_statement: use statement"; 
                                    codegen_UseStatement objInfo d id packContext

  
  | Ast.StmtRemove (d, id) ->     print_endline "codegen_statement: remove statement"; 
                                    codegen_RemoveStatement objInfo d id packContext

*)    
  
  | Ast.StmtRet (e) ->    print_endline "codegen_statement: return statement"; 
                                                    let v = codegen_Expression objInfo e packContext in
                                                    let cLine = String.concat "" [ "\t return "; v ; ";"] in
                                        Queue.add cLine (Pgmvalues.get_c_code packContext);
  | _ -> raise (Error "codegen_Statement: no valid match")  
    



(* compound code *)
and codegen_Compound objInfo cd packContext =   
  match cd with
  | Ast.Compound(sl) -> print_string "codegen_Compound: number of statements:";
                        print_int (List.length sl); print_endline "";
                        List.iter (fun s -> codegen_Statement objInfo s packContext) 
                        sl
  | _ -> raise (Error "codegen_Compound: no valid match")







(* ************************ *)



(** procedures *)

(** formals related functions 

These functions extract parts of an formal object structure.  Formals are method arguments 
that have a name and type componenent *)


(** function takes in 
let get_formal_type typObj packContext = 
  match typObj with
  | (Ast.Identifier (id), t) -> 
          let logStr = String.concat "" 
          [ "get_formal_type: formal name "; id ; " type " ; 
                    Langtypes.get_type t packContext ] in
      print_endline logStr;
            fst (Langtypes.get_type t packContext)
*)

let get_formal_name = function
  | (Ast.Identifier (id), t) -> id



(** This function is used by the system to take in a list containing a method's
    arguments (represented by "formal" entities in the language definition) and
    and a single element containing a class type.  This returns an array containing
    types. The first element of the array produced is the single element represents
    a pointer to the class type that the method is a member of.  This is then followed
    by the type encoded in the elements of the list (in the same order).  This array 
    will be passed in to the LLVM API to generate intermediate code for the method
    signature.
    @param clstype      Pointer type of class the method belonged to
    @param lst          List of Formal AST objects which represents method arguments
    @param addthis      if true, add type of implicit this pointer. 
let make_args_array addthis clstype lst packContext = 
    match addthis with
    | true ->
    Array.of_list(  clstype::(List.map (fun a -> get_formal_type a packContext ) lst)  )
    | false ->
    Array.of_list(  (List.map (fun a -> get_formal_type a packContext ) lst)  )    
*)


(** method generates C code for a function pointer for a particular method *)
let add_method_pointer_struct clsName meth packContext = 
 match meth with
 | Ast.MemMeth (Ast.Identifier(id), fl, ty, cd, i) ->
     let temp =
      match i with
      | true ->  getImplCastString clsName 
      | false -> getCstringType ty
      in
         
      let cLine = String.concat "" [ "\t"; temp ;" (*" ; id ; ")(void*," ; (getCstringFormalList fl) ; ");" ] in
          Queue.add cLine (Pgmvalues.get_c_code packContext)
    

(** method generates C code for a function pointer for a init function *)
let codegen_method_pointer_init clsName meth packContext = 
 match meth with
 | Ast.MemMeth (Ast.Identifier(id), fl, ty, cd, i) ->  (* TODO: re examine, see if there is a cleaner way *)
      let temp =
      match i with
(*      | true -> getImplCastString clsName *)
      | false -> getCstringType ty
      in
          
      let cLine = 
                    String.concat "" 
                     [ "\t((" ; clsName ; "_Methods*)"; (get_method_table_name clsName); ")->";
                                id ; " = ("; temp ;"(*)(void *," ;
                               (getCstringFormalList fl) ; ")) &" ;
                                  clsName ; "_" ; id ; ";"] in

      Queue.add cLine (Pgmvalues.get_c_code packContext)

let get_extend_arg_name clsName = 
 String.concat "" [(String.lowercase clsName) ; "_arg"]

let get_extend_temp_name clsName = 
 String.concat "" [(String.lowercase clsName) ; "_temp"]
(** create argument list for extension method *)
let codegen_extend_arg_list nl = 
    
  (* get the needed identifiers *)
    let needStrList = List.map (fun a ->
                      match a with 
                  | Ast.TypClsPtr(Ast.Identifier(id)) ->  id
                                  ) nl in
    
    if ((List.length needStrList) = 0) then
              ""
    (* if the list is 1 element long, just return that string *)
  else if ((List.length needStrList) = 1) then    
     match needStrList with 
     | [id] -> String.concat "" [","; id ; "_Interface* " ; (get_extend_arg_name id)]
  
    else
       let firstParameter = List.hd needStrList in
         let restOfParameters = List.tl needStrList in 
                                                                      
       List.fold_left (fun i a -> 
         (*String.concat "" [a ; "," ; i] *)
                             match a with 
                  | id ->  String.concat "" [i ; "," ; id ; "_Interface* " ; get_extend_arg_name id]  
                            ) firstParameter restOfParameters


let codegen_bottomdate_bottom_pointer_method clsName nl packContext = 
    
    let needClsName = 
    try 
      match (List.hd nl) with
      | Ast.TypClsPtr(Ast.Identifier(id)) -> id
    with 
    Failure desc -> "Object"
    in
    
    
    print_endline (String.concat "" ["codegen_bottomdate_bottom_pointer_method:"; clsName;"\n needset\n"]);
 
       
    let nsl = Typesymtbl.needset clsName packContext in
    (* List.iter (fun a -> print_string a; print_string ",") nsl; *)
   
    (* for each of these grab any class that implements them *)
    let nlsImplL = List.flatten ( List.map (fun a -> Typesymtbl.implements a packContext) nsl )   in
    (* print_endline "\n needs implementations\n"; 
    List.iter (fun a -> print_string a; print_string ",") nlsImplL;
    *)
    
    match nsl with
    | [] -> ()
    | _ ->   let tempL = nsl@nlsImplL in
                 (*
                 print_endline "\n temp\n"; 
                    List.iter (fun a -> print_string a; print_string ",") tempL;
                 *)
             let clauseStr = (String.concat "" ["\n\t\tp->methods==";get_method_table_name (List.hd tempL)]) in
             let t = List.fold_right 
                    (fun a b -> String.concat "" ["\n\t\tp->methods==";get_method_table_name a;
                    " ||"; b]  ) 
                     (List.tl tempL) clauseStr in
                    

           let ifStmt = String.concat "" 
               [ "\tif (\n";
                 "\t\t"; t  ;"\n\t  )\n\t\t";
                  "(("; getImplCastString clsName; ") p)->";
                 needClsName;"_bottom=e;" 
               ] in
           ignore (Queue.add ifStmt (Pgmvalues.get_c_code packContext) );
              
    
    let setString = (String.concat "" ["\t"; needClsName; "_bottomdateSelfPointers(p,e);"]) in
    Queue.add setString (Pgmvalues.get_c_code packContext) 
    

    (*
     match nsl with
     | [] -> print_endline "empty list"
     | _ -> List.iter (fun a -> let t = String.concat "" ["codegen_bottomdate_bottom_pointer_method" ; clsName; "pointer decendents - class: "; a] in
                                Queue.add t (Pgmvalues.get_c_code packContext)
                      ) nsl
     *)
     
let codegen_extend_method clsName nl il packContext = 
    
    let needClsName = 
    try 
      match (List.hd nl) with
      | Ast.TypClsPtr(Ast.Identifier(id)) -> id
    with 
    Failure desc -> "Object"
    in
    
    (* get name of class that is one implements, could be itself  *)   
    let implementsClsName = 
    try 
      match (List.hd il) with
      | Ast.TypClsPtr(Ast.Identifier(id)) -> id
    with 
    Failure desc -> clsName
    in 
    
    
   let cAlloc = String.concat "" 
         [ "\t"; (getImplCastString clsName);" x = ("; getImplCastString clsName ;") malloc(sizeof(" ; 
              clsName ; "_Impl));\n" ;
           "\tObject_Interface* bottom = findBottom(object_arg,  "; (get_method_table_name needClsName) ; ");\n" ;
           "\tObject_Interface* root = bottom->Object_cycle;\n";"\tx->methods = "; (get_method_table_name clsName) ;";\n\n"
              ] in
   Queue.add cAlloc (Pgmvalues.get_c_code packContext);     
    
    
    let extPtList = Typesymtbl.extension_chain clsName packContext in
    List.iter (fun a ->
                let s = String.concat "" 
                  ["\tx->" ; a ; "_bottom = (" ; (getImplCastString a); ") x;"] in
                Queue.add s (Pgmvalues.get_c_code packContext)
              ) extPtList;
               

    let s = String.concat "" ["\tx->" ; implementsClsName ; "_bottom = (struct " ; (getImplCastString implementsClsName) ;") x;"] in
      Queue.add s (Pgmvalues.get_c_code packContext);
    
    
    if (List.length nl > 0) then
    begin
    let updateSelfPtStmt = String.concat ""
       [ "\n\tObject_Interface* r = root->Object_cycle;\n";
         "\tdo {\n";
         "\t\t"; clsName; "_bottomdateSelfPointers(r, x);\n"] in
       Queue.add updateSelfPtStmt (Pgmvalues.get_c_code packContext);
    
    (* get list of transitive needs *)  
    let extPtList = Typesymtbl.extension_chain clsName packContext in
    
    (* for each of these grab any class that implements them *)
    let extPtListImplL = List.flatten (List.map (fun a -> Typesymtbl.implements a packContext) extPtList  ) in 
    
    List.iter (fun a -> 
      
              (* get list of decendents of element minus the current class *)  
              let t = (* List.filter (fun a -> a <> clsName) *)a::(Typesymtbl.decendents a packContext) in
    
              (* for each of these grab any class that implements them *)
              let tL = List.flatten (List.map (fun ai -> Typesymtbl.implements ai packContext) extPtList  ) in 
    
      
      
              let temp = (String.concat "" ["\n\t\tr->methods==";get_method_table_name clsName;""]) in
              
                  let mapClause = List.map  (fun a -> String.concat "" 
                  ["\n\t\tr->methods==";get_method_table_name a])
                  (t@tL)  in
                  
                  let s = String.concat "||" ("0"::mapClause) in
              (*
              let s = List.fold_right  (fun a b -> String.concat "" 
                  ["\n\t\tr->methods==";get_method_table_name a;"||"; b])
                  (extPtList@extPtListImplL) "" in
              *)
              let ifclauseStr = String.concat "" [s] in 
                
               let s = String.concat "" 
                  ["\t\tif ("; ifclauseStr;
                  "\n\t\t)  {\n\t\t\t ( ("; (getImplCastString clsName);
                  ")  x) -> " ; a ; "_up = ("; (getImplCastString a) ; ") r;\n\t\t}"] in
                Queue.add s (Pgmvalues.get_c_code packContext)
              ) extPtList;
              
        
    let updateSelfPtStmt2 = String.concat ""
       [ "\t\tr = r->Object_cycle;\n";
         "\t} while (r != root);\n"
       ] in
    Queue.add updateSelfPtStmt2 (Pgmvalues.get_c_code packContext)
    end;
  
    let cExtendList = String.concat "" 
         [
           "\tbottom->Object_cycle = (Object_Interface*) x;\n";
           "\tx->Object_cycle = root;\n"
         ] in
    Queue.add cExtendList (Pgmvalues.get_c_code packContext);
 


   let returnStmt = String.concat "" ["\treturn ( ";  getImplCastString clsName ; " ) x;"] in
      Queue.add returnStmt (Pgmvalues.get_c_code packContext)
  

let codegen_implement_method clsName nl il packContext = 
    
    
    (* get name of class that is one implements, could be itself  *)   
    let implementsClsName = 
    try 
      match (List.hd il) with
      | Ast.TypClsPtr(Ast.Identifier(id)) -> id
    with 
    Failure desc -> clsName
    in 
    
    begin
    let cAlloc = String.concat "" 
         [ 
         "\n\t//allocate memory for new segment, set segment type\n";
         "\t"; (getImplCastString clsName);" x = ("; getImplCastString clsName ;") malloc(sizeof(" ; 
              clsName ; "_Impl));\n" ;
         "\tObject_Interface* bottom = findBottom(object_arg,  "; (get_method_table_name clsName) ; ");\n" ;
         "\tObject_Interface* root = bottom->Object_cycle;\n";
         "\tx->methods = "; (get_method_table_name clsName) ;";\n\n"
              ] in
    Queue.add cAlloc (Pgmvalues.get_c_code packContext);     

  
    let cSetSegAttr = String.concat "" 
         [ "\tObject_Interface* r = object_arg;\n";
           "\t// if found, update segment's cycle, bottom, and implements attribute\n";
           "\tif (r != root) {\n";
           "\t\tx->Object_cycle = r->Object_cycle;\n";
           "\t\tx->"; implementsClsName ; "_bottom = ( ("; getImplCastString implementsClsName; " ) r)->"; implementsClsName ; "_bottom;\n";
           "\t}"
         ] in
    Queue.add cSetSegAttr (Pgmvalues.get_c_code packContext);
 
    let cChgSegUpPointer = String.concat "" 
         [ "\n";
           "\t// if this is not the bottom of the chain, \n";
           "\t// update up pointer of next one in the extends chain\n";
           "\tdo {\n";
           "\t\tr = r->Object_cycle;\n";
           "\t}\n";
           "\twhile ((r != object_arg) && "; 
           "(( ( (" ; clsName ; "_Methods*) (r->methods))->extends) == "; get_method_table_name implementsClsName ; ") ); \n";
           "\tif (r != object_arg) {\n";
          (* "\t\t( ("; getImplCastString implementsClsName; ") r)->"; implementsClsName ; "_up = x;\n"; *)
           "\t\tx->"; implementsClsName ; "_bottom =";
           "   ( (" ;getImplCastString implementsClsName ; ")r)->"; implementsClsName ; "_bottom;\n";
           "\t};\n\n";
           "\t// call convert routine with r and x has parameters ,\n";
           "\t// then finally dispose r\n";
           (* TODO: must eventually uncomment, constructor method call must incorporate signature *)
           (* have to pass this in from the calling method  *)
           (* "\t"; clsName ;"_construct( ("; getImplCastString clsName ;") x, ("; (getImplCastString implementsClsName); ") r);\n"; *)
           "\tfree(r);";
         ] in
    Queue.add cChgSegUpPointer (Pgmvalues.get_c_code packContext);

    let returnStmt = String.concat "" ["\treturn ( ";  getImplCastString clsName ; " ) x;"] in
      Queue.add returnStmt (Pgmvalues.get_c_code packContext)
 
    end


let codegen_new_method clsName packContext = 

begin
  let cNewList = String.concat ""
            [ 
            "// create new object of type "; clsName ; "\n";
            "// allocate root object segment,  call extend to add "; clsName; " segment\n";
            getImplCastString clsName ; " " ; clsName ; "_new() {\n";
            "\tObject_Interface* o = Object_new();\n";
            "\treturn  (" ; getImplCastString clsName ; ") "; clsName ; "_extend(o);\n";
            (*"\treturn (" ; clsName ; "_Interface castObject(o, " ; (get_method_table_name clsName) ; ");\n"; *)
            
            "}\n";
            ] in
  Queue.add cNewList (Pgmvalues.get_c_code packContext)
end



(** implementation of the has() function  not quite finished *) 
let codegen_has_method id packContext = 

    let cLocalVarStr = String.concat ""
    		[ "\tObject_Interface* r = object_arg;\n";
    		  "\tdo {";
    		] in
    Queue.add cLocalVarStr (Pgmvalues.get_c_code packContext);

    begin      
    (* for each of these grab any class that implements them *)
    let il = Typesymtbl.implements id packContext  in
   
    match il with
    | [] -> 
    		begin
    		   let cSetSegAttr = String.concat "" 
               [ "\t\tif (r->methods==";
                 (get_method_table_name id);
                 ") return true;";
               ] in
           
               Queue.add cSetSegAttr (Pgmvalues.get_c_code packContext);
 			end
    | _ ->   
    		begin
    		  let cSetSegAttr = String.concat "" 
               [ "\t// inspect each object segment type\n";
                 "\tdo {"
               ] in
           
               Queue.add cSetSegAttr (Pgmvalues.get_c_code packContext);
               
               
             let clauseStr = (String.concat "" ["\n\t\t\tp->methods==";get_method_table_name (List.hd il)]) in

             let t = List.fold_right 
                    (fun a b -> String.concat "" ["\n\t\t\tp->methods==";get_method_table_name a;
                    " ||"; b]  ) 
                     (List.tl il) clauseStr in
                    

             let ifStmt = String.concat "" 
               [ "\t\tif (\n";
                 "\t\t"; t  ;"\n\t  )\n\t\t";
                 "return true;\n"
               ] in
               
             Queue.add ifStmt (Pgmvalues.get_c_code packContext)
    		 end;
    end; 
   let cFinalStr = String.concat "" 
         [ 
           "\t\tr = r->Object_cycle;\n " ;
           "\t} while (r != object_arg);\n";
           "\treturn false;"
         ] in
           
  Queue.add cFinalStr (Pgmvalues.get_c_code packContext)     

(** method to take each formal and add it to the cg_table.  This is
        used later while generating procedure code to deterimine where an 
        identifier is defined
        
        @param fl                       list of formal AST elements
        @param cg_symtbl        code generation symtbl      
*) 
let codegen_add_formals_to_cg_symtbl fl cg_symtbl = 
    List.iter (fun a ->
                            match a with
                            | (Ast.Identifier(id), typ) -> SymTbl.sym_set_symtbl_val cg_symtbl id (SymTbl.STFormalCG(typ))
                        ) fl

(** method generate code for method passed in 
    @param meth         method AST object 
    @param addthis      true if this pointer should be added to the method 
    @param clsType      type of class, ie structure containing all attributes of the class 
    @param clsName      name of the class 
    @param packContext  tuple containing all package related objects needed for code generation *)
let codegen_procedure_code meth addthis clsName packContext = 

  let cgSymtbl = Pgmvalues.get_codegensymtbl packContext in
  let tySymtbl = Pgmvalues.get_typsymtbl packContext in
  match meth with
  | Ast.MemMeth (Ast.Identifier(id), fl, ty, cd, i) -> 

     (* add class level to symbol table *)
     (* must be added - new level for scoping.  Leave out for now *)
     (* SymTbl.sym_add_level_to_symtbl symtbl; ytty *)

     (* add formals to the codesymbol table *)
     codegen_add_formals_to_cg_symtbl fl cgSymtbl;

     (* emit C code for function declaration *)
      begin
      match addthis with
      | true ->
          (* emit method prototype  *)
          let cLine = String.concat "" [ (getCstringType ty); " " ; clsName; "_" ; id ;
                                         " ( " ; (getImplCastString clsName) ; " self,"; 
                                         (getCstringFormalList fl) ; ")" ; "{" 
                                       ] in
          Queue.add cLine (Pgmvalues.get_c_code packContext);

          begin
            let thisInfo = SymTbl.sym_get_symtbl_val tySymtbl clsName in
            match thisInfo with
            | SymTbl.STClsTyp(Ast.TypClsPtr(Ast.Identifier(clsTypeName)), constL, attrL, methL, needsL, implL) ->
                print_endline (String.concat "" [ "codegen_procedure_code: code for " ; clsName ; "::" ; id ]);  
                let objInfo = (clsTypeName, []) in
                codegen_Compound objInfo cd packContext
            | _ -> raise (Error "codegen_procedure_code: can't find class info") 
          end
      | false -> 
            let cLine = String.concat " " [ (getCstringType ty) ; id ; 
                                            "("; (getCstringFormalList fl) ; ")" ; "{" 
                                          ] in
            Queue.add cLine (Pgmvalues.get_c_code packContext);
            let objInfo = (clsName, []) in
              codegen_Compound objInfo cd packContext
      end;
      
      
      
      (* if constructor, add return statement return object pointer *)
      match i with 
      | false -> Queue.add "}" (Pgmvalues.get_c_code packContext)
      | true ->Queue.add "\treturn self;\n}" (Pgmvalues.get_c_code packContext)
        (* sym_remove_level_from_symtbl symtbl; *)           


(* ************************** *)



(** utility function which takes in an AST class member object as input and 
    returns true if it is a class method, false otherwise *)
let is_method = function
  | Ast.MemMeth(id, fl, ty,comp, i) -> true
  | _ ->  false


(** Given a AST member list, this utility function returns a list with only members which are 
    methods *)
let codegen_get_methods_from_member_list memList = 
  match memList with
  | Ast.MemList(ml) ->  List.filter is_method ml
  
(** Given a class name, return the name of the static pointer to methods
    defined for that class *)                   

(** Given an AST class object, it returns an ordered list of its attributes 
    In particular, the list will begin with attributes from its "highest" parent
    and traverse until  it reaches the class passed in. This language assumes 
    single inheritance so the ordering is unamibigous *)
let rec get_attribute_list clsTypEntry attrList packContext = 
  match clsTypEntry with
  | SymTbl.STClsTyp(Ast.TypClsPtr(Ast.Identifier(id)), cl, al, tml, nl, il) ->
      
      (* go find supertype *)
      match il with 
      | [] -> 
          let logStr = String.concat "" ["************************get_attribute_list: no superclass found for "; id ; " class"] in
            print_endline logStr; attrList
      | h::t -> 
          (* if supertype exists, collect the attributes *)
          match h with 
          | Ast.TypClsPtr(Ast.Identifier(iId)) -> 
              let logStr = String.concat "" ["**********************get_attribute_list: superclass found - "; iId] in
                print_endline logStr;               
              let typesymtbl = Pgmvalues.get_typsymtbl packContext in
              let e = SymTbl.sym_get_symtbl_val typesymtbl iId in
                 (get_attribute_list e attrList packContext) @ attrList    
               
  | _ -> let logStr = String.concat "" ["**************************get_attribute_list: no superclass found 1"] in
                print_endline logStr;   attrList
                   


   
let codegen_uniCls ast packContext =

  match ast with
    | Ast.Cls(Ast.Identifier(id), i, ml, sln, sli, inh) -> 
            
       let logStr = String.concat "" ["codegen_Cls: class found - "; id] in
        print_endline logStr;
      
      
     (* add class level to symbol table *)
     let typesymtbl = Pgmvalues.get_typsymtbl packContext in
        
     (* look type data in type symbol table *)
     let clsEntry = SymTbl.sym_get_symtbl_val typesymtbl id in
      
      (* get list of methods stored in the ast *)			
      let methodList = codegen_get_methods_from_member_list ml in	

        (* create structure that holds pointers to member functions *)
        let memStructLine = String.concat "" 
                   [ "\n";
                   "// structure holds pointers to methods for class ";id;"\n";
                   "// along with type of class that it extends, implements or inherits\n";
                   "// also used identify type of segment\n";
                   "typedef struct "; id ; "_Methods {";
                   "\n\tvoid* extends;\n\tvoid* implements;\n\tvoid* inherits;"
                   ] in         
        Queue.add memStructLine (Pgmvalues.get_c_code packContext);
                        
        List.iter (fun m -> add_method_pointer_struct id m packContext; 
        print_string "codegen_Cls: method pointer structure "; print_endline id) 
        methodList;
                
        let endMemStructLine = String.concat "" ["} "; id ; "_Methods;\n"] in         
            Queue.add endMemStructLine (Pgmvalues.get_c_code packContext);

     (* grab all the constants and attributes and use them *)
     match clsEntry with
     | SymTbl.STClsTyp(Ast.TypClsPtr(Ast.Identifier(id)), cl, al, tml, nl, il) ->            
        
		 (* string common to both interface and implementation structs *)
         (* TODO: modify the way self pointers are compiled  *)
               
         let objLine = String.concat "" 
              ["\t "; id ; "_Methods* methods;\n";       
               "\t "; "Object_Interface* Object_cycle;\t\t // *possibly* a pointer to a needed object\n"
              ] in
         let clsLine = String.concat "" 
              ["\t struct "; id ; "_Interface* "; id ;"_bottom;\t\t\t // pointer to latest mixed methods\n";         
              ] in         
 
      
        (* find the class this one implements, could be itself  *)
         let implementsClsName = 
            
            match il with
            | [] -> id
            | [Ast.TypClsPtr(Ast.Identifier(iId))] -> iId
            | _ -> id
            in 
             
         (* find needs list of the implemented class *)   
         let implCls = SymTbl.sym_get_symtbl_val typesymtbl (* (Pgmvalues.get_codegensymtbl packContext) *) implementsClsName in
         let implNeedL = 
            match implCls with
            | SymTbl.STClsTyp(Ast.TypClsPtr(Ast.Identifier(clsTypeName)), constL, attrL, methL, needsL, implL) -> needsL
            | _ -> il
         in
           
        (* use it to generate extension list *)
		let extl = Typesymtbl.extension_chain implementsClsName packContext in
        
		(* create implementation structure *)
        let cInterLine = String.concat "" ["\ntypedef struct "; id ; "_Impl {"] in         
        Queue.add cInterLine (Pgmvalues.get_c_code packContext);
        
        
        Queue.add objLine (Pgmvalues.get_c_code packContext);
        
        List.iter (fun a -> 
               
               let cLine = String.concat "" ["\t"; getImplCastString a ; "  " ; a ; "_bottom;\n";  
                                             "\t"; getImplCastString a ; "  " ; a ; "_up;"
                                            ] in
                         Queue.add cLine (Pgmvalues.get_c_code packContext);
                   ) extl;
                  
                  
              (* find class it implements, if that exists *)
              let ptName = 
                match il with
                | [] -> id
                | [Ast.TypClsPtr(Ast.Identifier(i))] -> i
                | [Ast.TypClsPtr(Ast.Identifier(i)); w] -> i
                in
              
               let clsImplLine = String.concat "" 
                  ["\t struct ";  getImplCastString ptName ; " "; ptName ;"_bottom;\t\t\t // pointer to latest mixed methods\n";         
                  ] in   
        
               Queue.add clsImplLine (Pgmvalues.get_c_code packContext);
       
         Queue.add "" (Pgmvalues.get_c_code packContext);
        
        
        (* emit code for attributes of all superclasses *)
        
        let superClsAttrL = get_attribute_list implCls [] packContext in
        
        
        List.iter (fun a -> 
            match a with 
            | SymTbl.STClsAttr(aName, aIdx, atyp, clsName) -> 
                 let attrLine = String.concat "" ["\t"; (getCstringTypeDeclaration atyp); " "; aName ; ";"] in         
                 Queue.add attrLine (Pgmvalues.get_c_code packContext)
        ) superClsAttrL;
        

        (* emit code for private local attributes *)
        List.iter (fun a -> 
            match a with 
            | SymTbl.STClsAttr(aName, aIdx, atyp, clsName) -> 
                 let attrLine = String.concat "" ["\t"; (getCstringTypeDeclaration atyp); " "; aName ; ";"] in         
                 Queue.add attrLine (Pgmvalues.get_c_code packContext)
        ) al;


 (*     
     match clsTypEntry with
    | Ast.Cls(Ast.Identifier(id), i, ml, sln, sli) -> 
    
         get attributes from the type symbol table  
        let typesymtbl = Pgmvalues.get_typsymtbl packContext in
        let clsEntry = SymTbl.sym_get_symtbl_val typesymtbl id in
*)


        let endLine = String.concat "" ["} "; id ; "_Impl;\n"] in         
            Queue.add endLine (Pgmvalues.get_c_code packContext);


        (* generate code for the methods with the class  *) 
     		List.iter (fun m -> codegen_procedure_code m true id packContext; 
     		print_string "codegen_Cls: generate code for class "; print_endline id) 
     		methodList;
				
    
        (* generate class init() function *)
        let initLine = String.concat "" 
                        ["\nvoid " ; id ; "_init() { \n" ;
                         "\t"; (get_method_table_name id);" = ";
                         " malloc(sizeof(" ;
                                 id; "_Methods));"  ] in         
            Queue.add initLine (Pgmvalues.get_c_code packContext);

        List.iter (fun m -> codegen_method_pointer_init id m packContext; 
        print_string "codegen_Cls: method init procedure "; print_endline id) 
        methodList;

                        
        Queue.add "}\n" (Pgmvalues.get_c_code packContext);  

        (* generate class update_bottom_pointers *)
        let updLine = (String.concat "" 
                        ["\nvoid " ; id ; "_bottomdateSelfPointers(Object_Interface* p, void* e) {"]) in         
        Queue.add updLine (Pgmvalues.get_c_code packContext);


        codegen_bottomdate_bottom_pointer_method implementsClsName implNeedL packContext;

        Queue.add "}\n" (Pgmvalues.get_c_code packContext);      


        (* generate class extend() function *) 
        let extLine = String.concat "" 
                        ["\n"; getImplCastString id ;" " ;  id ; "_extend(Object_Interface* object_arg";
												(*	(codegen_extend_arg_list nl); *)
                         ") {"  ] in         
           Queue.add extLine (Pgmvalues.get_c_code packContext);

        codegen_extend_method id nl il packContext; 
        print_string "codegen_Cls: method extend procedure "; print_endline id;
        methodList;
               
        Queue.add "}\n" (Pgmvalues.get_c_code packContext);
        
       (* generate class implement () function - replaces one implementation with another *) 
        let implLine = String.concat "" 
                        ["\n"; getImplCastString id ;" " ;  id ; "_implement(Object_Interface* object_arg";
                         ") {"  ] in         
           Queue.add implLine (Pgmvalues.get_c_code packContext);
         
        codegen_implement_method id nl il packContext; 
        print_string "codegen_Cls: method implement procedure "; print_endline id;
        methodList;
               
        Queue.add "}\n" (Pgmvalues.get_c_code packContext);

		(* generate class new() function *) 
		if (List.length nl == 0) then
		  codegen_new_method id packContext;
		  
		  
		(* generate class has() function *)
        let hasLine = String.concat "" 
                        ["\nint " ;  id ; "_has(Object_Interface* object_arg";
                         ") {"  ] in         
           Queue.add hasLine (Pgmvalues.get_c_code packContext);

        codegen_has_method id packContext; 
        print_string "codegen_Cls: method has procedure "; print_endline id;
        Queue.add "}\n" (Pgmvalues.get_c_code packContext);  
        

        ()


(** Entry point and the only routine that should be called from outside of the module.
    It accepts an abstract syntax tree structure for a particular package and generates
    C code for it.  It calls an initialization routine for the global variable,triggers
    the code generation process and returns global variable structure (which included access
    to the generated code) to the caller.
    
    @param ast      abstract syntax tree to be processed *)
let codegen_doc ast packContext = 
	
  match ast with
  | Ast.Package (Ast.Identifier(i), Ast.ClsList(ul) ) ->
				print_endline "codegen_doc: begin executing package code generation process...";
				ObjectC.object_static_code packContext;
        List.map (fun a ->
                   match a with
                   | Ast.Cls(Ast.Identifier(id), i, ml, nl, il, h) -> 
                      (* declare static pointer for class methods *)
                      let methPtLine = String.concat "" ["\nvoid* "; (get_method_table_name id); ";"] in
                      Queue.add methPtLine (Pgmvalues.get_c_code packContext) ) ul; 
                          
				List.iter (fun u -> codegen_uniCls u packContext) ul;
				packContext
  | Ast.Program (Ast.Identifier(i), Ast.ClsList(ul), cs ) ->
       print_endline "codegen_doc: begin executing program code generation process...";
			 print_endline "codegen_doc: emit static object code...";                
       ObjectC.object_static_code packContext; 
       List.map (fun a ->
                   match a with
                   | Ast.Cls(Ast.Identifier(id), i, ml, nl, il, h) -> 
                      (* declare static pointer for class methods *)
                      let methPtLine = String.concat "" ["\nvoid* "; (get_method_table_name id); ";"] in
                      Queue.add methPtLine (Pgmvalues.get_c_code packContext) ) ul; 

			 List.iter (fun u -> print_endline "codegen_doc: emit class code"; codegen_uniCls u packContext) ul;

			 print_endline "codegen_doc: emit object class code...";
       Queue.add "int main() {" (Pgmvalues.get_c_code packContext);
			 (* initialize all class information *)
			 Queue.add "\tObject_init();" (Pgmvalues.get_c_code packContext);   
             List.map (fun a ->
                            match a with
                            | Ast.Cls(Ast.Identifier(id), i, ml, nl, il, h) -> 
															 let initLine = String.concat "" ["\t" ; id ; "_init();" ] in
															    Queue.add initLine (Pgmvalues.get_c_code packContext) ) ul;      

       print_endline "codegen_doc: generate main function...";   
			 codegen_Compound ("main",[]) cs packContext;
			 Queue.add "}\n" (Pgmvalues.get_c_code packContext);
       packContext				

end;;