open Ast

module AstPrint = 
struct


let plf = print_endline "@@"

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
                     (*  List.iter (fun a -> print_char ' '; print_int a) il *)



and print_ast_uniOper t = 
    match t with
    | Ast.UOpMIN -> print_string "-"
    | Ast.UOpNOT -> print_string "not "
    | Ast.UOpPLS -> print_string "+"    


and print_ast_binOper t = 
    match t with
    | Ast.MOpMT -> print_string "*"
    | Ast.MOpDV -> print_string "\\"
    | Ast.MOpDIV -> print_string "div "
    | Ast.MOpMOD -> print_string "mod "
    | Ast.AOpPL -> print_string "+"
    | Ast.AOpSB -> print_string "-"


(*and print_ast_relOper t =         match t with *)
        | Ast.ROpLT -> print_string "<"
    | Ast.ROpGT -> print_string ">"
    | Ast.ROpLTE-> print_string "<="
    | Ast.ROpGTE -> print_string ">="
    | Ast.ROpE -> print_string "=" 
    | Ast.ROpNE -> print_string "!=" 

(* and print_ast_boolOper t = match t with *)

        | Ast.BoolAnd -> print_string "and "
    | Ast.BoolOr -> print_string "or "


and print_ast_expression t =
      match t with    
    | Ast.ExpUnary(u,e) -> print_ast_uniOper u; print_ast_expression e
    | Ast.ExpBinary(b,e1,e2) -> print_string "binary ("; print_ast_binOper b; print_ast_expression e1; 
                                                          print_ast_expression e2; print_char ')'
    | Ast.ExpNumber(i) -> print_string "number ("; print_int i; print_char ')'
    | Ast.ExpDes(d)-> print_string "des ("; print_ast_designator d ; print_char ')'
    | Ast.ExpNil -> print_string "nil "
        | Ast.ExpTrue -> print_string "true "
        | Ast.ExpFalse -> print_string "false "
    | Ast.ExpFuncCall(d) -> print_ast_designator d
    | Ast.ExpNew((t, e, el), il) -> print_string "new ";  print_ast_typ t;
                                                                    print_string "( ";
                                                                    List.iter (fun a -> print_ast_expression a; print_char ',') el;
                                                                    print_string ") - ";
                                                                    List.iter (fun i ->
                                                                                        match i with 
                                                                                        | Ast.Identifier(id) -> print_string id) il       
        | Ast.ExpAs(Ast.ExpDes(d), Ast.Identifier(i)) -> print_string "as "; print_ast_designator d; print_string i
    | Ast.ExpHas(Ast.ExpDes(d), Ast.Identifier(i)) ->  print_string "has "; print_ast_designator d; print_string i
        | _ -> plf; ()
        

and print_ast_design_help d =
    match d with
    | Ast.DNID(Ast.Identifier(id)) -> print_string id;
  | Ast.DNExp(el) -> print_string "[";  List.iter( fun e -> print_ast_expression e; print_char ',') el;
                                        print_string "]"  

and print_ast_dotted_name dn = 
    match dn with 
    | Ast.DottedName(dl) ->  List.iter (fun i -> print_ast_design_help i;  print_string ". ") dl


and print_ast_designator d =   
        match d with
        | Ast.Designator(dotName, el) ->  print_string "* dn ";
                                          print_ast_dotted_name dotName;
                                                                            print_string " -  act ("; 
                                                                         List.iter (fun a -> print_ast_expression a; print_char ',') el;
                                                                         print_char ')'; 
                                                                         print_char '*'


and print_ast_identifer_list il =    print_char ')';
        List.iter (fun i -> match i with 
                    | Ast.Identifier(id) -> print_string id; print_char ' ') il
                                                            
and print_ast_variable v =
     match v with
    | Ast.Variable(il, typ, vType) -> print_string "var ";
        List.iter (fun i ->
                  match i with 
                  | Ast.Identifier(id) -> print_string id; print_char '.') il

and print_ast_locals lvl =
     match lvl with
        | Ast.Locals(lvl) ->
              List.iter (fun lv ->  
                    match lv with 
                                | Ast.VCVar(Ast.Variable(il, typ, vType)) -> 
                                            print_string "local ";
                                    List.iter (fun i ->
                                match i with 
                                | Ast.Identifier(id) -> print_string id; print_char '.') il 
                            ) lvl                                                       

and print_ast_statement st =    
    match st with 
    | Ast.StmtDesign (Ast.ExpDes(d), e) -> print_ast_designator d; print_string ":=";
                                                print_ast_expression e; plf

    | Ast.StmtRaise  -> print_string "raise "; plf
    | Ast.StmtExtend (Ast.ExpDes(d), il) -> print_string "extend "; print_ast_designator d;
                                                                            print_char ' '; print_ast_identifer_list il;
                                                                            plf
  | Ast.StmtRemove (Ast.ExpDes(d), il) -> print_string "remove "; print_ast_designator d;
                                   print_char ' '; print_ast_identifer_list il;
                                   plf
  | Ast.StmtReplace (Ast.ExpDes(d), Ast.Identifier(id1), Ast.Identifier(id2)) -> print_string "replace "; 
                                     print_ast_designator d;
                                   print_char ' '; print_string id1;
                                                                     print_char ' '; print_string id2;
                                   plf
(*                                                              
  | Ast.StmtIf(e, c1, c2) -> print_string "if  "; print_ast_expression e; plf;
                                                 print_ast_compound c1;  plf;
                         print_ast_compound c2;  plf                                                    

  | Ast.StmtWhile (e, c) -> 
                                        print_string "while  "; print_ast_expression e; plf;

                                                    print_ast_compound c;  plf
*)

  | Ast.StmtLocals(lvl, c) ->
                print_ast_locals lvl;
        print_ast_compound c; plf               
  (*                                        
  | Ast.StmtVar (v, c) ->
                                        print_ast_variable v; plf;
                                        print_ast_compound c; plf

  | Ast.StmtConst(Ast.Identifier(id), t, e, c) -> 
                                    print_ast_typ t; print_char ' '; print_string id;
                                    print_string " := "; print_ast_expression e; plf;
                                    print_ast_compound c
    *)                              
                                    
  | Ast.StmtRet(e) ->
                     print_string " return "; print_ast_expression e; plf 
                    
    | Ast.StmtProcCall(d) -> print_string " call "; print_ast_expression d; plf



and print_ast_compound c =
    match c with
    | Ast.Compound(sl) -> print_endline "cpd{"; 
                           List.iter (fun s ->
                           print_string "stmt$"; print_ast_statement s; print_endline "$") sl; 
                                                print_endline "}"

 

and print_ast_member m =
    match m with
    | Ast.MemConst (Ast.Identifier(id), t ,v ,e) ->  
              print_string "const "; print_ast_typ t; print_char ' '; print_string id;
          print_string " := "; print_ast_expression e; plf
  | Ast.MemAttr(v) -> print_ast_variable v; plf;
  | Ast.MemMeth(Ast.Identifier(id), fl, t, c, init) -> 
                                     print_string "method "; print_string id; print_char '[';
                                   List.iter (fun i ->
                                        match i with 
                                        | (Ast.Identifier(fId),ft) -> 
                                                                                       print_string fId; print_string " : "; print_ast_typ ft;
                                                                                         print_string " ," ) fl;
                                                                 print_string "] returns "; print_ast_typ t;
                                                                 print_char '\n';
                                                                 print_ast_compound c
                              
  | Ast.MemInit(fl, c) -> print_string "init ";print_char '[';
                                   List.iter (fun i ->
                                        match i with 
                                        | (Ast.Identifier(fId),ft) -> 
                                                                                    print_string fId; print_string " : "; print_ast_typ ft;
                                                                                   print_string " ," ) fl;
                                                                     print_string "]";                                   
                                   print_ast_compound c 
    

and print_ast_member_list ml =      
    match ml with 
    | Ast.MemList(l) ->  List.iter (fun i -> print_char '-'; print_ast_member i; print_char '\n'; plf) l


and print_ast_class c =
  match c with
    | Ast.Cls(Ast.Identifier(id), i, ml, nl, il, inh) -> 
         print_string "AST class "; print_string id; plf; 
         print_string " needs list "; List.iter( fun i -> print_string i; print_char ',') nl; 
         print_string " implements list "; List.iter( fun i -> print_string i; print_char ',') il;
         print_string " inherit "; inh;
         print_string "(\n"; print_ast_member_list ml; print_endline ")"

                
and print_ast_class_list cl =
    match cl with
    | Ast.ClsList(l) -> List.iter(fun c -> print_ast_class c; plf) l

        
and print_ast_package p =
    match p with
    | Ast.Package(Ast.Identifier(id), cl) ->  print_string "package "; print_endline id;
                                                print_endline "begin package"; plf;
                                                                                    print_ast_class_list cl;
                                                                                    print_endline "end package"
        | Ast.Program(Ast.Identifier(id), cl, cs) ->  print_string "program "; print_endline id;
                                                print_endline "begin program"; plf;
                                          print_ast_class_list cl;
                                          print_endline "end program";
                                                                                    print_endline "main method;";
                                                                                    print_ast_compound cs 

                                                                                                                                    
end;;