open Genlex;;
open Token
open Ast
module Parser = 
struct

(** Parser
    This module is used to parse a stream of tokens generate an abstract syntax tree.
    The parse_document function is called to begin the process.  As tokens are matched, 
    associated parsing functions are called to create AST nodes and recursively call
    parsing functions to create AST subnodes. 
    
    The mx language being parsed is described in the comments and determines the 
    functions' pattern matching expressions.
*)

(* Utility Functions *)


(** binop_precedence - This holds the precedence for each binary operator that is defined *)
let binop_precedence:(Token.tokentype, int) Hashtbl.t = Hashtbl.create 30

(** precedence - Get the precedence of the pending binary operator token. *)
let get_precedence tok = try Hashtbl.find binop_precedence tok 
                                                with Not_found -> -1


let get_binary_operator tok =
    match tok with
  |  Token.LSS ->  Ast.ROpLT
  |  Token.GTR ->  Ast.ROpGT
  |  Token.LEQ ->  Ast.ROpLTE
  |  Token.GEQ ->  Ast.ROpGTE
  |  Token.EQL ->  Ast.ROpE
  |  Token.NEQ ->  Ast.ROpNE
  |  Token.PLUS ->  Ast.AOpPL
  |  Token.MINUS ->  Ast.AOpSB
  |  Token.TIMES ->  Ast.MOpMT
  |  Token.DIVIDE ->  Ast.MOpDV
  |  Token.DIV  ->  Ast.MOpDIV
  |  Token.MOD ->  Ast.MOpMOD
  |  Token.AND -> Ast.BoolAnd
  |  Token.OR -> Ast.BoolOr
  | _ -> raise (Stream.Error "get_binary_operator: operator not found")




(** This function is used to obtain a string from the variant structure tokenauxdata 

    @param string token
    @return string related with token 
    @raise Error if input is a non-string token *) 
let get_string_from_auxdata a =  
    match a with 
    Token.ST a -> a
    | _ -> raise (Stream.Error "get_string_from_auxdata: trying to retrieve a non-string value")

(** This function is used to obtain a integer from the variant structure tokenauxdata
    @param string token
    @return string related with token 
    @raise Error if input is a non-string token *) 
let get_int_from_auxdata a =  
    match a with 
    Token.IT a -> a
    | _ -> raise (Stream.Error "get_int_from_auxdata: trying to retrieve a non-integer value")

(** creates an AST subtree required for an expression found in the token stream. The 
    precedence is unary operators, primary elements, conjunction binary operators
    additive binary operators, multiplicative binary operators.  
 
    @param stream of token
    @return string related with token 
    @raise Error if input is a non-string token *) 




(* ----------------------------------------------------------------------------------- *)  

let rec parse_other_integers acc = parser
  | [< 'Token.Token ( Token.COMMA, (st,ln, col), al );
              'Token.Token ( Token.INT, (st1,ln1, col1), al1 );
            il=parse_other_integers ((get_int_from_auxdata (List.hd al1))::acc) >] -> il
    | [< >] -> []


and parse_integer_list = parser
 | [<  'Token.Token ( Token.INT, (st,ln, col), al); il=parse_other_integers [] >] -> 
        print_endline "parse: parse_integer_list";
        List.rev ((get_int_from_auxdata (List.hd al))::il )
 | [<  >] -> []


 (*----------------------------------------------------------------------------------- *) 



(* idList        ::= identifier {"," identifier}  *)


  
and make_id_list lst = 
    List.map (fun a -> Ast.Identifier a) lst 

and print_id_list arr = 
    List.map (fun a -> print_string "parse.print_id_list - identifier found: "; 
            print_endline a) arr
                

and parse_other_identifiers acc = parser
  | [<  'Token.Token ( Token.COMMA, (st,ln, col), al ); 
  'Token.Token ( Token.IDENTIFIER, (st1,ln1, col1), al1 ); 
  i=parse_other_identifiers ((get_string_from_auxdata(List.hd al1) )::acc) >] ->  i
  | [< >] ->  acc

and parse_idlist = parser
 | [<  'Token.Token ( Token.IDENTIFIER, (st,ln, col), al ); idl=parse_other_identifiers [] >] -> 
        print_endline "parse_idlist: parse_idlist";
        let l = (get_string_from_auxdata (List.hd al))::( List.rev (idl)) in
        ignore(print_id_list l);
        l
(* | [<  >] -> [] *)



(**parse expression here *)

(** primary      ::= integer | nil | true | false | designator 
    | "new" identifier [ "["expression "]" ] [ actuals ] "with" {identifier [ idlist ]} 
    | designator "as" identifier | designator "has" identifier  *)
let rec parse_primary = parser
 | [< 'Token.Token ( Token.INT, (st,ln, col), al ) >] -> 
                print_string "parse_primary: integer found - "; 
        let it = get_int_from_auxdata (List.hd al) in
        print_int it; get_precedence Token.INT; print_endline "";
        Ast.ExpNumber(it)
 | [< 'Token.Token ( Token.NIL, (st,ln, col), al ) >] -> 
        print_endline "parse_primary: nil found"; 
        Ast.ExpNil
 | [< 'Token.Token ( Token.TRUE, (st,ln, col), al ) >] -> 
        print_endline "parse_primary: true found"; 
        Ast.ExpTrue
 | [< 'Token.Token ( Token.FALSE, (st,ln, col), al ) >] -> 
        print_endline "parse_primary: false found";
        Ast.ExpFalse   
 | [< 'Token.Token ( Token.LPAREN, (st,ln, col), al ); 
        e=parse_expression;
      'Token.Token ( Token.RPAREN, (st,ln, col), al ) >] -> 
        print_endline "parse_primary: bracketed expression found";
        Ast.ExpBracketExp(e)             
 | [< dn=parse_designator; stream >] ->
        begin parser
        | [< 'Token.Token ( Token.AS, (st,ln, col), al );
             'Token.Token ( Token.IDENTIFIER, (st1,ln1, col1), al1) >] ->
             print_endline "parse_primary: as statement found";
             Ast.ExpAs(dn, Ast.Identifier(get_string_from_auxdata(List.hd al1))) 
        | [< 'Token.Token ( Token.HAS, (st,ln, col), al );
             'Token.Token ( Token.IDENTIFIER, (st1,ln1, col1), al1) >] ->
             print_endline "parse_primary: has statement found";
             Ast.ExpHas(dn, Ast.Identifier(get_string_from_auxdata(List.hd al1)))                                                                                 
        | [< >] -> 
             print_endline "parse_primary: designator";
             dn       
        end stream
        

 | [<'Token.Token ( Token.NEW, (st,ln, col), al ); 
     t=parse_type; stream >] ->
         begin parser
         | [<'Token.Token ( Token.LBRAK, (st,ln , col ), al  );
         e=parse_expression;
         'Token.Token ( Token.RBRAK, (st1,ln1, col1), al1 ) >] -> Ast.ExpNew((t,e,[]), [])
         | [< actl=parse_actuals >] -> Ast.ExpNew((t,Ast.ExpNone,actl), []) 
         | [< >] -> print_endline "parse_primary: new expression"; Ast.ExpNew((t,Ast.ExpNone,[]), [])
         end stream     


            (* remove supporting with clause for now *)
            (*  
                        print_endline "parse_primary: new statement found";
                        begin parser
                        | [< 'Token.Token ( Token.WITH, (st,ln, col), al ); il=parse_idlist >] -> 
                 print_endline "parse_primary: new object statement with actuals and mixins found";
                 Ast.ExpNew( t, ex ,actl), make_id_list il)
            | [<  >] -> 
                 print_endline "parse_primary: new object statement with actuals";
                 Ast.ExpNew( t,ex,actl), [])
            end stream
            *)

(*              
 | [<  dn=parse_dotted_name; stream >] ->
                begin parser
                | [< a=parse_actuals >] ->
                            print_endline "parse_primary: call found";
                            Ast.PriExpCall(Ast.Designator(dn,a))
                | [< >] ->  
                    print_endline "parse_primary: call found";
                    Ast.PriExpDot(dn)
                end stream
*)

            

(* binoprhs  ::= ('+' primary)*    *)
and parse_bin_rhs expr_prec lhs stream =
  match Stream.peek stream with
  (* If this is a binop, find its precedence. *)
  | Some (Token.Token (c, (tokStr, ln, col), tokAuxDataList)) when Hashtbl.mem binop_precedence c ->
      let token_prec = get_precedence c in
            print_string "parse_bin_rhs: Token "; print_string tokStr;
            print_string " value: "; print_int token_prec;
      print_endline " found in precedence table";

      (* If this is a binop that binds at least as tightly as the current binop,
       * consume it, otherwise we are done. *)
      if token_prec < expr_prec then lhs else begin
        (* Eat the binop. *)
        Stream.junk stream;

        (* Parse the primary expression after the binary operator. *)
        let rhs = parse_primary stream in

        (* Okay, we know this is a binop. *)
        let rhs =
          match Stream.peek stream with
          | Some (Token.Token (c2, tokData, tokAuxDataList)) ->
              (* If BinOp binds less tightly with rhs than the operator after
               * rhs, let the pending operator take rhs as its lhs. *)
              let next_prec = get_precedence c2 in
              if token_prec < next_prec
              then parse_bin_rhs (token_prec + 1) rhs stream
              else rhs
          | _ -> rhs
        in

        (* Merge lhs/rhs. *)
                let bo = get_binary_operator c in
        let lhs = Ast.ExpBinary (bo, lhs, rhs) in
                print_endline "parse_bin_rhs: Merge lhs/rhs";
        parse_bin_rhs expr_prec lhs stream
      end
  | _ -> print_endline "parse_bin_rhs: lhs only"; lhs



and parse_other_expressions acc = parser
  | [<  'Token.Token ( Token.COMMA, (st,ln, col), al ); 
            e=parse_expression; 
            el=parse_other_expressions (e::acc) >] ->  el
  | [< >] ->  acc

and parse_expression_list = parser
 | [<  e=parse_expression; el=parse_other_expressions [] >] -> 
        print_endline "parse_expression_list: parse_expression_list";
        e::( List.rev (el))
                (*
                let l = e::( List.rev (el)) in
        ignore(print_id_list l);
                *)
                
(* expression  ::= primary binoprhs *)
and parse_expression = parser
  | [< lhs=parse_primary; stream >] -> parse_bin_rhs 0 lhs stream;


(* --------------------------------------------------------------------------------- *)

and parse_optional_array_member = parser
 | [< 'Token.Token ( Token.ARRAY, (st,ln, col), al );
            (*
            'Token.Token ( Token.LBRAK, (st,ln, col), al )
            il=parse_expression_list;
            'Token.Token ( Token.RBRAK, (st,ln, col), al );
            *)
            'Token.Token ( Token.OF, (st,ln, col), al );
>] -> print_endline "parse: parse_optional_array_member - optional array found";
            []

(*
 | [< >] -> print_endline "optional_array_member: got here"; Ast.VTypPrivate
*)

(* type::= {"array" [ integer {"," integer }] "of"} (identifier | "integer" | "boolean") *)
(* note in this implementation type is always preceded by a colon *)

and parse_type = parser
        | [< il=parse_optional_array_member; stream >] ->
              begin parser
            | [< 'Token.Token ( Token.INTEGER, (st,ln, col), al ) >] -> 
                    print_endline "parse_type: INTEGER array type found";
                    Ast.TypArr(il, Ast.TypInt)
            | [< 'Token.Token ( Token.BOOLEAN, (st,ln, col), al ) >] ->  
                    print_endline "parse_type: - BOOLEAN array type found";
                    Ast.TypArr(il, Ast.TypBool)
            | [< 'Token.Token ( Token.IDENTIFIER, (st1, ln1, col1), al1) >] ->  
                   print_endline "parse_type: - CLASS array type found";
                                  Ast.TypArr(il, Ast.TypClsPtr(Ast.Identifier(get_string_from_auxdata (List.hd al1)) ) )
                   (*Ast.TypArrClsPtr(Ast.Identifier(get_string_from_auxdata (List.hd al1)), il) *)
                end stream
                
    | [< 'Token.Token ( Token.INTEGER, (st,ln, col), al ) >] -> 
            print_endline "parse_type: - INTEGER type found";
            Ast.TypInt
    | [< 'Token.Token ( Token.BOOLEAN, (st,ln, col), al ) >] ->  
            print_endline "parse_type: - BOOLEAN type found";
            Ast.TypBool
    | [< 'Token.Token ( Token.IDENTIFIER, (st1, ln1, col1), al1) >] ->  
            print_endline "parse_type: - CLASS type found";
            Ast.TypClsPtr(Ast.Identifier(get_string_from_auxdata (List.hd al1)))

 



(* ----------------------------------------------------------------------------------- *)



(* formals        ::= ["(" idList ":" type {"," idList ":" type } ")"]  *)

and make_formal_list lst t = 
    List.map (   fun a -> (Ast.Identifier(a),t)    ) lst
    

and parse_other_formals acc = parser
 | [<  'Token.Token ( Token.COMMA, (st, ln, col), al ); 
 'Token.Token ( Token.IDENTIFIER, (st1, ln1, col1), al1); 
 args=parse_idlist;
 'Token.Token ( Token.COLON, (st,ln, col), al );  
 t=parse_type; 
 l=parse_other_formals (  
 ((make_formal_list args t)@[(Ast.Identifier(get_string_from_auxdata (List.hd al1)),t)])@acc  
 ) >] 
    -> 
        print_string "parse_other_formals: formal in list: "; 
        print_endline (get_string_from_auxdata (List.hd al1));
        l
 | [<  >] -> acc

and parse_formals = parser

 | [< 'Token.Token ( Token.LPAREN, (st, ln, col), al ); stream>] ->
      begin parser
      
            | [< args=parse_idlist; 'Token.Token ( Token.COLON, (st,ln, col), al ); t=parse_type; f=parse_other_formals []; 
      'Token.Token ( Token.RPAREN, (st2, ln2, col2), al2) >] ->
              print_string "parse_formals: - first formal in list: "; 
              List.rev ( (make_formal_list args t)@f ) 
      | [< 'Token.Token ( Token.RPAREN, (st2, ln2, col2), al2) >] -> []
      
      end stream


(* ----------------------------------------------------------------------------------- *)




(* actuals        ::=  ["("] expression { [","] expression } [")"]  *)

and parse_optional_lparen = parser
| [< 'Token.Token (Token.LPAREN, (id, ln, col), al)>] -> 0
(* | [< >] -> 1 *)

and parse_optional_rparen = parser
| [< 'Token.Token(Token.RPAREN, (id, ln, col), al) >] -> 0
(* | [< >] -> 1 *)

and parse_other_actuals acc = parser
 | [<  'Token.Token(Token.COMMA, (id, ln, col), al); e=parse_expression; 
    l=parse_other_actuals ( e::acc  ) >] -> l
 | [<  >] -> acc

and parse_actuals = parser
 | [< lp=parse_optional_lparen; stream >] ->

        begin parser
 (* TODO: fix to allow expressions eventually *) 
        | [< rp=parse_optional_rparen >] ->
                    print_endline "parse_actuals: empty actual list";
                    []
            | [< e=parse_expression;  al=parse_other_actuals [] ; rp=parse_optional_rparen >] ->
                     print_endline "parse_actuals: actuals list end"; 
                    List.rev (e::al)                    
        end stream 



(* ----------------------------------------------------------------------------------- *)


(** designator    ::= identifier { ["."] identifier | actuals }                       
 TODO: need to fix, actuals don't need period *)

                                
and parse_other_dotted_names acc = parser
  | [<  'Token.Token ( Token.PERIOD, (st,ln, col), al ); 
            'Token.Token ( Token.IDENTIFIER, (st1,ln1, col1), al1 ); 
            d=parse_other_dotted_names (                    
                (Ast.DNID(Ast.Identifier(get_string_from_auxdata(List.hd al1))))  
                ::acc) >] ->  d
     | [< 'Token.Token ( Token.LBRAK, (st,ln, col), al );
        il=parse_expression_list;
        'Token.Token ( Token.RBRAK, (st,ln, col), al );
        d=parse_other_dotted_names (                    
        (Ast.DNExp(il) )  
        ::acc                   
        ) >] ->  d
  | [< >] ->  acc


and parse_dotted_name = parser
 | [<  'Token.Token ( Token.IDENTIFIER, (st,ln, col), al ); dnl=parse_other_dotted_names [] >] -> 
        let l = Ast.DNID(Ast.Identifier(get_string_from_auxdata (List.hd al))) in
               Ast.DottedName(l::(List.rev dnl))   
(* | [< >] -> [] *)

                                                                                                                                                                                                                                                                                                                                                                                                
(** identifier { "." identifier | "[" integer {, integer } "]" }    *)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              
and parse_designator = parser

 | [< dn=parse_dotted_name; stream>] ->
            begin parser
            | [<a=parse_actuals>]   -> Ast.ExpFuncCall(Ast.Designator(dn, a))  
            | [< >] -> Ast.ExpDes(Ast.Designator(dn, []))
            end stream          
 

and parse_else = parser

 | [< (*'Token.Token(Token.NL, (id, ln, col), al); *)
      'Token.Token(Token.ESLE, (id, ln, col), al); el=parse_statement_suite>] -> 
      print_endline "parse_else: else part parsed";  el
 | [< >] -> print_endline "parse_else: no else part found"; Ast.Compound([])    
 
and parse_if = parser

 | [< 'Token.Token(Token.THN, (id, ln, col), al); th=parse_statement_suite;  el=parse_else>] ->
    print_endline "parse_if: if statement parsed";(th,el) 




(** local_variable ::= variable | constant *)
and parse_local_variable = parser
    | [<  v=parse_variable >] -> Ast.VCVar(v)
(*  | [<  c=parse_constant >] -> Ast.VCConst(c) *)


(*
type variable =     Variable of identifier list * typ * vType

 test 

type constant =     Const of identifier * typ * expression 

type varorconst = VCConst of constant
                                | VCVar of variable
*)



and parse_other_local_variables acc = parser
  | [< 
        l=parse_local_variable;
        ll=parse_other_local_variables ( l::acc)
     >] ->  ll
  | [< >] ->  acc

and parse_local_variable_list = parser
 | [< l=parse_local_variable ; ll=parse_other_local_variables []  >] -> 
                print_endline "parse_local_variable_list - local variable list found"; 
                                  l::(List.rev ll) 
                                                                           


(** local_variable_suite  ::=  INDENT local_variable {local_variable} DEDENT *)
and parse_local_variable_suite = parser
 | [< 
  (*    'Token.Token ( Token.INDENT, (st2,ln2, col2), al2 ); *)
            ll=parse_local_variable_list
  (*    'Token.Token ( Token.DEDENT, (st3,ln3, col3), al3 ) *)
   >] -> print_endline "parse_local_variable_suite - statement list found"; ll         
   

                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                        
                                                                                                                                                                                                                                                                                                                                                                
(** compound_statement(t)  ::= "if" expression "then" compound [[NL] "else" compound]        
                    | "while" expression "do" compound                               
                    | local_variable_list statement suite                               *)                
and parse_compound_statement = parser
 | [< 'Token.Token ( Token.IF, (st,ln, col), al); e=parse_expression; i=parse_if >] -> 
        print_endline "parse_statement - if statement found"; Ast.StmtIf(e, fst i, snd i) 
 | [< 'Token.Token ( Token.WHILE, (st,ln, col), al); e=parse_expression;
            'Token.Token ( Token.DO, (st1,ln1, col1), al1); c=parse_statement_suite 
      >] -> 
        print_endline "parse_statement - while statement found"; Ast.StmtWhile(e,c) 

 | [<  ll=parse_local_variable_list; c=parse_statement_suite >] -> 
       print_endline "parse_statement - var statement found - local variables"; Ast.StmtLocals(Ast.Locals(ll),c)

(*
 | [<  c=parse_statement_suite >] -> 
       print_endline "parse_statement - var statement found"; Ast.StmtLocals(Ast.Locals([]),c)
*)
    





(** simple_statement(t)  ::= raise
                                        | designator [":=" expression] 
                    | "if" expression "then" compound [[NL] "else" compound]        
                    | "while" expression "do" compound                               
                    | variable compound                                             
                    | constant compound                                     
                    | "return" [expression]                                          
                    | "extend" dotted_name "with" identifier list
                    | "implement" dotted_name "with" identifier list                          
                    | "remove" identifier "from" dotted_name 
                    | "replace" identifier "with" identifer "in" dotted_name 
                                                       *)                 
and parse_simple_statement = parser
 | [< 'Token.Token ( Token.RAISE, (st,ln, col), al) >] -> 
        print_endline "parse_statement - raise statement found"; Ast.StmtRaise     
                 
 | [< d=parse_designator; stream >] ->
        begin parser
             | [< 'Token.Token ( Token.BECOMES, (st2,ln2, col2), al2); e=parse_expression >] -> 
                print_endline "parse_simple_statement - becomes statement found"; Ast.StmtDesign(d,e) 
             | [< >] -> Ast.StmtProcCall(d)         
      end stream
            
 | [< 'Token.Token ( Token.RETURN, (st,ln, col), al); stream >] ->
      begin parser
      | [< e=parse_expression >] -> print_endline "parse_simple_statement - return statement found"; Ast.StmtRet(e)
      | [< >] -> print_endline "parse_simple_statement - return statement found"; Ast.StmtRet(Ast.ExpNil)      
      end stream
   
                                                        
 | [< 'Token.Token ( Token.EXTD, (st,ln, col), al);
      dn=parse_designator;
      'Token.Token ( Token.WITH, (st2,ln2, col2), al2 );
      il=parse_idlist     
      >] -> print_endline "parse_simple_statement - extend statement found";
            Ast.StmtExtend(dn, make_id_list il )
 | [< 'Token.Token ( Token.IMPLEMENT, (st,ln, col), al);
      dn=parse_designator;
      'Token.Token ( Token.WITH, (st2,ln2, col2), al2 );
      il=parse_idlist     
      >] -> print_endline "parse_simple_statement - implement statement found";
            Ast.StmtImplement(dn, make_id_list il )
 | [< 'Token.Token ( Token.REMOVE, (st,ln, col), al);
            il=parse_idlist; 
      'Token.Token ( Token.FROM, (st2,ln2, col2), al2 );
            dn=parse_designator
      >] -> print_endline "parse_simple_statement - remove statement found";
            Ast.StmtRemove(dn, make_id_list il )
 | [< 'Token.Token ( Token.REPLACE, (st,ln, col), al);
      'Token.Token ( Token.IDENTIFIER, (st2,ln2, col2), al2 );
            'Token.Token ( Token.WITH, (st3,ln3, col3), al3 );
            'Token.Token ( Token.IDENTIFIER, (st4,ln4, col4), al4 ); 
            'Token.Token ( Token.INN, (st3,ln3, col3), al3 );    
       dn=parse_designator   
      >] -> print_endline "parse_simple_statement - replace statement found";
            Ast.StmtReplace(dn,Ast.Identifier(get_string_from_auxdata(List.hd al2)),
                                                        Ast.Identifier(get_string_from_auxdata(List.hd al4))) 


(** simple_statement_list  ::= simple_statement [ {";" simple_statement} | {simple_statement} ]  *)
and parse_other_simple_semicolon_statements acc = parser

  | [<  'Token.Token ( Token.SEMICOLON, (st,ln, col), al); 
        s=parse_simple_statement;
        sl=parse_other_simple_semicolon_statements (s::acc)
        >] ->  sl
                                
  | [< >] ->  acc


(** simple_statement_list  ::= simple_statement { ";" simple_statement} *)
and parse_other_simple_statements acc = parser

  | [< s=parse_simple_statement;
        sl=parse_other_simple_statements (s::acc)
        >] ->  sl               
  | [< >] ->  acc

and parse_simple_statement_list = parser
 | [< s=parse_simple_statement; stream >] ->
                begin parser
                | [< sscl=parse_other_simple_semicolon_statements [] >] -> 
                        print_endline "parse_simple_statements - simple statement list found"; 
                        (  s::(List.rev sscl)  )
    (*
        | [< sl=parse_other_simple_statements [] >] -> 
            print_endline "parse_simple_statements - simple statement list found"; 
            (  s::(List.rev sl)  )
                | [< >] -> [s]  

  *)
                end stream


(** statement  ::= simple_statement_list | compound_statement *)
and parse_statement = parser
 | [< sl=parse_simple_statement_list
   >] -> print_endline "parse_statement - simple statement list found"; sl            

 | [< c=parse_compound_statement >] 
            -> print_endline "parse_statement - compound statement"; [c]         
                                            
and parse_other_statements acc = parser
  | [< 
                s=parse_statement;
        sl=parse_other_statements (List.append s acc)
        >] ->  sl
  | [< >] ->  acc

and parse_statements = parser
 | [< s=parse_statement ; sl=parse_other_statements []  >] -> 
                print_endline "parse_statements - statement list found"; ( List.append s  (List.rev sl)  )
                                                                           


(** statement_suite  ::= simple_statement_list  | INDENT statement {statement} DEDENT *)
and parse_statement_suite = parser
 | [< sl=parse_simple_statement_list
   >] -> print_endline "parse_statement_suite - simple statement list found"; Ast.Compound(sl)          

 | [< 
        'Token.Token ( Token.INDENT, (st2,ln2, col2), al2 );
            sl=parse_statements;
      'Token.Token ( Token.DEDENT, (st3,ln3, col3), al3 ) 
   >] -> print_endline "parse_statement_suite - statement list found"; Ast.Compound(sl)         
                     

(** method ::= "method" identifier [formals] [":" type] statement_suite *)
and parse_method = parser
            
 | [< 'Token.Token ( Token.METHOD, ("METHOD" ,ln, col), al ); 
      'Token.Token ( Token.IDENTIFIER, (st1,ln1, col1), al1 ); stream
     >]  ->
        begin parser
        | [<fl=parse_formals; stream>] ->
                        begin parser
                        | [< 'Token.Token ( Token.COLON, (st,ln, col), al );
                                                      t=parse_type; cd=parse_statement_suite >] ->
                                      print_endline "parse_signature: formals and type found";     
                                      Ast.MemMeth(Ast.Identifier (get_string_from_auxdata(List.hd al1)), fl, t, cd, false)
                        | [< cd=parse_statement_suite >] ->
                          print_endline "parse_signature: formals and no type found";     
                          Ast.MemMeth(Ast.Identifier (get_string_from_auxdata(List.hd al1)), fl, Ast.TypVoid, cd,false)                    
                        end stream
                         
        | [< 'Token.Token ( Token.COLON, (st,ln, col), al );
                     t=parse_type; cd=parse_statement_suite >] ->  
            print_endline "parse_signature: formals and no type found";     
            Ast.MemMeth(Ast.Identifier (get_string_from_auxdata(List.hd al1)), [], t, cd,false)                    

        | [< cd=parse_statement_suite >] -> 
            print_endline "parse_signature: no formals and no type found";     
            Ast.MemMeth(Ast.Identifier (get_string_from_auxdata(List.hd al1)), [], Ast.TypVoid, cd,false)           
        end stream
        








(** "initialization" [formals] statement_suite   *)
and parse_init = parser
| [< 'Token.Token ( Token.INITIALIZATION, (st,ln, col), al ); 
      fl=parse_formals;
      cd=parse_statement_suite
   >]
     ->   
      print_endline "parse_init: init found"; 
      Ast.MemMeth (Ast.Identifier("construct"), fl, Ast.TypVoid, cd, false)   
                                (* was supposed to be true, to indicate constructor
                                  but currently not used *)    
      (* Ast.MemInit(fl, cd) *)




(** "const" identifier [":" type] "=" expression  *)

(*
and parse_constant = parser
 | [< 'Token.Token ( Token.CONST, (st,ln, col), al); 
      'Token.Token ( Token.IDENTIFIER, (st1,ln1, col1), al1); t=parse_type; 
      'Token.Token ( Token.EQL, (st2,ln2, col2), al2); e=parse_expression>] -> 
       print_endline "parse_statement - const statement found"; 
       Ast.Const(Ast.Identifier(get_string_from_auxdata(List.hd al1)),t, Ast.VTypPublic ,e) 


*)

(**  variable      ::= "var" idList ":" type                                            *)
and parse_variable = parser
 | [< 'Token.Token ( Token.VAR, (st,ln, col), al); il=parse_idlist;
      'Token.Token ( Token.COLON, (st,ln, col), al ); t=parse_type>] -> 
    Ast.Variable(make_id_list il, t, Ast.VTypPublic)

(* ------------------------------------------------------------------------------------ *)
   



(* member      ::=  constant | variable | method | init         *)
let parse_member = parser

(*
 | [< 'Token.Token ( Token.CONST, (st,ln, col), al );
      'Token.Token ( Token.IDENTIFIER, (st1,ln1, col1), al1 );
      t=parse_type;
      'Token.Token ( Token.EQL, (st2,ln2, col2), al2 ); 
       e=parse_expression
   >] -> 
        print_string "parse_simple_member: const found: ";  
        print_endline (get_string_from_auxdata (List.hd al1));
        Ast.MemConst(Ast.Identifier(get_string_from_auxdata(List.hd al1)), t, Ast.VTypPublic, e)
*)

 | [< v=parse_variable >]
      ->
        print_endline "parse_member: var found:";
        Ast.MemAttr(v)

 | [< m=parse_method >] -> print_endline "parse_member: method member found"; m
 | [< i=parse_init >] -> print_endline "parse_member: init member found"; i
            

             
let rec parse_other_members acc = parser
  | [<  
        p=parse_member;
        pl=parse_other_members (p::acc)
        >] ->  pl
  | [< >] ->  acc

let rec parse_member_list = parser
 | [< p=parse_member; pl=parse_other_members [] >] -> (  p::(List.rev pl)  )



(* returns list of spacing values in the first element of the tuple and the 
ast list in the second element *)

(* ------------------------------------------------------------------------------------ *)








(* ------------------------------------------------------------------------------------ *)




let parse_optional_clsextends = parser
 | [< 'Token.Token ( Token.EXTDS, (st1,ln1, col1), al1); nl=parse_idlist >] -> nl
 | [< >] -> print_endline "parse_optional_clsneeds:"; []

let parse_optional_clsimplements = parser
 | [< 'Token.Token ( Token.IMPLEMENTS, (st1,ln1, col1), al1); il=parse_idlist >] -> il
 | [< >] -> print_endline "parse_optional_clsneeds:"; []

let parse_optional_clsinherits = parser
 | [< 'Token.Token ( Token.INHERITS, (st1,ln1, col1), al1); 
      'Token.Token ( Token.IDENTIFIER, (st2, ln2, col2), al2) >] -> get_string_from_auxdata (List.hd al2)
 | [< >] -> print_endline "parse_optional_clsinherits:"; ""


(** class        ::= "class" identifier [EXTENDS idlist] [IMPLEMENTS idlist] [INHERITS id]
                                            INDENT member {NL member} DEDENT   *)
let rec parse_class  = parser
 | [<   'Token.Token ( Token.CLASS, ("CLASS",ln, col), al );
        'Token.Token ( Token.IDENTIFIER, (st1,ln1, col1), al1 );
        nl=parse_optional_clsextends;   
        il=parse_optional_clsimplements;
        h=parse_optional_clsinherits;
        'Token.Token ( Token.INDENT, (st2,ln2, col2), al2 );
        ml=parse_member_list;
        'Token.Token ( Token.DEDENT, (st3,ln3, col3), al3 )>] -> 
        print_string "parse: parse_class - class found: ";
        print_endline (get_string_from_auxdata (List.hd al1));
        
        Ast.Cls(
            (Ast.Identifier (get_string_from_auxdata (List.hd al1)), 1,
            (* (get_llvm_cls_int (get_string_from_auxdata (List.hd al1)) packContext), *)
            Ast.MemList(ml), nl, il, h))

 (* | [<  >] -> print_endline "parse: parse_class_list - no classes found.";  [] *)


let rec parse_other_classes acc = parser
  | [< 
        c=parse_class;
        cl=parse_other_classes (c::acc)
        >] ->  print_endline "parse_other_class:"; cl
  | [< >] ->  acc

let rec parse_class_list  = parser
 | [< c=parse_class; cl=parse_other_classes  [] >] -> (  c::(List.rev cl)  )


(**  package      ::= "package" identifier INDENT {unit} DEDENT  *)
let parse_package = parser

 | [< 'Token.Token ( Token.PACKAGE, (st,ln, col), al );  
        'Token.Token ( Token.IDENTIFIER, (st1,ln1, col1), al1 );
        'Token.Token ( Token.INDENT, (st2,ln2, col2), al2 ); 
         ul=parse_class_list; 
    'Token.Token ( Token.DEDENT, (st3,ln3, col3), al3 )
        >] -> 
        print_endline "parse_document() - begin parsing process";
        Ast.Package( Ast.Identifier (get_string_from_auxdata (List.hd al1)), Ast.ClsList(ul)  ) 


(** program    ::= "program" identifier INDENT class {NL class} "begin" statement suite "end" DEDENT *)
let parse_program  = parser

 | [< 'Token.Token ( Token.PROGRAM, (st,ln, col), al );  
        'Token.Token ( Token.IDENTIFIER, (st1,ln1, col1), al1 );
        'Token.Token ( Token.INDENT, (st2,ln2, col2), al2 ); 
        ul=parse_class_list;
        'Token.Token ( Token.BEGIN, (st3,ln3, col3), al3 );                  
        st=parse_statement_suite;
        'Token.Token ( Token.ED, (st4,ln4, col4), al4 ); 
        'Token.Token ( Token.DEDENT, (st5,ln5, col5), al5 ) 
        >] -> 
        print_endline "parse_document() - begin parsing process";
        Ast.Program ( Ast.Identifier (get_string_from_auxdata (List.hd al1)), Ast.ClsList(ul), st  )


(** compilation_unit ::= package | program *) 
let parse_compilation_unit packContext = parser

 | [< pk=parse_package >] -> (pk, packContext) 
 | [< pg=parse_program >] -> (pg, packContext)


(* cases below used for testing...This way we can unit test terms individually *)
(* for production version, cases below should be commented out *) 

(*
 | [< form=parse_formals>]  
    -> print_endline "init found - great";
        (Ast.Package( Ast.Identifier ("init"), Ast.ClsList([])  ) , packContext)


 | [< init=parse_init>]  
    -> print_endline "init found - great";
        (Ast.Package( Ast.Identifier ("init"), Ast.UniList([])  ) , packContext)


 | [< meth=parse_method>]  
    -> print_endline "meth found - great";
        (Ast.Package( Ast.Identifier ("meth"), Ast.ClsList([])  ) , packContext)

*)

(*

 | [< ss=parse_simple_statement >]  
    -> print_endline "simple statement found - great";
        (Ast.Package( Ast.Identifier ("ss"), Ast.ClsList([])  ) , packContext) 
*)

(*
 | [< ss=parse_statement_suite >]  
    -> print_endline "statement suite found - great";
        (Ast.Package( Ast.Identifier ("ss"), Ast.ClsList([])  ) , packContext) 
*)


(*
 | [< ss=parse_expression >]  
    -> print_endline "expression found - great";
        (Ast.Package( Ast.Identifier ("exp"), Ast.ClsList([])  ) , packContext) 
*)

(*
 | [< ss=parse_statement >]  
    -> print_endline "statement found - great";
        (Ast.Package( Ast.Identifier ("state"), Ast.UniList([])  ) , packContext) 

 | [< ss=parse_expression >]  
    -> print_endline "expression found - great";
        (Ast.Package( Ast.Identifier ("exp"), Ast.UniList([])  ) , packContext) 

 | [< dn=parse_dotted_name >]  
    -> print_endline "dotted name found - great";
        (Ast.Package( Ast.Identifier ("dn"), Ast.UniList([])  ) , packContext) 
*)
 | [<  >] ->  raise (Stream.Error "parse_compilation_unit: invalid parse compilation unit") 
end;;






