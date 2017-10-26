
module Ast = 
struct
(**
{b Abstract Syntax Tree} 

This module describes types used in the {e abstract syntax} or {e parse tree} of the
mx language.
*)

type identifier =   Identifier of string

(* TODO: check to see if required *)

(** used to store identifiers that have multiple components such as
obj.attr *)


(** Describe the types of data that this language supports *)
type typ =          TypVoid             (** no type specified, used for methods with no return value *)
                    | TypInt            (** integer *)
                    | TypBool           (** boolean *)
                    | TypChar           (** character *)
                    | TypStr            (** string *)
                    | TypPtr of typ     (** memory pointer *)
                    | TypClsPtr of identifier (** class *)
                    | TypArr of expression list * typ (** array *)
                    | TypUnk                (** undecidable type, error condition *) 

                    
(** TODO: rename to visiability scope of variables - currently NOT USED *)
and vType =         VTypPublic (** public *)
                    | VTypPrivate (** private *)


(** type of unary operators *)
and uniOper =       UOpMIN (** minus *)
                    | UOpNOT (** negation *)
                    | UOpPLS (** plus *)


(*
(** Print current state of stack for debugging purposes *)
let print_stack st =     print_string " Stack [";
                         List.iter (fun a -> print_char ' '; print_int a) st; 
                         print_endline "]"
*)

(** type of binary operators *)
and binOper =       MOpMT (** multiply *)
                    | MOpDV (** divide *)
                    | MOpDIV (** integer divide *)
                    | MOpMOD (** remainder *)
                                    | AOpPL (** add *)
                    | AOpSB (** subtract *)

                    
(** type of relative operators *)

                    | ROpLT (** less than *)
                    | ROpGT (** greater than *)
                    | ROpLTE (** less than or equal to *)
                    | ROpGTE (** greater than or equal to *)
                    | ROpE (** equal *)
                    | ROpNE (** not equal *)

(** type of short-circuit boolean binary operators - NOT USED *)
                                    | BoolAnd (** boolean and *)
                    | BoolOr (** boolean or *)
 

and expression =    
    | ExpUnary of uniOper * expression
    | ExpBinary of binOper * expression * expression
    | ExpNumber of int
    | ExpDes of designator 
    | ExpNil | ExpTrue | ExpFalse
    | ExpFuncCall of designator
    | ExpNew of (typ * expression * expression list) * identifier list (** class name , array size, initial arguement list, mixin name list *)
    | ExpAs of expression * identifier
    | ExpHas of expression * identifier
    | ExpNone
    | ExpBracketExp of expression
                        
and designHelp =        DNID of identifier
                                    | DNExp of expression list

and dottedName = DottedName of designHelp list

and designator =   Designator of dottedName * expression list


type variable =     Variable of identifier list * typ * vType
(* TODO: remove visibility *)                   
                                        
(* test *)

type constant = Const of identifier * typ * expression 

type varorconst = VCConst of constant
                | VCVar of variable

type locals = Locals of varorconst list

(* end test *)
                                                            
                    


type statement =    StmtDesign of (expression * expression)
                    | StmtRaise
                    | StmtExtend of (expression * identifier list)
                    | StmtImplement of (expression * identifier list)
                    | StmtRemove of (expression * identifier list)
                    | StmtReplace of (expression * identifier * identifier)
                    | StmtIf of (expression * compound * compound)
                    | StmtWhile of (expression * compound)
                    | StmtVar of (variable * compound)
                    | StmtLocals of (locals * compound)
                    | StmtConst of (identifier * typ * expression * compound)
                    | StmtRet of expression
                    | StmtProcCall of expression

and compound =      | Compound of statement list 
(* TODO: consider making compound a type of statement *)

type member =       MemConst of identifier * typ * vType * expression
                    | MemAttr of variable 
                    | MemMeth of identifier *  (identifier * typ) list * typ * compound * bool
          (* for new, we try making constructor just special methods...the bool flag*)
          (* set indicates that this is a special method *)
                    | MemInit of  (identifier * typ) list  * compound

type memberL =      MemList of member list


(** class definition - name, id, class members, extends list, implements list, inherit  **)
type cls =          Cls of identifier * int * memberL * string list * string list * string 


(* type compmix =       Compmix of name * string list *)
    
type clsL =         ClsList of cls list
                                            
type compUnit = 
                    | Package of identifier * clsL
                    | Program of identifier * clsL * compound
end;;
