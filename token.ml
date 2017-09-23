(** This module defines the types of tokens defined for the mx language.  The lexer 
    processes an input stream and converts it into a stream of tokens.  
 *)
 
 
(** Extra information added to a token for debugging purposes.  The string value is the 
    value used to print the token type when it is identified by the lexer.  The two integers
    indicate the row and col where the token is found in the input file *)
type tokendata = string * int * int


(** List of data associated with a token.  This is used with some tokens, such as the 
    indent token where the number of spaces are stored or the int token where the value
    of the integer is stored *)
type tokenauxdata = ST of string
                    | IT of int
                    | CH of char
                    
(** All actual token types in the mx language  *)
type tokentype =
  | ABORT 
  | ACTION 
  | AND  
  | ARRAY 
  | AS 
  | BEGIN
  | BOOLEAN 
  | CALL 
  | CASE  
  | CHAR  
  | CLASS
  | COMPOSE  
  | CONST  
  | DIV
  | DIVIDE
  | DO  
  | ESLE  (** else *)
  | ED (** end *)
  | EXTD (** extend *)
  | EXTDS (** extends *)
  | FALSE   
  | FROM
  | HAS 
  | IF 
  | INN 
  | IMPLEMENT
  | IMPLEMENTS
  | IMPORT
  | INHERITS  
  | INITIALIZATION
  | INTEGER 
  | INTERFACE  
  | MAIN
  | METHOD
  | MIXIN (** not used *)  
  | MOD
  | NEEDS (** no longer used *)
  | NEXT   (** not used *)
  | NEW  
  | NIL  
  | NOT  
  | OF  
  | OR  
  | PACKAGE  
  | PINT
  | PREV
  | PRIVATE  
  | PROGRAM    
  | PROTECTED  
  | PUBLIC 
  | RAISE 
  | REAL 
  | REMOVE
  | REPLACE 
  | REPEAT  
  | RETURN  
  | SHARED
  | SET  
  | SKIP  
  | SUPER  
  | THN  (** then *)
  | THIS  
  | TO  
  | TRUE  
  | TYPE  
  | UNTIL  
  | USE 
  | VAR  
  | WHEN  
  | WHILE  
  | WITH 
  | BECOMES  
  | COLON  
  | EQL  
  | NEQ  
  | LEQ  
  | LSS  
  | GEQ  
  | GTR  
  | EQUIV  
  | NEQUIV  
  | FOLLOWS  
  | IMPLIES  
  | LAND  
  | LOR  
  | TIMES  
  | PLUS  
  | MINUS  
  | ALL  
  | EXIST  
  | PERIOD  
  | COMMA  
  | SEMICOLON  
  | LPAREN  
  | RPAREN  
  | LBRACE  
  | RBRACE   
  | LBRAK  
  | RBRAK  
  
  | IDENTIFIER (** class names, method names, attributes, local variables etc *)
  | INT

  | KWD (** string data *)

  | INDENT (** indent character *)
  | DEDENT (** "undent" character *)
  | NL     (** new line *)
  
  
  (** Actual full token definition, with type, data, and list of aux data *)
  type token = Token of tokentype * tokendata * tokenauxdata list
  
  
  