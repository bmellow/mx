open Token

exception Error of string

module Lexer = 
struct

(** 
    {b Lexer}
    This module takes input and transform it into tokens defined in the
    @see 'Token.ml' Token module.  
    
*)


(** a stack structure which holds the current levels of indenting 
    in a input file as it is being parsed. Currently implemented as a
    list of integers, where each integer denotes a indent point and its 
    value is the number of spaces from the lastest newline (or beginning of file)
    the point resides. 
    {e Invariant} is that the indent point integer values are strictly 
    increasing from the bottom of the stack and that the values are 
    non-negative *) 
let spaceStack : int list ref = ref [0];;


(** functions for modifying global stack for spacing information.  Not designed to be 
    generic, but specifically designed for the {{: spacestack}} spacestack variable of 
    this module.
*)


(** Print current state of stack for debugging purposes *)
let print_stack st =     print_string " Stack [";
                         List.iter (fun a -> print_char ' '; print_int a) st; 
                         print_endline "]"
                         
(** Push a new indent point onto the stack. TODO: enforce invariant *)
let push_spacing st sp = 
                         print_string "stack: Pushed: "; 
                         print_int sp; 
                         print_stack st;
                         sp::st
                         
(** Pop the last indent point off the stack.  *)
let pop_spacing st =     
                         print_string "stack: Popped: "; 
                         print_int (List.hd st); 
                         print_stack st;
                         List.tl st


(* TODO: fix calculation of line column values *)



(** prints the type of token being consumed and the 
location of the input file where it is found.  This is just for
formatting the location as the caller is responsible for providing
location and token information *)
let print_token_location tk ln col = (* TODO: Disable for now
  print_string "token: "; print_string tk;
  print_string " at ("; print_int ln; print_string ","; print_int col;
  print_endline ")" *)
  ()





(** function adds DEDENT tokens to offset current INDENT tokens.  Called when the current
    column index is less than the length of the previous line in the stream.  An DEDENT
    token is added for every stack element greater than the current col.  Each DEDENT 
    token insertion also triggers the element on top of the stack to be popped off.

    @param lex  lex function used for processing stream
    
    @param sp  current column location of first non-space character of current line 
                in the stream
    
    @param ln  current row in stream being parsed (assumes row is 0 
            at the beginning of the stream and is incremented whenever a
            new line is encountered) 
            
    @param col current column in stream.  Value represents number of characters
                relative to the begininng of the stream or the last newline
                encountered (whichever value is less).  
                
    @param spstk stack that stores current indentation points in the stream. 
    
    @param res number of DEDENT tokens inserted thus far.
    
    @param instrm stream originally passed into top-level call 
    
    @param outstrm stream ultimately gets returned to the caller.  Has DEDENT
            tokens appended to the instrm *)    
let rec addDedentTokens (lex, sp, ln, col, stk, res, instrm, outstrm) =
 if ((List.hd stk) = sp) then
   (lex, sp, ln, col, stk, res, instrm ,[<  outstrm; lex ln col stk instrm>] ) 
 else
   begin
   print_token_location "DEDENT" ln col;
   addDedentTokens (lex, sp,ln, col, (pop_spacing stk), res+1, instrm,  
   [<'Token.Token (Token.DEDENT, ("DEDENT", 0, 0), []); outstrm >] )
   end



(** helper functions to process access members of tuple returned by addDedentTokens()
    functions.

    @param tup whose elements are defined as follows  
    
    {ul {- lex function used for processing stream}
    {- sp  current column location of first non-space character of current line 
                in the stream}
    {- ln  current row in stream being parsed (assumes row is 0 
            at the beginning of the stream and is incremented whenever a
            new line is encountered) } 
    {- col current column in stream.  Value represents number of characters
                relative to the begininng of the stream or the last newline
                encountered (whichever value is less). }
    {- spstk stack that stores current indentation points in the stream. }
    {- res number of DEDENT tokens inserted thus far. }
    {- instrm stream originally passed into top-level call }
    {- outstrm stream ultimately gets returned to the caller.  
            tokens appended to the instrm }
    }
            
    @return outstrn member of the tuple *) 
let get_outstrm (lex, sp, ln, col, stk, res, instrm, outstrm) = outstrm




(** Recursive function that takes an input stream of characters and converts
    it into a stream of tokens as explained in 
    @see <http://caml.inria.fr/pub/docs/manual-camlp4/manual003.html> Camlp4 
    documentation.
    
    Idea is that you do pattern match on the current stream, consume the match
    and replace it with the appropriate token definition.
    
    @param ln  current row in stream being parsed (assumes row is 0 
            at the beginning of the stream and is incremented whenever a
            new line is encountered) 
            
    @param col current column in stream.  Value represents number of characters
                relative to the begininng of the stream or the last newline
                encountered (whichever value is less).  
                
    @param spstk stack that stores current indentation points in the stream. 
    
    @return stream when characters are replaced with the appropriate tokens *)
let rec lex ln col spstk = parser
  (* Skip any whitespace. *)
  | [< ' ( ' ' | '\r' | '\t' as c); stream >] -> 
      print_string "char discarded:"; 
      print_char c; 
      print_token_location " - extra character" ln col;
      print_endline "";
      lex ln (col+1) spstk stream

  | [< ' ('\n'); stream >] -> lex_indent 0 (ln+1) 0 spstk stream 

  (* TODO: deal with bug where 2 character token are not handled correctly,
   currently second character is always eaten even if it is not part of the token.
   This causes the following token to be incorrect  [missing its first character]
   Current workaround is to ensure 2 character tokens in input are always followed 
   by a throwaway character *)
 
  
  (* semicolon symbol [;] *)
  | [< ' (';'); stream >] ->
      [< 'Token.Token (Token.SEMICOLON,( "SEMICOLON",ln,col), [] ); lex ln (col+1) spstk stream >] 

  (* multiplication symbol [*] *)
  | [< ' ('*'); stream >] ->
      [< 'Token.Token (Token.TIMES,( "TIMES",ln,col), [] ); lex ln (col+1) spstk stream >] 

  (* minus symbol [-] *)
  | [< ' ('-'); stream >] ->
      [< 'Token.Token (Token.MINUS,( "MINUS",ln,col), [] ); lex ln (col+1) spstk stream >] 


   (* comma symbol [,] *)
  | [< ' (','); stream=lex ln (col+1) spstk >] ->
      print_token_location "COMMA" ln col;
      [< 'Token.Token ( Token.COMMA, ("COMMA",ln,col),[] ); stream >]     
    
   (* period symbol [.] *)
  | [< ' ('.'); stream=lex ln (col+1) spstk >] ->
      print_token_location "PERIOD" ln col;
      [< 'Token.Token (Token.PERIOD, ("PERIOD",ln,col) ,[] ); stream >]   
      
   (* left parentheses symbol [(] *)
  | [< ' ('('); stream=lex ln (col+1) spstk >] ->
        print_token_location "LPAREN" ln col;  
        [< 'Token.Token ( Token.LPAREN , ( "LPAREN",ln,col), [] ); stream >]        
      
    (* right parentheses symbol [)] *)
  | [< ' (')'); stream=lex ln (col+1) spstk >] ->
        print_token_location "RPAREN" ln col;
        [< 'Token.Token (Token.RPAREN ,  ("RPAREN", ln,col),[] ); stream >]   
      
    (* left brace symbol [{] *)
  | [< ' ('{'); stream >] ->
      [< 'Token.Token (Token.LBRACE, ("LBRACE",ln,col), []); lex ln (col+1) spstk stream >]  
      
    (* right brace symbol [}] *)
  | [< ' ('}'); stream >] ->
      [< 'Token.Token (Token.RBRACE, ("RBRACE",ln,col), []); lex ln (col+1) spstk stream >] 
      
   (* left square bracket symbol [ [ ] *)
  | [< ' ('['); stream >] ->
      [< 'Token.Token (Token.LBRAK , ("LBRAK",ln,col), []); lex ln (col+1) spstk stream >]  
      
    (* right square symbol [ ] ] *)
  | [< ' (']'); stream >] ->
      [< 'Token.Token (Token.RBRAK, ("RBRAK",ln,col), []); lex ln (col+1) spstk stream >]  
      
  (* colon: [:], becomes [:=] *)
  | [< ' (':'); stream >] ->
      lex_colon ln (col+1) spstk stream      
      
     
  (* equal: [=], lessthanequal: [=<], equiv: [==], rimply: [=>], nequiv: [=/=] *)
  | [< ' ('='); stream >] ->
      lex_equal ln (col+1) spstk stream 

  (* nequal: [/=], and: [/\]  *)
  | [< ' ('/'); stream >] ->
      lex_fslash ln (col+1) spstk stream
(* 
  (* or: [\/]  *)
  | [< ' ('\\'); stream >] ->
      lex_bslash ln (col+1) spstk stream


*)


  (* greater than: [>], greaterthenequal [>=] *)
  | [< ' ('>'); stream >] ->
      lex_gthen ln (col+1) spstk stream

 (* less than: [<], limply [<=] *)
  | [< ' ('<'); stream >] ->
      lex_lthen ln (col+1) spstk stream

  (* identifier: [a-zA-Z][a-zA-Z0-9] *)
  | [< ' ('A' .. 'Z' | 'a' .. 'z' as c); stream >] ->
      let buffer = Buffer.create 1 in
      Buffer.add_char buffer c;
      lex_ident (buffer) ln (col+1) spstk stream
      
  (* number: [0-9.]+ *)

  | [< ' ('0' .. '9' as c); stream >] ->
      let buffer = Buffer.create 1 in
      Buffer.add_char buffer c;
      lex_int buffer ln (col+1) spstk stream

  (* for all symbol *)
  | [< ' ('!'); stream >] ->
      [< 'Token.Token (Token.KWD, ("!", ln,col), [Token.CH '!']) ; lex ln (col+1) spstk stream >] 

  (* exists symbol *)
  | [< ' ('?'); stream >] ->
      [< 'Token.Token (Token.KWD, ("?", ln, col), [Token.CH '?']) ; lex ln (col+1) spstk stream >] 

  (* plus symbol *)
  | [< ' ('+'); stream >] ->
      [< 'Token.Token (Token.PLUS, ("PLUS",ln,col), [Token.CH '+']) ; lex ln (col+1) spstk stream >] 
      

  (* Otherwise, just return the character as its ascii value. *)
  | [< 'c; stream >] ->
      print_token_location (string_of_int (int_of_char c) ) ln col;
      [< 'Token.Token (Token.KWD, (Char.escaped c,ln,col), [Token.CH c]); lex ln (col+1) spstk stream >]

  (* add dedents (if required) when we reach the end of the character stream *) 
  | [< stream >] -> if ((List.length spstk) < 2) then
                        [< >]
                    else
                        [< 'Token.Token (Token.DEDENT, ("DEDENT", 0, 0), []); lex ln col (pop_spacing spstk) stream  >]     
       
(** Function called when a colon is encountered in the stream.  If the next character 
    is an equals sign, interpret this as an assignment symbol and insert a BECOMES
    token in the stream.  Otherwise insert a COLON token into the stream 
    
    @param ln  current row in stream being parsed (assumes row is 0 
            at the beginning of the stream and is incremented whenever a
            new line is encountered) 
            
    @param col current column in stream.  Value represents number of characters
                relative to the begininng of the stream or the last newline
                encountered (whichever value is less). 
                
    @param spstk stack that stores current indentation points in the stream. 
    
    @return stream modified with appropriate token replacing matched characters *)
and lex_colon ln col spstk = parser
  | [< ' ('='); stream=lex ln (col+1) spstk  >] ->
      [< 'Token.Token (Token.BECOMES, ("BECOMES",ln,col), [] );  stream >]
  | [< 'c; stream=lex ln (col+1) spstk >] ->
      print_token_location "COLON" ln col;
      [< 'Token.Token (Token.COLON, ("COLON",ln,col), []); stream >]




(** Function called when an equal is encountered in the stream.  Check the 
    next character and see it matches another defined symbol. If so, consume character
    and interpret and insert sthe appropriate into thee token in the stream.  
    Otherwise insert a EQUAL token into the stream 
    
    @param ln  current row in stream being parsed (assumes row is 0 
            at the beginning of the stream and is incremented whenever a
            new line is encountered) 
            
    @param col current column in stream.  Value represents number of characters
                relative to the begininng of the stream or the last newline
                encountered (whichever value is less). 
                
    @param spstk stack that stores current indentation points in the stream. 
    
    @return stream modified with appropriate token replacing matched characters *)
and lex_equal ln col spstk = parser
  | [< ' ('/'); stream >] ->
      lex_nequ ln (col+1) spstk stream
            
  (* lessthanequal: [=<] *)   
  | [< ' ('<'); stream >] ->
      [< 'Token.Token (Token.LEQ,("LEQ",ln,col), []); lex ln (col+1) spstk stream >] 
  (* rimply: [=>] *)   
  | [< ' ('>'); stream >] ->
      [< 'Token.Token (Token.IMPLIES,("IMPLIES",ln,col), []); lex ln (col+1) spstk stream >]  
  (* equiv: [==] *)   
  | [< ' ('='); stream >] ->
      [< 'Token.Token (Token.EQUIV,("EQUIV",ln,col), []); lex ln (col+1) spstk  stream >]            
  (* equal: [=] *)      
  | [< 'c; stream=lex ln (col+1) spstk >] ->
      print_token_location "EQL" ln col;
      [< 'Token.Token (Token.EQL,("EQL",ln,col), []);  stream >]

(** Function called when a forward slash is encountered in the stream.  If the next character 
    is an equals sign, interpret this as an assignment symbol and insert a NEQUIV
    token in the stream.  Otherwise there is problem with the input and an exception 
    is thrown.
    
    @param ln  current row in stream being parsed (assumes row is 0 
            at the beginning of the stream and is incremented whenever a
            new line is encountered) 
            
    @param col current column in stream.  Value represents number of characters
                relative to the begininng of the stream or the last newline
                encountered (whichever value is less). 
                
    @param spstk stack that stores current indentation points in the stream.
    
    @return stream modified with appropriate token replacing matched characters *)
and lex_nequ ln col spstk = parser
  | [< ' ('='); stream >] ->
      [< 'Token.Token (Token.NEQUIV, ("NEQUIV",ln,col), []); lex ln (col+1) spstk stream >]
  (* error condition: *) 		     
  | [<  >] -> raise (Stream.Error "lex_nequ: lexing error")
            
and lex_fslash ln col spstk  = parser
  (* nequal: [/=] *)   
  | [< ' ('='); stream >] ->
      [< 'Token.Token (Token.NEQ, ("NEQ",ln,col), []); lex ln (col+1) spstk stream >] 
  (* and: [/\] *)       
  | [< ' ('\\'); stream >] ->
      [< 'Token.Token (Token.LAND, ("LAND",ln,col), []) ; lex ln (col+1) spstk stream >]
  (* divide: [/] *) 		     
  | [< stream  >] ->
      [< 'Token.Token (Token.DIVIDE, ("DIVIDE",ln,col), []) ; lex ln (col+1) spstk stream >]
 
and lex_bslash ln col spstk = parser 
  (* or [\/] *)       
  | [< ' ('/'); stream >] ->
      [< 'Token.Token (Token.LOR, ("LOR",ln,col), []); lex ln (col+1) spstk stream >]
  | [< 'c; stream >] ->
      print_token_location "KWD \\" ln col;
      [< 'Token.Token (Token.KWD, (Char.escaped c,ln,col), [Token.CH c]); lex ln (col+1) spstk stream >]


and lex_gthen ln col spstk  = parser 
  (* greaterthenequal [>=] *)       
  | [< ' ('='); stream >] ->
      [< 'Token.Token (Token.GEQ, ("GEQ",ln,col), []); lex ln (col+1) spstk stream >]
  (* greater than: [>] *) 		     
  | [< 'c; stream >] ->
      [< 'Token.Token (Token.GTR, ("GTR",ln,col), []); lex ln (col+1) spstk stream >]   

and lex_lthen ln col spstk = parser 
  (* limply [<=] *)       
  | [< ' ('='); stream >] ->
      [< 'Token.Token (Token.FOLLOWS, ("FOLLOWS",ln,col), []); lex ln (col+1) spstk stream >]
  (* less than: [<] *) 		     
  | [< 'c; stream >] ->
      [< 'Token.Token (Token.LSS, ("LSS",ln,col), []); lex ln (col+1) spstk stream >]       


(** Function called when a new line is encountered in the stream.  It moves the current
    column to the first non-space character.  The spaces/tabs/newlines are consumed 
    and are replaced by either a 
    
    - { b NL} token which indicates that the number of spaces is consistant with the previous 
                line containing non-space characters                
    - { b INDENT} token which indicates that the number of spaces is greater than the 
                  value at the top of the spacing stack.  Current column is added to the 
                  spacing stack.
    - a set of { b DEDENT} tokens which indicates that the number of spaces is less than 
                    the value at the top of the spacing stack. A {b DEDENT} is inserted
                    for each integer in the spacing stack that is greater than the current
                    column value. TODO: confirm semantics  
    
    @param ln  current row in stream being parsed (assumes row is 0 
            at the beginning of the stream and is incremented whenever a
            new line is encountered) 
            
    @param col current column in stream.  Value represents number of characters
                relative to the begininng of the stream or the last newline
                encountered (whichever value is less). 
                
    @param spstk stack that stores current indentation points in the stream. 
    
    @return stream modified with appropriate token replacing matched characters *)
and lex_indent sp ln col spstk = parser
  | [< ' (' ') ; stream >] ->
      lex_indent (sp+1) ln (col+1) spstk stream
      
  | [< ' ('\n') ; stream >] ->
      lex_indent 0 (ln+1) 0 spstk stream
     
  | [< stream >] -> 
      (* match !spaceStack with *)
      match spstk with 
(*
		     | h::t when sp=h ->            print_token_location "NL" ln col; 
                                    print_string "indent: NL Spacing: "; print_int sp;
                                    print_string " Stack Head: "; print_int h; 
                                    print_endline ""; 
                                   (* [< 'Token.Token (Token.NL, ("NL",ln,col), [Token.IT sp]); *)
                                    [< lex ln col spstk stream >]
*)                                    
     | h::t when sp>h ->            print_token_location "INDENT" ln col;
                                    print_string "indent: Indent Spacing: "; print_int sp;
                                    print_string " Stack Head: "; print_int h; 
                                    print_endline""; 

                                    (* push_spacing spaceStack sp; *)
                                    [< 'Token.Token (Token.INDENT, ("INDENT",ln,col),[] ); 
                                    lex ln col (push_spacing spstk sp) stream  >]
                                    
     | h::t when ( sp<h && (List.exists (fun a -> sp=a) t) && (List.length t > 1)) ->  
                                                                        
                                    
                                    get_outstrm 
                                    (
                                    addDedentTokens (lex, sp, ln, col, 
                                    spstk, 0, stream, [< >]) 
                                    )
                                    
                                    
     | h::t when sp=h -> 
                                    print_string "indent: ESpacing: "; print_int sp;
                                    print_string " EStack:"; 
                                    print_stack spstk; 
                                    (* raise (Stream.Error "lex_ident")  *)
                                    [< lex ln col spstk stream >]
																		
		 | _ ->													
																		raise (Stream.Error "indenting issue")



(** Function called when a digit is encountered in the stream.  It collects subsequent 
    digits found in a buffer.  An INT token is produced with the buffer contents once
    a non-numeric character is found.
    
    @param ln  current row in stream being parsed (assumes row is 0 
            at the beginning of the stream and is incremented whenever a
            new line is encountered) 
            
    @param col current column in stream.  Value represents number of characters
                relative to the begininng of the stream or the last newline
                encountered (whichever value is less). 
                
    @param spstk stack that stores current indentation points in the stream.
    
    @return stream modified with appropriate token replacing matched characters *)
and lex_int buffer ln col spstk  = parser
  | [< ' ('0' .. '9' as c); stream >] ->
      Buffer.add_char buffer c;
      lex_int buffer ln (col+1) spstk stream
      
  (* TODO should ensure that next non-numeric character is a space character *)
  
  | [< stream=lex ln col spstk >] ->
      print_string "token: INT added ";
      print_string (Buffer.contents buffer);
      print_endline "";
      [< 'Token.Token (Token.INT, (  (String.concat " " ["INT"; Buffer.contents buffer]), ln, col) 
      , [Token.IT (int_of_string (Buffer.contents buffer))] ); stream >]

(** Function called when a alphabetic character is encountered in the stream.  
    It collects subsequent characters and digits found in a buffer.  Once a 
    non-alphabetic or non-digit character is found.  The contents are checked against
    language keywords.  If a match is found, the appropriate token is placed in the 
    stream.  Otherwise, an IDENTIFIER token is inserted.
    
    @param ln  current row in stream being parsed (assumes row is 0 
            at the beginning of the stream and is incremented whenever a
            new line is encountered) 
            
    @param col current column in stream.  Value represents number of characters
                relative to the begininng of the stream or the last newline
                encountered (whichever value is less). 
                
    @param spstk stack that stores current indentation points in the stream.
    
    @return stream modified with appropriate token replacing matched characters *)
and lex_ident buffer ln col spstk = parser
  | [< ' ('A' .. 'Z' | 'a' .. 'z' | '0' .. '9' as c); stream >] ->
      Buffer.add_char buffer c;
      lex_ident buffer ln (col+1) spstk stream
  | [< stream=lex ln col spstk >] ->
      match Buffer.contents buffer with
      | "and" ->       print_token_location "AND" ln col; 
                       [< 'Token.Token (Token.AND,("AND",ln, col), [] ); stream >]
      | "as" ->        print_token_location "AS" ln col;  
                       [< 'Token.Token (Token.AS, ("AS",ln, col), []); stream >]
      | "array" ->     print_token_location "ARRAY" ln col; 
                       [< 'Token.Token (Token.ARRAY, ("ARRAY", ln, col), []); stream >]
      | "begin" ->     print_token_location "BEGIN" ln col;
                       [< 'Token.Token (Token.BEGIN, ("BEGIN",ln, col), []); stream >]
      | "boolean" ->   print_token_location "BOOLEAN" ln col;
                       [< 'Token.Token (Token.BOOLEAN, ("BOOLEAN",ln, col), []); stream >]
      | "char" ->      print_token_location "CHAR" ln col;
                       [< 'Token.Token (Token.CHAR, ("CHAR",ln, col), []); stream >]
      | "call" ->      print_token_location "CALL" ln col;
                       [< 'Token.Token (Token.CALL, ("CALL",ln, col), []); stream >]
      | "class" ->     print_token_location "CLASS" ln col;
                       [< 'Token.Token (Token.CLASS, ("CLASS",ln, col), []); stream >]
      (**
      | "compose" ->   print_token_location "COMPOSE" ln col;
                       [< 'Token.Token (Token.COMPOSE, ("COMPOSE",ln, col), []); stream >] 
      *)
      | "const" ->     print_token_location "COMPOSE" ln col;
                       [< 'Token.Token (Token.CONST, ("CONST",ln, col), []); stream >]     
      | "div" ->       print_token_location "DIV" ln col; 
                       [< 'Token.Token (Token.DIV, ("DIV",ln, col), []); stream >]
      | "do" ->        print_token_location "DO" ln col; 
                       [< 'Token.Token (Token.DO, ("DO",ln, col), []); stream >]
      | "else" ->      print_token_location "ELSE" ln col;
                       [< 'Token.Token (Token.ESLE, ("ESLE",ln, col), []); stream >]
      | "end" ->       print_token_location "ED" ln col;
                       [< 'Token.Token (Token.ED, ("ED",ln, col), []); stream >]
      | "extend" ->    print_token_location "EXTEND" ln col; 
                       [< 'Token.Token (Token.EXTD, ("EXTEND",ln, col), []); stream >]
      | "extends" ->   print_token_location "EXTENDS" ln col; 
                       [< 'Token.Token (Token.EXTDS, ("EXTENDS",ln, col), []); stream >] 
      | "false" ->     print_token_location "FALSE" ln col; 
                       [< 'Token.Token (Token.FALSE,("FALSE",ln, col), []); stream >]
      | "from" ->      print_token_location "FROM" ln col; 
                       [< 'Token.Token (Token.FROM,("FROM",ln, col), []); stream >]
      | "has" ->       print_token_location "HAS" ln col;  
                       [< 'Token.Token (Token.HAS, ("HAS",ln, col), []); stream >]
      | "if" ->        print_token_location "IF" ln col;
                       [< 'Token.Token (Token.IF, ("IF",ln, col), []); stream >]
      | "in" ->        print_token_location "IN" ln col;
                       [< 'Token.Token (Token.INN, ("IN",ln, col), []); stream >]
      | "inherits" ->   print_token_location "INHERITS" ln col;  
                       [< 'Token.Token (Token.INHERITS, ("INHERITS",ln, col), []); stream >]
      | "integer" ->   print_token_location "INTEGER" ln col;  
                       [< 'Token.Token (Token.INTEGER, ("INTEGER",ln, col), []); stream >]
      | "interface" -> print_token_location "INTERFACE" ln col;  
                       [< 'Token.Token (Token.INTERFACE, ("INTERFACE",ln, col), []); stream >]
      | "import" ->    print_token_location "IMPORT" ln col; 
                       [< 'Token.Token (Token.IMPORT, ("IMPORT",ln, col), []); stream >]
      | "initialization" -> 
                       print_token_location "INITIALIZATION" ln col; 
                       [< 'Token.Token (Token.INITIALIZATION,("INITIALIZATION",ln, col), []); stream >]
      | "implement" -> 
                       print_token_location "IMPLEMENT" ln col; 
                       [< 'Token.Token (Token.IMPLEMENT,("IMPLEMENT",ln, col), []); stream >]
      | "implements" -> 
                       print_token_location "IMPLEMENTS" ln col; 
                       [< 'Token.Token (Token.IMPLEMENTS,("IMPLEMENTS",ln, col), []); stream >]
      | "main" ->      print_token_location "MAIN" ln col;
                       [< 'Token.Token (Token.MAIN,("MAIN",ln, col), []); stream >]
      | "method" ->    print_token_location "METHOD" ln col;
                       [< 'Token.Token (Token.METHOD,("METHOD",ln, col), []); stream >]
      | "mod" ->       print_token_location "MOD" ln col;
                       [< 'Token.Token (Token.MOD, ("MOD",ln, col), []); stream >]
      (**
      | "needs" ->     print_token_location "NEEDS" ln col; 
                       [< 'Token.Token (Token.NEEDS,("NEEDS",ln, col), []); stream >]
      *)
      | "new" ->       print_token_location "NEW" ln col; 
                       [< 'Token.Token (Token.NEW, ("NEW",ln, col), []); stream >]
      | "nil" ->       print_token_location "NIL" ln col; 
                       [< 'Token.Token (Token.NIL, ("NIL",ln, col), []); stream >]
      | "not" ->       print_token_location "NOT" ln col;
                       [< 'Token.Token (Token.NOT, ("NOT",ln, col),[]); stream >]
      | "of" ->        print_token_location "OF" ln col; 
                       [< 'Token.Token (Token.OF, ("OF",ln, col), []); stream >]
      | "or" ->        print_token_location "OR" ln col;
                       [< 'Token.Token (Token.OR, ("OR",ln, col), []); stream >]
      | "package" ->   print_token_location "PACKAGE" ln col;
                       [< 'Token.Token (Token.PACKAGE, ("PACKAGE",ln, col), [] ); stream  >]
      | "program" ->   print_token_location "PROGRAM" ln col;
                       [< 'Token.Token (Token.PROGRAM, ("PROGRAM",ln, col), []); stream >]
      | "raise" ->     print_token_location "RAISE" ln col;
                       [< 'Token.Token (Token.RAISE, ("RAISE",ln, col), []); stream >]
      | "remove" ->    print_token_location "REMOVE" ln col;
                       [< 'Token.Token (Token.REMOVE, ("REMOVE",ln, col), []); stream >]
      | "replace" ->   print_token_location "REPLACE" ln col;
                       [< 'Token.Token (Token.REPLACE, ("REPLACE",ln, col), []); stream >]
      | "return" ->    print_token_location "RETURN" ln col;
                       [< 'Token.Token (Token.RETURN, ("RETURN",ln, col), []); stream >]
      | "set" ->       print_token_location "SET" ln col;
                       [< 'Token.Token (Token.SET, ("SET",ln, col), []); stream >]
      | "then" ->      print_token_location "THEN" ln col;
                       [< 'Token.Token (Token.THN, ("THEN",ln, col), []); stream >]
      | "this" ->      print_token_location "THIS" ln col;
                       [< 'Token.Token (Token.THIS, ("THIS",ln, col), []); stream >]
      | "true" ->      print_token_location "TRUE" ln col;
                       [< 'Token.Token (Token.TRUE, ("TRUE",ln, col), []); stream >]
      | "var" ->       print_token_location "VAR" ln col; 
                       [< 'Token.Token (Token.VAR, ("VAR",ln, col), []); stream >]
      | "while" ->     print_token_location "WHILE" ln col; 
                       [< 'Token.Token (Token.WHILE, ("WHILE",ln, col), []); stream >] 
      | "with" ->      print_token_location "WITH" ln col; 
                       [< 'Token.Token (Token.WITH, ("WITH",ln, col), []); stream >] 
      | id ->          print_token_location "IDENTIFIER" ln col;
                       [< 'Token.Token (Token.IDENTIFIER, (String.concat " " ["IDENTIFIER";id],ln, col), 
                       [Token.ST id]); stream >]
end;;