(*===----------------------------------------------------------------------===
 * Main driver code.
 *===----------------------------------------------------------------------===*)

open Symtbl
open Lexer
open Parser



open Typecheck
open Typesymtbl
open Ast 
open AstPrint

open Pgmvalues
open CodegenC

(*
open UTest001
open UTest002
open UTest003
open UTest004
open UTest005
open UTest006
open UTest007
open UTest008
open UTest009
open UTest010
open UTest011
open UTest012

open TTest001
open TTest002
open TTest003
open TTest004
open TTest005
open TTest006
open TTest007
*)


let truth_method = function
  | true -> print_string "true result"
  | false ->  print_string "false result"
	
(** used for unit testing of parser.  Makes sure that AST
    created when the parser processes input program
		is the same as the one expected.  
*)	
		
			
(*	
let unitTestForParser fstrm testAst gCon strTestName =  
    let aAndg = Parser.parse_document gCon fstrm in
    AstPrint.print_ast_package (fst aAndg);
    let check = (testAst = (fst aAndg)) in
    print_string strTestName;
    truth_method check; print_endline ""	

*)

(** instantiate/initialize the global variables for the program.  These 
	variables are contained in a tuple composed of the symbol tables and a queue storing the data*) 
let glb_init =

 
	
 (* initialize symbol tables *)
	let typeSt =  Stack.create () in	
		SymTbl.sym_add_level_to_typ_symtbl typeSt; 

  let codegenSt =  Stack.create () in 
    SymTbl.sym_add_level_to_cg_symtbl codegenSt;
		
	let cCodeQueue = Queue.create () in
	   (* Queue.add "\n" cCodeQueue *)
    
  let needsTable:(string,string list) Hashtbl.t = Hashtbl.create 107 in
		
  let res = ( typeSt, codegenSt, needsTable, cCodeQueue ) in
	res
	

let main () =
	print_endline "main() - begins";
	
	(** Install standard binary operators. 1 is the lowest precedence. *)
  print_endline "main() - build operator precedence table";
	
  (** boolean operators *)
  Hashtbl.add Parser.binop_precedence Token.AND 20;    (* lowest *)
  Hashtbl.add Parser.binop_precedence Token.OR 20;

		
	(** relational operators *)
  Hashtbl.add Parser.binop_precedence Token.LSS 30;
  Hashtbl.add Parser.binop_precedence Token.GTR 30;
  Hashtbl.add Parser.binop_precedence Token.LEQ 30;
  Hashtbl.add Parser.binop_precedence Token.GEQ 30;
  Hashtbl.add Parser.binop_precedence Token.EQL 30;
  Hashtbl.add Parser.binop_precedence Token.NEQ 30;
	
  (** multiplicative operators *)
  Hashtbl.add Parser.binop_precedence Token.TIMES 40;   
  Hashtbl.add Parser.binop_precedence Token.DIVIDE 40;
  Hashtbl.add Parser.binop_precedence Token.DIV 40;
  Hashtbl.add Parser.binop_precedence Token.MOD 40;
	
	
	(** additive operators *)
  Hashtbl.add Parser.binop_precedence Token.PLUS 50;
  Hashtbl.add Parser.binop_precedence Token.MINUS 50;
	
	(** multiplicative operators *)
	Hashtbl.add Parser.binop_precedence Token.TIMES 60;   
	Hashtbl.add Parser.binop_precedence Token.DIVIDE 60;
	Hashtbl.add Parser.binop_precedence Token.DIV 60;
	Hashtbl.add Parser.binop_precedence Token.MOD 60; 
	
	(** unary operators *)
	Hashtbl.add Parser.binop_precedence Token.MINUS 70; (** highest priority *)   
	Hashtbl.add Parser.binop_precedence Token.NOT 70; 
	Hashtbl.add Parser.binop_precedence Token.PLUS 70;     
	
	


(* reopen stream - this time for actual code generation *)
(* real code - to be put back *)
 
  let cmdLineList = Array.to_list(Sys.argv) in
	
	print_endline "main() - command line arguments";
  List.map (fun x -> print_string " "; print_string x; x) cmdLineList;
	print_endline "";
	
	
  let printDebugStreamOn = List.exists (fun x -> x = "-d") cmdLineList in
  let parserUnitTestOn = List.exists (fun x -> x = "-u") cmdLineList in
  let typeCheckUnitTestOn = List.exists (fun x -> x = "-t") cmdLineList in

	
  if printDebugStreamOn then
	begin
		(* First open and parse to see token stream generated, for debug purposes - *)
		(* NOT required *)

    flush stdout;
	print_endline "main() - debug stream";
    let fstreamDebug = 
      Lexer.lex 1 1 [0]  (Stream.of_channel (open_in Sys.argv.(1)))  in
		print_string "file used:" ; print_endline  Sys.argv.(1);

    (* dump stream to stdout *)
    Stream.iter
    (fun line -> 
       match line with
       | Token.Token (t,(s,l,c), al )  -> print_string "stream: " ; print_endline s
       | _ -> print_endline "stream: somthing extra"
       
    ) fstreamDebug
	end;

  print_endline "main() - opening input (source) file stream";   
  let fstream = Lexer.lex 1 1 [0]  (Stream.of_channel (open_in Sys.argv.(1)))  in
       (* let stream = 
       snd (Lexer.addD  ( (Lexer.size_of_spacing Lexer.spaceStack), fstream ) ) in
       *)

  print_endline "main() - initializing global context";   
  let glbContext = glb_init in 	 


(*
 if parserUnitTestOn then
 begin
    let glbContext = glb_init in

    let fstream001 = Lexer.lex 1 1 [0]  (Stream.of_channel (open_in "../mixTests/parser/uTest001.mx"))  in
    let fstream002 = Lexer.lex 1 1 [0]  (Stream.of_channel (open_in "../mixTests/parser/uTest002.mx"))  in
    let fstream003 = Lexer.lex 1 1 [0]  (Stream.of_channel (open_in "../mixTests/parser/uTest003.mx"))  in
    let fstream004 = Lexer.lex 1 1 [0]  (Stream.of_channel (open_in "../mixTests/parser/uTest004.mx"))  in
    let fstream005 = Lexer.lex 1 1 [0]  (Stream.of_channel (open_in "../mixTests/parser/uTest005.mx"))  in
    let fstream006 = Lexer.lex 1 1 [0]  (Stream.of_channel (open_in "../mixTests/parser/uTest006.mx"))  in
    let fstream007 = Lexer.lex 1 1 [0]  (Stream.of_channel (open_in "../mixTests/parser/uTest007.mx"))  in
    let fstream008 = Lexer.lex 1 1 [0]  (Stream.of_channel (open_in "../mixTests/parser/uTest008.mx"))  in
    let fstream009 = Lexer.lex 1 1 [0]  (Stream.of_channel (open_in "../mixTests/parser/uTest009.mx"))  in
    let fstream010 = Lexer.lex 1 1 [0]  (Stream.of_channel (open_in "../mixTests/parser/uTest010.mx"))  in
    let fstream011 = Lexer.lex 1 1 [0]  (Stream.of_channel (open_in "../mixTests/parser/uTest011.mx"))  in
    let fstream012 = Lexer.lex 1 1 [0]  (Stream.of_channel (open_in "../mixTests/parser/uTest012.mx"))  in
	
    unitTestForParser fstream001 UTest001.pTest001 glbContext "pTest001: ";
    unitTestForParser fstream002 UTest002.pTest002 glbContext "pTest002: ";
    unitTestForParser fstream003 UTest003.pTest003 glbContext "pTest003: ";
    unitTestForParser fstream004 UTest004.pTest004 glbContext "pTest004: ";
    unitTestForParser fstream005 UTest005.pTest005 glbContext "pTest005: ";
    unitTestForParser fstream006 UTest006.pTest006 glbContext "pTest006: ";
    unitTestForParser fstream007 UTest007.pTest007 glbContext "pTest007: ";
    unitTestForParser fstream008 UTest008.pTest008 glbContext "pTest008: ";
    unitTestForParser fstream009 UTest009.pTest009 glbContext "pTest009: ";
    unitTestForParser fstream010 UTest010.pTest010 glbContext "pTest010: ";
    unitTestForParser fstream011 UTest011.pTest011 glbContext "pTest011: ";
    unitTestForParser fstream012 UTest012.pTest012 glbContext "pTest012: ";
 end;

*)



 if typeCheckUnitTestOn then
 begin
 
(*
		print_string "tTest001 - ";
    let typeSt001 =  Stack.create () in    
        SymTbl.sym_add_level_to_typ_symtbl typeSt001;
        print_string "tTest001 result: "; print_endline (Typecheck.typechk_print_error (TTest001.test typeSt001 ));
*)

(* dont get why two instances of this fails....I thought they were independent *)
(*
    let tSt001 =  Stack.create () in
        SymTbl.sym_add_level_to_typ_symtbl tSt001;
        print_string "tTestxxx001 result: "; Typesymtbl.typesymtbl_print_error TTest001.tTableTest tSt001 TTest001.tsymTest001;
				glb_init
*)
(*
    print_string "tTest002 - ";
    let typeSt002 =  Stack.create () in    
        SymTbl.sym_add_level_to_typ_symtbl typeSt002;
        print_string "tTest002 result: "; print_endline (Typecheck.typechk_print_error (TTest002.test typeSt002) );

    print_string "tTest003 - ";
    let typeSt003 =  Stack.create () in    
        SymTbl.sym_add_level_to_typ_symtbl typeSt003;			
    print_string "tTest003 result: "; print_endline	(Typecheck.typechk_print_error (TTest003.test typeSt003));

    print_string "tTest004 - ";
    let typeSt004 =  Stack.create () in    
        SymTbl.sym_add_level_to_typ_symtbl typeSt004;           
    print_string "tTest004 result: "; print_endline (Typecheck.typechk_print_error (TTest004.test typeSt004));

    print_string "tTest005 - ";
    let typeSt005 =  Stack.create () in    
        SymTbl.sym_add_level_to_typ_symtbl typeSt005;           
    print_string "tTest005 result: "; print_endline (Typecheck.typechk_print_error (TTest005.test typeSt005));
*)


(*

   let tSt005 =  Stack.create () in
        SymTbl.sym_add_level_to_typ_symtbl tSt005;
        print_string "tTestxxx005 result: "; Typesymtbl.typesymtbl_print_error TTest005.tTableTest tSt005 TTest005.tsymTest005;

*)

(*
    print_string "tTest006 - ";
    let typeSt006 =  Stack.create () in    
        SymTbl.sym_add_level_to_typ_symtbl typeSt006;
        SymTbl.sym_set_symtbl_val typeSt006 "c" (SymTbl.STFormal(Ast.TypInt));
        print_string "tTest006 result: "; print_endline (Typecheck.typechk_print_error (TTest006.test typeSt006));

    print_string "tTest007 - ";
    let typeSt007 =  Stack.create () in    
        SymTbl.sym_add_level_to_typ_symtbl typeSt007;           
    print_string "tTest007 result: "; print_endline (Typecheck.typechk_print_error (TTest007.test typeSt007));
*) 
end;




 print_endline "main() - begin typecheck and parsing process."; 

 if not (typeCheckUnitTestOn || parserUnitTestOn) then
 begin

(*		
      let astAndGbl = Parser.parse_compilation_unit glbContext fstream in 
			astAndGbl;
			print_endline "************************done parsing only pass ************************";
			
*)

  let astAndGbl =   (*Typecheck.typechk_package type check activation *)
    (Typesymtbl.typesymtbl_package
(Parser.parse_compilation_unit glbContext fstream) ) in


(*
    print_endline "print out AST Treee.";   
				let astTree = fst astAndGbl in
				AstPrint.print_ast_package astTree; 
*)		

(* only for testing needs trees, to be removed *)
(*     let nl = (snd astAndGbl) in *)
     (*
     Hashtbl.iter  (fun c n -> 
        print_endline (String.concat "" ["class:"; c ; " needs :"; (List.hd n) ])     
        ) nl;
        
     *)
(*
     print_endline "type symbol table testing-----------------------------------------"; 
        
     let newList = (Typesymtbl.childrenX "G" (snd astAndGbl)) in
     begin
     match newList with
     | [] -> print_endline "empty list"
     | _ -> List.iter (fun a -> print_endline (String.concat "" ["g's child - class: "; a])) newList
     end;
    
    
     let newList = (Typesymtbl.childrenX "D" (snd astAndGbl)) in
     begin
     match newList with
     | [] -> print_endline "empty list"
     | _ -> List.iter (fun a -> print_endline (String.concat "" ["d's child - class: "; a])) newList
     end;

     let newList = (Typesymtbl.decendents "G" (snd astAndGbl)) in
     begin
     match newList with
     | [] -> print_endline "empty list"
     | _ -> List.iter (fun a -> print_endline (String.concat "" ["g's decendents - class: "; a])) newList
     end;
    
    
     let newList = (Typesymtbl.decendents "D" (snd astAndGbl)) in
     begin
     match newList with
     | [] -> print_endline "empty list"
     | _ -> List.iter (fun a -> print_endline (String.concat "" ["d's decendents - class: "; a])) newList
     end;
 
     let newList = (Typesymtbl.implements "D" (snd astAndGbl)) in
     begin
     match newList with
     | [] -> print_endline "implements empty list"
     | _ -> List.iter (fun a -> print_endline (String.concat "" [a ; "implements - class: D"])) newList
     end;    
         
     let d = Hashtbl.find (Pgmvalues.get_needsTable nl) "D" in
     let c = List.hd d in 
     
    let newList = (Typesymtbl.decendents c nl) in
     begin
     match newList with
     | [] -> print_endline "empty list"
     | _ -> List.iter (fun a -> print_endline (String.concat "" ["c's decendents - class: "; a])) newList
     end;       
*)  

(*    
    let newList = (Typesymtbl.needset "G" (snd astAndGbl)) in
     begin
     match newList with
     | [] -> print_endline "empty list"
     | _ -> List.iter (fun a -> print_endline (String.concat "" ["xxxxxxxxxxc's pointer decendents - class: "; a])) newList
     end; 
*)  
(*  
    let packContext = (snd astAndGbl) in    
    let newList = (Typesymtbl.extension_chain "G" packContext) in
     begin
     match newList with
     | [] -> print_endline "empty list"
     | _ -> List.iter (fun a -> print_endline (String.concat "" ["g's extensios chain - class: "; a])) newList
     end;        
     print_endline "asdfasdf";
 *)  
     print_endline "begin executing code generation process... disabled";

     let the_pack_code = CodegenC.codegen_doc (fst astAndGbl) (snd astAndGbl) in 
				 the_pack_code;

     print_endline "begin emitting C code process...";  


			let theCode = Pgmvalues.get_c_code (snd astAndGbl) in
					Queue.iter (fun cLine -> print_endline cLine) theCode;

      try
      let cout = open_out Sys.argv.(2) in
      let co = Format.formatter_of_out_channel cout in
      Queue.iter (fun cLine -> 
        Format.fprintf co "%s\n" cLine;
        print_endline cLine
      ) theCode;
      close_out cout
      with Sys_error _ as e ->
        Format.printf "Cannot open file \"%s\": %s\n" Sys.argv.(1) (Printexc.to_string e)


		
	(*	  print_int (Queue.length theCode); *)
	
end;;


main ()

