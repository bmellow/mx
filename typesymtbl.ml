(**
    This module will create an populate a symbol table containing the definitions
        of classes and their attributes found in the AST.
    
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

exception TypeError of string


module Typesymtbl = 
struct


let get_const_list (cl, al, ml, cn) = cl
let get_attr_list  (cl, al, ml, cn) = al
let get_meth_list  (cl, al, ml, cn) = ml
let get_class_name (cl, al, ml, cn) = cn


(** This function takes a list for identifiers and a type
    and returns a list of tuples with the string identifers and 
    the passed in type.  This was created because the language
        we are accepting allow variable declarations of the form
        var x,y,z type. This function allows us to store such
        values in the form (x,type), (y,type), (z,type) in the symbol
        table.  Note that the LLVM types are placeholders at this point. 
        @param    list of Ast identifiers
        @param    Ast types
        @return   list with (string, type) tuples *)  
let make_var_list idL t clsName packContext = 
   List.map (fun a -> match a with
                      | Ast.Identifier(id) -> SymTbl.STClsAttr(id,0,t, clsName )) idL


let find_ast_cls_element clsName wholeAst = 
    match wholeAst with
    | Ast.Package (i, Ast.ClsList(ul)) ->
        List.find (fun a -> match a with
                        | Ast.Cls(Ast.Identifier(id), clsID, Ast.MemList(ml), nl, il, i) when (id=clsName) -> true
                        | _ -> false
                   ) ul

let make_formals_list fl methName clsName packContext= 
   (* check for duplicate parameters *)
   let idList = 
      List.map (fun a -> match a with
                      | (Ast.Identifier(id),t) -> id) fl in
     let dup = Pgmvalues.doesDupExist idList in
   match dup with
   | true -> let errStr = String.concat "" ["TypeError: Duplicate argument names in method: "; methName] in
                    raise (TypeError errStr)
   | false -> List.map (fun a -> match a with
                      | (Ast.Identifier(id),t) -> SymTbl.STClsAttr(id,0,t,clsName )) fl
                                                                                                        (* perhaps this should be really formal or something else  *)




(** This takes an ast class node and populates type symbol table with type information 
    relevant to that classlist and puts it information into the symbol table. 
        Populated table is returned.  Note that the llvm types are placeholders ???? not sure yet. 
        @param ast         abstract syntax tree
        @param symtbl      type symbol table to be populated
        
        @return            type symble table after population
*)
let typesymtbl_add_cls_member_list packlst ast =
    let lst = snd packlst in
    let packContext = fst packlst in 
  match ast with
        | Ast.MemConst(Ast.Identifier(id),t,vT,e) -> (packContext, ((SymTbl.STClsConst(id,t )) :: (get_const_list lst),  get_attr_list lst,  get_meth_list lst, get_class_name lst) )  
        | Ast.MemAttr(Ast.Variable(idL,t,vT)) ->     (packContext, (get_const_list lst, (make_var_list idL t  (get_class_name lst) packContext) @ (get_attr_list lst),get_meth_list lst,  get_class_name lst ))
        | Ast.MemMeth(Ast.Identifier(id),fl,t,c, i) ->  (packContext, (get_const_list lst,  get_attr_list lst, SymTbl.STClsMeth(id,t, (make_formals_list fl id (get_class_name lst) packContext)  ) :: get_meth_list lst,  get_class_name lst))



(** This takes an ast class list node and populates type symbol table with types
    (class definitions). Function iteratively goes through each class in the 
        list and puts it information into the symbol table. Populated table is returned
        @param ast         abstract syntax tree
        @param symtbl      type symbol table to be populated
        
        @return            type symble table after population
        *)
let typesymtbl_add_cls astAndGbl ast = 
    let packContext = snd astAndGbl in
  match ast with
  | Ast.Cls(Ast.Identifier(id), clsID, Ast.MemList(ml), nl, il, i) ->  
        let packLsts = List.fold_left typesymtbl_add_cls_member_list (packContext, ([],[],[],id)) ml in 
        let pack = fst packLsts in
        let lsts = snd packLsts in
                
                let tysymtbl =  Pgmvalues.get_typsymtbl pack in
                
                (* check for duplicate class names  *)
                try
                    let errStr = String.concat "" ["typesymtbl_add_cls: see if class "; id ; " is already defined"] in 
                    print_endline errStr;
                    SymTbl.sym_get_symtbl_val tysymtbl id; 
                    
                    (* normal behaviour is if class name doesn't currently exist *)
                    let errStr = String.concat "" ["TypeError: Duplicate class names: "; id] in
                    raise (TypeError errStr)

                with SymTblError "sym_find_symbl: symbol could not be found" ->
        begin  
          let errStr = String.concat "" ["typesymtbl_add_cls: Success. Class "; id ; " is not yet defined"] in 
             print_endline errStr;
                        
                    (* recurse the attributes to insert attribute indices to type symbol *)
                    (* entries.                                                                                                                  *)

                    let al = List.fold_left  
                                (fun i a -> 
                                        match a with
                                        | SymTbl.STClsAttr(id,idx,t,cN ) -> 
                                                let idxupd = SymTbl.STClsAttr(id,(fst i),t,cN ) in
                                                        ((fst i) - 1, idxupd :: (snd i)))
                        
                                (* first argument to function, "i" *)
                                ( (List.length (get_attr_list lsts))-1 ,[])
                                
                                (* list provide second arguement "a" *)
                                (get_attr_list lsts) in
(*                  
                    (* now create class type pointer using attribute information  *) 
                    let clsAttrList = List.map (fun a ->
                                                                match a with
                                                                |  SymTbl.STClsAttr(id,idx,t,cN ) -> 
                                                                     let logStr = String.concat "" ["typesymtbl_add_cls: type symbol table class info: class: ";cN; ", attr: ";id; " index: "] in 
                                                                print_string logStr; print_int idx; print_endline "";
                                                                    lt) 
                                            (snd al) in
                                            
                    let clsAttrArr = Array.of_list clsAttrList in

            (* define named type of class for program *)
            let nmTyp =  named_struct_type context id in
                    struct_set_body nmTyp clsAttrArr false;    
          
            (* constructs the class type, which is pointer to a stucture    *)
            (* containing the types of member variables of within the class *)
            let clsType = pointer_type nmTyp in                              
*)      
                SymTbl.sym_set_symtbl_val tysymtbl id 
                       (SymTbl.STClsTyp(Ast.TypClsPtr(Ast.Identifier(id)),   
                       get_const_list lsts, (snd al), get_meth_list lsts, 
                         (List.map (fun a ->       
                          match a with
                            | s -> Ast.TypClsPtr(Ast.Identifier(s))) nl),
             (List.map (fun a ->       
                          match a with
                            | s -> Ast.TypClsPtr(Ast.Identifier(s))) il)
                         
            
            
            
            ));
                                    
             Hashtbl.add (Pgmvalues.get_needsTable packContext) id nl;
                  astAndGbl
                end


(** Entry point and the only routine that should be called from outside of the module.
        (except for unit testing). It accepts an abstract syntax tree structure for a 
        particular package and decorates the symbol table containing types.  
    
    
    @param astAndGbl        a tuple that contains the AST (via the root AST.Package node
                                                and the global context object        
        @return         symtbl table with types added in *)
let typesymtbl_package astAndGbl = 

    let ast = fst astAndGbl in
    let packContext = snd astAndGbl in
    print_endline "";
    print_endline "begin executing type symbol generation process...";  
  match ast with
  | Ast.Package (i, Ast.ClsList(ul)) -> print_endline "typesymtbl_package: start"; 
                                            List.fold_left typesymtbl_add_cls astAndGbl ul;
                                            print_endline "typesymtbl_package: end";
                                            (ast, packContext)
  | Ast.Program (i, Ast.ClsList(ul), cs) -> print_endline "typesymtbl_program: start"; 
                                            List.fold_left typesymtbl_add_cls astAndGbl ul;
                                            print_endline "typesymtbl_program: end";
                                            (ast, packContext)




(** This function is used to print error messages associated
        with typecheck error
        @param testFunc                 function that returns a symbol table storing
                                                                types.  
                @param symtbl                       type symbol table.  The one is pass into the
                                                                test function.  It will be populated with
                                                                type information and will be basis of output
                                                                for the test function
                @param testSymtblEnt        The test symtbl entry that we deem to be correct 
                                                                for this particular test case       
        @return                 none, just side effect of printing a description of 
                                                                the test result *)
let typesymtbl_print_error testFunc symtbl testSymtblEnt = 
     try           
          begin     
      match testSymtblEnt with
            | SymTbl.STClsTyp(Ast.TypClsPtr(Ast.Identifier(id)), cl, al, ml, nl, il) ->             
                  begin
                  let resST = testFunc symtbl in
                  let clsEntry = SymTbl.sym_get_symtbl_val resST id in
                    match clsEntry with
                    | SymTbl.STClsTyp(Ast.TypClsPtr(Ast.Identifier(id1)), cl1, al1, ml1, nl1, il1) ->
                          if  (Pgmvalues.isListSame cl cl1 (List.length cl) ) &&
                  (Pgmvalues.isListSame al al1 (List.length al) ) then
                                print_endline "TypeSymtbl success: yes"
                            else
                                  let errStr = String.concat "" 
                   ["TypeSymtbl success for class: "; id ] in
                  print_endline errStr
                    | _ -> let errStr = String.concat "" 
                                    ["TypeSymtbl error: "; id ; " class symtbl entry not in the correct format"] in
                                    print_endline errStr
                  end
            | _ -> print_endline "TypeSymtbl error: test symtbl entry not in the correct format"
            end
     with TypeError descr -> let errStr = String.concat "" ["typesymtbl_print_error: TypeError xxxerror:" ; descr] in 
                                                                    print_endline errStr





(**
    For a given class name, find the class names that immediately (as opposed to
    transitively need it. For example given the following defined relationships,
    D needs C, E needs C , the call to childrenX C, would return {D, E}.
    
    @param c              string that represents given class
    @param packContext    global structure
*)
let childrenX c packContext = 
    (* print_endline (String.concat "" ["childrenX: enter "; c]); *)

    (* retrieve needs table *)      
    let needTbl = (Pgmvalues.get_needsTable packContext) in
    (* for each mapping in the table, see if "needed" element is equal to the *)
    (* passed in value. If so, add the related "needs" element to the accumulating set *)
    let cList = Hashtbl.fold 
      (fun k v cl ->
            try
              List.find (fun a ->  (* print_string "childrenX:";  print_endline a; *)
                                   match a with 
                                   | w when w=c ->  true
                                   | _ -> false
                                  ) v; (cl@[k])
                        with Not_found -> (* print_endline "childrenX: not found"; *) cl
      )
    needTbl [] in
    List.iter(fun a -> print_string (String.concat "" [","; a])) cList;
    print_endline "";
        cList
    
let rec decendents c packContext = 
  
   print_string "decendents for class:";  print_endline c;  
   (* get children *)
   let cl = childrenX c packContext in 
   (* for each child , recursively find its direct children *)
   let tl = List.map (fun a -> decendents a packContext) cl in
  
   (* concatenate list of immediate decendents, and their children *)
   (cl@(List.flatten tl)) 

(**
  Given a class name, returns a list of class names which require 
  pointer to be updated.



*)
let needset c packContext =
      print_string (String.concat "" ["needset for class:";c; "{"]); 
      try 
      let pl  = Hashtbl.find (Pgmvalues.get_needsTable packContext) c in
      let parent = List.hd pl in
      
      let childDesc = decendents c packContext in

      let parentDesc = decendents parent packContext in
       
      let cList = (List.filter (fun a -> not (List.mem a childDesc) ) parentDesc) in
        List.iter(fun a -> print_string (String.concat "" [","; a])) cList;
        print_endline "}";
        parent::cList
      with 
      | Not_found -> print_endline ""; []
      | Failure desc -> print_endline "";[]


let rec extension_chain_helper clsName exl tysymtbl = 
  try 
  let cls = SymTbl.sym_get_symtbl_val tysymtbl clsName in
  match cls with
  | SymTbl.STClsTyp( Ast.TypClsPtr(Ast.Identifier(id)), cl, al,ml, nl,il ) ->
      match (List.hd nl) with
      | Ast.TypClsPtr(Ast.Identifier(id)) ->  extension_chain_helper id (id::exl) tysymtbl 
      | _ -> exl 
  with 
  | SymTblError descr -> exl
  | Failure descr -> exl 


(** this method returns a set for classes that a given class needs
    for example, if C needs B, B needs A, the call extension_chain 
    C will return the set 
    {B,A}
*)
let extension_chain c packContext = 
  let tysymtbl = (Pgmvalues.get_typsymtbl packContext) in
  extension_chain_helper c [] tysymtbl
  

(** for a type symbol table level, find if any C implements C' *)
let implementsHelper c tstl = 
  (* get class definition entries to obtain "implements" set *) 
    Hashtbl.fold 
      (fun k v a -> 
      (* for each class entry in the type symbol table, get the list *)
      (* of classes that it implements  *)                             
         match v with
         | SymTbl.STClsTyp( Ast.TypClsPtr(Ast.Identifier(id)), cl, al,ml, nl,il ) -> 
              if (List.exists (fun a -> match a with
                                 | Ast.TypClsPtr(Ast.Identifier(id2)) when id2=c ->true
                                 | _ -> false
                 )) il then
                   begin
                    
                  (* print_endline (String.concat "" 
                      ["implementsHelper: "; k ;
                       "implements " ; c
                      ]); *)
                   k::a
                   end
              else
                   begin
                  (* print_endline (String.concat "" 
                      ["implementsHelper: "; k ;
                       "does not implement " ; c
                      ]); *)
                   a 
                   end
          | _ -> (* "implementsHelper: not a class";*) a 
      ) tstl [] 

(** for a given class c, return a list of classes that implements it *)
let implements c packContext = 
  
  let tysymtbl = (Pgmvalues.get_typsymtbl packContext) in
  
  (* The symbol table is a stack of a hashtables, call helper function *)
  (* that calls both *) 
  
  (* get first *)
  let ctySymtbl = Stack.copy tysymtbl in
  while (Stack.length ctySymtbl) > 1 do
    Stack.pop ctySymtbl
  done;
  
  implementsHelper c (Stack.top ctySymtbl)
  


end;;
