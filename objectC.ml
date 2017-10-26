open Symtbl
open Ast
(*open Name*)
open Pgmvalues
open Langtypes

(* open Lookup *)
(*open Cstrings *)


exception Error of string


module ObjectC = 
struct
(**
{b ObjectC module} 

This module provides a C implementation of the "object class" found in 
all mx programs.  All classes in mx extend the Object class.

*)


let object_static_code packContext = 


let cIncludeList = String.concat ""
            [ "#include <stdlib.h>\n#include <stdio.h>\nvoid* object_MethodTable;\n"; 
            "struct Object_Interface;\n";
            "typedef struct {\n";
            "\tvoid (*print)(struct Object_Interface*);\n";
            "} Object_Methods;\n\n"
            ] in
          Queue.add cIncludeList (Pgmvalues.get_c_code packContext);


let cObjIterList = String.concat ""
            [ "typedef struct Object_Interface {\n"; 
            "\tObject_Methods* methods;\n";
            "\tstruct Object_Interface* Object_cycle;\n";
            "} Object_Interface;\n\n"
            ] in
          Queue.add cObjIterList (Pgmvalues.get_c_code packContext);

(*
let cHeadInterList = String.concat ""
            [ "typedef struct HeadObject_Interface {\n"; 
            "\tObject_Methods* methods;\n";
            "\tstruct Object_Interface* Object_cycle;\n";
            "} HeadObject_Interface;\n\n"
            ] in
          Queue.add cHeadInterList (Pgmvalues.get_c_code packContext);

let cObjImplList = String.concat ""
            [ "typedef struct {                   // implementation *extends* Stack_Interface\n"; 
            "\tObject_Methods* methods;       // mixin methods, identifying type\n";
            "\tObject_Interface* Object_cycle; // *possibly* pointer to needed Object\n";
            "} Object_Impl;\n\n"
            ] in
          Queue.add cObjImplList (Pgmvalues.get_c_code packContext);

let cHeadImplList = String.concat ""
            [ "typedef struct {\n"; 
            "\tObject_Methods* methods;\n";
            "\tObject_Interface* Object_cycle; // *possibly* pointer to needed Object\n";
            "} HeadObject_Impl;\n\n"
            ] in
          Queue.add cHeadImplList (Pgmvalues.get_c_code packContext);
*)

let cObjectPrt = String.concat ""
            [ "void Object_print(Object_Interface* self) {\n"; 
            "\tprintf(\"%p\", self);\n";
            "}\n\n"
            ] in
          Queue.add cObjectPrt (Pgmvalues.get_c_code packContext);


let cfindBottomObj  = String.concat ""
  [ "Object_Interface* findBottom(Object_Interface* p, void* t) {\n" ;
    "\t short type_found = 0;\n";
    "\t Object_Interface* q = p;\n";
    "\t Object_Interface* bottom = NULL;\n";
    "\t do {\n";
    "\t\t if (q->methods == t) type_found = 1;\n";
    "\t\t if (q->Object_cycle->methods == object_MethodTable) bottom = q;\n";
    "\t\t q = q->Object_cycle;\n";
    "}\t\t while(p != q);\n";
    "\tif (type_found) return bottom;\n";
    "\treturn 0;\n";   
    "}\n\n"
  ] in
  Queue.add cfindBottomObj (Pgmvalues.get_c_code packContext);  


let cCastObj = String.concat ""
            [ "void* castObject(void* m, void* t) {\n";
            "\t // not done yet\n"; 
            "\tObject_Interface* temp = ((Object_Interface*) m)->Object_cycle;\n\n";
            "\twhile ((temp->methods != object_MethodTable) && (temp->methods != t))\n";
            "\t\ttemp = temp->Object_cycle;\n\n";
            "\tif ((temp->methods == object_MethodTable) && (t != object_MethodTable)) {\n";
            "\t\tprintf(\"exception\");\n";
            "\t\treturn NULL;\n";
            "\t}\n";
            "\treturn temp;\n";
            "} \n\n"
            ] in
          Queue.add cCastObj (Pgmvalues.get_c_code packContext);


let cInitObj = String.concat ""
            [ "void Object_init() {\n\n"; 
            "\tobject_MethodTable = malloc(sizeof(Object_Methods));\n\n";
            "\t((Object_Methods*) object_MethodTable)->print = &Object_print;\n";
            "} \n\n"
            ] in
          Queue.add cInitObj (Pgmvalues.get_c_code packContext);

(*
let cCastObj = String.concat ""
            [ "void* castObject(void* m, void* t) {\n"; 
            "\tObject_Interface* temp = ((Object_Interface*) m)->Object_self;\n\n";
            "\twhile ((temp->methods != object_Methods) && (temp->methods != t))\n";
            "\t\ttemp = temp->Object_super;\n\n";
            "\tif ((temp->methods == object_Methods) && (t != object_Methods)) {\n";
            "\t\tprintf(\"exception\");\n";
            "\t\treturn NULL;\n";
            "\t}\n";
            "\treturn temp;\n";
            "} \n\n"
            ] in
          Queue.add cCastObj (Pgmvalues.get_c_code packContext);
*)

let cObjNew = String.concat ""
            [ "Object_Interface* Object_new() {\n"; 
            "\tObject_Interface* x = (Object_Interface *) malloc(sizeof(Object_Interface));\n";
            "\tx->methods = (Object_Methods*) object_MethodTable;\n";
            "\tx->Object_cycle = (Object_Interface*) x\n;";
            "\treturn (Object_Interface*) x;\n";
            "} \n\n"
            ] in
          Queue.add cObjNew (Pgmvalues.get_c_code packContext);



end;;
