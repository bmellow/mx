# mx
Prototype compiler build for a dynamic mixin-based language

This language was created by **Eden Burton** and **Emil Sekerinski** at the [Computing and Software Department of McMaster University](https://www.eng.mcmaster.ca/cas). Our goal was to create statically-typed language that supports dynamic mixins.

The code contained in this repository is an prototype implementation of a compiler for the language.  

The compiler implementation is written in OCaml, a functional language with support for object-oriented development. Here we provide a summary of the files created for the project.

# Project dependencies

- *ocaml* - (version 4.02.1)
- *camlp4* - (https://opam.ocaml.org/packages/camlp4/)


# Language definition modules 

- *token.ml* (defines tokens that are used by lexer)
- *ast.ml* (defines abstract syntax tree)

# Core compiler modules 

- *lexer.ml* (translates text input into tokens)
- *parser.ml* (translates tokens into an AST)
- *codegenC.ml* (generate C code from a given AST)

# Symbol table modules

- *symtbl.ml* (symbol table used to help with code generation based on context)
- *typesymtbl.ml* (symbol table to help with code generated based on needs relations between classes)


# Extra modules

- *astPrint.ml* (just for debugging purposes)
- *bindings.c* (allows support for calling c runtime library functions - depreciated)
- *langtypes.ml* (provides C keywords for AST defined types)
- *myocamlbuild.ml* (standard config file for building ocaml projects)
- *objectC.ml* (provide generated C code for Object class, present in all classes)
- *typecheck.ml* (in development … checks AST to ensure that typing rules are satisified in program)


# Scripts

- *buildMx* (build the mx compiler)
- *runMx* (script to compile mx program / generate code)
- *makeDocs* (make ocamldocs, interface documentation)
