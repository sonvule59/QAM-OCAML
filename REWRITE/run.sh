#!/bin/bash

# Step 1: Generate lexer.ml from lexer.mll
ocamllex lexer.mll

# Step 2: Generate parser.ml and parser.mli using Menhir
menhir --ocamlc "ocamlc -I +menhir" --explain parser.mly

# Step 3: Compile the files in the correct order
ocamlc -c ast.ml
ocamlc -c parser.mli
ocamlc -c parser.ml
ocamlc -c lexer.ml
ocamlc -o interpreter ast.cmo parser.cmo lexer.cmo interpreter.ml

# Step 4: Run the interpreter
./interpreter
