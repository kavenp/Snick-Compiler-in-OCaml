
With this README file you should also have found these:

Makefile:  
    A makefile for the COMP90045 project 2017

Makefile.depend:
    A listing of the file dependencies

snack.ml:
    The main module

snack_ast.ml:
    The data structures that make up the (currently limited) AST

snack_ast.mli:
    The interface file for snack_ast.ml

snack_lex.mll:
    An ocamllex specification for snack

snack_parse.mly:
    An ocamlyacc specification for snack

snack_pprint.ml:
    A pretty-printer - well, not really; for now this is just a stub

snack_pprint.mli:
    The interface file for snack_pprint.ml

To get started, study these files, in particular snack_ast.ml,
snack_lex.mll, and snack_parse.mly.  On a Unix machine you should 
be able to just type

    make

and that should generate some files for you, including the executable
snack. 

Write a small snack program, like this:

int tove;
read tove;
tove := 2*tove + 42;
write tove;

Say this program is in file mung.bean; now you should be able to run

    snack -p tove.snack

and something will happen (actually nothing very interesting, since
there is no real pretty-printer yet).  But at least you should not 
get error messages.

If your Unix system doesn't seem to recognise `snack', that
could be because your PATH variable hasn't been set correctly.
For now, just try instead

    ./snack -p tove.snack

