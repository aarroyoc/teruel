:- module(teruel, [render/3]).

:- use_module(library(dcgs)).
:- use_module(library(pio)).
:- use_module(library(lists)).
:- use_module(library(files)).

:- use_module(parser).
:- use_module(render).

render(InputCs, Vars, Output) :-
    atom_chars(Input, InputCs),
    canonical_dir(InputCs, FolderPath),
    once(phrase_from_file(parser(Tree, FolderPath), Input)),
    render_tree(Tree, Vars, Output).
