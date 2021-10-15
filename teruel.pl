:- module(teruel, [render/3]).

:- use_module(library(dcgs)).
:- use_module(library(pio)).
:- use_module(library(lists)).

:- use_module(parser).
:- use_module(render).

render(Input, Vars, Output) :-
    phrase_from_file(parser(Tree), Input),!,
    render_tree(Tree, Vars, Output).
