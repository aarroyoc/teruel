:- module(teruel, [render/3, html_render_response/2, html_render_response/3]).

:- use_module(library(dcgs)).
:- use_module(library(pio)).
:- use_module(library(lists)).
:- use_module(library(files)).

:- use_module(parser).
:- use_module(render).

render(Input, Vars, Output) :-
    canonical_dir(Input, FolderPath),
    once(phrase_from_file(parser(Tree, FolderPath), Input)),
    render_tree(Tree, Vars, Output).

html_render_response(Response, Input) :-
    html_render_response(Response, Input, []).

html_render_response(http_response(_, html(Output), _), Input, Vars) :-
    render(Input, Vars, Output).
