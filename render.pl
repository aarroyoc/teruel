:- module(render, [render_tree/3]).

:- use_module(library(lists)).

:- use_module(expr).

render_tree(node(text(X), Node), Vars, Output) :-
    render_tree(Node, Vars, Output1),
    append(X, Output1, Output).

render_tree(node(expr(ExprString), Node), Vars, Output) :-
    eval_expr(ExprString, Vars, ExprValue),
    render_tree(Node, Vars, Output1),
    append(ExprValue, Output1, Output).

render_tree(node(raw(X), Node), Vars, Output) :-
    render_tree(Node, Vars, Output1),
    append(X, Output1, Output).

render_tree([], _, []).