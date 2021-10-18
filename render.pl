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

render_tree(node(filter(FilterName, X), Node), Vars, Output) :-
    render_tree(X, Vars, Output0),
    render_tree(Node, Vars, Output1),
    atom_chars(FilterAtom, FilterName),
    call(FilterAtom, Output0, OutputFiltered),
    append(OutputFiltered, Output1, Output).

render_tree(node(if(Expr, X), Node), Vars, Output) :-
    eval_expr(Expr, Vars, Result),
    render_tree(Node, Vars, Output1),
    ( Result = "true" ->
      (
          render_tree(X, Vars, Output0),
          append(Output0, Output1, Output)     
      )
    ; Output1 = Output
    ).

render_tree(node(if_else(Expr, X, Y), Node), Vars, Output) :-
    eval_expr(Expr, Vars, Result),
    render_tree(Node, Vars, Output1),
    ( Result = "true" ->
        render_tree(X, Vars, Output0)
    ;   render_tree(Y, Vars, Output0)
    ),
    append(Output0, Output1, Output).

render_tree(node(for(LocalVar, ListVar, X), Node), Vars, Output) :-
    member(ListVar-ListValues, Vars),
    maplist(render_for(LocalVar, X, Vars), ListValues, Blocks),
    reverse(Blocks, BlocksReversed),
    foldl(append, BlocksReversed, [], Output0),
    render_tree(Node, Vars, Output1),
    append(Output0, Output1, Output).

render_tree(node(include(X), Xs), Vars, Output) :-
    render_tree(X, Vars, Output0),
    render_tree(Xs, Vars, Output1),
    append(Output0, Output1, Output).

render_tree([], _, []).

render_for(LocalVar, LocalNode, Vars, ListValue, Block) :-
    append(Vars, [LocalVar-ListValue], LocalVars),
    render_tree(LocalNode, LocalVars, Block).

% filters

lower(In, Out) :-
    maplist(char_lower, In, Out).

char_lower(Char, Lower) :-
    char_code(Char, Code),
    ((Code >= 65,Code =< 90) ->
        LowerCode is Code + 32,
        char_code(Lower, LowerCode)
    ;   Char = Lower).

count(In, Out) :-
    length(In, N),
    number_chars(N, Out).