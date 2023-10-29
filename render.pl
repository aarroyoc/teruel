:- module(render, [render_tree/3]).

:- use_module(library(lists)).
:- use_module(library(dcgs)).

:- use_module(expr).
:- use_module(filters).

render_tree(Tree, Vars, Output) :-
    maplist(render_node(Vars), Tree, Outputs),
    append(Outputs, Output).

render_node(_Vars, text(X), X).

render_node(Vars, expr(ExprString), Output) :-
    eval_expr(ExprString, Vars, Output).

render_node(_Vars, raw(X), X).

render_node(Vars, filter(FilterExpr, X), Output) :-
    render_tree(X, Vars, Output0),
    once(phrase(filter_(FilterName, FilterArgs), FilterExpr)),
    append("filter_", FilterName, FilterNameComplete),
    atom_chars(FilterAtom, FilterNameComplete),
    (FilterArgs = [] ->
	 call(FilterAtom, Output0, Output)
    ;    call(FilterAtom, Output0, Output, FilterArgs)
    ).

render_node(Vars, if(Expr, X, Y), Output) :-
    eval_expr(Expr, Vars, Result),
    ( Result = "true" ->
      render_tree(X, Vars, Output)
    ; render_tree(Y, Vars, Output)
    ).

render_node(Vars, for(LocalVar, ListExpr, X), Output) :-
    eval_expr(ListExpr, Vars, ListValues),
    maplist(render_for(LocalVar, X, Vars), ListValues, SubOutputs),
    append(SubOutputs, Output).

render_node(Vars, include(X), Output) :-
    render_tree(X, Vars, Output).

render_node(Vars, extends(X, Blocks), Output) :-
    maplist(prefix_blocks, Blocks, VarBlocks),
    append(Vars, VarBlocks, VarsExtends),
    render_tree(X, VarsExtends, Output).

render_node(Vars, block(Name, X), Output) :-
    append("__block__", Name, VarBlockName),
    findall(Block, member(VarBlockName-Block, Vars), BlocksR),
    reverse(BlocksR, Blocks),
    render_block([X|Blocks], Vars, Output).

render_block([Block], Vars, Output) :-
    render_tree(Block, Vars, Output).
render_block([Block|Blocks], Vars, Output) :-
    render_tree(Block, Vars, Super),
    render_block(Blocks, ["super"-Super|Vars], Output).
    

render_for(LocalVar, LocalNode, Vars, ListValue, Output) :-
    append(Vars, [LocalVar-ListValue], LocalVars),
    render_tree(LocalNode, LocalVars, Output).

prefix_blocks(BlockName-Block, VarBlockName-Block) :-
    append("__block__", BlockName, VarBlockName).
