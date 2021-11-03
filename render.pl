:- module(render, [render_tree/3]).

:- use_module(library(lists)).
:- use_module(library(dcgs)).

:- use_module(expr).
:- use_module(filters).

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

render_tree(node(filter(FilterExpr, X), Node), Vars, Output) :-
    render_tree(X, Vars, Output0),
    render_tree(Node, Vars, Output1),
    once(phrase(filter_(FilterName, FilterArgs), FilterExpr)),
    append("filter_", FilterName, FilterNameComplete),
    atom_chars(FilterAtom, FilterNameComplete),
    (FilterArgs = [] ->
        call(FilterAtom, Output0, OutputFiltered)
    ;   call(FilterAtom, Output0, OutputFiltered, FilterArgs)
    ),
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

render_tree(node(extends(X, Blocks)), Vars, Output) :-
    maplist(prefix_blocks, Blocks, VarBlocks),
    append(Vars, VarBlocks, VarsExtends),
    render_tree(X, VarsExtends, Output).

render_tree(node(block(Name, X), Xs), Vars, Output) :-
    append("__block__", Name, VarBlockName),
    findall(Block, member(VarBlockName-Block, Vars), BlocksR),
    reverse(BlocksR, Blocks),
    render_block([X|Blocks], Vars, Output0),
    render_tree(Xs, Vars, Output1),
    append(Output0, Output1, Output).

render_tree([], _, []).

render_block([Block], Vars, Output) :-
    render_tree(Block, Vars, Output).
render_block([Block|Blocks], Vars, Output) :-
    render_tree(Block, Vars, Super),
    render_block(Blocks, ["super"-Super|Vars], Output).
    

render_for(LocalVar, LocalNode, Vars, ListValue, Block) :-
    append(Vars, [LocalVar-ListValue], LocalVars),
    render_tree(LocalNode, LocalVars, Block).

prefix_blocks(BlockName-Block, VarBlockName-Block) :-
    append("__block__", BlockName, VarBlockName).
