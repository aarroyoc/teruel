:- module(expr, [eval_expr/3, filter_//2]).

:- use_module(library(dcgs)).
:- use_module(library(charsio)).
:- use_module(library(lists)).

:- use_module(filters).

% TODO: Concat
% TODO: Lists
% TODO: Builtins
% TODO: Docs
% TODO: In
% TODO: CI
% TODO: Birthday game
% TODO: HTML Scaping

eval_expr(ExprString, Vars, ExprValue) :-
    once(phrase(expr(ExprTree), ExprString)),
    eval(ExprTree, Vars, ExprOutValue),
    ( number(ExprOutValue) ->
      number_chars(ExprOutValue, ExprValue)
    ; ExprOutValue = true ->
        ExprValue = "true"
    ; ExprOutValue = false ->
        ExprValue = "false"
    ; ExprOutValue = ExprValue
    ).

expr(filter(FilterName, X)) -->
    raw_string_(X),
    " | ",
    raw_string_(FilterName).

expr(X) -->
    logic_expr(X).

logic_expr(X) -->
    bool_expr(X).

logic_expr(and(X, Y)) -->
    bool_expr(X),
    " and ",
    logic_expr(Y).

logic_expr(or(X, Y)) -->
    bool_expr(X),
    " or ",
    logic_expr(Y).

logic_expr(not(X)) -->
    "not ",
    logic_expr(X).

bool_expr(X) -->
    sum_expr(X).

bool_expr(eq(X, Y)) -->
    sum_expr(X),
    " == ",
    sum_expr(Y).

bool_expr(ne(X, Y)) -->
    sum_expr(X),
    " != ",
    sum_expr(Y).

bool_expr(lt(X, Y)) -->
    sum_expr(X),
    " < ",
    sum_expr(Y).

bool_expr(gt(X, Y)) -->
    sum_expr(X),
    " > ",
    sum_expr(Y).

bool_expr(le(X, Y)) -->
    sum_expr(X),
    " <= ",
    sum_expr(Y).

bool_expr(ge(X, Y)) -->
    sum_expr(X),
    " >= ",
    sum_expr(Y).

sum_expr(X) -->
    mul_expr(X).

sum_expr(sum(X, Y)) -->
    mul_expr(X),
    " + ",
    sum_expr(Y).

sum_expr(minus(X, Y)) -->
    mul_expr(X),
    " - ",
    sum_expr(Y).

mul_expr(X) -->
    data_expr(X).

mul_expr(mul(X, Y)) -->
    data_expr(X),
    " * ",
    mul_expr(Y).

mul_expr(div(X, Y)) -->
    data_expr(X),
    " / ",
    mul_expr(Y).

mul_expr(mod(X, Y)) -->
    data_expr(X),
    " % ",
    mul_expr(Y).


data_expr(string(X)) -->
    "\"",
    raw_string_(X),
    "\"".

data_expr(number(X)) -->
    number_(X).

data_expr(bool(true)) -->
    "true".

data_expr(bool(false)) -->
    "false".

data_expr(par(X)) -->
    "(",
    raw_string_(X),
    ")".

data_expr(var(X)) -->
    var_string_start_(X).

eval(filter(FilterExpr, X), Vars, Value) :-
    once(phrase(expr(Tree), X)),
    eval(Tree, Vars, Value0),
    once(phrase(filter_(FilterName, FilterArgs), FilterExpr)),
    append("filter_", FilterName, FilterNameComplete),
    atom_chars(FilterAtom, FilterNameComplete),
    (FilterArgs = [] ->
        call(FilterAtom, Value0, Value)
    ;   call(FilterAtom, Value0, Value, FilterArgs)
    ).
    

eval(and(X, Y), Vars, Value) :-
    eval(X, Vars, XValue),
    ( XValue = true ->
      eval(Y, Vars, Value)
    ; Value = false
    ).

eval(or(X, Y), Vars, Value) :-
    eval(X, Vars, XValue),
    ( XValue = false ->
      eval(Y, Vars, Value)
    ; Value = true
    ).

eval(not(X), Vars, Value) :-
    eval(X, Vars, XValue),
    ( XValue = false ->
      Value = true
    ; Value = false
    ).

eval(eq(X, Y), Vars, Value) :-
    eval(X, Vars, XValue),
    eval(Y, Vars, YValue),
    (XValue = YValue ->
      Value = true
    ; Value = false
    ).

eval(ne(X, Y), Vars, Value) :-
    eval(X, Vars, XValue),
    eval(Y, Vars, YValue),
    (XValue = YValue ->
      Value = false
    ; Value = true
    ).

eval(lt(X, Y), Vars, Value) :-
    eval(X, Vars, XValue),
    eval(Y, Vars, YValue),
    (XValue < YValue ->
      Value = true
    ; Value = false
    ).

eval(le(X, Y), Vars, Value) :-
    eval(X, Vars, XValue),
    eval(Y, Vars, YValue),
    (XValue =< YValue ->
      Value = true
    ; Value = false
    ).

eval(gt(X, Y), Vars, Value) :-
    eval(le(Y, X), Vars, Value).

eval(ge(X, Y), Vars, Value) :-
    eval(lt(Y, X), Vars, Value).


eval(mul(X, Y), Vars, Value) :-
    eval(X, Vars, XValue),
    eval(Y, Vars, YValue),
    Value is XValue * YValue.

eval(div(X, Y), Vars, Value) :-
    eval(X, Vars, XValue),
    eval(Y, Vars, YValue),
    Value is XValue / YValue.

eval(mod(X, Y), Vars, Value) :-
    eval(X, Vars, XValue),
    eval(Y, Vars, YValue),
    Value is XValue mod YValue.

eval(sum(X, Y), Vars, Value) :-
    eval(X, Vars, XValue),
    eval(Y, Vars, YValue),
    Value is XValue + YValue.

eval(minus(X, Y), Vars, Value) :-
    eval(X, Vars, XValue),
    eval(Y, Vars, YValue),
    Value is XValue - YValue.

eval(number(X), _, N) :-
    number_chars(N, X).
eval(string(X), _, X).
eval(bool(X), _, X).
eval(var(X), Vars, Value) :-
    phrase(var_dict_get(Value, Vars, []), X).
eval(par(X), Vars, Value) :-
    once(phrase(expr(Tree), X)),
    eval(Tree, Vars, Value).

filter_(FilterName, FilterArgs) -->
    raw_string_(FilterName),
    "(",
    !,
    filter_args_(FilterArgs),
    ")".

filter_(FilterName, []) -->
    raw_string_(FilterName).

filter_args_([ArgName-ArgValue|FilterArgs]) -->
    raw_string_(ArgName),
    "=",
    data_expr(ArgValue),
    ", ",
    filter_args_(FilterArgs).

filter_args_([ArgName-ArgValue]) -->
    raw_string_(ArgName),
    "=",
    data_expr(ArgValue).

var_dict_get(Value, Vars, Key) -->
    [X],
    {
        X \= '.',
        append(Key, [X], NewKey)
    },
    var_dict_get(Value, Vars, NewKey).

var_dict_get(Value, Vars, Key) -->
    ".",
    {
        member(Key-NewVars, Vars)
    },
    var_dict_get(Value, NewVars, []).

var_dict_get(Value, Vars, Key) -->
    [],
    {
        member(Key-Value, Vars)
    }.

var_string_start_([C|X]) -->
    [C],
    {
        char_type(C, alphabetic)
    },
    var_string_(X).

var_string_([C|X]) -->
    [C],
    {
        char_type(C, alnum)
    },
    var_string_(X).

var_string_(['.'|X]) -->
    ".",
    var_string_(X).

var_string_([]) -->
    [].

raw_string_([X|Xs]) -->
    [X],
    raw_string_(Xs).

raw_string_([]) -->
    [].

number_([D|Ds]) --> digit(D), number_(Ds).
number_([D])    --> digit(D).

digit(D) --> [D], { char_type(D, decimal_digit) }.
digit(D) --> [D], { D = '.'}.
