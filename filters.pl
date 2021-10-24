:- module(filters, [
    lower/2,
    upper/2,
    charcount/2,
    capitalize/2
]).

:- use_module(library(lists)).

lower(In, Out) :-
    maplist(char_lower, In, Out).

char_lower(Char, Lower) :-
    char_code(Char, Code),
    ((Code >= 65,Code =< 90) ->
        LowerCode is Code + 32,
        char_code(Lower, LowerCode)
    ;   Char = Lower).

upper(In, Out) :-
    maplist(char_upper, In, Out).

char_upper(Char, Upper) :-
    char_code(Char, Code),
    ((Code >= 97,Code =< 122) ->
        UpperCode is Code - 32,
        char_code(Upper, UpperCode)
    ;   Char = Upper).

charcount(In, Out) :-
    length(In, N),
    number_chars(N, Out).

capitalize([First|Rest], [UpperFirst|LowerRest]) :-
    char_upper(First, UpperFirst),
    lower(Rest, LowerRest).