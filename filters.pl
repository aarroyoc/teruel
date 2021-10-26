:- module(filters, [
    lower/2,
    upper/2,
    charcount/2,
    wordcount/2,
    capitalize/2,
    trim/2,
    trim_start/2,
    trim_end/2,
    truncate/3
]).

:- use_module(library(dcgs)).
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

wordcount(In, Out) :-
    once(phrase(wordsplit_(Words), In)),
    length(Words, N),
    number_chars(N, Out).

wordsplit_(Words) -->
    " ",
    wordsplit_(Words).
wordsplit_([]) -->
    word_([]).
wordsplit_([Word|Words]) -->
    word_(Word),
    wordsplit_(Words).

word_([Char|Chars]) -->
    [Char],
    {
        \+ member(Char, " "),
        !
    },
    word_(Chars).
word_([]) --> [].

capitalize([First|Rest], [UpperFirst|LowerRest]) :-
    char_upper(First, UpperFirst),
    lower(Rest, LowerRest).

trim(In, Out) :-
    trim_start(In, S),
    trim_end(S, Out).

trim_start([' '|In], Out) :-
    trim_start(In, Out).
trim_start([X|In], [X|In]) :-
    X \= ' '.

trim_end(In, Out) :-
    reverse(In, RIn),
    trim_start(RIn, ROut),
    reverse(ROut, Out).

truncate(In, Out, Args) :-
    member("length"-Length, Args),
    number_chars(N, Length),
    append(Out, _, In),
    length(Out, N).