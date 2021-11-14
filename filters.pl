:- module(filters, [
    filter_lower/2,
    filter_upper/2,
    filter_length/2,
    filter_wordcount/2,
    filter_capitalize/2,
    filter_trim/2,
    filter_trim_start/2,
    filter_trim_end/2,
    filter_truncate/3,
    filter_first/2,
    filter_last/2,
    filter_nth/3,
    filter_replace/3,
    filter_title/2,
    filter_join/3,
    filter_reverse/2,
    filter_sort/2,
    filter_sort/3,
    filter_unique/2,
    filter_floor/2,
    filter_ceiling/2
]).

:- use_module(library(dcgs)).
:- use_module(library(lists)).
:- use_module(library(pairs)).

% TODO: https://github.com/mthom/scryer-prolog/issues/748
filter_lower(In, Out) :-
    maplist(char_lower, In, Out).

char_lower(Char, Lower) :-
    char_code(Char, Code),
    ((Code >= 65,Code =< 90) ->
        LowerCode is Code + 32,
        char_code(Lower, LowerCode)
    ;   Char = Lower).

filter_upper(In, Out) :-
    maplist(char_upper, In, Out).

char_upper(Char, Upper) :-
    char_code(Char, Code),
    ((Code >= 97,Code =< 122) ->
        UpperCode is Code - 32,
        char_code(Upper, UpperCode)
    ;   Char = Upper).

filter_length(In, Out) :-
    length(In, N),
    number_chars(N, Out).

filter_wordcount(In, Out) :-
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

filter_capitalize([First|Rest], [UpperFirst|LowerRest]) :-
    char_upper(First, UpperFirst),
    filter_lower(Rest, LowerRest).

filter_trim(In, Out) :-
    filter_trim_start(In, S),
    filter_trim_end(S, Out).

filter_trim_start([' '|In], Out) :-
    filter_trim_start(In, Out).
filter_trim_start([X|In], [X|In]) :-
    X \= ' '.

filter_trim_end(In, Out) :-
    reverse(In, RIn),
    filter_trim_start(RIn, ROut),
    reverse(ROut, Out).

filter_truncate(In, Out, Args) :-
    member("length"-number(NString), Args),
    number_chars(N, NString),
    append(Out, _, In),
    length(Out, N).

filter_first([Out|_], Out).

filter_last([X], X).
filter_last([_|Xs], Out) :-
    filter_last(Xs, Out).

filter_nth(In, Out, Args) :-
    member("n"-number(NString), Args),
    number_chars(N, NString),
    nth0(N, In, Out).

filter_replace(In, Out, Args) :-
    member("from"-string(From), Args),
    member("to"-string(To), Args),
    once(phrase(replace_(From, To, Out), In)).
    

replace_(From, To, Out) -->
    string_(X),
    string_(From),
    {
        \+ append(From, _, X),
        append(X, To, Section),
        append(Section, Xs, Out)
    },
    replace_(From, To, Xs).

replace_(From, _, X) -->
    string_(X),
    {
        \+ append(From, _, X)
    }.

filter_title(In, Out) :-
    once(phrase(wordsplit_(Words), In)),
    maplist(filter_capitalize, Words, TitleWords),
    once(phrase(join_(" ", TitleWords), Out)).

join_(Joiner, [X|Xs]) -->
    string_(X),
    string_(Joiner),
    join_(Joiner, Xs).
join_(_, [X]) -->
    string_(X).

filter_join(In, Out, Args) :-
    member("sep"-string(Joiner), Args),
    once(phrase(join_(Joiner, In), Out)).

filter_reverse(In, Out) :-
    reverse(In, Out).

filter_sort(In, Out) :-
    sort(In, Out).

filter_sort(In, Out, Args) :-
    member("key"-string(Key), Args),
    map_list_to_pairs(get_key(Key), In, Pairs),
    keysort(Pairs, Sorted),
    pairs_values(Sorted, Out).

get_key(Key, Item, Value) :-
    member(Key-Value, Item).
    

filter_unique(In, Out) :-
    list_to_set(In, Out).

filter_floor(In, Out) :-
    Out is floor(In).

filter_ceiling(In, Out) :-
    Out is ceiling(In).

string_([X|Xs]) -->
    [X],
    string_(Xs).

string_([]) -->
    [].
