:- use_module(filters).

:- object(filters, extends(lgtunit)).

    test(lower, true(Result == "abc")) :-
        filters:filter_lower("AbC", Result).

    test(upper, true(Result == "ABC")) :-
        filters:filter_upper("aBc", Result).

    test(length, true(Result == "5")) :-
        filters:filter_length("abc  ", Result).

    test(wordcount, true(Result == "4")) :-
        filters:filter_wordcount("  hola  amigos de   Perú", Result).

    test(capitalize, true(Result == "Abc")) :-
        filters:filter_capitalize("abc", Result).

    test(trim, true(Result == "hola amigos")) :-
        filters:filter_trim("     hola amigos  ", Result).

    test(trim_start, true(Result == "hola amigos  ")) :-
        filters:filter_trim_start("     hola amigos  ", Result).

    test(trim_end, true(Result == "     hola amigos")) :-
        filters:filter_trim_end("     hola amigos  ", Result).

    test(truncate, true(Result == "hola")) :-
        filters:filter_truncate("hola amigos", Result, ["length"-number("4")]).

    test(first, true(Result == 1)) :-
        filters:filter_first([1,2,3], Result).

    test(last, true(Result == 3)) :-
        filters:filter_last([1,2,3], Result).

    test(nth, true(Result == 2)) :-
        filters:filter_nth([1,2,3], Result, ["n"-number("1")]).

    test(replace, true(Result == "adios amigos")) :-
        filters:filter_replace("hola amigos", Result, ["from"-string("hola"), "to"-string("adios")]).

    test(title, true(Result == "Hola Amigos Cruel")) :-
        filters:filter_title("hola amigos  cruel", Result).

    test(join, true(Result == "hola//amigos//cruel")) :-
        filters:filter_join(["hola", "amigos", "cruel"], Result, ["sep"-string("//")]).

    test(reverse, true(Result == "sogima aloh")) :-
        filters:filter_reverse("hola amigos", Result).

    test(simple_sort, true(Result == [1,2,3,4])) :-
        filters:filter_sort([4,2,1,3], Result).

    test(complex_sort, true(Result == [["age"-21, "name"-"Marisa"], ["age"-34, "name"-"Pepe"], ["age"-56, "name"-"César"]])) :-
        filters:filter_sort([["age"-34, "name"-"Pepe"], ["age"-21, "name"-"Marisa"], ["age"-56, "name"-"César"]], Result, ["key"-string("age")]).

    test(unique, true(Result == [1,2,3])) :-
        filters:filter_unique([1,2,3,1,2,3], Result).

:- end_object.