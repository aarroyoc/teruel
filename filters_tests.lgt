:- use_module(filters).

:- object(filters, extends(lgtunit)).

    test(lower) :- filters:lower("AbC", "abc").
    test(upper) :- filters:upper("aBc", "ABC").
    test(charcount) :- filters:charcount("abc  ", "5").
    test(wordcount) :- filters:wordcount("  hola  amigos de   Perú", "4").
    test(capitalize) :- filters:capitalize("abc", "Abc").
    test(trim) :- filters:trim("     hola amigos  ", "hola amigos").
    test(trim_start) :- filters:trim_start("     hola amigos  ", "hola amigos  ").
    test(trim_end) :- filters:trim_end("     hola amigos  ", "     hola amigos").
    test(truncate) :- filters:truncate("hola amigos", "hola", ["length"-"4"]).
    test(first) :- filters:first([1,2,3], 1).
    test(last) :- filters:last([1,2,3], 3).
    test(nth) :- filters:nth([1,2,3], 2, ["n"-"1"]).

:- end_object.