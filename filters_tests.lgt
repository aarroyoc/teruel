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

:- end_object.