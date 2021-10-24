:- use_module(filters).

:- object(filters, extends(lgtunit)).

    test(lower) :- filters:lower("AbC", "abc").
    test(upper) :- filters:upper("aBc", "ABC").
    test(charcount) :- filters:charcount("abc  ", "5").
    test(capitalize) :- filters:capitalize("abc", "Abc").

:- end_object.