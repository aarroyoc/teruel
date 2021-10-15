:- use_module(expr).

:- object(expr, extends(lgtunit)).

    test(number) :- expr:eval_expr("42", [], "42").
    test(string) :- expr:eval_expr("\"foo\"", [], "foo").
    test(bool) :- expr:eval_expr("true", [], "true").
    test(sum_2) :- expr:eval_expr("1 + 2", [], "3").
    test(sum_3) :- expr:eval_expr("1 + 2 + 3", [], "6").
    test(sum_minus) :- expr:eval_expr("1 + 2 - 3", [], "0").

:- end_object.