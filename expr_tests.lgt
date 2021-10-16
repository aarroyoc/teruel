:- use_module(expr).

:- object(expr, extends(lgtunit)).

    test(var) :- expr:eval_expr("username", ["username"-"aarroyoc"], "aarroyoc").
    test(number) :- expr:eval_expr("42", [], "42").
    test(string) :- expr:eval_expr("\"foo\"", [], "foo").
    test(bool) :- expr:eval_expr("true", [], "true").
    test(sum_2) :- expr:eval_expr("1 + 2", [], "3").
    test(sum_3) :- expr:eval_expr("1 + 2 + 3", [], "6").
    test(sum_minus) :- expr:eval_expr("1 + 2 - 3", [], "0").
    test(mul) :- expr:eval_expr("1 + 3 * 3 + 1", [], "11").
    test(div) :- expr:eval_expr("1 - 2 / 2", [], "0").
    test(equal) :- expr:eval_expr("1 + 2 == 3", [], "true").
    test(not_equal) :- expr:eval_expr("3 + 1 != 2 * 2", [], "false").
    test(and) :- expr:eval_expr("5 > 4 and 4 > 3", [], "true").
    test(or) :- expr:eval_expr("5 >= 4 or 3 < 1", [], "true").
    test(not) :- expr:eval_expr("not false or true", [], "false").

:- end_object.