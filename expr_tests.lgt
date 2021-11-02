:- use_module(expr).

:- object(expr, extends(lgtunit)).

    test(var) :- expr:eval_expr("username", ["username"-"aarroyoc"], "aarroyoc").
    test(dict_var) :- expr:eval_expr("user.login", ["user"-["login"-"aarroyoc", "password"-"123456"]], "aarroyoc").
    test(number) :- expr:eval_expr("42", [], "42").
    test(string) :- expr:eval_expr("\"foo\"", [], "foo").
    test(bool) :- expr:eval_expr("true", [], "true").
    test(sum_2) :- expr:eval_expr("1 + 2", [], "3").
    test(sum_3) :- expr:eval_expr("1 + 2 + 3", [], "6").
    test(sum_minus) :- expr:eval_expr("1 + 2 - 3", [], "0").
    test(mul) :- expr:eval_expr("1 + 3 * 3 + 1", [], "11").
    test(mul_par) :- expr:eval_expr("(1 + 3) * 3 + 1", [], "13").
    test(div) :- expr:eval_expr("1 - 2 / 2", [], "0").
    test(equal) :- expr:eval_expr("1 + 2 == 3", [], "true").
    test(not_equal) :- expr:eval_expr("3 + 1 != 2 * 2", [], "false").
    test(and) :- expr:eval_expr("5 > 4 and 4 > 3", [], "true").
    test(or) :- expr:eval_expr("5 >= 4 or 3 < 1", [], "true").
    test(not) :- expr:eval_expr("not false or true", [], "false").
    test(filter) :- expr:eval_expr("username | lower", ["username"-"AARROYOC"], "aarroyoc").
    test(multi_filter) :- expr:eval_expr("user.login | lower | charcount", ["user"-["login"-"aarroyoc", "password"-"123456"]], "8").
    test(filter_param) :- expr:eval_expr("user.login | lower | truncate(length=4)", ["user"-["login"-"aarroyoc", "password"-"123456"]], "aarr").
    test(nth) :- expr:eval_expr("links | nth(n=1)", ["links"-["https://github.com", "https://adrianistan.eu"]], "https://adrianistan.eu").

:- end_object.