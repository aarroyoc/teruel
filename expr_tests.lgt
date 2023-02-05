:- use_module(expr).

:- object(expr, extends(lgtunit)).

    test(var, true(Result == "aarroyoc")) :-
        expr:eval_expr("username", ["username"-"aarroyoc"], Result).

    test(dict_var, true(Result == "aarroyoc")) :-
        expr:eval_expr("user.login", ["user"-["login"-"aarroyoc", "password"-"123456"]], Result).

    test(number, true(Result == "42")) :-
        expr:eval_expr("42", [], Result).

    test(string, true(Result == "foo")) :-
        expr:eval_expr("\"foo\"", [], Result).

    test(bool, true(Result == "true")) :-
        expr:eval_expr("true", [], Result).

    test(sum_2, true(Result == "3")) :-
        expr:eval_expr("1 + 2", [], Result).

    test(sum_3, true(Result == "6")) :-
        expr:eval_expr("1 + 2 + 3", [], Result).

    test(sum_minus, true(Result == "0")) :-
        expr:eval_expr("1 + 2 - 3", [], Result).

    test(mul, true(Result == "11")) :-
        expr:eval_expr("1 + 3 * 3 + 1", [], Result).

    test(mul_par, true(Result == "13")) :-
        expr:eval_expr("(1 + 3) * 3 + 1", [], Result).

    test(sum_float, true(Result == "1.5")) :-
        expr:eval_expr("0.75 + 0.75", [], Result).

    test(div, true(Result == "0")) :-
        expr:eval_expr("1 - 2 / 2 | floor", [], Result).

    test(equal, true(Result == "true")) :-
        expr:eval_expr("1 + 2 == 3", [], Result).

    test(not_equal, true(Result == "false")) :-
        expr:eval_expr("3 + 1 != 2 * 2", [], Result).

    test(and, true(Result == "true")) :-
        expr:eval_expr("5 > 4 and 4 > 3", [], Result).

    test(or, true(Result == "true")) :-
        expr:eval_expr("5 >= 4 or 3 < 1", [], Result).

    test(not, true(Result == "false")) :-
        expr:eval_expr("not false or true", [], Result).

    test(filter, true(Result == "aarroyoc")) :-
        expr:eval_expr("username | lower", ["username"-"AARROYOC"], Result).

    test(multi_filter, true(Result == "8")) :-
        expr:eval_expr("user.login | lower | length", ["user"-["login"-"aarroyoc", "password"-"123456"]], Result).

    test(filter_param, true(Result == "aarr")) :-
        expr:eval_expr("user.login | lower | truncate(length=4)", ["user"-["login"-"aarroyoc", "password"-"123456"]], Result).

    test(nth, true(Result == "https://adrianistan.eu")) :-
        expr:eval_expr("links | nth(n=1)", ["links"-["https://github.com", "https://adrianistan.eu"]], Result).

    test(replace, true(Result == "aarropuc")) :-
        expr:eval_expr("username | replace(from=\"yo\", to=\"pu\")", ["username"-"aarroyoc"], Result).

:- end_object.
