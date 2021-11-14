:- use_module('teruel').

:- object(tests, extends(lgtunit)).

    test(trivial, true) :-
        true.

    test(simple_substitution, true(Assertion)) :-
        template_test("templates/a.in.html", "templates/a.out.html", ["username"-"aarroyoc"], Assertion).

    test(raw_block, true(Assertion)) :-
        template_test("templates/b.in.html", "templates/b.out.html", ["username"-"aarroyoc"], Assertion).

    test(comment_block, true(Assertion)) :-
        template_test("templates/c.in.html", "templates/c.out.html", ["username"-"aarroyoc"], Assertion).

    test(for_loop, true(Assertion)) :-
        template_test("templates/h.in.html", "templates/h.out.html", ["username"-"aarroyoc", "links"-["https://github.com", "https://adrianistan.eu"]], Assertion).

    test(for_loop_filter, true(Assertion)) :-
        template_test("templates/j.in.html", "templates/j.out.html", ["webs"-[["name"-"Google", "popularity"-100], ["name"-"GitHub", "popularity"-40]]], Assertion).

    % auxiliary predicates

    template_test(In, Out, Vars, Assertion) :-
      teruel:render(In, Vars, OutputReal),
      ^^text_file_assertion(Out, OutputReal, Assertion).

:- end_object.