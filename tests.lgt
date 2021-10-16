:- use_module('teruel').

:- object(tests, extends(lgtunit)).

    test(trivial) :- true.

    test(simple_substitution) :- template_test('templates/a.in.html', 'templates/a.out.html', ["username"-"aarroyoc"]).
    test(raw_block) :- template_test('templates/b.in.html', 'templates/b.out.html', ["username"-"aarroyoc"]).
    test(comment_block) :- template_test('templates/c.in.html', 'templates/c.out.html', ["username"-"aarroyoc"]).

    test(for_loop) :- template_test('templates/h.in.html', 'templates/h.out.html', ["username"-"aarroyoc", "links"-["https://github.com", "https://adrianistan.eu"]]).

    template_test(In, Out, Vars) :-
      teruel:render(In, Vars, OutputReal),
      open(Out, read, Stream),
      read_string(Stream, OutputExpected),
      OutputReal = OutputExpected.

    read_string(Stream, String) :-
        get_char(Stream, C),
        ( C = end_of_file ->
          String = []
        ; (read_string(Stream, S0), String = [C|S0])
        ).

:- end_object.