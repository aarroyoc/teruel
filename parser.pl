:- module(parser, [parser//1]).

:- use_module(library(lists)).
:- use_module(library(dif)).
:- use_module(library(pio)).

% Try to implement most of Tera: https://tera.netlify.app/docs/#templates
% TODO: elif
parser(node(expr(X), Xs)) -->
    "{{ ",
    string_(X),
    {
        length(X, N),
        N > 0
    },
    " }}",
    parser(Xs).

% statements
parser(node(raw(X), Xs)) -->
    "{% raw %}",
    raw_string_(X),
    "{% endraw %}",
    parser(Xs).

parser(node(filter(Filter, X), Xs)) -->
    "{% filter ",
    string_(Filter),
    " %}",
    parser(X),
    "{% endfilter %}",
    parser(Xs).

parser(node(if_else(Expr, X, Y), Xs)) -->
    "{% if ",
    string_(Expr),
    " %}",
    parser(X),
    "{% else %}",
    parser(Y),
    "{% endif %}",
    parser(Xs).

parser(node(if(Expr, X), Xs)) -->
    "{% if ",
    string_(Expr),
    " %}",
    parser(X),
    "{% endif %}",
    parser(Xs).

parser(node(for(LocalVar, ListVar, X), Xs)) -->
    "{% for ",
    string_(LocalVar),
    " in ",
    string_(ListVar),
    " %}",
    parser(X),
    "{% endfor %}",
    parser(Xs).

parser(node(include(X), Xs)) -->
    "{% include \"",
    string_(File),
    "\" %}",
    {
        atom_chars(AtomFile, File),
        once(phrase_from_file(parser(X), AtomFile))
    },
    parser(Xs).

% comments
parser(Xs) -->
    "{#",
    raw_string_(_),
    "#}",
    parser(Xs).

% normal text
parser(node(text(X), Xs)) -->
    string_(X),
    {
        length(X, N),
        N > 0,!
    },
    parser(Xs).

parser([]) --> [].

string_([X|Xs]) -->
    [X],
    {
        maplist(dif(X), "{}%")
    },
    string_(Xs).

string_([]) -->
    [].

raw_string_([X|Xs]) -->
    [X],
    raw_string_(Xs).

raw_string_([]) -->
    [].