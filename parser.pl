:- module(parser, [parser//2, canonical_dir/2]).

:- use_module(library(lists)).
:- use_module(library(dif)).
:- use_module(library(pio)).
:- use_module(library(files)).

% Try to implement most of Tera: https://tera.netlify.app/docs/#templates
parser(node(expr(X), Xs), Path) -->
    "{{ ",
    string_(X),
    {
        length(X, N),
        N > 0
    },
    " }}",
    parser(Xs, Path).

% statements
parser(node(raw(X), Xs), Path) -->
    "{% raw %}",
    raw_string_(X),
    "{% endraw %}",
    parser(Xs, Path).

parser(node(filter(Filter, X), Xs), Path) -->
    "{% filter ",
    string_(Filter),
    " %}",
    parser(X, Path),
    "{% endfilter %}",
    parser(Xs, Path).

parser(node(if_else(Expr, X, Y), Xs), Path) -->
    "{% if ",
    string_(Expr),
    " %}",
    parser(X, Path),
    "{% else %}",
    parser(Y, Path),
    "{% endif %}",
    parser(Xs, Path).

parser(node(if(Expr, X), Xs), Path) -->
    "{% if ",
    string_(Expr),
    " %}",
    parser(X, Path),
    "{% endif %}",
    parser(Xs, Path).

parser(node(for(LocalVar, ListVar, X), Xs), Path) -->
    "{% for ",
    string_(LocalVar),
    " in ",
    string_(ListVar),
    " %}",
    parser(X, Path),
    "{% endfor %}",
    parser(Xs, Path).

parser(node(include(X), Xs), Path) -->
    "{% include \"",
    string_(File),
    "\" %}",
    {
        concat_path(Path, File, PathFile),
        canonical_dir(PathFile, PathDir),
        atom_chars(AtomFile, PathFile),
        once(phrase_from_file(parser(X, PathDir), AtomFile))
    },
    parser(Xs, Path).

parser(node(extends(X, Blocks)), Path) -->
    "{% extends \"",
    string_(File),
    "\" %}",
    parser_blocks(Blocks, Path),
    {
        concat_path(Path, File, PathFile),
        canonical_dir(PathFile, PathDir),
        atom_chars(AtomFile, PathFile),
        phrase_from_file(parser(X, PathDir), AtomFile)
    }.

parser(node(block(Name, X), Xs), Path) -->
    "{% block ",
    string_(Name),
    " %}",
    parser(X, Path),
    "{% endblock %}",
    parser(Xs, Path).

% comments
parser(Xs, Path) -->
    "{#",
    raw_string_(_),
    "#}",
    parser(Xs, Path).

% normal text
parser(node(text(X), Xs), Path) -->
    string_(X),
    {
        length(X, N),
        N > 0,!
    },
    parser(Xs, Path).

parser([], _) --> [].

% Parser for child templates

parser_blocks([Name-X|Blocks], Path) -->
    string_(_),
    "{% block ",
    string_(Name),
    " %}",
    parser(X, Path),
    "{% endblock %}",
    parser_blocks(Blocks, Path).

parser_blocks([], _) --> [].

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

canonical_dir(Chars, FolderPath) :-
    path_canonical(Chars, Path),
    path_segments(Path, PathSegments),
    once(append(FolderPathSegments, [_], PathSegments)),
    path_segments(FolderPath, FolderPathSegments).

concat_path(Path, File, PathFile) :-
    path_segments(Path, P0),
    path_segments(File, P1),
    append(P0, P1, P2),
    path_segments(PathFile, P2).