:- module(parser, [parser//2, canonical_dir/2]).

:- use_module(library(lists)).
:- use_module(library(pio)).
:- use_module(library(files)).
:- use_module(library(dcgs)).

% Try to implement most of Tera: https://tera.netlify.app/docs/#templates

parser([], _) --> [].
parser([Node|Ast], Path) -->
    (
	parser_expr(Node)
    ;   parser_raw(Node)
    ;   parser_filter(Node, Path)
    ;   parser_if(Node, Path)
    ;   parser_for(Node, Path)
    ;   parser_include(Node, Path)
    ;   parser_extends(Node, Path)
    ;   parser_block(Node, Path)
    ;   parser_comment
    ;   parser_text(Node)
    ),
    parser(Ast, Path).

parser_expr(expr(X)) -->
    "{{ ",
    seq(X),
    " }}".

parser_raw(raw(X)) -->
    "{% raw %}",
    seq(X),
    "{% endraw %}".

parser_filter(filter(Filter, X), Path) -->
    "{% filter ",
    seq(Filter),
    " %}",
    parser_in_filter(X, Path).

parser_if(if(Expr, If, Else), Path) -->
    "{% if ",
    seq(Expr),
    " %}",
    parser_in_if(If, Else, Path).

parser_for(for(LocalVar, ListExpr, X), Path) -->
    "{% for ",
    seq(LocalVar),
    " in ",
    seq(ListExpr),
    " %}",!,
    parser_in_for(X, Path).

parser_include(include(X), Path) -->
    "{% include \"",
    seq(File),
    "\" %}",
    {
	concat_path(Path, File, PathFile),
	canonical_dir(PathFile, PathDir),
	once(phrase_from_file(parser(X, PathDir), PathFile))
    }.

parser_extends(extends(X, Blocks), Path) -->
    "{% extends \"",
    seq(File),
    "\" %}",
    parser_blocks(Blocks, Path),
    {
	concat_path(Path, File, PathFile),
	canonical_dir(PathFile, PathDir),
	phrase_from_file(parser(X, PathDir), PathFile)
    }.

parser_block(block(Name, X), Path) -->
    "{% block ",
    seq(Name),
    " %}",
    seq(Content),
    "{% endblock %}",
    { phrase(parser(X, Path), Content) }.

parser_comment -->
    "{#",
    seq(_),
    "#}".

parser_text(text([C])) --> [C].

parser_in_for([], _) --> "{% endfor %}".
parser_in_for([Node|Ast], Path) -->
    (
	parser_expr(Node)
    ;   parser_raw(Node)
    ;   parser_filter(Node, Path)
    ;   parser_if(Node, Path)
    ;   parser_for(Node, Path)
    ;   parser_include(Node, Path)
    ;   parser_comment
    ;   parser_text(Node)
    ),
    parser_in_for(Ast, Path).

parser_in_if([],[], _) --> "{% endif %}".
parser_in_if([], Else, Path) -->
    "{% else %}",
    parser_in_else(Else, Path).
parser_in_if([Node|Ast], Else, Path) -->
    (
	parser_expr(Node)
    ;   parser_raw(Node)
    ;   parser_filter(Node, Path)
    ;   parser_if(Node, Path)
    ;   parser_for(Node, Path)
    ;   parser_include(Node, Path)
    ;   parser_comment
    ;   parser_text(Node)
    ),
    parser_in_if(Ast, Else, Path).

parser_in_else([], _) --> "{% endif %}".
parser_in_else([Node|Ast], Path) -->
    (
	parser_expr(Node)
    ;   parser_raw(Node)
    ;   parser_filter(Node, Path)
    ;   parser_if(Node, Path)
    ;   parser_for(Node, Path)
    ;   parser_include(Node, Path)
    ;   parser_comment
    ;   parser_text(Node)
    ),
    parser_in_else(Ast, Path).

parser_in_filter([], _) --> "{% endfilter %}".
parser_in_filter([Node|Ast], Path) -->
    (
	parser_expr(Node)
    ;   parser_raw(Node)
    ;   parser_filter(Node, Path)
    ;   parser_if(Node, Path)
    ;   parser_for(Node, Path)
    ;   parser_include(Node, Path)
    ;   parser_comment
    ;   parser_text(Node)
    ),
    parser_in_filter(Ast, Path).

    
look_ahead(T), [T] --> [T].

% Parser for child templates

parser_blocks([Name-X|Blocks], Path) -->
    seq(_),
    "{% block ",
    seq(Name),
    " %}",
    parser_in_block(X, Path),
    parser_blocks(Blocks, Path).
parser_blocks([], _) --> whitespace_.

parser_in_block([], _) --> "{% endblock %}".
parser_in_block([Node|Ast], Path) -->
    (
	parser_expr(Node)
    ;   parser_raw(Node)
    ;   parser_filter(Node, Path)
    ;   parser_if(Node, Path)
    ;   parser_for(Node, Path)
    ;   parser_include(Node, Path)
    ;   parser_extends(Node, Path)
    ;   parser_block(Node, Path)
    ;   parser_comment
    ;   parser_text(Node)
    ),
    parser_in_block(Ast, Path).

whitespace_ -->
    [X],
    {
        memberchk(X, " \t\n\r")
    },
    whitespace_.

whitespace_ --> [].

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

contains(X, In) :-
    append(_, Y, X),
    append(In, _, Y).
