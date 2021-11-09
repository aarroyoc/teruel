:- use_module(library(random)).
:- use_module(lyncex).
:- use_module('../../teruel.pl').

listen(Port) :-
    http_listen(Port, [
        get(/, index),
        get('dice.svg', dice),
        get('htmx.min.js', htmx)
    ]).

index(_Request, Response) :-
    html_render_response(Response, "index.html").

dice(_Request, Response) :-
    random_integer(1, 7, N),
    render("dice.svg", ["n"-N], DiceSvg),
    http_status_code(Response, 200),
    http_headers(Response, ["content-type"-"image/svg+xml"]),
    http_body(Response, text_custom(DiceSvg)).

htmx(_Request, Response) :-
    http_status_code(Response, 200),
    http_headers(Response, ["content-type"-"text/javascript"]),
    http_body(Response, file("htmx.min.js")).