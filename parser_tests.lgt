:- use_module(parser).

:- object(parser, extends(lgtunit)).

    test(expr_basic) :- 
        phrase(parser:parser(Ast, ""), "Hola"),
	Ast = [text("H"), text("o"), text("l"), text("a")].

    test(expr_for_in_for) :-
        phrase(parser:parser(Ast, ""), "{% for post in posts %}{% for comment in post.comments %}{{ comment.id }}{% endfor %}{% for author in post.authors %}{{ author.name }}{% endfor %}{% endfor %}"),
	Ast = [for("post","posts",[for("comment","post.comments",[expr("comment.id")]),for("author","post.authors",[expr("author.name")])])].
	
:- end_object.
