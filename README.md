# WIP - Teruel - Template engine for Prolog

Teruel is a template engine in ISO Prolog, inspired by Tera and Jinja.

# Installation

# API Usage

Load the library:

```
:- use_module('teruel.pl').
```

The predicate `render/3` will be available for you.

* `render(+Filename, +Vars, -Output)`.
 - Filename. The template path
 - Vars. A list of pairs for the variables you want to be available in the template
 - Output. The output text after rendering.

```
render("templates/i.in.html", ["username"-"aarroyoc", "links"-["https://github.com", "https://adrianistan.eu"]], Output).
```

# Manual

Teruel is a template engine. It takes a text file annotated with some code in delimiters and returns a processed text file (renders it). It is usually used with HTML, but Teruel doesn't make any assumption.

There are 3 kinds of delimiters:

* Comments, which do nothing
* Expressions, which calculate a value
* Statements, which allows to control flow

## Comments

Comments use `{#` and `#}`. Text inside comments doesn't get rendered.

For example:

```
{# TODO: Add users #}
<button>Click here</button>
```

renders:

```
<button>Click here</button>
```

## Expressions

Expressions `{{` `}}` are code that evaluate to a value. Also, you can include expressions in some statements like if (see if statement). They can contain variables (which are defined by you, see API Usage) and some operations. However it is not a good idea to create complex expressions in the template, move that logic to your Prolog code.

The most basic expression is just rendering a variable:

```
Hello {{ name }}
```

renders to (Vars = ["name"-"Adrián"]):

```
Hello Adrián
```

Variables can be: numbers, strings, lists and maps. If you want to access a field on a map use the dot syntax:

```
Hello {{ user.display_name }}
```

renders to (Vars = ["user"-["display_name"-"Adrián"]])

```
Hello Adrián
```

### Math in expressions

If a variable is a number, you can do some math in it.

* `+` performs sum, `{{ 1 + 1 }}` renders `2`
* `-` performs substraction, `{{ 2 - 1}}` renders `1`
* `/` performs division, `{{ 10 / 2 }}` renders `5`
* `*` performs multiplication, `{{ 2 * 2 }}` renders `4`
* `%` performs modulo, `{{ 10 % 3 }}` renders `1`

They follow the usual priority rules (multiplication, division and modulo higher) but you can use parenthesis, `{{ (1 + 3) * 3 + 1 }}` renders: `13`

> __A note about whitespace.__ Teruel is very whitespace sensitive, if your template isn't rendering check that you have just 1 whitespace between things. Things like 1+1 won't be recognised: please use 1 + 1


### Filters

Values can be piped through a filter. A filter takes an input, some parameters (some filters only) and return an output. That output can be filtered too, making chains of filters. Filters use `|` to separate between them but the first one cannot be a filter, it needs to be a variable.

For example:
```
{{ name | lower | truncate(length=4) }}
```

renders (Vars = ["name"-"Adrián"])

```
adri
```

The complete list of filters Teruel has can be found below.


### Comparison and logic in expressions

Expressions, as we said earlier, can also be used in some statements like if. If you want to use them there, you probably want to know about the comparisons and logic operators. If used directly, they will render `true` or `false`.

To do comparisons: `==`, `!=`, `>=`, `<=`, `>` and `<` with their usual meanings.

To combine comparisons, use the logic operators: `and`, `or` and `not` with their usual meanings.

For example:
```
{{ 5 >= 4 or 3 < 1 }}
```

renders:

```
true
```

## Statements

Statements, `{%` and `%}`, can be used for more advanced stuff, like control flow and inheritance.

### If

The `if` statement allows you to conditionally render a piece of template based on a expression. You can use `{% else %}` to render a piece of template if the expression is false.

Let's see an example:

```
{% if user == "admin" %}
    <p>Welcome admin</p>
{% else %}
    <p>Welcome regular user</p>
{% endif %}
```

renders (Vars = ["user"-"admin"]):

```
<p>Welcome admin</p>
```

### For

The `for` statement loops over a list, repeating the piece of template for each element on the list, which is available in a new created variable.

```
{% for link in links %}
    <a href="{{ link }}">{{ link }}</a>
{% endfor %}
```

renders (Vars = ["links"-["https://github.com", "https://adrianistan.eu"]])

```
<a href="https://github.com">https://github.com</a>
<a href="https://adrianistan.eu">https://adrianistan.eu</a>
```

### Raw

Teruel uses some special characters as delimiters, so you can't use them in your text code unless you put them inside a `raw` statement. This statements don't do any kind of processing inside, copying the content as-is.

```
{% raw %}
{{ user }}
{% endraw %}
```

renders:

```
{{ user }}
```

### Filter

You can also use filter statements to apply a filter to a large chunk of template. They cannot be chained, but they can be nested (you can put a filter statement inside of another one).

```
{% filter lower %}
HELLO WORLD
{% endfilter %}
```

renders:

```
hello world
```

### Include

Load the contents of another template file and put it there in the main file. You cannot use variables as the filename.

```
{% include "footer.html" %}
```


### Block and extends (inheritance)

Teruel supports Jinja-like inheritance. What does that mean? It means you can create documents based on a parent template and just filling the "blocks".

For example, take a "base.html" template:
```
<!DOCTYPE html>
<html lang="en">
<head>
    {% block head %}
    <link rel="stylesheet" href="style.css" />
    <title>{% block title %}{% endblock %} - My Webpage</title>
    {% endblock %}
</head>
<body>
    <div id="content">{% block content %}{% endblock %}</div>
    <div id="footer">
        {% block footer %}
        &copy; Copyright 2008 by <a href="http://domain.invalid/">you</a>.
        {% endblock %}
    </div>
</body>
</html>
```

It defines a template, with some pieces surrounded by block statements. This blocks can be overriden by other templates that extend this one. For example, a "child.html" could be:

```
{% extends "base.html" %}
{% block title %}Index{% endblock %}
{% block head %}
    {{ super }}
    {% raw %}
    <style type="text/css">
        .important { color: #336699; }
    </style>
    {% endraw %}
{% endblock %}
{% block content %}
    <h1>Index</h1>
    <p class="important">
      Welcome to my awesome homepage.
    </p>
{% endblock %}
```
Child templates must start with an `extends` statement and can only contain, at the top level, block statements. The block statements use the same identifier to override the default content of the original template. However, the special variable `super`, allows us to render the base.html block too.

Rendering will give us:

```
<!DOCTYPE html>
<html lang="en">
<head>
    <link rel="stylesheet" href="style.css" />
    <title>Index - My Webpage</title>
    <style type="text/css">
        .important { color: #336699; }
    </style>
</head>
<body>
    <div id="content">
    <h1>Index</h1>
    <p class="important">
      Welcome to my awesome homepage.
    </p>
</div>
    <div id="footer">
        &copy; Copyright 2008 by <a href="http://domain.invalid/">you</a>.     
    </div>
</body>
</html>
```

## Filter list

Here are the list of filters Teruel supports:

* `lower` - Convert uppercase letters to lowercase
* `upper` - Convert lowercase letters to uppercase
* `length` - Counts the number of chars a string has or the number of items in a list
* `wordcount` - Counts the number of words a string has
* `capitalize` - Converts the first letter to uppercase, the rest to lowercase
* `trim` - Removes the whitespaces at the start and at the end
* `trim_start` - Removes the whitespaces at the start
* `trim_end` - Remove the whitespaces at the end
* `truncate(length=N)` - Truncates the string to the first N characters
* `first` - First element of an array
* `last` - Last element of an array
* `nth(n=N)` - Element N of the array. Starts counting at 0
* `replace(from=From, to=To)` - Replaces the From substring with the To substring in a bigger string.