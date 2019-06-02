## loxscript

An interpreter written in Rust for the [Lox language][lox], but with different
syntax and small adjustments. It uses a recursive descent parser, has a very
limited static analysis pass, and essentially runs a tree-walk interpreter.

[lox]: http://www.craftinginterpreters.com/the-lox-language.html

### comments

```
# Only line comments are supported at the moment.
# Starts with a `#` and goes to end-of-line.
```

### values

* `none`: the absence of a value. Similar to `null` and `nil` in other
  languages. `none` is only equal to itself and no other value.

* boolean: `true`, `false` literals. Only `none` and `false` is falsy,
  everything else is truthy.

* number: both integer and float literals. Normalized to floats
  under-the-hood.

* string: double quoted string literals. No escaping supported at the moment.

* functions: are first-class values. Can have nested functions too.

### errors

* parse errors: if there are syntax or static analysis issues.

* runtime errors: will halt the program.

### expressions

* `-`: unary negate. Flips the sign of numbers.

* `+`, `-`, `*`, `/`, `%`: basic arithmetic, plus modulo. Results in a number.
  Watch out for divide-by-zero, which results in a runtime error.  Note `+`
  supports string concatenation too.

* `<`, `>`, `<=`, `>=`: basic comparison. Less than, greater than, and their
  "equal to" counterparts. Results in a boolean.

* `==`, `!=`: equality testing. Results in a boolean.

* `not`, `and`, `or`: logical operations. First one is unary, and results in a
  boolean. Last two is binary, will short-circuit, and results in whatever the
  truthy or falsy value is.

* `foo(bar)`: function call. Argument count must match up with parameter
  count, or results in runtime error.

### statements

```
if foo {
    ...
} elif bar {
    ...
} elif baz {
    ...
} else {
    ...
}

for let i = a; i < b; i = i + c {
    ...
}

while foo != bar {
    ...
}
```

* `if ... {} elif ... {} else {}`: for branching. Parentheses are optional,
  but braces are required. Branch condition needs to be truthy to run the
  associated block.

* `for ...; ...; ... {}`: for iterating. First is "initialize", to declare or
  initialize iteration variable. Second is "condition" that need to result to
  truthy to start or continue the iteration. Third is "post" that runs after
  each iteration.

* `while ... {}`: for looping. Condition expression must result to truthy to
  start or continue execution of associate block.

* `break`, `continue`: controls `break`ing out of the loop, or `continue` to
  next iteration.

* `return foo`: return from a function. Value is optional, will default to
  `none`.

* `foo = bar`: assignment. Variable needs to already exist.

### declarations

* `let foo = bar`: declares a variable binding. Name `foo` is bound to value
  `bar`. Value is optional, will default to `none`.

* `fun foo(bar) {}`: declares a function of name `foo`. Parameter names are
  inside parentheses, and braces are required.

### classes

Basic class system with inheritance, instances, methods, and fields.

```
class Foo < Bar {
    init(a, b) {
        self.a = a;
        self.b = b;
    }

    bar() {
        super.bar();
    }
}

let f = Foo(a, b);
f.c = c;
f.bar();
```

* `class Foo {}`: declares a class of name `Foo`. Braces are required.

* `class Foo < Bar {}`: declares a class that inherits from a superclass.

* `bar() {}`: declares a method. Similar to function declaration but without
  the `fun` keyword.

* `super.method()`: access and invoke a method from the superclass.

* `self`: literal that points to the current object. Only available inside
  method declarations.

* `let f = Foo()`: class name is the constructor. Call to create a new
  instance of that class. The `init` method, if declared, will be invoked to
  instantiate the new object. Arguments must match arity of the `init` method.

* `f.bar = baz`: access fields and methods using `.` operator. Can assign to
  fields as well.

* `f.bar()`: access and invoke method.

### stdlib

A few built-in functions:

* `print(foo)`: print value to stdout.

* `clock()`: returns number of seconds unix epoch.

## getting started

Build, test, and run using `cargo`. Run the language specification test suite
with `test.py`, and a couple benchmarks with `bench.sh`.
