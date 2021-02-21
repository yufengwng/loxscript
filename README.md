## loxscript

An interpreter written in Rust for the [Lox language][lox], but with different
syntax and small adjustments. The `tree` module provides a tree-walk
interpreter that uses a recursive descent parser and very limited static
analysis.

[lox]: http://www.craftinginterpreters.com/the-lox-language.html

### comments

```
# Only line comments are supported at the moment.
# Starts with a `#` and goes to end-of-line.
```

### values

`none`
* The absence of a value. Similar to `null` and `nil` in other languages. Only
  equal to itself and no other value.

booleans
* `true`, `false` literals. Only `none` and `false` is falsy, everything else
  is truthy.

numbers
* Both integer and float literals. Normalized to floats under-the-hood.

strings
* Double quoted string literals. No support for escaping at the moment.

functions
* Are first-class values. Can have nested functions too.

objects
* Are instances of a class. Have fields and methods.

### errors

parse errors
* If there are syntax or static analysis issues.

runtime errors
* Will halt the program.

### expressions

`-`
* Unary negate. Flips the sign of numbers.

`+`, `-`, `*`, `/`, `%`
* Basic arithmetic, plus modulo. Results in a number. Watch out for
  divide-by-zero, which results in a runtime error. Note `+` supports string
  concatenation too.

`<`, `>`, `<=`, `>=`
* Basic comparison. Less than, greater than, and their "equal to" counterparts.
  Results in a boolean.

`==`, `!=`
* Equality testing. Results in a boolean.

`not`, `and`, `or`
* Logical operations. First one is unary, and results in a boolean. Last two is
  binary, will short-circuit, and results in whatever the truthy or falsy value
  is.

`foo(bar)`
* Function call. Argument count must match up with parameter count, or results
  in runtime error.

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

`if ... {} elif ... {} else {}`
* Branching. Parentheses are optional, but braces are required. Branch
  condition needs to be truthy to run the associated block.

`for ...; ...; ... {}`
* Iterating. First is "initialize", to declare or initialize iteration
  variable. Second is "condition" that need to result to truthy to start or
  continue the iteration. Third is "post" that runs after each iteration.

`while ... {}`
* Looping. Condition expression must result to truthy to start or continue
  execution of the associated block.

`break`, `continue`
* Controls `break`-ing out of the loop, or `continue` to next iteration.

`return foo`
* Return from a function. Value is optional, will default to `none`.

`foo = bar`
* Assignment. Variable needs to already exist.

### declarations

`let foo = bar`
* Declares a variable binding. Name `foo` is bound to value `bar`. Value is
  optional, will default to `none`.

`fun foo(bar) {}`
* Declares a function of name `foo`. Parameter names are inside parentheses,
  and braces are required.

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

`class Foo {}`
* Declares a class of name `Foo`. Braces are required.

`class Foo < Bar {}`
* Declares a class that inherits from a superclass.

`bar() {}`
* Declares a method. Similar to function declaration but without the `fun`
  keyword.

`super.method()`
* Access and invoke a method from the superclass. Only allowed within
  subclasses.

`self`
* Literal that points to the current object. Only available inside method
  declarations.

`let f = Foo()`
* Class name is the constructor. Call to create a new instance of that class.
  The `init` method, if declared, will be invoked to instantiate the new
  object. Arguments must match arity of the `init` method.

`f.bar = baz`
* Access fields and methods using `.` operator. Can assign to fields as well.

`f.bar()`
* Access and invoke method.

### stdlib

A few built-in functions:

* `print(foo)` - print value to stdout.

* `clock()` - returns number of seconds since unix epoch.

## getting started

Build, test, and run using `cargo`. Run the language specification test suite
with `test.py`, and benchmarks with `bench.sh`.
