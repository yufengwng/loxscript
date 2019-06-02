Specification files are source code files written in loxscript and contain
assertions that helps validate behavior of the interpreter implementation.
Adapted from [craftinginterpreters][ci-tests].

[ci-tests]: https://github.com/munificent/craftinginterpreters/tree/master/test

\# symbols

* `#=> ` Notes an assertion of output on stdout. The space is significant and
  everything after the space is treated as the expected output.

* `#!! ` Notes a lexing or parsing error. Everything after the space is treated
  as the expected error message.

* `#@! ` Notes a runtime error. Everything after the space is treated as the
  expected error message.

