# Clam

This is (or will soon possibly be) a scripting language.

## Todo

- [ ] Add AST pass to make sure `break` is only used in valid context (within loop)
- [ ] Figure out a way to allow mutation of captured variables from child scopes, or explicitly disallow it. Currently, if a variable belonging to a parent scope is mutated in a child scope, this mutation will not be reflected in the parent scope. Could also just disallow this pattern ("no mutation of captured variables from child scope")

- [x] Figure out a way to make AST nodes spanned cleanly. This may involve
    refactoring most of the parser
- [x] Do research into how typing should work, and how it can cleanly be
    represented
- [x] Figure out how to implement basic operators over primitive data in a clean way


Current thought for operators over primitive data:
First option is to continue down current path and implement these purely as
"base cases" within the interpreter.
