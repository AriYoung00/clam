# Clam

This is (or will soon possibly be) a scripting language.

## Todo

- [ ] Figure out a way to make AST nodes spanned cleanly. This may involve
    refactoring most of the parser
- [ ] Do research into how typing should work, and how it can cleanly be
    represented
- [ ] Figure out how to implement basic operators over primitive data in a clean way


Current thought for operators over primitive data:
First option is to continue down current path and implement these purely as
"base cases" within the interpreter.
