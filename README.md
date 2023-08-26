# Clam

This is (or will soon possibly be) a scripting language.

## Todo

- [ ] Add collection primitive (stack-allocated)
- [ ] Add user-defined types
    - [x] Modify AST/parser to define mod as `Either<FnDef, StructDef>`
    - [x] Add structs to ctx so we can check all fields get populated
    - [x] Add struct instantiation to parser / AST (Expr::Literal)
    - [ ] Consider ways to / whether we want to remove the leading `.` for struct literals
    - [ ] Add struct instantiation to eval_let
- [ ] Create test case exercising "loops never exit" bug -- probably related to environment cloning / mutation somewhere
- [ ] integreate [datatest crate](https://docs.rs/datatest/latest/datatest/) with top-level tests
- [ ] create top level benchmarks (maybe using datatest crate as well)
- [ ] compare performance with python in a variety of cases
    - [ ] figure out why fibonacci is slower than python
- [ ] Add AST pass to make sure `break` is only used in valid context (within loop)
    - matches python behavior -- `break` outside loop is error as soon as a module is loaded, rather than when the break is encountered
- [ ] Figure out a way to allow mutation of captured variables from child scopes, or explicitly disallow it. Currently, if a variable belonging to a parent scope is mutated in a child scope, this mutation will not be reflected in the parent scope. Could also just disallow this pattern ("no mutation of captured variables from child scope")
- [ ] Add error recovery to parser
- [ ] Add useful error messages to top-level

- [x] Figure out a way to make AST nodes spanned cleanly. This may involve
    refactoring most of the parser
- [x] Do research into how typing should work, and how it can cleanly be
    represented
- [x] Figure out how to implement basic operators over primitive data in a clean way

