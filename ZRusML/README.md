# ZRusML compiler

This is a homework for programming language translations course.

License: LGPL for implementation code + WTFPL for test examles

Authors: Shangareev Rustam, rustam.letitbe@gmail.com; Danil Yevdokimov, st084331@student.spbu.ru

### Features done:

- Parser
- Type inferencer
- Compiler from ANF and text containing code
- Unit tests

### Features details:

- Parser based on abstract syntactic tree
- Type inferencer supports generic type, errors check and printing, types printing
- ANF builder, which preliminarily liftes lambdas and converts closures
- LLVM compiler from ANF
- Compiler that collects all stages together, which can successfully execute correct code and notify about errors in incorrect code
- Unit tests for parser, lambda lifting, closure conversion, anf building, type inferencing and LLVM compiler

### Mini-language supports

- Int, Bool
- Functions, lambda, recursive functions
- Generic types
- Binary and unary operators, let...in binding, if statements, partially application

and etc., more examples in tests.