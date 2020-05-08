Mini-C Language Reference Manual

Gabriel Brown [Uni]
Liat Goldfarb [Uni]
Haomin Gu     [Uni]
Aaron Pickard [Uni]
Columbia University in the City of New York
New York, NY 10027

Contents:
1. Introduction
2. Lexical conventions
2.1. Comments
2.2. Identifiers
2.3. Keywords
2.4. Constants
2.4.1. Integer constants
2.4.2. Floating point constants
2.4.3. Character constants
2.5. Strings
3. Syntax
4. Name
5. Objects and lvalues
6. Conversions
6.1. Characters & integers
6.2. Float & double
6.3. Float and



1. Introduction 
There are many languages on the internet that call themselves "Mini-C". The "Mini-C" described in this document is a computer language closely related to the C programming language defined by D. M. Ritchie that has been built specifically for the DeepSEA architecture. In this intended use case, Mini-C represents an intermediate language that the DeepSEA architecture initially compiles to, before it in turn is compiled to bytecode via Yul.

Due to its relationship to C, MIni-C bears substantive similarities to C; developers wishing to understand C should refer to the C Reference Manual at https://www.bell-labs.com/usr/dmr/www/cman.pdf. In the interests of completeness and clarity, this Language Reference Manual's table of contents is very similar to the C Reference Manual.

2. Lexical conventions
