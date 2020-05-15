This folder contains all of our groupwork except for the report that should
be submitted separately.

The folder Demo_Code contains sample minic programs, as well as the
transpiled YUL, outputted from our transpiler.

The folder Transpiler contains the files for our transpiler. To build the
transpiler, run "ocamlbuild transpiler.native" on the commandline while in
the Transpiler folder

To compile a minic program into yul, run "cat ../Demo_Code/***example.mc***
| transpiler.native" on the commandline while in the Transpiler folder.
