This is a transpiler from MiniC to YUL based on the MicroC compiler given in
class

to build the transpiler, run "ocamlbuild transpiler.native" on the command
line (bash)
then, to transpile a program from minic called "example.mc" into YUL, run:
cat example.mc | transpiler.native

output will be printed to the terminal.
To print output to a file, run:

cat example.mc | transpiler.native > output.yul
