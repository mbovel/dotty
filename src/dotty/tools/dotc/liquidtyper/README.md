LiquidTyper Readme
----------

This branch of Dotty introduces syntax and type checking for logically qualified base types in Scala.
An additional typer phase, the *LiquidTyper*, tabulates subtyping relations among such qualified types and discharges
the proof obligations using an SMT solver.

Be aware that LiquidTyper is currently under active development and in an experimental state.
There are, however, several examples that demonstrate the extension of Scala's capabilities already supported.
To run them,
* Switch to Java 8 (required by sbt 0.13): `eval "$(cs java --jvm 8 --env)"`
* Make sure that [Z3](https://github.com/Z3Prover/z3) is available on the $PATH of your system,
* Compile Dotty (`sbt compile`),
* Run the tests using `sbt testOnly *liquidtyper*`.

The `lib/` directory already contains pre-built jars for
[Leon](https://github.com/epfl-lara/leon) and its `scala-smtlib` dependency.
