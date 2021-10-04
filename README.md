# hs-stack-graphs

This is an experimental binding to the C interface of the [stack-graphs](https://github.com/github/stack-graphs/) library. As of now it exists to stress-test certain library invariants. Eventually I want this to be a view into what a stack graphs library would look like without the Rust implementation's C-compatibility constraints, and to provide a good place to do REPL-style interaction with and animated visualizations of stack graphs.

You _must_ use `stack` to develop this library, as `cabal` does not have the ability to specify a relative path to a dependent C library. Furthermore, you'll need to patch it with the included diff. (I don't know whether said diff should go upstream.)

To play around with this:
- `git clone`
- `git submodule update --init --recursive`
- `cd vendor/stack-graphs`
- `g apply < ../../stackgraphs.diff`
- `stack test`
