# kalk
Kalk is a calculator (both program and library) that supports user-defined variables and functions.

![](example.png)

## Features
* Operators: +, -, \*, /, !
* Groups: (), ⌈⌉, ⌋⌊
* [Pre-defined functions and constants](https://github.com/PaddiM8/kalk/blob/master/kalk/src/prelude.rs)
* User-defined functions and variables. `f(x, y) = xy`, `x = 5`
* Understands fairly ambiguous syntax. Eg. `2sin50 + 2xy`
* Syntax highlighting
* Special-symbol completion on tab. Eg. write `sqrt` and press tab. It will be turned into `√`.

## Installing
Make sure you have `diffutils` `gcc` `make` and `m4` installed.

### Cargo install
Run `cargo install kalk_cli`

### Compiling yourself
1. Go into the `kalk_cli` directory.
2. Run `cargo build --release`
3. Grab the binary from `targets/release`
