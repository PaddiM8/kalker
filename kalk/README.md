# kalk
Kalk is a math parser library that supports user-defined variables and functions. An example of what it can parse:

```
f(x, y) = sum(1, 3, (2sin4/x!)^y) + cos(n deg)
a = 3
f(a, 2)
```
`>> 1.1899401098014355`

## Features
* Operators: +, -, \*, /, !
* Groups: (), ⌈⌉, ⌋⌊
* [Pre-defined functions and constants](https://github.com/PaddiM8/kalk/blob/master/kalk/src/prelude.rs)
* User-defined functions and variables. `f(x, y) = xy`, `x = 5`
* Understands fairly ambiguous syntax. Eg. `2sin50 + 2xy`
* Syntax highlighting
* Special-symbol completion on tab. Eg. write `sqrt` and press tab. It will be turned into `√`.

## Rust Usage
```rust
use kalk::parser;
let mut parser_context = parser::Context::new();
let precision = 53;
let result = parser::eval(&mut parser_context, "5*3", precision).unwrap().unwrap();
assert_eq!(result.to_f64(), 15f64);
```

### Using f64 instead of rug::Float
The cargo feature `rug` enables rug, and is used by default. If you disalbe this, kalk will use `f64` instead, making it more portable.

### Compiling
Make sure you have `diffutils` `gcc` `make` and `m4` installed.

## JavaScript Usage
```js
const kalk = await import("@paddim8/kalk");
const context = new kalk.Context();
console.log(context.evaluate("2pi + 3").toScientificNotation().toString());
```