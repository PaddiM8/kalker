# kalk
Kalk is a math parser library that supports user-defined variables and functions. An example of what it can parse:

```
f(x, y) = sum(1, 3, (2sin4/x!)^y) + cos(n deg)
a = 3
f(a, 2)
```
`>> 1.1899401098014355`

## Features
* Operators: `+`, `-`, `*`, `/`, `!`
* Groups: `()`, `[]`, `⌈ceil⌉`, `⌊floor⌋`
* [Vectors](https://kalker.xyz/#vectors): (x, y, z, ...)
* [Matrices](https://kalker.xyz/#matrices): [x, y, z; a, b, c; ...]
* [Pre-defined functions and constants](https://kalker.xyz/#functions)
* User-defined functions and variables. `f(x, y) = xy`, `x = 5`
* Root finding using Newton's method (eg. x^2 = 64). Note: estimation and limited to one root
* Derivative of functions (derivatives of noisy functions or of higher order can be a bit inaccurate). `f'(2)`, `sin'(-pi)`
* Integration. `∫(0, pi, sin(x) dx)` or `∫(0, π, sin(x) dx)`, maybe sometimes be slightly off
* Understands fairly ambiguous syntax. Eg. `2sin50 + 2xy`
* Sum function: `sum(start, to, expression)` Eg. `sum(1, 3, 2n+1)` is the same as `2*1+1 + 2*2+1 + 2*3+1` = `15`
* Piecewise functions: `f(x) = { f(x + 1) if x <= 1; x otherwise }`, pressing enter before typing the final `}` will make a new line without submitting
* Different number bases: Either with a format like `0b1101`, `0o5.3`, `0xff` or a format like `1101_2`. The latter does not support letters, as they would be interpreted as variables
* Misc: separate expressions by a semicolon to write them on the same line, use the `ans` variable to get the value of the previously calculated expression

## Rust Usage
```rust
use kalk::parser;
let mut parser_context = parser::Context::new();
let precision = 53;
let result = parser::eval(&mut parser_context, "5*3", precision).unwrap().unwrap();
assert_eq!(result.to_f64(), 15f64);
```

### Using f64 instead of rug::Float
The cargo feature `rug` enables rug, and is used by default. If you disable this, kalk will use `f64` instead, making it more portable.

### Compiling
Make sure you have `diffutils` `gcc` `make` and `m4` installed.

## JavaScript Usage
```js
const kalk = await import("@paddim8/kalk");
const context = new kalk.Context();
console.log(context.evaluate("2pi + 3").toScientificNotation().toString());
```
