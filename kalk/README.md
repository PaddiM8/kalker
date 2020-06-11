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

## Usage
```
use kalk::parser;

...
let parser_context = parser::Context::new();
assert_eq!(parser_context.eval("5*3").unwrap().unwrap(), 15);
```

## Compiling
Make sure you have `diffutils` `gcc` `make` and `m4` installed.
