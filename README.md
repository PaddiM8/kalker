# kalk
Kalk is a CLI calculator that supports user-defined variables and functions. An example of what it can parse:

```
f(x, y) = sum(1, 3, (2sin4/x!)^y) + cos(n deg)
a = 3
f(a, 2)
```
`>> 1.1899401098014355`

## Compiling
1. Make sure you have `diffutils` `gcc` `make` and `m4` installed.
2. Go into the `kalk_cli` directory.
3. Run `cargo build --release`
4. Grab the binary from `targets/release`
