![](logo.png)

Kalker is a calculator program/website that supports user-defined variables, functions, differentiation, and integration. It runs on Windows, macOS, Linux, Android, and in web browsers (with WebAssembly).

[![Crates.io](https://img.shields.io/crates/v/kalker)](https://crates.io/crates/kalker) ![npm](https://img.shields.io/npm/v/@paddim8/kalk) [![GitHub](https://img.shields.io/github/license/PaddiM8/kalk)](https://github.com/PaddiM8/kalker/blob/master/LICENSE) [![Docs.rs](https://docs.rs/kalk/badge.svg)](https://docs.rs/kalk/latest/kalk/) ![Build status](https://img.shields.io/github/actions/workflow/status/PaddiM8/kalker/build.yml?branch=master&label=build%20%26%20test)

[Website - Try it out here!](https://kalker.xyz)

<img src="preview.png" width="750">

# Features

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
* Syntax highlighting
* Special-symbol completion on tab. Eg. write `sqrt` and press tab. It will be turned into `√`
* Sum function: `sum(start, to, expression)` Eg. `sum(1, 3, 2n+1)` is the same as `2*1+1 + 2*2+1 + 2*3+1` = `15`
* Piecewise functions: `f(x) = { f(x + 1) if x <= 1; x otherwise }`, pressing enter before typing the final `}` will make a new line without submitting
* Load a file including predefined functions and constants. For example, if you're going to use kalker for physics, you load up your file with physics functions/constants when starting kalker. This is done either using the `-i file` flag or by putting files in a certain directory and then doing `load filename` inside kalker. [More about files here](https://kalker.xyz/#files)
* Different number bases: Either with a format like `0b1101`, `0o5.3`, `0xff` or a format like `1101_2`. The latter does not support letters, as they would be interpreted as variables
* Misc: separate expressions by a semicolon to write them on the same line, use the `ans` variable to get the value of the previously calculated expression

# Installation

## Package managers

### macOS (Homebrew)
`brew install kalker`

### macOS (MacPorts)
`sudo port install kalker` ([info](https://ports.macports.org/port/kalker/))

### Arch Linux
`kalker` in the AUR, eg. `yay -S kalker`

### Nix/NixOS
`kalker` in the [`nixpkgs`](https://search.nixos.org/packages?channel=unstable&show=kalker&from=0&size=50&sort=relevance&type=packages&query=kalker) repository.
The most up to date version is also available as a [`flake`](https://search.nixos.org/flakes?channel=unstable&show=kalker&from=0&size=50&sort=relevance&type=packages&query=kalker).

### NetBSD
`pkgin install kalker` (from the [`official repositories`](https://pkgsrc.se/math/kalker))

## Binaries

Pre-compiled binaries for Linux, Windows, and macOS (64-bit) are available in the [releases page](https://github.com/PaddiM8/kalker/releases).

## Compiling

**Minimum rust version: v1.36.0**. Make sure you have `diffutils` `gcc` `make` and `m4` installed. **If you use windows:** [follow the instructions here](https://docs.rs/gmp-mpfr-sys/1.2.3/gmp_mpfr_sys/index.html#building-on-windows) (don't forget to install `mingw-w64-x86_64-rust` in MSYS2).

### Cargo

Run `cargo install kalker`

### Manually

1. Go into the `cli` directory.
2. Run `cargo build --release`
3. Grab the binary from `targets/release`

# Libraries

There are currently three different libraries related to kalker.

* [kalk](https://crates.io/crates/kalk): The Rust crate that powers it all.
* [@paddim8/kalk](https://www.npmjs.com/package/@paddim8/kalk): JavaScript bindings for `kalk`. This lets you use it in the browser thanks to WebAssembly.
* [@paddim8/kalk-component](https://www.npmjs.com/package/@paddim8/kalk-component): A web component that runs `@paddim8/kalk`, which let's you use kalk in the browser with a command line-like interface.

# Syntax

A complete reference can be found on [the website](https://kalker.xyz).

# Contributing

## kalk and cli (Rust)

After making changes to the kalk library (in `kalk/`), you can easily try them out by going to the root of the project directory, and doing `cargo run`. This will start kalker (cli), with the new changes. If you're using Windows, you will need to [follow the instructions here](https://docs.rs/gmp-mpfr-sys/1.2.3/gmp_mpfr_sys/index.html#building-on-windows), but also make sure to install `mingw-w64-x86_64-rust` in MSYS2.

All Rust code is expected to be formatted with `rustfmt

## web (Svelte, TypeScript, Sass)

Run:
1. `npm install`  
2. `npm run dev` - this will automatically re-compile the project when changes are made

## mobile (Android)

Run:  
1. `npm install`
2. `npm run build`
3. `npx cap sync`
4. Build the project using Android Studio, or Gradle directly.
