#!/bin/python

from random import randint
import mpmath


funcs = [
    (mpmath.arg, "arg"),
    (mpmath.ceil, "ceil"),
    (mpmath.exp, "exp"),
    (mpmath.floor, "floor"),
    (mpmath.log10, "log"),
    (mpmath.ln, "ln"),
    (mpmath.sqrt, "sqrt"),
    (mpmath.acos, "acos"),
    (mpmath.acosh, "acosh"),
    (mpmath.acsc, "acsc"),
    (mpmath.acsch, "acsch"),
    (mpmath.acot, "acot"),
    (mpmath.acoth, "acoth"),
    (mpmath.asec, "asec"),
    (mpmath.asech, "asech"),
    (mpmath.asin, "asin"),
    (mpmath.asinh, "asinh"),
    (mpmath.atan, "atan"),
    (mpmath.atanh, "atanh"),
    (mpmath.cos, "cos"),
    (mpmath.cosh, "cosh"),
    (mpmath.csc, "csc"),
    (mpmath.csch, "csch"),
    (mpmath.cot, "cot"),
    (mpmath.coth, "coth"),
    (mpmath.sec, "sec"),
    (mpmath.sech, "sech"),
    (mpmath.sin, "sin"),
    (mpmath.sinh, "sinh"),
    (mpmath.tan, "tan"),
    (mpmath.tanh, "tanh"),
]

for func in funcs:
    args = [
        (randint(-10, 10) / 10, 0),
        (0, randint(-10, 10) / 10),
        (randint(-10, 10) / 10, randint(-10, 10) / 10),
        (randint(-10, 10) / 10, randint(-10, 10) / 10),
        (randint(15, 30) / 10, 0),
        (randint(-30, -15) / 10, 0),
        (0, 0)
    ]

    for arg in args:
        result = 0
        defined = True
        try:
            result = func[0](complex(arg[0], arg[1]))
        except:
            defined = False

        print("({0}, ({1}f64, {2}f64), ({3}, {4})),".format(
            func[1],
            arg[0],
            arg[1],
            str(round(result.real, 7)) + "f64" if defined else "f64::NAN",
            str(round(result.imag, 7)) + "f64" if defined else "f64::NAN"))
