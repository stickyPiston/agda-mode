
# JavaScript bindings for Iepje

Design priorities:

1. Coverage:
  * Supporting the JS functions needed by `Iepje.Internal`
2. Maintainability:
  * Minimal dependencies
  * Minimal JavaScript
  * Clear organization
3. Safety

## Minimal Dependencies

Only browser-implemented JavaScript functions are bound.

No 3rd party JavaScript libraries are required.

## Minimal JavaScript

JavaScript glue code is limited to `{-# COMPILE JS #-}` pragmas, and those are kept short.

## Organization

Directory structure follows the structure of the [web standards].

[web standards]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/JavaScript_technologies_overview

## Safety

This library aims to provide these safety guarantees to its users:

* The run-time effects of programs won't be changed by Agda's compile-time evaluation
* Type errors will be raised at compile-time, instead of at run-time.

Threats to these properties include:

* Mutability of JavaScript data
* `null`ability of JavaScript expressions
* Other side effects, e.g. the result of `Date.now()` changes depending on *when* it is evaluated
* Programmer error

Protections provided by in this library include:

* Wrapping the return type of all value-level bindings in `IO`
* Fitting types to bindings, following the MDN docs
