# Call a function with a list of arguments

A faster alternative to
[`do.call`](https://rdrr.io/r/base/do.call.html), especially when there
are large objects in the argument list. Named arguments are looked up by
symbol in the evaluation environment rather than being copied into the
call, which avoids the copying overhead of
[`base::do.call`](https://rdrr.io/r/base/do.call.html). Unnamed
arguments are embedded in the call directly and do not benefit from this
optimization. Borrowed from `Gmisc::fastDoCall`.

## Usage

``` r
do_call(what, args, quote = FALSE, envir = parent.frame())
```

## Arguments

- what:

  either a function or a non-empty character string naming the function
  to be called.

- args:

  a *list* of arguments to the function call. The `names` attribute of
  `args` gives the argument names.

- quote:

  a logical value indicating whether to quote the arguments.

- envir:

  an environment within which to evaluate the call. This will be most
  useful if `what` is a character string and the arguments are symbols
  or quoted expressions.

## Value

The result of the function call
