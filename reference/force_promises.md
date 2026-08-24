# Force all bindings in an environment

Evaluates every binding in `env` so that unforced argument promises
(created by lazy evaluation) are replaced by their values. This prevents
`env` from keeping its calling frame alive when it is referenced by a
returned object (e.g. a ggplot's `plot_env`) and later serialized (e.g.
by [`saveRDS()`](https://rdrr.io/r/base/readRDS.html)), which would
otherwise pull the caller's data into the serialized object.

## Usage

``` r
force_promises(env = parent.frame())
```

## Arguments

- env:

  The environment whose bindings to force. Default: the caller's frame.

## Value

Invisibly `NULL`.
