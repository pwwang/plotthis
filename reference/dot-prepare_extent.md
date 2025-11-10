# Prepare the extent for spatial plots

Prepare the extent for spatial plots

## Usage

``` r
.prepare_extent(ext)
```

## Arguments

- ext:

  A numeric vector of length 4 specifying the extent as
  `c(xmin, xmax, ymin, ymax)`, or a `SpatExtent` object from the `terra`
  package.

## Value

A `SpatExtent` object if `ext` is a numeric vector, or the original
`SpatExtent` if it is already one. NULL is returned if `ext` is NULL.
