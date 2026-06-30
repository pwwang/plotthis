# Flip y-coordinates for spatial data objects

These internal S3 methods flip the y-coordinates of `SpatRaster`,
`SpatVector`, and `data.frame` objects.

For rasters, the raster is flipped vertically and its extent is negated.
For vectors, the y-coordinates of all geometries are negated. For data
frames, the specified y column is negated.

## Usage

``` r
.flip_y(data, ...)

# S3 method for class 'SpatRaster'
.flip_y(data, ...)

# S3 method for class 'SpatVector'
.flip_y(data, ...)

# S3 method for class 'data.frame'
.flip_y(data, y = "y", ...)
```

## Arguments

- data:

  A `SpatRaster`, `SpatVector`, or `data.frame`.

- ...:

  Additional arguments (not used).

- y:

  A character string specifying the y column name for `data.frame`
  input. Ignored for `SpatRaster` and `SpatVector`. Default is `"y"`.

## Value

For `SpatRaster` input, a `SpatRaster` flipped vertically with negated
y-extent. For `SpatVector` input, a `SpatVector` with negated
y-coordinates. For `data.frame` input, a data frame with the specified
`y` column negated.

## Details

These functions are intended for internal use to facilitate coordinate
transformations. When visualizing spatial data, it is often necessary to
flip the y-axis to put the origin at the top-left corner. However,
[`geom_sf()`](https://ggplot2.tidyverse.org/reference/ggsf.html) does
not work with
[`scale_y_reverse()`](https://ggplot2.tidyverse.org/reference/scale_continuous.html).
See [this GitHub
comment](https://github.com/tidyverse/ggplot2/issues/4021#issuecomment-650787582)
for details. These functions negate the y-coordinates so that the axis
labels can be displayed with reversed sign, mimicking
`scale_y_reverse()`.
