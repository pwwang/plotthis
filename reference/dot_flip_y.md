# Flip values on the y-axis direction, and negate the Y-Coordinates of SpatRaster, SpatVector Object and data.frame

These internal functions flip the y-coordinates of `SpatRaster` and
`SpatVector` objects from the `terra` package. For rasters, the function
vertically flips the raster and adjusts its extent accordingly. For
vectors, the function negates the y-coordinates of all geometries. For
data frames, it negates the values in the specified y column.

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

  A `SpatRaster` or `SpatVector` object from the `terra` package, or a
  data.frame with x and y columns.

## Value

For `SpatRaster` input, a `SpatRaster` object with flipped y-coordinates
and adjusted extent. For `SpatVector` input, a `SpatVector` object with
y-coordinates negated. For `data.frame` input, a data frame with the
specified y column negated.

## Details

These functions are intended for internal use to facilitate coordinate
transformations. When visualizing spatial data, it is often necessary to
flip the y-axis to put the origin at the top left corner. However, a lot
of elements have to be visualized with
[`ggplot2::geom_sf()`](https://ggplot2.tidyverse.org/reference/ggsf.html),
which won't work with
[`ggplot2::scale_y_reverse()`](https://ggplot2.tidyverse.org/reference/scale_continuous.html).
See also [this GitHub issue
comment](https://github.com/tidyverse/ggplot2/issues/4021#issuecomment-650787582).
So we need these functions to flip the values along the y-axis and
negate the y-coordinates. This way, we can remove the negative sign from
the y-axis labels to mimick the behavior of `scale_y_reverse()`.#'
