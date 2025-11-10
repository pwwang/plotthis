# Combine plots into one

Combine plots into one

## Usage

``` r
combine_plots(
  plots,
  combine = TRUE,
  nrow = NULL,
  ncol = NULL,
  byrow = NULL,
  axes = NULL,
  axis_titles = NULL,
  guides = NULL,
  design = NULL,
  recalc_size = TRUE
)
```

## Arguments

- plots:

  A list of plots

- combine:

  Whether to combine the plots into one

- nrow:

  The number of rows in the combined plot

- ncol:

  The number of columns in the combined plot

- byrow:

  Whether to fill the plots by row

- recalc_size:

  Whether to re-calculate the size of the combined plot

## Value

The faceted plot. If guess_size is TRUE, attr(p, "height") and attr(p,
"width") will be set
