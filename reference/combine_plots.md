# Combine plots into one

Combine plots into one

## Usage

``` r
combine_plots(
  plots,
  combine = TRUE,
  split_by = NULL,
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

- split_by:

  The column name to split the plots by. When provided, the combined
  data from all sub-plots is available via `p$data`.

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
