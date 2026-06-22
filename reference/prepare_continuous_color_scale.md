# Prepare continuous color scale limits with quantile/cutoff controls

Computes the lower and upper cutoffs for a continuous color/fill scale,
applies data winsorization (clamping), and returns the results needed by
[`scale_fill_gradientn`](https://ggplot2.tidyverse.org/reference/scale_gradient.html)
or
[`scale_color_gradientn`](https://ggplot2.tidyverse.org/reference/scale_gradient.html).

## Usage

``` r
prepare_continuous_color_scale(
  data,
  column,
  lower_quantile = 0,
  upper_quantile = 0.99,
  lower_cutoff = NULL,
  upper_cutoff = NULL,
  bg_cutoff = NULL
)
```

## Arguments

- data:

  A data frame.

- column:

  The column name in `data` to use for the color scale.

- lower_quantile, upper_quantile:

  Lower and upper quantiles for the continuous color/fill scale. The
  actual cutoffs are determined by these quantiles when `lower_cutoff`
  and `upper_cutoff` are `NULL`. Defaults: `lower_quantile = 0`,
  `upper_quantile = 0.99`.

- lower_cutoff, upper_cutoff:

  Explicit lower and upper cutoffs for the continuous color/fill scale.
  When `NULL` (the default), the cutoffs are determined by
  `lower_quantile` and `upper_quantile` via
  [`quantile`](https://rdrr.io/r/stats/quantile.html). Values outside
  the `[lower_cutoff, upper_cutoff]` range are clamped (winsorized) to
  the nearest cutoff value.

- bg_cutoff:

  Optional numeric cutoff — values `<= bg_cutoff` are set to `NA` before
  computing cutoffs. Default is `NULL`.

## Value

A list with components:

- data:

  The modified data frame (with winsorized `column`).

- feat_colors_value:

  Numeric vector of 100 evenly-spaced values from `lower_cutoff` to
  `upper_cutoff`.

- limits:

  Numeric vector of length 2 giving the range
  `c(lower_cutoff, upper_cutoff)`.
