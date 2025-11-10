# Expand the plot area with CSS-like padding

Expand the plot area with CSS-like padding

## Usage

``` r
norm_expansion(
  expand,
  x_type,
  y_type,
  continuous_default = c(0.05, 0),
  discrete_default = c(0, 0.6)
)
```

## Arguments

- expand:

  A numeric vector of length 1, 2, 3, or 4 The values to expand the x
  and y axes. It is like CSS padding. When a single value is provided,
  it is used for both axes on both sides. When two values are provided,
  the first value is used for the top/bottom side and the second value
  is used for the left/right side. When three values are provided, the
  first value is used for the top side, the second value is used for the
  left/right side, and the third value is used for the bottom side. When
  four values are provided, the values are used for the top, right,
  bottom, and left sides, respectively. You can also use a named vector
  to specify the values for each side. When the axis is discrete, the
  values will be applied as 'add' to the 'expansion' function. When the
  axis is continuous, the values will be applied as 'mult' to the
  'expansion' function. See also
  <https://ggplot2.tidyverse.org/reference/expansion.html>

- x_type:

  The type of x-axis, either "continuous" or "discrete"

- y_type:

  The type of y-axis, either "continuous" or "discrete"

## Value

A list with x and y values for expand
