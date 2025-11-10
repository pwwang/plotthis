# Color palettes collected in plotthis.

Color palettes collected in plotthis.

## Usage

``` r
palette_this(
  x,
  n = 100,
  palette = "Paired",
  palcolor = NULL,
  type = "auto",
  keep_names = TRUE,
  alpha = 1,
  matched = FALSE,
  reverse = FALSE,
  NA_keep = FALSE,
  NA_color = "grey80",
  transparent = TRUE
)
```

## Arguments

- x:

  A vector of character/factor or numeric values. If missing, numeric
  values 1:n will be used as x.

- n:

  The number of colors to return for numeric values.

- palette:

  Palette name. All available palette names can be queried with
  [`show_palettes()`](https://pwwang.github.io/plotthis/reference/show_palettes.md).

- palcolor:

  Custom colors used to create a color palette.

- type:

  Type of `x`. Can be one of "auto", "discrete" or "continuous". The
  default is "auto", which automatically detects if `x` is a numeric
  value.

- keep_names:

  Whether to keep the names of the color vector.

- alpha:

  The alpha value of the colors. Default is 1.

- matched:

  If `TRUE`, will return a color vector of the same length as `x`.

- reverse:

  Whether to invert the colors.

- NA_keep:

  Whether to keep the color assignment to NA in `x`.

- NA_color:

  Color assigned to NA if NA_keep is `TRUE`.

- transparent:

  Whether to make the colors transparent when alpha \< 1. When `TRUE`,
  [`ggplot2::alpha()`](https://ggplot2.tidyverse.org/reference/reexports.html)
  is used to make the colors transparent. Otherwise, `adjcolors` is used
  to adjust the colors based on the alpha. The color will be not be
  actually transparent. For example,
  `ggplot2::alpha("red", 0.5) == "#FF000080"`; while
  `adjcolors("red", 0.5) == "#FF8080"`.

## Value

A vector of colors.
