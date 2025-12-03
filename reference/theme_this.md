# A ggplot2 theme and palettes for plotthis Borrowed from the `theme_this` function in the `SCP` pipeline

A ggplot2 theme and palettes for plotthis Borrowed from the `theme_this`
function in the `SCP` pipeline

## Usage

``` r
theme_this(aspect.ratio = NULL, base_size = NULL, font_family = NULL, ...)
```

## Arguments

- aspect.ratio:

  The aspect ratio of the plot

- base_size:

  The base size of the text If not specified, it will use the value from
  `getOption("theme_this.base_size", 12)`. If you want to change the
  default base size, you can set the option `theme_this.base_size`. This
  is applied to all plots using this theme.

- font_family:

  The font family of the text If not specified, it will use the value
  from `getOption("theme_this.font_family")`. If you want to change the
  default font family, you can set the option `theme_this.font_family`.
  This is applied to all plots using this theme. To list available font
  families, you can use the
  [`systemfonts::system_fonts()`](https://systemfonts.r-lib.org/reference/system_fonts.html)
  function.

- ...:

  Other arguments for
  [`theme()`](https://ggplot2.tidyverse.org/reference/theme.html)

## Value

A ggplot2 theme

## See also

<https://github.com/zhanghao-njmu/SCP>
