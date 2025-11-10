# Atomic function for clustree plot

Atomic function for clustree plot

## Usage

``` r
ClustreePlotAtomic(
  data,
  prefix,
  flip = FALSE,
  alpha = 0.85,
  palette = "Paired",
  palcolor = NULL,
  edge_palette = "Spectral",
  edge_palcolor = NULL,
  aspect.ratio = 1,
  legend.position = "right",
  legend.direction = "vertical",
  title = NULL,
  subtitle = NULL,
  xlab = NULL,
  ylab = NULL,
  expand = c(0.1, 0.1),
  theme = "theme_this",
  theme_args = list(),
  ...
)
```

## Arguments

- data:

  A data frame.

- prefix:

  A character string of the prefix of the columns to plot. The columns
  with the prefix will be used to plot the tree.

- flip:

  A logical value to flip the tree.

- alpha:

  A numeric value of the transparency of the nodes. Only used when
  `node_alpha` is not provided in `...`.

- palette:

  A character string specifying the palette to use. A named list or
  vector can be used to specify the palettes for different `split_by`
  values.

- palcolor:

  A character string specifying the color to use in the palette. A named
  list can be used to specify the colors for different `split_by`
  values. If some values are missing, the values from the palette will
  be used (palcolor will be NULL for those values).

- edge_palette:

  A character string of the palette name to color the edges.

- edge_palcolor:

  A character vector of colors to color the edges.

- aspect.ratio:

  A numeric value specifying the aspect ratio of the plot.

- legend.position:

  A character string specifying the position of the legend. if
  `waiver()`, for single groups, the legend will be "none", otherwise
  "right".

- legend.direction:

  A character string specifying the direction of the legend.

- title:

  A character string specifying the title of the plot. A function can be
  used to generate the title based on the default title. This is useful
  when split_by is used and the title needs to be dynamic.

- subtitle:

  A character string specifying the subtitle of the plot.

- xlab:

  A character string specifying the x-axis label.

- ylab:

  A character string specifying the y-axis label.

- expand:

  The values to expand the x and y axes. It is like CSS padding. When a
  single value is provided, it is used for both axes on both sides. When
  two values are provided, the first value is used for the top/bottom
  side and the second value is used for the left/right side. When three
  values are provided, the first value is used for the top side, the
  second value is used for the left/right side, and the third value is
  used for the bottom side. When four values are provided, the values
  are used for the top, right, bottom, and left sides, respectively. You
  can also use a named vector to specify the values for each side. When
  the axis is discrete, the values will be applied as 'add' to the
  'expansion' function. When the axis is continuous, the values will be
  applied as 'mult' to the 'expansion' function. See also
  <https://ggplot2.tidyverse.org/reference/expansion.html>

- theme:

  A character string or a theme class (i.e. ggplot2::theme_classic)
  specifying the theme to use. Default is "theme_this".

- theme_args:

  A list of arguments to pass to the theme function.

- ...:

  Other arguments passed to
  [`clustree::clustree`](https://lazappi.github.io/clustree/reference/clustree.html).
