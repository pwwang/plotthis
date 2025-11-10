# SplitBarPlotAtomic

Create a split bar plot without splitting the data.

## Usage

``` r
SplitBarPlotAtomic(
  data,
  x,
  y,
  y_sep = "_",
  flip = FALSE,
  alpha_by = NULL,
  alpha_reverse = FALSE,
  alpha_name = NULL,
  order_y = list(`+` = c("x_desc", "alpha_desc"), `-` = c("x_desc", "alpha_asc")),
  bar_height = 0.9,
  lineheight = 0.5,
  max_charwidth = 80,
  fill_by = NULL,
  fill_by_sep = "_",
  fill_name = NULL,
  direction_pos_name = "positive",
  direction_neg_name = "negative",
  theme = "theme_this",
  theme_args = list(),
  palette = "Spectral",
  palcolor = NULL,
  facet_by = NULL,
  facet_scales = "free_y",
  facet_nrow = NULL,
  facet_ncol = NULL,
  facet_byrow = TRUE,
  aspect.ratio = 1,
  x_min = NULL,
  x_max = NULL,
  legend.position = "right",
  legend.direction = "vertical",
  title = NULL,
  subtitle = NULL,
  xlab = NULL,
  ylab = NULL,
  keep_empty = FALSE,
  ...
)
```

## Arguments

- data:

  A data frame.

- x:

  The column name of the terms on the x axis. There should be both
  negative and positive values.

- y:

  The column name(s) of the values. If there are multiple columns, they
  will be concatenated.

- y_sep:

  A character string to concatenate the x columns if there are multiple.

- flip:

  A logical value indicating whether to flip the x and y axes.

- alpha_by:

  A character string indicating the column name to use for the
  transparency of the bars.

- alpha_reverse:

  A logical value indicating whether to reverse the transparency.

- alpha_name:

  A character string indicating the legend name of the transparency.

- order_y:

  A list of character strings indicating the order of the y axis. The
  keys are "+", "-", or "*". However, "+/-" should not be mixed with
  "*". The values are "x_asc", "x_desc", "alpha_asc", or "alpha_desc",
  indicating how to order the y axis. The default is
  `list("+" = c("x_desc", "alpha_desc"), "-" = c("x_desc", "alpha_asc"))`,
  meaning the positive values are ordered by the x-axis values in
  descending order and the alpha values in descending order, and the
  negative values are ordered by the x-axis values in descending order
  and the alpha values in ascending order. The "\*" key is used to order
  the y axis without considering the direction.

- bar_height:

  A numeric value indicating the height of the bars.

- lineheight:

  A numeric value indicating the height of the text.

- max_charwidth:

  A numeric value indicating the maximum width of the text.

- fill_by:

  A character string indicating the column name to use for the fill of
  the bars.

- fill_by_sep:

  A character string to concatenate the fill columns if there are
  multiple.

- fill_name:

  A character string indicating the legend name of the fill.

- direction_pos_name:

  A character string indicating the name of the positive direction.

- direction_neg_name:

  A character string indicating the name of the negative direction.

- theme:

  A character string or a theme class (i.e. ggplot2::theme_classic)
  specifying the theme to use. Default is "theme_this".

- theme_args:

  A list of arguments to pass to the theme function.

- palette:

  A character string specifying the palette to use. A named list or
  vector can be used to specify the palettes for different `split_by`
  values.

- palcolor:

  A character string specifying the color to use in the palette. A named
  list can be used to specify the colors for different `split_by`
  values. If some values are missing, the values from the palette will
  be used (palcolor will be NULL for those values).

- facet_by:

  A character string specifying the column name of the data frame to
  facet the plot. Otherwise, the data will be split by `split_by` and
  generate multiple plots and combine them into one using
  [`patchwork::wrap_plots`](https://patchwork.data-imaginist.com/reference/wrap_plots.html)

- facet_scales:

  Whether to scale the axes of facets. Default is "fixed" Other options
  are "free", "free_x", "free_y". See
  [`ggplot2::facet_wrap`](https://ggplot2.tidyverse.org/reference/facet_wrap.html)

- facet_nrow:

  A numeric value specifying the number of rows in the facet. When
  facet_by is a single column and facet_wrap is used.

- facet_ncol:

  A numeric value specifying the number of columns in the facet. When
  facet_by is a single column and facet_wrap is used.

- facet_byrow:

  A logical value indicating whether to fill the plots by row. Default
  is TRUE.

- aspect.ratio:

  A numeric value specifying the aspect ratio of the plot.

- x_min:

  A numeric value indicating the minimum value of the x axis.

- x_max:

  A numeric value indicating the maximum value of the x axis.

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

- keep_empty:

  A logical value indicating whether to keep empty groups. If FALSE,
  empty groups will be removed.

- ...:

  Additional arguments.
