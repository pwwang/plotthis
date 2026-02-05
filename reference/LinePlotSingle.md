# LinePlotSingle

Line plot without groups.

## Usage

``` r
LinePlotSingle(
  data,
  x,
  y = NULL,
  fill_point_by_x = TRUE,
  color_line_by_x = TRUE,
  facet_by = NULL,
  add_bg = FALSE,
  bg_palette = "stripe",
  bg_palcolor = NULL,
  bg_alpha = 0.2,
  add_errorbars = FALSE,
  errorbar_width = 0.1,
  errorbar_alpha = 1,
  errorbar_color = "grey30",
  errorbar_linewidth = 0.75,
  errorbar_min = NULL,
  errorbar_max = NULL,
  errorbar_sd = NULL,
  highlight = NULL,
  highlight_size = pt_size - 0.75,
  highlight_color = "red2",
  highlight_alpha = 0.8,
  pt_alpha = 1,
  pt_size = 5,
  add_hline = FALSE,
  hline_type = "solid",
  hline_width = 0.5,
  hline_color = "black",
  hline_alpha = 1,
  line_type = "solid",
  line_width = 1,
  line_alpha = 0.8,
  theme = "theme_this",
  theme_args = list(),
  palette = "Paired",
  palcolor = NULL,
  x_text_angle = 0,
  aspect.ratio = 1,
  legend.position = "right",
  legend.direction = "vertical",
  title = NULL,
  subtitle = NULL,
  xlab = NULL,
  ylab = NULL,
  keep_empty = FALSE,
  keep_na = FALSE,
  ...
)
```

## Arguments

- data:

  A data frame.

- x:

  A character string specifying the column name of the data frame to
  plot for the x-axis.

- y:

  A character string specifying the column name of the data frame to
  plot for the y-axis.

- fill_point_by_x:

  A logical value indicating whether to color the points by the x-axis
  values. If FALSE, the lines will be colored a single color (the first
  color in the palette).

- color_line_by_x:

  A logical value indicating whether to color the lines by the x-axis
  values. If FALSE, the lines will be colored a single color (the first
  color in the palette).

- facet_by:

  A character string specifying the column name of the data frame to
  facet the plot. Otherwise, the data will be split by `split_by` and
  generate multiple plots and combine them into one using
  [`patchwork::wrap_plots`](https://patchwork.data-imaginist.com/reference/wrap_plots.html)

- add_bg:

  A logical value indicating whether to add a background to the plot.

- bg_palette:

  The palette to use for the background.

- bg_palcolor:

  The color to use for the background.

- bg_alpha:

  The alpha value of the background.

- add_errorbars:

  A logical value indicating whether to add error bars to the plot.

- errorbar_width:

  The width of the error bars.

- errorbar_alpha:

  The alpha value of the error bars.

- errorbar_color:

  The color to use for the error bars. If "line", the error bars will be
  colored the same as the lines.

- errorbar_linewidth:

  The line width of the error bars.

- errorbar_min:

  The column in the data frame containing the lower bound of the error
  bars.

- errorbar_max:

  The column in the data frame containing the upper bound of the error
  bars.

- errorbar_sd:

  The column in the data frame containing the standard deviation of the
  error bars. If errorbar_min and errorbar_max are not provided, this
  column will be used to calculate the error bars. errorbar_min = y -
  errorbar_sd, errorbar_max = y + errorbar_sd. If errorbar_min and
  errorbar_max are provided, this column will be ignored.

- highlight:

  A vector of indexes or rownames to select the points to highlight. It
  could also be an expression (in string) to filter the data.

- highlight_size:

  The size of the highlighted points.

- highlight_color:

  A character vector specifying the color of the highlighted points.
  Default is "red".

- highlight_alpha:

  A numeric value specifying the transparency of the highlighted points.
  Default is 1.

- pt_alpha:

  The alpha value of the points.

- pt_size:

  The size of the points.

- add_hline:

  A numeric value indicating the y-intercept of a horizontal line to add
  to the plot. If FALSE, no horizontal line will be added.

- hline_type:

  The type of line to draw for the horizontal line.

- hline_width:

  The width of the horizontal line.

- hline_color:

  The color of the horizontal line. When `group_by` is provided, this
  can be TRUE to use the same color as the lines.

- hline_alpha:

  The alpha value of the horizontal line.

- line_type:

  The type of line to draw.

- line_width:

  The width of the line.

- line_alpha:

  The alpha value of the line.

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

- x_text_angle:

  A numeric value specifying the angle of the x-axis text.

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

- keep_empty:

  One of FALSE, TRUE and "level". It can also take a named list to
  specify different behavior for different columns. Without a named
  list, the behavior applies to the categorical/character columns used
  on the plot, for example, the `x`, `group_by`, `fill_by`, etc.

  - `FALSE` (default): Drop empty factor levels from the data before
    plotting.

  - `TRUE`: Keep empty factor levels and show them as a separate
    category in the plot.

  - `"level"`: Keep empty factor levels, but do not show them in the
    plot. But they will be assigned colors from the palette to maintain
    consistency across multiple plots. Alias: `levels`

- keep_na:

  A logical value or a character to replace the NA values in the data.
  It can also take a named list to specify different behavior for
  different columns. If TRUE or NA, NA values will be replaced with NA.
  If FALSE, NA values will be removed from the data before plotting. If
  a character string is provided, NA values will be replaced with the
  provided string. If a named vector/list is provided, the names should
  be the column names to apply the behavior to, and the values should be
  one of TRUE, FALSE, or a character string. Without a named
  vector/list, the behavior applies to categorical/character columns
  used on the plot, for example, the `x`, `group_by`, `fill_by`, etc.

- ...:

  Additional arguments.
