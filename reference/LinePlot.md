# Line Plot

Visualizing the change of a numeric value over the progression of a
categorical variable.

## Usage

``` r
LinePlot(
  data,
  x,
  y = NULL,
  group_by = NULL,
  group_by_sep = "_",
  split_by = NULL,
  split_by_sep = "_",
  fill_point_by_x_if_no_group = TRUE,
  color_line_by_x_if_no_group = TRUE,
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
  keep_na = FALSE,
  keep_empty = FALSE,
  line_type = "solid",
  line_width = 1,
  line_alpha = 0.8,
  add_hline = FALSE,
  hline_type = "solid",
  hline_width = 0.5,
  hline_color = "black",
  hline_alpha = 1,
  theme = "theme_this",
  theme_args = list(),
  palette = "Paired",
  palcolor = NULL,
  x_text_angle = 0,
  aspect.ratio = 1,
  legend.position = "right",
  legend.direction = "vertical",
  facet_by = NULL,
  facet_scales = "fixed",
  combine = TRUE,
  nrow = NULL,
  ncol = NULL,
  byrow = TRUE,
  facet_nrow = NULL,
  facet_ncol = NULL,
  facet_byrow = TRUE,
  facet_args = list(),
  title = NULL,
  subtitle = NULL,
  xlab = NULL,
  ylab = NULL,
  seed = 8525,
  axes = NULL,
  axis_titles = axes,
  guides = NULL,
  design = NULL,
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

- group_by:

  Columns to group the data for plotting For those plotting functions
  that do not support multiple groups, They will be concatenated into
  one column, using `group_by_sep` as the separator

- group_by_sep:

  A character string specifying the separator to use when concatenating
  multiple columns.

- split_by:

  The column(s) to split data by and plot separately.

- split_by_sep:

  The separator for multiple split_by columns. See `split_by`

- fill_point_by_x_if_no_group:

  A logical value indicating whether to color the points by the x-axis
  values when there is no group_by column.

- color_line_by_x_if_no_group:

  A logical value indicating whether to color the lines by the x-axis
  values

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

- line_type:

  The type of line to draw.

- line_width:

  The width of the line.

- line_alpha:

  The alpha value of the line.

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

- facet_by:

  A character string specifying the column name of the data frame to
  facet the plot. Otherwise, the data will be split by `split_by` and
  generate multiple plots and combine them into one using
  [`patchwork::wrap_plots`](https://patchwork.data-imaginist.com/reference/wrap_plots.html)

- facet_scales:

  Whether to scale the axes of facets. Default is "fixed" Other options
  are "free", "free_x", "free_y". See
  [`ggplot2::facet_wrap`](https://ggplot2.tidyverse.org/reference/facet_wrap.html)

- combine:

  Whether to combine the plots into one when facet is FALSE. Default is
  TRUE.

- nrow:

  A numeric value specifying the number of rows in the facet.

- ncol:

  A numeric value specifying the number of columns in the facet.

- byrow:

  A logical value indicating whether to fill the plots by row.

- facet_nrow:

  A numeric value specifying the number of rows in the facet. When
  facet_by is a single column and facet_wrap is used.

- facet_ncol:

  A numeric value specifying the number of columns in the facet. When
  facet_by is a single column and facet_wrap is used.

- facet_byrow:

  A logical value indicating whether to fill the plots by row. Default
  is TRUE.

- facet_args:

  A list of arguments to pass to
  [`ggplot2::facet_wrap()`](https://ggplot2.tidyverse.org/reference/facet_wrap.html)
  or
  [`ggplot2::facet_grid()`](https://ggplot2.tidyverse.org/reference/facet_grid.html).
  when there is no group_by column.

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

- seed:

  The random seed to use. Default is 8525.

- axes:

  A string specifying how axes should be treated. Passed to
  [`patchwork::wrap_plots()`](https://patchwork.data-imaginist.com/reference/wrap_plots.html).
  Only relevant when `split_by` is used and `combine` is TRUE. Options
  are:

  - 'keep' will retain all axes in individual plots.

  - 'collect' will remove duplicated axes when placed in the same run of
    rows or columns of the layout.

  - 'collect_x' and 'collect_y' will remove duplicated x-axes in the
    columns or duplicated y-axes in the rows respectively.

- axis_titles:

  A string specifying how axis titltes should be treated. Passed to
  [`patchwork::wrap_plots()`](https://patchwork.data-imaginist.com/reference/wrap_plots.html).
  Only relevant when `split_by` is used and `combine` is TRUE. Options
  are:

  - 'keep' will retain all axis titles in individual plots.

  - 'collect' will remove duplicated titles in one direction and merge
    titles in the opposite direction.

  - 'collect_x' and 'collect_y' control this for x-axis titles and
    y-axis titles respectively.

- guides:

  A string specifying how guides should be treated in the layout. Passed
  to
  [`patchwork::wrap_plots()`](https://patchwork.data-imaginist.com/reference/wrap_plots.html).
  Only relevant when `split_by` is used and `combine` is TRUE. Options
  are:

  - 'collect' will collect guides below to the given nesting level,
    removing duplicates.

  - 'keep' will stop collection at this level and let guides be placed
    alongside their plot.

  - 'auto' will allow guides to be collected if a upper level tries, but
    place them alongside the plot if not.

- design:

  Specification of the location of areas in the layout, passed to
  [`patchwork::wrap_plots()`](https://patchwork.data-imaginist.com/reference/wrap_plots.html).
  Only relevant when `split_by` is used and `combine` is TRUE. When
  specified, `nrow`, `ncol`, and `byrow` are ignored. See
  [`patchwork::wrap_plots()`](https://patchwork.data-imaginist.com/reference/wrap_plots.html)
  for more details.

- ...:

  Additional arguments.

## Value

A ggplot object or wrap_plots object or a list of ggplot objects

## Examples

``` r
# \donttest{
data <- data.frame(
   x = factor(c("A", "B", "C", "D", "A", "B", "C", "D"), levels = LETTERS[1:6]),
   y = c(10, 8, 16, 4, 6, 12, 14, 2),
   group = c("G1", "G1", "G1", "G1", "G2", "G2", "G2", "G2"),
   facet = c("F1", "F1", "F2", "F2", "F3", "F3", "F4", "F4")
)

LinePlot(data, x = "x", y = "y")

LinePlot(data, x = "x", y = "y", highlight = "group == 'G1'",
   fill_point_by_x_if_no_group = FALSE, color_line_by_x_if_no_group = FALSE)

LinePlot(data, x = "x", y = "y", group_by = "group")

LinePlot(data, x = "x", y = "y", group_by = "group",
   add_hline = 10, hline_color = "red")

LinePlot(data, x = "x", y = "y", group_by = "group", add_bg = TRUE,
   highlight = "y > 10")

LinePlot(data, x = "x", y = "y", group_by = "group", facet_by = "facet")

LinePlot(data, x = "x", y = "y", group_by = "group", split_by = "facet")

LinePlot(data, x = "x", y = "y", split_by = "group",
         palcolor = list(G1 = c("red", "blue"), G2 = c("green", "black")))


# keep_na and keep_empty
data <- data.frame(
   x = factor(c("A", "B", NA, "D", "A", "B", NA, "D"), levels = LETTERS[1:4]),
   y = c(10, 8, 16, 4, 6, 12, 14, 2),
   group = factor(c("G1", "G1", "G1", NA, NA, "G3", "G3", "G3"),
     levels = c("G1", "G2", "G3")),
   facet = c("F1", "F1", "F2", "F2", "F3", "F3", "F4", "F4")
)

LinePlot(data, x = "x", y = "y", keep_na = TRUE)

LinePlot(data, x = "x", y = "y", keep_empty = TRUE)

LinePlot(data, x = "x", y = "y", keep_empty = 'level')

LinePlot(data, x = "x", y = "y", group_by = "group", keep_na = TRUE)

LinePlot(data, x = "x", y = "y", group_by = "group", keep_empty = TRUE)

LinePlot(data, x = "x", y = "y", group_by = "group",
   keep_empty = list(x = TRUE, group = 'level'))

# }
```
