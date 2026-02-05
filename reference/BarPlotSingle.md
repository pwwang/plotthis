# BarPlotSingle

Create a bar plot without groups.

## Usage

``` r
BarPlotSingle(
  data,
  x,
  x_sep = "_",
  y = NULL,
  flip = FALSE,
  facet_by = NULL,
  facet_scales = "fixed",
  label = NULL,
  label_nudge = 0.02,
  label_fg = "black",
  label_size = 4,
  label_bg = "white",
  label_bg_r = 0.1,
  add_bg = FALSE,
  bg_palette = "stripe",
  bg_palcolor = NULL,
  bg_alpha = 0.2,
  theme = "theme_this",
  theme_args = list(),
  palette = "Paired",
  palcolor = NULL,
  alpha = 1,
  x_text_angle = 0,
  aspect.ratio = 1,
  y_min = NULL,
  y_max = NULL,
  legend.position = "right",
  legend.direction = "vertical",
  add_line = NULL,
  line_color = "red2",
  line_width = 0.6,
  line_type = 2,
  line_name = NULL,
  add_trend = FALSE,
  trend_color = "black",
  trend_linewidth = 1,
  trend_ptsize = 2.5,
  title = NULL,
  subtitle = NULL,
  xlab = NULL,
  ylab = NULL,
  keep_na = FALSE,
  keep_empty = FALSE,
  expand = waiver(),
  fill_by_x = TRUE,
  width = 0.9,
  ...
)
```

## Arguments

- data:

  A data frame.

- x:

  A character vector specifying the column as the x axis of the plot. A
  character/factor column is expected.

- x_sep:

  A character string to concatenate the columns in `x`, if multiple
  columns are provided.

- y:

  A character vector specifying the column as the y axis of the plot.
  Default is NULL, meaning the y axis is the count of the data.

- flip:

  A logical value indicating whether to flip the x and y axes.

- facet_by:

  A character string specifying the column name of the data frame to
  facet the plot. Otherwise, the data will be split by `split_by` and
  generate multiple plots and combine them into one using
  [`patchwork::wrap_plots`](https://patchwork.data-imaginist.com/reference/wrap_plots.html)

- facet_scales:

  Whether to scale the axes of facets. Default is "fixed" Other options
  are "free", "free_x", "free_y". See
  [`ggplot2::facet_wrap`](https://ggplot2.tidyverse.org/reference/facet_wrap.html)

- label:

  A column name for the values to be displayed on the top of the bars.
  If TRUE, the y values will be displayed.

- label_nudge:

  A numeric value to nudge the labels (the distance between the label
  and the top of the bar).

- label_fg:

  A character string indicating the color of the label.

- label_size:

  A numeric value indicating the size of the label.

- label_bg:

  A character string indicating the background color of the label.

- label_bg_r:

  A numeric value indicating the radius of the background.

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

- alpha:

  A numeric value specifying the transparency of the plot.

- x_text_angle:

  A numeric value specifying the angle of the x-axis text.

- aspect.ratio:

  A numeric value specifying the aspect ratio of the plot.

- y_min:

  A numeric value to specify the minimum value of the y axis.

- y_max:

  A numeric value to specify the maximum value of the y axis.

- legend.position:

  A character string specifying the position of the legend. if
  `waiver()`, for single groups, the legend will be "none", otherwise
  "right".

- legend.direction:

  A character string specifying the direction of the legend.

- add_line:

  A numeric value indicating the y value to add a horizontal line.

- line_color:

  A character string indicating the color of the line.

- line_width:

  A numeric value indicating the size of the line.

- line_type:

  A numeric value indicating the type of the line.

- line_name:

  A character string indicating the name of the line.

- add_trend:

  A logical value to add trend line to the plot.

- trend_color:

  A character string to specify the color of the trend line.

- trend_linewidth:

  A numeric value to specify the width of the trend line.

- trend_ptsize:

  A numeric value to specify the size of the trend line points.

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

- fill_by_x:

  A logical value indicating whether to fill the bars by the x-axis
  values. If FALSE, the bars will be filled a single color (the first
  color in the palette).

- width:

  A numeric value specifying the width of the bars.

- ...:

  Additional arguments.

## Value

A ggplot object.
