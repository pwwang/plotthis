# Bar plot with groups

Create a bar plot with groups.

## Usage

``` r
BarPlotGrouped(
  data,
  x,
  x_sep = "_",
  y = NULL,
  scale_y = FALSE,
  flip = FALSE,
  group_by,
  group_by_sep = "_",
  group_name = NULL,
  theme = "theme_this",
  theme_args = list(),
  palette = "Paired",
  palcolor = NULL,
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
  alpha = 1,
  x_text_angle = 0,
  aspect.ratio = 1,
  add_line = NULL,
  line_color = "red2",
  line_width = 0.6,
  line_type = 2,
  line_name = NULL,
  add_trend = FALSE,
  trend_color = "black",
  trend_linewidth = 1,
  trend_ptsize = 2.5,
  position = "auto",
  position_dodge_preserve = "total",
  y_min = NULL,
  y_max = NULL,
  legend.position = "right",
  legend.direction = "vertical",
  title = NULL,
  subtitle = NULL,
  xlab = NULL,
  ylab = NULL,
  keep_na = FALSE,
  keep_empty = FALSE,
  expand = waiver(),
  width = 0.8,
  facet_by = NULL,
  facet_scales = "fixed",
  ...
)
```

## Arguments

- data:

  A data frame.

- x:

  A character string specifying the column name of the data frame to
  plot for the x-axis.

- x_sep:

  A character string to concatenate the columns in `x`, if multiple
  columns are provided.

- y:

  A character string specifying the column name of the data frame to
  plot for the y-axis.

- scale_y:

  A logical value indicating whether to scale the total y values in each
  group to 100%. Only works when group_by is specified.

- flip:

  A logical value indicating whether to flip the x and y axes.

- group_by:

  A character vector specifying the column as the group_by of the plot.
  A character/factor column is expected.

- group_by_sep:

  A character string to concatenate the columns in `group_by`, if
  multiple columns are provided.

- group_name:

  A character string to specify the name of the group_by in the legend.

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

- add_bg:

  A logical value indicating whether to add a background to the plot.

- bg_palette:

  A character string indicating the palette to use for the background.

- bg_palcolor:

  A character string indicating the color to use for the background.

- bg_alpha:

  A numeric value indicating the alpha of the background.

- alpha:

  A numeric value specifying the transparency of the plot.

- x_text_angle:

  A numeric value specifying the angle of the x-axis text.

- aspect.ratio:

  A numeric value specifying the aspect ratio of the plot.

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

- position:

  A character string indicating the position of the bars. If "auto", the
  position will be "stack" if group_by has more than 5 levels, otherwise
  "dodge". "fill" is also a valid option. Only works when group_by is
  not NULL.

- position_dodge_preserve:

  Should dodging preserve the "total" width of all elements at a
  position, or the width of a "single" element?

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

  Logical or character. Whether to keep rows with NA values on
  categorical axes.

  - `FALSE` (default): Remove rows with NA values in categorical axes.

  - `TRUE`: Keep NA values and display them as a separate category
    (shown as "NA"). For plots with both x and y categorical, applies to
    both axes.

  - `"x"`: Keep NA values only on the x-axis, remove from y-axis.

  - `"y"`: Keep NA values only on the y-axis, remove from x-axis.

  - `c("x", "y")` or `"xy"`: Explicitly keep NA on both axes (same as
    `TRUE`).

  **Special cases:** For `AreaPlot`, `LinePlot`, and `TrendPlot`,
  keeping NA values would break the visual continuity. Setting
  `keep_na = TRUE` will raise an error for these plot types.

- keep_empty:

  Logical or character. Whether to keep unused factor levels on
  categorical axes.

  - `FALSE` (default): Drop unused factor levels via
    [`droplevels()`](https://rdrr.io/r/base/droplevels.html).

  - `TRUE`: Keep all factor levels defined in the data, even if they
    have no observations. For plots with both x and y categorical,
    applies to both axes.

  - `"x"`: Keep unused levels only on the x-axis, drop from y-axis.

  - `"y"`: Keep unused levels only on the y-axis, drop from x-axis.

  - `c("x", "y")` or `"xy"`: Explicitly keep unused levels on both axes
    (same as `TRUE`).

  **Note:** This parameter is distinct from `keep_na`. Use
  `keep_empty = TRUE` when you need to show all possible categories
  (e.g., all 12 months even if some have no data). For more complex
  completeness requirements, use
  [`tidyr::complete()`](https://tidyr.tidyverse.org/reference/complete.html)
  before plotting.

  **Backward compatibility:** If `keep_na` is not specified and
  `keep_empty` is provided, `keep_empty` will control both NA values and
  unused levels (legacy behavior).

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

- width:

  A numeric value specifying the width of the bars.

- facet_by:

  A character string specifying the column name of the data frame to
  facet the plot. Otherwise, the data will be split by `split_by` and
  generate multiple plots and combine them into one using
  [`patchwork::wrap_plots`](https://patchwork.data-imaginist.com/reference/wrap_plots.html)

- facet_scales:

  Whether to scale the axes of facets. Default is "fixed" Other options
  are "free", "free_x", "free_y". See
  [`ggplot2::facet_wrap`](https://ggplot2.tidyverse.org/reference/facet_wrap.html)

- ...:

  Additional arguments.

## Value

A ggplot object.
