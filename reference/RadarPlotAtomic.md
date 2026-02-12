# Atomic Radar plot

Atomic Radar plot

## Usage

``` r
RadarPlotAtomic(
  data,
  x,
  x_sep = "_",
  group_by = NULL,
  group_by_sep = "_",
  y = NULL,
  group_name = NULL,
  groups = NULL,
  scale_y = c("group", "global", "x", "none"),
  y_min = 0,
  y_max = NULL,
  y_nbreaks = 4,
  polygon = FALSE,
  fill = TRUE,
  linewidth = 1,
  pt_size = 4,
  max_charwidth = 16,
  bg_color = "grey80",
  bg_alpha = 0.1,
  theme = "theme_this",
  theme_args = list(),
  palette = "Paired",
  palcolor = NULL,
  facet_by = NULL,
  facet_scales = "fixed",
  facet_ncol = NULL,
  facet_nrow = NULL,
  facet_byrow = TRUE,
  alpha = 0.2,
  aspect.ratio = 1,
  legend.position = waiver(),
  legend.direction = "vertical",
  keep_na = FALSE,
  keep_empty = FALSE,
  title = NULL,
  subtitle = NULL,
  ...
)
```

## Arguments

- data:

  A data frame.

- x:

  A character string of the column name to plot on the x-axis/circles. A
  character/factor column is expected.

- x_sep:

  A character string to concatenate the columns in `x`, if multiple
  columns are provided.

- group_by:

  A character string of the column name(s) to group the data (the lines)
  by. Character/factor column(s) is expected.

- group_by_sep:

  A character string to concatenate the columns in `group_by`, if
  multiple columns are provided.

- y:

  A character string of the column name to plot on the y-axis. A numeric
  column is expected. If NULL, the count of the x-axis column in each
  group will be used.

- group_name:

  A character string to name the legend of group.

- groups:

  A character vector of group values (in the `group_by` column) to
  include in the plot. If NULL, all groups will be included. This can be
  used to exclude certain groups from the plot or to specify the order
  of groups in the legend. Only applicable when `group_by` is provided.
  And this implies `keep_empty` for `group_by` is FALSE, which means the
  groups not in the data will not be shown in the legend.

- scale_y:

  How should the y-axis be scaled? Default is "group". Other options are
  "global", "x" and "none".

  - If "group", the y-axis will be scaled to the fraction within each
    group.

  - If "global", the y-axis will be scaled to the fraction of the total.

  - If "x", the y-axis will be scaled to the fraction of the total
    within each x-axis group.

  - If "none", the y-axis will be scaled to the count of each x-axis
    group.

- y_min:

  A numeric value to set the minimum value of the y-axis.

- y_max:

  A numeric value to set the maximum value of the y-axis.

- y_nbreaks:

  A numeric value to set the number of breaks in the y-axis.

- polygon:

  A logical value to draw the polygons instead of the circles as panel
  grid.

- fill:

  A logical value to fill the polygons with colors.

- linewidth:

  A numeric value to set the width of the lines.

- pt_size:

  A numeric value to set the size of the points.

- max_charwidth:

  A numeric value to set the maximum character width for the x labels.

- bg_color:

  A character string to set the background color of the plot.

- bg_alpha:

  A numeric value to set the transparency of the background color.

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

- facet_ncol:

  A numeric value specifying the number of columns in the facet. When
  facet_by is a single column and facet_wrap is used.

- facet_nrow:

  A numeric value specifying the number of rows in the facet. When
  facet_by is a single column and facet_wrap is used.

- facet_byrow:

  A logical value indicating whether to fill the plots by row. Default
  is TRUE.

- alpha:

  A numeric value specifying the transparency of the plot.

- aspect.ratio:

  A numeric value specifying the aspect ratio of the plot.

- legend.position:

  A character string specifying the position of the legend. if
  `waiver()`, for single groups, the legend will be "none", otherwise
  "right".

- legend.direction:

  A character string specifying the direction of the legend.

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

- title:

  A character string specifying the title of the plot. A function can be
  used to generate the title based on the default title. This is useful
  when split_by is used and the title needs to be dynamic.

- subtitle:

  A character string specifying the subtitle of the plot.

- ...:

  Additional arguments.

## Value

A ggplot object
