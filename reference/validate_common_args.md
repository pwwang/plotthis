# Validate common arguments

Validate common arguments

## Usage

``` r
validate_common_args(
  seed,
  facet_by = NULL,
  plot_type = NULL,
  split_by = NULL,
  split_by_sep = "_",
  group_by = NULL,
  group_by_sep = "_",
  facet_scales = "fixed",
  facet_nrow = NULL,
  facet_ncol = NULL,
  facet_byrow = TRUE,
  theme = "theme_scp",
  theme_args = list(),
  palette = NULL,
  palcolor = NULL,
  expand = NULL,
  keep_na = FALSE,
  keep_empty = FALSE,
  alpha = 1,
  x_text_angle = 0,
  aspect.ratio = 1,
  legend.position = "right",
  legend.direction = "vertical",
  title = NULL,
  subtitle = NULL,
  xlab = NULL,
  ylab = NULL,
  combine = TRUE,
  nrow = NULL,
  ncol = NULL,
  byrow = TRUE,
  ...
)
```

## Arguments

- seed:

  The random seed to use. Default is 8525.

- facet_by:

  A character string specifying the column name of the data frame to
  facet the plot. Otherwise, the data will be split by `split_by` and
  generate multiple plots and combine them into one using
  [`patchwork::wrap_plots`](https://patchwork.data-imaginist.com/reference/wrap_plots.html)

- split_by:

  The column(s) to split data by and plot separately.

- split_by_sep:

  The separator for multiple split_by columns. See `split_by`

- group_by:

  Columns to group the data for plotting For those plotting functions
  that do not support multiple groups, They will be concatenated into
  one column, using `group_by_sep` as the separator

- group_by_sep:

  The separator for multiple group_by columns. See `group_by`

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

- alpha:

  A numeric value specifying the transparency of the plot.

- x_text_angle:

  A numeric value specifying the angle of the x-axis text.

- aspect.ratio:

  A numeric value specifying the aspect ratio of the plot.

- legend.position:

  A character string specifying the position of the legend. if
  [`waiver()`](https://ggplot2.tidyverse.org/reference/waiver.html), for
  single groups, the legend will be "none", otherwise "right".

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

- combine:

  Whether to combine the plots into one when facet is FALSE. Default is
  TRUE.

- nrow:

  A numeric value specifying the number of rows in the facet.

- ncol:

  A numeric value specifying the number of columns in the facet.

- byrow:

  A logical value indicating whether to fill the plots by row.

- ...:

  Additional arguments.
