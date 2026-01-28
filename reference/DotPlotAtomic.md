# Dot Plot without splitting the data

Dot Plot without splitting the data

## Usage

``` r
DotPlotAtomic(
  data,
  x,
  y,
  x_sep = "_",
  y_sep = "_",
  flip = FALSE,
  lollipop = FALSE,
  size_by = NULL,
  fill_by = NULL,
  fill_cutoff = NULL,
  fill_reverse = FALSE,
  size_name = NULL,
  fill_name = NULL,
  fill_cutoff_name = NULL,
  theme = "theme_this",
  theme_args = list(),
  palette = "Spectral",
  palcolor = NULL,
  alpha = 1,
  facet_by = NULL,
  facet_scales = "fixed",
  facet_ncol = NULL,
  facet_nrow = NULL,
  facet_byrow = TRUE,
  x_text_angle = 0,
  aspect.ratio = 1,
  legend.position = "right",
  legend.direction = "vertical",
  add_bg = FALSE,
  bg_palette = "stripe",
  bg_palcolor = NULL,
  bg_alpha = 0.2,
  bg_direction = c("vertical", "horizontal", "v", "h"),
  title = NULL,
  subtitle = NULL,
  xlab = NULL,
  ylab = NULL,
  keep_na = FALSE,
  keep_empty = FALSE,
  ...
)
```

## Arguments

- data:

  A data frame.

- x:

  A character vector specifying the column to use for the x-axis. Could
  be either numeric or factor/character. When multiple columns are
  provided, they will be concatenated with 'x_sep'.

- y:

  A character vector specifying the column to use for the y-axis. Could
  be either numeric or factor/character. When multiple columns are
  provided, they will be concatenated with 'y_sep'.

- x_sep:

  A character vector to concatenate multiple columns in x. Default is
  "\_".

- y_sep:

  A character vector to concatenate multiple columns in y. Default is
  "\_".

- flip:

  A logical value indicating whether to flip the x and y axes. Default
  is FALSE.

- lollipop:

  A logical value indicating whether to make it a lolipop plot. Default
  is FALSE. When TRUE, 'x' should be a numeric column and 'y' should be
  a factor/character column.

- size_by:

  Which column to use as the size of the dots. It must be a numeric
  column. If not provided, the size will be the count of the instances
  for each 'y' in 'x'. For 'ScatterPlot', it can be a single numeric
  value to specify the size of the dots.

- fill_by:

  Which column to use as the fill the dots. It must be a numeric column.
  If not provided, all dots will be filled with the same color at the
  middle of the palette.

- fill_cutoff:

  A numeric value specifying the cutoff for the fill column.

- fill_reverse:

  A logical value indicating whether to reverse the fill direction.
  Default is FALSE. By default, the fill direction is "up". If TRUE, the
  fill direction is "down". When the direction is "up", the values less
  than the cutoff will be filled with grey. When the direction is
  "down", the values greater than the cutoff will be filled with grey.

- size_name:

  A character vector specifying the name for the size legend.

- fill_name:

  A character vector specifying the name for the fill legend.

- fill_cutoff_name:

  A character vector specifying the name for the fill cutoff legend.

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

- add_bg:

  A logical value indicating whether to add a background color to the
  plot. Default is FALSE.

- bg_palette:

  A character vector specifying the palette for the background color.
  Default is "stripe".

- bg_palcolor:

  A character vector specifying the color for the background color.

- bg_alpha:

  A numeric value specifying the alpha for the background color. Default
  is 0.2.

- bg_direction:

  A character vector specifying the direction for the background color.
  Default is "vertical". Other options are "horizontal". "h" and "v" are
  also accepted.

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

- ...:

  Additional arguments.

## Value

A ggplot object
