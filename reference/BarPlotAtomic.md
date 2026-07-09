# Atomic bar plot (internal)

Dispatcher that routes to
[`BarPlotSingle`](https://pwwang.github.io/plotthis/reference/BarPlotSingle.md)
(when `group_by = NULL`) or
[`BarPlotGrouped`](https://pwwang.github.io/plotthis/reference/BarPlotGrouped.md)
(when `group_by` is provided). This is the core implementation layer —
it takes a **single** data frame (no `split_by` support) and returns a
`ggplot` object with faceting applied.

## Usage

``` r
BarPlotAtomic(
  data,
  x,
  x_sep = "_",
  y = NULL,
  scale_y = FALSE,
  flip = FALSE,
  group_by = NULL,
  group_by_sep = "_",
  group_name = NULL,
  fill_by = TRUE,
  fill_name = NULL,
  label_nudge = 0.02,
  label = NULL,
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
  palette = NULL,
  palcolor = NULL,
  palreverse = FALSE,
  alpha = 1,
  lower_quantile = 0,
  upper_quantile = 0.99,
  lower_cutoff = NULL,
  upper_cutoff = NULL,
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
  trend_ptsize = 2,
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
  width = waiver(),
  facet_by = NULL,
  facet_scales = "fixed",
  facet_nrow = NULL,
  facet_ncol = NULL,
  facet_byrow = TRUE,
  facet_args = list(),
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

  A character string to join multiple `x` columns. Default `"_"`.

- y:

  A character string specifying the column name of the data frame to
  plot for the y-axis.

- scale_y:

  A logical value. When `TRUE`, y-values are scaled to proportions
  within each x position so that each position's total is 100\\ Only
  applicable when `position = "stack"`.

- flip:

  Logical; if `TRUE`, swap the x and y axes.

- group_by:

  Columns to group the data for plotting For those plotting functions
  that do not support multiple groups, They will be concatenated into
  one column, using `group_by_sep` as the separator

- group_by_sep:

  The separator for multiple group_by columns. See `group_by`

- group_name:

  A character string for the group fill legend title. When `NULL`, the
  `group_by` column name is used.

- fill_by:

  A variable used to fill the bars. Both categorical and numeric columns
  are accepted:

  - `TRUE` (default) — fill by the x-axis values.

  - `FALSE` — solid fill (first palette colour).

  - A column name (character/factor) — discrete colour scale.

  - A column name (numeric) — continuous gradient with quantile / cutoff
    controls.

  Ignored when `group_by` is provided (fill is determined by
  `group_by`).

- fill_name:

  A character string for the fill legend title. Only applies when
  `group_by = NULL` and the fill is from `fill_by`.

- label_nudge:

  A numeric value controlling the distance between labels and the bar
  top, expressed as a fraction of the data range.

- label:

  A column name (or `TRUE`) for text labels on bars. When `TRUE`, the
  y-axis values are labelled. When a column name, the values in that
  column are used.

- label_fg:

  A character string specifying the label text colour.

- label_size:

  A numeric value specifying the label text size.

- label_bg:

  A character string specifying the label background colour.

- label_bg_r:

  A numeric value specifying the label background corner radius.

- add_bg:

  Logical; add alternating background stripes behind the bars.

- bg_palette:

  Palette for the background stripes.

- bg_palcolor:

  Custom colours for the background stripes.

- bg_alpha:

  Alpha transparency for the background stripes.

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

- palreverse:

  A logical value indicating whether to reverse the palette. Default is
  FALSE.

- alpha:

  A numeric value specifying the transparency of the plot.

- lower_quantile, upper_quantile:

  Lower and upper quantiles for the continuous color/fill scale. The
  actual cutoffs are determined by these quantiles when `lower_cutoff`
  and `upper_cutoff` are `NULL`. Defaults: `lower_quantile = 0`,
  `upper_quantile = 0.99`.

- lower_cutoff, upper_cutoff:

  Explicit lower and upper cutoffs for the continuous color/fill scale.
  When `NULL` (the default), the cutoffs are determined by
  `lower_quantile` and `upper_quantile` via
  [`quantile`](https://rdrr.io/r/stats/quantile.html). Values outside
  the `[lower_cutoff, upper_cutoff]` range are clamped (winsorized) to
  the nearest cutoff value.

- x_text_angle:

  A numeric value specifying the angle of the x-axis text.

- aspect.ratio:

  A numeric value specifying the aspect ratio of the plot.

- add_line:

  A numeric y-intercept for a horizontal reference line.

- line_color:

  Colour of the reference line.

- line_width:

  Width of the reference line.

- line_type:

  Linetype of the reference line (e.g., 1 = solid, 2 = dashed).

- line_name:

  Legend name for the reference line.

- add_trend:

  Logical; add a trend line and points connecting the bar tops.

- trend_color:

  Colour of the trend line.

- trend_linewidth:

  Width of the trend line.

- trend_ptsize:

  Size of the trend line points.

- position:

  A character string specifying the bar layout: `"auto"` (default: dodge
  when \\\le\\5 groups, stack otherwise), `"dodge"` (side-by-side), or
  `"stack"` (stacked on top of each other).

- position_dodge_preserve:

  A character string passed to
  [`position_dodge2()`](https://ggplot2.tidyverse.org/reference/position_dodge.html):
  `"total"` preserves the overall bar group width; `"single"` preserves
  individual bar widths.

- y_min, y_max:

  Numeric limits for the y-axis (or x-axis when flipped).

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

- width:

  A numeric value specifying the bar width (0–1).

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

- facet_args:

  A list of additional arguments passed to the faceting function (e.g.,
  `scales`, `labeller`).

- ...:

  Additional arguments.

## Value

A `ggplot` object, possibly faceted, with `height` and `width`
attributes (in inches) attached.

## Dispatch logic

- **Without `group_by`** — delegates to `BarPlotSingle`. `fill_by`
  controls bar colouring: `TRUE` (default) fills by x-axis values,
  `FALSE` uses a single colour, a character column produces discrete
  colours, and a numeric column produces a continuous gradient.

- **With `group_by`** — delegates to `BarPlotGrouped`. `fill_by` must
  match `group_by` or be left as default; an explicit mismatch raises a
  stop error. `position` controls dodge vs. stack layout with automatic
  selection based on group count (\\\le\\5 → dodge, \>5 → stack).

After the delegate returns,
[`facet_plot()`](https://pwwang.github.io/plotthis/reference/facet_plot.md)
wraps the result with `facet_wrap` / `facet_grid` if `facet_by` is
provided.
