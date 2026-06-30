# Single bar plot (no groups)

Core implementation for drawing a bar plot without grouping — each
x-axis category is a single bar. This is the simpler code path
dispatched by
[`BarPlotAtomic`](https://pwwang.github.io/plotthis/reference/BarPlotAtomic.md)
when `group_by = NULL`. Bars can be filled by a categorical variable, a
continuous variable (numeric colour gradient), or a solid colour.

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
  fill_by = TRUE,
  fill_name = NULL,
  width = 0.9,
  ...
)
```

## Arguments

- data:

  A data frame.

- x:

  A character vector of column name(s) for the x-axis. Character/factor
  columns are expected. Multiple columns are concatenated with `x_sep`.

- x_sep:

  A character string to join multiple `x` columns. Default `"_"`.

- y:

  A character string specifying the numeric column for the y-axis.
  Default `NULL` — the count of observations per x category is used.

- flip:

  Logical; if `TRUE`, swap the x and y axes.

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

  A column name (or `TRUE`) for text labels on bars. When `TRUE`, the
  y-axis values are labelled. When a column name, the values in that
  column are used.

- label_nudge:

  A numeric value controlling the distance between labels and the bar
  top, expressed as a fraction of the data range.

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

- y_min, y_max:

  Numeric limits for the y-axis (or x-axis when flipped).

- legend.position:

  A character string specifying the position of the legend. if
  `waiver()`, for single groups, the legend will be "none", otherwise
  "right".

- legend.direction:

  A character string specifying the direction of the legend.

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

- fill_by:

  A variable used to fill the bars. Can be `TRUE` (default; fill by
  x-axis values), `FALSE` (solid fill), or a column name (categorical or
  numeric). When `group_by` is used in
  [`BarPlotAtomic`](https://pwwang.github.io/plotthis/reference/BarPlotAtomic.md),
  this parameter is ignored.

- fill_name:

  A character string for the fill legend title.

- width:

  A numeric value specifying the bar width (0–1).

- ...:

  Additional arguments.

## Value

A `ggplot` object with `height` and `width` attributes (in inches)
attached.

## Architecture

1.  **Column resolution** — `x`, `y`, and `facet_by` are validated and
    transformed via
    [`check_columns`](https://pwwang.github.io/plotthis/reference/check_columns.md).
    Multi-column `x` is concatenated with `x_sep`.

2.  **Count aggregation** — when `y = NULL`, the count of observations
    per (`x`, `facet_by`) combination is computed as a new `.y` column.
    Factor levels are preserved.

3.  **Fill resolution** — `fill_by` can be `TRUE` (use x values),
    `FALSE`/`NULL` (solid fill), a categorical column name (discrete
    colour scale), or a numeric column name (continuous gradient).
    Discrete fills use
    [`palette_this()`](https://pwwang.github.io/plotthis/reference/palette_this.md)
    for colour assignment; numeric fills use
    [`prepare_continuous_color_scale()`](https://pwwang.github.io/plotthis/reference/prepare_continuous_color_scale.md)
    with quantile / cutoff clamping and `scale_fill_gradientn()`.

4.  **Background stripes** — when `add_bg = TRUE`,
    [`bg_layer()`](https://pwwang.github.io/plotthis/reference/bg_layer.md)
    adds alternating horizontal or vertical stripe fills behind the
    bars.

5.  **Labels** — when `label` is set, values are displayed on or near
    the bar tops via
    [`geom_text_repel()`](https://ggrepel.slowkow.com/reference/geom_text_repel.html)
    (non-flipped) or `geom_text()` (flipped). The y-position is nudged
    by `label_nudge` × the data range.

6.  **Trend line** — when `add_trend = TRUE`, a line and points are
    overlaid across the bar tops.

7.  **Horizontal reference line** — `add_line` draws a horizontal line
    at the specified y-value, with a colour legend entry named by
    `line_name`.

8.  **Dimension calculation** —
    [`calculate_plot_dimensions()`](https://pwwang.github.io/plotthis/reference/calculate_plot_dimensions.md)
    computes plot height and width from the x-axis category count, label
    character widths, legend metrics, and flip state. When flipped,
    height scales with the number of x categories.

9.  **Coordinate transform** — when `flip = TRUE`, `coord_flip()` swaps
    axes; otherwise `coord_cartesian()` applies `y_min` / `y_max`
    limits. Free-scale faceting skips limit constraints.
