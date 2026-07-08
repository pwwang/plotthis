# Box / Violin / Bar / Beeswarm plot

`BoxPlot` draws box plots or bar plots (mean ± error bars) with
extensive customisation options. Supports jittered or beeswarm points,
paired observations with connecting lines, trend lines, statistical test
annotations (pairwise or omnibus), background stripes, reference lines,
point highlighting, and custom summary statistic overlays.

This is the public API — it delegates to
[`BoxViolinPlot`](https://pwwang.github.io/plotthis/reference/BoxViolinPlot-internal.md)
with `base = "box"` or `base = "bar"`, which in turn dispatches to
[`BoxViolinPlotAtomic`](https://pwwang.github.io/plotthis/reference/BoxViolinPlotAtomic.md)
for each `split_by` level.

`ViolinPlot` draws violin plots with extensive customisation options.
Supports jittered or beeswarm points, box plot overlays, trend lines,
statistical test annotations, background stripes, reference lines, point
highlighting, and custom summary statistic overlays.

This is the public API — it delegates to
[`BoxViolinPlot`](https://pwwang.github.io/plotthis/reference/BoxViolinPlot-internal.md)
with `base = "violin"`, which dispatches to
[`BoxViolinPlotAtomic`](https://pwwang.github.io/plotthis/reference/BoxViolinPlotAtomic.md)
for each `split_by` level.

`BeeswarmPlot` draws beeswarm plots — points arranged by the beeswarm
algorithm to avoid overlap while displaying the distribution density.
This is a convenience wrapper that delegates to
[`BoxViolinPlot`](https://pwwang.github.io/plotthis/reference/BoxViolinPlot-internal.md)
with `base = "none"` and `add_beeswarm = TRUE`.

Requires the ggbeeswarm package. To get a beeswarm plot WITH a box plot,
use `BeeswarmPlot(..., add_box = TRUE)`. To get a violin plot with
beeswarm points, use `ViolinPlot(..., add_beeswarm = TRUE)`.

## Usage

``` r
BoxPlot(
  data,
  x,
  x_sep = "_",
  y = NULL,
  base = c("box", "bar"),
  in_form = c("long", "wide"),
  split_by = NULL,
  split_by_sep = "_",
  symnum_args = NULL,
  sort_x = NULL,
  flip = FALSE,
  keep_empty = FALSE,
  keep_na = FALSE,
  group_by = NULL,
  group_by_sep = "_",
  group_name = NULL,
  paired_by = NULL,
  x_text_angle = ifelse(isTRUE(flip), 0, 45),
  step_increase = 0.1,
  fill_mode = ifelse(!is.null(group_by), "dodge", "x"),
  palreverse = FALSE,
  position_dodge_preserve = "total",
  theme = "theme_this",
  theme_args = list(),
  palette = "Paired",
  palcolor = NULL,
  alpha = 1,
  aspect.ratio = NULL,
  legend.position = "right",
  legend.direction = "vertical",
  add_point = FALSE,
  pt_color = if (isTRUE(add_beeswarm)) NULL else "grey30",
  pt_size = NULL,
  pt_alpha = 1,
  jitter_width = NULL,
  jitter_height = 0,
  stack = FALSE,
  y_max = NULL,
  y_min = NULL,
  y_brackets = NULL,
  add_beeswarm = FALSE,
  beeswarm_method = "swarm",
  beeswarm_cex = 1,
  beeswarm_priority = "ascending",
  beeswarm_dodge = 0.9,
  add_trend = FALSE,
  trend_color = NULL,
  trend_linewidth = 1,
  trend_ptsize = 2,
  add_stat = NULL,
  stat_name = NULL,
  stat_color = "black",
  stat_size = 1,
  stat_stroke = 1,
  stat_shape = 25,
  add_errorbar = "SEM",
  errorbar_color = "grey20",
  errorbar_width = 0.4,
  errorbar_linewidth = 0.6,
  add_bg = FALSE,
  bg_palette = "stripe",
  bg_palcolor = NULL,
  bg_alpha = 0.2,
  add_line = NULL,
  line_color = "red2",
  line_width = 0.6,
  line_type = 2,
  highlight = NULL,
  highlight_color = "red2",
  highlight_size = 1,
  highlight_alpha = 1,
  comparisons = NULL,
  ref_group = NULL,
  pairwise_method = "wilcox.test",
  multiplegroup_comparisons = FALSE,
  multiple_method = "kruskal.test",
  sig_label = "p.format",
  sig_labelsize = 3.5,
  hide_ns = FALSE,
  facet_by = NULL,
  facet_scales = "fixed",
  facet_ncol = NULL,
  facet_nrow = NULL,
  facet_byrow = TRUE,
  title = NULL,
  subtitle = NULL,
  xlab = NULL,
  ylab = NULL,
  seed = 8525,
  combine = TRUE,
  nrow = NULL,
  ncol = NULL,
  byrow = TRUE,
  axes = NULL,
  axis_titles = axes,
  guides = NULL,
  ...
)

ViolinPlot(
  data,
  x,
  x_sep = "_",
  y = NULL,
  in_form = c("long", "wide"),
  split_by = NULL,
  split_by_sep = "_",
  symnum_args = NULL,
  sort_x = NULL,
  flip = FALSE,
  keep_empty = FALSE,
  keep_na = FALSE,
  group_by = NULL,
  group_by_sep = "_",
  group_name = NULL,
  paired_by = NULL,
  x_text_angle = ifelse(isTRUE(flip), 0, 45),
  step_increase = 0.1,
  fill_mode = ifelse(!is.null(group_by), "dodge", "x"),
  palreverse = FALSE,
  position_dodge_preserve = "total",
  theme = "theme_this",
  theme_args = list(),
  palette = "Paired",
  palcolor = NULL,
  alpha = 1,
  aspect.ratio = NULL,
  legend.position = "right",
  legend.direction = "vertical",
  add_point = FALSE,
  pt_color = if (isTRUE(add_beeswarm)) NULL else "grey30",
  pt_size = NULL,
  pt_alpha = 1,
  jitter_width = NULL,
  jitter_height = 0,
  stack = FALSE,
  y_max = NULL,
  y_min = NULL,
  y_brackets = NULL,
  add_beeswarm = FALSE,
  beeswarm_method = "swarm",
  beeswarm_cex = 1,
  beeswarm_priority = "ascending",
  beeswarm_dodge = 0.9,
  add_box = FALSE,
  box_color = "black",
  box_width = 0.1,
  box_ptsize = 2.5,
  add_trend = FALSE,
  trend_color = NULL,
  trend_linewidth = 1,
  trend_ptsize = 2,
  add_stat = NULL,
  stat_name = NULL,
  stat_color = "black",
  stat_size = 1,
  stat_stroke = 1,
  stat_shape = 25,
  add_bg = FALSE,
  bg_palette = "stripe",
  bg_palcolor = NULL,
  bg_alpha = 0.2,
  add_line = NULL,
  line_color = "red2",
  line_width = 0.6,
  line_type = 2,
  highlight = NULL,
  highlight_color = "red2",
  highlight_size = 1,
  highlight_alpha = 1,
  comparisons = NULL,
  ref_group = NULL,
  pairwise_method = "wilcox.test",
  multiplegroup_comparisons = FALSE,
  multiple_method = "kruskal.test",
  sig_label = "p.format",
  sig_labelsize = 3.5,
  hide_ns = FALSE,
  facet_by = NULL,
  facet_scales = "fixed",
  facet_ncol = NULL,
  facet_nrow = NULL,
  facet_byrow = TRUE,
  title = NULL,
  subtitle = NULL,
  xlab = NULL,
  ylab = NULL,
  seed = 8525,
  combine = TRUE,
  nrow = NULL,
  ncol = NULL,
  byrow = TRUE,
  axes = NULL,
  axis_titles = axes,
  guides = NULL,
  ...
)

BeeswarmPlot(
  data,
  x,
  x_sep = "_",
  y = NULL,
  in_form = c("long", "wide"),
  split_by = NULL,
  split_by_sep = "_",
  symnum_args = NULL,
  sort_x = NULL,
  flip = FALSE,
  keep_empty = FALSE,
  keep_na = FALSE,
  group_by = NULL,
  group_by_sep = "_",
  group_name = NULL,
  paired_by = NULL,
  x_text_angle = ifelse(isTRUE(flip), 0, 45),
  step_increase = 0.1,
  fill_mode = ifelse(!is.null(group_by), "dodge", "x"),
  palreverse = FALSE,
  theme = "theme_this",
  theme_args = list(),
  palette = "Paired",
  palcolor = NULL,
  alpha = 1,
  aspect.ratio = NULL,
  legend.position = "right",
  legend.direction = "vertical",
  pt_color = NULL,
  pt_size = NULL,
  pt_alpha = 1,
  position_dodge_preserve = "total",
  jitter_width = NULL,
  jitter_height = 0,
  stack = FALSE,
  y_max = NULL,
  y_min = NULL,
  y_brackets = NULL,
  add_violin = FALSE,
  beeswarm_method = "swarm",
  beeswarm_cex = 1,
  beeswarm_priority = "ascending",
  beeswarm_dodge = 0.9,
  add_box = FALSE,
  box_color = "black",
  box_width = 0.1,
  box_ptsize = 2.5,
  add_trend = FALSE,
  trend_color = NULL,
  trend_linewidth = 1,
  trend_ptsize = 2,
  add_stat = NULL,
  stat_name = NULL,
  stat_color = "black",
  stat_size = 1,
  stat_stroke = 1,
  stat_shape = 25,
  add_bg = FALSE,
  bg_palette = "stripe",
  bg_palcolor = NULL,
  bg_alpha = 0.2,
  add_line = NULL,
  line_color = "red2",
  line_width = 0.6,
  line_type = 2,
  highlight = NULL,
  highlight_color = "red2",
  highlight_size = 1,
  highlight_alpha = 1,
  comparisons = NULL,
  ref_group = NULL,
  pairwise_method = "wilcox.test",
  multiplegroup_comparisons = FALSE,
  multiple_method = "kruskal.test",
  sig_label = "p.format",
  sig_labelsize = 3.5,
  hide_ns = FALSE,
  facet_by = NULL,
  facet_scales = "fixed",
  facet_ncol = NULL,
  facet_nrow = NULL,
  facet_byrow = TRUE,
  title = NULL,
  subtitle = NULL,
  xlab = NULL,
  ylab = NULL,
  seed = 8525,
  combine = TRUE,
  nrow = NULL,
  ncol = NULL,
  byrow = TRUE,
  axes = NULL,
  axis_titles = axes,
  guides = NULL,
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

- base:

  A character string: `"box"` (default) or `"bar"`. Bar plots show group
  means with optional error bars.

- in_form:

  A character string: `"long"` (default) or `"wide"`. In wide form, `x`
  columns are pivoted to long format.

- split_by:

  The column(s) to split the data by for separate sub-plots.

- split_by_sep:

  Separator for concatenated `split_by` columns.

- symnum_args:

  A list of arguments passed to
  [`symnum`](https://rdrr.io/r/stats/symnum.html) for symbolic p-value
  coding.

- sort_x:

  An R expression string (e.g., `"mean(y)"`) to order x-axis categories.
  Default `NULL` keeps the original order. When `keep_empty_x` is
  `TRUE`, empty levels are placed last.

- flip:

  Logical; if `TRUE`, swap the x and y axes.

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

- group_by:

  Columns to group the data for plotting For those plotting functions
  that do not support multiple groups, They will be concatenated into
  one column, using `group_by_sep` as the separator

- group_by_sep:

  The separator for multiple group_by columns. See `group_by`

- group_name:

  A character string for the dodge legend title.

- paired_by:

  A character string naming a column that identifies paired
  observations. Forces `add_point = TRUE` and connects paired
  observations with lines.

- x_text_angle:

  A numeric value specifying the angle of the x-axis text.

- step_increase:

  Fractional step increase for stacking significance brackets when
  multiple comparisons exist.

- fill_mode:

  A character string controlling fill colour mapping: `"dodge"` (fill by
  `group_by`, discrete), `"x"` (fill by x-axis categories, discrete),
  `"mean"` or `"median"` (fill by pre-computed statistic, continuous
  gradient).

- palreverse:

  A logical value indicating whether to reverse the palette. Default is
  FALSE.

- position_dodge_preserve:

  Passed to
  [`position_dodge()`](https://ggplot2.tidyverse.org/reference/position_dodge.html):
  `"total"` preserves the overall group width; `"single"` preserves
  individual element width.

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

- aspect.ratio:

  A numeric value specifying the aspect ratio of the plot.

- legend.position:

  A character string specifying the position of the legend. if
  [`waiver()`](https://ggplot2.tidyverse.org/reference/waiver.html), for
  single groups, the legend will be "none", otherwise "right".

- legend.direction:

  A character string specifying the direction of the legend.

- add_point:

  Logical; add jittered or beeswarm points to the plot.

- pt_color:

  Colour of the points. When `add_beeswarm = TRUE` and `pt_color` is
  `NULL`, points are coloured by the fill variable.

- pt_size:

  Numeric size of the points. Default computed from data size:
  `min(3000 / nrow(data), 0.6)`.

- pt_alpha:

  Numeric transparency of the points.

- jitter_width:

  Numeric width of the jitter. Defaults to `0.5`, but set to `0` when
  `paired_by` is provided.

- jitter_height:

  Numeric height of the jitter. Default `0`.

- stack:

  Logical; stack facetted panels in a compact layout with shared strip
  labels.

- y_max, y_min:

  Numeric y-axis limits, or quantile notation strings (e.g., `"q95"` for
  the 95th percentile, `"q5"` for the 5th percentile).

- y_brackets:

  Numeric y-axis position for significance brackets (or p-value labels
  for multiple comparisons). If NULL, the brackets are placed above the
  maximum y-value.

- add_beeswarm:

  Logical; use
  [`ggbeeswarm::geom_beeswarm()`](https://rdrr.io/pkg/ggbeeswarm/man/geom_beeswarm.html)
  for non-overlapping point layout instead of jitter. Requires the
  `ggbeeswarm` package.

- beeswarm_method:

  Beeswarm layout method: `"swarm"`, `"compactswarm"`, `"hex"`,
  `"square"`, or `"center"`.

- beeswarm_cex:

  Numeric scaling for point spacing. Larger values spread points more.

- beeswarm_priority:

  Point layout priority: `"ascending"`, `"descending"`, `"density"`, or
  `"random"`.

- beeswarm_dodge:

  Numeric dodge width for beeswarm points when `group_by` is provided.
  Default `0.9`.

- add_trend:

  Logical; add trend lines connecting group medians.

- trend_color:

  Colour of the trend line. When `NULL` and `group_by` is present, lines
  are coloured per group.

- trend_linewidth:

  Width of the trend line.

- trend_ptsize:

  Size of the trend line points.

- add_stat:

  A summary function (e.g., `mean`, `median`) to display as a point with
  a shape legend entry.

- stat_name:

  Legend title for the stat summary shape.

- stat_color:

  Colour of the stat summary point.

- stat_size:

  Size of the stat summary point.

- stat_stroke:

  Stroke width of the stat summary point.

- stat_shape:

  Shape (an integer) for the stat summary point. Uses
  [`scale_shape_identity()`](https://ggplot2.tidyverse.org/reference/scale_identity.html)
  so the shape is rendered directly.

- add_errorbar:

  Type of error bars for bar plots. See Details.

- errorbar_color, errorbar_width, errorbar_linewidth:

  Error bar appearance controls.

- add_bg:

  Logical; add alternating background stripes.

- bg_palette:

  Palette for the background stripes.

- bg_palcolor:

  Custom colours for the background stripes.

- bg_alpha:

  Alpha transparency for the background stripes.

- add_line:

  A numeric y-intercept for a horizontal reference line.

- line_color:

  Colour of the reference line.

- line_width:

  Width of the reference line.

- line_type:

  Linetype of the reference line.

- highlight:

  A specification of points to highlight: `TRUE` (all), a numeric index
  vector, a logical expression string, or a character vector of row
  names.

- highlight_color:

  Colour of highlighted points.

- highlight_size:

  Size of highlighted points.

- highlight_alpha:

  Alpha of highlighted points.

- comparisons:

  A logical value (`TRUE` for all pairs) or a list of two-element
  vectors specifying pairwise comparisons. Only available when
  `fill_mode = "dodge"` (i.e., `group_by` is present).

- ref_group:

  A character string specifying the reference group for comparisons.

- pairwise_method:

  Method for pairwise tests. Default `"wilcox.test"`.

- multiplegroup_comparisons:

  Logical; perform an omnibus test (e.g., Kruskal-Wallis) across all
  groups.

- multiple_method:

  Method for the omnibus test. Default `"kruskal.test"`.

- sig_label:

  Label format for significance annotations. For pairwise comparisons:
  `"p.format"`, `"p.signif"`, or a glue template (e.g., `"p = {p}"`).
  For multiple-group tests: `"p.format"` or `"p.signif"`.

- sig_labelsize:

  Size of the significance label text.

- hide_ns:

  Logical; hide non-significant comparison labels.

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

  A numeric seed for reproducibility.

- combine:

  Logical; when `TRUE` (default), returns a combined `patchwork` object.
  When `FALSE`, returns a named list of `ggplot` objects.

- ncol, nrow:

  Integer number of columns / rows for the combined layout.

- byrow:

  Logical; fill the combined layout by row (default `TRUE`).

- axes, axis_titles:

  Character strings for axis handling in the combined layout.

- guides:

  Character string for legend collection across panels.

- ...:

  Additional arguments.

- add_box:

  Logical; overlay a box plot on the primary geometry. Mutually
  exclusive with `base = "box"` and `base = "bar"`.

- box_color:

  Colour of the overlaid box plot outline and fill.

- box_width:

  Width of the overlaid box plot.

- box_ptsize:

  Size of the median point in the overlaid box plot.

- add_violin:

  Logical; whether to add a violin plot behind the beeswarm points.
  **Not supported** — the function will stop with an error directing you
  to use `ViolinPlot(..., add_beeswarm = TRUE)` instead.

## Value

A `ggplot` object, a `patchwork` object, or a named list of `ggplot`
objects (when `combine = FALSE`), each with `height` and `width`
attributes in inches.

## Bar plots (`base = "bar"`)

When `base = "bar"`, bars display group means with optional error bars.
`add_errorbar` controls the error bar type:

- `"SEM"` (default) — standard error of the mean.

- `"SD"` — standard deviation.

- `"CI"` or `"CI95"` — 95\\

- `"none"` — no error bars.

Error bars are computed via a custom `stat_summary(fun.data = ...)` that
handles per-group mean, SD, and sample size.

## Examples

``` r
# \donttest{
set.seed(8525)
data <- data.frame(
    x = rep(LETTERS[1:8], each = 40),
    y = c(rnorm(160), rnorm(160, mean = 1)),
    group1 = sample(c("g1", "g2"), 320, replace = TRUE),
    group2 = sample(c("h1", "h2", "h3", "h4"), 320, replace = TRUE)
)

# Basic box plot
BoxPlot(data, x = "x", y = "y")


# With beeswarm points
BoxPlot(data, x = "x", y = "y", add_beeswarm = TRUE, pt_color = "grey30")


# Stacked + flipped + faceted
BoxPlot(data,
    x = "x", y = "y",
    stack = TRUE, flip = TRUE, facet_by = "group1",
    add_bg = TRUE, bg_palette = "Paired")


# Stacked + flipped + split_by with per-split colours
BoxPlot(data,
    x = "x", y = "y",
    stack = TRUE, flip = TRUE, split_by = "group1",
    add_bg = TRUE, bg_palette = "Paired",
    palcolor = list(g1 = c("red", "blue"), g2 = c("blue", "red")))


# sort_x — order by mean(y)
data <- data.frame(
  x = factor(rep(LETTERS[1:5], each = 40),
     levels = c(LETTERS[1:2], "unused", LETTERS[3:5])),
  y = c(rnorm(40, mean = 5), rnorm(40, mean = 4), rnorm(40, mean = 3),
     rnorm(40, mean = 2), rnorm(40, mean = 1))
)
BoxPlot(data, x = "x", y = "y", sort_x = "mean(y)", keep_empty = TRUE)

BoxPlot(data, x = "x", y = "y", sort_x = "mean(-y)", keep_empty = TRUE)


# Wide-form data
data_wide <- data.frame(A = rnorm(100), B = rnorm(100), C = rnorm(100))
BoxPlot(data_wide, x = c("A", "B", "C"), in_form = "wide")


# Paired observations with connecting lines and paired test
paired_data <- data.frame(
    subject = rep(paste0("s", 1:10), each = 2),
    visit = rep(c("pre", "post"), times = 10),
    value = rnorm(20))
BoxPlot(paired_data,
    x = "visit", y = "value", comparisons = TRUE,
    paired_by = "subject", add_point = TRUE)


# Paired + grouped
paired_group_data <- data.frame(
    subject = rep(paste0("s", 1:6), each = 2),
    x = rep(c("A", "B"), each = 6),
    group = rep(c("before", "after"), times = 6),
    value = rnorm(12))
BoxPlot(paired_group_data,
    x = "x", y = "value",
    paired_by = "subject", group_by = "group",
    comparisons = TRUE, pt_size = 3, pt_color = "red")
#> Warning: Forcing 'add_point' = TRUE when 'paired_by' is provided.
#> Warning: [Box/Violin/BeeswarmPlot] Some pairwise comparisons may fail due to insufficient data points or variability. Adjusting data to ensure valid comparisons.


# keep_na and keep_empty examples
data <- data.frame(
    x = factor(rep(c(LETTERS[1:3], NA, LETTERS[5:8]), each = 40),
       levels = c(LETTERS[1:8])),
    y = c(rnorm(160), rnorm(160, mean = 1)),
    group1 = sample(c("g1", "g2"), 320, replace = TRUE),
    group2 = factor(sample(c("h1", NA, "h3", "h4"), 320, replace = TRUE),
       levels = c("h1", "h2", "h3", "h4")))

BoxPlot(data, x = "x", y = "y")

BoxPlot(data, x = "x", y = "y", keep_na = TRUE, keep_empty = TRUE)

BoxPlot(data, x = "x", y = "y", keep_na = TRUE, keep_empty = TRUE,
        facet_by = "group2")

BoxPlot(data, x = "x", y = "y", keep_na = TRUE, keep_empty = 'level')

BoxPlot(data, x = "x", y = "y", group_by = "group2")

BoxPlot(data, x = "x", y = "y", group_by = "group2",
        keep_na = TRUE, keep_empty = TRUE)

BoxPlot(data, x = "x", y = "y", group_by = "group2",
        keep_na = TRUE, keep_empty = 'level')


# Per-column keep_na / keep_empty
BoxPlot(data, x = "x", y = "y", group_by = "group2",
        keep_na = list(x = TRUE, group2 = FALSE),
        keep_empty = list(x = FALSE, group2 = TRUE))


# Bar plot (base = "bar")
data$y <- abs(data$y)
BoxPlot(data, x = "x", y = "y", base = "bar")

BoxPlot(data, x = "x", y = "y", base = "bar", add_errorbar = "SD")

BoxPlot(data, x = "x", y = "y", base = "bar", add_errorbar = "CI95")

BoxPlot(data, x = "x", y = "y", base = "bar", add_errorbar = "none")

BoxPlot(data, x = "x", y = "y", base = "bar", group_by = "group1")

BoxPlot(data, x = "x", y = "y", base = "bar", add_point = TRUE)

BoxPlot(data, x = "x", y = "y", base = "bar",
        fill_mode = "mean", palette = "Blues")

# }
# \donttest{
ViolinPlot(data, x = "x", y = "y")

ViolinPlot(data, x = "x", y = "y", add_beeswarm = TRUE, pt_color = "grey30")

ViolinPlot(data, x = "x", y = "y", add_box = TRUE)

ViolinPlot(data, x = "x", y = "y", add_point = TRUE)

ViolinPlot(data, x = "x", y = "y", add_trend = TRUE)

ViolinPlot(data, x = "x", y = "y", add_stat = mean)

ViolinPlot(data, x = "x", y = "y", add_bg = TRUE)

ViolinPlot(data, x = "x", y = "y", add_line = 0)


# Grouped
ViolinPlot(data, x = "x", y = "y", group_by = "group1")


# Grouped + faceted + box overlay
ViolinPlot(data,
    x = "x", y = "y", group_by = "group1",
    facet_by = "group2", add_box = TRUE)
#> Warning: Groups with fewer than two datapoints have been dropped.
#> ℹ Set `drop = FALSE` to consider such groups for position adjustment purposes.
#> Warning: Groups with fewer than two datapoints have been dropped.
#> ℹ Set `drop = FALSE` to consider such groups for position adjustment purposes.


# Highlight
ViolinPlot(data,
    x = "x", y = "y", add_point = TRUE,
    highlight = 'group1 == "g1"', alpha = 0.8,
    highlight_size = 1.5, pt_size = 1, add_box = TRUE)


# Pairwise comparisons with formatted labels
if (requireNamespace("ggpubr", quietly = TRUE)) {
  # https://github.com/kassambara/ggpubr/issues/751
  library(ggpubr)
  ViolinPlot(data,
    x = "x", y = "y", group_by = "group1",
    comparisons = TRUE, sig_label = "p = {p}")
}
#> Loading required package: ggplot2


# Explicit comparison list + hide non-significant
ViolinPlot(data,
    x = "x", y = "y", sig_label = "p.format", hide_ns = TRUE,
    facet_by = "group2", comparisons = list(c("D", "E")))


# Continuous fill (mean) + omnibus test
ViolinPlot(data,
    x = "x", y = "y", fill_mode = "mean",
    facet_by = "group2", palette = "Blues",
    multiplegroup_comparisons = TRUE)
#> Warning: The following aesthetics were dropped during statistical transformation: fill.
#> ℹ This can happen when ggplot fails to infer the correct grouping structure in
#>   the data.
#> ℹ Did you forget to specify a `group` aesthetic or to convert a numerical
#>   variable into a factor?
#> Warning: The following aesthetics were dropped during statistical transformation: fill.
#> ℹ This can happen when ggplot fails to infer the correct grouping structure in
#>   the data.
#> ℹ Did you forget to specify a `group` aesthetic or to convert a numerical
#>   variable into a factor?
#> Warning: The following aesthetics were dropped during statistical transformation: fill.
#> ℹ This can happen when ggplot fails to infer the correct grouping structure in
#>   the data.
#> ℹ Did you forget to specify a `group` aesthetic or to convert a numerical
#>   variable into a factor?


# Per-split palettes
ViolinPlot(data,
    x = "x", y = "y", fill_mode = "mean",
    split_by = "group1", palette = c(g1 = "Blues", g2 = "Reds"))


# Stacked faceting
ViolinPlot(data,
    x = "x", y = "y", stack = TRUE,
    facet_by = "group2", add_box = TRUE, add_bg = TRUE,
    bg_palette = "Paired")

# }
# \donttest{
# Basic beeswarm
BeeswarmPlot(data, x = "x", y = "y")


# Control point size
BeeswarmPlot(data, x = "x", y = "y", pt_size = 1)


# Beeswarm with box overlay
BeeswarmPlot(data, x = "x", y = "y", add_box = TRUE, pt_color = "grey30")


# Grouped
BeeswarmPlot(data, x = "x", y = "y", group_by = "group1")


# Grouped without dodging
BeeswarmPlot(data, x = "x", y = "y", group_by = "group1",
             beeswarm_dodge = NULL)


# Hex layout with wider spacing
BeeswarmPlot(data,
    x = "x", y = "y", beeswarm_method = "hex",
    beeswarm_cex = 2)
#> Warning: In `position_beeswarm`, method `hex` discretizes the data axis (a.k.a the
#> continuous or non-grouped axis).
#> This may result in changes to the position of the points along that axis,
#> proportional to the value of `cex`.
#> To prevent this behavior, set `preserve.data.axis=TRUE`.
#> This warning is displayed once per session.

# }
```
