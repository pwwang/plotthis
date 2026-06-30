# Box / Violin / Bar / Beeswarm plot (internal)

Internal wrapper that handles `split_by` processing and dispatches to
[`BoxViolinPlotAtomic`](https://pwwang.github.io/plotthis/reference/BoxViolinPlotAtomic.md)
for each split. This is the intermediate layer between the three public
APIs
([`BoxPlot`](https://pwwang.github.io/plotthis/reference/boxviolinplot.md),
[`ViolinPlot`](https://pwwang.github.io/plotthis/reference/boxviolinplot.md),
and
[`BeeswarmPlot`](https://pwwang.github.io/plotthis/reference/boxviolinplot.md))
and the core implementation.

## Usage

``` r
BoxViolinPlot(
  data,
  x,
  x_sep = "_",
  y = NULL,
  base = c("box", "violin", "bar", "none"),
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
  add_beeswarm = FALSE,
  beeswarm_method = "swarm",
  beeswarm_cex = 1,
  beeswarm_priority = "ascending",
  beeswarm_dodge = 0.9,
  add_box = FALSE,
  box_color = "black",
  box_width = 0.1,
  box_ptsize = 2.5,
  add_errorbar = "SEM",
  errorbar_color = "grey20",
  errorbar_width = 0.4,
  errorbar_linewidth = 0.6,
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

- x_sep:

  A character string to join multiple `x` columns. Default `"_"`.

- y:

  A character string specifying the column name of the data frame to
  plot for the y-axis.

- base:

  A character string specifying the primary plot type: `"box"` (box
  plot), `"violin"` (violin plot), `"bar"` (mean bars with optional
  error bars), or `"none"` (no primary geometry, used by beeswarm
  plots).

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
  `waiver()`, for single groups, the legend will be "none", otherwise
  "right".

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

- add_box:

  Logical; overlay a box plot on the primary geometry. Mutually
  exclusive with `base = "box"` and `base = "bar"`.

- box_color:

  Colour of the overlaid box plot outline and fill.

- box_width:

  Width of the overlaid box plot.

- box_ptsize:

  Size of the median point in the overlaid box plot.

- add_errorbar:

  Type of error bars for bar plots (`base = "bar"`): `"SEM"` (standard
  error of the mean, default), `"SD"` (standard deviation), `"CI"` or
  `"CI95"` (95\\ interval), or `"none"`. Silently ignored for non-bar
  bases.

- errorbar_color:

  Colour of the error bar lines and caps.

- errorbar_width:

  Width of the error bar caps.

- errorbar_linewidth:

  Line width of the error bars.

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
  `scale_shape_identity()` so the shape is rendered directly.

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

- design:

  A custom layout design for the combined plot.

- ...:

  Additional arguments.

## Value

A `ggplot` object, a `patchwork` object, or a named list of `ggplot`
objects (when `combine = FALSE`), each with `height` and `width`
attributes in inches.

## split_by workflow

When `split_by` is provided:

1.  [`check_keep_na()`](https://pwwang.github.io/plotthis/reference/check_keep_na.md)
    and
    [`check_keep_empty()`](https://pwwang.github.io/plotthis/reference/check_keep_empty.md)
    normalise the `keep_na` / `keep_empty` arguments.

2.  The `split_by` column is validated and its NA / empty levels are
    processed. It is then removed from the per-column lists.

3.  The data is split by `split_by` (preserving level order). If
    `split_by` is `NULL`, the data is wrapped in a single-element list
    with name `"..."`.

4.  Per-split `palette`, `palcolor`, `legend.position`, and
    `legend.direction` are resolved.

5.  [`BoxViolinPlotAtomic()`](https://pwwang.github.io/plotthis/reference/BoxViolinPlotAtomic.md)
    is called for each split. When `title` is a function, it receives
    the split level name for dynamic titles.

6.  Results are combined via
    [`combine_plots()`](https://pwwang.github.io/plotthis/reference/combine_plots.md).
