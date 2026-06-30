# Atomic Box / Violin / Bar / Beeswarm plot (internal)

Core implementation for drawing box plots, violin plots, bar plots (mean
± error bars), or beeswarm plots. This is the workhorse behind
[`BoxPlot`](https://pwwang.github.io/plotthis/reference/boxviolinplot.md),
[`ViolinPlot`](https://pwwang.github.io/plotthis/reference/boxviolinplot.md),
and
[`BeeswarmPlot`](https://pwwang.github.io/plotthis/reference/boxviolinplot.md)
— it takes a **single** data frame (no `split_by` support) and returns a
`ggplot` object.

The `base` parameter selects the primary geometry:

- `"box"` — `geom_boxplot()`

- `"violin"` — `geom_violin()`

- `"bar"` — `stat_summary(fun = mean, geom = "col")` with optional error
  bars (SEM, SD, or CI).

- `"none"` — no primary geometry (used by
  [`BeeswarmPlot`](https://pwwang.github.io/plotthis/reference/boxviolinplot.md)
  to draw beeswarm points alone).

## Usage

``` r
BoxViolinPlotAtomic(
  data,
  x,
  x_sep = "_",
  y = NULL,
  base = c("box", "violin", "bar", "none"),
  in_form = c("long", "wide"),
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
  position_dodge_preserve = "total",
  fill_mode = ifelse(!is.null(group_by), "dodge", "x"),
  palreverse = FALSE,
  symnum_args = NULL,
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
  y_nbreaks = 4,
  jitter_width = NULL,
  jitter_height = 0,
  stack = FALSE,
  y_max = NULL,
  y_min = NULL,
  y_trans = "identity",
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
  ...
)
```

## Arguments

- data:

  A data frame.

- x:

  A character string of column name(s) for the x-axis. Character/factor
  columns are expected. Multiple columns are concatenated with `x_sep`.

- x_sep:

  A character string to join multiple `x` columns. Default `"_"`.

- y:

  A character string of the numeric column for the y-axis. Not required
  when `in_form = "wide"` (data values are taken from the `x` columns).

- base:

  A character string specifying the primary plot type: `"box"` (box
  plot), `"violin"` (violin plot), `"bar"` (mean bars with optional
  error bars), or `"none"` (no primary geometry, used by beeswarm
  plots).

- in_form:

  A character string: `"long"` (default) or `"wide"`. In wide form, `x`
  columns are pivoted to long format.

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

  A character vector of column name(s) to dodge the boxes/violins by.
  Multiple columns are concatenated with `group_by_sep`.

- group_by_sep:

  A character string to separate concatenated `group_by` columns.
  Default `"_"`.

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

- position_dodge_preserve:

  Passed to
  [`position_dodge()`](https://ggplot2.tidyverse.org/reference/position_dodge.html):
  `"total"` preserves the overall group width; `"single"` preserves
  individual element width.

- fill_mode:

  A character string controlling fill colour mapping: `"dodge"` (fill by
  `group_by`, discrete), `"x"` (fill by x-axis categories, discrete),
  `"mean"` or `"median"` (fill by pre-computed statistic, continuous
  gradient).

- palreverse:

  A logical value indicating whether to reverse the palette. Default is
  FALSE.

- symnum_args:

  A list of arguments passed to
  [`symnum`](https://rdrr.io/r/stats/symnum.html) for symbolic p-value
  coding.

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

- y_nbreaks:

  Integer number of y-axis breaks.

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

- y_trans:

  A character string for y-axis transformation (e.g., `"log10"`).

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

  The random seed to use. Default is 8525.

- ...:

  Additional arguments.

## Value

A `ggplot` object, possibly faceted, with `height` and `width`
attributes (in inches) attached.

## Architecture

1.  **Wide-to-long conversion** — when `in_form = "wide"`, data is
    pivoted via
    [`tidyr::pivot_longer()`](https://tidyr.tidyverse.org/reference/pivot_longer.html).
    The `keep_na` / `keep_empty` lists are filtered to the new column
    names.

2.  **Column resolution** — `x`, `y`, `group_by`, `facet_by`, and
    `paired_by` are validated via
    [`check_columns`](https://pwwang.github.io/plotthis/reference/check_columns.md).

3.  **NA / empty-level handling** — per-column `keep_empty` settings are
    extracted for `x`, `group_by`, and `facet_by` independently.

4.  **Beeswarm validation** — if `add_beeswarm = TRUE`, the `ggbeeswarm`
    package is required. Beeswarm is disabled (with a warning) when
    `paired_by` is provided.

5.  **Paired data validation** — structural checks ensure each (`x`,
    `paired_by`) combination has exactly 2 observations (one per group
    when `group_by` is present). Paired observations force
    `add_point = TRUE`.

6.  **Summary statistics** — `.y_mean` and `.y_median` are pre-computed
    per (`x`, `group_by`, `facet_by`) for use in trend lines and fill
    modes.

7.  **y-axis limits** — `y_max` / `y_min` accept numeric values or
    quantile notation (`"q95"`, `"q5"`). For bar plots, the limit is
    extended upward by the error bar extent.

8.  **Highlight** — `highlight` can be `TRUE` (all points), a numeric
    index vector, a logical expression string, or a character vector of
    row names.

9.  **sort_x** — an R expression string (e.g., `"mean(y)"`) evaluated
    per x-level to reorder categories.

10. **Flip transformation** — when `flip = TRUE`, factor levels are
    reversed and `aspect.ratio` is inverted.

11. **Base geometry** — the primary geom is added: `geom_boxplot()`,
    `geom_violin()`, or `stat_summary(fun = mean, geom = "col")`. Error
    bars (SEM / SD / CI) are layered on bar plots via a custom
    `stat_summary(fun.data = ...)`.

12. **Fill mode** — `fill_mode` controls colour mapping:

    - `"dodge"` — fill by `group_by` (discrete).

    - `"x"` — fill by x-axis categories (discrete).

    - `"mean"` / `"median"` — fill by pre-computed mean/median
      (continuous gradient).

13. **Box overlay** — when `add_box = TRUE` on a non-box base, a box
    plot is overlaid via
    [`ggnewscale::new_scale_fill()`](https://eliocamp.github.io/ggnewscale/reference/new_scale.html)
    with a white fill/black outline.

14. **Statistical comparisons** — two pathways:

    - **Pairwise** (`comparisons`) — uses
      [`ggpubr::geom_pwc()`](https://rpkgs.datanovia.com/ggpubr/reference/geom_pwc.html)
      with automatic or explicit comparison pairs. Data is preprocessed
      to avoid test failures from zero-variance or all-NA groups.

    - **Multiple-group** (`multiplegroup_comparisons`) — uses
      [`ggpubr::stat_compare_means()`](https://rpkgs.datanovia.com/ggpubr/reference/stat_compare_means.html)
      for omnibus tests (e.g., Kruskal-Wallis).

    After comparison layers are added, `y_max_use` is expanded to
    accommodate significance brackets.

15. **Points** — jittered points (`geom_point()` with
    `position_jitterdodge`) or beeswarm points
    ([`ggbeeswarm::geom_beeswarm()`](https://rdrr.io/pkg/ggbeeswarm/man/geom_beeswarm.html)).
    Paired observations add connecting lines (`geom_line()`) between
    matched subjects.

16. **Trend lines** — `stat_summary(fun = first)` draws lines connecting
    group medians. When `trend_color` is `NULL` and `group_by` is
    present, lines are coloured per group.

17. **Reference lines** — `geom_hline()` at the specified y-intercept.

18. **Stat summary points** — a custom `stat_summary()` point layer
    displaying a user-specified summary statistic (e.g., mean) with a
    shape legend entry.

19. **Stack layout** — when `stack = TRUE`, facets are arranged with
    shared strip labels and negative panel spacing for a compact stacked
    appearance.

20. **Dimension calculation** —
    [`calculate_plot_dimensions()`](https://pwwang.github.io/plotthis/reference/calculate_plot_dimensions.md)
    accounts for the number of x-levels × dodge groups, flip state, and
    stack layout adjustments. Minimum dimensions are enforced from label
    character widths.

21. **Faceting** —
    [`facet_plot()`](https://pwwang.github.io/plotthis/reference/facet_plot.md)
    wraps the result with the appropriate strip position based on
    flip/stack state.
