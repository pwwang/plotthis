# Atomic ring plot (internal)

Core implementation for drawing a single ring (multi-layer donut) plot.
This is the workhorse behind the exported
[`RingPlot`](https://pwwang.github.io/plotthis/reference/RingPlot.md)
function — it takes a **single** data frame (no `split_by` support) and
returns a `ggplot` object. Each level of `x` becomes a concentric ring,
and within each ring the `group_by` variable divides the ring into
filled segments drawn via `geom_col()` with `coord_polar("y")`.

When `y = NULL`, the count of observations per combination is used.
y-values are always normalised to proportions (0–1) within each (`x`,
`facet_by`) group so that each complete ring sums to 1.

## Usage

``` r
RingPlotAtomic(
  data,
  x = NULL,
  y = NULL,
  group_by = NULL,
  group_by_sep = "_",
  group_name = NULL,
  label = NULL,
  clockwise = TRUE,
  keep_na = FALSE,
  keep_empty = FALSE,
  facet_by = NULL,
  facet_scales = "free_y",
  facet_ncol = NULL,
  facet_nrow = NULL,
  facet_byrow = TRUE,
  theme = "theme_this",
  theme_args = list(),
  palette = "Paired",
  palcolor = NULL,
  palreverse = FALSE,
  alpha = 1,
  aspect.ratio = 1,
  legend.position = "right",
  legend.direction = "vertical",
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

  A character string specifying the column to define the rings
  (concentric layers) of the plot. Each unique value becomes one ring.
  When `NULL`, a single-ring (pie-chart-like) plot is produced.
  Character or factor columns are expected. Multiple columns are not
  supported for `x` in ring plots.

- y:

  A character string specifying the numeric column for segment sizing.
  When `NULL` (default), the count of observations in each (`x`,
  `group_by`, `facet_by`) combination is used. Values are always
  normalised to proportions within each ring.

- group_by:

  A character vector of column names to divide each ring into filled
  segments. Each unique combination becomes one segment per ring.
  Multiple columns are concatenated with `group_by_sep`. When `NULL`, a
  single unsegmented ring is drawn and the legend is hidden.

- group_by_sep:

  A character string to join multiple `group_by` columns. Default `"_"`.
  Ignored when `group_by` is a single column.

- group_name:

  A character string used as the fill legend title. When `NULL`, the
  `group_by` column name is used.

- label:

  A logical value controlling whether ring labels are shown. Labels
  display the `x` values (ring names) at the inner edge of each ring.
  Default `NULL` auto-selects: `FALSE` for single-ring plots, `TRUE` for
  multi-ring plots.

- clockwise:

  A logical value. When `TRUE` (default), segments within each ring are
  drawn clockwise (top-to-right). When `FALSE`, they are drawn
  counter-clockwise. The legend also reverses to match the drawing
  order.

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

- aspect.ratio:

  A numeric value specifying the aspect ratio of the plot.

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

- seed:

  The random seed to use. Default is 8525.

- ...:

  Additional arguments.

## Value

A `ggplot` object with `height` and `width` attributes (in inches)
attached.

## Architecture

1.  **Column resolution** — `x`, `group_by`, `y`, and `facet_by` are
    validated and transformed via
    [`check_columns`](https://pwwang.github.io/plotthis/reference/check_columns.md).
    If `x` is `NULL`, a dummy `.x` column (value `1`) is created,
    producing a single-ring (pie-chart-like) plot. If `group_by` is
    `NULL`, a dummy `.group_by` factor is created and the legend is
    suppressed. Multi-column `group_by` inputs are concatenated with
    `group_by_sep`.

2.  **NA / empty-level handling** —
    [`process_keep_na_empty()`](https://pwwang.github.io/plotthis/reference/process_keep_na_empty.md)
    applies `keep_na` and `keep_empty` policies. Per-column `keep_empty`
    settings are extracted for `x`, `group_by`, and `facet_by`
    independently. The facet columns must have identical `keep_empty`
    values.

3.  **Count aggregation** — when `y = NULL`, the count of observations
    in each unique (`x`, `group_by`, `facet_by`) combination is computed
    as a new `.y` column. Factor levels are preserved after aggregation.

4.  **Proportion scaling** — y-values are divided by the sum within each
    (`x`, `facet_by`) group, normalising each ring to sum to 1.0. This
    is essential for polar coordinates where each ring represents a full
    360 degrees.

5.  **Ring resolution** — The levels of `x` are reversed so the
    outermost ring appears first in the data (rings are drawn outward
    from the origin). `NA` levels are appended.

6.  **Label logic** — When `label = NULL`, labels are automatically
    suppressed for single-ring plots and shown for multi-ring plots.
    Labels are placed at the inner edge of each ring via `geom_label()`.

7.  **Clockwise ordering** — When `clockwise = TRUE` (default), the
    `group_by` levels are reversed so segments appear clockwise from the
    top. The legend guide also reverses its order.

8.  **Colour mapping** —
    [`palette_this()`](https://pwwang.github.io/plotthis/reference/palette_this.md)
    assigns colours to all `group_by` levels, including `NA` (defaulting
    to `"grey80"`).

9.  **Plot assembly** — The `ggplot` object is built with `geom_col()`
    (ring segments), `coord_polar("y")` (polar transform),
    `scale_x_discrete()` (with a leading space level as the donut hole),
    and `scale_fill_manual()` with per-group colours. All axes, ticks,
    grid lines, and panel borders are removed for a clean appearance.
    The fill scale `drop` argument is controlled by `keep_empty_group`.

10. **Dimension calculation** —
    [`calculate_plot_dimensions()`](https://pwwang.github.io/plotthis/reference/calculate_plot_dimensions.md)
    computes plot height and width from `aspect.ratio` and legend
    metrics. The resulting `height` / `width` attributes are stored on
    the `ggplot` object.

11. **Faceting** —
    [`facet_plot()`](https://pwwang.github.io/plotthis/reference/facet_plot.md)
    wraps the plot with `facet_wrap` / `facet_grid` if `facet_by` is
    provided, respecting the `keep_empty` setting for facet variables.
