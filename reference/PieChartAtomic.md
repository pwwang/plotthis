# Atomic pie chart (internal)

Core implementation for drawing a single pie chart. This is the
workhorse behind the exported
[`PieChart`](https://pwwang.github.io/plotthis/reference/PieChart.md)
function — it takes a **single** data frame (no `split_by` support) and
returns a `ggplot` object. The chart maps the proportion of each x-axis
category to the angle of a pie slice using
[`coord_polar()`](https://ggplot2.tidyverse.org/reference/coord_radial.html),
with optional faceting and per-slice labels.

## Usage

``` r
PieChartAtomic(
  data,
  x,
  y = NULL,
  label = y,
  clockwise = TRUE,
  keep_na = FALSE,
  keep_empty = FALSE,
  theme = "theme_this",
  theme_args = list(),
  palette = "Paired",
  palcolor = NULL,
  palreverse = FALSE,
  alpha = 1,
  facet_by = NULL,
  facet_scales = "free_y",
  facet_ncol = NULL,
  facet_nrow = NULL,
  facet_byrow = TRUE,
  aspect.ratio = 1,
  legend.position = "right",
  legend.direction = "vertical",
  title = NULL,
  subtitle = NULL,
  xlab = NULL,
  ylab = NULL,
  ...
)
```

## Arguments

- data:

  A data frame.

- x:

  A character string specifying the column name for the x-axis
  (categories). Must be character or factor. Each unique value becomes a
  pie slice.

- y:

  A character string specifying the numeric column for the y-axis. When
  `NULL` (default), the count of observations in each (`x`, `facet_by`)
  combination is used and stored as `.y`.

- label:

  A character string specifying the column to use for slice labels.
  `NULL` (default) hides labels. When `TRUE`, the `y` values are used as
  labels. When `y = NULL`, use `".y"` to label with the computed counts.

- clockwise:

  A logical value. When `TRUE` (default), the pie slices are ordered
  clockwise starting from the top. When `FALSE`, slices are ordered
  counter-clockwise.

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

- ...:

  Additional arguments.

## Value

A `ggplot` object with `height` and `width` attributes (in inches)
attached.

## Architecture

1.  **ggplot dispatch** — selects `gglogger::ggplot` or
    [`ggplot2::ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)
    based on `getOption("plotthis.gglogger.enabled")`.

2.  **Column resolution** — `x` is forced to factor; `y` is validated
    (numeric, optional).

3.  **Count aggregation** — when `y = NULL`, the count of observations
    per (`x`, `facet_by`) combination is computed as a new `.y` column.
    Factor levels are preserved after aggregation.

4.  **Label resolution** — when `label = TRUE`, the label column is set
    to `y`. The `label` column is then validated.

5.  **Facet column resolution** — `facet_by` columns are validated as
    factors, allowing up to two columns.

6.  **NA / empty-level handling** —
    [`process_keep_na_empty()`](https://pwwang.github.io/plotthis/reference/process_keep_na_empty.md)
    applies `keep_na` and `keep_empty` policies. Per-column `keep_empty`
    settings are extracted for `x` and `facet_by`. When more than one
    facet column is provided, their `keep_empty` values must be
    identical.

7.  **Clockwise ordering** — when `clockwise = TRUE` (default), the
    levels of `x` are reversed so that the first slice starts from the
    top and proceeds clockwise. The data is then sorted by the (possibly
    reversed) `x` factor levels.

8.  **Position calculation** — if faceted, grouping by facet variables;
    computes cumulative sums (`csum`) and label midpoint positions
    (`pos`) for each slice.

9.  **Colour mapping** —
    [`palette_this()`](https://pwwang.github.io/plotthis/reference/palette_this.md)
    assigns colours to all `x` levels, including `NA` (defaulting to
    `"grey80"`).

10. **Plot assembly** — the `ggplot` object is built with
    `geom_col(width = 1)` and `coord_polar(theta = "y")` to create the
    circular pie layout. The fill scale uses `scale_fill_manual()` with
    per-slice colours; the `drop` argument is controlled by
    `keep_empty_x`.

11. **Labels** — when `label` is not `NULL`,
    [`geom_label_repel()`](https://ggrepel.slowkow.com/reference/geom_text_repel.html)
    adds text labels at the computed midpoint positions (`pos`).

12. **Dimension calculation** —
    [`calculate_plot_dimensions()`](https://pwwang.github.io/plotthis/reference/calculate_plot_dimensions.md)
    computes plot height and width from `aspect.ratio` and legend
    metrics. The resulting `height` / `width` attributes are stored on
    the `ggplot` object.

13. **Faceting** —
    [`facet_plot()`](https://pwwang.github.io/plotthis/reference/facet_plot.md)
    wraps the plot with `facet_wrap` / `facet_grid` if `facet_by` is
    provided, respecting the `keep_empty` setting for facet variables.
