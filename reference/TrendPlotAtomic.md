# Atomic trend plot (internal)

Core implementation for drawing a single trend plot. This is the
workhorse behind the exported
[`TrendPlot`](https://pwwang.github.io/plotthis/reference/TrendPlot.md)
function – it takes a **single** data frame (no `split_by` support) and
returns a `ggplot` object. A trend plot combines stacked bars
([`geom_col()`](https://ggplot2.tidyverse.org/reference/geom_bar.html))
with a semi-transparent area background
([`geom_area()`](https://ggplot2.tidyverse.org/reference/geom_ribbon.html))
to show how one or more groups accumulate across a discrete x-axis. This
hybrid style sits between a bar plot and an area plot, preserving the
discrete category separation of bars while softening the visual with an
area fill.

The function supports count aggregation (omit `y` to plot observation
counts per x-category), proportion scaling (`scale_y = TRUE` normalises
each x position to 100\\

## Usage

``` r
TrendPlotAtomic(
  data,
  x,
  y = NULL,
  x_sep = "_",
  group_by = NULL,
  group_by_sep = "_",
  group_name = NULL,
  scale_y = FALSE,
  theme = "theme_this",
  theme_args = list(),
  palette = "Paired",
  palcolor = NULL,
  palreverse = FALSE,
  alpha = 1,
  facet_by = NULL,
  facet_scales = "fixed",
  facet_ncol = NULL,
  facet_nrow = NULL,
  facet_byrow = TRUE,
  x_text_angle = 0,
  aspect.ratio = 1,
  legend.position = waiver(),
  legend.direction = "vertical",
  title = NULL,
  subtitle = NULL,
  xlab = NULL,
  ylab = NULL,
  keep_empty = FALSE,
  keep_na = FALSE,
  ...
)
```

## Arguments

- data:

  A data frame.

- x:

  A character string specifying the column name to plot on the x-axis.
  Must be character or factor. Multiple columns can be provided; they
  are concatenated with `x_sep` as the separator.

- y:

  A character string specifying the numeric column for the y-axis. When
  `NULL`, the count of observations in each (`x`, `group_by`,
  `facet_by`) combination is used.

- x_sep:

  A character string used to join multiple `x` columns. Default `"_"`.
  Ignored when `x` is a single column.

- group_by:

  A character vector of column names to fill the bars and areas by. Each
  unique combination becomes a separate stacked segment. Multiple
  columns are concatenated with `group_by_sep`. When `NULL`, a single
  filled bar/area is drawn and the legend is hidden.

- group_by_sep:

  A character string to separate concatenated `group_by` columns.
  Default `"_"`.

- group_name:

  A character string used as the fill legend title. When `NULL`, the
  `group_by` column name is used.

- scale_y:

  A logical value. When `TRUE`, y-values are scaled to proportions
  within each (`x`, `facet_by`) group so that each x position stacks to
  1.0. The y-axis labels switch from numeric to percent format
  automatically.

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

- ...:

  Additional arguments.

## Value

A `ggplot` object with `height` and `width` attributes (in inches)
attached.

## Architecture

1.  **Column resolution** – `x`, `y`, `group_by`, and `facet_by` are
    validated and transformed via
    [`check_columns`](https://pwwang.github.io/plotthis/reference/check_columns.md).
    Multi-column inputs for `x` and `group_by` are concatenated into
    single columns using their respective separators (`x_sep`,
    `group_by_sep`).

2.  **Count aggregation** – when `y = NULL`, the count of observations
    in each unique (`x`, `group_by`, `facet_by`) combination is computed
    as a new `.count` column. Factor levels are preserved after
    aggregation.

3.  **Proportion scaling** – when `scale_y = TRUE`, the y-values are
    divided by the sum within each (`x`, `facet_by`) group, producing a
    proportion (0–1). Percent labels are used automatically on the
    y-axis.

4.  **NA / empty-level handling** –
    [`process_keep_na_empty()`](https://pwwang.github.io/plotthis/reference/process_keep_na_empty.md)
    applies `keep_na` and `keep_empty` policies. Per-column `keep_empty`
    settings are extracted for `x`, `group_by`, and `facet_by`
    independently. The facet columns must have identical `keep_empty`
    values. Note that `keep_empty = TRUE` is not supported globally
    because empty categories would break the continuity of the x-axis.

5.  **Group fill setup** – when `group_by = NULL`, a dummy `.fill`
    factor is created so the single group still draws with the first
    palette colour. The legend is suppressed
    (`legend.position = "none"`).

6.  **Data completion** –
    [`complete()`](https://tidyr.tidyverse.org/reference/complete.html)
    pads all `x` by `group_by` (by `facet_by`) combinations with
    `y = 0`. This prevents
    [`geom_area()`](https://ggplot2.tidyverse.org/reference/geom_ribbon.html)
    from interpolating across missing groups, which would otherwise
    cause stacked areas to exceed the correct total.

7.  **Area layer construction** – the completed data is expanded by
    duplicating each row and offsetting the x-axis positions by \\\pm
    0.2\\ on a numeric scale, creating discrete-width area strips that
    align perfectly with the bars.

8.  **Colour mapping** –
    [`palette_this()`](https://pwwang.github.io/plotthis/reference/palette_this.md)
    assigns colours to all `group_by` levels, including `NA` (defaulting
    to `"grey80"`).

9.  **Plot assembly** – the `ggplot` object is built with `geom_area()`
    (at half `alpha`, grey outline) as a background fill, overlaid with
    `geom_col()` (full `alpha`, black outline, width 0.4) as the
    foreground bars. Both layers use `position_stack(vjust = 0.5)`. The
    x-axis uses `scale_x_discrete()` and the y-axis uses
    `scale_y_continuous()` (percent labels when scaled). The fill scale
    `drop` argument is controlled by `keep_empty_group`.

10. **Dimension calculation** –
    [`calculate_plot_dimensions()`](https://pwwang.github.io/plotthis/reference/calculate_plot_dimensions.md)
    computes plot height and width from the x-axis category count,
    `aspect.ratio`, and legend metrics. The base height increases for
    plots with 10 or more x categories (from 4.5 to 6.5 inches). The
    resulting `height` and `width` attributes are stored on the `ggplot`
    object.

11. **Faceting** –
    [`facet_plot()`](https://pwwang.github.io/plotthis/reference/facet_plot.md)
    wraps the plot with `facet_wrap` / `facet_grid` if `facet_by` is
    provided, respecting the `keep_empty` setting for facet variables.
