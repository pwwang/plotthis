# Single-series line plot (internal)

Core implementation for drawing a single-series line plot. This is the
workhorse behind
[`LinePlotAtomic`](https://pwwang.github.io/plotthis/reference/LinePlotAtomic.md)
for ungrouped data – it takes a **single** data frame (no `split_by`
support) and returns a `ggplot` object. Each x-axis category receives
its own point and connecting line, with optional per-x coloring, error
bars, highlighted points, and a horizontal reference line.

## Usage

``` r
LinePlotSingle(
  data,
  x,
  y = NULL,
  fill_point_by_x = TRUE,
  color_line_by_x = TRUE,
  facet_by = NULL,
  add_bg = FALSE,
  bg_palette = "stripe",
  bg_palcolor = NULL,
  bg_alpha = 0.2,
  add_errorbars = FALSE,
  errorbar_width = 0.1,
  errorbar_alpha = 1,
  errorbar_color = "grey30",
  errorbar_linewidth = 0.75,
  errorbar_min = NULL,
  errorbar_max = NULL,
  errorbar_sd = NULL,
  highlight = NULL,
  highlight_size = pt_size - 0.75,
  highlight_color = "red2",
  highlight_alpha = 0.8,
  pt_alpha = 1,
  pt_size = 5,
  add_hline = FALSE,
  hline_type = "solid",
  hline_width = 0.5,
  hline_color = "black",
  hline_alpha = 1,
  line_type = "solid",
  line_width = 1,
  line_alpha = 0.8,
  theme = "theme_this",
  theme_args = list(),
  palette = "Paired",
  palcolor = NULL,
  palreverse = FALSE,
  x_text_angle = 0,
  aspect.ratio = 1,
  legend.position = "right",
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

  A character string specifying the column name for the x-axis. Must be
  character or factor.

- y:

  A character string specifying the numeric column for the y-axis. When
  `NULL`, the count of observations per x level is used.

- fill_point_by_x:

  A logical value. When TRUE (default), points are filled by the x-axis
  categories using the palette. When FALSE, all points use the first
  palette colour.

- color_line_by_x:

  A logical value. When TRUE (default), lines are coloured by the x-axis
  categories using the palette. When FALSE, all lines use the first
  palette colour.

- facet_by:

  A character string specifying the column name of the data frame to
  facet the plot. Otherwise, the data will be split by `split_by` and
  generate multiple plots and combine them into one using
  [`patchwork::wrap_plots`](https://patchwork.data-imaginist.com/reference/wrap_plots.html)

- add_bg:

  A logical value. When TRUE, alternating background stripes are drawn
  via
  [`bg_layer()`](https://pwwang.github.io/plotthis/reference/bg_layer.md).
  Default FALSE.

- bg_palette:

  A character string specifying the palette for the background stripe
  colours. Default `"stripe"`.

- bg_palcolor:

  A character vector of colours for the background stripes. When NULL
  (default), colours are derived from `bg_palette`.

- bg_alpha:

  A numeric value in `[0, 1]` for the transparency of background
  stripes. Default 0.2.

- add_errorbars:

  A logical value. When TRUE, error bars are added via
  [`geom_errorbar()`](https://ggplot2.tidyverse.org/reference/geom_linerange.html).
  Requires `errorbar_sd` or `errorbar_min`/`errorbar_max`. Default
  FALSE.

- errorbar_width:

  A numeric value for the width of the error bar caps. Default 0.1.

- errorbar_alpha:

  A numeric value in `[0, 1]` for the transparency of error bars.
  Default 1.

- errorbar_color:

  A character string for the colour of the error bars. When `"line"`,
  error bars are coloured the same as the lines (by x when
  `color_line_by_x = TRUE`, or single colour otherwise). Default
  `"grey30"`.

- errorbar_linewidth:

  A numeric value for the line width of error bars. Default 0.75.

- errorbar_min:

  A character string naming the column with the lower error bar bound.
  Ignored when `errorbar_sd` is provided.

- errorbar_max:

  A character string naming the column with the upper error bar bound.
  Ignored when `errorbar_sd` is provided.

- errorbar_sd:

  A character string naming the column with the standard deviation. When
  `errorbar_min` and `errorbar_max` are not provided, error bars are
  computed as y +/- `errorbar_sd`.

- highlight:

  A vector of row indices, row names, a single string expression (e.g.
  `"y > 10"`) filtering rows to highlight, or TRUE to highlight all
  points. When NULL (default), no highlighting is applied.

- highlight_size:

  A numeric value for the size of highlighted points. Defaults to
  `pt_size - 0.75`.

- highlight_color:

  A character string for the colour of highlighted points. Default
  `"red2"`.

- highlight_alpha:

  A numeric value in `[0, 1]` for the transparency of highlighted
  points. Default 0.8.

- pt_alpha:

  A numeric value in `[0, 1]` for the transparency of points. Default 1.

- pt_size:

  A numeric value for the point size. Default 5.

- add_hline:

  A numeric value specifying the y-intercept of a horizontal reference
  line. When FALSE (default), no line is drawn.

- hline_type:

  A character string specifying the line type of the horizontal
  reference line. Default `"solid"`.

- hline_width:

  A numeric value for the width of the horizontal reference line.
  Default 0.5.

- hline_color:

  A character string for the colour of the horizontal reference line.
  Default `"black"`.

- hline_alpha:

  A numeric value in `[0, 1]` for the transparency of the horizontal
  reference line. Default 1.

- line_type:

  A character string specifying the line type. Default `"solid"`.

- line_width:

  A numeric value for the line width (in mm). Default 1.

- line_alpha:

  A numeric value in `[0, 1]` for the transparency of the line. Default
  0.8.

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

A `ggplot` object with `height` and `width` attributes (in inches).

## Architecture

1.  **ggplot dispatch** – selects `gglogger::ggplot` or
    [`ggplot2::ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)
    based on `getOption("plotthis.gglogger.enabled")`.

2.  **Column resolution** – `x` (forced to factor), `y`, and `facet_by`
    are validated via
    [`check_columns`](https://pwwang.github.io/plotthis/reference/check_columns.md).

3.  **Count aggregation** – when `y = NULL`, the count of observations
    per x level (and facet) is computed and used as `y`.

4.  **Error bar validation** – if `add_errorbars = TRUE`, checks that
    either `errorbar_min`/`errorbar_max` or `errorbar_sd` is provided.

5.  **NA / empty-level handling** –
    [`process_keep_na_empty()`](https://pwwang.github.io/plotthis/reference/process_keep_na_empty.md)
    applies `keep_na` and `keep_empty` policies.

6.  **Highlight parsing** – `highlight` (indices, row names, expression
    string, or `TRUE`) is resolved to a subset of data rows.

7.  **Background layer** – when `add_bg = TRUE`, alternating background
    stripes are drawn via
    [`bg_layer()`](https://pwwang.github.io/plotthis/reference/bg_layer.md).

8.  **Horizontal reference line** – when `add_hline` is numeric, a
    `geom_hline()` is added at that intercept.

9.  **Colour assignment** –
    [`palette_this()`](https://pwwang.github.io/plotthis/reference/palette_this.md)
    assigns colours to all x-axis levels (including `NA`, defaulting to
    `"grey80"`).

10. **Line rendering** – when `color_line_by_x = TRUE`, lines are
    coloured per x level via
    [`geom_line()`](https://ggplot2.tidyverse.org/reference/geom_path.html)
    with `aes(color = x)` and `scale_color_manual()`. Otherwise a single
    colour (the first palette entry) is used.

11. **Error bars** – when `add_errorbars = TRUE`, three colour modes are
    supported: follow the line colour (`"line"`), track x levels, or use
    a constant colour. Error bars are added via
    [`geom_errorbar()`](https://ggplot2.tidyverse.org/reference/geom_linerange.html).

12. **Point rendering** – when `fill_point_by_x = TRUE`, points are
    filled per x level via
    [`geom_point()`](https://ggplot2.tidyverse.org/reference/geom_point.html)
    with `aes(fill = x)` and `scale_fill_manual()`. Otherwise a single
    fill colour (first palette entry) is used.

13. **Highlight overlay** – an additional `geom_point()` layer draws
    highlighted rows in the specified colour and size.

14. **Plot assembly** – `scale_x_discrete()`, `labs()`, theme
    application, axis styling (angle, grid lines), and legend
    positioning.

15. **Dimension calculation** –
    [`calculate_plot_dimensions()`](https://pwwang.github.io/plotthis/reference/calculate_plot_dimensions.md)
    computes `height` and `width` attributes (in inches) from x-axis
    category count, aspect ratio, and legend metrics.
