# Atomic scatter plot (internal)

Core implementation for drawing a single scatter plot. This is the
workhorse behind the exported
[`ScatterPlot`](https://pwwang.github.io/plotthis/reference/ScatterPlot.md)
function – it takes a **single** data frame (no `split_by` support) and
returns a `ggplot` object. The plot positions points by `x` and `y`
coordinates (both numeric), with optional size encoding via a third
numeric variable (`size_by`), and colour encoding (`color_by`) that can
be numeric (continuous gradient) or categorical (discrete palette).

Key features:

- **Variable point size** – `size_by` accepts either a numeric constant
  (uniform dot size) or a column name (size scales with value via
  `scale_size_area()`).

- **Colour modes** – `color_by` can be numeric (rendered as a continuous
  gradient via `scale_fill_gradientn()` or `scale_color_gradientn()`) or
  factor/character (discrete palette via
  `scale_fill_manual()`/`scale_color_manual()`).

- **Shape support** – shapes 21–25 support separate fill and border
  (`color`) aesthetics; all other shapes use only the colour aesthetic.
  The `border_color` parameter can be a constant colour, `TRUE` (track
  the fill gradient), or omitted.

- **Colour scale trimming** – `lower_quantile` / `upper_quantile` (or
  explicit `lower_cutoff` / `upper_cutoff`) trim/clamp the continuous
  colour scale extremes.

- **Axis transformation** – `xtrans` and `ytrans` apply arbitrary scale
  transformations (e.g. `"log10"`, `"sqrt"`) to the x and y axes via
  `scale_x_continuous()` / `scale_y_continuous()`.

- **Point highlighting** – `highlight` accepts indices, rownames,
  logical `TRUE`, or a string expression to select points that are
  overlaid with a distinct shape, size, colour, and alpha.

## Usage

``` r
ScatterPlotAtomic(
  data,
  x,
  y,
  size_by = 2,
  size_name = NULL,
  color_by = NULL,
  color_name = NULL,
  palreverse = FALSE,
  theme = "theme_this",
  theme_args = list(),
  alpha = ifelse(shape %in% 21:25, 0.65, 1),
  shape = 21,
  border_color = "black",
  lower_quantile = 0,
  upper_quantile = 0.99,
  lower_cutoff = NULL,
  upper_cutoff = NULL,
  xtrans = "identity",
  ytrans = "identity",
  highlight = NULL,
  highlight_shape = 16,
  highlight_size = 3,
  highlight_color = "red",
  highlight_alpha = 1,
  palette = ifelse(!is.null(color_by) && !is.numeric(data[[color_by]]), "Paired",
    "Spectral"),
  palcolor = NULL,
  facet_by = NULL,
  facet_scales = "fixed",
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

  A character string specifying the numeric column to use for the
  x-axis. A numeric column is expected.

- y:

  A character string specifying the numeric column to use for the
  y-axis. A numeric column is expected.

- size_by:

  Either a numeric constant (uniform dot size) or a character string
  naming a numeric column whose values control dot size via
  `scale_size_area(max_size = 6)`. Default: `2`.

- size_name:

  A character string for the size legend title. When `NULL` (default),
  the `size_by` column name is used. Ignored when `size_by` is a numeric
  constant.

- color_by:

  A character string naming a column whose values control dot colour.
  Can be numeric (continuous gradient via `scale_fill_gradientn()` /
  `scale_color_gradientn()`) or factor/character (discrete palette via
  `scale_fill_manual()` / `scale_color_manual()`). For shapes 21–25, the
  colour is applied to the fill aesthetic. When `NULL` (default), all
  dots are rendered in a single colour derived from the palette.

- color_name:

  A character string for the colour legend title. When `NULL` (default),
  the `color_by` column name is used.

- palreverse:

  A logical value indicating whether to reverse the palette. Default is
  FALSE.

- theme:

  A character string or a theme class (i.e. ggplot2::theme_classic)
  specifying the theme to use. Default is "theme_this".

- theme_args:

  A list of arguments to pass to the theme function.

- alpha:

  A numeric value specifying the transparency of the plot.

- shape:

  A numeric value specifying the point shape. Default: `21` (filled
  circle with border). Shapes 21–25 support separate fill and border
  colour aesthetics; all other shapes use a single colour aesthetic.

- border_color:

  Controls the point border colour. For shapes 21–25:

  - `"black"` (default) – constant black border.

  - A colour string (e.g. `"red"`, `"#FF0000"`) – constant colour
    border.

  - `TRUE` – border colour tracks the `color_by` gradient / palette via
    `scale_color_gradientn()` / `scale_color_manual()`.

  For shapes without a fill aesthetic (not 21–25), this parameter has no
  effect.

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

- xtrans:

  A character string specifying the transformation for the x-axis,
  passed to `scale_x_continuous(trans = ...)`. Common options:
  `"identity"` (default), `"log10"`, `"log2"`, `"sqrt"`, `"reverse"`.
  See
  [`ggplot2::continuous_scale()`](https://ggplot2.tidyverse.org/reference/continuous_scale.html)
  for a full list.

- ytrans:

  A character string specifying the transformation for the y-axis,
  passed to `scale_y_continuous(trans = ...)`. Common options:
  `"identity"` (default), `"log10"`, `"log2"`, `"sqrt"`, `"reverse"`.

- highlight:

  Specifies which points to highlight with an overlaid `geom_point()`
  layer. Accepted values:

  - `NULL` (default) – no highlighting.

  - `TRUE` – all points are highlighted.

  - A numeric vector – row indices of points to highlight.

  - A single character string – an R expression (e.g. `"x > 0"`) that is
    parsed with
    [`rlang::parse_expr()`](https://rlang.r-lib.org/reference/parse_expr.html)
    and evaluated via
    [`filter()`](https://dplyr.tidyverse.org/reference/filter.html) to
    select rows.

  - A character vector – rownames of points to highlight. An error is
    thrown if the data has no rownames.

- highlight_shape:

  A numeric value specifying the point shape for highlighted points.
  Default: `16` (filled circle). Shapes 21–25 use the `fill` aesthetic;
  other shapes use `color`.

- highlight_size:

  A numeric value specifying the size of highlighted points. Default:
  `3`.

- highlight_color:

  A character string specifying the colour of highlighted points.
  Default: `"red"`.

- highlight_alpha:

  A numeric value in `[0, 1]` specifying the transparency of highlighted
  points. Default: `1`.

- palette:

  A character string specifying the palette to use. A named list or
  vector can be used to specify the palettes for different `split_by`
  values.

- palcolor:

  A character string specifying the color to use in the palette. A named
  list can be used to specify the colors for different `split_by`
  values. If some values are missing, the values from the palette will
  be used (palcolor will be NULL for those values).

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

1.  **ggplot dispatch** – selects `gglogger::ggplot` or
    [`ggplot2::ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)
    based on `getOption("plotthis.gglogger.enabled")`.

2.  **Column resolution** – `facet_by` is validated and forced to factor
    via
    [`check_columns()`](https://pwwang.github.io/plotthis/reference/check_columns.md).
    `size_by` is validated when it is a column name; numeric constants
    are kept as-is. `color_by` is validated.

3.  **Dummy colour guard** – when `color_by = NULL`, a synthetic
    `.color_by` column (constant `""`) is created and the colour legend
    is suppressed.

4.  **Categorical colour coercion** – when `color_by` is a non-numeric
    column, it is forced to factor via
    [`check_columns()`](https://pwwang.github.io/plotthis/reference/check_columns.md)
    with `force_factor = TRUE`.

5.  **Continuous colour scale preparation** – when `color_by` is
    numeric,
    [`prepare_continuous_color_scale()`](https://pwwang.github.io/plotthis/reference/prepare_continuous_color_scale.md)
    computes quantile-trimmed or cutoff-clamped colour range endpoints
    (`feat_colors_value`) and winsorizes out-of-range data.

6.  **Highlight resolution** – `highlight` is processed into a subset
    data frame (`hidata`) via one of four paths: `TRUE` (all points),
    numeric index vector, a single string expression (parsed with
    [`rlang::parse_expr()`](https://rlang.r-lib.org/reference/parse_expr.html)
    and applied via
    [`filter()`](https://dplyr.tidyverse.org/reference/filter.html)), or
    a character vector of rownames. An error is thrown if rownames are
    used on data without row names.

7.  **Shape fill detection** – determines whether the point shape
    (`shape`) supports a separate fill aesthetic (shapes 21–25).

8.  **Base ggplot** – initialises `ggplot(data, aes(x, y))`.

9.  **Point layer configuration** – the aesthetic mapping and constant
    aesthetics for `geom_point()` are assembled:

    - *Fill vs colour*: shapes 21–25 map `fill` (and optionally `color`
      when `border_color = TRUE`); other shapes map `color`.

    - *Size*: a numeric `size_by` is passed as a constant aesthetic; a
      column name is mapped to the `size` aesthetic.

10. **Size scale** – when `size_by` is a column name,
    `scale_size_area(max_size = 6)` is added with a size legend (order =
    1, title = `size_name %||% size_by`).

11. **Fill / colour scale (4 branches)**:

    - *Shape with fill + numeric colour*: `scale_fill_gradientn()` with
      `feat_colors_value` rescaling and either a colour-bar legend or
      suppressed guide. When `border_color = TRUE`, a matching
      `scale_color_gradientn()` is added.

    - *Shape with fill + factor colour*: `scale_fill_manual()` with
      [`palette_this()`](https://pwwang.github.io/plotthis/reference/palette_this.md)
      colours and a discrete legend. When `border_color = TRUE`, a
      matching `scale_color_manual()` is added.

    - *Shape without fill + numeric colour*: `scale_color_gradientn()`
      with continuous colour-bar legend.

    - *Shape without fill + factor colour*: `scale_color_manual()` with
      discrete legend.

12. **Highlight overlay** – when `hidata` is non-`NULL`, a second
    `geom_point()` layer is added using `highlight_shape`,
    `highlight_color`, `highlight_size`, and `highlight_alpha`. The fill
    or colour aesthetic is chosen based on whether `highlight_shape` is
    21–25.

13. **Axis scales** – `scale_x_continuous(trans = xtrans)` and
    `scale_y_continuous(trans = ytrans)` apply the requested
    transformations.

14. **Labels and theme** – `labs(title, subtitle, x, y)` sets plot
    annotations. `do_call(theme, theme_args)` applies the theme.
    `panel.grid.major` is set to grey80 dashed lines.

15. **Dimension calculation** –
    [`calculate_plot_dimensions()`](https://pwwang.github.io/plotthis/reference/calculate_plot_dimensions.md)
    computes plot `height` and `width` from `base_height = 5`,
    `aspect.ratio`, and legend metrics (number of legend items and
    maximum label character width).

16. **Faceting** –
    [`facet_plot()`](https://pwwang.github.io/plotthis/reference/facet_plot.md)
    wraps the plot with `facet_wrap` / `facet_grid` if `facet_by` is
    provided.
