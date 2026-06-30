# Atomic split bar plot (internal)

Core implementation for drawing a split (divergent / waterfall-style)
bar plot. Bars extend left (negative values) and right (positive values)
from a central zero line, with the y-axis listing categories. Bars can
be coloured by a categorical or continuous fill variable, and their
opacity can encode an additional numeric variable via `alpha_by`.

This is the workhorse behind the exported
[`SplitBarPlot`](https://pwwang.github.io/plotthis/reference/barplot.md)
(also aliased as
[`WaterfallPlot`](https://pwwang.github.io/plotthis/reference/barplot.md)).

## Usage

``` r
SplitBarPlotAtomic(
  data,
  x,
  y,
  y_sep = "_",
  flip = FALSE,
  alpha_by = NULL,
  alpha_reverse = FALSE,
  alpha_name = NULL,
  order_y = list(`+` = c("x_desc", "alpha_desc"), `-` = c("x_desc", "alpha_asc")),
  bar_height = 0.9,
  lineheight = 0.5,
  max_charwidth = 80,
  fill_by = NULL,
  fill_by_sep = "_",
  fill_name = NULL,
  direction_name = "direction",
  direction_pos_name = "positive",
  direction_neg_name = "negative",
  theme = "theme_this",
  theme_args = list(),
  palette = "Spectral",
  palcolor = NULL,
  palreverse = FALSE,
  lower_quantile = 0,
  upper_quantile = 0.99,
  lower_cutoff = NULL,
  upper_cutoff = NULL,
  facet_by = NULL,
  facet_scales = "free_y",
  facet_nrow = NULL,
  facet_ncol = NULL,
  facet_byrow = TRUE,
  aspect.ratio = 1,
  x_min = NULL,
  x_max = NULL,
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

  A character string specifying the numeric column for the x-axis.
  Values determine bar direction: positive → right, negative → left.

- y:

  A character string (or vector) specifying the column(s) for the
  category axis. Each unique value becomes a bar. Multiple columns are
  concatenated with `y_sep`.

- y_sep:

  A character string to join multiple `y` columns. Default `"_"`.

- flip:

  Logical; if `TRUE`, swap the x and y axes (bars become vertical rather
  than horizontal).

- alpha_by:

  A character string naming a numeric column to encode as bar opacity.
  Default `NULL` (all bars fully opaque).

- alpha_reverse:

  Logical; if `TRUE`, reverse the alpha scale direction (solid for low
  values, transparent for high).

- alpha_name:

  A character string for the alpha legend title.

- order_y:

  A named list controlling the vertical ordering of bars. Keys are `"+"`
  (positive bars), `"-"` (negative bars), or `"*"` (all bars). Values
  are character vectors of ordering criteria: `"x_asc"`, `"x_desc"`,
  `"alpha_asc"`, `"alpha_desc"`. Default orders positive bars by
  descending x and descending alpha; negative bars by descending x and
  ascending alpha.

- bar_height:

  A numeric value (0–1) specifying the bar height as a fraction of the
  available category slot.

- lineheight:

  A numeric value controlling the line height of wrapped category
  labels.

- max_charwidth:

  An integer specifying the maximum character width for wrapping
  category labels.

- fill_by:

  A character string (or vector) naming the column(s) for bar fill
  colour. If `NULL`, the direction (positive/negative) is used. Can be
  categorical or numeric.

- fill_by_sep:

  A character string to join multiple `fill_by` columns. Default `"_"`.

- fill_name:

  A character string for the fill legend title.

- direction_name:

  A character string naming the internal direction column (used in
  legends). Default `"direction"`.

- direction_pos_name:

  A character string labelling the positive direction in the legend.
  Default `"positive"`.

- direction_neg_name:

  A character string labelling the negative direction in the legend.
  Default `"negative"`.

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

- aspect.ratio:

  A numeric value specifying the aspect ratio of the plot.

- x_min, x_max:

  Numeric limits for the x-axis. When `NULL`, symmetric limits are
  computed from the maximum absolute x-value.

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

A `ggplot` object, possibly faceted, with `height` and `width`
attributes (in inches) attached.

## Architecture

1.  **Column resolution** — `x`, `y`, `fill_by`, `alpha_by`, and
    `facet_by` are validated via
    [`check_columns`](https://pwwang.github.io/plotthis/reference/check_columns.md).
    Multi-column `y` and `fill_by` are concatenated with their
    respective separators.

2.  **Direction assignment** — a `direction_name` column is created from
    the sign of `x`, assigning each row to the positive or negative
    group (customisable via `direction_pos_name` /
    `direction_neg_name`).

3.  **Fill resolution** — if `fill_by` is `NULL`, the direction column
    is used as the fill (two-colour palette). Categorical fills use a
    discrete colour scale; numeric fills use
    [`prepare_continuous_color_scale()`](https://pwwang.github.io/plotthis/reference/prepare_continuous_color_scale.md)
    with quantile / cutoff clamping and `scale_fill_gradientn()`.

4.  **Alpha by** — when `alpha_by` is provided, bar opacity encodes an
    additional numeric variable via `scale_alpha_continuous()`.

5.  **Ordering** — `order_y` is a named list controlling the vertical
    ordering of bars. The keys `"+"` and `"-"` specify separate
    orderings for positive and negative bars; key `"*"` applies a single
    ordering to all bars. Values are character vectors of ordering
    criteria: `"x_asc"`, `"x_desc"`, `"alpha_asc"`, `"alpha_desc"`.

6.  **Empty level padding** — when `keep_empty_y = TRUE`, missing
    y-levels are padded per facet (or globally) with zero-height bars so
    they still appear on the axis.

7.  **Text labels** — category names are drawn beside the bars via
    `geom_text()` at the zero line. When flipped, labels are rotated
    90°. Long labels are wrapped via
    [`str_wrap()`](https://stringr.tidyverse.org/reference/str_wrap.html)
    using `max_charwidth`.

8.  **Plot assembly** — the ggplot is built with
    `geom_vline(xintercept = 0)` for the centre line, `geom_col()` for
    bars, and the appropriate fill / alpha scales.

9.  **Dimension calculation** —
    [`calculate_plot_dimensions()`](https://pwwang.github.io/plotthis/reference/calculate_plot_dimensions.md)
    computes dimensions from the number of y categories, scaled by
    `bar_height / 4`. Flipping adjusts the aspect ratio.

10. **Faceting** —
    [`facet_plot()`](https://pwwang.github.io/plotthis/reference/facet_plot.md)
    wraps the result, defaulting to `facet_scales = "free_y"`.
