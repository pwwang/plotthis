# Atomic dot/lollipop plot

Core implementation for dot plots and lollipop plots. This is the
internal workhorse dispatched by both
[`DotPlot()`](https://pwwang.github.io/plotthis/reference/dotplot.md)
and
[`LollipopPlot()`](https://pwwang.github.io/plotthis/reference/dotplot.md).
It renders a matrix of points where dot size encodes one variable and
dot fill colour encodes another, with optional background stripes and a
lollipop (bar + dot) display mode.

## Usage

``` r
DotPlotAtomic(
  data,
  x,
  y,
  x_sep = "_",
  y_sep = "_",
  flip = FALSE,
  lollipop = FALSE,
  size_by = NULL,
  fill_by = NULL,
  fill_cutoff = NULL,
  palreverse = FALSE,
  size_name = NULL,
  fill_name = NULL,
  fill_cutoff_name = NULL,
  size_min = 1,
  size_max = 10,
  theme = "theme_this",
  theme_args = list(),
  palette = "Spectral",
  palcolor = NULL,
  alpha = 1,
  border_color = "black",
  border_size = 0.5,
  border_alpha = 1,
  lower_quantile = 0,
  upper_quantile = 0.99,
  lower_cutoff = NULL,
  upper_cutoff = NULL,
  facet_by = NULL,
  facet_scales = "fixed",
  facet_ncol = NULL,
  facet_nrow = NULL,
  facet_byrow = TRUE,
  x_text_angle = 0,
  aspect.ratio = 1,
  legend.position = "right",
  legend.direction = "vertical",
  add_bg = FALSE,
  bg_palette = "stripe",
  bg_palcolor = NULL,
  bg_alpha = 0.2,
  bg_direction = c("vertical", "horizontal", "v", "h"),
  title = NULL,
  subtitle = NULL,
  xlab = NULL,
  ylab = NULL,
  keep_na = FALSE,
  keep_empty = FALSE,
  ...
)
```

## Arguments

- data:

  A data frame.

- x:

  A character vector specifying the column(s) to use for the x-axis. Can
  be numeric (for scatter/lollipop mode) or factor/character (for dot
  matrix mode). When multiple columns are provided for a non-numeric
  axis, they are concatenated with `x_sep` as the separator.

- y:

  A character vector specifying the column(s) to use for the y-axis. Can
  be numeric (for scatter mode) or factor/character (for dot matrix or
  lollipop mode). When multiple columns are provided for a non-numeric
  axis, they are concatenated with `y_sep` as the separator.

- x_sep:

  A character string used to join multiple `x` column values into a
  single factor level. Only used when x is non-numeric and multiple
  columns are provided. Default: `"_"`.

- y_sep:

  A character string used to join multiple `y` column values into a
  single factor level. Only used when y is non-numeric and multiple
  columns are provided. Default: `"_"`.

- flip:

  A logical value. If `TRUE`, the x and y axes are swapped via
  `coord_flip()`. Dimension calculation accounts for the flip. Default:
  `FALSE`.

- lollipop:

  A logical value. If `TRUE`, renders a lollipop plot with bars
  extending from x = 0 to each data point, capped by filled dots.
  Requires `x` to be numeric and `y` to be factor/character. Default:
  `FALSE`.

- size_by:

  A character string naming a numeric column whose values control dot
  size. When `NULL` (the default), the per-combination observation count
  is computed automatically (via `dplyr::summarise(n = n())`) and used
  as the size variable. If `fill_by` is also present, the first value of
  `fill_by` per combination is retained with a warning. A single numeric
  value is also accepted and sets a constant dot size (used by
  `ScatterPlot`).

- fill_by:

  A character string naming a numeric column whose values control the
  fill colour of the dots (and lollipop inner bars). A continuous
  gradient from `palette` is applied via `scale_fill_gradientn()`. When
  `NULL` (the default), all dots are filled with a single constant
  colour from the middle of the palette.

- fill_cutoff:

  A string expression specifying which values of `fill_by` to grey out.
  Format: an operator followed by a number, e.g. `"< 18"`, `"<= 18"`,
  `"> 18"`, or `">= 18"`. Values matching the condition are set to `NA`
  and rendered in grey (`"grey80"`), while the rest are coloured by the
  fill gradient. The operator determines which side of the threshold is
  greyed out, independent of `palreverse`. A numeric value is also
  accepted as shorthand for `"<"` (e.g. `18` is equivalent to `"< 18"`).
  Requires `fill_by` to be set.

- palreverse:

  A logical value indicating whether to reverse the palette. Default is
  FALSE.

- size_name:

  A character string for the size legend title. When `NULL` (the
  default), the `size_by` column name is used.

- fill_name:

  A character string for the fill colour-bar legend title. When `NULL`
  (the default), the `fill_by` column name is used.

- fill_cutoff_name:

  A character string for the fill cutoff legend title (shown when
  `fill_cutoff` is active). Defaults to `"<fill_by> <fill_cutoff>"`,
  e.g. `"mpg < 18"`.

- size_min:

  A numeric value for the smallest dot size in the
  `scale_size(range = c(size_min, size_max))` range. Default: `1`.

- size_max:

  A numeric value for the largest dot size in the
  `scale_size(range = c(size_min, size_max))` range. Default: `10`.

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

- border_color:

  Controls the dot border colour and lollipop outer-shadow appearance:

  - `TRUE` — dot borders and lollipop inner bars follow the `fill_by`
    gradient via `scale_color_gradientn()`; lollipop outer shadow is
    black.

  - `"black"` (default) — constant black borders on dots and black outer
    shadow on lollipop bars.

  - A colour string (e.g. `"red"`, `"#FF0000"`) — constant colour for
    both dot borders and lollipop outer shadows.

  - `FALSE` — no dot borders and no lollipop outer shadow (the inner
    coloured bars remain visible in lollipop mode).

- border_size:

  A numeric value for the stroke width of dot borders and the base
  linewidth of lollipop bars. In lollipop mode, the outer shadow uses
  `border_size * 4` and the inner bar uses `border_size * 2`. Default:
  `0.5`.

- border_alpha:

  A numeric value in `[0, 1]` controlling the transparency of dot
  borders and lollipop bar segments. Default: `1`.

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

- add_bg:

  A logical value. If `TRUE`, alternating background stripes are drawn
  behind the points via
  [`bg_layer()`](https://pwwang.github.io/plotthis/reference/bg_layer.md).
  The striped axis is determined by `bg_direction`. Requires the striped
  axis to be non-numeric. Default: `FALSE`.

- bg_palette:

  A character string specifying the palette for the background stripe
  colours. Passed to
  [`bg_layer()`](https://pwwang.github.io/plotthis/reference/bg_layer.md).
  Default: `"stripe"`.

- bg_palcolor:

  A character vector of colours for the background stripes. Passed to
  [`bg_layer()`](https://pwwang.github.io/plotthis/reference/bg_layer.md).
  When `NULL` (default), colours are derived from `bg_palette`.

- bg_alpha:

  A numeric value in `[0, 1]` for the transparency of the background
  stripes. Default: `0.2`.

- bg_direction:

  A character string specifying which axis receives the alternating
  background stripes. `"vertical"` (default) stripes by x levels;
  `"horizontal"` stripes by y levels. Abbreviations `"v"` and `"h"` are
  also accepted.

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

- ...:

  Additional arguments.

## Value

A `ggplot` object with `height` and `width` attributes.

## Details

The function supports two display modes:

- **Dot plot** (`lollipop = FALSE`, the default) — renders
  [`ggplot2::geom_point()`](https://ggplot2.tidyverse.org/reference/geom_point.html)
  with `shape = 21` (filled circle). Either or both axes can be numeric
  (producing a scatter plot) or factor (producing a dot matrix). Dot
  size scales by `size_by` (or the per-combination observation count
  when `size_by` is `NULL`) via
  `scale_size(range = c(size_min, size_max))`. Dot fill follows the
  `fill_by` column via `scale_fill_gradientn()`.

- **Lollipop plot** (`lollipop = TRUE`) — expects a numeric `x` and a
  factor/character `y`. Renders a two-layer bar: an outer shadow segment
  followed by an inner coloured segment, each capped by a filled dot.
  The outer shadow is black (or a custom colour when `border_color` is a
  string), and the inner segment + dot fill follow `fill_by`.

When `add_bg = TRUE`, alternating background stripes are drawn via
[`bg_layer()`](https://pwwang.github.io/plotthis/reference/bg_layer.md)
along the discrete axis (vertical stripes for factor `x`, horizontal
stripes for factor `y`). The fill colour scale is prepared via
[`prepare_continuous_color_scale()`](https://pwwang.github.io/plotthis/reference/prepare_continuous_color_scale.md)
and can be trimmed with `lower_quantile`/`upper_quantile` or
`lower_cutoff`/`upper_cutoff`.

When `fill_cutoff` is set (e.g. `"< 18"`), values of `fill_by` matching
the condition are set to `NA` and rendered in grey ("grey80"), with a
separate legend entry documenting the cutoff.

**Border colour modes:**

- `border_color = TRUE` — dot borders and lollipop inner bar colour
  track the `fill_by` gradient via `scale_color_gradientn()`; lollipop
  outer shadow is black.

- `border_color = "black"` (default) — constant black borders on dots
  and a black outer shadow on lollipop bars.

- `border_color = FALSE` — no dot borders, no lollipop outer shadow (the
  inner coloured segment remains).

## Architecture

**DotPlotAtomic** executes the following steps:

1.  **ggplot dispatch** — selects `gglogger::ggplot` or
    [`ggplot2::ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)
    based on `getOption("plotthis.gglogger.enabled")`.

2.  **bg_direction normalisation** —
    [`match.arg()`](https://rdrr.io/r/base/match.arg.html) resolves
    `"v"`/`"h"` abbreviations to `"vertical"`/`"horizontal"`.

3.  **Axis type detection** — determines whether `x` (resp. `y`) is
    numeric by checking that the column is a single name and the data
    column is neither character nor factor.

4.  **Column resolution** — non-numeric x and y are processed via
    [`check_columns()`](https://pwwang.github.io/plotthis/reference/check_columns.md)
    with `force_factor = TRUE, allow_multi = TRUE, concat_multi = TRUE`
    using `x_sep`/`y_sep`. `fill_by` and `facet_by` are also validated.

5.  **NA / empty handling** —
    [`process_keep_na_empty()`](https://pwwang.github.io/plotthis/reference/process_keep_na_empty.md)
    filters data and extracts `keep_empty` settings for x, y, and
    facet_by.

6.  **Multi-facet keep_empty guard** — when `facet_by` has more than one
    column, the `keep_empty` values must be identical for all facet
    columns (consistent drop behaviour).

7.  **fill_cutoff guard** — errors if `fill_cutoff` is set but `fill_by`
    is `NULL`.

8.  **Size-by resolution** — when `size_by = NULL`:

    - Groups data by the unique combination of x, y, and facet_by
      columns and counts rows per combination into `.size`.

    - If `fill_by` is present, `summarise()` also takes the first value
      of `fill_by` per group (with a warning that only the first value
      is used).

    - Factor levels of x, y, and facet_by are preserved post-summary.

    - Sets `size_by <- ".size"`.

9.  **fill_cutoff parsing** — if `fill_cutoff` is set:

    - Numeric shorthand (e.g. `18`) is converted to `"< 18"`.

    - The string is parsed with regex `^(<=?|>=?)\\s*(-?[0-9.]+)$` to
      extract operator and numeric threshold.

    - A [`switch()`](https://rdrr.io/r/base/switch.html) on the operator
      NAs-out matching values in `fill_by` (e.g. `"<"` sets values
      *below* the threshold to `NA`).

    - A label `"<fill_by> <fill_cutoff>"` is generated for the legend.

10. **Default fill_by** — when `fill_by = NULL`, a synthetic `.fill_by`
    column (constant `1`) is created and the fill legend is suppressed.

11. **Continuous colour scale preparation** — when `fill_by` is numeric,
    [`prepare_continuous_color_scale()`](https://pwwang.github.io/plotthis/reference/prepare_continuous_color_scale.md)
    computes `feat_colors_value` (the range endpoints after optional
    quantile trimming or cutoff clamping).

12. **Base ggplot** — initialises `ggplot(data, aes(x, y))`.

13. **Background layer** — if `add_bg = TRUE`:

    - Vertical stripes (`bg_direction = "vertical"`) require a
      non-numeric x-axis; horizontal stripes require a non-numeric
      y-axis.

    - Calls
      [`bg_layer()`](https://pwwang.github.io/plotthis/reference/bg_layer.md)
      with the relevant discrete column and stripe styling.

14. **Discrete axis scales** — `scale_x_discrete()` /
    `scale_y_discrete()` with `drop = !isTRUE(keep_empty_*)` for
    non-numeric axes.

15. **Lollipop branch** (`lollipop = TRUE`):

    - Sets x-axis expansion to `c(0, 0, 0.05, 0)` (bars start at x = 0
      with a 5\\

    - **Outer shadow** — `geom_segment()` from x = 0 to the data value,
      coloured black (or `border_color` when a string), with linewidth
      `border_size * 4`. Skipped when `border_color = FALSE`.

    - **Inner coloured bar** — `geom_segment()` mapped to the `fill_by`
      gradient via `scale_color_gradientn()`, with linewidth
      `border_size * 2`.

    - [`ggnewscale::new_scale_color()`](https://eliocamp.github.io/ggnewscale/reference/new_scale.html)
      resets the colour scale for the subsequent point layer.

16. **Point layer** — `geom_point(shape = 21)` (filled circle with
    border) with four dispatch paths:

    - *Numeric size + gradient border*: `aes(fill, color)` both mapped
      to `fill_by`; `size = size_by` constant.

    - *Numeric size + constant/no border*: `aes(fill, color = "")` with
      a constant colour string.

    - *Column size + gradient border*: `aes(size, fill, color)` with
      `scale_size(range = c(size_min, size_max))` and a size legend
      (title = `size_name`, order = 1).

    - *Column size + constant/no border*: `aes(size, fill, color = "")`
      with the same size scale.

17. **Fill scale** — `scale_fill_gradientn()` with
    [`palette_this()`](https://pwwang.github.io/plotthis/reference/palette_this.md),
    `feat_colors_value` for rescaled colour mapping,
    `na.value = "grey80"`, and a colour-bar legend (title = `fill_name`,
    order = 2) or `guide_none()` when no `fill_by` was provided.

18. **Labels** — `labs(title, subtitle, x, y)` with fallback to column
    names via `%||%`.

19. **Theme** — `do_call(theme, theme_args)` plus `panel.grid.major`
    (grey80 dashed) and rotated x-axis text via
    `calc_just(x_text_angle)`.

20. **Colour (border) scale**:

    - `border_color = TRUE` — `scale_color_gradientn()` following the
      `fill_by` gradient (with border_alpha), guide suppressed (the fill
      colour-bar serves both).

    - `border_color = FALSE` — `scale_color_manual()` with
      `"transparent"`, guide suppressed.

    - Constant string — `scale_color_manual()` with the alpha-adjusted
      colour, guide suppressed.

21. **fill_cutoff legend** — when `fill_cutoff` is active and there are
    NA values in `fill_by`, a `guide_legend()` (order = 3) is added
    showing the cutoff label with a grey fill and the border colour.

22. **Flip** — optional `coord_flip()` swaps x and y axes.

23. **Dimension calculation** —
    [`calculate_plot_dimensions()`](https://pwwang.github.io/plotthis/reference/calculate_plot_dimensions.md)
    with `base_height = 4.5`, `aspect.ratio = NULL`, and per-axis scale
    factors (0.9 for the categorical axis driving width, 0.6 for the
    categorical axis driving height). Legend width, y-axis label length,
    and minimum dimensions (width ≥ 5, height ≥ 4) are factored in. A
    fallback manual calculation is used when
    [`calculate_plot_dimensions()`](https://pwwang.github.io/plotthis/reference/calculate_plot_dimensions.md)
    returns `NULL`.

24. **Faceting** —
    [`facet_plot()`](https://pwwang.github.io/plotthis/reference/facet_plot.md)
    applies `facet_grid`/`facet_wrap` with
    `drop = !isTRUE(keep_empty_facet)`.
