# Atomic density/histogram plot

Core implementation for density and histogram plots. This is the
internal workhorse dispatched by both the
[`DensityPlot()`](https://pwwang.github.io/plotthis/reference/densityhistoplot.md)
and
[`Histogram()`](https://pwwang.github.io/plotthis/reference/densityhistoplot.md)
public wrappers. It renders a grouped density curve or histogram, with
optional data-distribution bars along the y=0 axis, trend-line
interpolation (histogram only), and full faceting support.

The function supports two plotting modes selected by `type`:

- **density** — renders
  [`ggplot2::geom_density()`](https://ggplot2.tidyverse.org/reference/geom_density.html)
  for each group.

- **histogram** — renders
  [`ggplot2::geom_histogram()`](https://ggplot2.tidyverse.org/reference/geom_histogram.html)
  with optional trend overlays (`use_trend`, `add_trend`), including
  zero-skip interpolation (`trend_skip_zero`) that uses
  [`zoo::na.approx()`](https://rdrr.io/pkg/zoo/man/na.approx.html) to
  bridge gaps where a bin has zero observations under a transformed
  y-axis.

When `add_bars = TRUE`, a data rug is drawn along the bottom of the plot
using
[`ggplot2::geom_linerange()`](https://ggplot2.tidyverse.org/reference/geom_linerange.html).
Each group's bars are offset vertically so they stack without
overlapping.

## Usage

``` r
DensityHistoPlotAtomic(
  data,
  x,
  group_by = NULL,
  group_by_sep = "_",
  group_name = NULL,
  xtrans = "identity",
  ytrans = "identity",
  type = c("density", "histogram"),
  bins = NULL,
  binwidth = NULL,
  flip = FALSE,
  keep_na = FALSE,
  keep_empty = FALSE,
  add_bars = FALSE,
  bar_height = 0.025,
  bar_alpha = 1,
  bar_width = 0.1,
  position = "identity",
  use_trend = FALSE,
  add_trend = FALSE,
  trend_alpha = 1,
  trend_linewidth = 0.8,
  trend_pt_size = 1.5,
  trend_skip_zero = FALSE,
  palette = "Paired",
  palcolor = NULL,
  palreverse = FALSE,
  alpha = 0.5,
  theme = "theme_this",
  theme_args = list(),
  aspect.ratio = 1,
  title = NULL,
  subtitle = NULL,
  xlab = NULL,
  ylab = NULL,
  expand = c(bottom = 0, left = 0, right = 0),
  facet_by = NULL,
  facet_scales = "fixed",
  facet_ncol = NULL,
  facet_nrow = NULL,
  facet_byrow = TRUE,
  legend.position = ifelse(is.null(group_by), "none", "right"),
  legend.direction = "vertical",
  ...
)
```

## Arguments

- data:

  A data frame.

- x:

  A character string specifying the column name for the x-axis values. A
  numeric column is expected.

- group_by:

  A character string specifying the column(s) to group the data by.
  Multiple columns are concatenated with `group_by_sep`. Each group
  receives a distinct fill and outline colour.

- group_by_sep:

  A character string used to join multiple `group_by` column values into
  a single factor level. Default: `"_"`.

- group_name:

  A character string used as the legend title for the `group_by`
  aesthetic. When `NULL` (default), the (possibly concatenated)
  `group_by` column name is used.

- xtrans:

  A character string specifying the transformation applied to the
  x-axis. Passed to `ggplot2::scale_x_continuous(transform = ...)`.
  Supported values include `"identity"` (default), `"log10"`, `"log2"`,
  `"sqrt"`, `"reverse"`, etc.

- ytrans:

  A character string specifying the transformation applied to the
  y-axis. Passed to `ggplot2::scale_y_continuous(transform = ...)`. Used
  by `trend_skip_zero` to correctly interpolate across zero bins on a
  transformed scale. Default: `"identity"`.

- type:

  A character string specifying the plot type. `"density"` (default)
  renders `geom_density()`; `"histogram"` renders `geom_histogram()`
  with optional trend overlays.

- bins:

  A numeric value specifying the number of bins for the histogram.
  Ignored when `type = "density"`. Defaults to `30` when neither `bins`
  nor `binwidth` is provided.

- binwidth:

  A numeric value specifying the width of individual bins for the
  histogram. Ignored when `type = "density"`. Takes precedence over
  `bins` when both are set.

- flip:

  A logical value. If `TRUE`, the x and y axes are swapped via
  `coord_flip()`. Dimension calculation accounts for the flip.

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

- add_bars:

  A logical value. If `TRUE`, a data-distribution rug is drawn along the
  y = 0 axis using `geom_linerange()`. Each group's bars are vertically
  offset to avoid overlap.

- bar_height:

  A numeric value specifying the height (in data units, relative to the
  maximum y) of the rug bars added by `add_bars`. The actual pixel
  height scales with `max_y`. Default: `0.025`.

- bar_alpha:

  A numeric value in `[0, 1]` for the transparency of the rug bars.
  Default: `1`.

- bar_width:

  A numeric value passed as the `linewidth` aesthetic of
  `geom_linerange()`. Controls the thickness of each rug tick. Default:
  `0.1`.

- position:

  A character string specifying the position adjustment for the bars or
  density curves. Default: `"identity"`, which shows the actual count /
  density per group (unlike `ggplot2`'s default `"stack"`). Other
  options: `"stack"`, `"dodge"`, `"fill"`.

- use_trend:

  A logical value. If `TRUE`, the histogram bars are replaced entirely
  by a trend line (points + connecting line). Only applies when
  `type = "histogram"`.

- add_trend:

  A logical value. If `TRUE`, a trend line is overlaid on top of the
  histogram bars. Only applies when `type = "histogram"`.

- trend_alpha:

  A numeric value in `[0, 1]` controlling the transparency of the trend
  points and line. Default: `1`.

- trend_linewidth:

  A numeric value for the thickness of the trend line. Default: `0.8`.

- trend_pt_size:

  A numeric value for the size of the trend points. Default: `1.5`.

- trend_skip_zero:

  A logical value. If `TRUE`, bins with zero count are set to `NA`
  before the trend line is computed, and
  [`zoo::na.approx()`](https://rdrr.io/pkg/zoo/man/na.approx.html) is
  used to interpolate across the gaps — producing a continuous curve
  even when some bins are empty. Requires `ytrans` to be correctly
  specified. Only applies when `type = "histogram"` and `use_trend` or
  `add_trend` is active.

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

- theme:

  A character string or a theme class (i.e. ggplot2::theme_classic)
  specifying the theme to use. Default is "theme_this".

- theme_args:

  A list of arguments to pass to the theme function.

- aspect.ratio:

  A numeric value specifying the aspect ratio of the plot.

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

- expand:

  The values to expand the x and y axes. It is like CSS padding. When a
  single value is provided, it is used for both axes on both sides. When
  two values are provided, the first value is used for the top/bottom
  side and the second value is used for the left/right side. When three
  values are provided, the first value is used for the top side, the
  second value is used for the left/right side, and the third value is
  used for the bottom side. When four values are provided, the values
  are used for the top, right, bottom, and left sides, respectively. You
  can also use a named vector to specify the values for each side. When
  the axis is discrete, the values will be applied as 'add' to the
  'expansion' function. When the axis is continuous, the values will be
  applied as 'mult' to the 'expansion' function. See also
  <https://ggplot2.tidyverse.org/reference/expansion.html>

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

- legend.position:

  A character string specifying the position of the legend. if
  `waiver()`, for single groups, the legend will be "none", otherwise
  "right".

- legend.direction:

  A character string specifying the direction of the legend.

- ...:

  Additional arguments.

## Architecture

**DensityHistoPlotAtomic** executes the following steps:

1.  **ggplot dispatch** — selects `gglogger::ggplot` or
    [`ggplot2::ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)
    based on `getOption("plotthis.gglogger.enabled")`.

2.  **Type check** — `match.arg(type, c("density", "histogram"))`.

3.  **Expansion normalization** —
    [`norm_expansion()`](https://pwwang.github.io/plotthis/reference/norm_expansion.md)
    converts the `expand` vector into CSS-padding-style x/y components.

4.  **Column resolution** —
    [`check_columns()`](https://pwwang.github.io/plotthis/reference/check_columns.md)
    validates `x`, `group_by` (force_factor, allow_multi, concat_multi),
    and `facet_by`.

5.  **Default group** — when `group_by` is `NULL`, a synthetic `.group`
    factor with a single empty-string level is created so the
    colour-mapping pipeline runs uniformly.

6.  **Histogram bin default** — if `type = "histogram"` and neither
    `bins` nor `binwidth` is set, `bins = 30` with a message.

7.  **Add-bars pre-calculation** — when `add_bars = TRUE`:

    - For density: max y = `max(density(x)$y) * 1.5`.

    - For histogram: max y = max bin count from `cut(x, s)`.

    - Computes `.ymin` and `.ymax` per row, offset by
      `bar_height * max_y` for each group so rugs stack without
      colliding.

8.  **NA / empty handling** —
    [`process_keep_na_empty()`](https://pwwang.github.io/plotthis/reference/process_keep_na_empty.md)
    filters data and `keep_empty` values are extracted for group and
    facet dimensions.

9.  **Palette resolution** —
    [`palette_this()`](https://pwwang.github.io/plotthis/reference/palette_this.md)
    maps group levels to colours.

10. **Base ggplot + scales** — initialises
    `ggplot(data, aes(x, fill, color))`, then adds `scale_fill_manual()`
    / `scale_color_manual()`. When `keep_empty_group` is `TRUE`,
    `drop = FALSE`, `breaks`, and `limits` are set to preserve empty
    factor levels.

11. **Geometry layer**:

    - *Histogram (no trend)*:
      `geom_histogram(alpha, bins, binwidth, position)`.

    - *Histogram (use_trend / add_trend)*: adds
      `stat_bin(geom = "point")` for trend points +
      `stat_bin(geom = "line")` for the trend curve. When
      `trend_skip_zero = TRUE`, an `after_stat()` expression sets zero
      counts to `NA`, transforms y, applies
      [`zoo::na.approx()`](https://rdrr.io/pkg/zoo/man/na.approx.html)
      per `..group..`, and inverts — producing a continuous trend that
      interpolates over empty bins.

    - *Density*: `geom_density(alpha, position)`.

12. **Add-bars geometry** — if `add_bars = TRUE`, `geom_linerange()`
    draws vertical ticks at the pre-computed `.ymin` / `.ymax`
    positions.

13. **Scales, theme, labels** — x/y continuous scales with transforms,
    theme applied via `do_call(theme, theme_args)`, and axis / title
    labels (default y-lab: "Count" for histogram, "Density" for
    density).

14. **Flip** — optional `coord_flip()`.

15. **Dimension calculation** —
    `calculate_plot_dimensions(base_height = 3.5, aspect.ratio, legend, flip)`
    sets `height` / `width` attributes.

16. **Faceting** —
    [`facet_plot()`](https://pwwang.github.io/plotthis/reference/facet_plot.md)
    applies `facet_grid` / `facet_wrap`.
