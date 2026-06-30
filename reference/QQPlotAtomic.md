# Atomic QQ/PP plot

Core implementation for drawing a single quantile-quantile (QQ) or
probability-probability (PP) plot. This is the internal workhorse
dispatched by the exported
[`QQPlot`](https://pwwang.github.io/plotthis/reference/QQPlot.md)
function – it takes a **single** data frame (no `split_by` support) and
returns a `ggplot` object. The function compares the empirical
distribution of a numeric variable against a theoretical distribution
(default: standard normal) via the qqplotr package.

Two plot types are supported via the `type` parameter:

- **QQ plot** (`type = "qq"`, the default) – plots sample quantiles
  against theoretical quantiles. Deviations from the reference line
  indicate departures from the assumed distribution (skewness, heavy
  tails, outliers).

- **PP plot** (`type = "pp"`) – plots empirical cumulative probability
  against theoretical cumulative probability. PP plots are more
  sensitive to deviations in the centre of the distribution, while QQ
  plots are more sensitive at the tails.

The function can overlay **confidence bands** (`band`) around the
reference line using several methods (pointwise confidence intervals,
Kolmogorov-Smirnov, Tukey's simultaneous intervals, or bootstrap).
Multiple bands can be combined and each receives a separate fill colour
from the `palette`.

## Usage

``` r
QQPlotAtomic(
  data,
  val,
  val_trans = NULL,
  type = c("qq", "pp"),
  band = NULL,
  line = list(),
  point = list(),
  fill_name = "Bands",
  band_alpha = 0.5,
  theme = "theme_this",
  theme_args = list(),
  palette = "Spectral",
  palcolor = NULL,
  palreverse = FALSE,
  facet_by = NULL,
  facet_scales = "fixed",
  facet_ncol = NULL,
  facet_nrow = NULL,
  facet_byrow = TRUE,
  aspect.ratio = 1,
  legend.position = waiver(),
  legend.direction = "vertical",
  title = NULL,
  subtitle = NULL,
  seed = 8525,
  xlim = NULL,
  ylim = NULL,
  xlab = ifelse(type == "qq", "Theoretical Quantiles", "Probability Points"),
  ylab = ifelse(type == "qq", "Sample Quantiles", "Cumulative Probability"),
  ...
)
```

## Arguments

- data:

  A data frame.

- val:

  A character string naming the numeric column whose distribution is
  compared against the theoretical distribution.

- val_trans:

  A transformation function applied to the `val` column before plotting.
  For example, `log` or `sqrt`. Default: `NULL` (no transformation).

- type:

  A character string specifying the plot type. Either `"qq"`
  (quantile-quantile, the default) or `"pp"` (probability-probability).
  Partial matching is supported.

- band:

  A list of arguments passed to
  [`stat_qq_band`](https://rdrr.io/pkg/qqplotr/man/stat_qq_band.html) or
  [`stat_pp_band`](https://rdrr.io/pkg/qqplotr/man/stat_pp_band.html),
  depending on `type`. Set to `TRUE` or an empty list to use default
  arguments. Set to `NULL` (the default) to suppress bands entirely. To
  add multiple bands, provide a list of lists, each containing arguments
  for one band (e.g. different `bandType` or `distribution`). Each band
  can also include a custom `mapping` aesthetic to control its fill
  colour legend entry.

- line:

  A list of arguments passed to `stat_qq_line` or
  [`stat_pp_line`](https://rdrr.io/pkg/qqplotr/man/stat_pp_line.html),
  depending on `type`. Default:
  [`list()`](https://rdrr.io/r/base/list.html) (adds a reference line
  with default arguments). Set to `NULL` to omit the line entirely.

- point:

  A list of arguments passed to
  [`stat_qq_point`](https://rdrr.io/pkg/qqplotr/man/stat_qq_point.html)
  or
  [`stat_pp_point`](https://rdrr.io/pkg/qqplotr/man/stat_pp_point.html),
  depending on `type`. Default:
  [`list()`](https://rdrr.io/r/base/list.html) (adds points with default
  arguments). Set to `NULL` to omit points (not recommended).

- fill_name:

  A character string for the fill legend title used when bands are
  present. Default: `"Bands"`.

- band_alpha:

  A numeric value in `[0, 1]` setting the transparency of all bands.
  Individual bands can override this via `alpha` inside the `band`
  argument list. Default: `0.5`.

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

- seed:

  A numeric value for the random seed used internally. Default: `8525`.
  Passed to [`set.seed`](https://rdrr.io/r/base/Random.html).

- xlim:

  A numeric vector of length 2 specifying the x-axis limits. Default:
  `NULL` (use data range).

- ylim:

  A numeric vector of length 2 specifying the y-axis limits. Default:
  `NULL` (use data range).

- xlab:

  A character string specifying the x-axis label.

- ylab:

  A character string specifying the y-axis label.

- ...:

  Additional arguments.

## Value

A `ggplot` object with `height` and `width` attributes (in inches).

## Architecture

**QQPlotAtomic** executes the following steps:

1.  **ggplot dispatch** – selects `gglogger::ggplot` or
    [`ggplot2::ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)
    based on `getOption("plotthis.gglogger.enabled")`.

2.  **Type validation** –
    [`match.arg()`](https://rdrr.io/r/base/match.arg.html) resolves
    `type` to `"qq"` or `"pp"`.

3.  **Input validation** –
    [`stopifnot()`](https://rdrr.io/r/base/stopifnot.html) checks that
    `band` is `TRUE`, a list, or `NULL`; `line` and `point` are lists or
    `NULL`; and `xlim`/`ylim` are numeric vectors of length 2 or `NULL`.

4.  **Column resolution** –
    [`check_columns()`](https://pwwang.github.io/plotthis/reference/check_columns.md)
    validates the `val` column.

5.  **Value transformation** – when `val_trans` is provided, it is
    applied to the `val` column (e.g. log-transform).

6.  **Base ggplot** – initialises
    `ggplot(data, aes(sample = !!sym(val)))`. The `sample` aesthetic is
    the standard interface for qqplotr.

7.  **Band rendering** – when `band` is not `NULL`:

    1.  Selects the band stat function:
        [`qqplotr::stat_qq_band`](https://rdrr.io/pkg/qqplotr/man/stat_qq_band.html)
        for QQ plots or
        [`qqplotr::stat_pp_band`](https://rdrr.io/pkg/qqplotr/man/stat_pp_band.html)
        for PP plots.

    2.  Converts `band = TRUE` to an empty list (default arguments).

    3.  Normalises a single band (non-list or named list) into a
        list-of-lists format.

    4.  Iterates over bands, assigning each a default fill aesthetic
        (`"Band_1"`, `"Band_2"`, ..., up to a maximum of 10). The user
        can override the fill mapping via `mapping` inside each band's
        argument list.

    5.  Sets `alpha` per band, falling back to the `band_alpha`
        parameter.

    6.  Adds each band to the plot via `do_call(band_fn, bnd)`.

8.  **Legend position resolution** – if no bands were rendered or all
    bands use default `"Band_"` names, the legend defaults to `"none"`
    (when `legend.position` is a `waiver`); otherwise defaults to
    `"right"`.

9.  **Reference line** – when `line` is not `NULL`, adds
    `qqplotr::stat_qq_line` (QQ) or
    [`qqplotr::stat_pp_line`](https://rdrr.io/pkg/qqplotr/man/stat_pp_line.html)
    (PP) via
    [`do_call()`](https://pwwang.github.io/plotthis/reference/do_call.md).

10. **Points** – when `point` is not `NULL`, adds
    [`qqplotr::stat_qq_point`](https://rdrr.io/pkg/qqplotr/man/stat_qq_point.html)
    (QQ) or
    [`qqplotr::stat_pp_point`](https://rdrr.io/pkg/qqplotr/man/stat_pp_point.html)
    (PP) via
    [`do_call()`](https://pwwang.github.io/plotthis/reference/do_call.md).

11. **Fill colour scale** – when bands are present,
    `scale_fill_manual()` is added with colours resolved via
    [`palette_this()`](https://pwwang.github.io/plotthis/reference/palette_this.md)
    using the band names, `palette`, `palcolor`, and `palreverse`. The
    legend title is set to `fill_name`.

12. **Axis limits** –
    [`ggplot2::xlim()`](https://ggplot2.tidyverse.org/reference/lims.html)
    and
    [`ggplot2::ylim()`](https://ggplot2.tidyverse.org/reference/lims.html)
    are applied if `xlim` or `ylim` are set.

13. **Labels and theme** – `labs(title, subtitle, x, y)` with fallback
    to `val` for axis labels; `do_call(theme, theme_args)`;
    [`ggplot2::theme()`](https://ggplot2.tidyverse.org/reference/theme.html)
    with `aspect.ratio`, `panel.grid.major` (grey80, dashed),
    `legend.position`, and `legend.direction`.

14. **Dimension calculation** –
    [`calculate_plot_dimensions()`](https://pwwang.github.io/plotthis/reference/calculate_plot_dimensions.md)
    with `base_height = 4.5`, `aspect.ratio`, and legend metrics (number
    of bands, band name character width).

15. **Faceting** –
    [`facet_plot()`](https://pwwang.github.io/plotthis/reference/facet_plot.md)
    applies `facet_wrap` / `facet_grid` if `facet_by` is provided.
