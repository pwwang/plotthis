# QQ/PP plot

Produces a quantile-quantile (QQ) plot or probability-probability (PP)
plot to compare the empirical distribution of a numeric variable against
a theoretical distribution (default: standard normal). The function
delegates to the qqplotr package for the underlying statistics and
rendering.

Key features:

- **QQ and PP modes** – switch between quantile-quantile and
  probability-probability displays via `type`.

- **Confidence bands** – overlay one or more confidence bands
  (pointwise, KS, Tukey simultaneous, or bootstrap) with custom fill
  colours and alpha.

- **Reference line** – a diagonal reference line (QQ) or diagonal
  probability line (PP) for comparison.

- **Distribution fitting** – compare against any distribution supported
  by qqplotr (normal, exponential, uniform, etc.) by passing
  `distribution` and `dparams` inside the `band`, `line`, and `point`
  lists.

- **Detrending** – enable `detrend = TRUE` inside the argument lists to
  remove the reference line and visualise only deviations (flat PP plot
  centred at zero).

- **Splitting** – use `split_by` to produce separate QQ/PP plots for
  different groups, combined into a single layout.

## Usage

``` r
QQPlot(
  data,
  val,
  val_trans = NULL,
  type = c("qq", "pp"),
  split_by = NULL,
  split_by_sep = "_",
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
  xlim = NULL,
  ylim = NULL,
  xlab = ifelse(type == "qq", "Theoretical Quantiles", "Probability Points"),
  ylab = ifelse(type == "qq", "Sample Quantiles", "Cumulative Probability"),
  combine = TRUE,
  nrow = NULL,
  ncol = NULL,
  byrow = TRUE,
  seed = 8525,
  axes = NULL,
  axis_titles = axes,
  guides = NULL,
  design = NULL,
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

- split_by:

  The column(s) to split data by and plot separately.

- split_by_sep:

  The separator for multiple split_by columns. See `split_by`

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

- combine:

  Whether to combine the plots into one when facet is FALSE. Default is
  TRUE.

- nrow:

  A numeric value specifying the number of rows in the facet.

- ncol:

  A numeric value specifying the number of columns in the facet.

- byrow:

  A logical value indicating whether to fill the plots by row.

- seed:

  The random seed to use. Default is 8525.

- axes:

  A string specifying how axes should be treated. Passed to
  [`patchwork::wrap_plots()`](https://patchwork.data-imaginist.com/reference/wrap_plots.html).
  Only relevant when `split_by` is used and `combine` is TRUE. Options
  are:

  - 'keep' will retain all axes in individual plots.

  - 'collect' will remove duplicated axes when placed in the same run of
    rows or columns of the layout.

  - 'collect_x' and 'collect_y' will remove duplicated x-axes in the
    columns or duplicated y-axes in the rows respectively.

- axis_titles:

  A string specifying how axis titltes should be treated. Passed to
  [`patchwork::wrap_plots()`](https://patchwork.data-imaginist.com/reference/wrap_plots.html).
  Only relevant when `split_by` is used and `combine` is TRUE. Options
  are:

  - 'keep' will retain all axis titles in individual plots.

  - 'collect' will remove duplicated titles in one direction and merge
    titles in the opposite direction.

  - 'collect_x' and 'collect_y' control this for x-axis titles and
    y-axis titles respectively.

- guides:

  A string specifying how guides should be treated in the layout. Passed
  to
  [`patchwork::wrap_plots()`](https://patchwork.data-imaginist.com/reference/wrap_plots.html).
  Only relevant when `split_by` is used and `combine` is TRUE. Options
  are:

  - 'collect' will collect guides below to the given nesting level,
    removing duplicates.

  - 'keep' will stop collection at this level and let guides be placed
    alongside their plot.

  - 'auto' will allow guides to be collected if a upper level tries, but
    place them alongside the plot if not.

- design:

  Specification of the location of areas in the layout, passed to
  [`patchwork::wrap_plots()`](https://patchwork.data-imaginist.com/reference/wrap_plots.html).
  Only relevant when `split_by` is used and `combine` is TRUE. When
  specified, `nrow`, `ncol`, and `byrow` are ignored. See
  [`patchwork::wrap_plots()`](https://patchwork.data-imaginist.com/reference/wrap_plots.html)
  for more details.

- ...:

  Additional arguments.

## Value

A `ggplot` object (single plot), a `patchwork` object (combined split
plots), or a named list of `ggplot` objects (when `combine = FALSE`),
each with `height` and `width` attributes in inches.

## split_by Workflow

When `split_by` is provided:

1.  **Common arg validation** –
    [`validate_common_args()`](https://pwwang.github.io/plotthis/reference/validate_common_args.md)
    checks the `seed` and `facet_by` constraints.

2.  **Theme processing** –
    [`process_theme()`](https://pwwang.github.io/plotthis/reference/process_theme.md)
    resolves the `theme` string or function.

3.  **split_by column resolution** –
    [`check_columns()`](https://pwwang.github.io/plotthis/reference/check_columns.md)
    validates the `split_by` column(s) with `force_factor = TRUE`.
    Multiple columns are concatenated with `split_by_sep`.

4.  **Data splitting** – the data frame is split by `split_by` levels
    (droplevels applied, level order preserved). If `split_by` is
    `NULL`, the data is wrapped in a single-element list with name
    `"..."`.

5.  **Per-split parameter resolution** –
    [`check_palette()`](https://pwwang.github.io/plotthis/reference/check_palette.md),
    [`check_palcolor()`](https://pwwang.github.io/plotthis/reference/check_palcolor.md),
    and
    [`check_legend()`](https://pwwang.github.io/plotthis/reference/check_legend.md)
    resolve per-split `palette`, `palcolor`, `legend.position`, and
    `legend.direction`.

6.  **Dispatch per split** –
    [`QQPlotAtomic()`](https://pwwang.github.io/plotthis/reference/QQPlotAtomic.md)
    is called for each split level. If `title` is a function, it
    receives the split level name and generates a dynamic title;
    otherwise the level name is used as the default title.

7.  **Combination** – results are combined via
    [`combine_plots()`](https://pwwang.github.io/plotthis/reference/combine_plots.md)
    (when `combine = TRUE`) or returned as a named list (when
    `combine = FALSE`).

## Examples

``` r
set.seed(8525)
data <- data.frame(norm = rnorm(100))

# Basic QQ plot with default confidence band
QQPlot(data, val = "norm", band = TRUE)


# Multiple confidence bands with custom fill labels
QQPlot(data, val = "norm", band = list(
    list(bandType = "ks", mapping = ggplot2::aes(fill = "KS"), alpha = 0.3),
    list(bandType = "ts", mapping = ggplot2::aes(fill = "TS")),
    list(bandType = "pointwise", mapping = ggplot2::aes(fill = "Normal")),
    list(bandType = "boot", mapping = ggplot2::aes(fill = "Bootstrap"))
), band_alpha = 0.6)


# Compare against exponential distribution
data(airquality, package = "datasets")
di <- "exp"
dp <- list(rate = 2)
QQPlot(airquality, val = "Ozone",
    band = list(distribution = di, dparams = dp),
    line = list(distribution = di, dparams = dp),
    point = list(distribution = di, dparams = dp)
)


# Detrended QQ plot: deviations from the reference line
de <- TRUE
QQPlot(airquality, val = "Ozone",
    band = list(distribution = di, dparams = dp, detrend = de),
    line = list(distribution = di, dparams = dp, detrend = de),
    point = list(distribution = di, dparams = dp, detrend = de)
)


# PP plot (probability-probability)
QQPlot(data, val = "norm", type = "pp", band = TRUE)


# PP plot with shifted/scaled normal distribution
dp <- list(mean = 2, sd = 2)
QQPlot(data, val = "norm", type = "pp",
    band = list(dparams = dp),
    point = list(dparams = dp))


# PP plot with custom intercept/slope line
QQPlot(data, val = "norm", type = "pp", band = TRUE,
    line = list(ab = c(.2, .5)))


# Detrended PP plot with axis limits
di <- "exp"
dp <- list(rate = .022)
de <- TRUE
QQPlot(airquality, val = "Ozone", type = "pp",
    band = list(distribution = di, detrend = de, dparams = dp),
    line = list(detrend = de),
    point = list(distribution = di, detrend = de, dparams = dp),
    ylim = c(-.5, .5)
)
```
