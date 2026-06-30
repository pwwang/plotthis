# Atomic rarefaction / extrapolation plot (internal)

Core implementation for drawing a single rarefaction (or extrapolation)
curve from biodiversity data. This is the workhorse behind the exported
[`RarefactionPlot`](https://pwwang.github.io/plotthis/reference/RarefactionPlot.md)
— it takes a **single** fortifed data frame (produced by
[`fortify()`](https://ggplot2.tidyverse.org/reference/fortify.html) on
an `iNEXT` object) and returns a `ggplot` object.

The function renders three types of curves:

1.  **Sample-size-based** (`type = 1`) — species diversity as a function
    of sample size (number of individuals or sampling units), with
    rarefaction (interpolation) and extrapolation segments.

2.  **Sample completeness** (`type = 2`) — sample coverage as a function
    of sample size.

3.  **Coverage-based** (`type = 3`) — species diversity as a function of
    sample coverage.

Observed data points are marked with `geom_point()` (shape per group),
the rarefaction / extrapolation lines are drawn with `geom_line()` using
a solid/dashed linetype to distinguish the two phases, and confidence
intervals are rendered as semi-transparent ribbons when `se = TRUE` and
the fortifed data contains `y.lwr` / `y.upr` columns.

## Usage

``` r
RarefactionPlotAtomic(
  data,
  type = 1,
  se = TRUE,
  group_by = "group",
  group_name = NULL,
  pt_size = 3,
  line_width = 1,
  theme = "theme_this",
  theme_args = list(),
  palette = "Spectral",
  palcolor = NULL,
  palreverse = FALSE,
  alpha = 0.2,
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

  A data frame produced by
  [`fortify()`](https://ggplot2.tidyverse.org/reference/fortify.html) on
  an `iNEXT` object. Expected to contain columns `x` (sample size or
  coverage), `y` (diversity or coverage estimate), `Method`
  (`"Observed"`, `"Rarefaction"`, `"Extrapolation"`), `y.lwr` / `y.upr`
  (confidence bounds, optional), `datatype` (`"abundance"` or
  `"incidence"`), `group` (assemblage name, renamed from `Assemblage`),
  and `q` (diversity order, renamed from `Order.q`).

- type:

  An integer specifying the curve type: `1` for sample-size-based
  rarefaction/extrapolation, `2` for sample completeness, or `3` for
  coverage-based rarefaction/extrapolation. A vector of types can be
  passed and the data will be fortifed for all of them; faceting or
  splitting then separates the panels. Default: `1`.

- se:

  A logical value indicating whether to display confidence intervals as
  semi-transparent ribbons around the estimated curve. When `NULL` (the
  default), it resolves to `TRUE` if the fortifed data contains `y.lwr`
  and `y.upr` columns, and `FALSE` otherwise.

- group_by:

  A character vector specifying how to group the data for colouring the
  lines. Must be one or both of `"q"` (diversity order) and `"group"`
  (assemblage/site). Multiple values are concatenated with
  `group_by_sep`. When `NULL`, a dummy `".group"` column is created and
  the legend is hidden. Default: `"group"`.

- group_name:

  A character string used as the title for the colour (and shape)
  legend. When `NULL` (the default), the value of `group_by` is used.

- pt_size:

  A numeric value specifying the size of the observed-data points.
  Default: `3`.

- line_width:

  A numeric value specifying the width of the rarefaction /
  extrapolation lines. Default: `1`.

- theme:

  A character string or a theme class (i.e. ggplot2::theme_classic)
  specifying the theme to use. Default is "theme_this".

- theme_args:

  A list of arguments to pass to the theme function.

- palette:

  A character string specifying the colour palette to use. A named list
  or vector can be used to specify palettes for different `split_by`
  levels in the exported
  [`RarefactionPlot`](https://pwwang.github.io/plotthis/reference/RarefactionPlot.md).
  Default: `"Spectral"`.

- palcolor:

  A character string specifying the color to use in the palette. A named
  list can be used to specify the colors for different `split_by`
  values. If some values are missing, the values from the palette will
  be used (palcolor will be NULL for those values).

- palreverse:

  A logical value indicating whether to reverse the palette. Default is
  FALSE.

- alpha:

  A numeric value between 0 and 1 specifying the transparency of the
  confidence-interval ribbon fill. Default: `0.2`.

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

  Additional arguments (currently unused in the atomic function;
  accepted for forward compatibility with the exported wrapper).

## Value

A `ggplot` object with `height` and `width` attributes (in inches)
attached.

## Architecture

1.  **ggplot dispatch** — selects `gglogger::ggplot` or
    [`ggplot2::ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)
    based on `getOption("plotthis.gglogger.enabled")`.

2.  **Axis labels** — set according to the combination of `type` and the
    data's `datatype` attribute (`"abundance"` vs. `"incidence"`):

    - `type = 2`: x = "Number of individuals" or "Number of sampling
      units", y = "Sample coverage".

    - `type = 3 || 4`: x = "Sample coverage", y = "Species diversity".

    - `type = 1` (abundance): x = "Number of individuals", y = "Species
      diversity".

    - `type = 1` (incidence): x = "Number of sampling units", y =
      "Species diversity".

3.  **Group name fallback** — if `group_name` is `NULL`, it is set to
    the value of `group_by` (i.e. `"q"` or `"group"`).

4.  **Base ggplot** — initialises
    `ggplot(data, aes(x, y, color = group_by))`.

5.  **Observed data points** — `geom_point()` on rows where
    `Method == "Observed"`, with shape mapped to the group variable.

6.  **Colour scale** — `scale_color_manual()` using
    [`palette_this()`](https://pwwang.github.io/plotthis/reference/palette_this.md).
    The legend is suppressed when `group_by` is the dummy `".group"`.

7.  **Shape scale** — `scale_shape_discrete()` with the same
    legend-suppression logic.

8.  **Rarefaction / extrapolation lines** — `geom_line()` with linetype
    mapped to the `lty` column (`"Rarefaction"` vs. `"Extrapolation"`).

9.  **Linetype scale** — `scale_linetype_manual()` with `solid` and
    `dashed` values, legend key width set to 1 cm.

10. **Theme and labels** — `do_call(theme, theme_args)`,
    `panel.grid.major` (grey80 dashed), aspect ratio, legend position /
    direction, and title / subtitle.

11. **Confidence ribbon** — when `se = TRUE`, a
    `geom_ribbon(aes(ymin = y.lwr, ymax = y.upr, fill = group_by))` is
    added with the same per-group palette at transparency `alpha`.

12. **Dimension calculation** —
    [`calculate_plot_dimensions()`](https://pwwang.github.io/plotthis/reference/calculate_plot_dimensions.md)
    computes `height` / `width` attributes from `base_height = 4.5`,
    `aspect.ratio`, and legend metrics.

13. **Faceting** —
    [`facet_plot()`](https://pwwang.github.io/plotthis/reference/facet_plot.md)
    wraps the plot with `facet_wrap` / `facet_grid` if `facet_by` is
    provided.
