# Atomic Correlation Plot

Core implementation for drawing a scatter plot of two variables with a
linear regression line, optional correlation statistics, and point
highlighting. This is the workhorse behind the exported
[`CorPlot`](https://pwwang.github.io/plotthis/reference/CorPlot.md) — it
takes a **single** data frame (no `split_by` support) and returns a
`ggplot` object with faceting applied.

The function supports **group-based colouring** (`group_by`), point
highlighting by expression or rowname, multiple annotation items
(regression equation, R-squared, p-value, Spearman/Pearson/Kendall
correlation, N), raster rendering for large datasets, configurable
regression line style, and faceting.

## Usage

``` r
CorPlotAtomic(
  data,
  x,
  y,
  group_by = NULL,
  group_by_sep = "_",
  group_name = NULL,
  pt_size = 2,
  pt_shape = 16,
  alpha = 1,
  raster = FALSE,
  raster_dpi = c(512, 512),
  highlight = NULL,
  highlight_color = "black",
  highlight_size = 1,
  highlight_alpha = 1,
  highlight_stroke = 0.8,
  anno_items = c("eq", "r2", "p"),
  anno_size = 3,
  anno_fg = "black",
  anno_bg = "white",
  anno_bg_r = 0.1,
  anno_position = c("topleft", "topright", "bottomleft", "bottomright", "tl", "tr", "bl",
    "br"),
  add_smooth = TRUE,
  smooth_color = "red2",
  smooth_width = 1.5,
  smooth_se = FALSE,
  theme = "theme_this",
  theme_args = list(),
  palette = ifelse(is.null(group_by), "Spectral", "Paired"),
  palcolor = NULL,
  palreverse = FALSE,
  title = NULL,
  subtitle = NULL,
  xlab = NULL,
  ylab = NULL,
  facet_by = NULL,
  facet_scales = "fixed",
  facet_ncol = NULL,
  facet_nrow = NULL,
  facet_byrow = TRUE,
  aspect.ratio = 1,
  legend.position = waiver(),
  legend.direction = "vertical",
  seed = 8525,
  ...
)
```

## Arguments

- data:

  A data frame.

- x:

  A character string specifying the column name for the x-axis. Must be
  numeric.

- y:

  A character string specifying the column name for the y-axis. Must be
  numeric.

- group_by:

  A character vector of column names to colour the points by. Each
  unique combination becomes a separate group in the legend. Multiple
  columns are concatenated with `group_by_sep`. When `NULL`, all points
  are a single colour and the legend is hidden.

- group_by_sep:

  A character string to separate concatenated `group_by` columns.
  Default `"_"`.

- group_name:

  A character string used as the colour legend title. When `NULL`, the
  `group_by` column name is used.

- pt_size:

  A numeric value specifying the size of the points. Default: `2`.

- pt_shape:

  A numeric value specifying the shape of the points (see
  [`geom_point`](https://ggplot2.tidyverse.org/reference/geom_point.html)).
  Default: `16` (filled circle).

- alpha:

  A numeric value specifying the transparency of the plot.

- raster:

  A logical value. When `TRUE`, uses
  [`scattermore::geom_scattermore()`](https://rdrr.io/pkg/scattermore/man/geom_scattermore.html)
  for efficient rendering of large datasets. Default: `FALSE`.

- raster_dpi:

  An integer vector of length 1 or 2 specifying the raster resolution in
  (width, height) pixels. When a single value is provided, it is
  recycled. Default: `c(512, 512)`.

- highlight:

  Specifies which points to emphasise. Can be:

  - `TRUE` — highlight all points.

  - A character expression (e.g. `'Species == "setosa"'`) — evaluated
    via
    [`dplyr::filter`](https://dplyr.tidyverse.org/reference/filter.html).

  - A character vector — matched against rownames of the data.

  - A numeric vector — treated as row indices.

  Default: `NULL` (no highlighting).

- highlight_color:

  A character string specifying the colour of the highlighted point
  borders. Default: `"black"`.

- highlight_size:

  A numeric value specifying the size of the highlighted points (the
  inner fill). Default: `1`.

- highlight_alpha:

  A numeric value specifying the alpha transparency of the highlighted
  points. Default: `1`.

- highlight_stroke:

  A numeric value specifying the stroke width of the highlighted point
  borders. The outer layer size is `highlight_size + highlight_stroke`.
  Default: `0.8`.

- anno_items:

  A character vector specifying which statistics to display as text
  annotation. Available items: `"eq"` (regression equation), `"r2"`
  (R-squared), `"p"` (p-value), `"spearman"`, `"pearson"`, `"kendall"`,
  `"n"` (observation count). Default: `c("eq", "r2", "p")`.

- anno_size:

  A numeric value specifying the font size of the annotation text
  (scaled by `base_size / 12`). Default: `3`.

- anno_fg:

  A character string specifying the colour of the annotation text.
  Default: `"black"`.

- anno_bg:

  A character string specifying the background colour of the annotation
  text boxes. Default: `"white"`.

- anno_bg_r:

  A numeric value specifying the corner radius of the annotation text
  background boxes. Default: `0.1`.

- anno_position:

  A character string specifying the corner position of the annotation
  text. One of `"topleft"` (alias `"tl"`), `"topright"` (`"tr"`),
  `"bottomleft"` (`"bl"`), `"bottomright"` (`"br"`).

- add_smooth:

  A logical value. When `TRUE` (default), a linear regression line
  (`geom_smooth(method = "lm")`) is added.

- smooth_color:

  A character string specifying the colour of the regression line.
  Default: `"red2"`.

- smooth_width:

  A numeric value specifying the linewidth of the regression line.
  Default: `1.5`.

- smooth_se:

  A logical value. When `TRUE`, a standard error band is drawn around
  the regression line. Default: `FALSE`.

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

- seed:

  The random seed to use. Default is 8525.

- ...:

  Additional arguments.

## Value

A `ggplot` object with `height` and `width` attributes (in inches)
attached.

## Architecture

1.  **ggplot dispatch** — selects `gglogger::ggplot` or
    [`ggplot2::ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)
    based on `getOption("plotthis.gglogger.enabled")`.

2.  **Parameter normalisation** —
    [`match.arg()`](https://rdrr.io/r/base/match.arg.html) resolves
    `anno_position` abbreviations (`"tl"`, `"tr"`, `"bl"`, `"br"`) to
    their full forms. `raster_dpi` is expanded to length 2 when given as
    a scalar.

3.  **Column resolution** — `x`, `y`, and `group_by` are validated via
    [`check_columns`](https://pwwang.github.io/plotthis/reference/check_columns.md).
    Multi-column `group_by` is concatenated with `group_by_sep`.

4.  **Grouping fallback** — when `group_by = NULL`, a dummy `.group`
    column is created and the legend is suppressed
    (`legend.position = "none"`).

5.  **Annotation data calculation** — the data is grouped by `facet_by`
    and a linear model (`lm(y ~ x)`) is fitted per group. Each requested
    `anno_items` is computed:

    - `"eq"` — regression equation `y = a + bx`.

    - `"r2"` — R-squared of the model.

    - `"p"` — p-value of the x coefficient.

    - `"spearman"` — Spearman's rho.

    - `"pearson"` — Pearson's r.

    - `"kendall"` — Kendall's tau.

    - `"n"` — number of observations.

    The results are stored in an `annodata` data frame for
    `geom_text_repel`.

6.  **Highlight parsing** — the `highlight` argument is resolved into a
    `.highlight` logical column:

    - `TRUE` — highlights all points.

    - A character expression — evaluated via
      [`dplyr::filter`](https://dplyr.tidyverse.org/reference/filter.html)
      to select rows.

    - A character vector — matched against rownames of the data.

    - A numeric vector — treated as row indices.

7.  **Point rendering** — two branches:

    - **Raster mode** (`raster = TRUE`) — uses
      [`scattermore::geom_scattermore()`](https://rdrr.io/pkg/scattermore/man/geom_scattermore.html)
      for efficient rendering of large datasets. Highlighted points are
      drawn in two layers (stroke + fill).

    - **Vector mode** (`raster = FALSE`, default) — uses
      [`ggplot2::geom_point()`](https://ggplot2.tidyverse.org/reference/geom_point.html)
      with configurable size, shape, and alpha. Highlighted points get
      an outer stroke via a second point layer.

8.  **Regression line** — `geom_smooth(method = "lm")` draws the linear
    regression line with optional standard error band (`smooth_se`).

9.  **Annotation text** —
    [`ggrepel::geom_text_repel()`](https://ggrepel.slowkow.com/reference/geom_text_repel.html)
    places the computed annotations at the specified `anno_position`
    corner, with background styling.

10. **Colour scale** — `scale_color_manual()` maps group levels to
    colours via
    [`palette_this()`](https://pwwang.github.io/plotthis/reference/palette_this.md).

11. **Labels and theme** — `labs()` sets titles and axis labels. The
    theme is applied via
    [`do_call()`](https://pwwang.github.io/plotthis/reference/do_call.md),
    with `aspect.ratio`, `legend.position`, and `legend.direction`
    enforced.

12. **Dimension calculation** —
    [`calculate_plot_dimensions()`](https://pwwang.github.io/plotthis/reference/calculate_plot_dimensions.md)
    computes `height` and `width` attributes from `base_height = 4.5`,
    `aspect.ratio`, legend metrics, and the number of group levels.

13. **Faceting** —
    [`facet_plot()`](https://pwwang.github.io/plotthis/reference/facet_plot.md)
    applies `facet_wrap` / `facet_grid` if `facet_by` is provided.
