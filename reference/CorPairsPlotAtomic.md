# Atomic Correlation Pairs Plot

Core implementation for drawing a correlation pairs (scatterplot matrix)
grid. This is the workhorse behind the exported
[`CorPairsPlot`](https://pwwang.github.io/plotthis/reference/CorPairsPlot.md)
— it takes a **single** data frame (no `split_by` support) and returns a
`patchwork` object.

The grid arranges all pairwise scatter plots of selected columns. The
upper or lower triangle displays correlation tiles (fill) while the
opposite triangle displays scatter plots with regression lines. The
diagonal can show density plots, violin plots, histograms, box plots, or
a simple diagonal line.

The function supports **four layout orientations** (`layout`), **three
correlation methods** (`cor_method`), configurable diagonal plots using
other plotthis functions, and custom correlation tile formatting.

## Usage

``` r
CorPairsPlotAtomic(
  data,
  columns = NULL,
  group_by = NULL,
  group_by_sep = "_",
  group_name = NULL,
  diag_type = NULL,
  diag_args = list(),
  layout = c(".\\", "\\.", "/.", "./"),
  cor_method = c("pearson", "spearman", "kendall"),
  cor_palette = "RdBu",
  cor_palcolor = NULL,
  cor_size = 3,
  cor_format = "corr: {round(corr, 2)}",
  cor_fg = "black",
  cor_bg = "white",
  cor_bg_r = 0.1,
  theme = "theme_this",
  theme_args = list(),
  palette = ifelse(is.null(group_by), "Spectral", "Paired"),
  palcolor = NULL,
  palreverse = FALSE,
  title = NULL,
  subtitle = NULL,
  facet_by = NULL,
  legend.position = "right",
  legend.direction = "vertical",
  seed = 8525,
  ...
)
```

## Arguments

- data:

  A data frame.

- columns:

  A character vector of column names to include in the pairs plot. When
  `NULL` (default), all columns except `group_by` are used. At least two
  columns are required.

- group_by:

  A character vector of column names to colour the scatter points by.
  Each unique combination becomes a separate group. Required for
  `diag_type = "violin"` and `diag_type = "box"`. Multiple columns are
  concatenated with `group_by_sep`.

- group_by_sep:

  A character string to separate concatenated `group_by` columns.
  Default `"_"`.

- group_name:

  A character string used as the colour legend title in the scatter
  plots. When `NULL`, the `group_by` column name is used.

- diag_type:

  A character string specifying the plot type for diagonal cells. One of
  `"density"`, `"violin"`, `"histogram"`, `"box"`, or `"none"` (diagonal
  line). Default: `"density"` (no `group_by`) or `"violin"` (with
  `group_by`).

- diag_args:

  A named list of additional arguments passed to the diagonal plot
  function
  ([`DensityPlot`](https://pwwang.github.io/plotthis/reference/densityhistoplot.md),
  [`ViolinPlot`](https://pwwang.github.io/plotthis/reference/boxviolinplot.md),
  [`Histogram`](https://pwwang.github.io/plotthis/reference/densityhistoplot.md),
  or
  [`BoxPlot`](https://pwwang.github.io/plotthis/reference/boxviolinplot.md)).
  Default: [`list()`](https://rdrr.io/r/base/list.html).

- layout:

  A character string specifying the layout orientation. One of the
  following codes (dot = scatter, backslash/slash = diagonal): `.\`,
  `\\.`, `/.`, `./`. Default: `.\`.

- cor_method:

  A character string specifying the correlation method for the fill
  tiles. One of `"pearson"`, `"spearman"`, `"kendall"`. Default:
  `"pearson"`.

- cor_palette:

  A character string specifying the colour palette for the correlation
  fill tiles. Default: `"RdBu"`.

- cor_palcolor:

  A character vector of custom colours used to create the correlation
  tile palette. When `NULL`, the palette's default colours are used.

- cor_size:

  A numeric value specifying the font size of the correlation text in
  the fill tiles. Default: `3`.

- cor_format:

  A character string specifying a glue template for formatting the
  correlation text. The template is evaluated by
  [`glue::glue()`](https://glue.tidyverse.org/reference/glue.html) with
  access to `corr` (the correlation value), `x`, and `y` (the column
  names). Default: `"corr: \{round(corr, 2)\}"`.

- cor_fg:

  A character string specifying the colour of the correlation text.
  Default: `"black"`.

- cor_bg:

  A character string specifying the background colour of the correlation
  text boxes. Default: `"white"`.

- cor_bg_r:

  A numeric value specifying the corner radius of the correlation text
  background boxes. Default: `0.1`.

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

- facet_by:

  A character string specifying the column name of the data frame to
  facet the plot. Otherwise, the data will be split by `split_by` and
  generate multiple plots and combine them into one using
  [`patchwork::wrap_plots`](https://patchwork.data-imaginist.com/reference/wrap_plots.html)

- legend.position:

  A character string specifying the position of the legend. if
  `waiver()`, for single groups, the legend will be "none", otherwise
  "right".

- legend.direction:

  A character string specifying the direction of the legend.

- seed:

  The random seed to use. Default is 8525.

- ...:

  Additional arguments passed to the underlying
  [`CorPlot`](https://pwwang.github.io/plotthis/reference/CorPlot.md)
  for the scatter plot cells.

## Value

A `patchwork` object with `height` and `width` attributes (in inches)
attached.

## Details

`theme` and `theme_args` are supported and passed to each individual
cell plot.

## Architecture

1.  **ggplot dispatch** — selects `gglogger::ggplot` or
    [`ggplot2::ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)
    based on `getOption("plotthis.gglogger.enabled")`.

2.  **facet_by guard** — raises an error if `facet_by` is provided (not
    supported in pairs plots; use `split_by` instead).

3.  **Column resolution** — when `columns` is `NULL`, all columns
    (except `group_by`) are used. Otherwise, `columns` are validated via
    [`check_columns()`](https://pwwang.github.io/plotthis/reference/check_columns.md).
    At least two columns are required.

4.  **Default diag_type** — when `diag_type` is `NULL`, it defaults to
    `"density"` (no `group_by`) or `"violin"` (with `group_by`).

5.  **Layout determination** — `get_plot_info()` is called for each cell
    `(i, j)` to determine:

    - **Type**: `layout` (diagonal line), `"cor"` (scatter plot with
      regression), `"fill"` (correlation tile).

    - **Axis/label positions**: which sides of the cell should show
      x-axis, y-axis, x-label, and y-label, based on the cell's location
      in the matrix and the chosen layout.

6.  **Cell rendering** — `get_plot()` draws each cell based on its
    `info$type`:

    - **Diagonal, `diag_type = "none"`** — draws an X or backslash
      diagonal line via `geom_line()`.

    - **Diagonal, `"density"`** — delegates to
      [`DensityPlot()`](https://pwwang.github.io/plotthis/reference/densityhistoplot.md).

    - **Diagonal, `"violin"`** — delegates to
      [`ViolinPlot()`](https://pwwang.github.io/plotthis/reference/boxviolinplot.md);
      requires `group_by`.

    - **Diagonal, `"histogram"`** — delegates to
      [`Histogram()`](https://pwwang.github.io/plotthis/reference/densityhistoplot.md).

    - **Diagonal, `"box"`** — delegates to
      [`BoxPlot()`](https://pwwang.github.io/plotthis/reference/boxviolinplot.md);
      requires `group_by`.

    - **Off-diagonal, `"cor"`** — delegates to
      [`CorPlot()`](https://pwwang.github.io/plotthis/reference/CorPlot.md)
      for the scatter plot with regression line.

    - **Off-diagonal, `"fill"`** — draws a `geom_tile()` filled by the
      correlation value with `scale_fill_gradientn()` and displays the
      formatted correlation via
      [`ggrepel::geom_text_repel()`](https://ggrepel.slowkow.com/reference/geom_text_repel.html).

7.  **Axis/label positioning** — axes, axis titles, and labels are
    positioned at the margins of the full grid based on `info$xaxis`,
    `info$yaxis`, `info$xlab`, `info$ylab` using `scale_x_*`/`scale_y_*`
    with appropriate `position` arguments.

8.  **Layout assembly** — all cell plots are arranged into a matrix via
    [`patchwork::wrap_plots()`](https://patchwork.data-imaginist.com/reference/wrap_plots.html)
    with `ncol = length(columns)`. Guides are collected, and the
    title/subtitle are added via `plot_annotation()`. The result is
    wrapped in
    [`patchwork::wrap_elements()`](https://patchwork.data-imaginist.com/reference/wrap_elements.html)
    so the title displays correctly.

9.  **Dimension calculation** —
    [`calculate_plot_dimensions()`](https://pwwang.github.io/plotthis/reference/calculate_plot_dimensions.md)
    computes `height` and `width` attributes from
    `base_height = sqrt(length(columns)) * 4`, a fixed aspect ratio of
    1, and legend metrics.
