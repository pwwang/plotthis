# Volcano plot

Produces a volcano plot — a scatter plot that displays statistical
significance (typically -log10 adjusted p-value) on the y-axis versus
magnitude of change (log2 fold change) on the x-axis. Points are
coloured automatically by significance category (`"sig_pos_x"`,
`"sig_neg_x"`, `"insig"`) or by a user-supplied column. The most
significant features can be labelled automatically via
[`geom_text_repel()`](https://ggrepel.slowkow.com/reference/geom_text_repel.html),
and specific points can be highlighted.

The function supports **automatic labelling** of top features (by
distance to origin), **mirrored layout** via `flip_negatives`, **x-axis
trimming** to reduce the influence of extreme values, **faceting**, and
**splitting** into separate sub-plots via `split_by` with per-split
colour palette and legend control.

## Usage

``` r
VolcanoPlot(
  data,
  x,
  y,
  ytrans = "-log10",
  color_by = NULL,
  color_name = NULL,
  xlim = NULL,
  flip_negatives = FALSE,
  x_cutoff = NULL,
  y_cutoff = 0.05,
  split_by = NULL,
  split_by_sep = "_",
  label_by = NULL,
  x_cutoff_name = NULL,
  y_cutoff_name = NULL,
  x_cutoff_color = "red2",
  y_cutoff_color = "blue2",
  x_cutoff_linetype = "dashed",
  y_cutoff_linetype = "dashed",
  x_cutoff_linewidth = 0.5,
  y_cutoff_linewidth = 0.5,
  pt_size = 2,
  pt_alpha = 0.5,
  pt_shape = 21,
  pt_border_color = TRUE,
  pt_border_size = 0.5,
  nlabel = 5,
  labels = NULL,
  label_size = 3,
  label_fg = "black",
  label_bg = "white",
  label_bg_r = 0.1,
  highlight = NULL,
  highlight_color = "red",
  highlight_size = 2,
  highlight_alpha = 1,
  highlight_stroke = 0.5,
  raster = NULL,
  raster_dpi = c(512, 512),
  trim = c(0, 1),
  facet_by = NULL,
  facet_scales = "fixed",
  facet_ncol = NULL,
  facet_nrow = NULL,
  facet_byrow = TRUE,
  theme = "theme_this",
  theme_args = list(),
  palette = "Spectral",
  palcolor = NULL,
  palreverse = FALSE,
  title = NULL,
  subtitle = NULL,
  xlab = NULL,
  ylab = NULL,
  aspect.ratio = 1,
  legend.position = "right",
  legend.direction = "vertical",
  seed = 8525,
  combine = TRUE,
  nrow = NULL,
  ncol = NULL,
  byrow = TRUE,
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

- x:

  A character string specifying the column name of the data frame to
  plot for the x-axis.

- y:

  A character string specifying the column name of the data frame to
  plot for the y-axis.

- ytrans:

  A function or a function name (as a string) to transform the y-axis
  values before plotting. The transformed values are used for both the
  y-axis and cutoff comparisons. Default: `"-log10"` (converts p-values
  to a -log10 scale). Other named functions can be passed as strings,
  e.g. `"sqrt"`.

- color_by:

  A character string specifying the column name to colour the points by.
  When `NULL` (default), points are automatically categorised as
  `"sig_pos_x"`, `"sig_neg_x"`, or `"insig"` based on `x_cutoff` and
  `y_cutoff`, and the colour legend is suppressed. When a column name is
  provided, the colour mapping follows the column type — discrete
  (character/factor) uses `scale_color_manual()` with the specified
  `palette`; numeric (continuous) uses `scale_color_gradientn()`.

- color_name:

  A character string for the colour legend title when `color_by` is a
  numeric column. When `NULL` (default), the `color_by` column name is
  used.

- xlim:

  A numeric vector of length 2 to set the x-axis limits. Passed to
  [`xlim()`](https://ggplot2.tidyverse.org/reference/lims.html). When
  `NULL` (default), limits are determined automatically from the data.

- flip_negatives:

  A logical value. When `TRUE`, y-values of points with negative
  x-values are multiplied by -1, creating a mirrored volcano plot where
  both up- and down-regulated features show their significance on the
  same side of the y-axis. A horizontal line at `y = 0` and
  absolute-value axis labels are added. Default: `FALSE`.

- x_cutoff:

  A numeric value specifying the x-axis significance cutoff. Both the
  negative and positive of this value are used as vertical threshold
  lines. When `NULL` or `0`, no x-cutoff line is drawn. Default: `NULL`.

- y_cutoff:

  A numeric value specifying the y-axis significance cutoff in the
  **original** (untransformed) scale. The value is transformed by
  `ytrans` before plotting. When `NULL`, no y-cutoff line is drawn and
  the category assignment uses only the x-cutoff. Default: `0.05`.

- split_by:

  The column(s) to split the data by and produce separate sub-plots.
  Multiple columns are concatenated with `split_by_sep`.

- split_by_sep:

  A character string to separate concatenated `split_by` columns.
  Default `"_"`.

- label_by:

  A character string specifying the column whose values are used as
  label text. When `NULL` (default), row names of the data frame are
  used.

- x_cutoff_name:

  A character string for the x-cutoff legend entry. When `"none"`, the
  legend for the x-cutoff line is suppressed entirely (the line is still
  drawn). When `NULL` (default), a label of the form
  `"<x> = +/-<value>"` is generated.

- y_cutoff_name:

  A character string for the y-cutoff legend entry. When `"none"`, the
  legend for the y-cutoff line is suppressed entirely (the line is still
  drawn). When `NULL` (default), a label of the form
  `"<ylab> = <value>"` is generated.

- x_cutoff_color:

  A character string specifying the colour of the x-axis cutoff line(s).
  Default: `"red2"`.

- y_cutoff_color:

  A character string specifying the colour of the y-axis cutoff line(s).
  Default: `"blue2"`.

- x_cutoff_linetype:

  A character string specifying the linetype of the x-axis cutoff
  line(s). Default: `"dashed"`.

- y_cutoff_linetype:

  A character string specifying the linetype of the y-axis cutoff
  line(s). Default: `"dashed"`.

- x_cutoff_linewidth:

  A numeric value specifying the linewidth of the x-axis cutoff line(s).
  Default: `0.5`.

- y_cutoff_linewidth:

  A numeric value specifying the linewidth of the y-axis cutoff line(s).
  Default: `0.5`.

- pt_size:

  A numeric value specifying the point size for all data points.
  Default: `2`.

- pt_alpha:

  A numeric value in `[0, 1]` specifying the transparency of all data
  points. Default: `0.5`.

- pt_shape:

  A numeric value specifying the point shape. Default: `21` (filled
  circle with border). Shapes 21–25 support separate fill and border
  colour aesthetics; all other shapes use a single colour aesthetic. In
  raster mode, all points are drawn as filled circles (shape is
  ignored).

- pt_border_color:

  Controls the point border colour. For shapes 21–25:

  - `TRUE` (default) – border colour tracks the `color_by` gradient /
    palette.

  - A colour string (e.g. `"black"`) – constant colour border.

  `FALSE` or `NULL` disables the border. For shapes without a fill
  aesthetic (not 21–25), this parameter has no effect. In raster mode
  the border is drawn as a slightly larger disc behind each point (the
  shape is always a circle there), and `TRUE` falls back to a border
  disc in the `color_by` colour.

- pt_border_size:

  A numeric value specifying the point border size (stroke width, in
  mm). `0` disables the border. Default: `0.5`.

- nlabel:

  An integer specifying the number of top features to label
  automatically. Points are ranked by Euclidean distance to the origin
  within each `sign(x)` group (and per facet level if `facet_by` is
  set). Only non-insignificant points receive labels. Default: `5`.

- labels:

  A character vector of row names or integer indices specifying which
  points to label. Overrides automatic `nlabel` selection. When `NULL`
  (default), top `nlabel` points are chosen automatically.

- label_size:

  A numeric value specifying the font size of the labels. Default: `3`.

- label_fg:

  A character string specifying the text colour of the labels. Default:
  `"black"`.

- label_bg:

  A character string specifying the background colour of the label boxes
  (passed to `geom_text_repel(bg.color = ...)`). Default: `"white"`.

- label_bg_r:

  A numeric value specifying the corner radius of the label background
  boxes (passed to `geom_text_repel(bg.r = ...)`). Default: `0.1`.

- highlight:

  A character vector of row names or integer indices specifying which
  points to highlight with an overlaid point layer in `highlight_color`.
  When `NULL` (default), no highlighting is applied.

- highlight_color:

  A character string specifying the colour of the highlight points.
  Default: `"red"`.

- highlight_size:

  A numeric value specifying the point size of the highlight layer.
  Default: `2`.

- highlight_alpha:

  A numeric value in `[0, 1]` specifying the transparency of the
  highlight points. Default: `1`.

- highlight_stroke:

  A numeric value specifying the stroke width of the highlight point
  borders. Default: `0.5`.

- raster:

  A logical value. If `TRUE`, points are rendered via
  [`scattermore::geom_scattermore()`](https://rdrr.io/pkg/scattermore/man/geom_scattermore.html)
  for efficient rasterised plotting. Default is `NULL`, which
  auto-enables when `nrow(data) > 1e3`.

- raster_dpi:

  A numeric vector of length 2 `[x_dpi, y_dpi]` specifying the raster
  resolution in pixels. Passed to
  `scattermore::geom_scattermore(pixels = ...)`. Default is
  `c(512, 512)`. If a single value is provided it is recycled to both
  dimensions.

- trim:

  A numeric vector of length 2 specifying quantile bounds for
  winsorizing the x-axis values. Values below the first quantile are
  clamped to that quantile; values above the second quantile are clamped
  to that quantile. Both values must be in `[0, 1]`. When both bounds
  are nonzero and of opposite sign, they are symmetrised to the smaller
  absolute value. Default: `c(0, 1)` (no trimming).

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

- combine:

  Logical; when `TRUE` (default), returns a combined `patchwork` object.
  When `FALSE`, returns a named list of individual `ggplot` objects.

- ncol, nrow:

  Integer number of columns / rows for the combined layout (passed to
  [`wrap_plots`](https://patchwork.data-imaginist.com/reference/wrap_plots.html)).

- byrow:

  Logical; fill the combined layout by row. Default `TRUE` (passed to
  [`wrap_plots`](https://patchwork.data-imaginist.com/reference/wrap_plots.html)).

- axes:

  A character string specifying how axes should be treated across the
  combined layout (passed to
  [`wrap_plots`](https://patchwork.data-imaginist.com/reference/wrap_plots.html)).

- axis_titles:

  A character string specifying how axis titles should be treated across
  the combined layout. Defaults to `axes`.

- guides:

  A character string specifying how guides (legends) should be collected
  across panels. Default `"collect"` (passed to
  [`combine_plots()`](https://pwwang.github.io/plotthis/reference/combine_plots.md)).

- design:

  A custom layout design for the combined plot (passed to
  [`combine_plots()`](https://pwwang.github.io/plotthis/reference/combine_plots.md)).

- ...:

  Additional arguments.

## Value

A `ggplot` object, a `patchwork` object, or a named list of `ggplot`
objects (when `combine = FALSE`), each with `height` and `width`
attributes in inches.

## split_by Workflow

When `split_by` is provided:

1.  The `split_by` column(s) are validated via
    [`check_columns()`](https://pwwang.github.io/plotthis/reference/check_columns.md)
    with `force_factor = TRUE` and `concat_multi = TRUE` (multiple
    columns are concatenated with `split_by_sep`).

2.  The data frame is split by `split_by` (preserving factor level
    order). If `split_by` is `NULL`, the data is wrapped in a
    single-element list with name `"..."`.

3.  Per-split `palette`, `palcolor`, `legend.position`, and
    `legend.direction` are resolved via
    [`check_palette()`](https://pwwang.github.io/plotthis/reference/check_palette.md),
    [`check_palcolor()`](https://pwwang.github.io/plotthis/reference/check_palcolor.md),
    and
    [`check_legend()`](https://pwwang.github.io/plotthis/reference/check_legend.md).

4.  [`VolcanoPlotAtomic()`](https://pwwang.github.io/plotthis/reference/VolcanoPlotAtomic.md)
    is called for each split. If `title` is a function, it receives the
    split level name and can generate dynamic titles.

5.  Results are combined via
    [`combine_plots()`](https://pwwang.github.io/plotthis/reference/combine_plots.md)
    (when `combine = TRUE`) or returned as a named list.

## Examples

``` r
# \donttest{
set.seed(8525)
n <- 200
n_de <- 150

## Non-DE genes
fc_null <- rnorm(n - n_de, 0, 0.35)
z_null  <- rnorm(n - n_de, 0, 1)

## DE genes
fc_de <- rnorm(
  n_de,
  mean = sample(c(-1, 1), n_de, replace = TRUE),
  sd = 0.45
)

## Make significance related to effect size,
## but with substantial variation
z_de <- fc_de * rnorm(n_de, 4.5, 0.8)

avg_log2FC <- c(fc_null, fc_de)
z <- c(z_null, z_de)

p_val <- 2 * pnorm(-abs(z))
p_val_adj <- p.adjust(p_val, method = "BH")

## Shuffle genes
i <- sample(n)

data <- data.frame(
  avg_log2FC = avg_log2FC[i],
  p_val_adj = p_val_adj[i],
  gene = paste0("gene", seq_len(n))[i],
  pct_diff = rnorm(n, 0, 1),
  group = sample(LETTERS[1:2], n, replace = TRUE)
)

# --- Basic usage ---
VolcanoPlot(data, x = "avg_log2FC", y = "p_val_adj", color_by = "pct_diff",
   y_cutoff_name = "-log10(0.05)")

# --- With gene labels ---
VolcanoPlot(data, x = "avg_log2FC", y = "p_val_adj", color_by = "pct_diff",
   y_cutoff_name = "-log10(0.05)", label_by = "gene")

# --- Mirrored layout ---
VolcanoPlot(data, x = "avg_log2FC", y = "p_val_adj", y_cutoff_name = "none",
   flip_negatives = TRUE, label_by = "gene")

# --- With faceting ---
VolcanoPlot(data, x = "avg_log2FC", y = "p_val_adj", y_cutoff_name = "none",
   flip_negatives = TRUE, facet_by = "group", label_by = "gene")

# --- With splitting ---
VolcanoPlot(data, x = "avg_log2FC", y = "p_val_adj", y_cutoff_name = "none",
   flip_negatives = TRUE, split_by = "group", label_by = "gene")

# --- With highlighting ---
VolcanoPlot(data, x = "avg_log2FC", y = "p_val_adj", y_cutoff_name = "none",
   highlight = c("gene196", "gene151"), label_by = "gene")

# --- Per-split palettes ---
VolcanoPlot(data, x = "avg_log2FC", y = "p_val_adj", color_by = "pct_diff",
   y_cutoff_name = "-log10(0.05)", split_by = "group", label_by = "gene",
   palette = c(A = "Spectral", B = "PuOr"))

# Trim extreme x-values (winsorize to 40% and 50% quantiles), for demo purposes
VolcanoPlot(data, x = "avg_log2FC", y = "p_val_adj", color_by = "pct_diff",
   y_cutoff_name = "-log10(0.05)", trim = c(0.4, 0.5))

# }
```
