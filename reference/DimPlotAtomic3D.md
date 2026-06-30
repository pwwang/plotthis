# Internal helper to create a 3D dimension reduction plot using plotly

Renders an interactive 3D scatter plot via **plotly** for dimension
reduction data with three ordination axes. Called automatically by
[`DimPlotAtomic()`](https://pwwang.github.io/plotthis/reference/DimPlotAtomic.md)
when `length(dims) == 3`.

The function supports two visualisation modes:

- **group_by** — discrete group colouring with one trace per group,
  matching the 2D legend behaviour via `label_use`.

- **features** — continuous colour scale with a `plotly` colorscale bar.

Graphs / networks are rendered as 3D line segments (with NA separators
for line breaks). Lineage curves are fitted via
[`stats::loess()`](https://rdrr.io/r/stats/loess.html) per lineage
column and drawn as smoothed paths. For datasets exceeding 100 000
points, per-point hover text is disabled to reduce the JSON payload.

## Usage

``` r
DimPlotAtomic3D(
  data,
  dims,
  group_by = NULL,
  features = NULL,
  colorby,
  colors = NULL,
  feat_colors_value = NULL,
  label_use = NULL,
  labels_tb = NULL,
  keep_empty_group = FALSE,
  bg_color = "grey80",
  color_name = "",
  pt_size = NULL,
  pt_alpha = 1,
  show_stat = TRUE,
  title = NULL,
  subtitle = NULL,
  xlab = NULL,
  ylab = NULL,
  label = FALSE,
  label_insitu = FALSE,
  label_size = 4,
  label_fg = "white",
  label_bg = "black",
  highlight = NULL,
  highlight_color = "black",
  highlight_size = 1,
  highlight_stroke = 0.8,
  highlight_alpha = 1,
  graph = NULL,
  edge_size = c(0.05, 0.5),
  edge_alpha = 0.1,
  edge_color = "grey40",
  lineages = NULL,
  lineages_trim = c(0.01, 0.99),
  lineages_span = 0.75,
  lineages_palette = "Dark2",
  lineages_palcolor = NULL,
  palette = "Spectral",
  palcolor = NULL,
  palreverse = FALSE,
  n_sampled = NULL
)
```

## Arguments

- data:

  A data frame.

- dims:

  A character vector of the column names to plot on the x, y (and
  optionally z) axes or a numeric vector of the column indices. When 3
  dimensions are provided, a 3D interactive plot is created using
  plotly. Supported in 3D: group_by, features, labels, highlight,
  lineages, graph/network, show_stat, order. Not supported in 3D:
  add_mark, stat_by, add_density, velocity, hex, facet_by, raster.

- group_by:

  A character string of the column name to group the data by for
  discrete colouring. A character/factor column is expected. If multiple
  columns are provided, the columns will be concatenated with
  `group_by_sep`.

- features:

  A character vector of the column names to plot as features (continuous
  colouring). When multiple features are provided and `facet_by` is not
  set, the data is pivoted to long format and faceted by feature name.

- colorby:

  A character string naming the column used for colour mapping (either
  the `group_by` factor or the `features` numeric column).

- colors:

  A named character vector mapping group levels to hex colours. Only
  used in group_by mode.

- feat_colors_value:

  A numeric vector of the colour-scale anchor values for feature mode,
  as returned by
  [`prepare_continuous_color_scale()`](https://pwwang.github.io/plotthis/reference/prepare_continuous_color_scale.md).

- label_use:

  A character vector of formatted legend labels (with optional count
  annotations) used in group_by mode.

- labels_tb:

  A table of group counts for trace iteration in group_by mode.

- keep_empty_group:

  A logical value. If `TRUE`, empty factor levels are preserved; traces
  with zero rows are still added to the legend.

- bg_color:

  A character string specifying the colour used for NA-valued points and
  background context points drawn from other facets. Default is
  `"grey80"`.

- color_name:

  A character string used as the title for the continuous colour bar in
  feature mode. Default is `""`.

- pt_size:

  A numeric value of the point size. If NULL (default), the point size
  is auto-calculated as `min(3000 / nrow(data), 0.6)` so large datasets
  automatically get smaller points.

- pt_alpha:

  A numeric value in `[0, 1]` for the point transparency. Default is
  `1`.

- show_stat:

  A logical value. If `TRUE` (default), the number of points per group
  is shown in the legend labels and subtitle. Ignored when
  `theme = "theme_blank"`.

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

- label:

  A logical value. If `TRUE`, group labels (numeric indices by default,
  or group names when `label_insitu = TRUE`) are placed at the median
  coordinates of each group. Forced to `TRUE` when `label_repel` or
  `label_insitu` is set.

- label_insitu:

  A logical value. If `TRUE`, the raw group names are placed at the
  group median coordinates instead of numeric indices. Forces
  `label = TRUE`. Default is `FALSE`.

- label_size:

  A numeric value for the label text size. Passed to
  [`ggrepel::geom_text_repel()`](https://ggrepel.slowkow.com/reference/geom_text_repel.html).
  Default is `4`.

- label_fg:

  A character string for the label text (foreground) colour. Default is
  `"white"`.

- label_bg:

  A character string for the label background / outline colour. Default
  is `"black"`.

- highlight:

  A specification for highlighted points:

  - `NULL` (default): no highlighting.

  - `TRUE`: highlight all points (adds a dark outline around every
    point).

  - A character string: a dplyr filter expression (e.g.,
    `"clusters == 'Ductal'"`).

  - A character vector: row names to highlight.

  - A numeric vector: row indices to highlight.

- highlight_color:

  A character string for the colour of the outer highlight ring. Default
  is `"black"`.

- highlight_size:

  A numeric value for the size of the inner (coloured) highlight point.
  Default is `1`.

- highlight_stroke:

  A numeric value for the thickness of the outer highlight ring (the
  difference between the outer ring size and `highlight_size`). Default
  is `0.8`.

- highlight_alpha:

  A numeric value in `[0, 1]` for the transparency of highlighted
  points. Default is `1`.

- graph:

  A specification for network / graph edges to overlay. Sources:

  - A character string starting with `"@"` (e.g., `"@graph"`): extracts
    the attribute named `"graph"` from `attributes(data)`.

  - A `Graph` object (e.g., Seurat): coerced to dense matrix via
    [`as.matrix()`](https://rdrr.io/r/base/matrix.html).

  - A `matrix`, `data.frame`, or `dgCMatrix`: used directly as the
    adjacency matrix.

  - Numeric indices or character column names: extracts columns from
    `data`. Edges are drawn for non-zero, lower-triangle entries.
    Requires `data` to have row names matching the matrix dimnames.

- edge_size:

  A numeric vector of length 2 specifying the range `[min, max]` for
  `scale_linewidth_continuous(range = ...)` applied to edge widths.
  Default is `c(0.05, 0.5)`.

- edge_alpha:

  A numeric value in `[0, 1]` for the transparency of graph edges.
  Default is `0.1`.

- edge_color:

  A character string for the colour of graph edges. Default is
  `"grey40"`.

- lineages:

  A character vector of column names representing pseudotime /
  trajectory lineages. Each column is fitted with a LOESS smooth
  (`span = lineages_span, degree = 2`) across the 2D embedding, after
  trimming the top and bottom `lineages_trim` quantiles. Only supported
  in 2D without `facet_by`.

- lineages_trim:

  A numeric vector of length 2 specifying the lower and upper quantile
  thresholds `[0, 1]` for trimming lineage values before LOESS fitting.
  Default is `c(0.01, 0.99)`.

- lineages_span:

  A numeric value passed as `span` to
  [`stats::loess()`](https://rdrr.io/r/stats/loess.html) controlling the
  smoothness of the lineage curve. Smaller values follow the data more
  closely. Default is `0.75`.

- lineages_palette:

  A character string naming the palette for lineage colours. Default is
  `"Dark2"`.

- lineages_palcolor:

  A character vector of specific colours for lineage curves. Default is
  `NULL` (auto-resolved from `lineages_palette`).

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

- n_sampled:

  An integer indicating how many points were retained after
  down-sampling (displayed in the subtitle). `NULL` if no sampling
  occurred.

## Architecture

**DimPlotAtomic3D** executes the following steps:

1.  **plotly availability check** — errors early if the `plotly` package
    is not installed.

2.  **Axis labels** — defaults `xlab` / `ylab` / `zlab` from `dims`.

3.  **Marker size scaling** — multiplies `pt_size` by 2 to convert from
    ggplot2 mm units to plotly pixel units.

4.  **Large-data detection** — disables per-point hover text when
    `nrow(data) > 1e5`.

5.  **Base plotly object** —
    [`plotly::plot_ly()`](https://rdrr.io/pkg/plotly/man/plot_ly.html)
    initialisation.

6.  **Graph / network edges** (if `graph` is provided) — resolves the
    graph source (`@attribute`, Graph object, matrix, data.frame, column
    indices, or column names), aligns row/column names with `data`, sets
    upper-triangle and zero entries to `NA`, melts into an edge data
    frame, builds x/y/z coordinate vectors (interleaved with NA for line
    breaks), and adds `scatter3d` line traces.

7.  **Main scatter traces**:

    - *group_by mode*: iterates over `labels_tb` entries, creating one
      trace per group with the resolved colour. NA groups use
      `bg_color`. Legend labels use `label_use` (with count annotations
      if `show_stat = TRUE`).

    - *features mode*: builds a plotly-style colorscale from the
      continuous palette. NA points are rendered first in `bg_color`
      without a colour bar; non-NA points follow with a
      `showscale = TRUE` colour bar.

8.  **Highlight** — if `highlight` is specified, highlighted points are
    plotted as open circles with increased marker size (to account for
    stroke). Supports `TRUE` (all points), a filter expression string,
    or a vector of row names / numeric indices.

9.  **Lineages** — each lineage column is validated, trimmed to
    `[lineages_trim[1], lineages_trim[2]]` quantiles, ordered by value,
    and a `loess(span = lineages_span, degree = 2)` smooth is fitted per
    dimension. The smoothed curve is rendered as a 3D line trace.

10. **Labels** — when `label = TRUE` and `group_by` is set, group median
    coordinates are computed via
    [`aggregate()`](https://rdrr.io/r/stats/aggregate.html). In 3D,
    plotly does not support text background/outline, so `label_bg` is
    used directly as the text colour. Text size is scaled by 3× for
    plotly.

11. **Layout** — subtitle is composited with an optional "Showing N
    sampled points" note. The full title is rendered as HTML
    (`<br><sup>...</sup>`). The scene sets
    `aspectratio = list(x = 1, y = 1, z = 1)` and axis titles.
