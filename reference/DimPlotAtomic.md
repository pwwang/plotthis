# Atomic Dimension Reduction Plot without splitting the data

Core implementation for dimension reduction visualisation. This is the
internal workhorse dispatched by both
[`DimPlot()`](https://pwwang.github.io/plotthis/reference/dimplot.md)
(group-based) and
[`FeatureDimPlot()`](https://pwwang.github.io/plotthis/reference/dimplot.md)
(continuous feature expression). It renders a 2D scatter plot of
ordination axes with extensive annotation capabilities, and
automatically delegates to
[`DimPlotAtomic3D()`](https://pwwang.github.io/plotthis/reference/DimPlotAtomic3D.md)
for interactive 3D plots when three `dims` are provided.

The function supports two primary colouring modes:

- **group_by** — discrete factor colouring with a legend of group
  levels, optional density/statistical overlays, and group mark / label
  annotations.

- **features** — continuous numeric colouring with a gradient colour
  bar, multi-feature faceting via
  [`tidyr::pivot_longer()`](https://tidyr.tidyverse.org/reference/pivot_longer.html),
  and optional cutoff / quantile trimming via
  [`prepare_continuous_color_scale()`](https://pwwang.github.io/plotthis/reference/prepare_continuous_color_scale.md).

Additional annotation layers include: graph / network edges drawn as
segments between connected nodes, 2D density contours (filled or
outline), group marks (hull, ellipse, rect, circle via `ggforce`),
lineage curves (LOESS-smoothed paths with optional whiskers), velocity /
RNA-velocity arrows (raw, grid, or stream via
[`VelocityPlot()`](https://pwwang.github.io/plotthis/reference/VelocityPlot.md)),
statistical summary mini-plots (pie, ring, bar, line) embedded at group
centroids, background points from other facets (faded context), and
group labels with repulsion (via
[`ggrepel::geom_text_repel()`](https://ggrepel.slowkow.com/reference/geom_text_repel.html)).

Rendering scales automatically: scatter points for small datasets,
[`scattermore::geom_scattermore()`](https://rdrr.io/pkg/scattermore/man/geom_scattermore.html)
raster for `n > 1e5`, or hex-binned aggregation (`stat_summary_hex()` /
`geom_hex()`). Legend assembly uses
[`cowplot::get_plot_component()`](https://wilkelab.org/cowplot/reference/get_plot_component.html)
with independent guide-boxes for base groups, lineages, velocity, and
stat-by annotations — combined via `rbind` / `cbind` and re-inserted
with `add_grob()`.

## Usage

``` r
DimPlotAtomic(
  data,
  dims = 1:2,
  group_by = NULL,
  group_by_sep = "_",
  features = NULL,
  lower_quantile = 0,
  upper_quantile = 0.99,
  lower_cutoff = NULL,
  upper_cutoff = NULL,
  pt_size = NULL,
  pt_alpha = 1,
  bg_color = "grey80",
  bg_cutoff = NULL,
  color_name = "",
  label_insitu = FALSE,
  show_stat = !identical(theme, "theme_blank"),
  label = FALSE,
  label_size = 4,
  label_fg = "white",
  label_bg = "black",
  label_bg_r = 0.1,
  label_repel = FALSE,
  label_repulsion = 20,
  label_pt_size = 1,
  label_pt_color = "black",
  label_segment_color = "black",
  order = c("as-is", "reverse", "high-top", "low-top", "random"),
  highlight = NULL,
  highlight_alpha = 1,
  highlight_size = 1,
  highlight_color = "black",
  highlight_stroke = 0.8,
  add_mark = FALSE,
  mark_type = c("hull", "ellipse", "rect", "circle"),
  mark_expand = unit(3, "mm"),
  mark_alpha = 0.1,
  mark_linetype = 1,
  stat_by = NULL,
  stat_plot_type = c("pie", "ring", "bar", "line"),
  stat_plot_size = 0.1,
  stat_palette = "Set1",
  stat_args = list(),
  graph = NULL,
  edge_size = c(0.05, 0.5),
  edge_alpha = 0.1,
  edge_color = "grey40",
  add_density = FALSE,
  density_color = "grey80",
  density_filled = FALSE,
  density_filled_palette = "Greys",
  density_filled_palcolor = NULL,
  lineages = NULL,
  lineages_trim = c(0.01, 0.99),
  lineages_span = 0.75,
  lineages_palette = "Dark2",
  lineages_palcolor = NULL,
  lineages_arrow = ggplot2::arrow(length = unit(0.1, "inches")),
  lineages_linewidth = 1,
  lineages_line_bg = "white",
  lineages_line_bg_stroke = 0.5,
  lineages_whiskers = FALSE,
  lineages_whiskers_linewidth = 0.5,
  lineages_whiskers_alpha = 0.5,
  velocity = NULL,
  velocity_plot_type = c("raw", "grid", "stream"),
  velocity_n_neighbors = NULL,
  velocity_density = 1,
  velocity_smooth = 0.5,
  velocity_scale = 1,
  velocity_min_mass = 1,
  velocity_cutoff_perc = 5,
  velocity_group_palette = "Set2",
  velocity_group_palcolor = NULL,
  arrow_angle = 20,
  arrow_color = "black",
  streamline_l = 5,
  streamline_minl = 1,
  streamline_res = 1,
  streamline_n = 15,
  arrow_alpha = 1,
  streamline_width = c(0, 0.8),
  streamline_alpha = 1,
  streamline_color = NULL,
  streamline_palette = "RdYlBu",
  streamline_palcolor = NULL,
  streamline_bg_color = "white",
  streamline_bg_stroke = 0.5,
  keep_na = FALSE,
  keep_empty = FALSE,
  facet_by = NULL,
  facet_scales = "fixed",
  facet_nrow = NULL,
  facet_ncol = NULL,
  facet_byrow = TRUE,
  title = NULL,
  subtitle = NULL,
  xlab = NULL,
  ylab = NULL,
  theme = "theme_this",
  theme_args = list(),
  aspect.ratio = 1,
  legend.position = "right",
  legend.direction = "vertical",
  raster = NULL,
  raster_dpi = c(512, 512),
  hex = FALSE,
  hex_linewidth = 0.5,
  hex_count = !is.null(group_by),
  hex_bins = 50,
  hex_binwidth = NULL,
  palette = ifelse(is.null(features), "Paired", "Spectral"),
  palcolor = NULL,
  palreverse = FALSE,
  seed = 8525,
  ...
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

- group_by_sep:

  A character string to concatenate the columns in `group_by`, if
  multiple columns are provided.

- features:

  A character vector of the column names to plot as features (continuous
  colouring). When multiple features are provided and `facet_by` is not
  set, the data is pivoted to long format and faceted by feature name.

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

- pt_size:

  A numeric value of the point size. If NULL (default), the point size
  is auto-calculated as `min(3000 / nrow(data), 0.6)` so large datasets
  automatically get smaller points.

- pt_alpha:

  A numeric value in `[0, 1]` for the point transparency. Default is
  `1`.

- bg_color:

  A character string specifying the colour used for NA-valued points and
  background context points drawn from other facets. Default is
  `"grey80"`.

- bg_cutoff:

  A numeric threshold. Feature values with absolute value below this
  cutoff are set to `NA` (and therefore rendered in `bg_color`). Default
  is `NULL`.

- color_name:

  A character string used as the title for the continuous colour bar in
  feature mode. Default is `""`.

- label_insitu:

  A logical value. If `TRUE`, the raw group names are placed at the
  group median coordinates instead of numeric indices. Forces
  `label = TRUE`. Default is `FALSE`.

- show_stat:

  A logical value. If `TRUE` (default), the number of points per group
  is shown in the legend labels and subtitle. Ignored when
  `theme = "theme_blank"`.

- label:

  A logical value. If `TRUE`, group labels (numeric indices by default,
  or group names when `label_insitu = TRUE`) are placed at the median
  coordinates of each group. Forced to `TRUE` when `label_repel` or
  `label_insitu` is set.

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

- label_bg_r:

  A numeric value for the background fill ratio of the label bounding
  box. Passed to `ggrepel::geom_text_repel(bg.r = ...)`. Default is
  `0.1`.

- label_repel:

  A logical value. If `TRUE`, labels are repelled from each other with
  force `label_repulsion`. A visible point anchor is drawn. Forces
  `label = TRUE`.

- label_repulsion:

  A numeric value for the repulsion force when `label_repel = TRUE`.
  Passed to `ggrepel::geom_text_repel(force = ...)`. Default is `20`.

- label_pt_size:

  A numeric value for the size of the anchor point drawn when
  `label_repel = TRUE`. Default is `1`.

- label_pt_color:

  A character string for the colour of the label anchor point. Default
  is `"black"`.

- label_segment_color:

  A character string for the colour of the line segment connecting the
  label to the anchor. Used in non-repel mode (`label_repel = FALSE`)
  where `min.segment.length = 0`. Default is `"black"`.

- order:

  A character string controlling the draw order of points:

  - `"as-is"` (default) — the row order in the data is preserved.

  - `"reverse"` — rows are reversed.

  - `"high-top"` — points with high values (last factor levels for
    group_by) are drawn last (on top).

  - `"low-top"` — points with low values (first factor levels) are drawn
    last.

  - `"random"` — rows are randomly shuffled.

  For `high-top` and `low-top`, NA values are always plotted at the
  bottom. When applied to `group_by`, only the draw order changes —
  legend colours and order are unaffected. Within the same level, point
  order is preserved. For precise control, set factor levels before
  plotting. See
  <https://github.com/pwwang/scplotter/issues/29#issuecomment-3009694130>
  for examples.

- highlight:

  A specification for highlighted points:

  - `NULL` (default): no highlighting.

  - `TRUE`: highlight all points (adds a dark outline around every
    point).

  - A character string: a dplyr filter expression (e.g.,
    `"clusters == 'Ductal'"`).

  - A character vector: row names to highlight.

  - A numeric vector: row indices to highlight.

- highlight_alpha:

  A numeric value in `[0, 1]` for the transparency of highlighted
  points. Default is `1`.

- highlight_size:

  A numeric value for the size of the inner (coloured) highlight point.
  Default is `1`.

- highlight_color:

  A character string for the colour of the outer highlight ring. Default
  is `"black"`.

- highlight_stroke:

  A numeric value for the thickness of the outer highlight ring (the
  difference between the outer ring size and `highlight_size`). Default
  is `0.8`.

- add_mark:

  A logical value. If `TRUE`, group boundaries are drawn around points
  using `ggforce` marks. Requires `group_by`. Only supported in 2D.

- mark_type:

  A character string specifying the mark shape. Options: `"hull"`
  (convex hull, default), `"ellipse"`, `"rect"`, or `"circle"`.

- mark_expand:

  A unit value for the outward expansion of the mark boundary. Passed to
  `ggforce::geom_mark_*(expand = ...)`. Default is `unit(3, "mm")`.

- mark_alpha:

  A numeric value in `[0, 1]` for the transparency of the mark fill.
  Default is `0.1`.

- mark_linetype:

  A numeric value for the line type of the mark boundary. Default is `1`
  (solid).

- stat_by:

  A character string naming a column used to compute per-group
  statistical summary mini-plots embedded at group centroid positions.
  Only supported with `group_by` (not `features`). Only supported in 2D
  without `facet_by`.

- stat_plot_type:

  A character string specifying the mini-plot type. Options: `"pie"`
  (default), `"ring"`, `"bar"`, or `"line"`.

- stat_plot_size:

  A numeric value for the size of the stat mini-plot, expressed as a
  fraction of the axis range. Default is `0.1`.

- stat_args:

  A list of additional arguments passed to the stat plot function (e.g.,
  `list(palette = "Set1")`). Default is `list(palette = "Set1")`.

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

- add_density:

  A logical value. If `TRUE`, a 2D density layer is overlaid. Only
  supported in 2D.

- density_color:

  A character string for the colour of the density contour lines. Used
  when `density_filled = FALSE`. Default is `"grey80"`.

- density_filled:

  A logical value. If `TRUE`, the density is rendered as a filled raster
  (`stat_density_2d(geom = "raster")`) instead of contour lines. A
  separate fill scale is used.

- density_filled_palette:

  A character string naming the palette for the filled density layer.
  Default is `"Greys"`.

- density_filled_palcolor:

  A character vector of specific colours for the filled density palette.
  Default is `NULL` (auto-resolved from `density_filled_palette`).

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

- lineages_arrow:

  A ggplot2 `arrow` specification applied to the end of lineage paths.
  Default is `arrow(length = unit(0.1, "inches"))`.

- lineages_linewidth:

  A numeric value for the width of the lineage curve lines. Default is
  `1`.

- lineages_line_bg:

  A character string for the colour of the background (wider) stroke
  drawn behind each lineage curve for improved visibility. Default is
  `"white"`.

- lineages_line_bg_stroke:

  A numeric value for the additional width of the background stroke
  relative to `lineages_linewidth`. The background line has total width
  `lineages_linewidth + lineages_line_bg_stroke`. Default is `0.5`.

- lineages_whiskers:

  A logical value. If `TRUE`, short line segments connect the smoothed
  lineage curve to the original data coordinates of the fitted points.
  Default is `FALSE`.

- lineages_whiskers_linewidth:

  A numeric value for the width of the whisker lines. Default is `0.5`.

- lineages_whiskers_alpha:

  A numeric value in `[0, 1]` for the transparency of the whisker lines.
  Default is `0.5`.

- velocity:

  A specification for RNA-velocity arrows. Can be:

  - `NULL` (default): no velocity overlay.

  - A character / integer vector: column names or indices in `data` for
    the velocity embedding.

  - A data frame or matrix: the velocity embedding itself (must align
    with `data` rows). Only supported in 2D without `facet_by`.

- velocity_plot_type:

  A character string specifying the velocity rendering style. Options:
  `"raw"` (arrows from embedding), `"grid"` (grid-based arrows), or
  `"stream"` (streamlines). Default is `"raw"`.

- velocity_n_neighbors:

  A numeric value for the number of neighbours used in the velocity grid
  computation. Default is `NULL` (auto).

- velocity_density:

  A numeric value for the velocity kernel density bandwidth. Default is
  `1`.

- velocity_smooth:

  A numeric value for the velocity smoothing parameter. Default is
  `0.5`.

- velocity_scale:

  A numeric value for scaling the velocity arrows. Default is `1`.

- velocity_min_mass:

  A numeric value for the minimum cell mass threshold in velocity grid
  computation. Default is `1`.

- velocity_cutoff_perc:

  A numeric value for the velocity cutoff percentage. Default is `5`.

- velocity_group_palette:

  A character string naming the palette for velocity group colours (used
  in `"raw"` plot type). Default is `"Set2"`.

- velocity_group_palcolor:

  A character vector of specific colours for velocity groups. Default is
  `NULL` (auto-resolved from `velocity_group_palette`).

- arrow_angle:

  A numeric value specifying the angle of the arrowheads in degrees.
  Applied to [`arrow`](https://rdrr.io/r/grid/arrow.html) when
  `plot_type` is `"raw"` or `"grid"`. Default is 20.

- arrow_color:

  A character string specifying the color of the velocity arrows. For
  `plot_type = "stream"`, this sets only the arrowhead color. Default is
  `"black"`.

- streamline_l:

  A numeric value specifying the integration length of the streamlines.
  Passed to
  [`geom_streamline`](https://eliocamp.github.io/metR/reference/geom_streamline.html)
  as the `L` parameter. Default is 5.

- streamline_minl:

  A numeric value specifying the minimum streamline length. Shorter
  streamlines are not drawn. Passed to
  [`geom_streamline`](https://eliocamp.github.io/metR/reference/geom_streamline.html)
  as the `min.L` parameter. Default is 1.

- streamline_res:

  A numeric value specifying the resolution of the streamline
  integration. Passed to
  [`geom_streamline`](https://eliocamp.github.io/metR/reference/geom_streamline.html)
  as the `res` parameter. Default is 1.

- streamline_n:

  A numeric value specifying the number of streamlines to draw. Passed
  to
  [`geom_streamline`](https://eliocamp.github.io/metR/reference/geom_streamline.html)
  as the `n` parameter. Default is 15.

- arrow_alpha:

  A numeric value between 0 and 1 specifying the transparency of the
  velocity arrows. Only used when `plot_type = "raw"` or `"grid"`; for
  `plot_type = "stream"`, use `streamline_alpha` instead. Default is 1.

- streamline_width:

  A numeric vector of length 2 specifying the range of line widths for
  streamlines. Passed to `scale_size(range = ...)`. Only used when
  `streamline_color` is `NULL`. Default is `c(0, 0.8)`.

- streamline_alpha:

  A numeric value between 0 and 1 specifying the transparency of the
  velocity streamlines. Default is 1.

- streamline_color:

  An optional character string specifying a fixed color for streamlines.
  When `NULL` (the default), streamlines are colored by velocity
  magnitude using `streamline_palette`.

- streamline_palette:

  A character string specifying the color palette for streamline
  velocity magnitude. Passed to
  [`palette_this`](https://pwwang.github.io/plotthis/reference/palette_this.md).
  Only used when `streamline_color` is `NULL`. Default is `"RdYlBu"`.

- streamline_palcolor:

  An optional character vector of specific colors for the streamline
  velocity gradient. If `NULL`, colors are generated from
  `streamline_palette`. Default is `NULL`.

- streamline_bg_color:

  A character string specifying the background (outline) color applied
  to streamlines to create a stroke effect. Default is `"white"`.

- streamline_bg_stroke:

  A numeric value specifying the additional line width of the background
  stroke relative to the foreground streamline. Default is 0.5.

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

- theme:

  A character string or a theme class (i.e. ggplot2::theme_classic)
  specifying the theme to use. Default is "theme_this".

- theme_args:

  A list of arguments to pass to the theme function.

- aspect.ratio:

  A numeric value specifying the aspect ratio of the plot.

- legend.position:

  A character string specifying the position of the legend. if
  `waiver()`, for single groups, the legend will be "none", otherwise
  "right".

- legend.direction:

  A character string specifying the direction of the legend.

- raster:

  A logical value. If `TRUE`, points are rendered via
  [`scattermore::geom_scattermore()`](https://rdrr.io/pkg/scattermore/man/geom_scattermore.html)
  for efficient rasterised plotting. Default is `NULL`, which
  auto-enables when `nrow(data) > 1e5`.

- raster_dpi:

  A numeric vector of length 2 `[x_dpi, y_dpi]` specifying the raster
  resolution in pixels. Passed to
  `scattermore::geom_scattermore(pixels = ...)`. Default is
  `c(512, 512)`. If a single value is provided it is recycled to both
  dimensions.

- hex:

  A logical value. If `TRUE`, points are rendered as hexagonal bins via
  `geom_hex()` / `stat_summary_hex()`. Not supported with highlight.
  Default is `FALSE`. Only supported in 2D.

- hex_linewidth:

  A numeric value for the width of the hexagon boundary lines. Default
  is `0.5`.

- hex_count:

  A logical value. If `TRUE` and `group_by` is set, hex fill alpha is
  mapped to `after_stat(count)` so denser bins are more opaque. For
  `features` mode hex_count is ignored. Default is `!is.null(group_by)`.

- hex_bins:

  A numeric value for the number of hex bins along each axis. Passed to
  `geom_hex(bins = ...)`. Default is `50`.

- hex_binwidth:

  A numeric value for the width of individual hex bins. Passed to
  `geom_hex(binwidth = ...)`. Takes precedence over `hex_bins` when set.

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

- seed:

  The random seed to use. Default is 8525.

- ...:

  Additional arguments.

## Value

A ggplot object or a plotly object (when 3 dimensions are provided)

## Architecture

**DimPlotAtomic** executes the following steps:

1.  **ggplot dispatch** — selects `gglogger::ggplot` or
    [`ggplot2::ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html).

2.  **Order validation** — `match.arg(order)`.

3.  **Dimension resolution** — converts numeric indices to column names,
    validates exactly 2 or 3 dims via
    [`check_columns()`](https://pwwang.github.io/plotthis/reference/check_columns.md).

4.  **Column resolution** — validates `group_by` (force_factor,
    concat_multi), `features`, and `facet_by`. Requires at least one of
    `group_by` or `features`. Blocks `facet_by` when multiple features
    are present (features themselves become facets).

5.  **Auto defaults** — `pt_size` defaults to
    `min(3000 / nrow(data), 0.6)`; `raster` auto-enables when
    `nrow(data) > 1e5`; `theme_blank` suppresses axis labels.

6.  **Label force-enable** — if `label_repel` or `label_insitu` is
    `TRUE`, `label` is forced to `TRUE` with a message.

7.  **NA / empty handling** —
    [`process_keep_na_empty()`](https://pwwang.github.io/plotthis/reference/process_keep_na_empty.md)
    filters the data; `keep_empty` values extracted for group_by and
    facet_by.

8.  **group_by preprocessing** — group values / colours resolved via
    [`palette_this()`](https://pwwang.github.io/plotthis/reference/palette_this.md).
    NA levels mapped to literal `"NA"` string. `label_use` is
    constructed based on `label`, `label_insitu`, and `show_stat`
    combinations. Facet labeller is set to `as_labeller()` with
    per-facet N annotations when `show_stat = TRUE`.

9.  **Multi-feature pivot** — when `length(features) > 1`, data is
    pivoted to long format via
    [`tidyr::pivot_longer()`](https://tidyr.tidyverse.org/reference/pivot_longer.html)
    with `.feature` as the facet variable and `.value` as the numeric
    column.

10. **Continuous colour scale** — when `features` is set,
    [`prepare_continuous_color_scale()`](https://pwwang.github.io/plotthis/reference/prepare_continuous_color_scale.md)
    applies quantile/cutoff trimming and returns anchor values for the
    gradient.

11. **Point ordering** — applies the selected `order` strategy (as-is,
    reverse, high-top, low-top, random) by sorting or shuffling rows.

12. **3D branch** — if `length(dims) == 3`, emits a warning for
    unsupported features, optionally down-samples with stratified group
    sampling (when `raster = TRUE`), and delegates to
    [`DimPlotAtomic3D()`](https://pwwang.github.io/plotthis/reference/DimPlotAtomic3D.md).

13. **Group marks** (2D only, when `add_mark = TRUE`) — dispatches to
    `ggforce::geom_mark_hull/ellipse/rect/circle`, with
    `new_scale_fill()` / `new_scale_color()` to isolate mark scales.

14. **Graph / network** (2D only) — resolves the graph source (same
    logic as 3D), melts the matrix, handles faceted data by per-facet
    edge splitting, and adds `geom_segment(aes(linewidth = value))` with
    `scale_linewidth_continuous()`.

15. **Density overlay** (2D only) — filled
    (`stat_density_2d(geom = "raster")`) or outline
    (`geom_density_2d()`).

16. **Base scales and theme** — x / y limits from data range,
    `do_call(theme, theme_args)`, aspect ratio and legend position.

17. **Background points for facet context** — for faceted plots
    (excluding multi-feature auto-faceting), points from other facets
    are added as faded background (raster / hex / point depending on
    settings).

18. **Main point layer** — dispatches by rendering mode:

    - *raster*:
      [`scattermore::geom_scattermore()`](https://rdrr.io/pkg/scattermore/man/geom_scattermore.html)
      with separate NA / non-NA group layers.

    - *hex*: `geom_hex()` (group_by with optional count alpha, or
      `stat_summary_hex()` for features). Raises `has_fill = TRUE`.

    - *standard*: `geom_point()`.

19. **Highlight** (2D only) — resolves highlight specification (TRUE,
    filter expression, row names, indices), errors on hex + highlight
    combo, renders highlight points with stroke (outer ring + inner
    colour).

20. **Colour scales** — `scale_color_manual()` for group_by (with
    `keep_empty`-aware `breaks`/`limits`/`drop`), or
    `scale_color_gradientn()` for features. `scale_fill_manual()` /
    `scale_fill_gradientn()` added when `has_fill` is TRUE.

21. **Base legend** — extracted via
    `cowplot::get_plot_component("guide-box-bottom")`.

22. **Lineages** (2D only, no facet_by) — per-lineage LOESS fitting
    (`span = lineages_span, degree = 2`) with optional whiskers
    connecting smoothed to raw coordinates. Rendered as `geom_path()`
    with background stroke + foreground colour, plus
    `scale_color_manual()`. Legend extracted as a separate guide-box.

23. **Velocity** (2D only, no facet_by) — delegates to
    [`VelocityPlot()`](https://pwwang.github.io/plotthis/reference/VelocityPlot.md)
    with `return_layer = TRUE`. Adds `new_scale_color()` when the
    velocity layer has its own colour scale. Legend extracted
    separately.

24. **Stat-by mini-plots** (2D only, no facet_by, no features) —
    dispatches to
    [`PieChart()`](https://pwwang.github.io/plotthis/reference/PieChart.md)
    /
    [`RingPlot()`](https://pwwang.github.io/plotthis/reference/RingPlot.md)
    /
    [`BarPlot()`](https://pwwang.github.io/plotthis/reference/barplot.md)
    /
    [`LinePlot()`](https://pwwang.github.io/plotthis/reference/LinePlot.md)
    via
    [`do_call()`](https://pwwang.github.io/plotthis/reference/do_call.md).
    Each mini-plot is rendered as `annotation_custom()` at the group's
    median coordinates, scaled by `stat_plot_size * range`. Legend
    extracted from the stat plot.

25. **Group labels** (2D only, no features) — `geom_text_repel()` with
    optional repulsion (`force = label_repulsion`) or fixed segments
    (`force = 0`, bold, `min.segment.length = 0`). Labels positioned at
    group median coordinates (computed per facet if applicable).

26. **Legend assembly** — when additional legends exist (lineages,
    velocity, stat_by), they are combined with the base legend via
    `cbind` (vertical direction) or `rbind` (horizontal). The combined
    legend is re-inserted via
    `add_grob(gtable, legend, legend.position)`.

27. **Dimension calculation** —
    `calculate_plot_dimensions(base_height = 5.5, aspect.ratio, legend_n, legend_nchar)`
    sets `height` / `width` attributes.

28. **Faceting** —
    [`facet_plot()`](https://pwwang.github.io/plotthis/reference/facet_plot.md)
    is called only when no additional legends were assembled (otherwise
    the combined-legend grob is returned directly).
