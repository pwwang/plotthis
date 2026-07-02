# Atomic linked heatmap (internal)

Draws two heatmaps side-by-side with spline link lines connecting
matching rows across the left and right heatmaps. This is the core
implementation layer — it takes a **single** data frame and a full set
of left/right parameters, delegates to
[`HeatmapAtomic`](https://pwwang.github.io/plotthis/reference/HeatmapAtomic.md)
(with `return_ht = TRUE`) to obtain prepared
[`ComplexHeatmap::Heatmap`](https://rdrr.io/pkg/ComplexHeatmap/man/Heatmap.html)
objects, extracts exact dimension information, and then assembles
everything into a composite `grid` layout.

## Usage

``` r
LinkedHeatmapAtomic(
  data,
  left_values_by,
  right_values_by,
  left_rows_by,
  right_rows_by,
  left_columns_by,
  right_columns_by,
  left_columns_split_by = NULL,
  right_columns_split_by = NULL,
  left_pie_group_by = NULL,
  right_pie_group_by = NULL,
  rows_split_by = NULL,
  values_fill = NA,
  palette = "RdBu",
  palcolor = NULL,
  palreverse = FALSE,
  pie_size_name = "size",
  pie_size = NULL,
  pie_values = "length",
  pie_palette = "Spectral",
  pie_palcolor = NULL,
  bars_sample = 100,
  label = identity,
  label_size = 10,
  label_color = "black",
  label_name = "label",
  mark = identity,
  mark_color = "black",
  mark_size = 1,
  mark_name = "mark",
  violin_fill = NULL,
  boxplot_fill = NULL,
  dot_size = 8,
  dot_size_name = "size",
  legend_items = NULL,
  legend_discrete = FALSE,
  legend.position = "right",
  legend.direction = "vertical",
  lower_quantile = 0,
  upper_quantile = 0.99,
  lower_cutoff = NULL,
  upper_cutoff = NULL,
  add_bg = FALSE,
  bg_alpha = 0.5,
  keep_na = FALSE,
  keep_empty = FALSE,
  add_reticle = FALSE,
  reticle_color = "grey",
  left_cluster_rows = NULL,
  right_cluster_rows = NULL,
  cluster_columns = NULL,
  show_row_names = NULL,
  show_column_names = NULL,
  border = TRUE,
  title = NULL,
  title_params = NULL,
  column_title = NULL,
  row_title = NULL,
  na_col = "grey85",
  column_names_side = "bottom",
  row_annotation = NULL,
  row_annotation_side = NULL,
  row_annotation_palette = NULL,
  row_annotation_palcolor = NULL,
  row_annotation_type = NULL,
  row_annotation_params = NULL,
  row_annotation_agg = NULL,
  column_annotation = NULL,
  column_annotation_side = NULL,
  column_annotation_palette = NULL,
  column_annotation_palcolor = NULL,
  column_annotation_type = NULL,
  column_annotation_params = NULL,
  column_annotation_agg = NULL,
  links_span = 0.5,
  link_width_by = NULL,
  link_width_scale = 5,
  link_color = "grey30",
  link_alpha = 0.8,
  alpha = 1,
  seed = 8525,
  padding = 15,
  base_size = 1,
  aspect.ratio = NULL,
  draw_opts = list(),
  layer_fun_callback = NULL,
  cell_type = c("tile", "bars", "label", "mark", "label+mark", "mark+label", "dot",
    "violin", "boxplot", "pie"),
  cell_agg = NULL,
  ...
)
```

## Arguments

- data:

  A data frame in long format. Each row represents one observation;
  columns specify row/column membership for both left and right heatmaps
  as well as the values to encode as color.

- left_values_by, right_values_by:

  Column name whose values determine the fill color of cells in the left
  / right heatmap. These are the primary data columns visualized by the
  colour scale.

- left_rows_by, right_rows_by:

  Column name that defines the rows of the left / right heatmap. Each
  unique value becomes one row.

- left_columns_by, right_columns_by:

  Column name that defines the columns of the left / right heatmap. Each
  unique value becomes one column.

- left_columns_split_by, right_columns_split_by:

  Optional column name to split the columns of the left / right heatmap
  into groups (passed to
  [`ComplexHeatmap::Heatmap`](https://rdrr.io/pkg/ComplexHeatmap/man/Heatmap.html)
  as `column_split`).

- left_pie_group_by, right_pie_group_by:

  When `cell_type = "pie"`, column(s) to group values by within each pie
  cell for the left / right heatmap. Multiple columns are concatenated.

- rows_split_by:

  Optional column name to split the rows of **both** heatmaps into
  groups (passed as `row_split`). When provided, row names in the link
  table are prefixed with the split level to disambiguate rows across
  splits.

- values_fill:

  A value used to fill missing cells in the matrix (passed to
  `HeatmapAtomic`). Default is `NA` (cells with no data are left empty).

- palette:

  A character string naming a palette (see
  [`show_palettes`](https://pwwang.github.io/plotthis/reference/show_palettes.md))
  or a character vector of colours for the main heatmap colour scale.
  Default `"RdBu"`. Applied to both heatmaps unless overridden per-side
  via `...`.

- palcolor:

  A custom colour vector that overrides `palette` for the main heatmap
  colour scale. Applied to both heatmaps unless overridden per-side.

- palreverse:

  Logical; if `TRUE`, reverse the palette direction.

- pie_size_name:

  Legend title for the pie size when `cell_type = "pie"`.

- pie_size:

  A numeric value or function returning the pie radius. When a function,
  it receives the count of groups in the pie and should return a radius.

- pie_values:

  A function or string (convertible via
  [`match.arg`](https://rdrr.io/r/base/match.arg.html)) to compute the
  value represented by each pie slice. Default `"length"` counts
  observations per group.

- pie_palette, pie_palcolor:

  Palette and custom colours for pie slice fill colours.

- bars_sample:

  Number of observations sampled per cell when `cell_type = "bars"`.
  Default 100.

- label:

  A function to compute text labels when `cell_type = "label"` (or
  `"label+mark"`). Receives the aggregated value for a cell and
  optionally row/column indices and names. See
  [`HeatmapAtomic`](https://pwwang.github.io/plotthis/reference/HeatmapAtomic.md)
  for the full dispatch contract.

- label_size:

  Default point size for label text (used as fallback when the `label`
  function does not return a `size` field).

- label_color:

  Default colour for label text (used as fallback when the `label`
  function does not return a `color` field).

- label_name:

  Legend title for the label colour scale.

- mark:

  A function to compute mark symbols when `cell_type = "mark"` (or
  `"label+mark"`). Same dispatch contract as `label`. See
  [`HeatmapAtomic`](https://pwwang.github.io/plotthis/reference/HeatmapAtomic.md)
  for supported mark types.

- mark_color:

  Default mark colour (fallback).

- mark_size:

  Default mark stroke width in pt (fallback).

- mark_name:

  Legend title for the mark colour scale.

- violin_fill:

  A character vector of colours to use as fill for violin plots when
  `cell_type = "violin"`. If `NULL`, the annotation colour is used.

- boxplot_fill:

  A character vector of colours to use as fill for boxplots when
  `cell_type = "boxplot"`. If `NULL`, the annotation colour is used.

- dot_size:

  Dot size when `cell_type = "dot"`. Can be a numeric value or a
  function.

- dot_size_name:

  Legend title for the dot size.

- legend_items:

  A named numeric vector specifying custom legend entries for the main
  colour scale. Names become the displayed labels.

- legend_discrete:

  Logical; if `TRUE`, treat the main colour scale as discrete.

- legend.position:

  A character string specifying where to place the combined legend:
  `"right"` (default), `"left"`, `"top"`, `"bottom"`, or `"none"`.

- legend.direction:

  Legend stacking direction: `"vertical"` (default) or `"horizontal"`.

- lower_quantile, upper_quantile:

  Quantiles used for clipping the colour scale when `lower_cutoff` /
  `upper_cutoff` are `NULL`. Defaults are 0 and 0.99 respectively.

- lower_cutoff, upper_cutoff:

  Explicit cutoffs for the colour scale. Values outside the range are
  clamped (winsorized). Override `lower_quantile` / `upper_quantile`
  when set.

- add_bg:

  Logical; if `TRUE`, add a background fill behind non-tile cell types.
  Not used for `cell_type = "tile"` or `"bars"`.

- bg_alpha:

  Numeric in \\\[0, 1\]\\ for background transparency.

- keep_na, keep_empty:

  Passed through to `HeatmapAtomic`. See
  [`common_args`](https://pwwang.github.io/plotthis/reference/common_args.md)
  for details.

- add_reticle:

  Logical; if `TRUE`, draw a reticle (crosshair pattern) over the
  heatmap.

- reticle_color:

  Colour for the reticle lines.

- left_cluster_rows, right_cluster_rows:

  Logical controlling whether rows are clustered in the left / right
  heatmap. `NULL` (default) lets `HeatmapAtomic` decide based on
  `rows_orderby`/`cluster_rows`.

- cluster_columns:

  Logical; cluster columns in both heatmaps. `NULL` lets `HeatmapAtomic`
  decide.

- show_row_names, show_column_names:

  Logical; show row/column names.

- border:

  Logical; draw a border around each heatmap. Default `TRUE`.

- title:

  A character string for the overall plot title. A function can be used
  to generate a dynamic title from the default. Note that, `left_title`
  and `right_title` are used to set the title for each heatmap, and
  `title` is used to set the overall title for the combined plot.

- title_params:

  A list of parameters passed to
  [`grid::grid.text()`](https://rdrr.io/r/grid/grid.text.html) to
  control the title appearance. Default is
  `list(gp = gpar(fontsize = 14, fontface = "bold"))`.

- column_title, row_title:

  Character title displayed above the columns / beside the rows of each
  heatmap.

- na_col:

  Colour used for `NA` cells. Default `"grey85"`.

- column_names_side:

  Side for column names. Default `"bottom"`.

- row_annotation:

  A structured list specifying row annotations. See
  [`HeatmapAtomic`](https://pwwang.github.io/plotthis/reference/HeatmapAtomic.md)
  for the full specification.

- row_annotation_side:

  **Deprecated**: use `row_annotation` with the `side` sub-key instead.

- row_annotation_palette:

  **Deprecated**: use `row_annotation` with the `palette` sub-key
  instead.

- row_annotation_palcolor:

  **Deprecated**: use `row_annotation` with the `palcolor` sub-key
  instead.

- row_annotation_type:

  **Deprecated**: use `row_annotation` with the `type` sub-key instead.

- row_annotation_params:

  **Deprecated**: use `row_annotation` with the `params` sub-key
  instead.

- row_annotation_agg:

  **Deprecated**: use `row_annotation` with the `agg` sub-key instead.

- column_annotation:

  A structured list specifying column annotations. See
  [`HeatmapAtomic`](https://pwwang.github.io/plotthis/reference/HeatmapAtomic.md)
  for the full specification.

- column_annotation_side:

  **Deprecated**: use `column_annotation` with the `side` sub-key
  instead.

- column_annotation_palette:

  **Deprecated**: use `column_annotation` with the `palette` sub-key
  instead.

- column_annotation_palcolor:

  **Deprecated**: use `column_annotation` with the `palcolor` sub-key
  instead.

- column_annotation_type:

  **Deprecated**: use `column_annotation` with the `type` sub-key
  instead.

- column_annotation_params:

  **Deprecated**: use `column_annotation` with the `params` sub-key
  instead.

- column_annotation_agg:

  **Deprecated**: use `column_annotation` with the `agg` sub-key
  instead.

- links_span:

  Width (in inches) of the gap column between the two heatmaps where
  link curves are drawn. Default 0.5.

- link_width_by:

  Optional column name in `data` whose values determine the stroke width
  of each link line (e.g. interaction strength). Values are min-max
  scaled to \\\[0, 1\]\\ and multiplied by `link_width_scale`. You can
  also pass a numeric value to use a constant width for all links.

- link_width_scale:

  Numeric scaling factor applied to the normalised link intensity values
  to produce final line widths (`lwd`). Default 5.

- link_color:

  Colour of the link spline curves. Default `"grey30"`.

- link_alpha:

  Alpha transparency of link curves in \\\[0, 1\]\\. Default 0.8.

- alpha:

  Alpha transparency for heatmap cells in \\\[0, 1\]\\.

- seed:

  Random seed for reproducibility. Default 8525.

- padding:

  Padding around the heatmap in CSS order (top, right, bottom, left).
  Supports 1–4 values. Default 15 (mm).

- base_size:

  A positive numeric scalar used as a scaling factor for the overall
  heatmap size. Default 1 (no scaling). Values \> 1 enlarge all cell
  dimensions proportionally.

- aspect.ratio:

  Height-to-width ratio of a single heatmap cell. When `NULL` (default),
  sensible defaults are chosen per `cell_type` (e.g. 1 for tiles, 0.5
  for bars, 2 for violins).

- draw_opts:

  A named list of additional arguments passed to
  [`draw,HeatmapList-method`](https://rdrr.io/pkg/ComplexHeatmap/man/draw-HeatmapList-method.html).
  Internally managed arguments (`padding`, `show_heatmap_legend`, etc.)
  take precedence.

- layer_fun_callback:

  A function to add custom graphical layers on top of each heatmap cell.
  Receives `j`, `i`, `x`, `y`, `w`, `h`, `fill`, `sr`, `sc`. See
  [`Heatmap`](https://rdrr.io/pkg/ComplexHeatmap/man/Heatmap.html) for
  details.

- cell_type:

  The type of cell to render. One of `"tile"` (default), `"bars"`,
  `"label"`, `"mark"`, `"label+mark"` (or `"mark+label"`), `"dot"`,
  `"violin"`, `"boxplot"`, `"pie"`. Different cell types use different
  `cell_fun` / `layer_fun` implementations.

- cell_agg:

  A function to aggregate values within each cell when
  `cell_type = "tile"` or `"label"`. Default is
  [`mean`](https://rdrr.io/r/base/mean.html).

- ...:

  Additional arguments passed to
  [`Heatmap`](https://rdrr.io/pkg/ComplexHeatmap/man/Heatmap.html).
  Arguments prefixed with `left_` or `right_` are routed to the
  corresponding heatmap (the prefix is stripped before passing).
  Unprefixed arguments are passed to both heatmaps but can be overridden
  by a prefixed version. Notable examples:

  - `left_name`, `right_name` — legend titles for the left / right
    colour scales.

  - `left_row_gap`, `right_row_gap` — per-side row gaps.

  - `left_column_gap`, `right_column_gap` — per-side column gaps.

  - `row_names_max_width`, `column_names_max_height` — passed through to
    [`ComplexHeatmap::Heatmap`](https://rdrr.io/pkg/ComplexHeatmap/man/Heatmap.html).

## Value

A `patchwork`-wrapped grob (class `wrap_plots`) with `height` and
`width` attributes (in inches). The original `data` is stored in
`p$data`.

## Architecture

1.  **Parameter resolution** — extra arguments passed via `...` are
    split into left-specific and right-specific groups by prefix
    (`left_` vs `right_`). Each group is merged with the shared defaults
    using `%||%` fallback chains, giving the caller full per-side
    override capability.

2.  **Cell dimension pre-computation** — the internal helper
    `.get_dim_pre()` calculates exact cell width and height (in
    `"inches"` units) from `cell_type`, `aspect.ratio`, `base_size`, and
    the unique row/column counts (including split groups). These are
    passed as explicit `width` / `height` arguments to
    [`ComplexHeatmap::Heatmap()`](https://rdrr.io/pkg/ComplexHeatmap/man/Heatmap.html)
    so that cells have a guaranteed physical size rather than using
    `null` units.

3.  **Delegation to `HeatmapAtomic`** — both the left and right heatmaps
    are created by calling `do_call(HeatmapAtomic, ...)` with
    `return_ht = TRUE`. This returns the prepared `Heatmap` object (with
    `cell_w`/`cell_h` attributes) without drawing it.

4.  **Dimension extraction** — exact component heights and widths are
    read from the prepared `Heatmap` objects via
    `ComplexHeatmap:::component_height()` and
    `ComplexHeatmap:::component_width()`. These 9-element vectors break
    down every part of the heatmap (title, dendrogram, names,
    annotations, body) so the composite layout can be sized precisely.

5.  **Link table construction** — rows in the left and right heatmaps
    are matched by grouping the input data on the row identifier columns
    (plus optional `rows_split_by`). Each link's position is mapped to
    the ordered row index in its respective heatmap (respecting
    clustering order).

6.  **Legend collection** — legends from both heatmaps are collected,
    packed via
    [`ComplexHeatmap::packLegend()`](https://rdrr.io/pkg/ComplexHeatmap/man/packLegend.html),
    and placed according to `legend.position` (left, right, top, or
    bottom).

7.  **Composite drawing** — a `grid.layout` is built dynamically based
    on the legend position. Each heatmap is drawn inside a centered,
    exact-size viewport so
    [`ComplexHeatmap::draw()`](https://rdrr.io/pkg/ComplexHeatmap/man/draw-dispatch.html)
    sizes the body to the pre-computed row/column counts. Link lines are
    drawn as `grid.xspline()` curves in a dedicated gap column between
    the two heatmaps.

8.  **Dimension attributes** — the final `patchwork`-wrapped grob
    carries `height` and `width` attributes (in inches) for consistent
    rendering in downstream layouts.
