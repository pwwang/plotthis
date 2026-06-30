# Atomic heatmap (internal)

Core implementation for drawing a single heatmap using
[`ComplexHeatmap::Heatmap`](https://rdrr.io/pkg/ComplexHeatmap/man/Heatmap.html).
This function takes pre-processed data (already in long format with
resolved row/column identifiers) and handles colour mapping, cell
rendering, annotation construction, and the final `draw()` call. It is
the workhorse behind the exported
[`Heatmap`](https://pwwang.github.io/plotthis/reference/Heatmap.md)
function and is also reused by
[`LinkedHeatmapAtomic`](https://pwwang.github.io/plotthis/reference/LinkedHeatmapAtomic.md)
(via `return_ht = TRUE`).

## Usage

``` r
HeatmapAtomic(
  data,
  values_by,
  values_fill = NA,
  rows_by = NULL,
  rows_split_by = NULL,
  columns_by = NULL,
  columns_split_by = NULL,
  palette = "RdBu",
  palcolor = NULL,
  palreverse = FALSE,
  pie_size_name = "size",
  pie_size = NULL,
  pie_values = "length",
  pie_group_by = NULL,
  pie_palette = "Spectral",
  pie_palcolor = NULL,
  bars_sample = 100,
  label = scales::label_number_auto(),
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
  cluster_columns = TRUE,
  cluster_rows = TRUE,
  show_row_names = NULL,
  show_column_names = NULL,
  border = TRUE,
  title = NULL,
  column_title = NULL,
  row_title = NULL,
  na_col = "grey85",
  row_names_side = "right",
  column_names_side = "bottom",
  column_annotation = NULL,
  column_annotation_side = "top",
  column_annotation_palette = "Paired",
  column_annotation_palcolor = NULL,
  column_annotation_type = "auto",
  column_annotation_params = list(),
  column_annotation_agg = NULL,
  row_annotation = NULL,
  row_annotation_side = "left",
  row_annotation_palette = "Paired",
  row_annotation_palcolor = NULL,
  row_annotation_type = "auto",
  row_annotation_params = list(),
  row_annotation_agg = NULL,
  flip = FALSE,
  alpha = 1,
  seed = 8525,
  padding = 15,
  base_size = 1,
  aspect.ratio = NULL,
  draw_opts = list(),
  return_ht = FALSE,
  layer_fun_callback = NULL,
  cell_type = "tile",
  cell_agg = NULL,
  ...
)
```

## Arguments

- data:

  A data frame in long format. Each row represents one observation;
  columns specify row/column membership and the value to encode as
  colour.

- values_by:

  A character of column name in `data` that contains the values to be
  plotted. This is required when `in_form` is `"long"`. For other
  formats, the values are pivoted into a column named by `values_by`.

- values_fill:

  A value used to fill missing cells in the matrix. Default `NA`.
  Missing values prevent clustering when not filled.

- rows_by:

  A vector of column names in `data` that contains the row information.
  This is used to create the rows of the heatmap. When `in_form` is
  `"long"` or `"wide-columns"`, this is requied, and multiple columns
  can be specified, which will be concatenated by `rows_by_sep` into a
  single column.

- rows_split_by:

  A character of column name in `data` that contains the split
  information for rows.

- columns_by:

  A vector of column names in `data` that contains the column
  information. This is used to create the columns of the heatmap. When
  `in_form` is `"long"` or `"wide-rows"`, this is required, and multiple
  columns can be specified, which will be concatenated by
  `columns_by_sep` into a single column.

- columns_split_by:

  A character of column name in `data` that contains the split
  information for columns.

- palette:

  A character string naming a palette (see
  [`show_palettes`](https://pwwang.github.io/plotthis/reference/show_palettes.md))
  or a character vector of colours for the main heatmap colour scale.
  Default `"RdBu"`.

- palcolor:

  A custom colour vector overriding `palette`.

- palreverse:

  A logical value indicating whether to reverse the palette. Default is
  FALSE.

- pie_size_name:

  Legend title for the pie size.

- pie_size:

  A numeric value or function returning the pie radius. When a function,
  it receives the count of groups in the pie.

- pie_values:

  A function or string (convertible via
  [`match.arg`](https://rdrr.io/r/base/match.arg.html)) to compute the
  value represented by each pie slice. Default `"length"` counts
  observations per group.

- pie_group_by:

  A character of column name in `data` that contains the group
  information for pie charts. This is used to create pie charts in the
  heatmap when `cell_type` is `"pie"`.

- pie_palette, pie_palcolor:

  Palette and custom colours for pie slice fill colours.

- bars_sample:

  Number of observations sampled per cell when `cell_type = "bars"`.
  Default 100.

- label:

  A function to compute text labels when `cell_type = "label"` (or
  `"label+mark"`). Receives the aggregated value for a cell and
  optionally row/column indices and names. See below for the full
  dispatch contract.

- label_size:

  Default point size for label text (used as fallback when the `label`
  function does not return a `size` field).

- label_color:

  Default colour for label text (fallback).

- label_name:

  Legend title for the label colour scale. The legend is shown
  automatically when the `label` function returns a `legend` field for
  at least one cell.

- mark:

  A function to compute mark symbols when `cell_type = "mark"` (or
  `"label+mark"`). Same dispatch contract as `label`.

- mark_color:

  Default mark colour (fallback).

- mark_size:

  Default mark stroke width (`lwd`) in pt (fallback).

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

  A character string specifying the position of the legend. if
  `waiver()`, for single groups, the legend will be "none", otherwise
  "right".

- legend.direction:

  A character string specifying the direction of the legend.

- lower_quantile, upper_quantile, lower_cutoff, upper_cutoff:

  Quantile or explicit cutoffs for clipping the colour scale. Applied to
  aggregated values for `tile` / `label` cell types; applied to raw
  values for `bars` / `violin` / `boxplot` types.

- add_bg:

  Logical; if `TRUE`, add a background fill behind non-tile cell types.
  Not used for `cell_type = "tile"` or `"bars"`.

- bg_alpha:

  Numeric in \\\[0, 1\]\\ for background transparency.

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

- add_reticle:

  Logical; if `TRUE`, draw a reticle (crosshair pattern) over the
  heatmap.

- reticle_color:

  Colour for the reticle lines.

- cluster_columns:

  Logical; cluster the columns. If `TRUE` and `columns_split_by` is
  provided, clustering is applied within each split group.

- cluster_rows:

  Logical; cluster the rows. If `TRUE` and `rows_split_by` is provided,
  clustering is applied within each split group.

- show_row_names:

  Logical; show row names. If `TRUE`, the legend of the row group
  annotation is hidden.

- show_column_names:

  Logical; show column names. If `TRUE`, the legend of the column group
  annotation is hidden.

- border:

  A logical value indicating whether to draw borders around the heatmap.
  If `TRUE`, slice borders are also drawn. Default `TRUE`.

- title:

  The global (column) title of the heatmap.

- column_title:

  Character string/vector used as the column group annotation title.

- row_title:

  Character string/vector used as the row group annotation title.

- na_col:

  Colour for `NA` cells. Default `"grey85"`.

- row_names_side:

  Side for row names. Default `"right"`.

- column_names_side:

  Side for column names. Default `"bottom"`.

- column_annotation:

  A character vector of column names, or a named list, specifying column
  annotations. See the **Annotations** section for the full
  specification.

- column_annotation_side:

  A character string or named list specifying which side each column
  annotation is placed on. Accepts `"top"` (default) or `"bottom"`. With
  a named list, use keys `.col`, `.col.split`, and `.default` for
  per-annotation control.

- column_annotation_palette, column_annotation_palcolor:

  Palette and custom colours for column annotations. Can be a named list
  keyed by annotation name.

- column_annotation_type:

  Annotation type: `"auto"` (default), `"simple"`, `"pie"`, `"ring"`,
  `"bar"`, `"violin"`, `"boxplot"`, `"density"`, `"label"`, `"points"`,
  `"lines"`. Can be a named list for per-annotation control. Aliases:
  `.col.split`, `.col`.

- column_annotation_params:

  A named list of additional parameters passed to each column annotation
  function. Use aliases `.col`/`.cols` for `columns_by` and
  `.col.split`/`.cols.split` for `columns_split_by`. Setting a key to
  `FALSE` disables that annotation; `$<key>$show_legend` controls its
  legend visibility. See
  [`HeatmapAnnotation`](https://rdrr.io/pkg/ComplexHeatmap/man/HeatmapAnnotation.html)
  for details.

- column_annotation_agg:

  A function or named list of functions to aggregate values for each
  column annotation. Defaults vary by annotation type.

- row_annotation, row_annotation_side, row_annotation_palette,
  row_annotation_palcolor, row_annotation_type, row_annotation_params,
  row_annotation_agg:

  Row annotation equivalents of the `column_annotation_*` parameters.
  Sides default to `"left"`. Aliases: `.row` /`.rows` for `rows_by`,
  `.rows.split`/`.row.split` for `rows_split_by`.

- flip:

  Logical; if `TRUE`, swap rows and columns transparently. The caller
  does not need to swap row- and column-related arguments manually.

- alpha:

  Alpha transparency for heatmap cells in \\\[0, 1\]\\.

- seed:

  The random seed to use. Default is 8525.

- padding:

  Padding around the heatmap in CSS order (top, right, bottom, left).
  Supports 1–4 values. Default 15 (mm). Note that this is different from
  [`ComplexHeatmap::draw()`](https://rdrr.io/pkg/ComplexHeatmap/man/draw-dispatch.html)'s
  `padding` argument which uses bottom-left-top-right order.

- base_size:

  A positive numeric scalar used as a scaling factor for the overall
  heatmap size. Default 1 (no scaling). Values \> 1 enlarge all cell
  dimensions proportionally.

- aspect.ratio:

  Height-to-width ratio of a single heatmap cell. When `NULL` (default),
  sensible per-`cell_type` defaults are used: 1 for
  `tile`/`label`/`dot`, 0.5 for `bars`, and 2 for
  `violin`/`boxplot`/`pie`. The ratio is constrained by the overall plot
  dimensions.

- draw_opts:

  A named list of additional arguments passed to
  [`draw,HeatmapList-method`](https://rdrr.io/pkg/ComplexHeatmap/man/draw-HeatmapList-method.html).
  Internally managed arguments take precedence.

- return_ht:

  Logical; if `TRUE`, return the prepared
  [`ComplexHeatmap::Heatmap`](https://rdrr.io/pkg/ComplexHeatmap/man/Heatmap.html)
  object without drawing. Used internally by
  [`LinkedHeatmapAtomic`](https://pwwang.github.io/plotthis/reference/LinkedHeatmapAtomic.md).

- layer_fun_callback:

  A function to add custom graphical layers on top of each heatmap cell.
  Receives `j`, `i`, `x`, `y`, `w`, `h`, `fill`, `sr`, `sc`. See
  [`Heatmap`](https://rdrr.io/pkg/ComplexHeatmap/man/Heatmap.html) for
  details.

- cell_type:

  The type of cell to render. One of `"tile"` (default), `"bars"`,
  `"label"`, `"mark"`, `"label+mark"` (or `"mark+label"`), `"dot"`,
  `"violin"`, `"boxplot"`, `"pie"`. See the **Cell types** section for
  details.

- cell_agg:

  A function to aggregate values within each cell when
  `cell_type = "tile"` or `"label"`. Default is
  [`mean`](https://rdrr.io/r/base/mean.html).

- ...:

  Additional arguments passed to
  [`Heatmap`](https://rdrr.io/pkg/ComplexHeatmap/man/Heatmap.html). When
  `row_names_max_width` is passed, a `unit` is expected but numeric
  values (default unit `"inches"`) or strings like `"5inches"` are also
  accepted. Unmatched arguments produce a warning.

## Value

An object of heatmap that is wrapped by
[`patchwork::wrap_plots()`](https://patchwork.data-imaginist.com/reference/wrap_plots.html)

## Note

Removed parameters: `rows_palette`, `rows_palcolor`, `columns_palette`,
`columns_palcolor`, `columns_split_palette`, `columns_split_palcolor`,
`rows_split_palette`, `rows_split_palcolor` — use
`row_annotation_palette`/`row_annotation_palcolor` with key `.row` or
`.rows.split`, and
`column_annotation_palette`/`column_annotation_palcolor` with `.col` or
`.col.split`. Also removed: `row_name_annotation`, `row_name_legend`,
`column_name_annotation`, `column_name_legend` — set
`row_annotation_params$.row` / `column_annotation_params$.col` to
`FALSE` and use `$show_legend` within the param entry.

## Architecture

1.  **Annotation resolution** — row and column annotation parameters
    (side, type, palette, params, aggregation) are resolved with alias
    support (`.row`, `.col`, `.rows.split`, etc.) so that built-in
    name/split annotations and user-defined annotations can be
    configured uniformly.

2.  **Keep-NA / keep-empty handling** — `keep_na` and `keep_empty` are
    processed per column to control whether `NA` values and empty factor
    levels are preserved in the data before constructing the heatmap
    matrix.

3.  **Flip** — when `flip = TRUE`, row and column parameters are swapped
    transparently so the caller does not need to manually swap every
    argument.

4.  **Cell dimension calculation** — cell width and height are
    pre-computed from `cell_type`, `aspect.ratio`, `base_size`, and the
    unique row/column counts (accounting for split groups). These are
    passed as explicit `"inches"` units to
    [`ComplexHeatmap::Heatmap()`](https://rdrr.io/pkg/ComplexHeatmap/man/Heatmap.html)
    so cells have guaranteed physical sizes.

5.  **Colour mapping** — the main colour scale is built from `palette` /
    `palcolor` with optional quantile-based or explicit cutoff clamping.

6.  **Cell function dispatch** — depending on `cell_type`, the
    appropriate `cell_fun` or `layer_fun` is installed (tile, bars,
    label, mark, label+mark, dot, violin, boxplot, pie).

7.  **Annotation construction** — row and column annotations are built
    via
    [`ComplexHeatmap::HeatmapAnnotation()`](https://rdrr.io/pkg/ComplexHeatmap/man/HeatmapAnnotation.html)
    using the resolved type and parameters.

8.  **Drawing** — the heatmap is drawn via
    [`ComplexHeatmap::draw()`](https://rdrr.io/pkg/ComplexHeatmap/man/draw-dispatch.html)
    with controlled padding and legend collection. When
    `return_ht = TRUE`, the prepared `Heatmap` object is returned
    without drawing.

9.  **Dimension attributes** — the returned object carries `height` /
    `width` attributes (in inches) and `cell_w` / `cell_h` attributes
    for downstream layout calculations.

## Label / mark dispatch contract

The `label` and `mark` functions can take 1, 3, or 5 arguments. The
first argument is always the aggregated value for a single cell. With 3
arguments, the second and third are the row and column indices. With 5
arguments, the fourth and fifth are the row and column names. The
function should return one of:

- `NA` — nothing is drawn for this cell.

- A character scalar — used as the label text / mark type; default size
  and colour are used.

- A named list with fields `label`/`mark`, `size`, `color`, `legend`,
  and `order` (all optional; smaller `order` appears first in the
  legend).

For label cells with custom indices, use
[`ComplexHeatmap::pindex()`](https://rdrr.io/pkg/ComplexHeatmap/man/pindex.html)
to translate between data and heatmap coordinates.

## Supported mark types

- **Primitives:** `-` (h-line), `|` (v-line), `+` (cross), `/` (l-diag),
  `\` (r-diag), `x` (both diags), `o` (circle), `()` (edge-touching
  circle), `<>` (diamond).

- **With rectangular border:** `[]`, `[-]`, `[|]`, `[+]`, `[/]`, `[\]`,
  `[x]`, `[o]`, `[()]`, `[<>]`.

- **With full circle:** `(-)`, `(|)`, `(+)`, `(/)`, `(\)`, `(x)`, `(o)`,
  `(<>)`.

- **With diamond:** `<->`, `<|>`, `<+>`, `</>`, `<\>`, `<x>`, `<o>`.

- **Octagon:** [`{}`](https://rdrr.io/r/base/Paren.html), `{-}`, `{|}`,
  `{+}`, `{/}`, `{\}`, `{x}`, `{o}`, `{()}`, `{<>}`.

- **Combinations:** e.g. `[(|)]`, `[(-)]`, `[(+)]`, `[(/)]`, `[(\]`,
  `[(x)]`, `[(o)]`, `[(<>)]`.

## Annotations

Row and column annotations are built via
[`HeatmapAnnotation`](https://rdrr.io/pkg/ComplexHeatmap/man/HeatmapAnnotation.html).
Built-in name annotations (for `rows_by` / `columns_by`) and split
annotations (for `rows_split_by` / `columns_split_by`) are added
automatically and can be configured using the aliases `.row` / `.col`
and `.rows.split` / `.col.split` in `row_annotation_params`,
`column_annotation_type`, etc. Setting an alias to `FALSE` in `*_params`
disables that annotation. Ordering within each side: name annotations
are closest to the body, split annotations farthest away, user-defined
annotations in between.
