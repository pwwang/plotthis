# Atomic heatmap without split

Atomic heatmap without split

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
  column_title = character(0),
  row_title = character(0),
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
  return_grob = FALSE,
  padding = 15,
  base_size = 1,
  aspect.ratio = NULL,
  draw_opts = list(),
  layer_fun_callback = NULL,
  cell_type = "tile",
  cell_agg = NULL,
  ...
)
```

## Arguments

- data:

  A data frame used to create the heatmap. The data should be in a long
  form where each row represents a instance in the heatmap.

- values_by:

  A character of column name in `data` that contains the values to be
  plotted. This is required when `in_form` is `"long"`. For other
  formats, the values are pivoted into a column named by `values_by`.

- values_fill:

  A value to fill in the missing values in the heatmap. When there is
  missing value in the data, the cluster_rows and cluster_columns will
  fail.

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

  A character string specifying the palette of the heatmap cells.

- palcolor:

  A character vector of colors to override the palette of the heatmap
  cells.

- palreverse:

  A logical value indicating whether to reverse the palette. Default is
  FALSE.

- pie_size_name:

  A character string specifying the name of the legend for the pie size.

- pie_size:

  A numeric value or a function specifying the size of the pie chart. If
  it is a function, the function should take `count` as the argument and
  return the size.

- pie_values:

  A function or character that can be converted to a function by
  [`match.arg()`](https://rdrr.io/r/base/match.arg.html) to calculate
  the values for the pie chart. Default is "length". The function should
  take a vector of values as the argument and return a single value, for
  each group in `pie_group_by`.

- pie_group_by:

  A character of column name in `data` that contains the group
  information for pie charts. This is used to create pie charts in the
  heatmap when `cell_type` is `"pie"`.

- pie_palette:

  A character string specifying the palette of the pie chart.

- pie_palcolor:

  A character vector of colors to override the palette of the pie chart.

- bars_sample:

  An integer specifying the number of samples to draw the bars.

- label:

  A function to calculate the labels for the heatmap cells. It can take
  either 1, 3, or 5 arguments. The first argument is the aggregated
  value for a single cell. If it takes 3 arguments, the second and third
  arguments are the row and column indices of that cell. If it takes 5
  arguments, the second and third arguments are the row and column
  indices, and the fourth and fifth arguments are the row and column
  names. The function should return one of:

  - `NA` — no label is drawn for this cell.

  - A character scalar — used as the label text; `label_size` and
    `label_color` are used for size and color.

  - A named list with any of the following fields:

    - `label`: character scalar for the label text.

    - `size`: numeric pt size (overrides `label_size`).

    - `color`: character color string (overrides `label_color`).

    - `legend`: character string used as the legend entry for this
      cell's color/label combination.

    - `order`: integer controlling the position of this legend entry —
      smaller values appear first (top) in the legend. Entries without
      an `order` are appended after all explicitly ordered entries. For
      the indices, if you have the same dimension of data (same order of
      rows and columns) as the heatmap, you need to use
      [`ComplexHeatmap::pindex()`](https://rdrr.io/pkg/ComplexHeatmap/man/pindex.html)
      to get the correct values.

- label_size:

  A numeric value specifying the default size (pt) of the labels when
  `cell_type = "label"`. Used as fallback when the `label` function does
  not return a `size` field.

- label_color:

  A character string specifying the default color of the labels when
  `cell_type = "label"`. Used as fallback when the `label` function does
  not return a `color` field. Default is `"black"`.

- label_name:

  A character string specifying the title of the label legend. Default
  is `"label"`. The legend is shown automatically when the `label`
  function returns a list with a `legend` field for at least one cell —
  no extra configuration needed. Set `legend.position = "none"` to
  suppress all legends.

- mark:

  A function to calculate the marks drawn on top of heatmap cells when
  `cell_type = "mark"`. Same dispatch rules as `label` (1, 3, or 5
  arguments). The function should return one of:

  - `NA` — no mark is drawn for this cell.

  - A character scalar — the mark type string; `mark_color` and
    `mark_size` are used for appearance.

  - A named list with any of the following fields:

    - `mark` (or first unnamed element): character scalar, the mark type
      string.

    - `size`: numeric stroke width (lwd), overrides `mark_size`.

    - `color`: character color string, overrides `mark_color`.

    - `legend`: character string used as the legend entry key.

    - `order`: integer controlling legend entry position (smaller =
      higher). **Supported mark types:**

  - Primitives: `-` (h-line), `|` (v-line), `+` (cross), `/` (l-diag),
    `\` (r-diag), `x` (both diags), `o` (circle with gap), `()` (circle
    touching edge), `<>` (diamond).

  - With rectangular border: `[]`, `[-]`, `[|]`, `[+]`, `[/]`, `[\]`,
    `[x]`, `[o]`, `[()]`, `[<>]`.

  - With full circle: `(-)`, `(|)`, `(+)`, `(/)`, `(\)`, `(x)`, `(o)`,
    `(<>)`.

  - With diamond: `<->`, `<|>`, `<+>`, `</>`, `<\>`, `<x>`, `<o>`.

  - Octagon (standalone or wrapper):
    [`{}`](https://rdrr.io/r/base/Paren.html), `{-}`, `{|}`, `{+}`,
    `{/}`, `{\\}`, `{x}`, `{o}`, `{()}`, `{<>}`.

  - Combinations: e.g. `[(|)]`, `[(-)]`, `[(+)]`, `[(/)]`, `[(\)]`,
    `[(x)]`, `[(o)]`, `[(<>)]`.

  \[\]: R:%5C \[x\]: R:x \[o\]: R:o \[()\]: R:() \[\<\>\]: R:%3C%3E
  \[(\|)\]: R:(%7C) \[(-)\]: R:(-) \[(+)\]: R:(+) \[(/)\]: R:(/)
  \[(\\\]: R:(%5C%5C) \[(x)\]: R:(x) \[(o)\]: R:(o) \[(\<\>)\]:
  R:(%3C%3E)

- mark_color:

  A character string specifying the default color of the marks when
  `cell_type = "mark"`. Used as fallback when the `mark` function does
  not return a `color` field. Default is `"black"`.

- mark_size:

  A numeric value specifying the default stroke width (lwd) of the marks
  when `cell_type = "mark"`. Used as fallback when the `mark` function
  does not return a `size` field. Default is `1`.

- mark_name:

  A character string specifying the title of the mark legend. Default is
  `"mark"`. The legend is shown automatically when the `mark` function
  returns a list with a `legend` field.

- violin_fill:

  A character vector of colors to override the fill color of the violin
  plot. If NULL, the fill color will be the same as the annotion.

- boxplot_fill:

  A character vector of colors to override the fill color of the
  boxplot. If NULL, the fill color will be the same as the annotion.

- dot_size:

  A numeric value specifying the size of the dot or a function to
  calculate the size from the values in the cell or a function to
  calculate the size from the values in the cell.

- dot_size_name:

  A character string specifying the name of the legend for the dot size.
  If NULL, the dot size legend will not be shown.

- legend_items:

  A numeric vector with names to specifiy the items in the main legend.
  The names will be working as the labels of the legend items.

- legend_discrete:

  A logical value indicating whether the main legend is discrete.

- legend.position:

  A character string specifying the position of the legend. if
  `waiver()`, for single groups, the legend will be "none", otherwise
  "right".

- legend.direction:

  A character string specifying the direction of the legend.

- lower_quantile, upper_quantile, lower_cutoff, upper_cutoff:

  Vector of minimum and maximum cutoff values or quantile values for
  each feature. It's applied to aggregated values when aggregated values
  are used (e.g. plot_type tile, label, etc). It's applied to raw values
  when raw values are used (e.g. plot_type bars, etc).

- add_bg:

  A logical value indicating whether to add a background to the heatmap.
  Does not work with `cell_type = "bars"` or `cell_type = "tile"`.

- bg_alpha:

  A numeric value between 0 and 1 specifying the transparency of the
  background.

- keep_na:

  Whether we should keep NA groups in rows, columns and split_by
  variables. Default is FALSE. FALSE to remove NA groups; TRUE to keep
  NA groups. A vector of column names can also be provided to specify
  which columns to keep NA groups. Note that the record will be removed
  if any of the grouping columns has NA and is not specified to keep NA.

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

  A logical value indicating whether to add a reticle to the heatmap.

- reticle_color:

  A character string specifying the color of the reticle.

- cluster_columns:

  A logical value indicating whether to cluster the columns. If TRUE and
  columns_split_by is provided, the clustering will only be applied to
  the columns within the same split.

- cluster_rows:

  A logical value indicating whether to cluster the rows. If TRUE and
  rows_split_by is provided, the clustering will only be applied to the
  rows within the same split.

- show_row_names:

  A logical value indicating whether to show the row names. If TRUE, the
  legend of the row group annotation will be hidden.

- show_column_names:

  A logical value indicating whether to show the column names. If TRUE,
  the legend of the column group annotation will be hidden.

- border:

  A logical value indicating whether to draw the border of the heatmap.
  If TRUE, the borders of the slices will be also drawn.

- title:

  The global (column) title of the heatmap

- column_title:

  A character string/vector of the column name(s) to use as the title of
  the column group annotation.

- row_title:

  A character string/vector of the column name(s) to use as the title of
  the row group annotation.

- na_col:

  A character string specifying the color for missing values. The
  default is "grey85".

- row_names_side:

  A character string specifying the side of the row names. The default
  is "right".

- column_names_side:

  A character string specifying the side of the column names. The
  default is "bottom".

- column_annotation:

  A character string/vector of the column name(s) to use as the column
  annotation. Or a list with the keys as the names of the annotation and
  the values as the column names.

- column_annotation_side:

  A character string specifying the side of the column annotation. Could
  be a list with the keys as the names of the annotation and the values
  as the sides.

- column_annotation_palette:

  A character string specifying the palette of the column annotation.
  The default is "Paired". Could be a list with the keys as the names of
  the annotation and the values as the palettes.

- column_annotation_palcolor:

  A character vector of colors to override the palette of the column
  annotation. Could be a list with the keys as the names of the
  annotation and the values as the palcolors.

- column_annotation_type:

  A character string specifying the type of the column annotation. The
  default is "auto". Other options are "simple", "pie", "ring", "bar",
  "violin", "boxplot", "density". Could be a list with the keys as the
  names of the annotation and the values as the types. If the type is
  "auto", the type will be determined by the type and number of the
  column data.

- column_annotation_params:

  A list of parameters passed to the annotation function. Could be a
  list with the keys as the names of the annotation and the values as
  the parameters. For the name/split annotations, use aliases:
  `.col`/`.cols`/`.column`/`.columns` for `columns_by`,
  `.col.split`/`.cols.split`/`.column.split`/`.columns.split` for
  `columns_split_by`. Setting a key to `FALSE` disables that annotation.
  `$<key>$show_legend` controls the legend for that annotation. See
  [`anno_pie()`](https://pwwang.github.io/plotthis/reference/heatmap-anno.md),
  [`anno_ring()`](https://pwwang.github.io/plotthis/reference/heatmap-anno.md),
  [`anno_bar()`](https://pwwang.github.io/plotthis/reference/heatmap-anno.md),
  [`anno_violin()`](https://pwwang.github.io/plotthis/reference/heatmap-anno.md),
  [`anno_boxplot()`](https://pwwang.github.io/plotthis/reference/heatmap-anno.md),
  [`anno_density()`](https://pwwang.github.io/plotthis/reference/heatmap-anno.md),
  [`anno_simple()`](https://pwwang.github.io/plotthis/reference/heatmap-anno.md),
  [`anno_points()`](https://pwwang.github.io/plotthis/reference/heatmap-anno.md)
  and
  [`anno_lines()`](https://pwwang.github.io/plotthis/reference/heatmap-anno.md)
  for the parameters of each annotation function.

- column_annotation_agg:

  A function to aggregate the values in the column annotation.

- row_annotation:

  A character string/vector of the column name(s) to use as the row
  annotation. Or a list with the keys as the names of the annotation and
  the values as the column names.

- row_annotation_side:

  A character string specifying the side of the row annotation. Could be
  a list with the keys as the names of the annotation and the values as
  the sides.

- row_annotation_palette:

  A character string specifying the palette of the row annotation. The
  default is "Paired". Could be a list with the keys as the names of the
  annotation and the values as the palettes.

- row_annotation_palcolor:

  A character vector of colors to override the palette of the row
  annotation. Could be a list with the keys as the names of the
  annotation and the values as the palcolors.

- row_annotation_type:

  A character string specifying the type of the row annotation. The
  default is "auto". Other options are "simple", "pie", "ring", "bar",
  "violin", "boxplot", "density". Could be a list with the keys as the
  names of the annotation and the values as the types. If the type is
  "auto", the type will be determined by the type and number of the row
  data.

- row_annotation_params:

  A list of parameters passed to the annotation function. Could be a
  list with the keys as the names of the annotation and the values as
  the parameters. For the name/split annotations, use aliases:
  `.row`/`.rows` for `rows_by`, `.rows.split`/`.row.split` for
  `rows_split_by`. Setting a key to `FALSE` disables that annotation.
  `$<key>$show_legend` controls the legend. Same structure as
  `column_annotation_params`.

- row_annotation_agg:

  A function to aggregate the values in the row annotation.

- flip:

  A logical value indicating whether to flip the heatmap. The idea is
  that, you can simply set `flip = TRUE` to flip the heatmap. You don't
  need to swap the arguments related to rows and columns, except those
  you specify via `...` that are passed to
  [`ComplexHeatmap::Heatmap()`](https://rdrr.io/pkg/ComplexHeatmap/man/Heatmap.html)
  directly.

- alpha:

  A numeric value between 0 and 1 specifying the transparency of the
  heatmap cells.

- seed:

  The random seed to use. Default is 8525.

- return_grob:

  A logical value indicating whether to return the grob object of the
  heatmap. This is useful when merging multiple heatmaps using
  patchwork.

- padding:

  A numeric vector of length 4 specifying the padding of the heatmap in
  the order of top, right, bottom, left. Like padding in css. Note that
  it is different than the `padding` argument in
  [`ComplexHeatmap::draw()`](https://rdrr.io/pkg/ComplexHeatmap/man/draw-dispatch.html),
  which is the padding in the order of bottom, left, top, right. It also
  support 1, 2, 3 values like css padding. When 1 element is provided,
  it will be used for all sides. When 2 elements are provided, the first
  one will be used for top and bottom, and the second one will be used
  for left and right. When 3 elements are provided, the first one will
  be used for top, the second one will be used for left and right, and
  the third one will be used for bottom. When 4 elements are provided,
  they will be used for top, right, bottom, and left respectively. If no
  unit is provided, the default unit will be "mm".

- base_size:

  A positive numeric scalar used as a scaling factor for the overall
  heatmap size. Default is `1` (no scaling). Values greater than 1
  enlarge the heatmap; values less than 1 shrink it. Internally, all
  calculated cell dimensions are multiplied by this factor.

- aspect.ratio:

  A positive numeric scalar giving the height-to-width ratio of a single
  heatmap cell. When `NULL` (default), sensible per-`cell_type` defaults
  are used:

  - `tile`, `label`, `dot`: square cells (ratio = 1).

  - `bars`: wider-than-tall cells (ratio = 0.5) so individual bars are
    legible.

  - `violin`, `boxplot`, `pie`: square cells with a larger base size
    (0.5 in) so embedded sub-plots have enough room. Provide an explicit
    value to override these defaults (e.g. `aspect.ratio = 2` for
    portrait cells, `aspect.ratio = 0.5` for landscape cells). Note that
    for `cell_type = "pie"` the cells are always drawn square by
    ComplexHeatmap regardless of this setting; use it primarily to
    budget the figure size. Note that the aspect ratio is not guaranteed
    to be perfectly preserved; it will also be restricted by the size
    and height/width ratio of the entire plot itself.

- draw_opts:

  A named list of additional arguments passed to
  [`ComplexHeatmap::draw()`](https://rdrr.io/pkg/ComplexHeatmap/man/draw-dispatch.html).
  Arguments already managed internally (`annotation_legend_list`,
  `padding`, `show_annotation_legend`, `annotation_legend_side`,
  `column_title`) take precedence over any values supplied here. See
  <https://jokergoo.github.io/ComplexHeatmap/reference/draw-HeatmapList-method.html>
  for available options.

- layer_fun_callback:

  A function to add additional layers to the heatmap. The function
  should have the following arguments: `j`, `i`, `x`, `y`, `w`, `h`,
  `fill`, `sr` and `sc`. Please also refer to the `layer_fun` argument
  in
  [`ComplexHeatmap::Heatmap`](https://rdrr.io/pkg/ComplexHeatmap/man/Heatmap.html).

- cell_type:

  A character string specifying the type of the heatmap cells. The
  default is "tile" Other options are "bars", "label", "mark",
  "label+mark" (or equivalently "mark+label"), "dot", "violin",
  "boxplot" and "pie". Use "label+mark" to render both marks (drawn
  first, as background) and text labels (drawn on top) in each cell
  simultaneously, combining all `label_*` and `mark_*` parameters. Note
  that for pie chart, the values under columns specified by `rows` will
  not be used directly. Instead, the values will just be counted in
  different `pie_group_by` groups. `NA` values will not be counted.

- cell_agg:

  A function to aggregate the values in the cell, for the cell type
  "tile" and "label". The default is `mean`.

- ...:

  Other arguments passed to
  [`ComplexHeatmap::Heatmap()`](https://rdrr.io/pkg/ComplexHeatmap/man/Heatmap.html)
  When `row_names_max_width` is passed, a unit is expected. But you can
  also pass a numeric values, with a default unit "inches", or a string
  like "5inches" to specify the number and unit directly. Unmatched
  arguments will be warned and ignored.

## Value

A drawn HeatmapList object if `return_grob = FALSE`. Otherwise, a
grob/gTree object.

## Note

Removed parameters: `rows_palette`, `rows_palcolor`, `columns_palette`,
`columns_palcolor`, `columns_split_palette`, `columns_split_palcolor`,
`rows_split_palette`, `rows_split_palcolor` — use
`row_annotation_palette`/`row_annotation_palcolor` with key `.row`
(alias for `rows_by`) or `.rows.split` (alias for `rows_split_by`);
similarly `column_annotation_palette`/ `column_annotation_palcolor` with
`.col`/`.column` or `.col.split`/`.column.split`. Also removed:
`row_name_annotation`, `row_name_legend`, `column_name_annotation`,
`column_name_legend` — set `row_annotation_params$.row` to `FALSE` to
disable the row name annotation; use `$show_legend` inside the param
entry to control legend visibility. Use `column_annotation_params$.col`
similarly.
