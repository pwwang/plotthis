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
  rows_palette = "Paired",
  rows_palcolor = NULL,
  rows_split_palette = "simspec",
  rows_split_palcolor = NULL,
  columns_palette = "Paired",
  columns_palcolor = NULL,
  columns_split_palette = "simspec",
  columns_split_palcolor = NULL,
  pie_size_name = "size",
  pie_size = NULL,
  pie_values = "length",
  pie_group_by = NULL,
  pie_palette = "Spectral",
  pie_palcolor = NULL,
  bars_sample = 100,
  label = identity,
  label_size = 10,
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
  column_name_annotation = TRUE,
  column_name_legend = NULL,
  row_name_annotation = TRUE,
  row_name_legend = NULL,
  cluster_columns = TRUE,
  cluster_rows = TRUE,
  show_row_names = !row_name_annotation,
  show_column_names = !column_name_annotation,
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

- rows_palette:

  A character string specifying the palette of the row group annotation.
  The default is "Paired".

- rows_palcolor:

  A character vector of colors to override the palette of the row group
  annotation.

- rows_split_palette:

  A character string specifying the palette of the row split annotation.
  The default is "simspec".

- rows_split_palcolor:

  A character vector of colors to override the palette of the row split
  annotation.

- columns_palette:

  A character string specifying the palette of the column group
  annotation. The default is "Paired".

- columns_palcolor:

  A character vector of colors to override the palette of the column
  group annotation.

- columns_split_palette:

  A character string specifying the palette of the column split
  annotation. The default is "simspec".

- columns_split_palcolor:

  A character vector of colors to override the palette of the column
  split annotation.

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
  values. If it takes 3 arguments, the second and third arguments are
  the row and column indices. If it takes 5 arguments, the second and
  third arguments are the row and column indices, the fourth and fifth
  arguments are the row and column names. The function should return a
  character vector of the same length as the aggregated values. If the
  function returns NA, no label will be shown for that cell. For the
  indices, if you have the same dimension of data (same order of rows
  and columns) as the heatmap, you need to use
  [`ComplexHeatmap::pindex()`](https://rdrr.io/pkg/ComplexHeatmap/man/pindex.html)
  to get the correct values.

- label_size:

  A numeric value specifying the size of the labels when
  `cell_type = "label"`.

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

- column_name_annotation:

  A logical value indicating whether to add the column annotation for
  the column names. which is a simple annotaion indicating the column
  names.

- column_name_legend:

  A logical value indicating whether to show the legend of the column
  name annotation.

- row_name_annotation:

  A logical value indicating whether to add the row annotation for the
  row names. which is a simple annotaion indicating the row names.

- row_name_legend:

  A logical value indicating whether to show the legend of the row name
  annotation.

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
  the parameters passed to the annotation function. For the parameters
  for names (columns_by, rows_by, columns_split_by, rows_split_by), the
  key should be "name.(name)", where `(name)` is the name of the
  annotation. See
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
  the parameters. Same as `column_annotation_params`.

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

- layer_fun_callback:

  A function to add additional layers to the heatmap. The function
  should have the following arguments: `j`, `i`, `x`, `y`, `w`, `h`,
  `fill`, `sr` and `sc`. Please also refer to the `layer_fun` argument
  in
  [`ComplexHeatmap::Heatmap`](https://rdrr.io/pkg/ComplexHeatmap/man/Heatmap.html).

- cell_type:

  A character string specifying the type of the heatmap cells. The
  default is values. Other options are "bars", "label", "dot", "violin",
  "boxplot". Note that for pie chart, the values under columns specified
  by `rows` will not be used directly. Instead, the values will just be
  counted in different `pie_group_by` groups. `NA` values will not be
  counted.

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
