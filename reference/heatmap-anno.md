# Heatmap annotation functions

Heatmap annotation functions

## Usage

``` r
.anno_ggseries(
  x,
  split_by = NULL,
  group_by,
  column,
  title,
  which = "row",
  palette,
  palcolor = NULL,
  border = TRUE,
  legend.direction,
  show_legend = TRUE,
  .plotting,
  ...
)

anno_pie(
  x,
  split_by = NULL,
  group_by,
  column,
  title,
  which = "row",
  palette,
  palcolor = NULL,
  border = TRUE,
  legend.direction,
  show_legend = TRUE,
  ...
)

anno_ring(
  x,
  split_by = NULL,
  group_by,
  column,
  title,
  which = "row",
  palette,
  palcolor = NULL,
  border = TRUE,
  legend.direction,
  show_legend = TRUE,
  ...
)

anno_bar(
  x,
  split_by = NULL,
  group_by,
  column,
  title,
  which = "row",
  palette,
  palcolor = NULL,
  border = TRUE,
  legend.direction,
  show_legend = TRUE,
  ...
)

anno_violin(
  x,
  split_by = NULL,
  group_by,
  column,
  title,
  which = "row",
  palette,
  palcolor = NULL,
  border = TRUE,
  legend.direction,
  show_legend = TRUE,
  ...
)

anno_boxplot(
  x,
  split_by = NULL,
  group_by,
  column,
  title,
  which = "row",
  palette,
  palcolor = NULL,
  border = TRUE,
  legend.direction,
  show_legend = TRUE,
  ...
)

anno_density(
  x,
  split_by = NULL,
  group_by,
  column,
  title,
  which = "row",
  palette,
  palcolor = NULL,
  border = TRUE,
  legend.direction,
  show_legend = TRUE,
  ...
)

anno_simple(
  x,
  split_by = NULL,
  group_by = NULL,
  column = NULL,
  title,
  which = "row",
  palette,
  palcolor = NULL,
  border = TRUE,
  legend.direction,
  show_legend = TRUE,
  alpha = 1,
  ...
)

anno_points(
  x,
  split_by = NULL,
  group_by,
  column,
  title,
  which = "row",
  palette,
  palcolor = NULL,
  border = TRUE,
  legend.direction,
  show_legend = TRUE,
  alpha = 1,
  ...
)

anno_lines(
  x,
  split_by = NULL,
  group_by,
  column,
  title,
  which = "row",
  palette,
  palcolor = NULL,
  border = TRUE,
  legend.direction,
  show_legend = TRUE,
  alpha = 1,
  add_points = TRUE,
  ...
)
```

## Arguments

- x:

  A data frame

- split_by:

  A character string of the column name to split the data (heatmap)

- group_by:

  A character string of the column name to group the data (rows or
  columns of the heatmap)

- column:

  A character string of the column name of the data `x` to plot

- title:

  A character string to name the legend

- which:

  A character string specifying the direction of the annotation. Default
  is "row". Other options are "column".

- palette:

  A character string specifying the palette of the annotation

- palcolor:

  A character vector of colors to override the palette

- border:

  A logical value indicating whether to draw the border of the
  annotation

- legend.direction:

  A character string specifying the direction of the legend. Default is
  "vertical". Other options are "horizontal".

- show_legend:

  A logical value indicating whether to show the legend

- .plotting:

  A function to create the plot for each split and each group

- ...:

  Other arguments passed to
  [`ComplexHeatmap::AnnotationFunction`](https://rdrr.io/pkg/ComplexHeatmap/man/AnnotationFunction.html)
  The parameters passed to `row_annotation_params` and
  `column_annotation_params` will be passed here.

- alpha:

  A numeric value between 0 and 1 specifying the transparency of the
  annotation

- add_points:

  A logical value indicating whether to add points to the annotation
