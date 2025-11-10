# Heatmap annotation function for categorical data

Heatmap annotation function for categorical data

## Usage

``` r
.anno_ggcat(
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
```

## Arguments

- x:

  A data frame

- split_by:

  A character string of the column name to split the data

- group_by:

  A character string of the column name to group the data

- column:

  A character string of the column name to plot

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
