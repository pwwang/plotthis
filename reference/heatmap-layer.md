# Heatmap layer functions used to draw on the heatmap cells

Heatmap layer functions used to draw on the heatmap cells

## Usage

``` r
layer_white_bg(j, i, x, y, w, h, fill)

layer_bg(j, i, x, y, w, h, fill, alpha)

layer_reticle(j, i, x, y, w, h, fill, color)

layer_dot(
  j,
  i,
  x,
  y,
  w,
  h,
  fill,
  data,
  dot_size,
  alpha,
  row_names = NULL,
  col_names = NULL
)

layer_bars(j, i, x, y, w, h, fill, flip, col_fun, data, alpha)

layer_pie(j, i, x, y, w, h, fill, palette, palcolor, data, pie_size)

layer_boxviolin(j, i, x, y, w, h, fill, flip, data, colors, fn)
```

## Arguments

- j:

  An integer specifying the column index

- i:

  An integer specifying the row index

- x:

  A numeric vector specifying the x position

- y:

  A numeric vector specifying the y position

- w:

  A numeric vector specifying the width

- h:

  A numeric vector specifying the height

- fill:

  A character vector specifying the fill color

- alpha:

  A numeric value between 0 and 1 specifying the transparency of the
  fill color

- color:

  A character vector specifying the color of the reticle

- data:

  A dataframe used to create the annotation. Different from the data
  used to create the heatmap itself, which is aggregated data. This
  dataframe is the original data, where each cell could have multiple
  values.

- dot_size:

  A numeric value specifying the size of the dot or a function to
  calculate the size from the values in the cell. The function can take
  1, 3, or 5 arguments: the first argument is the values in the cell
  before aggregation; the 2nd and 3rd arguments are the row and column
  indices; the 4th and 5th arguments are the row and column names.

- row_names:

  Row names from the heatmap matrix.

- col_names:

  Column names from the heatmap matrix.

- col_fun:

  A function to calculate the color of the bars

- colors:

  A character vector specifying the fill color of the violin plot. If
  not provided, the fill color of row/column annotation will be used
