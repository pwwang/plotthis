# Get a ggplot layer for background

Get a ggplot layer for background

## Usage

``` r
bg_layer(
  data,
  x,
  keep_empty,
  palette,
  palcolor,
  alpha,
  facet_by,
  direction = "vertical"
)
```

## Arguments

- data:

  A data frame

- x:

  A character string specifying the column name of the data frame to
  plot for the x-axis

- keep_empty:

  A character string specifying whether to keep empty levels

- palette:

  A character string specifying the palette to use

- palcolor:

  A character string specifying the color to use in the palette

- alpha:

  A numeric value specifying the transparency of the plot

- facet_by:

  A character string specifying the column name(s) of the data frame to
  facet the plot

- direction:

  A character string specifying the direction for the background

## Value

A ggplot layer for background
