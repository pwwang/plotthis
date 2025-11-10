# Get a ggplot layer for background

Get a ggplot layer for background

## Usage

``` r
bg_layer(
  data,
  x,
  palette,
  palcolor,
  alpha,
  keep_empty,
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

- palette:

  A character string specifying the palette to use

- palcolor:

  A character string specifying the color to use in the palette

- alpha:

  A numeric value specifying the transparency of the plot

- keep_empty:

  A logical value indicating whether to keep empty groups

- facet_by:

  A character string specifying the column name(s) of the data frame to
  facet the plot

- direction:

  A character string specifying the direction for the background

## Value

A ggplot layer for background
