# Wrap spatial plot if plotted independently

This function is used to wrap spatial plots if they are plotted
independently with return_layer = FALSE.

## Usage

``` r
.wrap_spatial_layers(
  layers,
  ext = NULL,
  flip_y = TRUE,
  legend.position = "right",
  legend.direction = "vertical",
  title = NULL,
  subtitle = NULL,
  xlab = NULL,
  ylab = NULL,
  theme = "theme_box",
  theme_args = list()
)
```

## Arguments

- layers:

  A list of ggplot layers to be wrapped.

- ext:

  A numeric vector of length 4 specifying the extent as
  `c(xmin, xmax, ymin, ymax)`. Default is NULL.

- flip_y:

  Whether to flip the y-axis direction. Default is TRUE.

- legend.position:

  The position of the legend. Default is "right".

- legend.direction:

  The direction of the legend. Default is "vertical".

- title:

  The title of the plot. Default is NULL.

- subtitle:

  The subtitle of the plot. Default is NULL.

- xlab:

  The x-axis label. Default is NULL.

- ylab:

  The y-axis label. Default is NULL.

- theme:

  The theme to be used for the plot. Default is "theme_box".

- theme_args:

  A list of arguments to be passed to the theme function. Default is an
  empty list.

## Value

A ggplot object with the specified layers.
