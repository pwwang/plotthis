# Calculate plot dimensions with aspect ratio consideration

This function calculates plot height and width taking into account:

- Content-based scaling (number of items on axes)

- Aspect ratio constraints

- Legend position and direction

- Minimum and maximum dimension bounds

## Usage

``` r
calculate_plot_dimensions(
  base_height = 4.5,
  aspect.ratio = 1,
  n_x = NULL,
  n_y = NULL,
  x_scale_factor = 0.5,
  y_scale_factor = 0.5,
  legend.position = "right",
  legend.direction = "vertical",
  legend_n = 1,
  legend_nchar = 5,
  flip = FALSE,
  min_width = 3,
  min_height = 3,
  max_width = 12,
  max_height = 12
)
```

## Arguments

- base_height:

  Base height for the plot (before legend adjustments). Default is 4.5.

- aspect.ratio:

  Aspect ratio (height/width). If NULL, width is calculated
  independently.

- n_x:

  Number of categories on x-axis (for width scaling)

- n_y:

  Number of categories on y-axis (for height scaling)

- x_scale_factor:

  Scaling factor per x-axis category. Default is 0.5.

- y_scale_factor:

  Scaling factor per y-axis category. Default is 0.5.

- legend.position:

  Position of legend ("none", "right", "left", "top", "bottom")

- legend.direction:

  Direction of legend ("vertical" or "horizontal")

- flip:

  Whether the plot is flipped (inverts aspect ratio)

- min_width:

  Minimum width in inches. Default is 3.

- min_height:

  Minimum height in inches. Default is 3.

- max_width:

  Maximum width in inches. Default is 12.

- max_height:

  Maximum height in inches. Default is 12.

## Value

A list with height and width components
