# Atomic volcano plot

Atomic volcano plot

## Usage

``` r
VolcanoPlotAtomic(
  data,
  x,
  y,
  ytrans = function(n) -log10(n),
  color_by = NULL,
  color_name = NULL,
  flip_negatives = FALSE,
  x_cutoff = NULL,
  y_cutoff = 0.05,
  trim = c(0, 1),
  xlim = NULL,
  x_cutoff_name = NULL,
  y_cutoff_name = NULL,
  x_cutoff_color = "red2",
  y_cutoff_color = "blue2",
  x_cutoff_linetype = "dashed",
  y_cutoff_linetype = "dashed",
  x_cutoff_linewidth = 0.5,
  y_cutoff_linewidth = 0.5,
  pt_size = 2,
  pt_alpha = 0.5,
  nlabel = 5,
  labels = NULL,
  label_by = NULL,
  label_size = 3,
  label_fg = "black",
  label_bg = "white",
  label_bg_r = 0.1,
  highlight = NULL,
  highlight_color = "red",
  highlight_size = 2,
  highlight_alpha = 1,
  highlight_stroke = 0.5,
  facet_by = NULL,
  facet_scales = "fixed",
  facet_ncol = NULL,
  facet_nrow = NULL,
  facet_byrow = TRUE,
  theme = "theme_this",
  theme_args = list(),
  palette = "Spectral",
  palcolor = NULL,
  title = NULL,
  subtitle = NULL,
  xlab = NULL,
  ylab = NULL,
  aspect.ratio = 1,
  legend.position = "right",
  legend.direction = "vertical",
  seed = 8525,
  ...
)
```

## Arguments

- data:

  A data frame.

- x:

  A character string specifying the column name of the data frame to
  plot for the x-axis.

- y:

  A character string specifying the column name of the data frame to
  plot for the y-axis.

- ytrans:

  A function to transform the y-axis values.

- color_by:

  A character vector of column names to color the points by. If NULL,
  the points will be filled by the x and y cutoff value.

- color_name:

  A character string to name the legend of color.

- flip_negatives:

  A logical value to flip the y-axis for negative x values.

- x_cutoff:

  A numeric value to set the x-axis cutoff. Both negative and positive
  of this value will be used.

- y_cutoff:

  A numeric value to set the y-axis cutoff. Note that the y-axis cutoff
  will be transformed by `ytrans`. So you should provide the original
  value.

- trim:

  A numeric vector of length 2 to trim the x-axis values. The values
  must be in the range from 0 to 1, which works as quantile to trim the
  x-axis values. For example, c(0.01, 0.99) will trim the 1% and 99%
  quantile of the x-axis values. If the values are less then 1% or
  greater than 99% quantile, the values will be set to the 1% or 99%
  quantile.

- xlim:

  A numeric vector of length 2 to set the x-axis limits.

- x_cutoff_name:

  A character string to name the x-axis cutoff. If "none", the legend
  for the x-axis cutoff will not be shown.

- y_cutoff_name:

  A character string to name the y-axis cutoff. If "none", the legend
  for the y-axis cutoff will not be shown.

- x_cutoff_color:

  A character string to color the x-axis cutoff line.

- y_cutoff_color:

  A character string to color the y-axis cutoff line.

- x_cutoff_linetype:

  A character string to set the x-axis cutoff line type.

- y_cutoff_linetype:

  A character string to set the y-axis cutoff line type.

- x_cutoff_linewidth:

  A numeric value to set the x-axis cutoff line size.

- y_cutoff_linewidth:

  A numeric value to set the y-axis cutoff line size.

- pt_size:

  A numeric value to set the point size.

- pt_alpha:

  A numeric value to set the point transparency.

- nlabel:

  A numeric value to set the number of labels to show. The points will
  be ordered by the distance to the origin. Top `nlabel` points will be
  labeled.

- labels:

  A character vector of row names or indexes to label the points.

- label_by:

  A character string of column name to use as labels. If NULL, the row
  names will be used.

- label_size:

  A numeric value to set the label size.

- label_fg:

  A character string to set the label color.

- label_bg:

  A character string to set the label background color.

- label_bg_r:

  A numeric value specifying the radius of the background of the label.

- highlight:

  A character vector of row names or indexes to highlight the points.

- highlight_color:

  A character string to set the highlight color.

- highlight_size:

  A numeric value to set the highlight size.

- highlight_alpha:

  A numeric value to set the highlight transparency.

- highlight_stroke:

  A numeric value to set the highlight stroke size.

- facet_by:

  A character string specifying the column name of the data frame to
  facet the plot. Otherwise, the data will be split by `split_by` and
  generate multiple plots and combine them into one using
  [`patchwork::wrap_plots`](https://patchwork.data-imaginist.com/reference/wrap_plots.html)

- facet_scales:

  Whether to scale the axes of facets. Default is "fixed" Other options
  are "free", "free_x", "free_y". See
  [`ggplot2::facet_wrap`](https://ggplot2.tidyverse.org/reference/facet_wrap.html)

- facet_ncol:

  A numeric value specifying the number of columns in the facet. When
  facet_by is a single column and facet_wrap is used.

- facet_nrow:

  A numeric value specifying the number of rows in the facet. When
  facet_by is a single column and facet_wrap is used.

- facet_byrow:

  A logical value indicating whether to fill the plots by row. Default
  is TRUE.

- theme:

  A character string or a theme class (i.e. ggplot2::theme_classic)
  specifying the theme to use. Default is "theme_this".

- theme_args:

  A list of arguments to pass to the theme function.

- palette:

  A character string specifying the palette to use. A named list or
  vector can be used to specify the palettes for different `split_by`
  values.

- palcolor:

  A character string specifying the color to use in the palette. A named
  list can be used to specify the colors for different `split_by`
  values. If some values are missing, the values from the palette will
  be used (palcolor will be NULL for those values).

- title:

  A character string specifying the title of the plot. A function can be
  used to generate the title based on the default title. This is useful
  when split_by is used and the title needs to be dynamic.

- subtitle:

  A character string specifying the subtitle of the plot.

- xlab:

  A character string specifying the x-axis label.

- ylab:

  A character string specifying the y-axis label.

- aspect.ratio:

  A numeric value specifying the aspect ratio of the plot.

- legend.position:

  A character string specifying the position of the legend. if
  `waiver()`, for single groups, the legend will be "none", otherwise
  "right".

- legend.direction:

  A character string specifying the direction of the legend.

- seed:

  The random seed to use. Default is 8525.

- ...:

  Additional arguments.

## Value

A ggplot object
