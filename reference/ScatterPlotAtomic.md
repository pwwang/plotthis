# Scatter Plot Atomic

Scatter Plot Atomic

## Usage

``` r
ScatterPlotAtomic(
  data,
  x,
  y,
  size_by = 2,
  size_name = NULL,
  color_by = NULL,
  color_name = NULL,
  color_reverse = FALSE,
  theme = "theme_this",
  theme_args = list(),
  alpha = ifelse(shape %in% 21:25, 0.65, 1),
  shape = 21,
  border_color = "black",
  xtrans = "identity",
  ytrans = "identity",
  highlight = NULL,
  highlight_shape = 16,
  highlight_size = 3,
  highlight_color = "red",
  highlight_alpha = 1,
  palette = ifelse(!is.null(color_by) && !is.numeric(data[[color_by]]), "Paired",
    "Spectral"),
  palcolor = NULL,
  facet_by = NULL,
  facet_scales = "fixed",
  facet_ncol = NULL,
  facet_nrow = NULL,
  facet_byrow = TRUE,
  aspect.ratio = 1,
  legend.position = "right",
  legend.direction = "vertical",
  title = NULL,
  subtitle = NULL,
  xlab = NULL,
  ylab = NULL,
  ...
)
```

## Arguments

- data:

  A data frame.

- x:

  A character vector specifying the column to use for the x-axis. A
  numeric column is expected.

- y:

  A character vector specifying the column to use for the y-axis. A
  numeric column is expected.

- size_by:

  Which column to use as the size of the dots. It must be a numeric
  column. Or it can be a numeric value to specify the size of the dots.

- size_name:

  A character vector specifying the name for the size legend.

- color_by:

  Which column to use as the color of the dots. It could be a numeric
  column or a factor/character column. For shapes 21-25, the color is
  applied to the fill color.

- color_name:

  A character vector specifying the name for the color legend.

- color_reverse:

  A logical value indicating whether to reverse the color direction.
  Default is FALSE.

- theme:

  A character string or a theme class (i.e. ggplot2::theme_classic)
  specifying the theme to use. Default is "theme_this".

- theme_args:

  A list of arguments to pass to the theme function.

- alpha:

  A numeric value specifying the transparency of the dots. Default is 1.
  For shapes 21-25, the transparency is applied to the fill color.

- shape:

  A numeric value specifying the shape of the points. Default is 21.

- border_color:

  A character vector specifying the color for the border of the points.
  Or TRUE to use the fill color as the border color.

- xtrans:

  A character vector specifying the transformation of the x-axis.
  Default is "identity".

- ytrans:

  A character vector specifying the transformation of the y-axis.
  Default is "identity".

- highlight:

  A vector of indexes or rownames to select the points to highlight. It
  could also be an expression (in string) to filter the data.

- highlight_shape:

  A numeric value specifying the shape of the highlighted points.
  Default is 16.

- highlight_size:

  A numeric value specifying the size of the highlighted points. Default
  is 3.

- highlight_color:

  A character vector specifying the color of the highlighted points.
  Default is "red".

- highlight_alpha:

  A numeric value specifying the transparency of the highlighted points.
  Default is 1.

- palette:

  A character string specifying the palette to use. A named list or
  vector can be used to specify the palettes for different `split_by`
  values.

- palcolor:

  A character string specifying the color to use in the palette. A named
  list can be used to specify the colors for different `split_by`
  values. If some values are missing, the values from the palette will
  be used (palcolor will be NULL for those values).

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

- aspect.ratio:

  A numeric value specifying the aspect ratio of the plot.

- legend.position:

  A character string specifying the position of the legend. if
  `waiver()`, for single groups, the legend will be "none", otherwise
  "right".

- legend.direction:

  A character string specifying the direction of the legend.

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

- ...:

  Additional arguments.

## Value

A ggplot object
