# RarefactionPlotAtomic

This function generates a rarefraction plot for a given dataset.

## Usage

``` r
RarefactionPlotAtomic(
  data,
  type = 1,
  se = TRUE,
  group_by = "group",
  group_name = NULL,
  pt_size = 3,
  line_width = 1,
  theme = "theme_this",
  theme_args = list(),
  palette = "Spectral",
  palcolor = NULL,
  alpha = 0.2,
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

  An iNEXT object or a list of data that will be handled by
  [iNEXT::iNEXT](https://rdrr.io/pkg/iNEXT/man/iNEXT.html).

- type:

  three types of plots: sample-size-based rarefaction/extrapolation
  curve (`type = 1`); sample completeness curve (`type = 2`);
  coverage-based rarefaction/extrapolation curve (`type = 3`).

- se:

  a logical variable to display confidence interval around the estimated
  sampling curve. Default to `NULL` which means TRUE if the data has the
  lower and upper bounds.

- group_by:

  A character string indicating how to group the data (color the lines).
  Possible values are "q" and "group"

- group_name:

  A character string indicating the name of the group, showing as the
  legend title.

- pt_size:

  A numeric value specifying the size of the points.

- line_width:

  A numeric value specifying the width of the lines.

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

- alpha:

  A numeric value specifying the transparency of the plot.

- facet_by:

  A character string indicating how to facet the data and plots Possible
  values are "q" and "group"

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

  Additional arguments to pass to
  [iNEXT::iNEXT](https://rdrr.io/pkg/iNEXT/man/iNEXT.html) when `data`
  is not an iNEXT object.

## Value

A ggplot object.
