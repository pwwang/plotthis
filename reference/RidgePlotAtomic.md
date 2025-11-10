# Atomic ridge plot

Atomic ridge plot

## Usage

``` r
RidgePlotAtomic(
  data,
  x = NULL,
  in_form = c("long", "wide"),
  group_by = NULL,
  group_by_sep = "_",
  group_name = NULL,
  add_vline = NULL,
  vline_type = "solid",
  vline_color = TRUE,
  vline_width = 0.5,
  vline_alpha = 1,
  flip = FALSE,
  alpha = 0.8,
  scale = NULL,
  theme = "theme_this",
  theme_args = list(),
  palette = "Paired",
  palcolor = NULL,
  title = NULL,
  subtitle = NULL,
  xlab = NULL,
  ylab = NULL,
  x_text_angle = 90,
  keep_empty = FALSE,
  reverse = FALSE,
  facet_by = NULL,
  facet_scales = "fixed",
  facet_ncol = NULL,
  facet_nrow = NULL,
  facet_byrow = TRUE,
  aspect.ratio = 1,
  legend.position = "none",
  legend.direction = "vertical",
  ...
)
```

## Arguments

- data:

  A data frame It has two forms: wide and long. For the wide form, the
  values should under different 'group_by' columns. For the long form,
  the values should be under the 'x' column and the 'group_by' column
  should be provided, which should be a single column with the group
  names.

- x:

  A character string specifying the column name for the values A numeric
  column is expected. If 'data' is in the wide form, 'x' should be NULL.
  The values will be taken from the data under 'group_by' columns.

- in_form:

  A character string specifying the form of the data. Default is "long".

- group_by:

  A character string specifying the column name to group the data These
  groups will be shown on the y-axis.

- group_by_sep:

  A character string to concatenate the columns in `group_by` if
  multiple columns are provided If 'data' is in the wide form, the
  columns will not be concatenated.

- group_name:

  A character string to name the legend of 'group_by', if
  'legend.position' is not "none".

- add_vline:

  A numeric vector or a named list of numeric values to add vertical
  lines to the plot. If a named list is provided, the names should match
  the levels of 'group_by'. If `TRUE`, the vertical lines will be added
  at the mean of each group.

- vline_type:

  The type of line to draw for the vertical line.

- vline_color:

  The color of the vertical line. If `TRUE`, the vertical lines will be
  colored according to the group colors.

- vline_width:

  The width of the vertical line.

- vline_alpha:

  The alpha value of the vertical line.

- flip:

  A logical value. If TRUE, the plot will be flipped.

- alpha:

  A numeric value specifying the alpha of the ridges.

- scale:

  A numeric value to scale the ridges. See also
  [`geom_density_ridges`](https://wilkelab.org/ggridges/reference/geom_density_ridges.html).

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

- x_text_angle:

  A numeric value specifying the angle of the x-axis text.

- keep_empty:

  A logical value. If TRUE, keep the empty groups on the y-axis.

- reverse:

  A logical value. If TRUE, reverse the order of the groups on the
  y-axis.

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

- ...:

  Additional arguments.
