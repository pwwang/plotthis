# RarefactionPlot

This function generates a rarefraction plot for a given dataset.

## Usage

``` r
RarefactionPlot(
  data,
  type = 1,
  se = NULL,
  group_by = "group",
  group_by_sep = "_",
  group_name = NULL,
  split_by = NULL,
  split_by_sep = "_",
  theme = "theme_this",
  theme_args = list(),
  palette = "Spectral",
  palcolor = NULL,
  alpha = 0.2,
  pt_size = 3,
  line_width = 1,
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
  seed = 8525,
  combine = TRUE,
  nrow = NULL,
  ncol = NULL,
  byrow = TRUE,
  axes = NULL,
  axis_titles = axes,
  guides = NULL,
  design = NULL,
  ...
)
```

## Arguments

- data:

  A data frame.

- type:

  three types of plots: sample-size-based rarefaction/extrapolation
  curve (`type = 1`); sample completeness curve (`type = 2`);
  coverage-based rarefaction/extrapolation curve (`type = 3`).

- se:

  a logical variable to display confidence interval around the estimated
  sampling curve. Default to `NULL` which means TRUE if the data has the
  lower and upper bounds.

- group_by:

  Columns to group the data for plotting For those plotting functions
  that do not support multiple groups, They will be concatenated into
  one column, using `group_by_sep` as the separator

- group_by_sep:

  A character string indicating how to separate the group_by column if
  both "q" and "group" are used. for 'group_by'. Default to "\_".

- group_name:

  A character string indicating the name of the group, showing as the
  legend title.

- split_by:

  A character string indicating how to split the data and plots Possible
  values are "q" and "group"

- split_by_sep:

  The separator for multiple split_by columns. See `split_by`

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

- pt_size:

  A numeric value specifying the size of the points.

- line_width:

  A numeric value specifying the width of the lines.

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

- seed:

  The random seed to use. Default is 8525.

- combine:

  Whether to combine the plots into one when facet is FALSE. Default is
  TRUE.

- nrow:

  A numeric value specifying the number of rows in the facet.

- ncol:

  A numeric value specifying the number of columns in the facet.

- byrow:

  A logical value indicating whether to fill the plots by row.

- axes:

  A string specifying how axes should be treated. Passed to
  [`patchwork::wrap_plots()`](https://patchwork.data-imaginist.com/reference/wrap_plots.html).
  Only relevant when `split_by` is used and `combine` is TRUE. Options
  are:

  - 'keep' will retain all axes in individual plots.

  - 'collect' will remove duplicated axes when placed in the same run of
    rows or columns of the layout.

  - 'collect_x' and 'collect_y' will remove duplicated x-axes in the
    columns or duplicated y-axes in the rows respectively.

- axis_titles:

  A string specifying how axis titltes should be treated. Passed to
  [`patchwork::wrap_plots()`](https://patchwork.data-imaginist.com/reference/wrap_plots.html).
  Only relevant when `split_by` is used and `combine` is TRUE. Options
  are:

  - 'keep' will retain all axis titles in individual plots.

  - 'collect' will remove duplicated titles in one direction and merge
    titles in the opposite direction.

  - 'collect_x' and 'collect_y' control this for x-axis titles and
    y-axis titles respectively.

- guides:

  A string specifying how guides should be treated in the layout. Passed
  to
  [`patchwork::wrap_plots()`](https://patchwork.data-imaginist.com/reference/wrap_plots.html).
  Only relevant when `split_by` is used and `combine` is TRUE. Options
  are:

  - 'collect' will collect guides below to the given nesting level,
    removing duplicates.

  - 'keep' will stop collection at this level and let guides be placed
    alongside their plot.

  - 'auto' will allow guides to be collected if a upper level tries, but
    place them alongside the plot if not.

- design:

  Specification of the location of areas in the layout, passed to
  [`patchwork::wrap_plots()`](https://patchwork.data-imaginist.com/reference/wrap_plots.html).
  Only relevant when `split_by` is used and `combine` is TRUE. When
  specified, `nrow`, `ncol`, and `byrow` are ignored. See
  [`patchwork::wrap_plots()`](https://patchwork.data-imaginist.com/reference/wrap_plots.html)
  for more details.

- ...:

  Additional arguments.

## Value

A ggplot object or wrap_plots object or a list of ggplot objects

## Examples

``` r
# \donttest{
set.seed(8525)
spider <- list(
   Girdled = c(46, 22, 17, 15, 15, 9, 8, 6, 6, 4, rep(2, 4), rep(1, 12)),
   Logged = c(88, 22, 16, 15, 13, 10, 8, 8, 7, 7, 7, 5, 4, 4, 4, 3, 3, 3, 3,
     2, 2, 2, 2, rep(1, 14))
)

RarefactionPlot(spider)

RarefactionPlot(spider, q = c(0, 1, 2), facet_by = "q")

RarefactionPlot(spider, q = c(0, 1, 2), split_by = "q")

RarefactionPlot(spider, q = c(0, 1, 2), split_by = "q",
                palette = c("0" = "Paired", "1" = "Set1", "2" = "Dark2"))

RarefactionPlot(spider, q = c(0, 1, 2), group_by = "q",
 facet_by = "group", palette = "Set1", type = 3)

# }
```
