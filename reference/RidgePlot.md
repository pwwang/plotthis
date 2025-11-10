# Ridge Plot

Ridge plot to illustrate the distribution of the data in different
groups.

## Usage

``` r
RidgePlot(
  data,
  x = NULL,
  in_form = c("long", "wide"),
  split_by = NULL,
  split_by_sep = "_",
  group_by = NULL,
  group_by_sep = "_",
  group_name = NULL,
  scale = NULL,
  add_vline = NULL,
  vline_type = "solid",
  vline_color = TRUE,
  vline_width = 0.5,
  vline_alpha = 1,
  flip = FALSE,
  alpha = 0.8,
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
  combine = TRUE,
  nrow = NULL,
  ncol = NULL,
  byrow = TRUE,
  seed = 8525,
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

- x:

  A character string specifying the column name of the data frame to
  plot for the x-axis.

- in_form:

  A character string specifying the form of the data. Default is "long".

- split_by:

  The column(s) to split data by and plot separately.

- split_by_sep:

  The separator for multiple split_by columns. See `split_by`

- group_by:

  Columns to group the data for plotting For those plotting functions
  that do not support multiple groups, They will be concatenated into
  one column, using `group_by_sep` as the separator

- group_by_sep:

  The separator for multiple group_by columns. See `group_by`

- group_name:

  A character string to name the legend of 'group_by', if
  'legend.position' is not "none".

- scale:

  A numeric value to scale the ridges. See also
  [`geom_density_ridges`](https://wilkelab.org/ggridges/reference/geom_density_ridges.html).

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

  A numeric value specifying the transparency of the plot.

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

  A logical value indicating whether to keep empty groups. If FALSE,
  empty groups will be removed.

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

- combine:

  Whether to combine the plots into one when facet is FALSE. Default is
  TRUE.

- nrow:

  A numeric value specifying the number of rows in the facet.

- ncol:

  A numeric value specifying the number of columns in the facet.

- byrow:

  A logical value indicating whether to fill the plots by row.

- seed:

  The random seed to use. Default is 8525.

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

A ggplot object or wrap_plots object or a list of ggplot objects. If no
`split_by` is provided, a single plot (ggplot object) will be returned.
If 'combine' is TRUE, a wrap_plots object will be returned. If 'combine'
is FALSE, a list of ggplot objects will be returned.

## Examples

``` r
set.seed(8525)
data <- data.frame(
   x = c(rnorm(250, -1), rnorm(250, 1)),
   group = rep(LETTERS[1:5], each = 100)
)
RidgePlot(data, x = "x")  # fallback to a density plot
#> Picking joint bandwidth of 0.371

RidgePlot(data, x = "x", add_vline = 0, vline_color = "black")
#> Picking joint bandwidth of 0.371

RidgePlot(data, x = "x", group_by = "group")
#> Picking joint bandwidth of 0.378

RidgePlot(data, x = "x", group_by = "group", reverse = TRUE)
#> Picking joint bandwidth of 0.378

RidgePlot(data, x = "x", group_by = "group",
   add_vline = TRUE, vline_color = TRUE, alpha = 0.7)
#> Picking joint bandwidth of 0.378


# wide form
data_wide <- data.frame(
   A = rnorm(100),
   B = rnorm(100),
   C = rnorm(100),
   D = rnorm(100),
   E = rnorm(100),
   group = sample(letters[1:4], 100, replace = TRUE)
)
RidgePlot(data_wide, group_by = LETTERS[1:5], in_form = "wide")
#> Picking joint bandwidth of 0.337

RidgePlot(data_wide, group_by = LETTERS[1:5], in_form = "wide", facet_by = "group")
#> Picking joint bandwidth of 0.429
#> Picking joint bandwidth of 0.367
#> Picking joint bandwidth of 0.416
#> Picking joint bandwidth of 0.428
#> Picking joint bandwidth of 0.429
#> Picking joint bandwidth of 0.367
#> Picking joint bandwidth of 0.416
#> Picking joint bandwidth of 0.428

RidgePlot(data_wide, group_by = LETTERS[1:5], in_form = "wide", split_by = "group",
   palette = list(a = "Reds", b = "Blues", c = "Greens", d = "Purples"))
#> Picking joint bandwidth of 0.429
#> Picking joint bandwidth of 0.416
#> Picking joint bandwidth of 0.367
#> Picking joint bandwidth of 0.428
```
