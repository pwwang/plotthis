# Atomic radar/spider plot (internal)

Core implementation for drawing a single radar (or spider) chart. This
is the workhorse behind the exported
[`RadarPlot`](https://pwwang.github.io/plotthis/reference/radarplot.md)
and
[`SpiderPlot`](https://pwwang.github.io/plotthis/reference/radarplot.md)
functions — it takes a **single** data frame (no `split_by` support) and
returns a `ggplot` object.

Radar charts display multivariate data on a two-dimensional polar
coordinate system where each variable (x-axis category) is placed along
a radial axis evenly spaced around the circle. The data values are
connected by lines forming a polygon (or multiple polygons when
`group_by` is provided). When `polygon = FALSE` (the default) the grid
is rendered as concentric circles (classic radar chart); when
`polygon = TRUE` straight polygonal grid lines are used (spider chart
variant).

The polar coordinate system is implemented via a local `coord_radar()`
helper that extends
[`ggplot2::CoordPolar`](https://ggplot2.tidyverse.org/reference/Coord.html)
with `is_linear = TRUE`, allowing discrete x-axis positions to be mapped
to evenly spaced angular coordinates.

## Usage

``` r
RadarPlotAtomic(
  data,
  x,
  x_sep = "_",
  group_by = NULL,
  group_by_sep = "_",
  y = NULL,
  group_name = NULL,
  groups = NULL,
  scale_y = c("group", "global", "x", "none"),
  y_min = 0,
  y_max = NULL,
  y_nbreaks = 4,
  polygon = FALSE,
  fill = TRUE,
  linewidth = 1,
  pt_size = 4,
  max_charwidth = 16,
  bg_color = "grey80",
  bg_alpha = 0.1,
  theme = "theme_this",
  theme_args = list(),
  palette = "Paired",
  palcolor = NULL,
  palreverse = FALSE,
  facet_by = NULL,
  facet_scales = "fixed",
  facet_ncol = NULL,
  facet_nrow = NULL,
  facet_byrow = TRUE,
  alpha = 0.2,
  aspect.ratio = 1,
  legend.position = waiver(),
  legend.direction = "vertical",
  keep_na = FALSE,
  keep_empty = FALSE,
  title = NULL,
  subtitle = NULL,
  ...
)
```

## Arguments

- data:

  A data frame.

- x:

  A character string specifying the column name of the data frame to
  plot on the x-axis (the radial categories). Must be character or
  factor. Multiple columns can be provided; they are concatenated with
  `x_sep` as the separator.

- x_sep:

  A character string used to join multiple `x` columns. Default `"_"`.
  Ignored when `x` is a single column.

- group_by:

  A character vector of column names to group the data into separate
  filled polygons. Each unique combination becomes a distinct polygon
  layer. Multiple columns are concatenated with `group_by_sep`. When
  `NULL`, a single polygon is drawn with no legend.

- group_by_sep:

  A character string used to join multiple `group_by` columns. Default
  `"_"`.

- y:

  A character string specifying the numeric column for the radial axis.
  When `NULL`, the count of observations in each (`x`, `group_by`,
  `facet_by`) combination is used.

- group_name:

  A character string used as the colour/fill legend title. When `NULL`,
  the `group_by` column name is used.

- groups:

  A character vector of group values (in the `group_by` column) to
  include in the plot. When `NULL`, all groups are included. This can
  control which groups appear and their legend order. Implies
  `keep_empty = FALSE` for the `group_by` column: groups not present in
  the data are not shown in the legend.

- scale_y:

  How should the radial axis be scaled? Default is `"group"`. Options
  are `"group"`, `"global"`, `"x"`, and `"none"`.

  - `"group"` — scaled to the fraction within each group.

  - `"global"` — scaled to the fraction of the total.

  - `"x"` — scaled to the fraction within each x-axis category.

  - `"none"` — raw counts or values, no scaling.

- y_min:

  A numeric value setting the minimum of the radial axis. Default `0`.

- y_max:

  A numeric value setting the maximum of the radial axis. When `NULL`,
  the maximum data value is used.

- y_nbreaks:

  A numeric value for the number of breaks (concentric grid lines) on
  the radial axis. Default `4`.

- polygon:

  A logical value. When `TRUE`, the background grid is drawn as a
  polygon (spider chart style); when `FALSE` (default), concentric
  circles are used (radar chart style).

- fill:

  A logical value. When `TRUE` (default), the data polygons are filled
  with the group colour. When `FALSE`, only outlines are drawn.

- linewidth:

  A numeric value for the width of the polygon outline lines. Default
  `1`.

- pt_size:

  A numeric value for the size of the data point markers. Default `4`.

- max_charwidth:

  A numeric value for the maximum character width of x-axis labels
  before wrapping. Default `16`.

- bg_color:

  A character string specifying the background fill colour. Default
  `"grey80"`.

- bg_alpha:

  A numeric value for the transparency of the background fill. Default
  `0.1`.

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

- palreverse:

  A logical value indicating whether to reverse the palette. Default is
  FALSE.

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

- alpha:

  A numeric value specifying the transparency of the plot.

- aspect.ratio:

  A numeric value specifying the aspect ratio of the plot.

- legend.position:

  A character string specifying the position of the legend. if
  `waiver()`, for single groups, the legend will be "none", otherwise
  "right".

- legend.direction:

  A character string specifying the direction of the legend.

- keep_na:

  A logical value or a character to replace the NA values in the data.
  It can also take a named list to specify different behavior for
  different columns. If TRUE or NA, NA values will be replaced with NA.
  If FALSE, NA values will be removed from the data before plotting. If
  a character string is provided, NA values will be replaced with the
  provided string. If a named vector/list is provided, the names should
  be the column names to apply the behavior to, and the values should be
  one of TRUE, FALSE, or a character string. Without a named
  vector/list, the behavior applies to categorical/character columns
  used on the plot, for example, the `x`, `group_by`, `fill_by`, etc.

- keep_empty:

  One of FALSE, TRUE and "level". It can also take a named list to
  specify different behavior for different columns. Without a named
  list, the behavior applies to the categorical/character columns used
  on the plot, for example, the `x`, `group_by`, `fill_by`, etc.

  - `FALSE` (default): Drop empty factor levels from the data before
    plotting.

  - `TRUE`: Keep empty factor levels and show them as a separate
    category in the plot.

  - `"level"`: Keep empty factor levels, but do not show them in the
    plot. But they will be assigned colors from the palette to maintain
    consistency across multiple plots. Alias: `levels`

- title:

  A character string specifying the title of the plot. A function can be
  used to generate the title based on the default title. This is useful
  when split_by is used and the title needs to be dynamic.

- subtitle:

  A character string specifying the subtitle of the plot.

- ...:

  Additional arguments.

## Value

A `ggplot` object with `height` and `width` attributes (in inches)
attached.

## Architecture

1.  **Column resolution** — `x`, `y`, `group_by`, and `facet_by` are
    validated and transformed via
    [`check_columns`](https://pwwang.github.io/plotthis/reference/check_columns.md).
    Multi-column inputs for `x` and `group_by` are concatenated using
    `x_sep` and `group_by_sep`.

2.  **Group setup** — When `group_by` is `NULL`, a dummy `.group` factor
    is created so polygons still draw. The legend is suppressed. When
    `groups` is provided, data is filtered to only those group levels.

3.  **Count aggregation** — When `y = NULL`, the count of observations
    in each unique (`x`, `group_by`, `facet_by`) combination is
    computed. Factor levels are preserved after aggregation.

4.  **Proportion scaling** — The `scale_y` argument controls y-value
    normalisation: `"group"` scales within each group, `"global"` within
    each facet (or the whole dataset), `"x"` within each x category, and
    `"none"` leaves values raw. Percent labels are used for scaled
    modes.

5.  **NA / empty-level handling** —
    [`process_keep_na_empty()`](https://pwwang.github.io/plotthis/reference/process_keep_na_empty.md)
    applies `keep_na` and `keep_empty` policies. Per-column `keep_empty`
    settings for `x`, `group_by`, and `facet_by` are extracted; facet
    columns must share the same value.

6.  **Coordinate setup** — The local `coord_radar()` creates a
    `CoordPolar` subclass with `is_linear = TRUE`, placing discrete x
    positions at evenly spaced angles.

7.  **Y-axis range** — `y_min`, `y_max`, and `y_nbreaks` determine the
    radial axis limits and the number of concentric grid lines.

8.  **Colour mapping** —
    [`palette_this()`](https://pwwang.github.io/plotthis/reference/palette_this.md)
    assigns colours to all `group_by` levels, including `NA` (defaulting
    to `"grey80"`).

9.  **Background layer** — When `polygon = TRUE`, a polygonal background
    fills the area between `y_min` and `y_max`. When `polygon = FALSE`,
    a smooth circular background is interpolated via 360 sample points.

10. **Radial grid lines** — `geom_path()` draws radial lines from
    `y_min` to `y_max` at each x-axis position, respecting facets.

11. **Polygon grid** — When `polygon = TRUE`, concentric polygonal grid
    lines are drawn at each break level via `geom_polygon()`.

12. **Data rendering** — `geom_polygon()` draws the filled (or unfilled)
    polygons for each group. `geom_point()` adds points at each
    observation. Radial axis labels are rendered as text at a fixed
    angular offset.

13. **Scale configuration** — `scale_y_continuous()` sets the radial
    axis limits and breaks. `scale_x_discrete()` positions the category
    labels. Colour scales use `scale_fill_manual()` and
    `scale_color_manual()` with `drop` controlled by `keep_empty_group`.

14. **Circular grid lines** — When `polygon = FALSE`, dashed circular
    grid lines are styled via `panel.grid.major.y`.

15. **Dimension calculation** —
    [`calculate_plot_dimensions()`](https://pwwang.github.io/plotthis/reference/calculate_plot_dimensions.md)
    computes plot height and width from `aspect.ratio`, legend metrics,
    and a base height.

16. **Faceting** —
    [`facet_plot()`](https://pwwang.github.io/plotthis/reference/facet_plot.md)
    wraps the plot with `facet_wrap` / `facet_grid` if `facet_by` is
    supplied, respecting the `keep_empty` setting for facet variables.
