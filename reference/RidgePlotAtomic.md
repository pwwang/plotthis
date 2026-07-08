# Atomic ridge plot

Core implementation for ridge (joy) plots. Renders overlapping density
curves for each group on the y-axis using
[`ggridges::geom_density_ridges()`](https://wilkelab.org/ggridges/reference/geom_density_ridges.html),
with optional vertical reference lines and wide-to-long data conversion.

The function accepts data in two forms:

- **long form** (default) — a numeric `x` column plus a `group_by`
  factor column whose levels become the y-axis ridges.

- **wide form** — multiple numeric columns named in `group_by` are
  gathered via
  [`tidyr::pivot_longer()`](https://tidyr.tidyverse.org/reference/pivot_longer.html)
  into `.x` / `.group` columns, then processed identically to the long
  form.

Vertical reference lines (`add_vline`) can be specified as a numeric
vector (same lines for all groups), a named list (per-group values), or
`TRUE` (group means). When `vline_color = TRUE`, each line is coloured
with a darkened blend of the corresponding ridge fill.

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
  palreverse = FALSE,
  title = NULL,
  subtitle = NULL,
  xlab = NULL,
  ylab = NULL,
  x_text_angle = 90,
  x_min = NULL,
  x_max = NULL,
  keep_na = FALSE,
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

  A data frame. Accepted in two forms:

  - **long** (`in_form = "long"`): a numeric column (named by `x`) and a
    factor column (named by `group_by`) whose levels become y-axis
    ridges.

  - **wide** (`in_form = "wide"`): multiple numeric columns listed in
    `group_by` are gathered into `.x` / `.group` via
    [`tidyr::pivot_longer()`](https://tidyr.tidyverse.org/reference/pivot_longer.html).

- x:

  A character string specifying the column name for the numeric values
  plotted on the x-axis. When `in_form = "wide"`, `x` should be `NULL`;
  the gathered values are stored in a synthetic `.x` column.

- in_form:

  A character string specifying whether `data` is in `"long"` (default)
  or `"wide"` format.

- group_by:

  A character string specifying the column(s) whose levels define the
  individual ridges on the y-axis. Multiple columns are concatenated
  with `group_by_sep`. In wide mode, these are the column names to
  gather.

- group_by_sep:

  A character string used to join multiple `group_by` column values into
  a single factor level. In wide form the columns are not concatenated
  (each becomes its own ridge). Default: `"_"`.

- group_name:

  A character string used as the legend title for the `group_by` fill
  aesthetic. Defaults to the (concatenated) `group_by` column name.

- add_vline:

  A specification for vertical reference lines:

  - `NULL` or `FALSE`: no lines.

  - `TRUE`: draw a line at the mean of each group.

  - A numeric vector: draw the same lines for all groups.

  - A named list of numeric vectors: per-group lines, where names should
    match `group_by` levels.

- vline_type:

  A character string specifying the line type for the vertical reference
  lines. Passed as `linetype` to `geom_vline()`. Default: `"solid"`.

- vline_color:

  The colour of the vertical reference lines:

  - A literal colour value or vector (recycled): applied directly.

  - `TRUE` (default): each line is coloured with a darkened blend of the
    corresponding ridge fill colour, computed via
    `blend_colors(mode = "multiply")`.

- vline_width:

  A numeric value for the thickness of the vertical reference lines.
  Passed as `linewidth` to `geom_vline()`. Default: `0.5`.

- vline_alpha:

  A numeric value in `[0, 1]` for the transparency of the vertical
  reference lines. Default: `1`.

- flip:

  A logical value. If `TRUE`, the axes are swapped via `coord_flip()`.
  X-axis text angle and grid-line placement are adjusted accordingly.

- alpha:

  A numeric value in `[0, 1]` for the transparency of the ridge fill.
  Default: `0.8`.

- scale:

  A numeric value controlling the vertical overlap of ridges. Passed to
  `ggridges::geom_density_ridges(scale = ...)`. Smaller values increase
  overlap. When `NULL`, ggridges auto-computes the scale.

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

  A numeric value specifying the angle (in degrees) for x-axis text when
  `flip = TRUE`. Used with
  [`calc_just()`](https://pwwang.github.io/plotthis/reference/calc_just.md)
  to compute optimal `hjust` / `vjust`. Default: `90`.

- x_min, x_max:

  Numeric limits for the x-axis. When `NULL` (default), limits are
  determined from the data range. Passed to `coord_cartesian()`.

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

- reverse:

  A logical value. If `TRUE`, the y-axis group order is reversed. NA
  groups are renamed to the literal string `"NA"` and placed at the end.

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

  Additional arguments passed to
  [`ggridges::geom_density_ridges()`](https://wilkelab.org/ggridges/reference/geom_density_ridges.html)
  (bandwidth, jittered_points, quantile_lines, etc.).

## Architecture

**RidgePlotAtomic** executes the following steps:

1.  **ggplot dispatch** — selects `gglogger::ggplot` or
    [`ggplot2::ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html).

2.  **Wide-to-long conversion** — when `in_form = "wide"`, calls
    [`tidyr::pivot_longer()`](https://tidyr.tidyverse.org/reference/pivot_longer.html)
    on the `group_by` columns, producing `.group` (factor) and `.x`
    (values). `x` and `group_by` are redirected to these synthetic
    columns.

3.  **Column resolution** —
    [`check_columns()`](https://pwwang.github.io/plotthis/reference/check_columns.md)
    validates `x`, `group_by` (force_factor, allow_multi, concat_multi),
    and `facet_by`.

4.  **Default group** — when `group_by` is `NULL`, a synthetic `.group`
    factor with a single space character level is created so the fill
    pipeline runs uniformly.

5.  **Reverse ordering** — if `reverse = TRUE`, factor levels of
    `group_by` are reversed, flipping the y-axis ridge order.

6.  **NA / empty handling** —
    [`process_keep_na_empty()`](https://pwwang.github.io/plotthis/reference/process_keep_na_empty.md)
    filters data. When `reverse = TRUE` and any group value is `NA`, the
    NA level is renamed to the literal string `"NA"` and moved to the
    end of the factor so colour mapping and display remain consistent.

7.  **Palette resolution** —
    [`palette_this()`](https://pwwang.github.io/plotthis/reference/palette_this.md)
    maps group levels to fill colours.

8.  **Base ggplot** — initialises `ggplot(data, aes(x, y, fill))` with
    `group_by` on the y-axis.

9.  **Ridge geometry** — `ggridges::geom_density_ridges(alpha, scale)`.
    When `scale` is `NULL`, ggridges auto-computes the overlap factor.

10. **Vertical reference lines** — if `add_vline` is not `NULL` /
    `FALSE`:

    - `add_vline = TRUE` → computes `tapply(x, group_by, mean)`.

    - `vline_color = TRUE` → resolves per-group line colours by
      darkening each fill colour via `blend_colors(mode = "multiply")`.
      Named list elements are matched to factor levels.

    - Adds `geom_vline(xintercept, linetype, linewidth, color, alpha)`.

11. **Scales and labels** —
    `scale_y_discrete(drop = !keep_empty_group)`,
    `scale_x_continuous()`, and `labs()`.

12. **Fill scale** — `scale_fill_manual()`. When
    `keep_empty_group = TRUE`, `drop = FALSE`, `breaks`, and `limits`
    are set to preserve empty factor levels.

13. **Flip-aware theme** — when `flip = TRUE`:

    - `coord_flip()` is applied.

    - x-axis text angle is set from `x_text_angle` with computed `hjust`
      / `vjust` via
      [`calc_just()`](https://pwwang.github.io/plotthis/reference/calc_just.md).

    - Major grid lines are drawn on the x-axis.

    - When `flip = FALSE`, y-axis text is right-aligned and grid lines
      appear on the y-axis.

14. **Theme application** — `do_call(theme, theme_args)` applies the
    resolved theme function, then `aspect.ratio` and legend position are
    set.

15. **Dimension calculation** —
    `calculate_plot_dimensions(base_height = 1, n_y = nlevels(group_by), y_scale_factor = 1, aspect.ratio, legend, flip)`
    sets `height` / `width` attributes. The base height of 1 unit per
    ridge keeps individual ridges compact.

16. **Faceting** —
    [`facet_plot()`](https://pwwang.github.io/plotthis/reference/facet_plot.md)
    applies `facet_grid` / `facet_wrap`, with
    `drop = !keep_empty_facet`.
