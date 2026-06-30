# Atomic UpSet plot (internal)

Core implementation for drawing a single UpSet plot. This is the
internal workhorse behind the exported
[`UpsetPlot`](https://pwwang.github.io/plotthis/reference/UpsetPlot.md)
function — it takes a **single** data partition (no `split_by` support)
and returns a `ggplot` object. The plot comprises three visual
components:

- A **horizontal bar chart** (top) showing the size of each
  intersection, with bars filled by a gradient of intersection count.

- A **set-size bar chart** (left) showing the total number of elements
  in each set (added automatically by `ggupset`).

- A **combination matrix** (bottom) where rows are sets, columns are
  intersections, and dots indicate set membership. Connected dots trace
  the intersection pattern.

## Usage

``` r
UpsetPlotAtomic(
  data,
  in_form = "auto",
  group_by = NULL,
  group_by_sep = "_",
  id_by = NULL,
  label = TRUE,
  label_fg = "black",
  label_size = NULL,
  label_bg = "white",
  label_bg_r = 0.1,
  palette = "material-indigo",
  palcolor = NULL,
  palreverse = FALSE,
  alpha = 1,
  specific = TRUE,
  combmatrix_gap = 6,
  theme = "theme_this",
  theme_args = list(),
  title = NULL,
  subtitle = NULL,
  xlab = NULL,
  ylab = NULL,
  aspect.ratio = 0.6,
  legend.position = "right",
  legend.direction = "vertical",
  levels = NULL,
  ...
)
```

## Arguments

- data:

  A data frame.

- in_form:

  A character string specifying the input format. One of `"auto"`
  (default; detect from `data` structure), `"long"`, `"wide"`, `"list"`,
  or `"upset"`.

- group_by:

  Columns to group the data for plotting For those plotting functions
  that do not support multiple groups, They will be concatenated into
  one column, using `group_by_sep` as the separator

- group_by_sep:

  The separator for multiple group_by columns. See `group_by`

- id_by:

  A character string specifying the column name for instance
  identifiers. Required for long format; optional for wide format (a
  synthetic `.id` column is created if omitted).

- label:

  A logical value. When `TRUE` (default), count labels are displayed
  above each intersection bar via
  [`geom_text_repel()`](https://ggrepel.slowkow.com/reference/geom_text_repel.html).

- label_fg:

  A character string specifying the colour of the label text. Default:
  `"black"`.

- label_size:

  A numeric value specifying the size of the label text. Default: `NULL`
  (computed from `base_size / 12 * 3.5`).

- label_bg:

  A character string specifying the background fill colour of the label.
  Default: `"white"`.

- label_bg_r:

  A numeric value specifying the corner radius of the label background,
  passed to `geom_text_repel(bg.r)`. Default: `0.1`.

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

- alpha:

  A numeric value specifying the transparency of the plot.

- specific:

  A logical value. When `TRUE` (default), only specific intersections
  are returned (elements belonging exclusively to the shown set
  combination). When `FALSE`, all overlapping items are included. See
  <https://github.com/gaospecial/ggVennDiagram/issues/64>.

- combmatrix_gap:

  A numeric value specifying the gap between rows of the combination
  matrix, measured at `base_size = 12`. The actual gap is scaled by
  `text_size_scale = base_size / 12`. Default: `6`.

- theme:

  A character string or a theme class (i.e. ggplot2::theme_classic)
  specifying the theme to use. Default is "theme_this".

- theme_args:

  A list of arguments to pass to the theme function.

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

- levels:

  A character vector of set levels to include, in display order. When
  `NULL` (default), all detected sets are shown. Accepted for
  compatibility but not currently used by the internal implementation.

- ...:

  Additional arguments passed to
  [`scale_x_upset`](https://rdrr.io/pkg/ggupset/man/scale_x_upset.html).
  Useful options include `n_sets` and `n_intersections` to limit the
  number of shown sets / intersections.

## Value

A `ggplot` object with `height` and `width` attributes (in inches).

## Architecture

1.  **ggplot dispatch** — selects `gglogger::ggplot` or
    [`ggplot2::ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)
    based on `getOption("plotthis.gglogger.enabled")`.

2.  **Data preparation** —
    [`prepare_upset_data()`](https://pwwang.github.io/plotthis/reference/prepare_upset_data.md)
    converts the input into the internal `UpsetPlotData` format with an
    `Intersection` list-column (set-membership labels per element) and a
    `"group_order"` attribute for set ordering.

3.  **Text scale** — `base_size` from `theme_args` (default 12) drives a
    `text_size_scale` that proportionally scales the combination-matrix
    row gap, label sizes, and point sizes.

4.  **Bar geom** — `geom_bar()` counts rows per intersection. Bars are
    filled by count via `scale_fill_gradientn()` with colours from
    [`palette_this()`](https://pwwang.github.io/plotthis/reference/palette_this.md).

5.  **Bar labels** — when `label = TRUE`,
    [`geom_text_repel()`](https://ggrepel.slowkow.com/reference/geom_text_repel.html)
    places count labels above each bar, with configurable foreground,
    background, and size.

6.  **Scale x upset** — `ggupset::scale_x_upset(...)` is called,
    returning two components: a `ScaleUpset` (index `[[1]]`) and a
    `CoordCombMatrix` (index `[[2]]`). Only the scale is applied; the
    bundled coord is discarded to avoid a "Coordinate system already
    present" conflict.

7.  **Labels and theme** — `labs()` sets title, subtitle, and axis text.
    `do_call(theme, theme_args)` applies the chosen theme. Aspect ratio,
    legend position, legend direction, and grid lines are configured.

8.  **Combination matrix** —
    [`ggupset::axis_combmatrix()`](https://rdrr.io/pkg/ggupset/man/axis_combmatrix.html)
    builds the lower panel via an `override_plotting_function`:

    - Reorders rows to match `group_order` (reversed for top-to-bottom
      set listing).

    - Alternating row backgrounds via `geom_rect()` with white /
      `#F7F7F7` fills.

    - Set-membership dots (`geom_point()`) in black for present and
      `#E0E0E0` for absent.

    - Connecting lines (`geom_line()`) through observed dots to trace
      the intersection pattern.

9.  **Matrix theme** —
    [`ggupset::theme_combmatrix()`](https://rdrr.io/pkg/ggupset/man/theme_combmatrix.html)
    applies the combination-matrix appearance with `combmatrix_gap`
    between rows (scaled by `text_size_scale`).

10. **Dimension calculation** —
    [`calculate_plot_dimensions()`](https://pwwang.github.io/plotthis/reference/calculate_plot_dimensions.md)
    computes height from `n_sets` and width from `n_intersections`, both
    passed via `...` (default 99). `aspect.ratio` is used as a standard
    H/W coupling with `x_scale_factor = 0.6`. An additional width
    adjustment accounts for set-label character width.
