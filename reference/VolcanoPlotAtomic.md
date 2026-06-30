# Atomic volcano plot (internal)

Core implementation for drawing a single volcano plot. This is the
workhorse behind the exported
[`VolcanoPlot`](https://pwwang.github.io/plotthis/reference/VolcanoPlot.md)
function — it takes a **single** data frame (no `split_by` support) and
returns a `ggplot` object. The plot displays statistical significance
(typically -log10 adjusted p-value) on the y-axis versus magnitude of
change (log2 fold change) on the x-axis, with points coloured by
significance category or a user-supplied variable. Top features can be
automatically labelled via
[`geom_text_repel()`](https://ggrepel.slowkow.com/reference/geom_text_repel.html),
and specific points can be highlighted.

The function categorises points into three groups based on cutoff
thresholds: `"sig_pos_x"` (points exceeding both the positive x-cutoff
and y-cutoff), `"sig_neg_x"` (points exceeding both the negative
x-cutoff and y-cutoff), and `"insig"` (all remaining points). When
`color_by = NULL`, this categorisation drives point colouring; otherwise
the supplied column controls the colour scale.

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
  palreverse = FALSE,
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

  A function to transform the y-axis values before plotting. The default
  `function(n) -log10(n)` converts p-values to a -log10 scale. The
  transformed values are used for both the y-axis and cutoff
  comparisons.

- color_by:

  A character string specifying the column name to colour the points by.
  When `NULL` (default), points are automatically categorised as
  `"sig_pos_x"`, `"sig_neg_x"`, or `"insig"` based on `x_cutoff` and
  `y_cutoff`, and the colour legend is suppressed. When a column name is
  provided, the colour mapping follows the column type — discrete
  (character/factor) uses `scale_color_manual()` with the specified
  `palette`; numeric (continuous) uses `scale_color_gradientn()`.

- color_name:

  A character string for the colour legend title when `color_by` is a
  numeric column. When `NULL` (default), the `color_by` column name is
  used.

- flip_negatives:

  A logical value. When `TRUE`, y-values of points with negative
  x-values are multiplied by -1, creating a mirrored volcano plot where
  both up- and down-regulated features show their significance on the
  same side of the y-axis. A horizontal line at `y = 0` and
  absolute-value axis labels are added. Default: `FALSE`.

- x_cutoff:

  A numeric value specifying the x-axis significance cutoff. Both the
  negative and positive of this value are used as vertical threshold
  lines. When `NULL` or `0`, no x-cutoff line is drawn. Default: `NULL`.

- y_cutoff:

  A numeric value specifying the y-axis significance cutoff in the
  **original** (untransformed) scale. The value is transformed by
  `ytrans` before plotting. When `NULL`, no y-cutoff line is drawn and
  the category assignment uses only the x-cutoff. Default: `0.05`.

- trim:

  A numeric vector of length 2 specifying quantile bounds for
  winsorizing the x-axis values. Values below the first quantile are
  clamped to that quantile; values above the second quantile are clamped
  to that quantile. Both values must be in `[0, 1]`. When both bounds
  are nonzero and of opposite sign, they are symmetrised to the smaller
  absolute value. Default: `c(0, 1)` (no trimming).

- xlim:

  A numeric vector of length 2 to set the x-axis limits. Passed to
  [`xlim()`](https://ggplot2.tidyverse.org/reference/lims.html). When
  `NULL` (default), limits are determined automatically from the data.

- x_cutoff_name:

  A character string for the x-cutoff legend entry. When `"none"`, the
  legend for the x-cutoff line is suppressed entirely (the line is still
  drawn). When `NULL` (default), a label of the form
  `"<x> = +/-<value>"` is generated.

- y_cutoff_name:

  A character string for the y-cutoff legend entry. When `"none"`, the
  legend for the y-cutoff line is suppressed entirely (the line is still
  drawn). When `NULL` (default), a label of the form
  `"<ylab> = <value>"` is generated.

- x_cutoff_color:

  A character string specifying the colour of the x-axis cutoff line(s).
  Default: `"red2"`.

- y_cutoff_color:

  A character string specifying the colour of the y-axis cutoff line(s).
  Default: `"blue2"`.

- x_cutoff_linetype:

  A character string specifying the linetype of the x-axis cutoff
  line(s). Default: `"dashed"`.

- y_cutoff_linetype:

  A character string specifying the linetype of the y-axis cutoff
  line(s). Default: `"dashed"`.

- x_cutoff_linewidth:

  A numeric value specifying the linewidth of the x-axis cutoff line(s).
  Default: `0.5`.

- y_cutoff_linewidth:

  A numeric value specifying the linewidth of the y-axis cutoff line(s).
  Default: `0.5`.

- pt_size:

  A numeric value specifying the point size for all data points.
  Default: `2`.

- pt_alpha:

  A numeric value in `[0, 1]` specifying the transparency of all data
  points. Default: `0.5`.

- nlabel:

  An integer specifying the number of top features to label
  automatically. Points are ranked by Euclidean distance to the origin
  within each `sign(x)` group (and per facet level if `facet_by` is
  set). Only non-insignificant points receive labels. Default: `5`.

- labels:

  A character vector of row names or integer indices specifying which
  points to label. Overrides automatic `nlabel` selection. When `NULL`
  (default), top `nlabel` points are chosen automatically.

- label_by:

  A character string specifying the column whose values are used as
  label text. When `NULL` (default), row names of the data frame are
  used.

- label_size:

  A numeric value specifying the font size of the labels. Default: `3`.

- label_fg:

  A character string specifying the text colour of the labels. Default:
  `"black"`.

- label_bg:

  A character string specifying the background colour of the label boxes
  (passed to `geom_text_repel(bg.color = ...)`). Default: `"white"`.

- label_bg_r:

  A numeric value specifying the corner radius of the label background
  boxes (passed to `geom_text_repel(bg.r = ...)`). Default: `0.1`.

- highlight:

  A character vector of row names or integer indices specifying which
  points to highlight with an overlaid point layer in `highlight_color`.
  When `NULL` (default), no highlighting is applied.

- highlight_color:

  A character string specifying the colour of the highlight points.
  Default: `"red"`.

- highlight_size:

  A numeric value specifying the point size of the highlight layer.
  Default: `2`.

- highlight_alpha:

  A numeric value in `[0, 1]` specifying the transparency of the
  highlight points. Default: `1`.

- highlight_stroke:

  A numeric value specifying the stroke width of the highlight point
  borders. Default: `0.5`.

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

A `ggplot` object with `height` and `width` attributes (in inches)
attached.

## Architecture

1.  **ggplot dispatch** — selects `gglogger::ggplot` or
    [`ggplot2::ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)
    based on `getOption("plotthis.gglogger.enabled")`.

2.  **Input validation** — `trim` (must be length-2 in `[0, 1]`) and
    `xlim` (must be length-2 or `NULL`) are validated via
    [`stopifnot()`](https://rdrr.io/r/base/stopifnot.html). `trim` is
    sorted.

3.  **Column resolution** — `x`, `y`, `color_by`, `facet_by`, and
    `label_by` are validated and transformed via
    [`check_columns`](https://pwwang.github.io/plotthis/reference/check_columns.md).
    Multi-column `facet_by` is concatenated with `force_factor = TRUE`.

4.  **y-axis transformation** — the y-column is transformed by
    `ytrans()` (default: `-log10(n)`). The `y_cutoff` value is also
    transformed.

5.  **x_cutoff defaulting** — if `x_cutoff` is `NULL`, it is set to `0`
    (suppressing the x-cutoff legend line).

6.  **Category assignment** — a `.category` factor with levels
    `c("sig_neg_x", "insig", "sig_pos_x")` is created:

    - When `y_cutoff` is non-`NULL`: points with `|x| > x_cutoff` AND
      `y > y_cutoff` are significant.

    - When `y_cutoff` is `NULL`: only the x-cutoff determines
      significance.

7.  **Color resolution** — three cases:

    - `color_by = NULL`: uses `.category` as a discrete colour column;
      the legend is suppressed.

    - Character/factor column: discrete colour scale via
      [`palette_this()`](https://pwwang.github.io/plotthis/reference/palette_this.md)
      and `scale_color_manual()`, guide suppressed.

    - Numeric column: continuous gradient via `scale_color_gradientn()`
      with a framed colour-bar legend.

8.  **Flip negatives** — when `flip_negatives = TRUE`, the y-values of
    points with negative x are multiplied by -1, creating a mirrored
    volcano where both up- and down-regulated features show their
    significance on the same side of the y-axis.

9.  **Label column** — `.label` is populated from `label_by` or
    `rownames(data)`.

10. **Trim / winsorize** — x-values beyond the trim quantile bounds are
    clamped. When both bounds are nonzero and of opposite sign, they are
    symmetrised to the smaller absolute value. Outlying points are
    marked in `.outlier`.

11. **Label selection** — two modes:

    - Explicit `labels`: the specified rows (by name or index) are
      marked for labelling.

    - Automatic: top `nlabel` points (by Euclidean distance to origin)
      are selected per `sign(x)` group, and per facet level if
      `facet_by` is set.

    All labels are filtered to exclude `"insig"` points.

12. **Data split** — data is split into `pos_data` (`x >= 0`) and
    `neg_data` (`x < 0`) so that `ggrepel` labels can nudge in opposite
    directions (positive points nudge left, negative points nudge
    right).

13. **Outlier jitter** — outlier points are rendered separately with
    `position_jitter()` to reduce overplotting.

14. **Base ggplot** — `geom_point()` layers for positive, negative, and
    outlier data, with colour mapped to `color_by`.

15. **Colour scale** — discrete: `scale_color_manual()` with `"insig"`
    forced to `"grey"` (when `palcolor` is `NULL`); continuous:
    `scale_color_gradientn()` with palette re-scaled so that the
    colour-bar is centred at 0.

16. **Highlight** — when `highlight` is provided, two additional
    `geom_point()` layers (non-outliers and outliers with jitter)
    overlay the highlighted points in `highlight_color`.

17. **x-cutoff lines** — vertical dashed lines at `+/- x_cutoff` via
    `geom_vline()` with
    [`new_scale_color()`](https://eliocamp.github.io/ggnewscale/reference/new_scale.html),
    labelled by `x_cutoff_name`. Suppressed when `x_cutoff` is `NULL` or
    `0`.

18. **y-cutoff lines** — horizontal dashed line(s) at `y_cutoff` (or
    `+/- y_cutoff` when `flip_negatives = TRUE`) via `geom_hline()` with
    [`new_scale_color()`](https://eliocamp.github.io/ggnewscale/reference/new_scale.html),
    labelled by `y_cutoff_name`.

19. **Flip-negatives axis** — when `flip_negatives = TRUE`, a solid
    `geom_hline(yintercept = 0)` is added and
    `scale_y_continuous(labels = abs)` formats the y-axis.

20. **x-axis limits** — optional `xlim` passed to
    [`ggplot2::xlim()`](https://ggplot2.tidyverse.org/reference/lims.html).

21. **Reference line and labels** — a grey80 dashed vertical line at
    `x = 0`, followed by `geom_text_repel()` for positive and negative
    labelled points with separate x-nudges.

22. **Labels and theme** — `labs()`, `coord_cartesian(clip = "off")`,
    `do_call(theme, theme_args)`, and theme elements for `aspect.ratio`,
    `legend.position`, and `legend.direction`.

23. **Dimension calculation** —
    [`calculate_plot_dimensions()`](https://pwwang.github.io/plotthis/reference/calculate_plot_dimensions.md)
    computes `height` and `width` attributes from `base_height = 5`,
    `aspect.ratio`, and legend geometry.

24. **Faceting** —
    [`facet_plot()`](https://pwwang.github.io/plotthis/reference/facet_plot.md)
    wraps the plot with `facet_wrap` / `facet_grid` if `facet_by` is
    provided.
