# Atomic Sankey / alluvial diagram (internal)

Core implementation for drawing a Sankey (alluvial) diagram using
[`geom_alluvium`](http://corybrunson.github.io/ggalluvial/reference/geom_alluvium.md)
(or
[`geom_flow`](http://corybrunson.github.io/ggalluvial/reference/geom_flow.md))
and
[`geom_stratum`](http://corybrunson.github.io/ggalluvial/reference/geom_stratum.md).
This function takes a **single** data frame (no `split_by` support) and
returns a `ggplot` object with faceting applied.

The function supports five input formats (`in_form`) which control how
the data columns are interpreted. Nodes (strata) are rendered as
vertical blocks whose fill colour and alpha can be customised
independently of the links (alluvia / flows) connecting them. Link
colours can match node colours or use a separate palette; link borders
can be set to a fixed colour or to follow the fill colour
(`links_color = ".fill"`).

Automatic legend resolution determines whether nodes on different x-axis
positions receive a merged legend or separate legends, based on overlaps
between stratum values across positions.

## Usage

``` r
SankeyPlotAtomic(
  data,
  in_form = c("auto", "long", "lodes", "wide", "alluvia", "counts"),
  x,
  x_sep = "_",
  y = NULL,
  stratum = NULL,
  stratum_sep = "_",
  alluvium = NULL,
  alluvium_sep = "_",
  flow = FALSE,
  nodes_color = "grey30",
  links_fill_by = NULL,
  links_fill_by_sep = "_",
  links_name = NULL,
  links_color = "gray80",
  nodes_palette = "Paired",
  nodes_palcolor = NULL,
  palreverse = FALSE,
  nodes_alpha = 1,
  nodes_label = FALSE,
  nodes_width = 0.25,
  nodes_label_miny = 0,
  nodes_legend = c("auto", "separate", "merge", "none"),
  expand = c(0, 0, 0, 0),
  links_palette = "Paired",
  links_palcolor = NULL,
  links_alpha = 0.6,
  legend.box = "vertical",
  keep_empty = TRUE,
  x_text_angle = 0,
  aspect.ratio = 1,
  legend.position = "right",
  legend.direction = "vertical",
  flip = FALSE,
  theme = "theme_this",
  theme_args = list(),
  title = NULL,
  subtitle = NULL,
  xlab = NULL,
  ylab = NULL,
  facet_by = NULL,
  facet_scales = "fixed",
  facet_ncol = NULL,
  facet_nrow = NULL,
  facet_byrow = TRUE,
  ...
)
```

## Arguments

- data:

  A data frame.

- in_form:

  A character string specifying the input data format. One of `"auto"`
  (default), `"long"`, `"lodes"`, `"wide"`, `"alluvia"`, or `"counts"`.
  `"long"` is an alias for `"lodes"`; `"wide"` is an alias for
  `"alluvia"`. See the `data` parameter of
  [`SankeyPlot`](https://pwwang.github.io/plotthis/reference/sankeyplot.md)
  for format descriptions.

- x:

  A character vector of column name(s) for the x-axis categories. Each
  unique value or concatenated pair represents a time point, state, or
  position along the horizontal axis. Behaviour depends on `in_form`:
  for `"lodes"` at least one column is expected; for `"alluvia"` and
  `"counts"` at least two columns are required. In the latter two cases
  `x_sep` is not used.

- x_sep:

  A character string to join multiple `x` columns when `in_form` is
  `"lodes"` or auto-determined as lodes. Default `"_"`.

- y:

  A character string specifying the numeric column for the y-axis
  (frequency / value). When `NULL` (default), the count of observations
  per combination of `x`, `stratum`, `alluvium`, `links_fill_by`, and
  `facet_by` is computed automatically. Ignored when `in_form` is
  `"counts"`.

- stratum:

  A character string specifying the column that defines the node
  categories at each x-axis position. Each unique value becomes a
  stratum (node block) at each x position. When `NULL`, defaults to
  `links_fill_by`. Multiple columns are concatenated with `stratum_sep`.
  Ignored in `"alluvia"` format.

- stratum_sep:

  A character string to join multiple `stratum` columns. Default `"_"`.

- alluvium:

  A character string specifying the column that identifies individual
  flows (alluvia) across x-axis positions. Each unique value represents
  a single observational unit tracked across positions. When `NULL` in
  `"counts"` format, an auto-generated identifier is created. Multiple
  columns are concatenated with `alluvium_sep`. Ignored in `"alluvia"`
  format.

- alluvium_sep:

  A character string to join multiple `alluvium` columns. Default `"_"`.

- flow:

  A logical value. When `FALSE` (default),
  [`geom_alluvium()`](http://corybrunson.github.io/ggalluvial/reference/geom_alluvium.md)
  is used for the links. When `TRUE`,
  [`geom_flow()`](http://corybrunson.github.io/ggalluvial/reference/geom_flow.md)
  is used instead, which draws the flows with a directional gradient
  between x positions.

- nodes_color:

  A character string specifying the border colour of the node (stratum)
  rectangles. Use the special value `".fill"` to match the border colour
  to the node fill colour. Default `"grey30"`.

- links_fill_by:

  A character string specifying the column that determines the fill
  colour of the links (alluvia / flows). When `NULL` in `"lodes"`
  format, defaults to `alluvium`. In `"counts"` format with the `"."`
  prefix, this parameter is required. Multiple columns are concatenated
  with `links_fill_by_sep`.

- links_fill_by_sep:

  A character string to join multiple `links_fill_by` columns. Default
  `"_"`.

- links_name:

  A character string for the legend title of the link fill scale. When
  `NULL` (default), the `links_fill_by` column name is used.

- links_color:

  A character string specifying the border colour of the links (alluvia
  / flows). Use the special value `".fill"` to match the link border
  colour to the link fill colour. Default `"gray80"`.

- nodes_palette:

  A character string specifying the colour palette for the node
  (stratum) fill. Passed to
  [`palette_this()`](https://pwwang.github.io/plotthis/reference/palette_this.md).
  Default `"Paired"`.

- nodes_palcolor:

  A character vector of custom colours for the node fill, used as
  `palcolor` in
  [`palette_this()`](https://pwwang.github.io/plotthis/reference/palette_this.md).
  When `NULL` (default), the palette colours are used directly.

- palreverse:

  A logical value indicating whether to reverse the palette. Default is
  FALSE.

- nodes_alpha:

  A numeric value in \\\[0, 1\]\\ controlling the transparency of the
  node (stratum) fill. Default `1`.

- nodes_label:

  A logical value. When `TRUE`, stratum labels are drawn inside each
  node using
  [`geom_label()`](https://ggplot2.tidyverse.org/reference/geom_text.html)
  with
  [`StatStratum`](http://corybrunson.github.io/ggalluvial/reference/ggalluvial-ggproto.md).
  Default `FALSE`.

- nodes_width:

  A numeric value (typically 0–1) specifying the width of the node
  (stratum) rectangles as a fraction of the x-axis spacing. Default
  `0.25`.

- nodes_label_miny:

  A numeric value specifying the minimum y (frequency) threshold for
  displaying node labels. Nodes with y-values below this threshold are
  not labelled. Default `0`.

- nodes_legend:

  Controls how the node legend is displayed. One of:

  `"auto"` (default)

  :   Automatically determined: if `nodes_label = TRUE`, or if `stratum`
      is identical to `links_fill_by` with matching colours, the legend
      is hidden. Otherwise, overlapping stratum values across x
      positions are checked: any overlap produces a merged legend; no
      overlap produces separate legends per x position.

  `"merge"`

  :   A single merged legend for all nodes.

  `"separate"`

  :   One legend per x-axis position, generated via separate
      [`scale_fill_manual()`](https://ggplot2.tidyverse.org/reference/scale_manual.html)
      layers.

  `"none"`

  :   No node legend is shown.

- expand:

  The values to expand the x and y axes. It is like CSS padding. When a
  single value is provided, it is used for both axes on both sides. When
  two values are provided, the first value is used for the top/bottom
  side and the second value is used for the left/right side. When three
  values are provided, the first value is used for the top side, the
  second value is used for the left/right side, and the third value is
  used for the bottom side. When four values are provided, the values
  are used for the top, right, bottom, and left sides, respectively. You
  can also use a named vector to specify the values for each side. When
  the axis is discrete, the values will be applied as 'add' to the
  'expansion' function. When the axis is continuous, the values will be
  applied as 'mult' to the 'expansion' function. See also
  <https://ggplot2.tidyverse.org/reference/expansion.html>

- links_palette:

  A character string specifying the colour palette for the link fill.
  Passed to
  [`palette_this()`](https://pwwang.github.io/plotthis/reference/palette_this.md).
  Default `"Paired"`.

- links_palcolor:

  A character vector of custom colours for the link fill, used as
  `palcolor` in
  [`palette_this()`](https://pwwang.github.io/plotthis/reference/palette_this.md).
  When `NULL` (default), the palette colours are used directly.

- links_alpha:

  A numeric value in \\\[0, 1\]\\ controlling the transparency of the
  link fill. Default `0.6`.

- legend.box:

  A character string specifying the arrangement of legend boxes, either
  `"vertical"` (default) or `"horizontal"`.

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

- x_text_angle:

  A numeric value specifying the angle of the x-axis text.

- aspect.ratio:

  A numeric value specifying the aspect ratio of the plot.

- legend.position:

  A character string specifying the position of the legend. if
  `waiver()`, for single groups, the legend will be "none", otherwise
  "right".

- legend.direction:

  A character string specifying the direction of the legend.

- flip:

  A logical value. When `TRUE`,
  [`coord_flip()`](https://ggplot2.tidyverse.org/reference/coord_flip.html)
  is applied to swap the x and y axes. Default `FALSE`.

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

- ...:

  Additional arguments passed to
  [`geom_alluvium()`](http://corybrunson.github.io/ggalluvial/reference/geom_alluvium.md)
  or
  [`geom_flow()`](http://corybrunson.github.io/ggalluvial/reference/geom_flow.md),
  depending on the `flow` setting. For `geom_flow` with a distinct
  `links_fill_by` column, passing `stat = "alluvium"` preserves the fill
  variable.

## Value

A `ggplot` object with `height` and `width` attributes (in inches)
attached.

## Architecture

1.  **Format resolution** — `in_form` is matched and aliased (`"long"` →
    `"lodes"`, `"wide"` → `"alluvia"`).

2.  **Data parsing by format** — one of five code paths executes:

    - **Lodes / long** — `x`, `stratum`, `alluvium`, and `links_fill_by`
      are validated via
      [`check_columns()`](https://pwwang.github.io/plotthis/reference/check_columns.md).
      Multi-column inputs are concatenated with their respective
      separators. When `y` is `NULL`, counts are computed per (`x`,
      `stratum`, `alluvium`, `links_fill_by`, `facet_by`).

    - **Counts** (x without `"."` prefix) — numeric `x` columns are
      pivoted to long form via
      [`pivot_longer()`](https://tidyr.tidyverse.org/reference/pivot_longer.html),
      creating the y-axis from the cell values. `alluvium` and `stratum`
      default to `links_fill_by`.

    - **Counts with source node** (x with `"."` as first element,
      `is_flowcounts`) — same as counts but also injects `links_fill_by`
      values as an additional first column of nodes, visualising how
      flows originate from source groups.

    - **Alluvia / wide** — validated via
      [`is_alluvia_form()`](http://corybrunson.github.io/ggalluvial/reference/alluvial-data.md)
      and converted to lodes form via
      [`to_lodes_form()`](http://corybrunson.github.io/ggalluvial/reference/alluvial-data.md).
      `stratum` and `alluvium` are ignored.

    - **Auto / fallback** — when no other branch matches, the lodes path
      is attempted and validated via
      [`is_lodes_form()`](http://corybrunson.github.io/ggalluvial/reference/alluvial-data.md).

3.  **Palette assignment** —
    [`palette_this()`](https://pwwang.github.io/plotthis/reference/palette_this.md)
    resolves colours for both nodes (`nodes_palette` / `nodes_palcolor`)
    and links (`links_palette` / `links_palcolor`).

4.  **Flow-counts guide logic** — when `is_flowcounts` is `TRUE`, the
    links guide is suppressed if the first-column node colours match the
    link colours, avoiding redundant legends. When the palettes are
    identical but colours differ (too few colours in the palette), the
    first-column node colours are reused.

5.  **Legend auto-detection** — when `nodes_legend = "auto"`: if
    `nodes_label = TRUE` or if `stratum` and `links_fill_by` share
    identical values and colours, the nodes legend is hidden. Otherwise,
    overlapping stratum values across x-axis positions are checked: any
    overlap triggers a merged legend; no overlap produces separate
    legends per position.

6.  **Base ggplot** — constructed with
    `aes(x = x, stratum = stratum, alluvium = alluvium, y = y)`.

7.  **Separate node fill layers** — when `stratum` differs from
    `links_fill_by` and `nodes_legend = "separate"`, a
    [`geom_col()`](https://ggplot2.tidyverse.org/reference/geom_bar.html) +
    [`scale_fill_manual()`](https://ggplot2.tidyverse.org/reference/scale_manual.html)
    layer pair is added per x-axis position, each followed by
    [`new_scale_fill()`](https://eliocamp.github.io/ggnewscale/reference/new_scale.html)
    to produce independent legends.

8.  **Link rendering** —
    [`geom_alluvium()`](http://corybrunson.github.io/ggalluvial/reference/geom_alluvium.md)
    (default) or
    [`geom_flow()`](http://corybrunson.github.io/ggalluvial/reference/geom_flow.md)
    when `flow = TRUE`. When `links_color = ".fill"`, the colour
    aesthetic is mapped to `links_fill_by` and the colour scale guide is
    suppressed. For `geom_flow` with a distinct `stratum` and
    `links_fill_by`, `stat = "alluvium"` is forced to preserve the fill
    variable through the flow stat transformation.

9.  **Link fill scale** —
    [`scale_fill_manual()`](https://ggplot2.tidyverse.org/reference/scale_manual.html)
    with `links_colors` and the resolved `links_guide`.

10. **Node rendering** —
    [`geom_stratum()`](http://corybrunson.github.io/ggalluvial/reference/geom_stratum.md)
    with `nodes_color` (border) and `nodes_alpha`. When
    `nodes_color = ".fill"`, the colour aesthetic maps to `stratum`.

11. **Node fill scale** —
    [`scale_fill_manual()`](https://ggplot2.tidyverse.org/reference/scale_manual.html)
    with `nodes_colors`. The guide is `"none"` when `nodes_legend` is
    `"none"` or `"separate"` (already handled by per-position layers);
    otherwise `"legend"`.

12. **Labels** — when `nodes_label = TRUE`,
    [`geom_label()`](https://ggplot2.tidyverse.org/reference/geom_text.html)
    with
    `stat = `[`StatStratum`](http://corybrunson.github.io/ggalluvial/reference/ggalluvial-ggproto.md)
    and `min.y = nodes_label_miny`.

13. **Axes, theme, labels** —
    [`scale_x_discrete()`](https://ggplot2.tidyverse.org/reference/scale_discrete.html),
    [`scale_y_continuous()`](https://ggplot2.tidyverse.org/reference/scale_continuous.html),
    custom theme, plot title / subtitle, axis labels, and aesthetic
    adjustments (aspect ratio, legend position / direction, grid
    removal, x-text angle).

14. **Coordinate flip** — when `flip = TRUE`,
    [`coord_flip()`](https://ggplot2.tidyverse.org/reference/coord_flip.html)
    swaps the axes.

15. **Dimension calculation** —
    [`calculate_plot_dimensions()`](https://pwwang.github.io/plotthis/reference/calculate_plot_dimensions.md)
    computes `height` and `width` attributes from the number of x-axis
    positions, legend metrics, and flip state, scaled by `aspect.ratio`.

16. **Faceting** —
    [`facet_plot()`](https://pwwang.github.io/plotthis/reference/facet_plot.md)
    wraps the result when `facet_by` is provided.
