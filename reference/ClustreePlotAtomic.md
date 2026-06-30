# Atomic clustree plot

Core implementation for clustree (clustering tree) plots. This internal
function renders a tree-based visualisation showing how cluster
assignments change across increasing clustering resolutions. It is
called by the exported
[`ClustreePlot()`](https://pwwang.github.io/plotthis/reference/ClustreePlot.md)
function.

## Usage

``` r
ClustreePlotAtomic(
  data,
  prefix,
  flip = FALSE,
  alpha = 0.85,
  palette = "Paired",
  palcolor = NULL,
  palreverse = FALSE,
  edge_palette = "Spectral",
  edge_palcolor = NULL,
  aspect.ratio = 1,
  legend.position = "right",
  legend.direction = "vertical",
  title = NULL,
  subtitle = NULL,
  xlab = NULL,
  ylab = NULL,
  expand = c(0.1, 0.1),
  theme = "theme_this",
  theme_args = list(),
  ...
)
```

## Arguments

- data:

  A data frame.

- prefix:

  A character string specifying the common prefix of the resolution
  columns in `data`. All columns whose names start with this prefix are
  selected as resolution columns. The suffix after the prefix is parsed
  as a numeric resolution value. Supports `"_"` and `"."` as separators
  between the prefix and the resolution value (e.g. `"res_0.5"` or
  `"p.0.5"`).

- flip:

  A logical value. If `TRUE`, the tree is flipped so that resolutions
  are displayed on the x-axis (left to right) and cluster assignments
  are shown as row labels on the y-axis. Default: `FALSE`.

- alpha:

  A numeric value in `[0, 1]` specifying the transparency of the nodes
  in the clustree plot. Only used when `node_alpha` is not explicitly
  provided via `...`. Default: `0.85`.

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

- edge_palette:

  A character string specifying the palette name for the edge colour
  gradient. Edges are coloured by the number of transitioning cells
  between clusters at adjacent resolutions, using
  [`ggraph::scale_edge_color_gradientn()`](https://ggraph.data-imaginist.com/reference/scale_edge_colour.html).
  Default: `"Spectral"`.

- edge_palcolor:

  A character vector of custom colours for the edge colour gradient.
  When `NULL` (the default), colours are derived from `edge_palette`.

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

- theme:

  A character string or a theme class (i.e. ggplot2::theme_classic)
  specifying the theme to use. Default is "theme_this".

- theme_args:

  A list of arguments to pass to the theme function.

- ...:

  Additional arguments passed to
  [`clustree::clustree()`](https://lazappi.github.io/clustree/reference/clustree.html).
  Commonly used overrides include `node_size_range`, `node_text_size`,
  `layout` (default: `"sugiyama"`), `show_axis`, and `node_text_colour`.
  Note that `x` (the data) and `prefix` are set internally and cannot be
  overridden here.

## Value

A `ggplot` object with `height` and `width` attributes.

## Details

The function expects a data frame containing columns named with a common
`prefix` followed by numeric resolution values (e.g. `"res_0.1"`,
`"res_0.3"`, `"res_0.5"`). Each column represents cluster assignments at
a given resolution, and the tree visualises how cells (rows) transition
between clusters as resolution increases.

The core layout is produced via
[`clustree::clustree()`](https://lazappi.github.io/clustree/reference/clustree.html),
which computes a `ggraph` layout (default: `"sugiyama"`) linking
clusters across adjacent resolutions. Nodes represent clusters and edges
represent cell transitions; edge width and colour encode the number of
transitioning cells. Cluster labels are overlaid on each node via
[`ggplot2::geom_text()`](https://ggplot2.tidyverse.org/reference/geom_text.html).

Key features:

- **Resolution-level node colouring** — each resolution level receives a
  distinct colour from `palette` via
  [`ggplot2::scale_color_manual()`](https://ggplot2.tidyverse.org/reference/scale_manual.html).

- **Edge gradient** — edges are coloured by transition count using
  [`ggraph::scale_edge_color_gradientn()`](https://ggraph.data-imaginist.com/reference/scale_edge_colour.html)
  with the `edge_palette` gradient.

- **Flip support** — `flip = TRUE` places resolutions on the x-axis with
  cluster IDs as y-axis row labels for left-to-right reading.

- **Automatic dimension calculation** — plot height and width are sized
  based on the number of resolutions, number of clusters, and legend
  configuration, with different formulae for flipped vs. non-flipped
  layouts.

## Architecture

**ClustreePlotAtomic** executes the following steps:

1.  **Column selection** — selects all columns whose names start with
    `prefix` via
    [`dplyr::starts_with()`](https://tidyselect.r-lib.org/reference/starts_with.html).

2.  **NA filtering** — removes rows with any missing values via
    [`stats::complete.cases()`](https://rdrr.io/r/stats/complete.cases.html).

3.  **Expansion normalisation** —
    [`norm_expansion()`](https://pwwang.github.io/plotthis/reference/norm_expansion.md)
    parses the `expand` argument for continuous x and y axes.

4.  **Empty data guard** — stops with an informative error if no columns
    match the prefix or no complete rows remain.

5.  **Resolution parsing** — extracts the suffix after `prefix` from
    each column name and strips leading separators (`"_"` or `"."`) if
    present. Values are converted to numeric and sorted. Column names
    are rewritten to use the original prefix for consistent `clustree`
    processing.

6.  **Cluster counting** — computes the number of unique clusters at the
    highest resolution (`max_clusters`), used for dimension calculation
    in the non-flipped layout.

7.  **Argument merging** — builds a call to
    [`clustree::clustree()`](https://lazappi.github.io/clustree/reference/clustree.html)
    by merging user-supplied `...` with sensible defaults for
    `node_alpha`, `edge_width`, `show_axis`, `layout` (`"sugiyama"`),
    `node_size_range` (`c(6, 12)`), `node_text_size` (`3`), and
    `node_text_colour` (`"black"`).

8.  **clustree execution** — calls
    `gglogger::register(clustree::clustree)` with the merged arguments
    to produce the base `ggplot` object.

9.  **Node labels** — overlays
    [`ggplot2::geom_text()`](https://ggplot2.tidyverse.org/reference/geom_text.html)
    with the cluster label from the `clustree` output, using the
    configured text size and colour.

10. **Resolution colour scale** — applies
    [`ggplot2::scale_color_manual()`](https://ggplot2.tidyverse.org/reference/scale_manual.html)
    with one colour per resolution level using
    [`palette_this()`](https://pwwang.github.io/plotthis/reference/palette_this.md).
    The colour legend is suppressed (the edge gradient legend serves as
    the primary guide).

11. **Edge colour scale** — applies
    [`ggraph::scale_edge_color_gradientn()`](https://ggraph.data-imaginist.com/reference/scale_edge_colour.html)
    with the `edge_palette` palette, encoding transition counts via a
    colour-bar guide with black frame and ticks.

12. **Theme and labels** — applies the selected theme and sets `title`,
    `subtitle`, x-axis label, and y-axis label (default y-label is the
    `prefix` with trailing separator stripped).

13. **Flip branch** (`flip = TRUE`):

    - Resolutions are placed on the x-axis (horizontal); clusters become
      y-axis rows.

    - Width scales with `max(3, 0.5 + nres * 1.0)`; height scales with
      `max(3, 0.5 + max_clusters * 0.5)` (or via aspect.ratio).

    - `scale_y_reverse()` + `coord_flip()` reorients the layout so
      resolutions read left-to-right.

    - Horizontal grid lines are dashed grey80; y-axis text and ticks are
      suppressed.

14. **Non-flip branch** (`flip = FALSE`, default):

    - Resolutions are on the y-axis (rows); clusters spread across the
      x-axis (columns).

    - Width scales with `max(3, 0.5 + max_clusters * 0.5)`; height
      scales with `max(3, 0.5 + nres * 1.0)` (or via aspect.ratio).

    - `coord_cartesian(clip = "off")` and `scale_y_continuous()` show
      resolution labels on the y-axis.

    - Vertical grid lines are dashed grey80; x-axis text and ticks are
      suppressed.

15. **Dimension storage** —
    [`calculate_plot_dimensions()`](https://pwwang.github.io/plotthis/reference/calculate_plot_dimensions.md)
    computes `height` and `width` attributes, stored on the `ggplot`
    object.
