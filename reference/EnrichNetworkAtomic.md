# Atomic enrichment network (internal)

Core implementation for drawing a single enrichment network – a
term-gene bipartite graph where enriched terms and their member genes
are shown as interconnected nodes. Term nodes are displayed as numbered
filled circles with a colour-coded legend; gene nodes are displayed as
labelled rectangles coloured by a blend of the term colours they belong
to. This is the workhorse behind the exported
[`EnrichNetwork`](https://pwwang.github.io/plotthis/reference/enrichmap1.md)
function – it takes a **single** data frame (no `split_by` support) and
returns a `ggplot` object.

The function constructs a bipartite graph between terms and genes,
computes a force-directed layout, optionally adjusts node positions to
reduce overlap, blends term colours for shared genes, and renders the
result as a labelled network.

## Usage

``` r
EnrichNetworkAtomic(
  data,
  top_term = 6,
  metric = "p.adjust",
  character_width = 50,
  layout = "fr",
  layoutadjust = TRUE,
  adjscale = 60,
  adjiter = 100,
  blendmode = "blend",
  labelsize = 5,
  theme = "theme_this",
  theme_args = list(),
  palette = "Paired",
  palcolor = NULL,
  palreverse = FALSE,
  alpha = 1,
  aspect.ratio = 1,
  legend.position = "right",
  legend.direction = "vertical",
  title = NULL,
  subtitle = NULL,
  xlab = NULL,
  ylab = NULL,
  seed = 8525,
  ...
)
```

## Arguments

- data:

  A data frame containing enrichment results in clusterProfiler format
  (see
  [`EnrichMapAtomic`](https://pwwang.github.io/plotthis/reference/EnrichMapAtomic.md)
  for the expected columns).

- top_term:

  An integer specifying the maximum number of terms to include. Terms
  are ranked by `metric` (ascending). Default `6`.

- metric:

  A character string specifying the significance metric for top-term
  selection: `"p.adjust"` (default) or `"pvalue"`.

- character_width:

  An integer specifying the maximum width (in characters) at which term
  descriptions are wrapped via `strwrap(width = character_width)`.
  Default `50`.

- layout:

  A character string naming the igraph layout algorithm. Built-in
  shortcuts: `"circle"`, `"tree"`, `"grid"`. Otherwise, the suffix
  passed to `layout_with_<layout>` in igraph. Default `"fr"`.

- layoutadjust:

  A logical value. When `TRUE` (default), applies
  [`adjust_network_layout()`](https://pwwang.github.io/plotthis/reference/adjust_network_layout.md)
  after the initial layout to reduce node overlap based on label width
  and a repulsion simulation.

- adjscale:

  A numeric value controlling the scale of the layout adjustment. Passed
  as the `scale` argument to
  [`adjust_network_layout()`](https://pwwang.github.io/plotthis/reference/adjust_network_layout.md).
  Default `60`.

- adjiter:

  A numeric value controlling the number of iterations for the layout
  adjustment. Passed as the `iter` argument to
  [`adjust_network_layout()`](https://pwwang.github.io/plotthis/reference/adjust_network_layout.md).
  Default `100`.

- blendmode:

  A character string specifying how gene colours are computed from the
  colours of the terms they belong to. One of `"blend"` (default),
  `"average"`, `"multiply"`, or `"screen"`. Passed to
  [`blend_colors()`](https://pwwang.github.io/plotthis/reference/blend_colors.md).

- labelsize:

  A numeric value specifying the font size of the numeric term labels
  displayed via
  [`ggrepel::geom_text_repel()`](https://ggrepel.slowkow.com/reference/geom_text_repel.html).
  Default `5`.

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

- ...:

  Additional arguments.

## Value

A `ggplot` object with `height` and `width` attributes (in inches)
attached.

## Architecture

1.  **ggplot dispatch** – selects `gglogger::ggplot` or
    [`ggplot2::ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)
    based on `getOption("plotthis.gglogger.enabled")`.

2.  **Data format conversion** – if `data` inherits from
    `"enrichResult"`, converts via
    [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html).

3.  **Column validation** –
    [`check_columns()`](https://pwwang.github.io/plotthis/reference/check_columns.md)
    verifies that `Description`, `GeneRatio`, `pvalue`, `p.adjust`, and
    `geneID` are present.

4.  **Top-term selection** – when `top_term` is not `NULL`, selects the
    top N terms by `metric` via
    [`dplyr::slice_min()`](https://dplyr.tidyverse.org/reference/slice.html).

5.  **Metric and description preparation** – computes
    \\-\log\_{10}(metric)\\ as the scoring variable. Wraps `Description`
    text to `character_width` and parses `geneID` by splitting on `"/"`.

6.  **Gene-term unnesting** – unnests the `geneID` column to produce a
    gene-term mapping table.

7.  **Bipartite node construction** – creates nodes for both terms
    (`class = "term"`) and genes (`class = "gene"`), carrying the
    `Database` attribute if present.

8.  **Bipartite edge construction** – edges connect each term to its
    member genes with uniform weight (`weight = 1`).

9.  **igraph graph construction** –
    [`igraph::graph_from_data_frame()`](https://r.igraph.org/reference/graph_from_data_frame.html)
    builds an undirected bipartite graph.

10. **Layout computation** – dispatches to the chosen igraph layout
    function (`layout_with_*`) or built-in shortcuts (`"circle"`,
    `"tree"`, `"grid"`).

11. **Layout adjustment** – when `layoutadjust = TRUE` (default), calls
    [`adjust_network_layout()`](https://pwwang.github.io/plotthis/reference/adjust_network_layout.md)
    to push overlapping nodes apart based on label width and a repulsion
    simulation.

12. **Node coordinates** – extracts vertex positions into `dim1` and
    `dim2` columns.

13. **Colour computation** – palette colours are assigned to term nodes.
    Gene node colours are computed by blending the colours of all
    connected terms via the specified `blendmode` using
    [`blend_colors()`](https://pwwang.github.io/plotthis/reference/blend_colors.md).

14. **Label colour** – each node's text colour is chosen as black or
    white based on the luminance of its fill colour (sum of RGB channels
    \> 510).

15. **Numeric labels** – term nodes receive sequential integer labels;
    gene nodes display their gene symbol.

16. **Custom legend key** – `draw_key_cust()` renders term legend
    entries as numbered circles via `ggrepel::shadowtextGrob()`.

17. **Edge rendering** –
    [`ggplot2::geom_segment()`](https://ggplot2.tidyverse.org/reference/geom_segment.html)
    draws edges coloured by the source term (legend suppressed).

18. **Gene node rendering** –
    [`ggplot2::geom_label()`](https://ggplot2.tidyverse.org/reference/geom_text.html)
    displays gene symbols with fill colour blended from connected terms.

19. **Term node rendering** – two `geom_point()` layers draw
    black-outlined circles filled with the term colour, with
    `draw_key_cust` as the key glyph.

20. **Term labels** –
    [`ggrepel::geom_text_repel()`](https://ggrepel.slowkow.com/reference/geom_text_repel.html)
    places the numeric term labels with white text on a black
    background.

21. **Scale configuration** – `scale_color_identity()` and
    `scale_fill_identity()` with a manual legend mapping term colours to
    term descriptions.

22. **Theme and dimensions** – applies the resolved theme, sets aspect
    ratio and legend position, then calls
    [`calculate_plot_dimensions()`](https://pwwang.github.io/plotthis/reference/calculate_plot_dimensions.md)
    to attach `height`/`width` attributes.
