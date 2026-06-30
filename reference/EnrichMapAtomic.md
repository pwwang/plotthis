# Atomic enrichment map (internal)

Core implementation for drawing a single enrichment map – a gene-set
similarity network where nodes represent enriched terms, node fill
colour encodes cluster membership, node size encodes the number of genes
per term, and edge thickness encodes the number of overlapping genes
between pairs of terms. This is the workhorse behind the exported
[`EnrichMap`](https://pwwang.github.io/plotthis/reference/enrichmap1.md)
function – it takes a **single** data frame (no `split_by` support) and
returns a `ggplot` object.

The function constructs an undirected graph from the term-term gene
overlap matrix, computes a force-directed layout, detects term clusters
via igraph community detection, and renders the result as a labelled
network plot with ggforce hull annotations around each cluster.

## Usage

``` r
EnrichMapAtomic(
  data,
  in_form = "clusterProfiler",
  top_term = 100,
  metric = "p.adjust",
  layout = "fr",
  minchar = 2,
  cluster = "fast_greedy",
  show_keyword = FALSE,
  nlabel = 4,
  character_width = 50,
  words_excluded = plotthis::words_excluded,
  mark = "ellipse",
  label = c("term", "feature"),
  labelsize = 5,
  expand = c(0.4, 0.4),
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
  with at least the columns: `Description`, `GeneRatio`, `pvalue`,
  `p.adjust`, and `geneID`.

  - `ID`, `qvalue`, `BgRatio`, and `Count` are optional.

  - `Description` is the term description, displayed as the default
    keyword.

  - `GeneRatio` is the fraction of input genes annotated to the term
    (e.g. `"10/500"`).

  - `BgRatio` is the fraction of background genes annotated to the term
    (e.g. `"50/20000"`).

  - `Count`, if given, must equal the numerator of `GeneRatio`.

  - `geneID` contains gene symbols separated by `"/"`.

- in_form:

  A character string specifying the input format. When `"auto"`
  (default), the function infers the format from the column names:
  clusterProfiler columns (`pvalue`, `p.adjust`, `qvalue`) or Enrichr
  columns (`P.value`, `Adjusted.P.value`). Other options are
  `"clusterProfiler"` and `"enrichr"`.

- top_term:

  An integer specifying the maximum number of terms to include. Terms
  are ranked by `metric` (ascending). Default `100`.

- metric:

  A character string specifying the significance metric used for
  top-term selection and node scoring: `"p.adjust"` (default) or
  `"pvalue"`. The value is transformed as \\-\log\_{10}(metric)\\.

- layout:

  A character string naming the igraph layout algorithm. Built-in
  shortcuts: `"circle"`, `"tree"`, `"grid"`. Otherwise, the suffix
  passed to `layout_with_<layout>` in igraph (e.g. `"fr"` for
  Fruchterman-Reingold, `"kk"` for Kamada-Kawai). Default `"fr"`.

- minchar:

  An integer specifying the minimum character length for words to be
  included as keywords when `show_keyword = TRUE`. Default `2`.

- cluster:

  A character string naming the igraph community detection algorithm.
  The suffix passed to `cluster_<cluster>` in igraph (e.g.
  `"fast_greedy"`, `"walktrap"`, `"edge_betweenness"`, `"infomap"`).
  Default `"fast_greedy"`.

- show_keyword:

  A logical value. When `TRUE`, the `Description` text is tokenized and
  the most significant words per cluster are shown as keywords. When
  `FALSE` (default), the original term descriptions are used as labels.

- nlabel:

  An integer specifying the number of keywords or term descriptions to
  show per cluster in the legend labels. Default `4`.

- character_width:

  An integer specifying the maximum width (in characters) at which
  keyword labels are wrapped via `strwrap(width = character_width)`.
  Default `50`.

- words_excluded:

  A character vector of words to exclude from keyword extraction when
  `show_keyword = TRUE`. Defaults to
  [`plotthis::words_excluded`](https://pwwang.github.io/plotthis/reference/words_excluded.md).

- mark:

  A character string naming the ggforce hull function. One of
  `"ellipse"` (default), `"rect"`, `"circle"`, or `"text"` – passed as
  the suffix to `geom_mark_<mark>`.

- label:

  A character string specifying what information to display in the
  legend labels. Either `"term"` (default; shows top term
  descriptions/keywords per cluster) or `"feature"` (shows top gene
  symbols per cluster).

- labelsize:

  A numeric value specifying the font size of the cluster labels drawn
  by the ggforce mark layer. Default `5`.

- expand:

  A numeric vector of length 2 (or 4) specifying the expansion of the x
  and y axes, processed via
  [`norm_expansion()`](https://pwwang.github.io/plotthis/reference/norm_expansion.md).
  Default `c(0.4, 0.4)`.

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
    [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html); if
    `in_form == "enrichr"`, calls
    [`prepare_enrichr_result()`](https://pwwang.github.io/plotthis/reference/prepare_enrichr_result.md)
    to rename columns and infer GeneRatio/BgRatio.

3.  **Column validation** –
    [`check_columns()`](https://pwwang.github.io/plotthis/reference/check_columns.md)
    verifies that `Description`, `GeneRatio`, `pvalue`, `p.adjust`, and
    `geneID` are present.

4.  **Top-term selection** – when `top_term` is not `NULL`, selects the
    top N terms by the chosen `metric` via
    [`dplyr::slice_min()`](https://dplyr.tidyverse.org/reference/slice.html).

5.  **Metric transformation** – computes \\-\log\_{10}(metric)\\ as the
    node scoring variable. Splits `geneID` on `"/"` to obtain per-term
    gene lists.

6.  **ID assignment** – uses the `ID` column if present; otherwise falls
    back to row names or generates synthetic IDs (`"GS1"`, `"GS2"`,
    ...).

7.  **Edge construction** – creates all pairwise combinations
    ([`utils::combn()`](https://rdrr.io/r/utils/combn.html)) of term IDs
    and computes the overlap count (intersection of geneID elements) as
    edge weight. Edges with weight 0 are dropped.

8.  **igraph graph construction** –
    [`igraph::graph_from_data_frame()`](https://r.igraph.org/reference/graph_from_data_frame.html)
    builds an undirected graph from the edge list, with node attributes
    from the term data.

9.  **Layout computation** – dispatches to the chosen igraph layout
    function (`layout_with_*`) or one of the built-in shortcuts
    (`"circle"`, `"tree"`, `"grid"`).

10. **Cluster detection** – runs the chosen igraph clustering algorithm
    (`cluster_*`) to group related terms into modules.

11. **Node coordinates** – extracts vertex positions and cluster
    assignments into a data frame with `dim1`, `dim2`, and `clusters`
    columns.

12. **Keyword extraction** – when `show_keyword = TRUE`, tokenizes
    `Description` text, filters words by `minchar` and `words_excluded`,
    scores remaining words by summed `metric` per cluster, and keeps the
    top `nlabel` keywords. Otherwise, the term `Description` texts
    themselves are used as keywords.

13. **Gene keyword extraction** – always computes per-cluster gene-level
    keywords (top `nlabel` genes by summed `metric`) for the feature
    legend mode.

14. **Mark layer** – `ggforce::geom_mark_*()` (ellipse, rect, circle, or
    text) draws cluster hulls with labels containing the cluster ID and
    either term or feature keywords.

15. **Edge rendering** –
    [`ggplot2::geom_segment()`](https://ggplot2.tidyverse.org/reference/geom_segment.html)
    draws edges with line width proportional to overlap weight.

16. **Node rendering** – `ggplot2::geom_point(shape = 21)` draws nodes
    sized by `Count` and filled by cluster membership.

17. **Scale configuration** – `scale_size()` for node size,
    `scale_linewidth()` for edge width, `scale_fill_manual()` for
    cluster colours with custom legend labels (term keywords or gene
    keywords depending on `label`).

18. **Theme and dimensions** – applies the resolved theme, sets aspect
    ratio and legend position, then calls
    [`calculate_plot_dimensions()`](https://pwwang.github.io/plotthis/reference/calculate_plot_dimensions.md)
    to attach `height`/`width` attributes.
