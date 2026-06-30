# Atomic GSEA plot (internal)

Core implementation for drawing a single GSEA (Gene Set Enrichment
Analysis) ridge plot. This is the workhorse behind the exported
[`GSEAPlot`](https://pwwang.github.io/plotthis/reference/gsea.md)
function — it takes a single gene set and produces a three-panel figure
showing the running enrichment score, hit positions, and the ranked list
metric.

The plot consists of three vertically stacked panels:

1.  **Running score panel** — traces the running enrichment score across
    the ranked gene list, with a peak annotation and optional gene
    labels for core enrichment genes.

2.  **Hit indicator panel** — marks the positions of gene set members
    with vertical lines, colour-coded by the rank metric value (red for
    positive, blue for negative).

3.  **Gene ranking panel** — shows every gene's rank as a vertical
    segment, with annotations indicating positively and negatively
    correlated tails and the zero-crossing point.

The running enrichment score is computed via
[`gsea_running_score()`](https://pwwang.github.io/plotthis/reference/gsea_running_score.md)
using the `gene_ranks` and `genes` provided. Core enrichment genes (from
the `core_enrichment` column) can be labelled on the score panel using
[`geom_text_repel()`](https://ggrepel.slowkow.com/reference/geom_text_repel.html).

## Usage

``` r
GSEAPlotAtomic(
  data,
  gene_ranks = "@gene_ranks",
  gs,
  genes,
  metric = "p.adjust",
  sample_coregenes = FALSE,
  line_width = 1.5,
  line_alpha = 1,
  line_color = "#6BB82D",
  n_coregenes = 10,
  genes_label = NULL,
  label_fg = "black",
  label_bg = "white",
  label_bg_r = 0.1,
  label_size = 4,
  title = NULL,
  subtitle = NULL,
  xlab = NULL,
  ylab = NULL,
  ...
)
```

## Arguments

- data:

  A data frame.

- gene_ranks:

  A named numeric vector of gene-level rank statistics, with gene
  identifiers as names. Used to calculate the running enrichment score
  and the ranked list metric. If a character string starting with `"@"`,
  the attribute of `data` with that name (minus the `"@"`) is used.

- gs:

  Character string specifying the `ID` of the gene set to plot. Must
  match a value in `data$ID`.

- genes:

  Character vector of gene identifiers belonging to the gene set. These
  are intersected with `names(gene_ranks)` for the running score
  calculation.

- metric:

  Character string specifying the column name in `data` used to compute
  significance stars in the subtitle. Default is `"p.adjust"`.

- sample_coregenes:

  Logical; if `TRUE`, core enrichment genes are sampled randomly for
  labelling. If `FALSE` (default), the first `n_coregenes` core
  enrichment genes are used.

- line_width:

  Numeric specifying the line width for the running enrichment score
  curve. Default is `1.5`.

- line_alpha:

  Numeric alpha transparency for the running score line and hit
  indicator bars. Default is `1`.

- line_color:

  Character string specifying the colour of the running enrichment score
  line. Default is `"#6BB82D"`.

- n_coregenes:

  Integer specifying the number of core enrichment genes to label on the
  running score plot. Default is `10`. Ignored when `genes_label` is
  provided.

- genes_label:

  Character vector of specific gene names to label on the running score
  plot. When provided, `n_coregenes` is ignored.

- label_fg:

  Character string specifying the text colour of gene labels. Default is
  `"black"`.

- label_bg:

  Character string specifying the background colour of gene labels.
  Default is `"white"`.

- label_bg_r:

  Numeric specifying the corner radius of the label background. Default
  is `0.1`.

- label_size:

  Numeric specifying the font size of the label text. Default is `4`.

- title:

  A character string specifying the title of the plot. A function can be
  used to generate the title based on the default title. This is useful
  when split_by is used and the title needs to be dynamic.

- subtitle:

  A character string specifying the subtitle of the plot.

- xlab:

  A character string specifying the x-axis label.

- ylab:

  Character string for a right-side y-axis label (rotated 90 degrees).
  When provided, an extra text grob is added to the right of the
  assembled plot, and the plot width increases from 7.5 to 8 inches.

- ...:

  Additional arguments.

## Value

A `patchwork` object with `height` and `width` attributes (in inches)
attached.

## Architecture

1.  **ggplot dispatch** — selects `gglogger::ggplot` or
    [`ggplot2::ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)
    based on `getOption("plotthis.gglogger.enabled")`.

2.  **Data subsetting** — if `data` contains multiple gene sets
    (`nrow > 1`), it is filtered to the single row matching `gs`. An
    error is raised if no rows match.

3.  **Gene rank resolution** — resolves `gene_ranks` from its
    `"@"`-prefixed attribute reference, validates that it is a named
    numeric vector, and sorts it in descending order.

4.  **Title and subtitle construction** — `title` defaults to
    `data$Description`. `subtitle` is built from the NES value and the
    `metric` column with significance stars (`ns/*/**/***/****`).

5.  **Plot data preparation** — builds a data frame with position (1:N),
    gene names, running enrichment score (via
    [`gsea_running_score()`](https://pwwang.github.io/plotthis/reference/gsea_running_score.md)
    with `hits_only = FALSE`), raw ranks, and a hit indicator column.

6.  **Running score panel** — renders the top panel with a red/blue
    background (positive/negative score regions), a horizontal baseline
    at `y = 0`, the running score line, and annotations for the peak
    (dashed reference lines and an upward/downward triangle). Title and
    subtitle are added via
    [`ggtitle()`](https://ggplot2.tidyverse.org/reference/labs.html).

7.  **Gene labeling** — when `n_coregenes > 1` or `genes_label` is
    provided, core enrichment genes are extracted from
    `data$core_enrichment` and labelled on the running score panel using
    `geom_text_repel()`. Missing genes trigger a warning.

8.  **Hit indicator panel** — renders the middle panel: vertical lines
    (`geom_linerange`) at each hit position, with coloured rectangles
    beneath showing the rank metric gradient (red for positive, blue for
    negative).

9.  **Gene ranking panel** — renders the bottom panel: vertical segments
    (`geom_segment`) for every gene's rank, with annotations for
    positively/negatively correlated tails. If both signs exist, the
    zero-crossing point is marked with a dashed vertical line.

10. **Panel assembly** — the three panels are stacked vertically via
    [`wrap_plots()`](https://patchwork.data-imaginist.com/reference/wrap_plots.html)
    with spacer panels and proportional heights (`3.5 / 1 / 1.5`). When
    `ylab` is provided, a rotated
    [`textGrob()`](https://rdrr.io/r/grid/grid.text.html) is added to
    the right side and the plot width increases from 7.5 to 8 inches.

11. **Dimensions** — height is fixed at 6.5 inches. Width is 7.5 inches
    (without `ylab`) or 8 inches (with `ylab`). Attributes `height` and
    `width` are stored on the returned object.
