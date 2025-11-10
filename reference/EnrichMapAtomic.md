# Atomic Enrichment Map

Atomic Enrichment Map

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

  A data frame containing the data to be plotted. It should be in the
  format of clusterProfiler enrichment result, which includes the
  columns: ID, Description, GeneRatio, BgRatio, pvalue, p.adjust,
  qvalue, geneID and Count.

  - The `ID`, `qvalue`, `BgRatio`, and `Count` columns are optional.

  - The `Description` is the description of the term.

  - The `GeneRatio` is the number of genes in the term divided by the
    total number of genes in the input list.

  - The `BgRatio` is the number of genes in the term divided by the
    total number of genes in the background list (all terms).

  - The `Count` column, if given, should be the same as the first number
    in GeneRatio.

- top_term:

  An integer specifying the number of top terms to show.

- metric:

  A character string specifying the metric to use for the size of the
  nodes. It is also used to order the terms when selected the top terms.
  Either "pvalue" or "p.adjust". The default is "p.adjust".

- layout:

  A character string specifying the layout of the graph. Either
  "circle", "tree", "grid" or other layout functions in `igraph`.

- minchar:

  An integer specifying the minimum number of characters to show in the
  keyword.

- cluster:

  A character string specifying the clustering method. Either
  "fast_greedy", "walktrap", "edge_betweenness", "infomap" or other
  clustering functions in `igraph`.

- show_keyword:

  A logical value specifying whether to show the keyword instead of
  Description/Term in the plot.

- nlabel:

  An integer specifying the number of labels to show in each cluster.

- character_width:

  The width of the characters used to wrap the keyword.

- words_excluded:

  A character vector specifying the words to exclude in the keyword.

- mark:

  A character string specifying the mark to use for the nodes. Either
  "ellipse", "rect", "circle", "text" or other mark functions in
  `ggforce`.

- label:

  A character string specifying the label to show in the legend. Either
  "term" or "feature". The default is "term".

- labelsize:

  A numeric value specifying the size of the label.

- expand:

  A numeric vector of length 2 specifying the expansion of the x and y
  axis.

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

A ggplot object
