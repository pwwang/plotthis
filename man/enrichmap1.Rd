% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/enrich.R
\name{EnrichMap}
\alias{EnrichMap}
\alias{EnrichNetwork}
\title{Enrichment Map/Network}
\usage{
EnrichMap(
  data,
  in_form = c("auto", "clusterProfiler", "clusterprofiler", "enrichr"),
  split_by = NULL,
  split_by_sep = "_",
  top_term = 10,
  metric = "p.adjust",
  layout = "fr",
  minchar = 2,
  cluster = "fast_greedy",
  show_keyword = FALSE,
  nlabel = 4,
  character_width = 50,
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
  combine = TRUE,
  nrow = NULL,
  ncol = NULL,
  byrow = TRUE,
  axes = NULL,
  axis_titles = axes,
  guides = NULL,
  design = NULL,
  ...
)

EnrichNetwork(
  data,
  in_form = c("auto", "clusterProfiler", "clusterprofiler", "enrichr"),
  split_by = NULL,
  split_by_sep = "_",
  top_term = 10,
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
  alpha = 1,
  aspect.ratio = 1,
  legend.position = "right",
  legend.direction = "vertical",
  title = NULL,
  subtitle = NULL,
  xlab = NULL,
  ylab = NULL,
  seed = 8525,
  combine = TRUE,
  nrow = NULL,
  ncol = NULL,
  byrow = TRUE,
  axes = NULL,
  axis_titles = axes,
  guides = NULL,
  design = NULL,
  ...
)
}
\arguments{
\item{data}{A data frame containing the data to be plotted.
It should be in the format of clusterProfiler enrichment result,
which includes the columns: ID, Description, GeneRatio, BgRatio, pvalue, p.adjust,
qvalue, geneID and Count.
\itemize{
\item The \code{ID}, \code{qvalue} and \code{Count} columns are optional.
\item The \code{Description} is the description of the term.
\item The \code{GeneRatio} is the number of genes in the term divided by the total number of genes in the input list.
\item The \code{BgRatio} is the number of genes in the term divided by the total number of genes in the background list (all terms).
\item The \code{Count} column, if given, should be the same as the first number in GeneRatio.
}

If you have enrichment results from multiple databases, you can combine them into one data frame and add a column (e.g. Database)
to indicate the database.
You can plot them in a single plot using the \code{split_by} argument (e.g. \code{split_by = "Database"}).}

\item{in_form}{A character string specifying the input format.
Either "auto", "clusterProfiler", "clusterprofiler" or "enrichr".
The default is "auto", which will try to infer the input format.}

\item{split_by}{The column(s) to split data by and plot separately.}

\item{split_by_sep}{The separator for multiple split_by columns. See \code{split_by}}

\item{top_term}{An integer specifying the number of top terms to show.}

\item{metric}{A character string specifying the metric to use for the size of the nodes.
It is also used to order the terms when selected the top terms.
Either "pvalue" or "p.adjust". The default is "p.adjust".}

\item{layout}{A character string specifying the layout of the graph.
Either "circle", "tree", "grid" or other layout functions in \code{igraph}.}

\item{minchar}{An integer specifying the minimum number of characters to show in the keyword.}

\item{cluster}{A character string specifying the clustering method.
Either "fast_greedy", "walktrap", "edge_betweenness", "infomap" or other clustering functions in \code{igraph}.}

\item{show_keyword}{A logical value specifying whether to show the keyword instead of Description/Term in the plot.}

\item{nlabel}{An integer specifying the number of labels to show in each cluster.}

\item{character_width}{The width of the characters used to wrap the keyword.}

\item{mark}{A character string specifying the mark to use for the nodes.
Either "ellipse", "rect", "circle", "text" or other mark functions in \code{ggforce}.}

\item{label}{A character string specifying the label to show in the legend.
Either "term" or "feature". The default is "term".}

\item{labelsize}{A numeric value specifying the size of the label.}

\item{expand}{The values to expand the x and y axes. It is like CSS padding.
When a single value is provided, it is used for both axes on both sides.
When two values are provided, the first value is used for the top/bottom side and the second value is used for the left/right side.
When three values are provided, the first value is used for the top side, the second value is used for the left/right side, and the third value is used for the bottom side.
When four values are provided, the values are used for the top, right, bottom, and left sides, respectively.
You can also use a named vector to specify the values for each side.
When the axis is discrete, the values will be applied as 'add' to the 'expansion' function.
When the axis is continuous, the values will be applied as 'mult' to the 'expansion' function.
See also \url{https://ggplot2.tidyverse.org/reference/expansion.html}}

\item{theme}{A character string or a theme class (i.e. ggplot2::theme_classic) specifying the theme to use.
Default is "theme_this".}

\item{theme_args}{A list of arguments to pass to the theme function.}

\item{palette}{A character string specifying the palette to use.
A named list or vector can be used to specify the palettes for different \code{split_by} values.}

\item{palcolor}{A character string specifying the color to use in the palette.
A named list can be used to specify the colors for different \code{split_by} values.
If some values are missing, the values from the palette will be used (palcolor will be NULL for those values).}

\item{alpha}{A numeric value specifying the transparency of the plot.}

\item{aspect.ratio}{A numeric value specifying the aspect ratio of the plot.}

\item{legend.position}{A character string specifying the position of the legend.
if \code{waiver()}, for single groups, the legend will be "none", otherwise "right".}

\item{legend.direction}{A character string specifying the direction of the legend.}

\item{title}{A character string specifying the title of the plot.
A function can be used to generate the title based on the default title.
This is useful when split_by is used and the title needs to be dynamic.}

\item{subtitle}{A character string specifying the subtitle of the plot.}

\item{xlab}{A character string specifying the x-axis label.}

\item{ylab}{A character string specifying the y-axis label.}

\item{seed}{The random seed to use. Default is 8525.}

\item{combine}{Whether to combine the plots into one when facet is FALSE. Default is TRUE.}

\item{nrow}{A numeric value specifying the number of rows in the facet.}

\item{ncol}{A numeric value specifying the number of columns in the facet.}

\item{byrow}{A logical value indicating whether to fill the plots by row.}

\item{axes}{A string specifying how axes should be treated. Passed to \code{\link[patchwork:wrap_plots]{patchwork::wrap_plots()}}.
Only relevant when \code{split_by} is used and \code{combine} is TRUE.
Options are:
\itemize{
\item 'keep' will retain all axes in individual plots.
\item 'collect' will remove duplicated axes when placed in the same run of rows or columns of the layout.
\item 'collect_x' and 'collect_y' will remove duplicated x-axes in the columns or duplicated y-axes in the rows respectively.
}}

\item{axis_titles}{A string specifying how axis titltes should be treated. Passed to \code{\link[patchwork:wrap_plots]{patchwork::wrap_plots()}}.
Only relevant when \code{split_by} is used and \code{combine} is TRUE.
Options are:
\itemize{
\item 'keep' will retain all axis titles in individual plots.
\item 'collect' will remove duplicated titles in one direction and merge titles in the opposite direction.
\item 'collect_x' and 'collect_y' control this for x-axis titles and y-axis titles respectively.
}}

\item{guides}{A string specifying how guides should be treated in the layout. Passed to \code{\link[patchwork:wrap_plots]{patchwork::wrap_plots()}}.
Only relevant when \code{split_by} is used and \code{combine} is TRUE.
Options are:
\itemize{
\item 'collect' will collect guides below to the given nesting level, removing duplicates.
\item 'keep' will stop collection at this level and let guides be placed alongside their plot.
\item 'auto' will allow guides to be collected if a upper level tries, but place them alongside the plot if not.
}}

\item{design}{Specification of the location of areas in the layout, passed to \code{\link[patchwork:wrap_plots]{patchwork::wrap_plots()}}.
Only relevant when \code{split_by} is used and \code{combine} is TRUE. When specified, \code{nrow}, \code{ncol}, and \code{byrow} are ignored.
See \code{\link[patchwork:wrap_plots]{patchwork::wrap_plots()}} for more details.}

\item{...}{Additional arguments.}

\item{layoutadjust}{A logical value specifying whether to adjust the layout of the network.}

\item{adjscale}{A numeric value specifying the scale of the adjustment.}

\item{adjiter}{A numeric value specifying the number of iterations for the adjustment.}

\item{blendmode}{A character string specifying the blend mode of the colors.
Either "blend", "average", "multiply" and "screen".}
}
\value{
A ggplot object or wrap_plots object or a list of ggplot objects
}
\description{
\code{EnrichMap} is a function to plot the enrichment map.
\code{EnrichNetwork} is a function to plot the enrichment network.
}
\examples{
\donttest{
data(enrich_example)
EnrichMap(enrich_example)
EnrichMap(enrich_example, label = "feature")
EnrichMap(enrich_example, show_keyword = TRUE, label = "term")
EnrichMap(enrich_example, show_keyword = TRUE, label = "feature")

data(enrich_multidb_example)
EnrichMap(enrich_multidb_example, split_by = "Database")
EnrichMap(enrich_multidb_example, split_by = "Database",
          palette = list(DB1 = "Paired", DB2 = "Set1"))
}
\donttest{
EnrichNetwork(enrich_example, top_term = 5)
}
}
