% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lineplot.R
\name{LinePlotAtomic}
\alias{LinePlotAtomic}
\title{LinePlotAtomic}
\usage{
LinePlotAtomic(
  data,
  x,
  y = NULL,
  group_by = NULL,
  fill_point_by_x_if_no_group = TRUE,
  color_line_by_x_if_no_group = TRUE,
  add_bg = FALSE,
  bg_palette = "stripe",
  bg_palcolor = NULL,
  bg_alpha = 0.2,
  add_errorbars = FALSE,
  errorbar_width = 0.1,
  errorbar_alpha = 1,
  errorbar_color = "grey30",
  errorbar_linewidth = 0.75,
  errorbar_min = NULL,
  errorbar_max = NULL,
  errorbar_sd = NULL,
  highlight = NULL,
  highlight_size = pt_size - 0.75,
  highlight_color = "red2",
  highlight_alpha = 0.8,
  pt_alpha = 1,
  pt_size = 5,
  add_hline = FALSE,
  hline_type = "solid",
  hline_width = 0.5,
  hline_color = "black",
  hline_alpha = 1,
  line_type = "solid",
  line_width = 1,
  line_alpha = 0.8,
  theme = "theme_this",
  theme_args = list(),
  palette = "Paired",
  palcolor = NULL,
  x_text_angle = 0,
  aspect.ratio = 1,
  legend.position = "right",
  legend.direction = "vertical",
  title = NULL,
  subtitle = NULL,
  xlab = NULL,
  ylab = NULL,
  keep_empty = FALSE,
  facet_by = NULL,
  facet_scales = "fixed",
  facet_args = list(),
  facet_nrow = NULL,
  facet_ncol = NULL,
  facet_byrow = TRUE,
  ...
)
}
\arguments{
\item{data}{A data frame.}

\item{x}{A character string specifying the column name of the data frame to plot for the x-axis.}

\item{y}{A character string specifying the column name of the data frame to plot for the y-axis.}

\item{group_by}{Columns to group the data for plotting
For those plotting functions that do not support multiple groups,
They will be concatenated into one column, using \code{group_by_sep} as the separator}

\item{fill_point_by_x_if_no_group}{A logical value indicating whether to color the points by the x-axis values
when there is no group_by column.}

\item{color_line_by_x_if_no_group}{A logical value indicating whether to color the lines by the x-axis values}

\item{add_bg}{A logical value indicating whether to add a background to the plot.}

\item{bg_palette}{The palette to use for the background.}

\item{bg_palcolor}{The color to use for the background.}

\item{bg_alpha}{The alpha value of the background.}

\item{add_errorbars}{A logical value indicating whether to add error bars to the plot.}

\item{errorbar_width}{The width of the error bars.}

\item{errorbar_alpha}{The alpha value of the error bars.}

\item{errorbar_color}{The color to use for the error bars.
If "line", the error bars will be colored the same as the lines.}

\item{errorbar_linewidth}{The line width of the error bars.}

\item{errorbar_min}{The column in the data frame containing the lower bound of the error bars.}

\item{errorbar_max}{The column in the data frame containing the upper bound of the error bars.}

\item{errorbar_sd}{The column in the data frame containing the standard deviation of the error bars.
If errorbar_min and errorbar_max are not provided, this column will be used to calculate the error bars.
errorbar_min = y - errorbar_sd, errorbar_max = y + errorbar_sd.
If errorbar_min and errorbar_max are provided, this column will be ignored.}

\item{highlight}{A vector of indexes or rownames to select the points to highlight.
It could also be an expression (in string) to filter the data.}

\item{highlight_size}{The size of the highlighted points.}

\item{highlight_color}{A character vector specifying the color of the highlighted points. Default is "red".}

\item{highlight_alpha}{A numeric value specifying the transparency of the highlighted points. Default is 1.}

\item{pt_alpha}{The alpha value of the points.}

\item{pt_size}{The size of the points.}

\item{add_hline}{A numeric value indicating the y-intercept of a horizontal line to add to the plot.
If FALSE, no horizontal line will be added.}

\item{hline_type}{The type of line to draw for the horizontal line.}

\item{hline_width}{The width of the horizontal line.}

\item{hline_color}{The color of the horizontal line.
When \code{group_by} is provided, this can be TRUE to use the same color as the lines.}

\item{hline_alpha}{The alpha value of the horizontal line.}

\item{line_type}{The type of line to draw.}

\item{line_width}{The width of the line.}

\item{line_alpha}{The alpha value of the line.}

\item{theme}{A character string or a theme class (i.e. ggplot2::theme_classic) specifying the theme to use.
Default is "theme_this".}

\item{theme_args}{A list of arguments to pass to the theme function.}

\item{palette}{A character string specifying the palette to use.
A named list or vector can be used to specify the palettes for different \code{split_by} values.}

\item{palcolor}{A character string specifying the color to use in the palette.
A named list can be used to specify the colors for different \code{split_by} values.
If some values are missing, the values from the palette will be used (palcolor will be NULL for those values).}

\item{x_text_angle}{A numeric value specifying the angle of the x-axis text.}

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

\item{keep_empty}{A logical value indicating whether to keep empty groups.
If FALSE, empty groups will be removed.}

\item{facet_by}{A character string specifying the column name of the data frame to facet the plot.
Otherwise, the data will be split by \code{split_by} and generate multiple plots
and combine them into one using \code{patchwork::wrap_plots}}

\item{facet_scales}{Whether to scale the axes of facets. Default is "fixed"
Other options are "free", "free_x", "free_y". See \code{ggplot2::facet_wrap}}

\item{facet_args}{A list of arguments to pass to \code{\link[ggplot2:facet_wrap]{ggplot2::facet_wrap()}} or \code{\link[ggplot2:facet_grid]{ggplot2::facet_grid()}}.
when there is no group_by column.}

\item{facet_nrow}{A numeric value specifying the number of rows in the facet.
When facet_by is a single column and facet_wrap is used.}

\item{facet_ncol}{A numeric value specifying the number of columns in the facet.
When facet_by is a single column and facet_wrap is used.}

\item{facet_byrow}{A logical value indicating whether to fill the plots by row. Default is TRUE.}

\item{...}{Additional arguments.}
}
\value{
A ggplot object
}
\description{
Line plot with atomic data.
}
\keyword{internal}
