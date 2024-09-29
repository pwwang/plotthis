
#' Common arguments for plots
#'
#' @name common_args
#'
#' @param data A data frame.
#' @param x A character string specifying the column name of the data frame to plot for the x-axis.
#' @param y A character string specifying the column name of the data frame to plot for the y-axis.
#' @param group_by Columns to group the data for plotting
#'   For those plotting functions that do not support multiple groups,
#'   They will be concatenated into one column, using \code{group_by_sep} as the separator
#' @param group_by_sep The separator for multiple group_by columns. See \code{group_by}
#' @param split_by The column(s) to split data by and plot separately.
#' @param split_by_sep The separator for multiple split_by columns. See \code{split_by}
#' @param keep_empty A logical value indicating whether to keep empty groups.
#'   If FALSE, empty groups will be removed.
#' @param theme A character string or a theme class (i.e. ggplot2::theme_classic) specifying the theme to use.
#'   Default is "theme_this".
#'
#' @param theme_args A list of arguments to pass to the theme function.
#' @param palette A character string specifying the palette to use.
#' @param palcolor A character string specifying the color to use in the palette.
#' @param alpha A numeric value specifying the transparency of the plot.
#' @param x_text_angle A numeric value specifying the angle of the x-axis text.
#' @param aspect.ratio A numeric value specifying the aspect ratio of the plot.
#' @param title A character string specifying the title of the plot.
#'  A function can be used to generate the title based on the default title.
#'  This is useful when split_by is used and the title needs to be dynamic.
#' @param subtitle A character string specifying the subtitle of the plot.
#' @param xlab A character string specifying the x-axis label.
#' @param ylab A character string specifying the y-axis label.
#' @param legend.position A character string specifying the position of the legend.
#'   if `waiver()`, for single groups, the legend will be "none", otherwise "right".
#' @param legend.direction A character string specifying the direction of the legend.
#' @param expand The values to expand the x and y axes. It is like CSS padding.
#'   When a single value is provided, it is used for both axes on both sides.
#'   When two values are provided, the first value is used for the top/bottom side and the second value is used for the left/right side.
#'   When three values are provided, the first value is used for the top side, the second value is used for the left/right side, and the third value is used for the bottom side.
#'   When four values are provided, the values are used for the top, right, bottom, and left sides, respectively.
#'   You can also use a named vector to specify the values for each side.
#'   When the axis is discrete, the values will be applied as 'add' to the 'expansion' function.
#'   When the axis is continuous, the values will be applied as 'mult' to the 'expansion' function.
#'   See also \url{https://ggplot2.tidyverse.org/reference/expansion.html}
#'
#' @param facet_by A character string specifying the column name of the data frame to facet the plot.
#'   Otherwise, the data will be split by \code{split_by} and generate multiple plots
#'   and combine them into one using \code{patchwork::wrap_plots}
#' @param facet_scales Whether to scale the axes of facets. Default is "fixed"
#'   Other options are "free", "free_x", "free_y". See \code{ggplot2::facet_wrap}
#' @param facet_nrow A numeric value specifying the number of rows in the facet.
#'   When facet_by is a single column and facet_wrap is used.
#' @param facet_ncol A numeric value specifying the number of columns in the facet.
#'   When facet_by is a single column and facet_wrap is used.
#' @param facet_byrow A logical value indicating whether to fill the plots by row. Default is TRUE.
#' @param combine Whether to combine the plots into one when facet is FALSE. Default is TRUE.
#' @param nrow A numeric value specifying the number of rows in the facet.
#' @param ncol A numeric value specifying the number of columns in the facet.
#' @param byrow A logical value indicating whether to fill the plots by row.
#' @param seed The random seed to use. Default is 8525.
#' @param ... Additional arguments.
#' @keywords internal
NULL

#' Validate common arguments
#' @keywords internal
#' @inheritParams common_args
validate_common_args <- function(
    seed,
    facet_by = NULL,
    plot_type = NULL,
    split_by = NULL,
    split_by_sep = "_",
    group_by = NULL,
    group_by_sep = "_",
    facet_scales = "fixed",
    facet_nrow = NULL,
    facet_ncol = NULL,
    facet_byrow = TRUE,
    theme = "theme_scp",
    theme_args = list(),
    palette = NULL,
    palcolor = NULL,
    expand = NULL,
    keep_empty = FALSE,
    alpha = 1,
    x_text_angle = 0,
    aspect.ratio = 1,
    legend.position = "right",
    legend.direction = "vertical",
    title = NULL,
    subtitle = NULL,
    xlab = NULL,
    ylab = NULL,
    combine = TRUE,
    nrow = NULL,
    ncol = NULL,
    byrow = TRUE,
    ...) {
    if (!is.numeric(seed)) {
        stop("'seed' must be a numeric value.")
    }

    set.seed(seed)

    if (length(facet_by) > 2) {
        stop("Too many columns specified in 'facet_by', only up to 2 columns are allowed.")
    }
}
