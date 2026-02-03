
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
#' @param keep_na A logical value or a character to replace the NA values in the data.
#'   It can also take a named list to specify different behavior for different columns.
#'   If TRUE, NA values will be replaced with "<NA>" string.
#'   If FALSE, NA values will be removed from the data before plotting.
#'   If a character string is provided, NA values will be replaced with the provided string.
#'   If a named vector/list is provided, the names should be the column names to apply the behavior to,
#'   and the values should be one of TRUE, FALSE, or a character string.
#'   Without a named vector/list, the behavior applies to column for the `x` axis.
#' @param keep_empty One of "FALSE", "TRUE" and "level". It can also take a named list to specify
#' different behavior for different columns. If a single logical value is provided, it will be converted
#' to a character value internally. Without a named list, the behavior applies to column for the `x` axis.
#' \itemize{
#'   \item{\code{FALSE} (default): Drop empty factor levels from the data before plotting.
#'    Alias: \code{false}}
#'   \item{\code{TRUE}: Keep empty factor levels and show them as a separate category in the plot.
#'    Alias: \code{true}}
#'   \item{\code{level}: Keep empty factor levels, but do not show them in the plot.
#'     But they will be assigned colors from the palette to maintain consistency across multiple plots.
#'    Alias: \code{levels}}
#' }
#' @param theme A character string or a theme class (i.e. ggplot2::theme_classic) specifying the theme to use.
#'   Default is "theme_this".
#' @param theme_args A list of arguments to pass to the theme function.
#' @param palette A character string specifying the palette to use.
#'   A named list or vector can be used to specify the palettes for different `split_by` values.
#' @param palcolor A character string specifying the color to use in the palette.
#'   A named list can be used to specify the colors for different `split_by` values.
#'   If some values are missing, the values from the palette will be used (palcolor will be NULL for those values).
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
#' @param axes A string specifying how axes should be treated. Passed to [`patchwork::wrap_plots()`].
#'   Only relevant when `split_by` is used and `combine` is TRUE.
#'   Options are:
#'   * 'keep' will retain all axes in individual plots.
#'   * 'collect' will remove duplicated axes when placed in the same run of rows or columns of the layout.
#'   * 'collect_x' and 'collect_y' will remove duplicated x-axes in the columns or duplicated y-axes in the rows respectively.
#' @param axis_titles A string specifying how axis titltes should be treated. Passed to [`patchwork::wrap_plots()`].
#'   Only relevant when `split_by` is used and `combine` is TRUE.
#'   Options are:
#'   * 'keep' will retain all axis titles in individual plots.
#'   * 'collect' will remove duplicated titles in one direction and merge titles in the opposite direction.
#'   * 'collect_x' and 'collect_y' control this for x-axis titles and y-axis titles respectively.
#' @param guides A string specifying how guides should be treated in the layout. Passed to [`patchwork::wrap_plots()`].
#'   Only relevant when `split_by` is used and `combine` is TRUE.
#'   Options are:
#'   * 'collect' will collect guides below to the given nesting level, removing duplicates.
#'   * 'keep' will stop collection at this level and let guides be placed alongside their plot.
#'   * 'auto' will allow guides to be collected if a upper level tries, but place them alongside the plot if not.
#' @param design Specification of the location of areas in the layout, passed to [`patchwork::wrap_plots()`].
#'   Only relevant when `split_by` is used and `combine` is TRUE. When specified, `nrow`, `ncol`, and `byrow` are ignored.
#'   See [`patchwork::wrap_plots()`] for more details.
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
    theme = "theme_this",
    theme_args = list(),
    palette = NULL,
    palcolor = NULL,
    expand = NULL,
    keep_na = FALSE,
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

    invisible(NULL)
}
