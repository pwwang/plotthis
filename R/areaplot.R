#' Atomic area plot
#'
#' @inheritParams common_args
#' @param x A character string of the column name to plot on the x-axis.
#'  A character/factor column is expected.
#' @param x_sep A character string to concatenate the columns in `x`, if multiple columns are provided.
#' @param y A character string of the column name to plot on the y-axis.
#'  A numeric column is expected.
#'  If NULL, the count of the x-axis column will be used.
#' @param scale_y A logical value to scale the y-axis by the total number in each x-axis group.
#' @param group_by A character vector of column names to fill the area plot by.
#'  If NULL, the plot will be filled by the first color of the palette.
#'  If multiple columns are provided, the columns will be concatenated with
#'  `group_by_sep` and used as the fill column.
#' @param group_by_sep A character string to separate the columns in `group_by`.
#' @param group_name A character string to name the legend of fill.
#' @keywords internal
#' @return A ggplot object
#' @importFrom rlang syms :=
#' @importFrom dplyr summarise n %>%
#' @importFrom ggplot2 geom_area scale_x_discrete scale_y_continuous scale_fill_manual
#' @importFrom ggplot2 labs theme element_line element_text position_stack waiver
AreaPlotAtomic <- function(
    data, x, y = NULL, x_sep = "_", group_by = NULL, group_by_sep = "_", group_name = NULL, scale_y = FALSE,
    theme = "theme_this", theme_args = list(), palette = "Paired", palcolor = NULL, alpha = 1,
    facet_by = NULL, facet_scales = "fixed", facet_ncol = NULL, facet_nrow = NULL, facet_byrow = TRUE,
    x_text_angle = 0, aspect.ratio = 1, legend.position = waiver(), legend.direction = "vertical",
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL, keep_empty = FALSE, ...
) {
    x <- check_columns(data, x, force_factor = TRUE, allow_multi = TRUE, concat_multi = TRUE, concat_sep = x_sep)
    y <- check_columns(data, y)
    group_by <- check_columns(data, group_by, force_factor = TRUE, allow_multi = TRUE, concat_multi = TRUE, concat_sep = group_by_sep)
    ggplot <- if (getOption("plotthis.gglogger.enabled", FALSE)) {
        gglogger::ggplot
    } else {
        ggplot2::ggplot
    }

    if (is.null(y)) {
        y <- ".count"
        data <- data %>%
            dplyr::group_by(!!!syms(unique(c(x, group_by, facet_by)))) %>%
            summarise(.count = n(), .groups = "drop")
    }

    if (isTRUE(scale_y)) {
        data <- data %>%
            dplyr::group_by(!!!syms(unique(c(x, facet_by)))) %>%
            mutate(!!sym(y) := !!sym(y) / sum(!!sym(y)))
    }

    if (is.null(group_by)) {
        data$.fill <- factor("")
        group_by <- ".fill"
        legend.position <- ifelse(inherits(legend.position, "waiver"), "none", "right")
    } else {
        data[[group_by]] <- droplevels(data[[group_by]])
        # fill up some missing group_by values for each x, and fill it with 0 for y
        fill_levels <- levels(data[[group_by]])
        complete_fill <- list(0)
        names(complete_fill) <- y
        data <- data %>%
            dplyr::group_by(!!!syms(unique(c(group_by, facet_by)))) %>%
            complete(!!sym(x), fill = complete_fill) %>%
            ungroup()
        legend.position <- ifelse(inherits(legend.position, "waiver"), "right", legend.position)
    }
    xs <- if (isTRUE(keep_empty)) levels(data[[x]]) else levels(droplevels(data[[x]]))

    just <- calc_just(x_text_angle)
    p <- ggplot(data, aes(x = as.numeric(!!sym(x)), y = !!sym(y), fill = !!sym(group_by))) +
        geom_area(alpha = alpha, color = "grey50", position = position_stack(vjust = 0.5)) +
        scale_x_discrete(drop = !keep_empty, expand = c(0, 0), breaks = xs, limits = xs) +
        scale_y_continuous(expand = c(0, 0), labels = if (isFALSE(scale_y)) scales::number else scales::percent) +
        scale_fill_manual(
            name = group_name %||% group_by,
            values = palette_this(levels(data[[group_by]]), palette = palette, palcolor = palcolor)
        ) +
        labs(title = title, subtitle = subtitle, x = xlab %||% x, y = ylab %||% y) +
        do.call(theme, theme_args) +
        ggplot2::theme(
            aspect.ratio = aspect.ratio,
            legend.position = legend.position,
            legend.direction = legend.direction,
            panel.grid.major = element_line(colour = "grey80", linetype = 2),
            axis.text.x = element_text(angle = x_text_angle, hjust = just$h, vjust = just$v)
        )

    height = 4.5
    width = 0.5 + length(xs) * 0.5
    if (!identical(legend.position, "none")) {
        if (legend.position %in% c("right", "left")) {
            width <- width + 1
        } else if (legend.direction == "horizontal") {
            height <- height + 1
        } else {
            width <- width + 2
        }
    }

    attr(p, "height") <- height
    attr(p, "width") <- width

    facet_plot(p, facet_by, facet_scales, facet_nrow, facet_ncol, facet_byrow,
        legend.position = legend.position, legend.direction = legend.direction)
}

#' Area plot
#'
#' @description A plot showing how one or more groups' numeric values change over the
#'  progression of a another variable
#' @inheritParams common_args
#' @inheritParams AreaPlotAtomic
#' @return A ggplot object or wrap_plots object or a list of ggplot objects
#' @importFrom ggplot2 waiver
#' @export
#' @examples
#' data <- data.frame(
#'     x = rep(c("A", "B", "C", "D"), 2),
#'     y = c(1, 3, 6, 4, 2, 5, 7, 8),
#'     group = rep(c("F1", "F2"), each = 4)
#' )
#' AreaPlot(data, x = "x", y = "y", group_by = "group")
#' AreaPlot(data, x = "x", y = "y", group_by = "group",
#'          scale_y = TRUE)
#' AreaPlot(data, x = "x", y = "y", split_by = "group")
AreaPlot <- function(
    data, x, y = NULL, x_sep = "_", split_by = NULL, split_by_sep = "_",
    group_by = NULL, group_by_sep = "_", group_name = NULL, scale_y = FALSE,
    theme = "theme_this", theme_args = list(), palette = "Paired", palcolor = NULL, alpha = 1,
    facet_by = NULL, facet_scales = "fixed", facet_ncol = NULL, facet_nrow = NULL, facet_byrow = TRUE,
    x_text_angle = 0, aspect.ratio = 1, legend.position = waiver(), legend.direction = "vertical",
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL, keep_empty = FALSE, seed = 8525,
    combine = TRUE, nrow = NULL, ncol = NULL, byrow = TRUE, ...
){
    validate_common_args(seed, facet_by = facet_by)
    theme <- process_theme(theme)
    split_by <- check_columns(data, split_by, force_factor = TRUE, allow_multi = TRUE, concat_multi = TRUE, concat_sep = split_by_sep)

    if (!is.null(split_by)) {
        datas <- split(data, data[[split_by]])
        # keep the order of levels
        datas <- datas[levels(data[[split_by]])]
    } else {
        datas <- list(data)
        names(datas) <- "..."
    }

    plots <- lapply(
        names(datas), function(nm) {
            default_title <- if (length(datas) == 1 && identical(nm, "...")) NULL else nm
            if (is.function(title)) {
                title <- title(default_title)
            } else {
                title <- title %||% default_title
            }
            AreaPlotAtomic(
                datas[[nm]],
                x = x, y = y, x_sep = x_sep, group_by = group_by, group_by_sep = group_by_sep, group_name = group_name, scale_y = scale_y,
                theme = theme, theme_args = theme_args, palette = palette, palcolor = palcolor, alpha = alpha,
                facet_by = facet_by, facet_scales = facet_scales, facet_ncol = facet_ncol, facet_nrow = facet_nrow, facet_byrow = facet_byrow,
                x_text_angle = x_text_angle, aspect.ratio = aspect.ratio, legend.position = legend.position, legend.direction = legend.direction,
                title = title, subtitle = subtitle, xlab = xlab, ylab = ylab, keep_empty = keep_empty, ...
            )
        }
    )

    combine_plots(plots, combine = combine, nrow = nrow, ncol = ncol, byrow = byrow)
}
