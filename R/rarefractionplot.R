#' RarefactionPlotAtomic
#'
#' This function generates a rarefraction plot for a given dataset.
#'
#' @inheritParams common_args
#' @param data An iNEXT object or a list of data that will be handled by [iNEXT::iNEXT].
#' @param type three types of plots: sample-size-based rarefaction/extrapolation curve (\code{type = 1});
#' sample completeness curve (\code{type = 2}); coverage-based rarefaction/extrapolation curve (\code{type = 3}).
#' @param se a logical variable to display confidence interval around the estimated sampling curve.
#'  Default to \code{NULL} which means TRUE if the data has the lower and upper bounds.
#' @param group_by A character string indicating how to group the data (color the lines).
#'  Possible values are "q" and "group"
#' @param group_name A character string indicating the name of the group, showing as the legend title.
#' @param pt_size A numeric value specifying the size of the points.
#' @param line_width A numeric value specifying the width of the lines.
#' @param facet_by A character string indicating how to facet the data and plots
#'  Possible values are "q" and "group"
#' @param ... Additional arguments to pass to [iNEXT::iNEXT] when \code{data} is not an iNEXT object.
#' @keywords internal
#' @return A ggplot object.
#' @importFrom dplyr rename
#' @importFrom ggplot2 fortify geom_point geom_line geom_ribbon scale_color_manual scale_linetype_manual
#' @importFrom ggplot2 element_line element_text scale_shape_discrete unit
RarefactionPlotAtomic <- function(
    data, type = 1, se = TRUE, group_by = "group", group_name = NULL, pt_size = 3, line_width = 1,
    theme = "theme_this", theme_args = list(), palette = "Spectral", palcolor = NULL, alpha = 0.2,
    facet_by = NULL, facet_scales = "fixed", facet_ncol = NULL, facet_nrow = NULL, facet_byrow = TRUE,
    aspect.ratio = 1, legend.position = "right", legend.direction = "vertical",
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL, ...
) {
    ggplot <- if (getOption("plotthis.gglogger.enabled", FALSE)) {
        gglogger::ggplot
    } else {
        ggplot2::ggplot
    }
    datatype <- unique(data$datatype)
    if (type == 2L) {
        if (identical(datatype, "abundance")) {
            xlab <- xlab %||% "Number of individuals"
            ylab <- ylab %||% "Sample coverage"
        } else {
            xlab <- xlab %||% "Number of sampling units"
            ylab <- ylab %||% "Sample coverage"
        }
    } else if (type == 3L || type == 4L) {
        xlab <- xlab %||% "Sample coverage"
        ylab <- ylab %||% "Species diversity"
    } else if (identical(datatype, "abundance")) {
        xlab <- xlab %||% "Number of individuals"
        ylab <- ylab %||% "Species diversity"
    } else {
        xlab <- xlab %||% "Number of sampling units"
        ylab <- ylab %||% "Species diversity"
    }

    group_name <- group_name %||% group_by
    p <- ggplot(data, aes(x = !!sym("x"), y = !!sym("y"), color = !!sym(group_by))) +
        geom_point(aes(shape = !!sym(group_by)), size = pt_size, data = data[data$Method == "Observed", , drop = FALSE]) +
        scale_color_manual(
            name = group_name,
            values = palette_this(levels(data[[group_by]]), palette = palette, palcolor = palcolor),
            guide = ifelse(identical(group_by, ".group"), "none", "legend")
        ) +
        scale_shape_discrete(name = group_name, guide = ifelse(identical(group_by, ".group"), "none", "legend")) +
        geom_line(aes(linetype = !!sym("lty")), linewidth = line_width) +
        scale_linetype_manual(name = "", values = c("solid", "dashed"),
            # make items wider
            guide = guide_legend(theme = ggplot2::theme(legend.key.width = unit(1, "cm")))) +
        do.call(theme, theme_args) +
        ggplot2::theme(
            aspect.ratio = aspect.ratio,
            legend.position = legend.position,
            legend.direction = legend.direction,
            panel.grid.major = element_line(colour = "grey80", linetype = 2)
        ) +
        labs(title = title, subtitle = subtitle, x = xlab, y = ylab)

    if (isTRUE(se)) {
        p <- p + geom_ribbon(aes(ymin = !!sym("y.lwr"), ymax = !!sym("y.upr"), fill= !!sym(group_by)),
            color = "transparent", alpha = alpha) +
            scale_fill_manual(
                name = group_name,
                values = palette_this(levels(data[[group_by]]), palette = palette, palcolor = palcolor),
                guide = ifelse(identical(group_by, ".group"), "none", "legend")
            )
    }

    height <- width <- 4.5
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


#' RarefactionPlot
#'
#' This function generates a rarefraction plot for a given dataset.
#'
#' @inheritParams common_args
#' @inheritParams RarefactionPlotAtomic
#' @param split_by A character string indicating how to split the data and plots
#'  Possible values are "q" and "group"
#' @param group_by_sep A character string indicating how to separate the group_by column if both "q" and "group" are used.
#'  for 'group_by'. Default to "_".
#' @export
#' @return A ggplot object or wrap_plots object or a list of ggplot objects
#' @examples
#' \donttest{
#' set.seed(8525)
#' spider <- list(
#'    Girdled = c(46, 22, 17, 15, 15, 9, 8, 6, 6, 4, rep(2, 4), rep(1, 12)),
#'    Logged = c(88, 22, 16, 15, 13, 10, 8, 8, 7, 7, 7, 5, 4, 4, 4, 3, 3, 3, 3,
#'      2, 2, 2, 2, rep(1, 14))
#' )
#'
#' RarefactionPlot(spider)
#' RarefactionPlot(spider, q = c(0, 1, 2), facet_by = "q")
#' RarefactionPlot(spider, q = c(0, 1, 2), split_by = "q")
#' RarefactionPlot(spider, q = c(0, 1, 2), group_by = "q",
#'  facet_by = "group", palette = "Set1", type = 3)
#' }
RarefactionPlot <- function(
    data, type = 1, se = NULL, group_by = "group", group_by_sep = "_", group_name = NULL, split_by = NULL, split_by_sep = "_",
    theme = "theme_this", theme_args = list(), palette = "Spectral", palcolor = NULL, alpha = 0.2, pt_size = 3, line_width = 1,
    facet_by = NULL, facet_scales = "fixed", facet_ncol = NULL, facet_nrow = NULL, facet_byrow = TRUE,
    aspect.ratio = 1, legend.position = "right", legend.direction = "vertical",
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL, seed = 8525,
    combine = TRUE, nrow = NULL, ncol = NULL, byrow = TRUE, ...
) {
    validate_common_args(seed, facet_by = facet_by)
    theme <- process_theme(theme)
    stopifnot("Invalid 'type' value. It must be 1, 2, or 3 or combination of them." =
        length(type) > 0 && length(setdiff(type, 1:3)) == 0)
    # check group_by, split_by, facet_by.
    # If given, they should be "q" or "group". Only facet_by could be both.
    stopifnot("Invalid 'group_by' value. It must be 'q' and/or 'group'." =
        is.null(group_by) || all(group_by %in% c("q", "group")))
    stopifnot("Invalid 'split_by' value. It must be 'q' and/or 'group'." =
        is.null(split_by) || all(split_by %in% c("q", "group")))
    stopifnot("Invalid 'facet_by' value. It must be 'q' and/or 'group'." =
        is.null(facet_by) || all(facet_by %in% c("q", "group")))
    # They should not overlap with each other.
    # If they are not NULL, they should be different.
    stopifnot("Invalid 'group_by'/'split_by'. They should not overlap." =
        is.null(group_by) || is.null(split_by) || length(intersect(group_by, split_by)) == 0)
    stopifnot("Invalid 'group_by'/'facet_by'. They should not overlap." =
        is.null(group_by) || is.null(facet_by) || length(intersect(group_by, facet_by)) == 0)
    stopifnot("Invalid 'split_by'/'facet_by'. They should not overlap." =
        is.null(split_by) || is.null(facet_by) || length(intersect(split_by, facet_by)) == 0)

    if (!inherits(data, "iNEXT")) {
        data <- iNEXT::iNEXT(data, ...)
    }

    data <- suppressWarnings({ fortify(data, type=type) })
    # rename Assemblage to group and Order.q to q
    data <- rename(data, group = "Assemblage", q = "Order.q")

    se <- se %||% ('y.lwr' %in% names(data))

    data$Method2 <- data$Method
    data$Method2[data$Method2 == "Observed"] <- "Rarefaction"
    data$lty <- factor(data$Method2, levels = c("Rarefaction", "Extrapolation"))

    group_by <- check_columns(data, group_by, force_factor = TRUE, allow_multi = TRUE,
        concat_multi = TRUE, concat_sep = group_by_sep)
    split_by <- check_columns(data, split_by, force_factor = TRUE, allow_multi = TRUE,
        concat_multi = TRUE, concat_sep = split_by_sep)
    facet_by <- check_columns(data, facet_by, force_factor = TRUE, allow_multi = TRUE)

    if (is.null(group_by)) {
        group_by <- ".group"
        data$.group <- factor("")
    }

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
            default_title <- if (length(datas) == 1 && identical(nm, "...")) {
                NULL
            } else {
               ifelse(identical(split_by, "q"), paste(split_by, nm, sep = " = "), nm)
            }
            if (is.function(title)) {
                title <- title(default_title)
            } else {
                title <- title %||% default_title
            }
            RarefactionPlotAtomic(datas[[nm]], type = type, se = se, group_by = group_by, group_name = group_name, pt_size = pt_size,
                theme = theme, theme_args = theme_args, palette = palette, palcolor = palcolor, line_width = line_width,
                alpha = alpha, facet_by = facet_by, facet_scales = facet_scales, facet_ncol = facet_ncol,
                facet_nrow = facet_nrow, facet_byrow = facet_byrow,
                aspect.ratio = aspect.ratio, legend.position = legend.position, legend.direction = legend.direction,
                title = title, subtitle = subtitle, xlab = xlab, ylab = ylab, ...
            )
        }
    )

    combine_plots(plots, combine = combine, nrow = nrow, ncol = ncol, byrow = byrow)
}
