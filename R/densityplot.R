#' Atomic density/histogram plot
#'
#' @inheritParams common_args
#' @param x A character string specifying the column name for the values
#'   A numeric column is expected.
#' @param group_by A character string specifying the column name to group the data
#' @param group_by_sep A character string to concatenate the columns in `group_by` if multiple columns are provided
#' @param group_name A character string to name the legend of group_by
#' @param type A character string specifying the type of plot. Default is "density".
#'  Other options are "histogram".
#' @param bins A numeric value specifying the number of bins for the histogram.
#' @param binwidth A numeric value specifying the width of the bins for the histogram.
#' @param flip A logical value. If TRUE, the plot will be flipped.
#' @param add_lines A logical value. If TRUE, add lines to the plot to show the data distribution on the bottom.
#' @param line_height A numeric value specifying the height of the lines. The actual height will be calculated based on the maximum density or count.
#' @param line_alpha A numeric value specifying the alpha of the lines.
#' @param line_width A numeric value specifying the width of the lines.
#' @importFrom gglogger ggplot
#' @importFrom ggplot2 geom_density scale_fill_manual labs theme geom_histogram coord_flip waiver
#' @importFrom ggplot2 scale_color_manual scale_x_continuous scale_y_continuous
#' @keywords internal
DensityHistoPlotAtomic <- function(
    data, x, group_by = NULL, group_by_sep = "_", group_name = NULL,
    type = c("density", "histogram"), bins = NULL, binwidth = NULL, flip = FALSE,
    add_lines = TRUE, line_height = 0.025, line_alpha = 1, line_width = .1,
    palette = "Paired", palcolor = NULL, alpha = .5, theme = "theme_this", theme_args = list(),
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL, expand = c(bottom = 0, left = 0, right = 0),
    facet_by = NULL, facet_scales = "fixed", facet_ncol = NULL, facet_nrow = NULL, facet_byrow = TRUE,
    legend.position = ifelse(is.null(group_by), "none", "right"), legend.direction = "vertical", ...) {
    type <- match.arg(type)
    expand <- norm_expansion(expand, x_type = "continuous", y_type = "continuous")
    x <- check_columns(data, x)
    group_by <- check_columns(data, group_by, force_factor = TRUE, allow_multi = TRUE, concat_multi = TRUE, concat_sep = group_by_sep)
    if (is.null(group_by)) {
        group_by <- ".group"
        data[[group_by]] <- factor("")
    }
    if (isTRUE(add_lines)) {
        if (type == "density") {
            # calculate the max density for the y-axis
            max_y <- max(stats::density(data[[x]])$y) * 1.5
        } else {
            # calculate the max count for the y-axis by bins
            if (is.null(bins) && is.null(binwidth)) {
                s <- seq(min(data[[x]]), max(data[[x]]), length.out = 30)
            } else if (!is.null(bins)) {
                s <- seq(min(data[[x]]), max(data[[x]]), length.out = bins)
            } else {
                s <- seq(min(data[[x]]), max(data[[x]]), by = binwidth)
            }
            max_y <- max(table(cut(data[[x]], s)))
        }
        lnheight <- line_height * max_y
        # calculate the ymin ymax for each group to plot the data lines
        data$.ymin <- lnheight * (1 - as.integer(data[[group_by]]))
        data$.ymax <- data$.ymin - lnheight
    }

    p <- ggplot(data, aes(x = !!sym(x), fill = !!sym(group_by), color = !!sym(group_by)))
    if (type == "histogram") {
        p <- p + geom_histogram(alpha = alpha, bins = bins, binwidth = binwidth)
    } else {
        p <- p + geom_density(alpha = alpha)
    }
    if (isTRUE(add_lines)) {
        p <- p +
            geom_linerange(aes(ymin = !!sym(".ymin"), ymax = !!sym(".ymax")), size = 1, alpha = line_alpha, linewidth = line_width)
    }
    p <- p +
        scale_fill_manual(
            name = group_name %||% group_by,
            values = palette_this(levels(data[[group_by]]), palette = palette, palcolor = palcolor)
        ) +
        scale_color_manual(
            name = group_name %||% group_by,
            values = palette_this(levels(data[[group_by]]), palette = palette, palcolor = palcolor)
        ) +
        scale_x_continuous(expand = expand$x) +
        scale_y_continuous(expand = expand$y) +
        do.call(theme, theme_args) +
        labs(title = title, subtitle = subtitle, x = xlab %||% x, y = ylab %||% ifelse(type == "histogram", "Count", "Density")) +
        ggplot2::theme(legend.position = legend.position, legend.direction = legend.direction)

    if (flip) {
        p <- p + coord_flip()
    }

    height <- 3.5
    width <- 4
    if (!identical(legend.position, "none")) {
        if (legend.position %in% c("right", "left")) {
            width <- width + 1
        } else if (legend.direction == "horizontal") {
            height <- height + 1
        } else {
            height <- height + 1
        }
    }
    if (flip) {
        attr(p, "height") <- width
        attr(p, "width") <- height
    } else {
        attr(p, "height") <- height
        attr(p, "width") <- width
    }

    facet_plot(p,
        facet_by = facet_by, facet_scales = facet_scales, ncol = facet_ncol,
        nrow = facet_nrow, byrow = facet_byrow, legend.position = legend.position,
        legend.direction = legend.direction
    )
}

#' Atomic ridge plot
#'
#' @inheritParams common_args
#' @param data A data frame
#'  It has two forms: wide and long.
#'  For the wide form, the values should under different 'group_by' columns.
#'  For the long form, the values should be under the 'x' column and the 'group_by' column should be provided,
#'  which should be a single column with the group names.
#' @param x A character string specifying the column name for the values
#'  A numeric column is expected.
#'  If 'data' is in the wide form, 'x' should be NULL. The values will be taken from the data under 'group_by' columns.
#' @param in_form A character string specifying the form of the data. Default is "long".
#' @param group_by A character string specifying the column name to group the data
#'  These groups will be shown on the y-axis.
#' @param group_by_sep A character string to concatenate the columns in `group_by` if multiple columns are provided
#'  If 'data' is in the wide form, the columns will not be concatenated.
#' @param group_name A character string to name the legend of 'group_by', if 'legend.position' is not "none".
#' @param flip A logical value. If TRUE, the plot will be flipped.
#' @param alpha A numeric value specifying the alpha of the ridges.
#' @param keep_empty A logical value. If TRUE, keep the empty groups on the y-axis.
#' @param reverse A logical value. If TRUE, reverse the order of the groups on the y-axis.
#' @param ... Additional arguments.
#' @keywords internal
RidgePlotAtomic <- function(
    data, x = NULL, in_form = c("long", "wide"), group_by = NULL, group_by_sep = "_", group_name = NULL,
    flip = FALSE, alpha = 1, theme = "theme_this", theme_args = list(), palette = "Paired", palcolor = NULL,
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL, x_text_angle = 90, keep_empty = FALSE, reverse = FALSE,
    facet_by = NULL, facet_scales = "fixed", facet_ncol = NULL, facet_nrow = NULL, facet_byrow = TRUE,
    aspect.ratio = 1, legend.position = "none", legend.direction = "vertical", ...) {
    in_form <- match.arg(in_form)
    if (in_form == "wide") {
        data <- data %>% pivot_longer(cols = group_by, names_to = ".group_by", values_to = ".x")
        x <- ".x"
        group_by <- ".group_by"
    }
    x <- check_columns(data, x)
    group_by <- check_columns(data, group_by, force_factor = TRUE, allow_multi = TRUE, concat_multi = TRUE, concat_sep = group_by_sep)
    if (isTRUE(reverse)) {
        data[[group_by]] <- factor(data[[group_by]], levels = rev(levels(data[[group_by]])))
    }

    p <- ggplot(data, aes(x = !!sym(x), y = !!sym(group_by), fill = !!sym(group_by))) +
        ggridges::geom_density_ridges(alpha = alpha) +
        scale_fill_manual(values = palette_this(levels(data[[group_by]]), palette = palette, palcolor = palcolor)) +
        scale_y_discrete(drop = !keep_empty, expand = c(0, 0)) +
        scale_x_continuous(expand = c(0, 0)) +
        do.call(theme, theme_args) +
        labs(title = title, subtitle = subtitle, x = xlab %||% x, y = ylab %||% group_by) +
        ggplot2::theme(
            aspect.ratio = aspect.ratio,
            legend.position = legend.position,
            legend.direction = legend.direction
        )

    if (flip) {
        just <- calc_just(x_text_angle)
        p <- p +
            ggplot2::theme(
                axis.text.x = element_text(angle = x_text_angle, hjust = just$h, vjust = just$v),
                axis.ticks.x = element_line(),
                panel.grid.major.x = element_line(color = "grey", linetype = 2)
            ) +
            coord_flip()
    } else {
        p <- p +
            ggplot2::theme(
                axis.text.x = element_text(),
                axis.text.y = element_text(hjust = 1),
                axis.ticks.y = element_line(),
                panel.grid.major.y = element_line(color = "grey", linetype = 2)
            )
    }

    height <- 1 + nlevels(data[[group_by]]) * 1
    width <- 3 + nlevels(data[[group_by]]) * 0.5
    if (!identical(legend.position, "none")) {
        if (legend.position %in% c("right", "left")) {
            width <- width + 1
        } else if (legend.direction == "horizontal") {
            height <- height + 1
        } else {
            height <- height + 2
        }
    }
    if (flip) {
        attr(p, "height") <- width
        attr(p, "width") <- height
    } else {
        attr(p, "height") <- height
        attr(p, "width") <- width
    }

    facet_plot(p,
        facet_by = facet_by, facet_scales = facet_scales, ncol = facet_ncol,
        nrow = facet_nrow, byrow = facet_byrow, legend.position = legend.position,
        legend.direction = legend.direction
    )
}

#' Ridge Plot
#'
#' @description Ridge plot to illustrate the distribution of the data in different groups.
#' @inheritParams common_args
#' @inheritParams RidgePlotAtomic
#' @export
#' @examples
#' set.seed(8525)
#' data <- data.frame(
#'    x = c(rnorm(250, -1), rnorm(250, 1)),
#'    group = rep(LETTERS[1:5], each = 100)
#' )
#' RidgePlot(data, x = "x", group_by = "group")
#' RidgePlot(data, x = "x", group_by = "group", reverse = TRUE)
#'
#' # wide form
#' data_wide <- data.frame(
#'    A = rnorm(100),
#'    B = rnorm(100),
#'    C = rnorm(100),
#'    D = rnorm(100),
#'    E = rnorm(100),
#'    group = sample(letters[1:4], 100, replace = TRUE)
#' )
#' RidgePlot(data_wide, group_by = LETTERS[1:5], in_form = "wide")
#' RidgePlot(data_wide, group_by = LETTERS[1:5], in_form = "wide", facet_by = "group")
RidgePlot <- function(
    data, x = NULL, in_form = c("long", "wide"), split_by = NULL, split_by_sep = "_",
    group_by = NULL, group_by_sep = "_", group_name = NULL,
    flip = FALSE, alpha = 1, theme = "theme_this", theme_args = list(), palette = "Paired", palcolor = NULL,
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL, x_text_angle = 90, keep_empty = FALSE, reverse = FALSE,
    facet_by = NULL, facet_scales = "fixed", facet_ncol = NULL, facet_nrow = NULL, facet_byrow = TRUE,
    aspect.ratio = 1, legend.position = "none", legend.direction = "vertical",
    combine = TRUE, nrow = NULL, ncol = NULL, byrow = TRUE, seed = 8525, ...) {

    validate_common_args(seed, facet_by = facet_by)
    split_by <- check_columns(data, split_by, force_factor = TRUE, allow_multi = TRUE,
        concat_multi = TRUE, concat_sep = split_by_sep)

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
            RidgePlotAtomic(datas[[nm]],
                x = x, in_form = in_form, group_by = group_by, group_by_sep = group_by_sep, group_name = group_name,
                flip = flip, alpha = alpha, theme = theme, theme_args = theme_args, palette = palette, palcolor = palcolor,
                title = title, subtitle = subtitle, xlab = xlab, ylab = ylab, x_text_angle = x_text_angle, keep_empty = keep_empty,
                reverse = reverse, facet_by = facet_by, facet_scales = facet_scales, facet_ncol = facet_ncol, facet_nrow = facet_nrow,
                aspect.ratio = aspect.ratio, legend.position = legend.position, legend.direction = legend.direction, ...
            )
        }
    )

    combine_plots(plots, combine = combine, nrow = nrow, ncol = ncol, byrow = byrow)
}

#' Density Plot / Histogram
#'
#' @description Density plot and histogram to illustrate the distribution of the data.
#' @rdname densityhistoplot
#' @inheritParams common_args
#' @inheritParams DensityHistoPlotAtomic
#' @return A ggplot object or wrap_plots object or a list of ggplot objects
#' @export
#' @examples
#' set.seed(8525)
#' data <- data.frame(
#'     x = c(rnorm(500, -1), rnorm(500, 1)),
#'     group = rep(c("A", "B"), each = 500),
#'     facet = sample(c("F1", "F2"), 1000, replace = TRUE)
#' )
#'
#' DensityPlot(data, x = "x")
#' DensityPlot(data, x = "x", group_by = "group", facet_by = "facet")
#' DensityPlot(data, x = "x", split_by = "facet", add_lines = FALSE)
DensityPlot <- function(
    data, x, group_by = NULL, group_by_sep = "_", group_name = NULL, split_by = NULL, split_by_sep = "_", flip = FALSE,
    palette = "Paired", palcolor = NULL, alpha = .5, theme = "theme_this", theme_args = list(),
    add_lines = TRUE, line_height = 0.025, line_alpha = 1, line_width = .1,
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL, expand = c(bottom = 0, left = 0, right = 0),
    facet_by = NULL, facet_scales = "free_y", facet_ncol = NULL, facet_nrow = NULL, facet_byrow = TRUE,
    legend.position = ifelse(is.null(group_by), "none", "right"), legend.direction = "vertical", seed = 8525,
    combine = TRUE, nrow = NULL, ncol = NULL, byrow = TRUE, ...) {
    validate_common_args(seed, facet_by = facet_by)
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
            DensityHistoPlotAtomic(datas[[nm]],
                x = x, group_by = group_by, group_by_sep = group_by_sep, group_name = group_name, type = "density", flip = flip,
                add_lines = add_lines, line_height = line_height, line_alpha = line_alpha, line_width = line_width,
                palette = palette, palcolor = palcolor, alpha = alpha, theme = theme, theme_args = theme_args,
                title = title, subtitle = subtitle, xlab = xlab, ylab = ylab, expand = expand,
                facet_by = facet_by, facet_scales = facet_scales, facet_ncol = facet_ncol, facet_nrow = facet_nrow, facet_byrow = facet_byrow,
                legend.position = legend.position, legend.direction = legend.direction, ...
            )
        }
    )

    combine_plots(plots, combine = combine, nrow = nrow, ncol = ncol, byrow = byrow)
}

#' @rdname densityhistoplot
#' @inheritParams common_args
#' @inheritParams DensityHistoPlotAtomic
#' @export
#' @examples
#' Histogram(data, x = "x")
#' Histogram(data, x = "x", group_by = "group", facet_by = "facet")
#' Histogram(data, x = "x", split_by = "facet", add_lines = FALSE)
Histogram <- function(
    data, x, group_by = NULL, group_by_sep = "_", group_name = NULL, split_by = NULL, split_by_sep = "_",
    flip = FALSE, bins = NULL, binwidth = NULL,
    add_lines = TRUE, line_height = 0.025, line_alpha = 1, line_width = .1,
    palette = "Paired", palcolor = NULL, alpha = .5, theme = "theme_this", theme_args = list(),
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL, expand = c(bottom = 0, left = 0, right = 0),
    facet_by = NULL, facet_scales = "free_y", facet_ncol = NULL, facet_nrow = NULL, facet_byrow = TRUE,
    legend.position = ifelse(is.null(group_by), "none", "right"), legend.direction = "vertical", seed = 8525,
    combine = TRUE, nrow = NULL, ncol = NULL, byrow = TRUE, ...) {
    validate_common_args(seed, facet_by = facet_by)
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
            DensityHistoPlotAtomic(datas[[nm]],
                x = x, group_by = group_by, group_by_sep = group_by_sep, group_name = group_name, type = "histogram",
                add_lines = add_lines, line_height = line_height, line_alpha = line_alpha, line_width = line_width,
                flip = flip, bins = bins, binwidth = binwidth, expand = expand,
                palette = palette, palcolor = palcolor, alpha = alpha, theme = theme, theme_args = theme_args,
                title = title, subtitle = subtitle, xlab = xlab, ylab = ylab,
                facet_by = facet_by, facet_scales = facet_scales, facet_ncol = facet_ncol, facet_nrow = facet_nrow, facet_byrow = facet_byrow,
                legend.position = legend.position, legend.direction = legend.direction, ...
            )
        }
    )

    combine_plots(plots, combine = combine, nrow = nrow, ncol = ncol, byrow = byrow)
}
