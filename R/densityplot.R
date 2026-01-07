#' Atomic density/histogram plot
#'
#' @inheritParams common_args
#' @param x A character string specifying the column name for the values
#'   A numeric column is expected.
#' @param group_by A character string specifying the column name to group the data
#' @param group_by_sep A character string to concatenate the columns in `group_by` if multiple columns are provided
#' @param group_name A character string to name the legend of group_by
#' @param xtrans A character string specifying the transformation of the x-axis. Default is "identity".
#'  Other options see transform of \code{\link[ggplot2]{scale_x_continuous}}.
#' @param ytrans A character string specifying the transformation of the y-axis. Default is "identity".
#'  Other options see transform of \code{\link[ggplot2]{scale_y_continuous}}.
#' @param type A character string specifying the type of plot. Default is "density".
#'  Other options are "histogram".
#' @param bins A numeric value specifying the number of bins for the histogram.
#' @param binwidth A numeric value specifying the width of the bins for the histogram.
#' @param flip A logical value. If TRUE, the plot will be flipped.
#' @param add_bars A logical value. If TRUE, add lines to the plot to show the data distribution on the bottom.
#' @param bar_height A numeric value specifying the height of the bars. The actual height will be calculated based on the maximum density or count.
#' @param bar_alpha A numeric value specifying the alpha of the bars.
#' @param bar_width A numeric value specifying the width of the bars.
#' @param position How should we position the values in each bin? Default is "identity".
#'  Unlike the default position = "stack" in [ggplot2::geom_histogram] or [ggplot2::geom_density],
#'  the default position is "identity" to show the actual count or density for each group.
#' @param use_trend A logical value. If TRUE, use trend line instead of histogram.
#' @param add_trend A logical value. If TRUE, add trend line to the histogram.
#' @param trend_alpha A numeric value specifying the alpha of the trend line and points
#' @param trend_linewidth A numeric value specifying the width of the trend line
#' @param trend_pt_size A numeric value specifying the size of the trend points
#' @param trend_skip_zero A logical value. If TRUE, skip the zero count when drawing the trend line.
#' @importFrom utils getFromNamespace
#' @importFrom zoo na.approx
#' @importFrom ggplot2 geom_density scale_fill_manual labs theme geom_histogram coord_flip waiver
#' @importFrom ggplot2 scale_color_manual scale_x_continuous scale_y_continuous stat_bin
#' @keywords internal
DensityHistoPlotAtomic <- function(
    data, x, group_by = NULL, group_by_sep = "_", group_name = NULL, xtrans = "identity", ytrans = "identity",
    type = c("density", "histogram"), bins = NULL, binwidth = NULL, flip = FALSE,
    add_bars = FALSE, bar_height = 0.025, bar_alpha = 1, bar_width = .1, position = "identity",
    use_trend = FALSE, add_trend = FALSE, trend_alpha = 1, trend_linewidth = 0.8, trend_pt_size = 1.5, trend_skip_zero = FALSE,
    palette = "Paired", palcolor = NULL, alpha = .5, theme = "theme_this", theme_args = list(),
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL, expand = c(bottom = 0, left = 0, right = 0),
    facet_by = NULL, facet_scales = "fixed", facet_ncol = NULL, facet_nrow = NULL, facet_byrow = TRUE,
    legend.position = ifelse(is.null(group_by), "none", "right"), legend.direction = "vertical", ...) {
    ggplot <- if (getOption("plotthis.gglogger.enabled", FALSE)) {
        gglogger::ggplot
    } else {
        ggplot2::ggplot
    }
    type <- match.arg(type)
    expand <- norm_expansion(expand, x_type = "continuous", y_type = "continuous")
    x <- check_columns(data, x)
    group_by <- check_columns(data, group_by, force_factor = TRUE, allow_multi = TRUE, concat_multi = TRUE, concat_sep = group_by_sep)
    if (is.null(group_by)) {
        group_by <- ".group"
        data[[group_by]] <- factor("")
    }
    if (is.null(bins) && is.null(binwidth) && type == "histogram") {
        bins <- 30
        message("Using `bins = 30`. Pick better value with `binwidth`.")
    }
    if (isTRUE(add_bars)) {
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
        lnheight <- bar_height * max_y
        # calculate the ymin ymax for each group to plot the data lines
        data$.ymin <- lnheight * (1 - as.integer(data[[group_by]]))
        data$.ymax <- data$.ymin - lnheight
    }

    p <- ggplot(data, aes(x = !!sym(x), fill = !!sym(group_by), color = !!sym(group_by))) +
        scale_fill_manual(
            name = group_name %||% group_by,
            values = palette_this(levels(data[[group_by]]), palette = palette, palcolor = palcolor)
        ) +
        scale_color_manual(
            name = group_name %||% group_by,
            values = palette_this(levels(data[[group_by]]), palette = palette, palcolor = palcolor)
        )

    if (type == "histogram") {
        if (!use_trend) {
            p <- p + geom_histogram(alpha = alpha, bins = bins, binwidth = binwidth, position = position, ...)
        }
        if (use_trend || add_trend) {
            p <- p + stat_bin(geom = "point", bins = bins, binwidth = binwidth, alpha = trend_alpha,
                size = trend_pt_size, position = position, ...)
            if (trend_skip_zero) {
                if (inherits(ytrans, "transform")) {
                    ytrans_obj <- ytrans
                } else if (is.character(ytrans)) {
                    ytrans_obj <- getFromNamespace(paste0("transform_", ytrans), "scales")()
                } else if (is.function(ytrans)) {
                    ytrans_obj <- ytrans()
                } else {
                    stop("ytrans should be a character, a transform object, or a function returning a transform object.")
                }
                p <- p + stat_bin(
                    aes(y = after_stat({
                        y <- ifelse(!!sym("count") > 0, !!sym("count"), NA)
                        y <- ytrans_obj$transform(y)
                        y <- split(y, !!sym("..group.."))
                        y <- unlist(lapply(y, na.approx, na.rm = FALSE))
                        ytrans_obj$inverse(y)
                    })), bins = bins, binwidth = binwidth,
                    geom = "line", position = position, linewidth = trend_linewidth, ...)
            } else {
                p <- p + stat_bin(aes(y = after_stat(!!sym("count"))), bins = bins, binwidth = binwidth,
                    geom = "line", position = position, linewidth = trend_linewidth, ...)
            }
        }
    } else {
        p <- p + geom_density(alpha = alpha, position = position, ...)
    }
    if (isTRUE(add_bars)) {
        p <- p +
            geom_linerange(aes(ymin = !!sym(".ymin"), ymax = !!sym(".ymax")), alpha = bar_alpha, linewidth = bar_width)
    }
    p <- p +
        scale_x_continuous(expand = expand$x, transform = xtrans) +
        scale_y_continuous(expand = expand$y, transform = ytrans) +
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
#' @param scale A numeric value to scale the ridges.
#'  See also \code{\link[ggridges]{geom_density_ridges}}.
#' @param add_vline A numeric vector or a named list of numeric values to add vertical lines to the plot.
#' If a named list is provided, the names should match the levels of 'group_by'.
#' If `TRUE`, the vertical lines will be added at the mean of each group.
#' @param vline_type The type of line to draw for the vertical line.
#' @param vline_color The color of the vertical line.
#' If `TRUE`, the vertical lines will be colored according to the group colors.
#' @param vline_width The width of the vertical line.
#' @param vline_alpha The alpha value of the vertical line.
#' @param ... Additional arguments.
#' @keywords internal
RidgePlotAtomic <- function(
    data, x = NULL, in_form = c("long", "wide"), group_by = NULL, group_by_sep = "_", group_name = NULL,
    add_vline = NULL, vline_type = "solid", vline_color = TRUE, vline_width = 0.5, vline_alpha = 1,
    flip = FALSE, alpha = 0.8, scale = NULL, theme = "theme_this", theme_args = list(), palette = "Paired", palcolor = NULL,
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL, x_text_angle = 90, keep_empty = FALSE, reverse = FALSE,
    facet_by = NULL, facet_scales = "fixed", facet_ncol = NULL, facet_nrow = NULL, facet_byrow = TRUE,
    aspect.ratio = 1, legend.position = "none", legend.direction = "vertical", ...) {
    ggplot <- if (getOption("plotthis.gglogger.enabled", FALSE)) {
        gglogger::ggplot
    } else {
        ggplot2::ggplot
    }
    in_form <- match.arg(in_form)
    if (in_form == "wide") {
        data <- data %>% pivot_longer(cols = group_by, names_to = ".group", values_to = ".x")
        x <- ".x"
        group_by <- ".group"
    }
    x <- check_columns(data, x)
    group_by <- check_columns(data, group_by, force_factor = TRUE, allow_multi = TRUE, concat_multi = TRUE, concat_sep = group_by_sep)
    if (is.null(group_by)) {
        group_by <- ".group"
        data[[group_by]] <- factor(" ")
    }
    if (isTRUE(reverse)) {
        data[[group_by]] <- factor(data[[group_by]], levels = rev(levels(data[[group_by]])))
    }

    colors <- palette_this(levels(data[[group_by]]), palette = palette, palcolor = palcolor)
    p <- ggplot(data, aes(x = !!sym(x), y = !!sym(group_by), fill = !!sym(group_by)))

    if (!is.null(scale)) {
        p <- p + ggridges::geom_density_ridges(alpha = alpha, scale = scale)
    } else {
        # Let the geom_density_ridges function to calculate the scale
        p <- p + ggridges::geom_density_ridges(alpha = alpha)
    }
    if (!is.null(add_vline) && !isFALSE(add_vline)) {
        if (isTRUE(add_vline)) {
            # calculate the mean of each group
            add_vline <- tapply(data[[x]], data[[group_by]], mean, na.rm = TRUE)
        }
        if (isTRUE(vline_color)) {
            if (!is.list(add_vline)) {
                add_vline <- as.list(add_vline)
                if (reverse) {
                    names(add_vline) <- rev(levels(data[[group_by]]))[1:length(add_vline)]
                } else {
                    names(add_vline) <- levels(data[[group_by]])[1:length(add_vline)]
                }
            }
            add_vline <- add_vline[intersect(levels(data[[group_by]]), names(add_vline))]
            vline_color <- sapply(
                colors[names(add_vline)],
                function(cl) blend_colors(c(cl, cl, cl), mode = "multiply")
            )
            add_vline <- unlist(add_vline, use.names = FALSE)
        }
        p <- p + geom_vline(
            xintercept = add_vline, linetype = vline_type, linewidth = vline_width,
            color = vline_color, alpha = vline_alpha
        )
    }
    p <- p +
        scale_fill_manual(values = colors) +
        scale_y_discrete(drop = !keep_empty, expand = c(0, 0)) +
        scale_x_continuous(expand = c(0, 0)) +
        labs(title = title, subtitle = subtitle, x = xlab %||% x, y = ylab %||% group_by)

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

    p <- p +
        do.call(theme, theme_args) +
        ggplot2::theme(
            aspect.ratio = aspect.ratio,
            legend.position = legend.position,
            legend.direction = legend.direction
        )

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
#' @return A ggplot object or wrap_plots object or a list of ggplot objects.
#'  If no `split_by` is provided, a single plot (ggplot object) will be returned.
#'  If 'combine' is TRUE, a wrap_plots object will be returned.
#'  If 'combine' is FALSE, a list of ggplot objects will be returned.
#' @export
#' @examples
#' set.seed(8525)
#' data <- data.frame(
#'    x = c(rnorm(250, -1), rnorm(250, 1)),
#'    group = rep(LETTERS[1:5], each = 100)
#' )
#' RidgePlot(data, x = "x")  # fallback to a density plot
#' RidgePlot(data, x = "x", add_vline = 0, vline_color = "black")
#' RidgePlot(data, x = "x", group_by = "group")
#' RidgePlot(data, x = "x", group_by = "group", reverse = TRUE)
#' RidgePlot(data, x = "x", group_by = "group",
#'    add_vline = TRUE, vline_color = TRUE, alpha = 0.7)
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
#' RidgePlot(data_wide, group_by = LETTERS[1:5], in_form = "wide", split_by = "group",
#'    palette = list(a = "Reds", b = "Blues", c = "Greens", d = "Purples"))
RidgePlot <- function(
    data, x = NULL, in_form = c("long", "wide"), split_by = NULL, split_by_sep = "_",
    group_by = NULL, group_by_sep = "_", group_name = NULL, scale = NULL,
    add_vline = NULL, vline_type = "solid", vline_color = TRUE, vline_width = 0.5, vline_alpha = 1,
    flip = FALSE, alpha = 0.8, theme = "theme_this", theme_args = list(), palette = "Paired", palcolor = NULL,
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL, x_text_angle = 90, keep_empty = FALSE, reverse = FALSE,
    facet_by = NULL, facet_scales = "fixed", facet_ncol = NULL, facet_nrow = NULL, facet_byrow = TRUE,
    aspect.ratio = 1, legend.position = "none", legend.direction = "vertical",
    combine = TRUE, nrow = NULL, ncol = NULL, byrow = TRUE, seed = 8525,
    axes = NULL, axis_titles = axes, guides = NULL, design = NULL, ...) {

    validate_common_args(seed, facet_by = facet_by)
    theme <- process_theme(theme)
    split_by <- check_columns(data, split_by, force_factor = TRUE, allow_multi = TRUE,
        concat_multi = TRUE, concat_sep = split_by_sep)

    if (!is.null(split_by)) {
        data[[split_by]] <- droplevels(data[[split_by]])
        datas <- split(data, data[[split_by]])
        # keep the order of levels
        datas <- datas[levels(data[[split_by]])]
    } else {
        datas <- list(data)
        names(datas) <- "..."
    }

    palette <- check_palette(palette, names(datas))
    palcolor <- check_palcolor(palcolor, names(datas))
    legend.direction <- check_legend(legend.direction, names(datas), "legend.direction")
    legend.position <- check_legend(legend.position, names(datas), "legend.position")

    plots <- lapply(
        names(datas), function(nm) {
            default_title <- if (length(datas) == 1 && identical(nm, "...")) NULL else nm
            if (is.function(title)) {
                title <- title(default_title)
            } else {
                title <- title %||% default_title
            }
            RidgePlotAtomic(datas[[nm]],
                x = x, in_form = in_form, group_by = group_by, group_by_sep = group_by_sep, group_name = group_name, scale = scale,
                add_vline = add_vline, vline_type = vline_type, vline_color = vline_color, vline_width = vline_width, vline_alpha = vline_alpha,
                flip = flip, alpha = alpha, theme = theme, theme_args = theme_args, palette = palette[[nm]], palcolor = palcolor[[nm]],
                title = title, subtitle = subtitle, xlab = xlab, ylab = ylab, x_text_angle = x_text_angle, keep_empty = keep_empty,
                reverse = reverse, facet_by = facet_by, facet_scales = facet_scales, facet_ncol = facet_ncol, facet_nrow = facet_nrow,
                aspect.ratio = aspect.ratio, legend.position = legend.position[[nm]], legend.direction = legend.direction[[nm]], ...
            )
        }
    )

    combine_plots(plots, combine = combine, nrow = nrow, ncol = ncol, byrow = byrow,
        axes = axes, axis_titles = axis_titles, guides = guides, design = design)
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
#' DensityPlot(data, x = "x", split_by = "facet", add_bars = TRUE)
#' DensityPlot(data, x = "x", split_by = "facet", add_bars = TRUE,
#'     palette = c(F1 = "Set1", F2 = "Set2"))
DensityPlot <- function(
    data, x, group_by = NULL, group_by_sep = "_", group_name = NULL, xtrans = "identity", ytrans = "identity",
    split_by = NULL, split_by_sep = "_", flip = FALSE, position = "identity",
    palette = "Paired", palcolor = NULL, alpha = .5, theme = "theme_this", theme_args = list(),
    add_bars = FALSE, bar_height = 0.025, bar_alpha = 1, bar_width = .1,
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL, expand = c(bottom = 0, left = 0, right = 0),
    facet_by = NULL, facet_scales = "free_y", facet_ncol = NULL, facet_nrow = NULL, facet_byrow = TRUE,
    legend.position = ifelse(is.null(group_by), "none", "right"), legend.direction = "vertical", seed = 8525,
    combine = TRUE, nrow = NULL, ncol = NULL, byrow = TRUE, axes = NULL, axis_titles = axes, guides = NULL, design = NULL,
    ...) {
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

    palette <- check_palette(palette, names(datas))
    palcolor <- check_palcolor(palcolor, names(datas))
    legend.direction <- check_legend(legend.direction, names(datas), "legend.direction")
    legend.position <- check_legend(legend.position, names(datas), "legend.position")

    plots <- lapply(
        names(datas), function(nm) {
            default_title <- if (length(datas) == 1 && identical(nm, "...")) NULL else nm
            if (is.function(title)) {
                title <- title(default_title)
            } else {
                title <- title %||% default_title
            }
            DensityHistoPlotAtomic(datas[[nm]],
                x = x, group_by = group_by, group_by_sep = group_by_sep, group_name = group_name,
                type = "density", flip = flip, xtrans = xtrans, ytrans = ytrans, position = position,
                add_bars = add_bars, bar_height = bar_height, bar_alpha = bar_alpha, bar_width = bar_width,
                palette = palette[[nm]], palcolor = palcolor[[nm]], alpha = alpha, theme = theme, theme_args = theme_args,
                title = title, subtitle = subtitle, xlab = xlab, ylab = ylab, expand = expand,
                facet_by = facet_by, facet_scales = facet_scales, facet_ncol = facet_ncol, facet_nrow = facet_nrow, facet_byrow = facet_byrow,
                legend.position = legend.position[[nm]], legend.direction = legend.direction[[nm]], ...
            )
        }
    )

    combine_plots(plots, combine = combine, nrow = nrow, ncol = ncol, byrow = byrow,
        axes = axes, axis_titles = axis_titles, guides = guides, design = design)
}

#' @rdname densityhistoplot
#' @inheritParams common_args
#' @inheritParams DensityHistoPlotAtomic
#' @export
#' @examples
#' set.seed(8525)
#' data <- data.frame(
#'     x = sample(setdiff(1:100, c(30:36, 50:55, 70:77)), 1000, replace = TRUE),
#'     group = factor(rep(c("A", "B"), each = 500), levels = c("A", "B")),
#'     facet = sample(c("F1", "F2"), 1000, replace = TRUE)
#' )
#'
#' Histogram(data, x = "x")
#' Histogram(data, x = "x", group_by = "group")
#' Histogram(data, x = "x", split_by = "facet", add_bars = TRUE)
#' Histogram(data, x = "x", group_by = "group", add_trend = TRUE)
#' Histogram(data, x = "x", group_by = "group", add_trend = TRUE, trend_skip_zero = TRUE)
#' Histogram(data, x = "x", group_by = "group", split_by = "facet",
#'  use_trend = TRUE, trend_pt_size = 3)
#' Histogram(data, x = "x", group_by = "group", split_by = "facet",
#'  palette = c(F1 = "Paired", F2 = "Spectral"))
Histogram <- function(
    data, x, group_by = NULL, group_by_sep = "_", group_name = NULL, xtrans = "identity", ytrans = "identity",
    split_by = NULL, split_by_sep = "_", flip = FALSE, bins = NULL, binwidth = NULL, trend_skip_zero = FALSE,
    add_bars = FALSE, bar_height = 0.025, bar_alpha = 1, bar_width = .1, position = "identity",
    use_trend = FALSE, add_trend = FALSE, trend_alpha = 1, trend_linewidth = 0.8, trend_pt_size = 1.5,
    palette = "Paired", palcolor = NULL, alpha = .5, theme = "theme_this", theme_args = list(),
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL, expand = c(bottom = 0, left = 0, right = 0),
    facet_by = NULL, facet_scales = "free_y", facet_ncol = NULL, facet_nrow = NULL, facet_byrow = TRUE,
    legend.position = ifelse(is.null(group_by), "none", "right"), legend.direction = "vertical", seed = 8525,
    combine = TRUE, nrow = NULL, ncol = NULL, byrow = TRUE, axes = NULL, axis_titles = axes, guides = NULL, design = NULL,
    ...) {
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

    palette <- check_palette(palette, names(datas))
    palcolor <- check_palcolor(palcolor, names(datas))
    legend.direction <- check_legend(legend.direction, names(datas), "legend.direction")
    legend.position <- check_legend(legend.position, names(datas), "legend.position")

    plots <- lapply(
        names(datas), function(nm) {
            default_title <- if (length(datas) == 1 && identical(nm, "...")) NULL else nm
            if (is.function(title)) {
                title <- title(default_title)
            } else {
                title <- title %||% default_title
            }
            DensityHistoPlotAtomic(datas[[nm]],
                x = x, group_by = group_by, group_by_sep = group_by_sep, group_name = group_name,
                type = "histogram", flip = flip, xtrans = xtrans, ytrans = ytrans, use_trend = use_trend, trend_skip_zero = trend_skip_zero,
                add_trend = add_trend, trend_alpha = trend_alpha, trend_linewidth = trend_linewidth, trend_pt_size = trend_pt_size,
                add_bars = add_bars, bar_height = bar_height, bar_alpha = bar_alpha, bar_width = bar_width,
                bins = bins, binwidth = binwidth, expand = expand, position = position,
                palette = palette[[nm]], palcolor = palcolor[[nm]], alpha = alpha, theme = theme, theme_args = theme_args,
                title = title, subtitle = subtitle, xlab = xlab, ylab = ylab,
                facet_by = facet_by, facet_scales = facet_scales, facet_ncol = facet_ncol, facet_nrow = facet_nrow, facet_byrow = facet_byrow,
                legend.position = legend.position[[nm]], legend.direction = legend.direction[[nm]], ...
            )
        }
    )

    combine_plots(plots, combine = combine, nrow = nrow, ncol = ncol, byrow = byrow,
        axes = axes, axis_titles = axis_titles, guides = guides, design = design)
}
