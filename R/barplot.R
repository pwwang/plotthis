#' BarPlotSingle
#'
#' @description Create a bar plot without groups.
#'
#' @inheritParams common_args
#' @param y A character vector specifying the column as the y axis of the plot.
#'   Default is NULL, meaning the y axis is the count of the data.
#' @param fill_by_x A logical value indicating whether to fill the bars by the x-axis values.
#'   If FALSE, the bars will be filled a single color (the first color in the palette).
#' @param width A numeric value specifying the width of the bars.
#' @param flip A logical value indicating whether to flip the x and y axes.
#' @param add_line A numeric value indicating the y value to add a horizontal line.
#' @param line_color A character string indicating the color of the line.
#' @param line_size A numeric value indicating the size of the line.
#' @param line_type A numeric value indicating the type of the line.
#' @param line_name A character string indicating the name of the line.
#' @param add_trend A logical value to add trend line to the plot.
#' @param trend_color A character string to specify the color of the trend line.
#' @param trend_linewidth A numeric value to specify the width of the trend line.
#' @param trend_ptsize A numeric value to specify the size of the trend line points.
#' @param y_min A numeric value to specify the minimum value of the y axis.
#' @param y_max A numeric value to specify the maximum value of the y axis.
#' @return A ggplot object.
#' @keywords internal
#' @importFrom rlang sym %||%
#' @importFrom dplyr %>% group_by summarise n
#' @importFrom gglogger ggplot
#' @importFrom ggplot2 aes geom_bar scale_fill_manual labs scale_x_discrete scale_y_continuous
#' @importFrom ggplot2 element_line waiver coord_flip scale_color_manual guide_legend coord_cartesian
BarPlotSingle <- function(
    data, x, y = NULL, flip = FALSE, facet_by = NULL,
    theme = "theme_this", theme_args = list(), palette = "Paired", palcolor = NULL,
    alpha = 1, x_text_angle = 0, aspect.ratio = 1, y_min = NULL, y_max = NULL,
    legend.position = "right", legend.direction = "vertical",
    add_line = NULL, line_color = "red2", line_size = .6, line_type = 2, line_name = NULL,
    add_trend = FALSE, trend_color = "black", trend_linewidth = 1, trend_ptsize = 2.5,
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL, keep_empty = FALSE,
    expand = 0, fill_by_x = TRUE, width = 0.9, ...
) {
    if (inherits(width, "waiver")) width <- 0.9
    if (inherits(expand, "waiver")) expand <- 0
    expand <- norm_expansion(expand, x_type = "discrete", y_type = "continuous")

    x <- check_columns(data, x, force_factor = TRUE)
    y <- check_columns(data, y)
    facet_by <- check_columns(data, facet_by, force_factor = TRUE, allow_multi = TRUE)
    if (is.null(y)) {
        data <- data %>% group_by(!!!syms(unique(c(x, facet_by)))) %>% summarise(.y = n(), .groups = "drop")
        y <- ".y"
    }

    if (isTRUE(fill_by_x)) {
        p <- ggplot(data, aes(x = !!sym(x), y = !!sym(y), fill = !!sym(x)))
        colors <- palette_this(levels(data[[x]]), palette = palette, palcolor = palcolor)
        guide = guide_legend(order = 1)
    } else {
        p <- ggplot(data, aes(x = !!sym(x), y = !!sym(y), fill = "fill"))
        colors <- palette_this("fill", palette = palette, palcolor = palcolor)
        guide = "none"
    }
    just <- calc_just(x_text_angle)

    p <- p + geom_col(alpha = alpha, width = width) +
        scale_fill_manual(name = x, values = colors, guide = guide) +
        labs(title = title, subtitle = subtitle, x = xlab %||% x, y = ylab %||% y) +
        scale_x_discrete(drop = !keep_empty, expand = expand$x) +
        scale_y_continuous(expand = expand$y) +
        do.call(theme, theme_args) +
        ggplot2::theme(
            aspect.ratio = aspect.ratio,
            legend.position = legend.position,
            legend.direction = legend.direction,
            panel.grid.major = element_line(colour = "grey80", linetype = 2),
            axis.text.x = element_text(angle = x_text_angle, hjust = just$h, vjust = just$v)
        )

    if (isTRUE(add_trend)) {
        p <- p +
            geom_line(aes(group = 1), position = position_dodge(width = 0.9), color = trend_color, linewidth = trend_linewidth) +
            geom_point(
                position = position_dodge(width = 0.9), color = "black", fill = "white",
                size = trend_ptsize, shape = 21
            )
    }

    if (!is.null(add_line)) {
        p <- p + geom_hline(
            aes(color = line_name %||% as.character(add_line), yintercept = add_line),
            linetype = line_type, linewidth = line_size
        ) + scale_color_manual(name = NULL, values = line_color, guide = guide_legend(order = 2))
    }

    if (isTRUE(flip)) {
        p <- p + coord_flip(ylim = c(y_min, y_max))
    } else {
        p <- p + coord_cartesian(ylim = c(y_min, y_max))
    }

    height <- 4.5
    width <- .5 + length(levels(data[[x]])) * .8
    if (!identical(legend, "none")) {
        if (legend.position %in% c("right", "left")) {
            width <- width + 1
        } else if (legend.direction == "horizontal") {
            height <- height + 1
        } else {
            width <- width + 2
        }
    }

    if (isTRUE(flip)) {
        attr(p, "height") <- width
        attr(p, "width") <- height
    } else {
        attr(p, "height") <- height
        attr(p, "width") <- width
    }
    p
}

#' Bar plot with groups
#'
#' @description Create a bar plot with groups.
#'
#' @inheritParams common_args
#' @param y A character vector specifying the column as the y axis of the plot.
#'  Default is NULL, meaning the y axis is the count of the data.
#' @param position A character string indicating the position of the bars.
#'  If "auto", the position will be "stack" if group_by has more than 5 levels, otherwise "dodge".
#'  "fill" is also a valid option. Only works when group_by is not NULL.
#' @param position_dodge_preserve Should dodging preserve the "total" width of all elements at a position, or the width of a "single" element?
#' @param add_bg A logical value indicating whether to add a background to the plot.
#' @param bg_palette A character string indicating the palette to use for the background.
#' @param bg_palcolor A character string indicating the color to use for the background.
#' @param bg_alpha A numeric value indicating the alpha of the background.
#' @param flip A logical value indicating whether to flip the x and y axes.
#' @param add_line A numeric value indicating the y value to add a horizontal line.
#' @param line_color A character string indicating the color of the line.
#' @param line_size A numeric value indicating the size of the line.
#' @param line_type A numeric value indicating the type of the line.
#' @param line_name A character string indicating the name of the line.
#' @param add_trend A logical value to add trend line to the plot.
#' @param trend_color A character string to specify the color of the trend line.
#' @param trend_linewidth A numeric value to specify the width of the trend line.
#' @param trend_ptsize A numeric value to specify the size of the trend line points.
#' @param y_min A numeric value to specify the minimum value of the y axis.
#' @param y_max A numeric value to specify the maximum value of the y axis.
#' @return A ggplot object.
#' @keywords internal
#' @importFrom rlang sym %||%
#' @importFrom dplyr %>% group_by summarise n
#' @importFrom gglogger ggplot
#' @importFrom ggplot2 aes geom_bar scale_fill_manual labs position_dodge2 coord_flip guide_legend scale_color_manual
BarPlotGrouped <- function(
    data, x, y = NULL, flip = FALSE, group_by, group_by_sep = "_",
    theme = "theme_this", theme_args = list(), palette = "Paired", palcolor = NULL,
    add_bg = FALSE, bg_palette = "stripe", bg_palcolor = NULL, bg_alpha = 0.2,
    alpha = 1, x_text_angle = 0, aspect.ratio = 1,
    add_line = NULL, line_color = "red2", line_size = .6, line_type = 2, line_name = NULL,
    add_trend = FALSE, trend_color = "black", trend_linewidth = 1, trend_ptsize = 2.5,
    position = "auto", position_dodge_preserve = "total", y_min = NULL, y_max = NULL,
    legend.position = "right", legend.direction = "vertical",
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL, keep_empty = FALSE,
    expand = c(bottom = 0), width = 0.8, facet_by = NULL, ...
) {
    group_by <- check_columns(data, group_by, force_factor = TRUE, allow_multi = TRUE, concat_multi = TRUE, concat_sep = group_by_sep)
    facet_by <- check_columns(data, facet_by, force_factor = TRUE, allow_multi = TRUE)
    x <- check_columns(data, x, force_factor = TRUE)
    y <- check_columns(data, y)
    if (is.null(y)) {
        data <- data %>%
            group_by(!!!syms(unique(c(x, group_by, facet_by)))) %>%
            summarise(.y = n(), .groups = "drop")
        y <- ".y"
    }
    if (inherits(width, "waiver")) width <- 0.8
    if (inherits(expand, "waiver")) {
        if (min(data[[y]], na.rm = TRUE) > 0) {
            expand <- c(bottom = 0)
        } else if (max(data[[y]], na.rm = TRUE) < 0) {
            expand <- c(top = 0)
        } else {
            expand <- NULL
        }
    }
    expand <- norm_expansion(expand, x_type = "discrete", y_type = "continuous")

    p <- ggplot(data, aes(x = !!sym(x), y = !!sym(y), fill = !!sym(group_by)))
    if (isTRUE(add_bg)) {
        p <- p + bg_layer(data, x, bg_palette, bg_palcolor, bg_alpha, keep_empty, facet_by)
    }

    colors <- palette_this(levels(data[[group_by]]), palette = palette, palcolor = palcolor)
    just <- calc_just(x_text_angle)
    if (position == "auto") {
        position <- if (length(colors) <= 5) position_dodge2(preserve = position_dodge_preserve) else "stack"
    } else if (position == "dodge") {
        position <- position_dodge2(preserve = position_dodge_preserve)
    }

    p <- p + geom_col(alpha = alpha, position = position, width = width) +
        scale_fill_manual(name = group_by, values = colors, drop = !keep_empty, guide = guide_legend(order = 1)) +
        labs(title = title, subtitle = subtitle, x = xlab %||% x, y = ylab %||% y) +
        scale_x_discrete(drop = !keep_empty, expand = expand$x) +
        scale_y_continuous(expand = expand$y) +
        do.call(theme, theme_args) +
        ggplot2::theme(
            aspect.ratio = aspect.ratio,
            legend.position = legend.position,
            legend.direction = legend.direction,
            panel.grid.major = element_line(colour = "grey80", linetype = 2),
            axis.text.x = element_text(angle = x_text_angle, hjust = just$h, vjust = just$v)
        )

    if (isTRUE(add_trend)) {
        if (is.null(trend_color)) {
            p <- p +
                geom_line(aes(group = !!sym(group_by), color = !!sym(group_by)),
                    position = position_dodge(width = 0.9), linewidth = trend_linewidth, show.legend = FALSE) +
                scale_color_manual(values = colors)
        } else {
            p <- p +
                geom_line(aes(group = !!sym(group_by)), position = position_dodge(width = 0.9), color = trend_color, linewidth = trend_linewidth)
        }
        p <- p + geom_point(
                position = position_dodge(width = 0.9), color = "black", fill = "white",
                size = trend_ptsize, shape = 21
            )
    }

    if (!is.null(add_line)) {
        p <- p + geom_hline(
            aes(color = line_name %||% as.character(add_line), yintercept = add_line),
            linetype = line_type, linewidth = line_size
        ) + scale_color_manual(name = NULL, values = line_color, guide = guide_legend(order = 2))
    }

    if (isTRUE(flip)) {
        p <- p + coord_flip(ylim = c(y_min, y_max))
    } else {
        p <- p + coord_cartesian(ylim = c(y_min, y_max))
    }

    height <- 4.5
    width <- .5 + length(levels(data[[x]])) * length(unique(data[[group_by]])) * .5
    if (legend.position %in% c("right", "left")) {
        width <- width + 1
    } else if (legend.direction == "horizontal") {
        height <- height + 1
    } else {
        width <- width + 2
    }

    if (isTRUE(flip)) {
        attr(p, "height") <- width
        attr(p, "width") <- height
    } else {
        attr(p, "height") <- height
        attr(p, "width") <- width
    }
    p
}

#' Atomic bar plot
#' @description Create a bar plot with or without groups. This function does not handle splitting but only facetting.
#' @inheritParams common_args
#' @param y A character vector specifying the column as the y axis of the plot.
#'  Default is NULL, meaning the y axis is the count of the data.
#' @param fill_by_x_if_no_group A logical value indicating whether to fill the bars by the x-axis values if there is no group_by.
#' @param flip A logical value indicating whether to flip the x and y axes.
#' @return A ggplot object.
#' @importFrom ggplot2 waiver
#' @keywords internal
BarPlotAtomic <- function(
    data, x, y = NULL, flip = FALSE, group_by = NULL, fill_by_x_if_no_group = TRUE,
    theme = "theme_this", theme_args = list(), palette = "Paired", palcolor = NULL,
    alpha = 1, x_text_angle = 0, aspect.ratio = 1,
    add_line = NULL, line_color = "red2", line_size = .6, line_type = 2, line_name = NULL,
    add_trend = FALSE, trend_color = "black", trend_linewidth = 1, trend_ptsize = 2,
    position = "auto", position_dodge_preserve = "total", y_min = NULL, y_max = NULL,
    legend.position = "right", legend.direction = "vertical",
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL, keep_empty = FALSE,
    expand = waiver(), width = waiver(), facet_by = NULL, facet_scales = "fixed",
    facet_nrow = NULL, facet_ncol = NULL, facet_byrow = TRUE, ...
) {
    if (is.null(group_by)) {
        p <- BarPlotSingle(
            data, x, y, facet_by = facet_by, flip = flip, line_name = line_name,
            theme = theme, theme_args = theme_args, palette = palette, palcolor = palcolor,
            alpha = alpha, x_text_angle = x_text_angle, aspect.ratio = aspect.ratio,
            add_line = add_line, line_color = line_color, line_size = line_size, line_type = line_type,
            add_trend = add_trend, trend_color = trend_color, trend_linewidth = trend_linewidth, trend_ptsize = trend_ptsize,
            legend.position = legend.position, legend.direction = legend.direction, y_min = y_min, y_max = y_max,
            title = title, subtitle = subtitle, xlab = xlab, ylab = ylab, keep_empty = keep_empty,
            expand = expand, fill_by_x = fill_by_x_if_no_group, width = width, ...
        )
    } else {
        p <- BarPlotGrouped(
            data, x, y, group_by, facet_by = facet_by, flip = flip, line_name = line_name,
            theme = theme, theme_args = theme_args, palette = palette, palcolor = palcolor,
            alpha = alpha, x_text_angle = x_text_angle, aspect.ratio = aspect.ratio,
            position = position, position_dodge_preserve = position_dodge_preserve, y_min = y_min, y_max = y_max,
            add_line = add_line, line_color = line_color, line_size = line_size, line_type = line_type,
            add_trend = add_trend, trend_color = trend_color, trend_linewidth = trend_linewidth, trend_ptsize = trend_ptsize,
            legend.position = legend.position, legend.direction = legend.direction,
            title = title, subtitle = subtitle, xlab = xlab, ylab = ylab, keep_empty = keep_empty,
            expand = expand, width = width, ...
        )
    }

    facet_plot(p, facet_by, facet_scales, facet_nrow, facet_ncol, facet_byrow)
}

#' Bar Plot
#'
#' @description Create a bar plot.
#' @inheritParams common_args
#' @inheritParams BarPlotAtomic
#' @return A ggplot object or wrap_plots object or a list of ggplot objects
#' @export
#' @importFrom ggplot2 waiver
#' @examples
#' data <- data.frame(
#'    x = c("A", "B", "C", "D", "E", "F", "G", "H"),
#'    y = c(10, 8, 16, 4, 6, 12, 14, 2),
#'    group = c("G1", "G1", "G2", "G2", "G3", "G3", "G4", "G4"),
#'    facet = c("F1", "F2", "F3", "F4", "F1", "F2", "F3", "F4")
#' )
#'
#' BarPlot(data, x = "x", y = "y")
#' BarPlot(data, x = "x", y = "y", fill_by_x_if_no_group = F)
#' BarPlot(data, x = "group", y = "y", group_by = "x")
#' BarPlot(data, x = "group", y = "y", group_by = "x",
#'         position = "dodge", add_bg = TRUE)
#' BarPlot(data, x = "x", y = "y", split_by = "group",
#'         facet_by = "facet", position = "dodge", facet_ncol = 1)
#' BarPlot(data, x = "group", y = "y", group_by = "x",
#'         position = "dodge", add_bg = TRUE, bg_palette = "Spectral")
#' # use the count
#' BarPlot(data, x = "group", ylab = "count")
#' # flip the plot
#' BarPlot(data, x = "group", flip = TRUE, ylab = "count")
BarPlot <- function(
    data, x, y = NULL, flip = FALSE, fill_by_x_if_no_group = TRUE, line_name = NULL,
    group_by = NULL, group_by_sep = "_", split_by = NULL, split_by_sep = "_",
    facet_by = NULL, facet_scales = "fixed", facet_ncol = NULL, facet_nrow = NULL, facet_byrow = TRUE,
    add_bg = FALSE, bg_palette = "stripe", bg_palcolor = NULL, bg_alpha = 0.2,
    add_line = NULL, line_color = "red2", line_size = .6, line_type = 2,
    add_trend = FALSE, trend_color = "black", trend_linewidth = 1, trend_ptsize = 2,
    theme = "theme_this", theme_args = list(), palette = "Paired", palcolor = NULL,
    alpha = 1, x_text_angle = 0, aspect.ratio = 1, y_min = NULL, y_max = NULL,
    position = "auto", position_dodge_preserve = "total",
    legend.position = "right", legend.direction = "vertical",
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL, keep_empty = FALSE,
    expand = waiver(), width = waiver(), combine = TRUE, nrow = NULL, ncol = NULL, byrow = TRUE, seed = 8525, ...
) {
    validate_common_args(seed, facet_by = facet_by)

    split_by <- check_columns(data, split_by, force_factor = TRUE, allow_multi = TRUE, concat_multi = TRUE, concat_sep = split_by_sep)

    if (!is.null(split_by)) {
        datas <- split(data, data[[split_by]])
        # keep the order of levels
        datas <- datas[levels(data[[split_by]])]
    } else {
        datas <- list(data)
    }

    plots <- lapply(
        datas, BarPlotAtomic,
        x = x, y = y, flip = flip, group_by = group_by, fill_by_x_if_no_group = fill_by_x_if_no_group,
        theme = theme, theme_args = theme_args, palette = palette, palcolor = palcolor,
        add_bg = add_bg, bg_palette = bg_palette, bg_palcolor = bg_palcolor, bg_alpha = bg_alpha,
        alpha = alpha, x_text_angle = x_text_angle, aspect.ratio = aspect.ratio, line_name = line_name,
        add_line = add_line, line_color = line_color, line_size = line_size, line_type = line_type,
        add_trend = add_trend, trend_color = trend_color, trend_linewidth = trend_linewidth, trend_ptsize = trend_ptsize,
        position = position, position_dodge_preserve = position_dodge_preserve, y_min = y_min, y_max = y_max,
        legend.position = legend.position, legend.direction = legend.direction,
        title = title, subtitle = subtitle, xlab = xlab, ylab = ylab, keep_empty = keep_empty,
        expand = expand, width = width, facet_by = facet_by, facet_scales = facet_scales,
        facet_ncol = facet_ncol, facet_nrow = facet_nrow, facet_byrow = facet_byrow,
        split_by = split_by, nrow = nrow, ncol = ncol, byrow = byrow, ...
    )

    combine_plots(plots, combine = combine, nrow = nrow, ncol = ncol, byrow = byrow)
}
