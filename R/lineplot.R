#' LinePlotSingle
#' @description Line plot without groups.
#'
#' @inheritParams common_args
#' @param fill_point_by_x A logical value indicating whether to color the points by the x-axis values.
#'   If FALSE, the lines will be colored a single color (the first color in the palette).
#' @param color_line_by_x A logical value indicating whether to color the lines by the x-axis values.
#'  If FALSE, the lines will be colored a single color (the first color in the palette).
#' @param line_type The type of line to draw.
#' @param line_width The width of the line.
#' @param line_alpha The alpha value of the line.
#' @param pt_alpha The alpha value of the points.
#' @param pt_size The size of the points.
#' @param add_bg A logical value indicating whether to add a background to the plot.
#' @param bg_palette The palette to use for the background.
#' @param bg_palcolor The color to use for the background.
#' @param bg_alpha The alpha value of the background.
#' @param add_errorbars A logical value indicating whether to add error bars to the plot.
#' @param errorbar_color The color to use for the error bars.
#'   If "line", the error bars will be colored the same as the lines.
#' @param errorbar_alpha The alpha value of the error bars.
#' @param errorbar_linewidth The line width of the error bars.
#' @param errorbar_width The width of the error bars.
#' @param errorbar_min The column in the data frame containing the lower bound of the error bars.
#' @param errorbar_max The column in the data frame containing the upper bound of the error bars.
#' @param errorbar_sd The column in the data frame containing the standard deviation of the error bars.
#'   If errorbar_min and errorbar_max are not provided, this column will be used to calculate the error bars.
#'   errorbar_min = y - errorbar_sd, errorbar_max = y + errorbar_sd.
#'   If errorbar_min and errorbar_max are provided, this column will be ignored.
#'
#' @keywords internal
#' @importFrom rlang sym
#' @importFrom ggplot2 geom_line scale_color_manual labs geom_rect geom_errorbar geom_point
LinePlotSingle <- function(
    data, x, y = NULL, fill_point_by_x = TRUE, color_line_by_x = TRUE, facet_by = NULL,
    add_bg = FALSE, bg_palette = "stripe", bg_palcolor = NULL, bg_alpha = 0.2,
    add_errorbars = FALSE, errorbar_width = 0.1, errorbar_alpha = 1,
    errorbar_color = "grey30", errorbar_linewidth = .75, errorbar_min = NULL, errorbar_max = NULL, errorbar_sd = NULL,
    pt_alpha = 1, pt_size = 5,
    line_type = "solid", line_width = 1, line_alpha = .8,
    theme = "theme_this", theme_args = list(), palette = "Paired", palcolor = NULL,
    x_text_angle = 0, aspect.ratio = 1,
    legend.position = "right", legend.direction = "vertical",
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL, keep_empty = FALSE, ...
) {
    ggplot <- if (getOption("plotthis.gglogger.enabled", FALSE)) {
        gglogger::ggplot
    } else {
        ggplot2::ggplot
    }
    x <- check_columns(data, x, force_factor = TRUE)
    y <- check_columns(data, y)
    facet_by <- check_columns(data, facet_by, force_factor = TRUE, allow_multi = TRUE)
    if (is.null(y)) {
        data <- data %>% group_by(!!!syms(unique(x, facet_by))) %>% summarise(.y = n(), .groups = "drop")
        y <- ".y"
    }

    if (isTRUE(add_errorbars)) {
        if (is.null(errorbar_sd) && (is.null(errorbar_min) || is.null(errorbar_max))) {
            stop("If 'errorbar_min' and 'errorbar_max' are not provided, 'errorbar_sd' must be provided.")
        }
        if (is.null(errorbar_min) || is.null(errorbar_max)) {
            data$errorbar_min <- data[[y]] - data[[errorbar_sd]]
            data$errorbar_max <- data[[y]] + data[[errorbar_sd]]
            errorbar_min <- "errorbar_min"
            errorbar_max <- "errorbar_max"
        }
    }

    if (keep_empty) {
        # fill y with 0 for empty x. 'drop' with scale_fill_* doesn't have color for empty x
        fill_list <- list(0)
        names(fill_list) <- y
        if (is.null(facet_by)) {
            data <- data %>% complete(!!sym(x), fill = fill_list)
        } else {
            data <- data %>% group_by(!!!syms(facet_by)) %>% complete(!!sym(x), fill = fill_list)
        }
    }

    p <- ggplot(data, aes(x = !!sym(x), y = !!sym(y)))
    if (isTRUE(add_bg)) {
        p <- p + bg_layer(data, x, bg_palette, bg_palcolor, bg_alpha, keep_empty, facet_by)
    }
    colors <- palette_this(levels(data[[x]]), palette = palette, palcolor = palcolor)
    if (isTRUE(color_line_by_x)) {
        p <- p + geom_line(
            aes(color = !!sym(x), group = 1),
            alpha = line_alpha, linetype = line_type, linewidth = line_width) +
            scale_color_manual(name = x, values = colors, guide = "legend", drop = !keep_empty)
    } else {
        p <- p + geom_line(
            aes(group = 1), color = colors[[1]],
            alpha = line_alpha, linetype = line_type, linewidth = line_width)
    }
    if (isTRUE(add_errorbars)) {
        if (errorbar_color == "line" && isTRUE(color_line_by_x)) {
            p <- p + geom_errorbar(
                aes(ymin = !!sym(errorbar_min), ymax = !!sym(errorbar_max), color = !!sym(x)),
                alpha = errorbar_alpha, width = errorbar_width, linewidth = errorbar_linewidth)
        } else if (errorbar_color == "line") {
            p <- p + geom_errorbar(
                aes(ymin = !!sym(errorbar_min), ymax = !!sym(errorbar_max)), color = colors[[1]],
                alpha = errorbar_alpha, width = errorbar_width, linewidth = errorbar_linewidth)
        } else {
            p <- p + geom_errorbar(
                aes(ymin = !!sym(errorbar_min), ymax = !!sym(errorbar_max)), color = errorbar_color,
                alpha = errorbar_alpha, width = errorbar_width, linewidth = errorbar_linewidth)
        }
    }
    if (isTRUE(fill_point_by_x)) {
        p <- p + geom_point(
            aes(fill = !!sym(x)),
            color = "grey20", alpha = pt_alpha, size = pt_size, shape = 21) +
            scale_fill_manual(name = x, values = colors, guide = "legend", drop = !keep_empty)
    } else {
        p <- p + geom_point(
            fill = colors[[1]],
            color = "grey20", alpha = pt_alpha, size = pt_size, shape = 21)
    }
    just <- calc_just(x_text_angle)
    p <- p + scale_x_discrete(drop = !keep_empty) +
        labs(title = title, subtitle = subtitle, x = xlab %||% x, y = ylab %||% y) +
        do.call(theme, theme_args) +
        ggplot2::theme(
            aspect.ratio = aspect.ratio,
            legend.position = legend.position,
            legend.direction = legend.direction,
            panel.grid.major = element_line(colour = "grey80", linetype = 2),
            axis.text.x = element_text(angle = x_text_angle, hjust = just$h, vjust = just$v)
        )

    height <- 4.5
    width <- .5 + nlevels(data[[x]]) * .8

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
    p
}

#' LinePlotGrouped
#'
#' @description Line plot with groups.
#' @inheritParams common_args
#' @inheritParams LinePlotSingle
#' @param group_by A character string specifying the column name of the data frame to group the plot.
#' @param group_by_sep A character string specifying the separator to use when concatenating multiple columns.
#' @return A ggplot object
#' @keywords internal
#' @importFrom rlang syms
#' @importFrom dplyr summarise %>% mutate n
#' @importFrom ggplot2 geom_line scale_color_manual labs geom_errorbar geom_point
LinePlotGrouped <- function(
    data, x, y = NULL, group_by, group_by_sep = "_", facet_by = NULL,
    add_bg = FALSE, bg_palette = "stripe", bg_palcolor = NULL, bg_alpha = 0.2,
    add_errorbars = FALSE, errorbar_width = 0.1, errorbar_alpha = 1,
    errorbar_color = "grey30", errorbar_linewidth = .75, errorbar_min = NULL, errorbar_max = NULL, errorbar_sd = NULL,
    pt_alpha = 1, pt_size = 5,
    line_type = "solid", line_width = 1, line_alpha = .8,
    theme = "theme_this", theme_args = list(), palette = "Paired", palcolor = NULL,
    x_text_angle = 0, aspect.ratio = 1,
    legend.position = "right", legend.direction = "vertical",
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL, keep_empty = FALSE, ...
) {
    ggplot <- if (getOption("plotthis.gglogger.enabled", FALSE)) {
        gglogger::ggplot
    } else {
        ggplot2::ggplot
    }
    x <- check_columns(data, x, force_factor = TRUE)
    y <- check_columns(data, y)
    group_by <- check_columns(
        data, group_by, force_factor = TRUE,
        allow_multi = TRUE, concat_multi = TRUE, concat_sep = group_by_sep
    )
    if (is.null(y)) {
        data <- data %>% dplyr::group_by(!!!syms(unique(c(x, group_by, facet_by)))) %>% summarise(.y = n(), .groups = "drop")
        y <- ".y"
    }

    if (isTRUE(add_errorbars)) {
        if (is.null(errorbar_sd) && (is.null(errorbar_min) || is.null(errorbar_max))) {
            stop("If 'errorbar_min' and 'errorbar_max' are not provided, 'errorbar_sd' must be provided.")
        }
        if (is.null(errorbar_min) || is.null(errorbar_max)) {
            data$errorbar_min <- data[[y]] - data[[errorbar_sd]]
            data$errorbar_max <- data[[y]] + data[[errorbar_sd]]
            errorbar_min <- "errorbar_min"
            errorbar_max <- "errorbar_max"
        }
    }

    if (keep_empty) {
        # fill y with 0 for empty group_by. 'drop' with scale_fill_* doesn't have color for empty group_by
        fill_list <- list(0)
        names(fill_list) <- y
        data <- data %>%
            dplyr::group_by(!!!syms(unique(c(x, facet_by)))) %>%
            complete(!!sym(group_by), fill = fill_list)
    }

    p <- ggplot(data, aes(x = !!sym(x), y = !!sym(y)))
    if (isTRUE(add_bg)) {
        p <- p + bg_layer(data, x, bg_palette, bg_palcolor, bg_alpha, keep_empty, facet_by)
    }
    colors <- palette_this(levels(data[[group_by]]), palette = palette, palcolor = palcolor)
    p <- p + geom_line(
        aes(color = !!sym(group_by), group = !!sym(group_by)),
        alpha = line_alpha, linetype = line_type, linewidth = line_width) +
        scale_color_manual(name = group_by, values = colors, guide = "legend", drop = !keep_empty)
    if (isTRUE(add_errorbars)) {
        if (errorbar_color == "line") {
            p <- p + geom_errorbar(
                aes(ymin = !!sym(errorbar_min), ymax = !!sym(errorbar_max), color = !!sym(group_by)),
                alpha = errorbar_alpha, width = errorbar_width, linewidth = errorbar_linewidth)
        } else {
            p <- p + geom_errorbar(
                aes(ymin = !!sym(errorbar_min), ymax = !!sym(errorbar_max)), color = errorbar_color,
                alpha = errorbar_alpha, width = errorbar_width, linewidth = errorbar_linewidth)
        }
    }
    p <- p + geom_point(
        aes(fill = !!sym(group_by)),
        color = "grey20", alpha = pt_alpha, size = pt_size, shape = 21) +
        scale_fill_manual(name = group_by, values = colors, guide = "legend", drop = !keep_empty)

    just <- calc_just(x_text_angle)
    p <- p + scale_x_discrete(drop = !keep_empty) +
        labs(title = title, subtitle = subtitle, x = xlab %||% x, y = ylab %||% y) +
        do.call(theme, theme_args) +
        ggplot2::theme(
            aspect.ratio = aspect.ratio,
            legend.position = legend.position,
            legend.direction = legend.direction,
            panel.grid.major = element_line(colour = "grey80", linetype = 2),
            axis.text.x = element_text(angle = x_text_angle, hjust = just$h, vjust = just$v)
        )

    height <- 4.5
    width <- .5 + nlevels(data[[x]]) * .8

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

    p
}

#' LinePlotAtomic
#'
#' @description Line plot with atomic data.
#' @inheritParams common_args
#' @inheritParams LinePlotGrouped
#' @param fill_point_by_x_if_no_group A logical value indicating whether to color the points by the x-axis values
#'  when there is no group_by column.
#' @param color_line_by_x_if_no_group A logical value indicating whether to color the lines by the x-axis values
#' @param facet_args A list of arguments to pass to [ggplot2::facet_wrap()] or [ggplot2::facet_grid()].
#' when there is no group_by column.
#' @return A ggplot object
#' @keywords internal
LinePlotAtomic <- function(
    data, x, y = NULL, group_by = NULL,
    fill_point_by_x_if_no_group = TRUE, color_line_by_x_if_no_group = TRUE,
    add_bg = FALSE, bg_palette = "stripe", bg_palcolor = NULL, bg_alpha = 0.2,
    add_errorbars = FALSE, errorbar_width = 0.1, errorbar_alpha = 1,
    errorbar_color = "grey30", errorbar_linewidth = .75, errorbar_min = NULL, errorbar_max = NULL, errorbar_sd = NULL,
    pt_alpha = 1, pt_size = 5,
    line_type = "solid", line_width = 1, line_alpha = .8,
    theme = "theme_this", theme_args = list(), palette = "Paired", palcolor = NULL,
    x_text_angle = 0, aspect.ratio = 1,
    legend.position = "right", legend.direction = "vertical",
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL, keep_empty = FALSE,
    facet_by = NULL, facet_scales = "fixed", facet_args = list(),
    facet_nrow = NULL, facet_ncol = NULL, facet_byrow = TRUE, ...
) {
    if (is.null(group_by)) {
        p <- LinePlotSingle(
            data = data, x = x, y = y, fill_point_by_x = fill_point_by_x_if_no_group,
            color_line_by_x = color_line_by_x_if_no_group, facet_by = facet_by,
            add_bg = add_bg, bg_palette = bg_palette, bg_palcolor = bg_palcolor, bg_alpha = bg_alpha,
            add_errorbars = add_errorbars, errorbar_width = errorbar_width, errorbar_alpha = errorbar_alpha,
            errorbar_color = errorbar_color, errorbar_linewidth = errorbar_linewidth,
            errorbar_min = errorbar_min, errorbar_max = errorbar_max, errorbar_sd = errorbar_sd,
            pt_alpha = pt_alpha, pt_size = pt_size,
            line_type = line_type, line_width = line_width, line_alpha = line_alpha,
            theme = theme, theme_args = theme_args, palette = palette, palcolor = palcolor,
            x_text_angle = x_text_angle, aspect.ratio = aspect.ratio,
            legend.position = legend.position, legend.direction = legend.direction,
            title = title, subtitle = subtitle, xlab = xlab, ylab = ylab, keep_empty = keep_empty, ...
        )
    } else {
        p <- LinePlotGrouped(
            data = data, x = x, y = y, group_by = group_by, facet_by = facet_by,
            add_bg = add_bg, bg_palette = bg_palette, bg_palcolor = bg_palcolor, bg_alpha = bg_alpha,
            add_errorbars = add_errorbars, errorbar_width = errorbar_width, errorbar_alpha = errorbar_alpha,
            errorbar_color = errorbar_color, errorbar_linewidth = errorbar_linewidth,
            errorbar_min = errorbar_min, errorbar_max = errorbar_max, errorbar_sd = errorbar_sd,
            pt_alpha = pt_alpha, pt_size = pt_size,
            line_type = line_type, line_width = line_width, line_alpha = line_alpha,
            theme = theme, theme_args = theme_args, palette = palette, palcolor = palcolor,
            x_text_angle = x_text_angle, aspect.ratio = aspect.ratio,
            legend.position = legend.position, legend.direction = legend.direction,
            title = title, subtitle = subtitle, xlab = xlab, ylab = ylab, keep_empty = keep_empty, ...
        )
    }
    facet_args$plot <- p
    facet_args["facet_by"] <- list(facet_by)
    facet_args["facet_scales"] <- list(facet_scales)
    facet_args["nrow"] <- list(facet_nrow)
    facet_args["ncol"] <- list(facet_ncol)
    facet_args["byrow"] <- list(facet_byrow)
    facet_args["legend.position"] <- list(legend.position)
    facet_args["legend.direction"] <- list(legend.direction)
    do.call(facet_plot, facet_args)
}

#' Line Plot
#'
#' @description Visualizing the change of a numeric value over the progression of a categorical variable.
#' @inheritParams common_args
#' @inheritParams LinePlotAtomic
#' @param group_by_sep A character string specifying the separator to use when concatenating multiple columns.
#' @return A ggplot object or wrap_plots object or a list of ggplot objects
#' @export
#' @examples
#' data <- data.frame(
#'    x = factor(c("A", "B", "C", "D", "A", "B", "C", "D"), levels = LETTERS[1:6]),
#'    y = c(10, 8, 16, 4, 6, 12, 14, 2),
#'    group = c("G1", "G1", "G1", "G1", "G2", "G2", "G2", "G2"),
#'    facet = c("F1", "F1", "F2", "F2", "F3", "F3", "F4", "F4")
#' )
#'
#' LinePlot(data, x = "x", y = "y")
#' LinePlot(data, x = "x", y = "y", group_by = "group")
#' LinePlot(data, x = "x", y = "y", group_by = "group", add_bg = TRUE)
#' LinePlot(data, x = "x", y = "y", group_by = "group", facet_by = "facet")
#' LinePlot(data, x = "x", y = "y", group_by = "group", split_by = "facet")
LinePlot <- function(
    data, x, y = NULL, group_by = NULL, group_by_sep = "_", split_by = NULL, split_by_sep = "_",
    fill_point_by_x_if_no_group = TRUE, color_line_by_x_if_no_group = TRUE,
    add_bg = FALSE, bg_palette = "stripe", bg_palcolor = NULL, bg_alpha = 0.2,
    add_errorbars = FALSE, errorbar_width = 0.1, errorbar_alpha = 1,
    errorbar_color = "grey30", errorbar_linewidth = .75, errorbar_min = NULL, errorbar_max = NULL, errorbar_sd = NULL,
    pt_alpha = 1, pt_size = 5,
    line_type = "solid", line_width = 1, line_alpha = .8,
    theme = "theme_this", theme_args = list(), palette = "Paired", palcolor = NULL,
    x_text_angle = 0, aspect.ratio = 1,
    legend.position = "right", legend.direction = "vertical",
    facet_by = NULL, facet_scales = "fixed",
    combine = TRUE, nrow = NULL, ncol = NULL, byrow = TRUE,
    facet_nrow = NULL, facet_ncol = NULL, facet_byrow = TRUE, facet_args = list(),
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL, keep_empty = FALSE, seed = 8525, ...
) {
    validate_common_args(seed, facet_by = facet_by)
    theme <- process_theme(theme)

    x <- check_columns(data, x, force_factor = TRUE)
    y <- check_columns(data, y)
    group_by <- check_columns(data, group_by, force_factor = TRUE, allow_multi = TRUE, concat_multi = TRUE, concat_sep = group_by_sep)
    split_by <- check_columns(data, split_by, force_factor = TRUE, allow_multi = TRUE, concat_multi = TRUE, concat_sep = split_by_sep)

    if (!is.null(split_by)) {
        datas <- split(data, data[[split_by]])
        # keep the order of levels
        datas <- datas[levels(data[[split_by]])]
        if (length(palette) > 1) {
            if (length(palette)!=length(datas)) {stop("split_by and palette length mismatches.")}
            if (is.null(names(palette))) {stop("palette should be named vector if multiple palettes are provided.")}
        } else {
            palette <- rep(palette, length(datas))
            names(palette) <- names(datas)
        }
        if (length(palcolor) > 1) {
            if (length(palcolor)!=length(datas)) {stop("split_by and palcolor length mismatches.")}
            if (is.null(names(palcolor))) {stop("palcolor should be named vector if multiple colors are provided.")}
        } else {
            if (!is.null(palcolor)) {
                palcolor <- rep(palcolor, length(datas))
                names(palcolor) <- names(datas)                
            }
        }
    } else {
        datas <- list(data)
        names(datas) <- "..."
        names(palette) <- "..."
        if (!is.null(palcolor)) {
            names(palcolor) <- "..."
        }
    }

    plots <- lapply(
        names(datas), function(nm) {
            default_title <- if (length(datas) == 1 && identical(nm, "...")) NULL else nm
            if (is.function(title)) {
                title <- title(default_title)
            } else {
                title <- title %||% default_title
            }
            LinePlotAtomic(datas[[nm]],
                x = x, y = y, group_by = group_by,
                fill_point_by_x_if_no_group = fill_point_by_x_if_no_group,
                color_line_by_x_if_no_group = color_line_by_x_if_no_group,
                add_bg = add_bg, bg_palette = bg_palette, bg_palcolor = bg_palcolor, bg_alpha = bg_alpha,
                add_errorbars = add_errorbars, errorbar_width = errorbar_width, errorbar_alpha = errorbar_alpha,
                errorbar_color = errorbar_color, errorbar_linewidth = errorbar_linewidth,
                errorbar_min = errorbar_min, errorbar_max = errorbar_max, errorbar_sd = errorbar_sd,
                pt_alpha = pt_alpha, pt_size = pt_size,
                line_type = line_type, line_width = line_width, line_alpha = line_alpha,
                theme = theme, theme_args = theme_args, palette = palette[nm], palcolor = palcolor[nm],
                x_text_angle = x_text_angle, aspect.ratio = aspect.ratio,
                legend.position = legend.position, legend.direction = legend.direction, facet_args = facet_args,
                facet_by = facet_by, facet_scales = facet_scales, facet_nrow = facet_nrow, facet_ncol = facet_ncol, facet_byrow = facet_byrow,
                title = title, subtitle = subtitle, xlab = xlab, ylab = ylab, keep_empty = keep_empty, ...
            )
        }
    )

    combine_plots(plots, combine = combine, nrow = nrow, ncol = ncol, byrow = byrow)
}
