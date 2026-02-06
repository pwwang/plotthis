#' BarPlotSingle
#'
#' @description Create a bar plot without groups.
#'
#' @inheritParams common_args
#' @param x A character vector specifying the column as the x axis of the plot.
#'  A character/factor column is expected.
#' @param x_sep A character string to concatenate the columns in `x`, if multiple columns are provided.
#' @param y A character vector specifying the column as the y axis of the plot.
#'   Default is NULL, meaning the y axis is the count of the data.
#' @param fill_by_x A logical value indicating whether to fill the bars by the x-axis values.
#'   If FALSE, the bars will be filled a single color (the first color in the palette).
#' @param width A numeric value specifying the width of the bars.
#' @param flip A logical value indicating whether to flip the x and y axes.
#' @param label A column name for the values to be displayed on the top of the bars.
#'  If TRUE, the y values will be displayed.
#' @param label_nudge A numeric value to nudge the labels (the distance between the label and the top of the bar).
#' @param label_fg A character string indicating the color of the label.
#' @param label_size A numeric value indicating the size of the label.
#' @param label_bg A character string indicating the background color of the label.
#' @param label_bg_r A numeric value indicating the radius of the background.
#' @param add_line A numeric value indicating the y value to add a horizontal line.
#' @param line_color A character string indicating the color of the line.
#' @param line_width A numeric value indicating the size of the line.
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
#' @importFrom tidyr complete
#' @importFrom ggplot2 aes geom_bar scale_fill_manual labs scale_x_discrete scale_y_continuous
#' @importFrom ggplot2 element_line waiver coord_flip scale_color_manual guide_legend coord_cartesian
#' @importFrom ggrepel geom_text_repel
BarPlotSingle <- function(
    data, x, x_sep = "_", y = NULL, flip = FALSE, facet_by = NULL, facet_scales = "fixed", label = NULL, label_nudge = 0.02,
    label_fg = "black", label_size = 4, label_bg = "white", label_bg_r = 0.1,
    add_bg = FALSE, bg_palette = "stripe", bg_palcolor = NULL, bg_alpha = 0.2,
    theme = "theme_this", theme_args = list(), palette = "Paired", palcolor = NULL,
    alpha = 1, x_text_angle = 0, aspect.ratio = 1, y_min = NULL, y_max = NULL,
    legend.position = "right", legend.direction = "vertical",
    add_line = NULL, line_color = "red2", line_width = .6, line_type = 2, line_name = NULL,
    add_trend = FALSE, trend_color = "black", trend_linewidth = 1, trend_ptsize = 2.5,
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL, keep_na = FALSE, keep_empty = FALSE,
    expand = waiver(), fill_by_x = TRUE, width = 0.9, ...) {
    ggplot <- if (getOption("plotthis.gglogger.enabled", FALSE)) {
        gglogger::ggplot
    } else {
        ggplot2::ggplot
    }
    if (inherits(width, "waiver")) width <- 0.9

    x <- check_columns(data, x, force_factor = TRUE, allow_multi = TRUE, concat_multi = TRUE, concat_sep = x_sep)
    y <- check_columns(data, y)
    facet_by <- check_columns(data, facet_by, force_factor = TRUE, allow_multi = TRUE)
    if (is.null(y)) {
        orig_data <- data
        data <- data %>%
            group_by(!!!syms(unique(c(x, facet_by)))) %>%
            summarise(.y = n(), .groups = "drop")
        # keep the factor levels
        for (col in unique(c(x, facet_by))) {
            data[[col]] <- factor(data[[col]], levels = levels(orig_data[[col]]))
        }
        rm(orig_data)
        y <- ".y"
    }
    if (isTRUE(label)) label <- y

    label <- check_columns(data, label)
    if (inherits(expand, "waiver")) {
        expand <- c(0, 0, 0, 0)
        if (!is.null(label)) {
            if (any(data[[y]] > 0)) {
                # what is the best ratio for the label nudge?
                # based on maximum y value?
                expand[1] <- 0.05 + label_nudge * 0.5
            }
            if (any(data[[y]] < 0)) {
                expand[3] <- 0.05 + label_nudge * 0.5
            }
        }
    }
    expand <- norm_expansion(expand, x_type = "discrete", y_type = "continuous")

    data <- process_keep_na_empty(data, keep_na, keep_empty)

    keep_empty_x <- keep_empty[[x]]

    if (isTRUE(fill_by_x)) {
        x_vals <- levels(data[[x]])
        if (anyNA(data[[x]])) x_vals <- c(x_vals, NA)
        colors <- palette_this(x_vals, palette = palette, palcolor = palcolor)
        guide <- guide_legend(order = 1)
        p <- ggplot(data, aes(x = !!sym(x), y = !!sym(y), fill = !!sym(x)))
    } else {
        colors <- palette_this("fill", palette = palette, palcolor = palcolor)
        guide <- "none"
        p <- ggplot(data, aes(x = !!sym(x), y = !!sym(y), fill = "fill"))
    }
    if (isTRUE(add_bg)) {
        p <- p + bg_layer(data, x, isTRUE(keep_empty_x), bg_palette, bg_palcolor, bg_alpha, facet_by)
    }

    just <- calc_just(x_text_angle)

    p <- p + geom_col(alpha = alpha, width = width, show.legend = TRUE)

    if (!is.null(label)) {
        # nudge doesn't work as aes, let's adjust the y
        label_data <- data
        label_data <- label_data[order(label_data[[x]]), , drop = FALSE]
        label_data$.label <- label_data[[label]]
        yr <- diff(range(data[[y]], na.rm = TRUE))
        label_data$.sign <- label_data[[y]] > 0
        label_data[[y]] <- label_data[[y]] + yr * label_nudge * ifelse(label_data$.sign, 1, -1)

        p <- p + geom_text_repel(
            data = label_data, mapping = aes(label = !!sym(".label")),
            color = label_fg, size = label_size, hjust = if (flip) ifelse(label_data$.sign, 0, 1) else 0.5,
            bg.color = label_bg, bg.r = label_bg_r, direction = "y", force = 0,
            min.segment.length = 0, max.overlaps = 100, segment.color = 'transparent'
        )
    }
    p <- p +
        # scale_fill_manual(name = x, values = colors, guide = guide) +
        labs(title = title, subtitle = subtitle, x = xlab %||% x, y = ylab %||% y) +
        scale_x_discrete(expand = expand$x, drop = !isTRUE(keep_empty_x)) +
        scale_y_continuous(expand = expand$y) +
        do.call(theme, theme_args) +
        ggplot2::theme(
            aspect.ratio = aspect.ratio,
            legend.position = legend.position,
            legend.direction = legend.direction,
            panel.grid.major = element_line(colour = "grey80", linetype = 2),
            axis.text.x = element_text(angle = x_text_angle, hjust = just$h, vjust = just$v)
        )

    if (isTRUE(keep_empty_x)) {
        p <- p +
            scale_fill_manual(
                values = colors,
                guide = guide,
                breaks = x_vals, limits = x_vals, drop = FALSE
            )
    } else {
        p <- p +
            scale_fill_manual(
                values = colors,
                guide = guide
            )
    }

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
            aes(color = line_name %||% paste0(y, " = ", add_line), yintercept = add_line),
            linetype = line_type, linewidth = line_width
        ) + scale_color_manual(name = NULL, values = line_color, guide = guide_legend(order = 2))
    }

    facet_free <- !is.null(facet_by) && (
        identical(facet_scales, "free") ||
            (!flip && identical(facet_scales, "free_y")) ||
            (flip && identical(facet_scales, "free_x"))
    )
    if (isTRUE(flip) && !facet_free) {
        p <- p + coord_flip(ylim = c(y_min, y_max))
    } else if (isTRUE(flip)) {
        p <- p + coord_flip()
    } else if (!facet_free) {
        p <- p + coord_cartesian(ylim = c(y_min, y_max))
    }

    height <- 3.5 + max(nchar(unlist(strsplit(levels(data[[x]]), "\n"))) * 0.1, 1)
    width <- .5 + min(nlevels(data[[x]]) * .8, height / aspect.ratio)
    if (!identical(legend.position, "none")) {
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
#' @inheritParams BarPlotSingle
#' @param scale_y A logical value indicating whether to scale the total y values in each group to 100%.
#' Only works when group_by is specified.
#' @param position A character string indicating the position of the bars.
#'  If "auto", the position will be "stack" if group_by has more than 5 levels, otherwise "dodge".
#'  "fill" is also a valid option. Only works when group_by is not NULL.
#' @param position_dodge_preserve Should dodging preserve the "total" width of all elements at a position, or the width of a "single" element?
#' @param add_bg A logical value indicating whether to add a background to the plot.
#' @param bg_palette A character string indicating the palette to use for the background.
#' @param bg_palcolor A character string indicating the color to use for the background.
#' @param bg_alpha A numeric value indicating the alpha of the background.
#' @param group_by A character vector specifying the column as the group_by of the plot.
#'  A character/factor column is expected.
#' @param group_by_sep A character string to concatenate the columns in `group_by`, if multiple columns are provided.
#' @param group_name A character string to specify the name of the group_by in the legend.
#' @return A ggplot object.
#' @keywords internal
#' @importFrom rlang sym syms %||%
#' @importFrom dplyr %>% summarise n mutate ungroup case_when last first
#' @importFrom ggplot2 aes geom_bar scale_fill_manual labs position_dodge2 coord_flip guide_legend scale_color_manual
BarPlotGrouped <- function(
    data, x, x_sep = "_", y = NULL, scale_y = FALSE, flip = FALSE, group_by, group_by_sep = "_", group_name = NULL,
    theme = "theme_this", theme_args = list(), palette = "Paired", palcolor = NULL,
    label = NULL, label_nudge = 0.02, label_fg = "black", label_size = 4, label_bg = "white", label_bg_r = 0.1,
    add_bg = FALSE, bg_palette = "stripe", bg_palcolor = NULL, bg_alpha = 0.2,
    alpha = 1, x_text_angle = 0, aspect.ratio = 1,
    add_line = NULL, line_color = "red2", line_width = .6, line_type = 2, line_name = NULL,
    add_trend = FALSE, trend_color = "black", trend_linewidth = 1, trend_ptsize = 2.5,
    position = "auto", position_dodge_preserve = "total", y_min = NULL, y_max = NULL,
    legend.position = "right", legend.direction = "vertical",
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL, keep_na = FALSE, keep_empty = FALSE,
    expand = waiver(), width = 0.8, facet_by = NULL, facet_scales = "fixed", ...) {

    ggplot <- if (getOption("plotthis.gglogger.enabled", FALSE)) {
        gglogger::ggplot
    } else {
        ggplot2::ggplot
    }
    group_by <- check_columns(data, group_by, force_factor = TRUE, allow_multi = TRUE, concat_multi = TRUE, concat_sep = group_by_sep)
    facet_by <- check_columns(data, facet_by, force_factor = TRUE, allow_multi = TRUE)
    x <- check_columns(data, x, force_factor = TRUE, allow_multi = TRUE, concat_multi = TRUE, concat_sep = x_sep)
    y <- check_columns(data, y)
    orig_data <- data
    if (is.null(y)) {
        data <- data %>%
            dplyr::group_by(!!!syms(unique(c(x, group_by, facet_by)))) %>%
            summarise(.y = n(), .groups = "drop")
        # keep the factor levels
        for (col in unique(c(x, group_by, facet_by))) {
            data[[col]] <- factor(data[[col]], levels = levels(orig_data[[col]]))
        }
        y <- ".y"
    }
    if (isTRUE(label)) {
        label <- y
    }
    label <- check_columns(data, label)
    if (isTRUE(scale_y)) {
        y_scaled <- paste0(y, "_scaled")
        data <- data %>%
            dplyr::group_by(!!!syms(unique(c(x, facet_by)))) %>%
            mutate(!!sym(y_scaled) := !!sym(y) / sum(!!sym(y))) %>%
            ungroup()

        for (col in unique(c, facet_by)) {
            data[[col]] <- factor(data[[col]], levels = levels(orig_data[[col]]))
        }
    }
    rm(orig_data)

    if (inherits(width, "waiver")) width <- 0.8

    data <- process_keep_na_empty(data, keep_na, keep_empty)

    keep_empty_x <- keep_empty[[x]]
    keep_empty_group <- keep_empty[[group_by]]
    group_vals <- levels(data[[group_by]])
    if (anyNA(data[[group_by]])) group_vals <- c(group_vals, NA)

    p <- ggplot(data, aes(x = !!sym(x), y = !!sym(ifelse(scale_y, y_scaled, y)), fill = !!sym(group_by)))
    if (isTRUE(add_bg)) {
        p <- p + bg_layer(data, x, isTRUE(keep_empty_x), bg_palette, bg_palcolor, bg_alpha, facet_by)
    }

    colors <- palette_this(group_vals, palette = palette, palcolor = palcolor)
    just <- calc_just(x_text_angle)
    if (position == "auto") {
        position <- if (length(colors) <= 5) {
            position_dodge2(preserve = position_dodge_preserve, width = width)
        } else {
            "stack"
        }
    } else if (position == "dodge") {
        position <- position_dodge2(preserve = position_dodge_preserve, width = width)
    }
    if (inherits(expand, "waiver")) {
        if (identical(position, "stack")) {
            expand <- c(top = 0, bottom = 0)
            if (!is.null(label)) {
                if (any(data[[y]] > 0)) {
                    expand['top'] <- 0.05 + label_nudge * 0.5
                }
                if (any(data[[y]] < 0)) {
                    expand['bottom'] <- 0.05 + label_nudge * 0.5
                }
            }
        } else if (min(data[[y]], na.rm = TRUE) >= 0) {
            expand <- c(bottom = 0)
        } else if (max(data[[y]], na.rm = TRUE) <= 0) {
            expand <- c(top = 0)
        } else {
            expand <- NULL
        }
    }
    expand <- norm_expansion(expand, x_type = "discrete", y_type = "continuous")

    p <- p + geom_col(alpha = alpha, position = position, width = width, show.legend = TRUE) +
        labs(title = title, subtitle = subtitle, x = xlab %||% x, y = ylab %||% y) +
        scale_x_discrete(expand = expand$x, drop = !isTRUE(keep_empty_x)) +
        scale_y_continuous(expand = expand$y) +
        do.call(theme, theme_args) +
        ggplot2::theme(
            aspect.ratio = aspect.ratio,
            legend.position = legend.position,
            legend.direction = legend.direction,
            panel.grid.major = element_line(colour = "grey80", linetype = 2),
            axis.text.x = element_text(angle = x_text_angle, hjust = just$h, vjust = just$v)
        )

    if (isTRUE(keep_empty_group)) {
        p <- p +
            scale_fill_manual(
                name = group_name %||% group_by,
                values = colors,
                guide = guide_legend(order = 1),
                breaks = group_vals, limits = group_vals, drop = FALSE
            )
    } else {
        p <- p +
            scale_fill_manual(
                name = group_name %||% group_by,
                values = colors,
                guide = guide_legend(order = 1)
            )
    }

    if (isTRUE(add_trend)) {
        if (is.null(trend_color)) {
            p <- p +
                geom_line(aes(group = !!sym(group_by), color = !!sym(group_by)),
                    position = position_dodge(width = 0.9), linewidth = trend_linewidth, show.legend = FALSE
                ) +
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
            linetype = line_type, linewidth = line_width
        ) + scale_color_manual(name = NULL, values = line_color, guide = guide_legend(order = 2))
    }

    if (!is.null(label)) {
        label_data <- data
        label_data <- label_data[order(label_data[[x]], label_data[[group_by]]), , drop = FALSE]
        label_data$.label <- label_data[[label]]
        .y <- ifelse(scale_y, y_scaled, y)
        label_data$.sign <- label_data[[.y]] > 0
        yr <- diff(range(data[[.y]], na.rm = TRUE))
        if (identical(position, "stack")) {
            # caculate the y-axis of the labels
            label_data <- mutate(
                label_data,
                !!sym(.y) := ifelse(
                    !!sym(".sign"),
                    cumsum(rev(!!sym(.y))) + yr * label_nudge,
                    cumsum(rev(!!sym(.y))) - yr * label_nudge
                ),
                .label = rev(!!sym(".label")),
                .by = unique(c(x, ".sign", facet_by))
            )

            p <- p + geom_text_repel(
                data = label_data, mapping = aes(label = !!sym(".label")), color = label_fg,
                size = label_size, hjust = if (flip) ifelse(label_data$.sign, 0, 1) else 0.5,
                bg.color = label_bg, bg.r = label_bg_r, direction = "y",
                force = 0, min.segment.length = 0, max.overlaps = 100, segment.color = 'transparent'
            )
        } else {
            label_data[[y]] <- label_data[[y]] + yr * label_nudge * ifelse(label_data$.sign, 1, -1)

            p <- p + geom_text_repel(
                data = label_data, mapping = aes(label = !!sym(".label")), color = label_fg,
                size = label_size, hjust = if (flip) ifelse(label_data$.sign, 0, 1) else 0.5,
                bg.color = label_bg, bg.r = label_bg_r, position = position, direction = "y",
                force = 0, min.segment.length = 0, max.overlaps = 100, segment.color = 'transparent'
            )
        }
    }

    facet_free <- !is.null(facet_by) && (
        identical(facet_scales, "free") ||
            (!flip && identical(facet_scales, "free_y")) ||
            (flip && identical(facet_scales, "free_x"))
    )
    if (isTRUE(flip) && facet_free) {
        p <- p + coord_flip()
    } else if (isTRUE(flip)) {
        p <- p + coord_flip(ylim = c(y_min, y_max))
    } else if (!facet_free) {
        p <- p + coord_cartesian(ylim = c(y_min, y_max))
    }

    height <- 4.5
    if (is.character(position) && position == "stack") {
        width <- max(min(.5 + nlevels(data[[x]]) * .8, 1.2 * height / aspect.ratio), 4.5)
    } else {
        width <- .5 + min(nlevels(data[[x]]) * length(unique(data[[group_by]])) * .5, 1.2 * height / aspect.ratio)
    }
    if (!identical(legend.position, "none")) {
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

#' Atomic bar plot
#'
#' @description Create a bar plot with or without groups. This function does not handle splitting but only facetting.
#' @inheritParams common_args
#' @inheritParams BarPlotSingle
#' @inheritParams BarPlotGrouped
#' @param fill_by_x_if_no_group A logical value indicating whether to fill the bars by the x-axis values if there is no group_by.
#' @param facet_args A list of arguments to pass to [ggplot2::facet_grid] or [ggplot2::facet_wrap].
#' @return A ggplot object.
#' @importFrom ggplot2 waiver
#' @keywords internal
BarPlotAtomic <- function(
    data, x, x_sep = "_", y = NULL, scale_y = FALSE, flip = FALSE, group_by = NULL, group_by_sep = "_", group_name = NULL,
    fill_by_x_if_no_group = TRUE, label_nudge = 0.02,
    label = NULL, label_fg = "black", label_size = 4, label_bg = "white", label_bg_r = 0.1,
    add_bg = FALSE, bg_palette = "stripe", bg_palcolor = NULL, bg_alpha = 0.2,
    theme = "theme_this", theme_args = list(), palette = "Paired", palcolor = NULL,
    alpha = 1, x_text_angle = 0, aspect.ratio = 1,
    add_line = NULL, line_color = "red2", line_width = .6, line_type = 2, line_name = NULL,
    add_trend = FALSE, trend_color = "black", trend_linewidth = 1, trend_ptsize = 2,
    position = "auto", position_dodge_preserve = "total", y_min = NULL, y_max = NULL,
    legend.position = "right", legend.direction = "vertical",
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL, keep_na = FALSE, keep_empty = FALSE,
    expand = waiver(), width = waiver(), facet_by = NULL, facet_scales = "fixed",
    facet_nrow = NULL, facet_ncol = NULL, facet_byrow = TRUE, facet_args = list(), ...) {
    if (is.null(group_by)) {
        p <- BarPlotSingle(
            data, x, x_sep, y,
            label = label, label_nudge = label_nudge,
            label_fg = label_fg, label_size = label_size, label_bg = label_bg, label_bg_r = label_bg_r,
            facet_by = facet_by, facet_scales = facet_scales, flip = flip, line_name = line_name,
            theme = theme, theme_args = theme_args, palette = palette, palcolor = palcolor,
            alpha = alpha, x_text_angle = x_text_angle, aspect.ratio = aspect.ratio, add_bg = add_bg,
            add_line = add_line, line_color = line_color, line_width = line_width, line_type = line_type,
            add_trend = add_trend, trend_color = trend_color, trend_linewidth = trend_linewidth, trend_ptsize = trend_ptsize,
            legend.position = legend.position, legend.direction = legend.direction, y_min = y_min, y_max = y_max,
            title = title, subtitle = subtitle, xlab = xlab, ylab = ylab, keep_na = keep_na, keep_empty = keep_empty,
            expand = expand, fill_by_x = fill_by_x_if_no_group, width = width, ...
        )
    } else {
        p <- BarPlotGrouped(
            data, x, x_sep, y,
            label = label, label_nudge = label_nudge,
            label_fg = label_fg, label_size = label_size, label_bg = label_bg, label_bg_r = label_bg_r,
            scale_y = scale_y, group_by = group_by, group_by_sep = group_by_sep, group_name = group_name,
            facet_by = facet_by, facet_scales = facet_scales, flip = flip, line_name = line_name,
            add_bg = add_bg, bg_palette = bg_palette, bg_palcolor = bg_palcolor, bg_alpha = bg_alpha,
            theme = theme, theme_args = theme_args, palette = palette, palcolor = palcolor,
            alpha = alpha, x_text_angle = x_text_angle, aspect.ratio = aspect.ratio,
            position = position, position_dodge_preserve = position_dodge_preserve, y_min = y_min, y_max = y_max,
            add_line = add_line, line_color = line_color, line_width = line_width, line_type = line_type,
            add_trend = add_trend, trend_color = trend_color, trend_linewidth = trend_linewidth, trend_ptsize = trend_ptsize,
            legend.position = legend.position, legend.direction = legend.direction,
            title = title, subtitle = subtitle, xlab = xlab, ylab = ylab, keep_na = keep_na, keep_empty = keep_empty,
            expand = expand, width = width, ...
        )
    }
    keep_empty_facet <- if (!is.null(facet_by)) keep_empty[[facet_by[1]]] else NULL
    if (length(facet_by) > 1) {
        stopifnot("[BarPlot] `keep_empty` for `facet_by` variables must be identical." =
            identical(keep_empty_facet, keep_empty[[facet_by[2]]]))
    }

    facet_args$plot <- p
    facet_args["facet_by"] <- list(facet_by)
    facet_args["facet_scales"] <- list(facet_scales)
    facet_args["drop"] <- list(!isTRUE(keep_empty_facet))
    facet_args["nrow"] <- list(facet_nrow)
    facet_args["ncol"] <- list(facet_ncol)
    facet_args["byrow"] <- list(facet_byrow)
    facet_args["legend.position"] <- list(legend.position)
    facet_args["legend.direction"] <- list(legend.direction)
    do.call(facet_plot, facet_args)
}

#' Bar Plot
#'
#' @description
#'  * `BarPlot` is used to create a bar plot.
#'  * `SplitBarPlot` (a.k.a `WaterfallPlot`) is used to create a bar plot with splitting the bars on the two sides.
#' @rdname barplot
#' @inheritParams common_args
#' @inheritParams BarPlotAtomic
#' @return A ggplot object or wrap_plots object or a list of ggplot objects
#' @export
#' @importFrom ggplot2 waiver
#' @examples
#' \donttest{
#' data <- data.frame(
#'     x = c("A", "B", "C", "D", "E", "F", "G", "H"),
#'     y = c(10, 8, 16, 4, 6, 12, 14, 2),
#'     group = c("G1", "G1", "G2", "G2", "G3", "G3", "G4", "G4"),
#'     facet = c("F1", "F2", "F3", "F4", "F1", "F2", "F3", "F4")
#' )
#'
#' BarPlot(data, x = "x", y = "y")
#' BarPlot(data, x = "x", y = "y", fill_by_x_if_no_group = FALSE)
#' BarPlot(data, x = "x", y = "y", label = TRUE)
#' BarPlot(data, x = "x", y = "y", label = "facet", label_nudge = 0)
#' BarPlot(data, x = "group", y = "y", group_by = "x")
#' BarPlot(data,
#'     x = "group", y = "y", group_by = "x",
#'     position = "dodge", add_bg = TRUE
#' )
#' BarPlot(data,
#'     x = "x", y = "y", split_by = "group",
#'     facet_by = "facet", position = "dodge", facet_ncol = 1
#' )
#' BarPlot(data,
#'     x = "x", y = "y", split_by = "group", facet_by = "facet",
#'     position = "dodge", facet_ncol = 1, guides = 'collect'
#' )
#' BarPlot(data,
#'     x = "x", y = "y", split_by = "group",
#'     palette = list(G1 = "Reds", G2 = "Blues", G3 = "Greens", G4 = "Purp"),
#'     facet_by = "facet", position = "dodge", facet_ncol = 1
#' )
#' BarPlot(data,
#'     x = "group", y = "y", group_by = "x",
#'     position = "dodge", add_bg = TRUE, bg_palette = "Spectral"
#' )
#' # use the count
#' BarPlot(data, x = "group", ylab = "count")
#' # flip the plot
#' BarPlot(data, x = "group", flip = TRUE, ylab = "count")
#'
#' data <- data.frame(
#'     x = factor(c("A", "B", "C", "D", "E", "F", NA, "H"), levels = LETTERS[1:10]),
#'     y = c(10, 8, 16, 4, 6, NA, 14, 2),
#'     group = factor(c("G1", "G1", "G2", NA, "G3", "G3", "G5", "G5"),
#'        levels = c("G1", "G2", "G3", "G4", "G5")),
#'     facet = factor(c("F1", NA, "F3", "F4", "F1", "F2", "F3", "F4"),
#'        levels = c("F1", "F2", "F3", "F4", "F5"))
#' )
#' # keep_na and keep_empty on simple barplots
#' BarPlot(data, x = "x", y = "y",
#'     title = "keep_na = FALSE; keep_empty = FALSE")
#' BarPlot(data, x = "x", y = "y",
#'     keep_na = TRUE, keep_empty = TRUE,
#'     title = "keep_na = TRUE; keep_empty = TRUE")
#' BarPlot(data, x = "x", y = "y",
#'     keep_na = TRUE, keep_empty = TRUE, facet_by = "facet",
#'     title = "keep_na = TRUE; keep_empty = TRUE; facet_by = 'facet'")
#' BarPlot(data, x = "x", y = "y",
#'     keep_na = TRUE, keep_empty = 'level',
#'     title = "keep_na = TRUE; keep_empty = 'level'")
#' BarPlot(data, x = "x", y = "y",
#'     keep_na = list(x = TRUE), keep_empty = list(x = FALSE),
#'     title = "keep_na: x=TRUE\nkeep_empty: x=FALSE"
#' )
#'
#' # keep_na and keep_empty on grouped barplots
#' BarPlot(data,
#'     x = "group", y = "y", group_by = "x",
#'     title = "keep_na = FALSE; keep_empty = FALSE")
#' BarPlot(data,
#'     x = "group", y = "y", group_by = "x",
#'     keep_na = TRUE, keep_empty = TRUE,
#'     title = "keep_na = TRUE; keep_empty = TRUE"
#' )
#' BarPlot(data,
#'     x = "group", y = "y", group_by = "x",
#'     keep_na = TRUE, keep_empty = TRUE, facet_by = "facet",
#'     title = "keep_na = TRUE; keep_empty = TRUE; facet_by = 'facet'"
#' )
#' BarPlot(data,
#'     x = "group", y = "y", group_by = "x",
#'     keep_na = TRUE, keep_empty = 'level',
#'     title = "keep_na = TRUE; keep_empty = 'level'"
#' )
#' BarPlot(data,
#'     x = "group", y = "y", group_by = "x",
#'     keep_na = list(x = TRUE, group = FALSE),
#'     keep_empty = list(x = FALSE, group = TRUE),
#'     title = "keep_na: x=TRUE, group=FALSE\nkeep_empty: x=FALSE, group=TRUE"
#' )
#' }
BarPlot <- function(
    data, x, x_sep = "_", y = NULL, flip = FALSE, fill_by_x_if_no_group = TRUE, line_name = NULL, label_nudge = 0.02,
    label = NULL, label_fg = "black", label_size = 4, label_bg = "white", label_bg_r = 0.1,
    group_by = NULL, group_by_sep = "_", group_name = NULL, split_by = NULL, split_by_sep = "_",
    facet_by = NULL, facet_scales = "fixed", facet_ncol = NULL, facet_nrow = NULL, facet_byrow = TRUE, facet_args = list(),
    add_bg = FALSE, bg_palette = "stripe", bg_palcolor = NULL, bg_alpha = 0.2,
    add_line = NULL, line_color = "red2", line_width = .6, line_type = 2,
    add_trend = FALSE, trend_color = "black", trend_linewidth = 1, trend_ptsize = 2,
    theme = "theme_this", theme_args = list(), palette = "Paired", palcolor = NULL,
    alpha = 1, x_text_angle = 0, aspect.ratio = 1, y_min = NULL, y_max = NULL,
    position = "auto", position_dodge_preserve = "total",
    legend.position = "right", legend.direction = "vertical",
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL, keep_empty = FALSE, keep_na = FALSE,
    expand = waiver(), width = waiver(), combine = TRUE, nrow = NULL, ncol = NULL, byrow = TRUE, seed = 8525,
    axes = NULL, axis_titles = axes, guides = NULL, design = NULL, ...) {
    validate_common_args(seed, facet_by = facet_by)
    keep_na <- check_keep_na(keep_na, c(x, split_by, facet_by, group_by))
    keep_empty <- check_keep_empty(keep_empty, c(x, split_by, facet_by, group_by))
    theme <- process_theme(theme)
    split_by <- check_columns(data, split_by, force_factor = TRUE, allow_multi = TRUE, concat_multi = TRUE, concat_sep = split_by_sep)

    if (!is.null(split_by)) {
        data <- process_keep_na_empty(data, keep_na, keep_empty, col = split_by)
        keep_na[[split_by]] <- NULL
        keep_empty[[split_by]] <- NULL

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
            title <- title %||% (if (length(datas) == 1 && identical(nm, "...")) NULL else nm)
            BarPlotAtomic(
                datas[[nm]],
                label = label, label_nudge = label_nudge,
                label_fg = label_fg, label_size = label_size, label_bg = label_bg, label_bg_r = label_bg_r,
                x = x, x_sep = x_sep, y = y, flip = flip, group_by = group_by, group_by_sep = group_by_sep, group_name = group_name,
                fill_by_x_if_no_group = fill_by_x_if_no_group,
                theme = theme, theme_args = theme_args, palette = palette[[nm]], palcolor = palcolor[[nm]], alpha = alpha,
                add_bg = add_bg, bg_palette = bg_palette, bg_palcolor = bg_palcolor, bg_alpha = bg_alpha,
                x_text_angle = x_text_angle, aspect.ratio = aspect.ratio, line_name = line_name,
                add_line = add_line, line_color = line_color, line_width = line_width, line_type = line_type,
                add_trend = add_trend, trend_color = trend_color, trend_linewidth = trend_linewidth, trend_ptsize = trend_ptsize,
                position = position, position_dodge_preserve = position_dodge_preserve, y_min = y_min, y_max = y_max,
                legend.position = legend.position[[nm]], legend.direction = legend.direction[[nm]],
                title = title, subtitle = subtitle, xlab = xlab, ylab = ylab, keep_empty = keep_empty, keep_na = keep_na,
                expand = expand, width = width, facet_by = facet_by, facet_scales = facet_scales,
                facet_ncol = facet_ncol, facet_nrow = facet_nrow, facet_byrow = facet_byrow, facet_args = facet_args,
                ...
            )
        }
    )

    combine_plots(plots, combine = combine, nrow = nrow, ncol = ncol, byrow = byrow,
        axes = axes, axis_titles = axis_titles, guides = guides, design = design)
}

#' SplitBarPlotAtomic
#'
#' @description Create a split bar plot without splitting the data.
#' @inheritParams common_args
#' @param x The column name of the terms on the x axis. There should be both negative and positive values.
#' @param y The column name(s) of the values. If there are multiple columns, they will be concatenated.
#' @param y_sep A character string to concatenate the x columns if there are multiple.
#' @param flip A logical value indicating whether to flip the x and y axes.
#' @param alpha_by A character string indicating the column name to use for the transparency of the bars.
#' @param alpha_reverse A logical value indicating whether to reverse the transparency.
#' @param alpha_name A character string indicating the legend name of the transparency.
#' @param order_y A list of character strings indicating the order of the y axis.
#'  The keys are "+", "-", or "*". However, "+/-" should not be mixed with "*".
#'  The values are "x_asc", "x_desc", "alpha_asc", or "alpha_desc", indicating how to order the y axis.
#'  The default is `list("+" = c("x_desc", "alpha_desc"), "-" = c("x_desc", "alpha_asc"))`, meaning
#'  the positive values are ordered by the x-axis values in descending order and the alpha values in descending order,
#'  and the negative values are ordered by the x-axis values in descending order and the alpha values in ascending order.
#'  The "*" key is used to order the y axis without considering the direction.
#' @param bar_height A numeric value indicating the height of the bars.
#' @param lineheight A numeric value indicating the height of the text.
#' @param max_charwidth A numeric value indicating the maximum width of the text.
#' @param fill_by A character string indicating the column name to use for the fill of the bars.
#' Can be either a categorical variable or a numeric variable.
#' @param fill_by_sep A character string to concatenate the fill columns if there are multiple.
#' @param fill_name A character string indicating the legend name of the fill.
#' @param direction_name A character string indicating the name of the direction column and will be shown in the legend.
#' @param direction_pos_name A character string indicating the name of the positive direction.
#' @param direction_neg_name A character string indicating the name of the negative direction.
#' @param x_min A numeric value indicating the minimum value of the x axis.
#' @param x_max A numeric value indicating the maximum value of the x axis.
#' @keywords internal
#' @importFrom stringr str_wrap
#' @importFrom forcats fct_relabel
#' @importFrom dplyr .data
#' @importFrom ggplot2 aes geom_vline geom_col geom_text scale_fill_manual labs scale_y_discrete position_nudge scale_alpha_continuous
#' @importFrom ggplot2 scale_alpha_continuous guide_none guide_legend
SplitBarPlotAtomic <- function(
    data, x, y, y_sep = "_", flip = FALSE, alpha_by = NULL, alpha_reverse = FALSE, alpha_name = NULL,
    order_y = list("+" = c("x_desc", "alpha_desc"), "-" = c("x_desc", "alpha_asc")), bar_height = 0.9,
    lineheight = 0.5, max_charwidth = 80, fill_by = NULL, fill_by_sep = "_",
    fill_name = NULL, direction_name = "direction", direction_pos_name = "positive", direction_neg_name = "negative",
    theme = "theme_this", theme_args = list(), palette = "Spectral", palcolor = NULL,
    facet_by = NULL, facet_scales = "free_y", facet_nrow = NULL, facet_ncol = NULL, facet_byrow = TRUE,
    aspect.ratio = 1, x_min = NULL, x_max = NULL,
    legend.position = "right", legend.direction = "vertical",
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL, keep_empty = FALSE, keep_na = FALSE,
    ...) {
    ggplot <- if (getOption("plotthis.gglogger.enabled", FALSE)) {
        gglogger::ggplot
    } else {
        ggplot2::ggplot
    }
    x <- check_columns(data, x)
    y <- check_columns(data, y, force_factor = TRUE, allow_multi = TRUE, concat_multi = TRUE, concat_sep = y_sep)
    fill_by <- check_columns(data, fill_by)
    fill_by <- fill_by %||% direction_name
    data[[direction_name]] <- ifelse(data[[x]] > 0, direction_pos_name, direction_neg_name)
    data[[direction_name]] <- factor(data[[direction_name]], levels = c(direction_pos_name, direction_neg_name))
    if (is.numeric(data[[fill_by]])) {
        fill_by_numeric <- TRUE
    } else {
        fill_by_numeric <- FALSE
        fill_by <- check_columns(data, fill_by, force_factor = TRUE, allow_multi = TRUE, concat_multi = TRUE, concat_sep = fill_by_sep)
    }

    alpha_by <- check_columns(data, alpha_by)
    facet_by <- check_columns(data, facet_by, force_factor = TRUE, allow_multi = TRUE)

    if (is.null(alpha_by)) {
        data$.alpha <- 1
        alpha_by <- ".alpha"
        alpha_guide <- guide_none()
    } else {
        alpha_guide <- guide_legend(order = 2)
    }

    if (!is.null(order_y)) {
        if (!is.list(order_y)) { order_y <- list("*" = order_y) }
        if (length(order_y) != 1 && length(order_y) != 2) {
            stop("'order_y' must be a list of length 1 or 2.")
        }
        if (length(order_y) == 1 && names(order_y) != "*") {
            stop("The name of the 'order_y' list must be '*' when it has only one element.")
        }
        if (length(order_y) == 2 && !all(c("+", "-") %in% names(order_y))) {
            stop("The names of the 'order_y' list must be '+' and '-' when it has two elements.")
        }
        for (o in order_y) {
            sapply(o, match.arg, c("x_asc", "x_desc", "alpha_asc", "alpha_desc"))
        }

        order_df <- function(df, dirs) {
            order_list <- list()
            for (dir in dirs) {
                if (dir == "x_asc") {
                    order_list[[dir]] <- df[[x]]
                } else if (dir == "x_desc") {
                    order_list[[dir]] <- -df[[x]]
                } else if (dir == "alpha_asc") {
                    order_list[[dir]] <- df[[alpha_by]]
                } else if (dir == "alpha_desc") {
                    order_list[[dir]] <- -df[[alpha_by]]
                }
            }
            df[do.call(order, order_list), , drop = FALSE]
        }
        if (length(order_y) == 1) {  # *
            data <- order_df(data, order_y[[1]])
        } else {  # +, -
            data[[direction_name]][is.na(data[[x]])] <- "positive"
            data[[x]][is.na(data[[x]])] <- 0
            data_pos <- order_df(data[data[[direction_name]] == direction_pos_name, ], order_y[["+"]])
            data_neg <- order_df(data[data[[direction_name]] == direction_neg_name, ], order_y[["-"]])
            data <- rbind(data_pos, data_neg)
            rm(data_pos, data_neg)
        }
        ordered_levels <- rev(unique(as.character(data[[y]])))
        ordered_levels <- c(ordered_levels, setdiff(levels(data[[y]]), unique(data[[y]])))
        data[[y]] <- factor(data[[y]], levels = ordered_levels)
    }
    data <- process_keep_na_empty(data, keep_na, keep_empty)
    data[[y]] <- fct_relabel(data[[y]], str_wrap, width = max_charwidth)

    # true: unused levels are kept on Y axis
    # false: unused levels are dropped
    # level: unused levels are dropped, but kept for determine fill_by colors
    keep_empty_y <- keep_empty[[y]]
    keep_empty_fill <- if (!fill_by_numeric) keep_empty[[fill_by]] else NULL
    keep_empty_facet <- if (!is.null(facet_by)) keep_empty[[facet_by[1]]] else NULL
    if (length(facet_by) > 1) {
        stopifnot("[SplitBarPlot] `keep_empty` for `facet_by` variables must be identical." =
            identical(keep_empty_facet, keep_empty[[facet_by[2]]]))
    }
    if (isTRUE(keep_empty_y)) {
        # Determine all possible y values globally (levels + NA if present anywhere)
        all_y_vals_global <- unique(c(levels(data[[y]]), if (anyNA(data[[y]])) NA else NULL))

        # Create facet grouping (or single group if no facets)
        if (!is.null(facet_by)) {
            facet_cols <- if (length(facet_by) > 1) {
                interaction(data[facet_by], drop = TRUE, sep = "___")
            } else {
                data[[facet_by[1]]]
            }
        } else {
            facet_cols <- factor(rep("__single__", nrow(data)))
        }

        # Add missing levels per facet (or globally if no facets)
        data_list <- split(data, facet_cols)
        data_list <- lapply(names(data_list), function(facet_val) {
            facet_data <- data_list[[facet_val]]
            y_vals <- unique(facet_data[[y]])
            missing_levels <- setdiff(all_y_vals_global, y_vals)

            if (length(missing_levels) > 0) {
                add_data <- data.frame(matrix(NA, nrow = length(missing_levels), ncol = ncol(facet_data)))
                colnames(add_data) <- colnames(facet_data)
                add_data[[y]] <- missing_levels
                add_data[[x]] <- 0
                add_data[[direction_name]] <- "positive"

                # Set facet variables if they exist
                if (!is.null(facet_by)) {
                    if (length(facet_by) > 1) {
                        facet_vals <- strsplit(facet_val, "___", fixed = TRUE)[[1]]
                        for (i in seq_along(facet_by)) {
                            add_data[[facet_by[i]]] <- facet_vals[i]
                        }
                    } else {
                        add_data[[facet_by[1]]] <- facet_val
                    }
                }

                facet_data <- rbind(facet_data, add_data)
            }
            facet_data
        })
        data <- do.call(rbind, data_list)
        rownames(data) <- NULL
    }

    x_min <- x_min %||% -max(abs(data[[x]]))
    x_max <- x_max %||% max(abs(data[[x]]))

    p <- ggplot(data, aes(x = !!sym(x), y = !!sym(y))) +
        geom_vline(xintercept = 0) +
        geom_col(aes(fill = !!sym(fill_by), alpha = !!sym(alpha_by)), color = "black", width = bar_height, show.legend = TRUE) +
        scale_alpha_continuous(
            name = alpha_name %||% alpha_by,
            range = if (alpha_reverse) c(1, 0.1) else c(0.1, 1),
            guide = alpha_guide
        )

    if (!fill_by_numeric) {
        fill_vals <- if (!isFALSE(keep_empty_fill) || !anyNA(data[[fill_by]])) {
            levels(data[[fill_by]])
        } else {
            c(levels(data[[fill_by]]), NA)
        }
        if (isTRUE(keep_empty_fill)) {
            p <- p +
                scale_fill_manual(
                    name = fill_name %||% fill_by,
                    values = palette_this(fill_vals, palette = palette, palcolor = palcolor),
                    guide = guide_legend(order = 1),
                    breaks = fill_vals, limits = fill_vals, drop = FALSE
                )
        } else {
            p <- p +
                scale_fill_manual(
                    name = fill_name %||% fill_by,
                    values = palette_this(fill_vals, palette = palette, palcolor = palcolor),
                    guide = guide_legend(order = 1)
                )
        }
    } else {
        p <- p + scale_fill_gradientn(
            name = fill_name %||% fill_by,
            n.breaks = 3,
            colors = palette_this(palette = palette, palcolor = palcolor),
            na.value = "grey80",
            guide = guide_colorbar(frame.colour = "black", ticks.colour = "black", title.hjust = 0)
        )
    }

    if (isTRUE(flip)) {
        p <- p +
            geom_text(
                aes(
                    x = 0, y = !!sym(y),
                    # nudge_x is not an aesthetic
                    label = ifelse(
                        is.na(!!sym(y)),
                        " NA ",
                        ifelse(.data[[x]] >= 0, gsub("(\\n|$)", " \\1", !!sym(y)), gsub("(^|\\n)", "\\1 ", !!sym(y)))
                    ),
                    hjust = ifelse(.data[[x]] >= 0, 1, 0),
                ),
                color = "black",
                lineheight = lineheight,
                angle = 90
            ) +
            ggplot2::theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
    } else {
        p <- p + geom_text(
            aes(
                x = 0, y = !!sym(y),
                # nudge_y is not an aesthetic
                label = ifelse(
                    is.na(!!sym(y)),
                    " NA ",
                    ifelse(.data[[x]] >= 0, gsub("(\\n|$)", " \\1", !!sym(y)), gsub("(^|\\n)", "\\1 ", !!sym(y)))
                ),
                hjust = ifelse(.data[[x]] >= 0, 1, 0),
            ),
            color = "black",
            lineheight = lineheight
        ) +
        ggplot2::theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
    }

    p <- p +
        labs(title = title, subtitle = subtitle, x = xlab %||% x, y = ylab %||% y) +
        # scale_x_continuous(expand = expand$x) +
        scale_y_discrete() +
        do.call(theme, theme_args) +
        ggplot2::theme(
            aspect.ratio = aspect.ratio,
            legend.position = legend.position,
            legend.direction = legend.direction
        )

    if (isTRUE(flip)) {
        p <- p + coord_flip(xlim = c(x_min, x_max))
        height <- 5.5
        width <- max(nlevels(data[[y]]) * bar_height / 4, 4.5)
    } else {
        p <- p + coord_cartesian(xlim = c(x_min, x_max))
        width <- 5.5
        height <- max(nlevels(data[[y]]) * bar_height / 4, 4.5)
    }
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
        legend.position = legend.position, legend.direction = legend.direction,
        drop = !isTRUE(keep_empty_facet))
}

#' @rdname barplot
#' @inheritParams common_args
#' @inheritParams SplitBarPlotAtomic
#' @export
#' @examples
#' \donttest{
#' set.seed(8525)
#' data <- data.frame(
#'     word = c("apple", "banana", "cherry", "date", "elderberry",
#'              "It is a very long term with a lot of words"),
#'     count = c(-10, 20, -30, 40, 50, 34),
#'     score = c(1, 2, 3, 4, 5, 3.2),
#'     group = c("A", "A", "B", "B", "C", "C")
#' )
#' SplitBarPlot(data, x = "count", y = "word", alpha_by = "score")
#' SplitBarPlot(data, x = "count", y = "word", alpha_by = "score",
#'              max_charwidth = 30, lineheight = 1.1)
#' SplitBarPlot(data, x = "count", y = "word", fill_by = "group")
#' SplitBarPlot(data, x = "count", y = "word", facet_by = "group",
#'              fill_name = "Direction")
#' SplitBarPlot(data, x = "count", y = "word", alpha_by = "score", split_by="group",
#'              palette = c(A = "Reds", B = "Blues", C = "Greens"))
#'
#' # test keep_na and keep_empty
#' data <- data.frame(
#'     word = factor(c("apple", "banana", "cherry", NA, "elderberry",
#'          "It is a very long term with a lot of words"),
#'          levels = c("apple", "banana", "cherry", "date", "elderberry",
#'              "unused", "It is a very long term with a lot of words")),
#'     count = c(-10, 20, NA, 40, 10, 34),
#'     score = c(1, 2, 3, 4, 5, 3.2),
#'     group = factor(sample(c("A", "A", "B", "B", "C", "C")),
#'          levels = c("A", "B", "C", "D"))
#' )
#'
#' SplitBarPlot(data, x = "count", y = "word", alpha_by = "score",
#'     title = "keep_na = FALSE; keep_empty = FALSE")
#' SplitBarPlot(data, x = "count", y = "word", alpha_by = "score",
#'     keep_na = TRUE, keep_empty = TRUE,
#'     title = "keep_na = TRUE; keep_empty = TRUE")
#' SplitBarPlot(data, x = "count", y = "word", alpha_by = "score",
#'     keep_na = TRUE, keep_empty = TRUE, facet_by = "group",
#'     title = "keep_na = TRUE; keep_empty = TRUE; facet_by = 'group'")
#' SplitBarPlot(data, x = "count", y = "word", alpha_by = "score",
#'     keep_na = TRUE, keep_empty = "level",
#'     title = "keep_na = TRUE; keep_empty = 'level'")
#' SplitBarPlot(data, x = "count", y = "word", alpha_by = "score",
#'     keep_na = list(word = FALSE), keep_empty = list(word = TRUE),
#'     title = "keep_na: word=FALSE; keep_empty: word=TRUE")
#' }
SplitBarPlot <- function(
    data, x, y, y_sep = "_", flip = FALSE, split_by = NULL, split_by_sep = "_",
    alpha_by = NULL, alpha_reverse = FALSE, alpha_name = NULL,
    order_y = list("+" = c("x_desc", "alpha_desc"), "-" = c("x_desc", "alpha_asc")), bar_height = 0.9,
    lineheight = 0.5, max_charwidth = 80, fill_by = NULL, fill_by_sep = "_",
    fill_name = NULL, direction_name = "direction", direction_pos_name = "positive", direction_neg_name = "negative",
    theme = "theme_this", theme_args = list(), palette = "Spectral", palcolor = NULL,
    facet_by = NULL, facet_scales = "free_y", facet_nrow = NULL, facet_ncol = NULL, facet_byrow = TRUE,
    aspect.ratio = 1, x_min = NULL, x_max = NULL,
    legend.position = "right", legend.direction = "vertical",
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL, keep_empty = FALSE, keep_na = FALSE,
    combine = TRUE, nrow = NULL, ncol = NULL, byrow = TRUE, seed = 8525,
    axes = NULL, axis_titles = axes, guides = NULL, design = NULL, ...
) {
    validate_common_args(seed, facet_by = facet_by)
    keep_na <- check_keep_na(keep_na, c(y, split_by, facet_by, fill_by))
    keep_empty <- check_keep_empty(keep_empty, c(y, split_by, facet_by, fill_by))
    theme <- process_theme(theme)
    split_by <- check_columns(data, split_by, force_factor = TRUE, allow_multi = TRUE, concat_multi = TRUE, concat_sep = split_by_sep)

    if (!is.null(split_by)) {
        data <- process_keep_na_empty(data, keep_na, keep_empty, col = split_by)
        keep_na[[split_by]] <- NULL
        keep_empty[[split_by]] <- NULL

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
            SplitBarPlotAtomic(datas[[nm]],
                x = x, y = y, y_sep = y_sep, flip = flip, alpha_by = alpha_by, alpha_reverse = alpha_reverse, alpha_name = alpha_name,
                order_y = order_y, bar_height = bar_height, lineheight = lineheight, max_charwidth = max_charwidth,
                fill_by = fill_by, fill_by_sep = fill_by_sep, fill_name = fill_name,
                direction_name = direction_name, direction_pos_name = direction_pos_name, direction_neg_name = direction_neg_name,
                theme = theme, theme_args = theme_args, palette = palette[[nm]], palcolor = palcolor[[nm]],
                facet_by = facet_by, facet_scales = facet_scales, facet_nrow = facet_nrow, facet_ncol = facet_ncol, facet_byrow = facet_byrow,
                aspect.ratio = aspect.ratio, x_min = x_min, x_max = x_max,
                legend.position = legend.position[[nm]], legend.direction = legend.direction[[nm]],
                title = title, subtitle = subtitle, xlab = xlab, ylab = ylab, keep_empty = keep_empty, keep_na = keep_na,
                ...
            )
        }
    )

    combine_plots(plots, combine = combine, nrow = nrow, ncol = ncol, byrow = byrow,
        axes = axes, axis_titles = axis_titles, guides = guides, design = design)
}

#' @rdname barplot
#' @inheritParams SplitBarPlot
#' @export
WaterfallPlot <- SplitBarPlot
