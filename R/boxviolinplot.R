#' Make the wide format data into long format
#'
#' @param data A data frame
#' @param dodge_by A character string specifying the column name of the data frame to dodge the plot.
#' @param dodge_by_sep A character string to concatenate the columns in `dodge_by`,
#'   if multiple columns are provided and the types of the columns are not logical/(0, 1).
#' @param facet_by A character string specifying the column name of the data frame to facet the plot.
#' @keywords internal
#' @return Transformed data with dodge_by and facet_by attributes
#' @importFrom dplyr %>% filter select
make_long <- function(data, dodge_by = NULL, dodge_by_sep = "_", facet_by = NULL) {
    if (length(dodge_by) < 2 && length(facet_by) < 2) {
        attr(data, "dodge_by") <- dodge_by
        attr(data, "facet_by") <- facet_by
        return(data)
    }

    if (length(dodge_by) > 1) {
        # if all dodge_by columns are logical/(0, 1), then pivot it to long format
        # otherwise, concatenate the columns
        if (!any(sapply(dodge_by, function(x) !is.logical(data[[x]]) && !all(data[[x]] %in% c(0, 1))))) {
            data <- data %>%
                pivot_longer(cols = dodge_by, names_to = ".dodge_by", values_to = ".dodge_value") %>%
                filter(!!sym(".dodge_value") == 1) %>%
                select(-".dodge_value")
        }
        dodge_by <- ".dodge_by"
    }

    if (length(facet_by) > 1) {
        if (!any(sapply(facet_by, function(x) !is.logical(data[[x]]) && !all(data[[x]] %in% c(0, 1))))) {
            data <- data %>%
                pivot_longer(cols = facet_by, names_to = ".facet_by", values_to = ".facet_value") %>%
                filter(!!sym(".facet_value") == 1) %>%
                select(-".facet_value")

            facet_by <- ".facet_by"
        }
    }

    attr(data, "dodge_by") <- dodge_by
    attr(data, "facet_by") <- facet_by
    return(data)
}

#' Atomic Box/Violin plot
#'
#' @inheritParams common_args
#' @param x A character string of the column name to plot on the x-axis.
#'   A character/factor column is expected. If multiple columns are provided, the columns will be concatenated with `x_sep`.
#' @param x_sep A character string to concatenate the columns in `x`, if multiple columns are provided.
#' @param y A character string of the column name to plot on the y-axis. A numeric column is expected.
#' @param base A character string to specify the base plot type. Either "box" or "violin".
#' @param intype A character string to specify the input data type. Either "long" or "wide".
#' @param sort_x A character string to specify the sorting of x-axis. Either "none", "mean_asc", "mean_desc", "mean", "median_asc", "median_desc", "median".
#' @param flip A logical value to flip the plot.
#' @param keep_empty A logical value to keep the empty levels in the x-axis.
#' @param dodge_by A character string of the column name to dodge the boxes/violins
#' @param dodge_by_sep A character string to concatenate the columns in `dodge_by`, if multiple columns are provided.
#' @param dodge_name A character string to name the legend of dodge.
#' @param fill_mode A character string to specify the fill mode. Either "dodge", "x", "mean", "median".
#' @param fill_reverse A logical value to reverse the fill colors for gradient fill (mean/median).
#' @param add_point A logical value to add (jitter) points to the plot.
#' @param point_color A character string to specify the color of the points.
#' @param point_size A numeric value to specify the size of the points.
#' @param point_alpha A numeric value to specify the transparency of the points.
#' @param jitter_width A numeric value to specify the width of the jitter.
#' @param jitter_height A numeric value to specify the height of the jitter.
#' @param stack A logical value whether to stack the facetted plot by 'facet_by'.
#' @param y_max A numeric value or a character string to specify the maximum value of the y-axis.
#' @param y_min A numeric value or a character string to specify the minimum value of the y-axis.
#' @param y_trans A character string to specify the transformation of the y-axis.
#' @param y_nbreaks A numeric value to specify the number of breaks in the y-axis.
#' @param add_box A logical value to add box plot to the plot.
#' @param box_color A character string to specify the color of the box plot.
#' @param box_width A numeric value to specify the width of the box plot.
#' @param box_ptsize A numeric value to specify the size of the box plot points in the middle.
#' @param add_trend A logical value to add trend line to the plot.
#' @param trend_color A character string to specify the color of the trend line.
#' @param trend_linewidth A numeric value to specify the width of the trend line.
#' @param trend_ptsize A numeric value to specify the size of the trend line points.
#' @param add_stat A character string to add statistical test to the plot.
#' @param stat_name A character string to specify the name of the stat legend.
#' @param stat_color A character string to specify the color of the statistical test.
#' @param stat_size A numeric value to specify the size of the statistical test.
#' @param stat_stroke A numeric value to specify the stroke of the statistical test.
#' @param stat_shape A numeric value to specify the shape of the statistical test.
#' @param add_bg A logical value to add background to the plot.
#' @param bg_palette A character string to specify the palette of the background.
#' @param bg_palcolor A character vector to specify the colors of the background.
#' @param bg_alpha A numeric value to specify the transparency of the background.
#' @param add_line A character string to add a line to the plot.
#' @param line_color A character string to specify the color of the line.
#' @param line_size A numeric value to specify the size of the line.
#' @param line_type A numeric value to specify the type of the line.
#' @param highlight A vector of character strings to highlight the points.
#'   It should be a subset of the row names of the data.
#'   If TRUE, it will highlight all points.
#' @param highlight_color A character string to specify the color of the highlighted points.
#' @param highlight_size A numeric value to specify the size of the highlighted points.
#' @param highlight_alpha A numeric value to specify the transparency of the highlighted points.
#' @param comparisons A logical value or a list of vectors to perform pairwise comparisons.
#' @param ref_group A character string to specify the reference group for comparisons.
#' @param pairwise_method A character string to specify the pairwise comparison method.
#' @param multiplegroup_comparisons A logical value to perform multiple group comparisons.
#' @param multiple_method A character string to specify the multiple group comparison method.
#' @param sig_label A character string to specify the label of the significance test.
#' @param sig_labelsize A numeric value to specify the size of the significance test label.
#' @return A ggplot object
#' @keywords internal
#' @importFrom stats median quantile
#' @importFrom rlang sym syms
#' @importFrom dplyr group_by mutate ungroup first
#' @importFrom gglogger ggplot
#' @importFrom ggplot2 geom_boxplot geom_violin geom_jitter geom_point geom_line geom_hline geom_vline
#' @importFrom ggplot2 scale_fill_manual scale_color_manual scale_shape_manual scale_linetype_manual stat_summary
#' @importFrom ggplot2 labs theme element_line element_text position_dodge position_jitter coord_flip layer_scales
#' @importFrom ggplot2 position_jitterdodge scale_shape_identity scale_size_manual scale_alpha_manual scale_y_continuous
BoxViolinPlotAtomic <- function(
    data, x, x_sep = "_", y, base = c("box", "violin"), intype = c("long", "wide"),
    sort_x = c("none", "mean_asc", "mean_desc", "mean", "median_asc", "median_desc", "median"),
    flip = FALSE, keep_empty = FALSE, dodge_by = NULL, dodge_by_sep = "_", dodge_name = NULL,
    x_text_angle = ifelse(isTRUE(flip) && isTRUE(stack), 90, 45),
    fill_mode = ifelse(!is.null(dodge_by), "dodge", "x"), fill_reverse = FALSE,
    theme = "theme_this", theme_args = list(), palette = "Paired", palcolor = NULL, alpha = 1,
    aspect.ratio = NULL, legend.position = "right", legend.direction = "vertical",
    add_point = FALSE, point_color = "grey30", point_size = NULL, point_alpha = 1, y_nbreaks = 4,
    jitter_width = 0.5, jitter_height = 0.1, stack = FALSE, y_max = NULL, y_min = NULL, y_trans = "identity",
    add_box = FALSE, box_color = "black", box_width = 0.1, box_ptsize = 2.5,
    add_trend = FALSE, trend_color = "black", trend_linewidth = 1, trend_ptsize = 2,
    add_stat = NULL, stat_name = NULL, stat_color = "black", stat_size = 1, stat_stroke = 1, stat_shape = 25,
    add_bg = FALSE, bg_palette = "stripe", bg_palcolor = NULL, bg_alpha = 0.2,
    add_line = NULL, line_color = "red2", line_size = .6, line_type = 2,
    highlight = NULL, highlight_color = "red2", highlight_size = 1, highlight_alpha = 1,
    comparisons = NULL, ref_group = NULL, pairwise_method = "wilcox.test",
    multiplegroup_comparisons = FALSE, multiple_method = "kruskal.test",
    sig_label = c("p.signif", "p.format"), sig_labelsize = 3.5,
    facet_by = NULL, facet_scales = "fixed", facet_ncol = NULL, facet_nrow = NULL, facet_byrow = TRUE,
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL, seed = 8525, ...) {
    set.seed(seed)
    intype <- match.arg(intype)
    if (intype == "wide") {
        data <- make_long(data, dodge_by = dodge_by, dodge_by_sep = dodge_by_sep, facet_by = facet_by)
        dodge_by <- attr(data, "dodge_by")
        facet_by <- attr(data, "facet_by")
    }

    x <- check_columns(data, x, force_factor = TRUE, allow_multi = TRUE, concat_multi = TRUE, concat_sep = x_sep)
    y <- check_columns(data, y)
    dodge_by <- check_columns(data, dodge_by,
        force_factor = TRUE,
        allow_multi = TRUE, concat_multi = TRUE, concat_sep = dodge_by_sep
    )
    facet_by <- check_columns(data, facet_by, force_factor = TRUE, allow_multi = TRUE)
    if (isTRUE(comparisons) && is.null(dodge_by)) {
        stop("'dodge_by' must be provided to when 'comparisons' is TRUE.")
    }
    if (isTRUE(multiplegroup_comparisons) || length(comparisons) > 0) {
        # if (!requireNamespace("ggpubr", quietly = TRUE)) {
        #     stop("ggpubr is required to perform comparisons.")
        # }
        if (!is.list(comparisons) && !isTRUE(comparisons)) {
            comparisons <- list(comparisons)
        }
        ncomp <- sapply(comparisons, length)
        if (any(ncomp) > 2) {
            stop("'comparisons' must be a list in which all elements must be vectors of length 2")
        }
        sig_label <- match.arg(sig_label)
        data <- data %>% unite(".compares_group", !!!syms(unique(c(x, dodge_by, facet_by))), sep = "_", remove = FALSE)
    }
    sort_x <- match.arg(sort_x)
    if (sort_x != "none" && !is.null(facet_by)) {
        stop("Cannot sort x-axis when facet_by is provided.")
    }
    data <- data %>%
        group_by(!!!syms(unique(c(x, dodge_by, facet_by)))) %>%
        mutate(.y_mean = mean(!!sym(y)), .y_median = median(!!sym(y))) %>%
        ungroup()

    values <- data[[y]][is.finite(data[[y]])]
    if (is.null(y_max)) {
        y_max_use <- max(values, na.rm = TRUE)
    } else if (is.character(y_max)) {
        q_max <- as.numeric(sub("(^q)(\\d+)", "\\2", y_max)) / 100
        y_max_use <- quantile(values, q_max, na.rm = TRUE)
    } else {
        y_max_use <- y_max
    }
    if (is.null(y_min)) {
        y_min_use <- min(values, na.rm = TRUE)
    } else if (is.character(y_min)) {
        q_min <- as.numeric(sub("(^q)(\\d+)", "\\2", y_min)) / 100
        y_min_use <- quantile(values, q_min, na.rm = TRUE)
    } else {
        y_min_use <- y_min
    }
    rm(values)

    if (!is.null(highlight)) {
        if (isTRUE(highlight)) {
            data$.highlight <- TRUE
        } else if (is.null(rownames(data))) {
            stop("Row names are missing in the data to select points to highlight.")
        } else {
            data$.highlight <- rownames(data) %in% highlight
        }
        if (isFALSE(add_point)) {
            warning("Forcing add_point = TRUE when highlight is provided.")
            add_point <- TRUE
        }
    } else {
        data$.highlight <- FALSE
    }
    data$.highlight <- factor(as.character(data$.highlight), levels = c("TRUE", "FALSE"))

    if (sort_x == "mean" || sort_x == "mean_asc") {
        data[[x]] <- stats::reorder(data[[x]], order(data$.y_mean))
    } else if (sort_x == "mean_desc") {
        data[[x]] <- stats::reorder(data[[x]], -order(data$.y_mean))
    } else if (sort_x == "median" || sort_x == "median_asc") {
        data[[x]] <- stats::reorder(data[[x]], order(data$.y_median))
    } else if (sort_x == "median_desc") {
        data[[x]] <- stats::reorder(data[[x]], -order(data$.y_median))
    }

    if (isTRUE(flip)) {
        data[[x]] <- factor(data[[x]], levels = rev(levels(data[[x]])))
        aspect.ratio <- 1 / aspect.ratio
        if (length(aspect.ratio) == 0 || is.na(aspect.ratio)) {
            aspect.ratio <- NULL
        }
    }

    base <- match.arg(base)
    if (isTRUE(add_box) && base == "box") {
        stop("Cannot add box plot to box plot.")
    }
    fill_mode <- match.arg(fill_mode, c("dodge", "x", "mean", "median"))
    if (fill_mode == "dodge") {
        fill_by <- dodge_by
    } else if (fill_mode == "x") {
        fill_by <- x
    } else if (fill_mode == "mean") {
        fill_by <- ".y_mean"
    } else {
        fill_by <- ".y_median"
    }
    p <- ggplot(data, aes(x = !!sym(x), y = !!sym(y), fill = !!sym(fill_by)))
    if (isTRUE(add_bg)) {
        p <- p + bg_layer(data, x, bg_palette, bg_palcolor, bg_alpha, keep_empty, facet_by)
    }

    if (base == "box") {
        p <- p + geom_boxplot(
            position = position_dodge(width = 0.9), color = "black", width = 0.8, outlier.shape = NA
        )
    } else {
        p <- p + geom_violin(
            scale = "width", trim = TRUE, alpha = alpha, position = position_dodge()
        )
    }
    if (fill_mode == "dodge") {
        p <- p + scale_fill_manual(
            name = dodge_name %||% dodge_by,
            values = palette_this(levels(data[[dodge_by]]), palette = palette, palcolor = palcolor)
        )
    } else if (fill_mode == "x") {
        p <- p + scale_fill_manual(
            name = x,
            values = palette_this(levels(data[[x]]), palette = palette, palcolor = palcolor)
        )
    } else {
        p <- p + scale_fill_gradientn(
            name = paste0(y, " (", fill_mode, ")"),
            n.breaks = 3,
            colors = palette_this(palette = palette, palcolor = palcolor, reverse = fill_reverse),
            na.value = "grey80",
            guide = guide_colorbar(frame.colour = "black", ticks.colour = "black", title.hjust = 0)
        )
    }

    if (isTRUE(add_box)) {
        p <- p +
            new_scale_fill() +
            geom_boxplot(
                position = position_dodge(width = 0.9), fill = box_color, color = box_color,
                width = box_width, show.legend = FALSE, outlier.shape = NA
            ) +
            stat_summary(
                fun = first, geom = "point", mapping = aes(y = !!sym(".y_median")),
                position = position_dodge(width = 0.9), color = "black", fill = "white",
                size = box_ptsize, shape = 21
            )
    }

    if (length(comparisons) > 0) {
        if (isTRUE(comparisons)) {
            group_use <- names(which(rowSums(table(data[[x]], data[[dodge_by]]) >= 2) >= 2))
            if (any(rowSums(table(data[[x]], data[[dodge_by]]) >= 2) >= 3)) {
                message("Detected more than 2 groups. Use multiple_method for comparison")
                method <- multiple_method
            } else {
                method <- pairwise_method
            }
            p <- p + ggpubr::stat_compare_means(
                data = data[data[[x]] %in% group_use, , drop = FALSE],
                mapping = aes(x = !!sym(x), y = !!sym(y), group = !!sym(".compares_group")),
                label = sig_label,
                label.y = y_max_use,
                size = sig_labelsize,
                step.increase = 0.1,
                tip.length = 0.03,
                vjust = 1,
                method = method
            )

            y_max_use <- layer_scales(p)$y$range$range[2]
        } else {
            p <- p + ggpubr::stat_compare_means(
                mapping = aes(x = !!sym(x), y = !!sym(y), group = !!sym(".compares_group")),
                label = sig_label,
                label.y = y_max_use,
                size = sig_labelsize,
                step.increase = 0.1,
                tip.length = 0.03,
                vjust = 0,
                comparisons = comparisons,
                ref.group = ref_group,
                method = pairwise_method
            )
            y_max_use <- layer_scales(p)$y$range$range[1] + (layer_scales(p)$y$range$range[2] - layer_scales(p)$y$range$range[1]) * 1.15
        }
    }
    if (isTRUE(multiplegroup_comparisons)) {
        p <- p + ggpubr::stat_compare_means(
            mapping = aes(x = !!sym(x), y = !!sym(y), group = !!sym(".compares_group")),
            method = multiple_method,
            label = sig_label,
            label.y = y_max_use,
            size = sig_labelsize,
            vjust = 1.2,
            hjust = 0
        )
        y_max_use <- layer_scales(p)$y$range$range[1] + (layer_scales(p)$y$range$range[2] - layer_scales(p)$y$range$range[1]) * 1.15
    }

    if (isTRUE(add_point)) {
        p <- p +
            geom_point(
                aes(fill = !!sym(fill_by), color = !!sym(".highlight"), size = !!sym(".highlight"), alpha = !!sym(".highlight")),
                position = position_jitterdodge(
                    jitter.width = jitter_width, jitter.height = jitter_height, dodge.width = 0.9, seed = seed
                ),
                show.legend = FALSE
            ) +
            scale_color_manual(values = c("TRUE" = highlight_color, "FALSE" = point_color)) +
            scale_size_manual(values = c("TRUE" = highlight_size, "FALSE" = point_size %||% min(3000 / nrow(data), 0.6))) +
            scale_alpha_manual(values = c("TRUE" = highlight_alpha, "FALSE" = point_alpha))
    }

    if (isTRUE(add_trend)) {
        p <- p +
            stat_summary(
                fun = first, geom = "line", mapping = if (!is.null(dodge_by)) {
                    aes(y = !!sym(".y_median"), group = !!sym(dodge_by))
                } else {
                    aes(y = !!sym(".y_median"), group = 1)
                },
                position = position_dodge(width = 0.9), color = trend_color, linewidth = trend_linewidth
            ) +
            stat_summary(
                fun = first, geom = "point", mapping = if (!is.null(dodge_by)) {
                    aes(y = !!sym(".y_median"), group = !!sym(dodge_by))
                } else {
                    aes(y = !!sym(".y_median"), group = 1)
                },
                position = position_dodge(width = 0.9), color = "black", fill = "white",
                size = trend_ptsize, shape = 21
            )
    }

    if (!is.null(add_line)) {
        p <- p + geom_hline(
            yintercept = add_line,
            color = line_color, linetype = line_type, linewidth = line_size
        )
    }

    if (!is.null(add_stat)) {
        p <- p + stat_summary(
            fun = add_stat, geom = "point", mapping = if (!is.null(dodge_by)) {
                aes(shape = !!sym("stat_shape"), group = !!sym(dodge_by))
            } else {
                aes(shape = !!sym("stat_shape"), group = 1)
            },
            position = position_dodge(width = 0.9), color = stat_color, fill = stat_color, size = stat_size, stroke = stat_stroke,
        ) + scale_shape_identity(
            labels = stat_name %||% paste0(y, " (", deparse(substitute(add_stat)), ")"),
            guide = guide_legend(title = "", order = 2)
        )
    }

    just <- calc_just(x_text_angle)
    p <- p +
        scale_x_discrete(drop = !keep_empty) +
        labs(title = title, subtitle = subtitle, x = xlab %||% x, y = ylab %||% y) +
        do.call(theme, theme_args) +
        ggplot2::theme(
            aspect.ratio = aspect.ratio,
            axis.text.x = element_text(angle = x_text_angle, hjust = just$h, vjust = just$v),
            legend.position = legend.position,
            legend.direction = legend.direction,
        )

    p <- p + scale_y_continuous(trans = y_trans, n.breaks = y_nbreaks)

    height <- width <- 0
    if (!identical(legend.position, "none")) {
        if (legend.position %in% c("right", "left")) {
            width <- width + 1
        } else if (legend.direction == "horizontal") {
            height <- height + 1
        } else {
            height <- height + 2
        }
    }
    x_maxchars <- max(nchar(levels(data[[x]])))
    nx <- nlevels(data[[x]])
    nd <- ifelse(is.null(dodge_by), 1, nlevels(data[[dodge_by]]))
    if (isTRUE(flip) && isTRUE(stack)) {
        facet_nrow <- facet_nrow %||% 1
        strip_position <- "top"
        p <- p + theme(
            strip.text.x = element_text(angle = 90),
            panel.grid.major.x = element_line(color = "grey", linetype = 2),
            panel.spacing.x = unit(-1, "pt")
        ) + coord_flip(ylim = c(y_min_use, y_max_use))
        width <- width + 2 + x_maxchars * 0.05
        height <- height + nx * nd * 0.3
    } else if (isTRUE(flip) && isFALSE(stack)) {
        strip_position <- "top"
        p <- p + theme(
            strip.text.y = element_text(angle = 0),
            panel.grid.major.x = element_line(color = "grey", linetype = 2),
        ) + coord_flip(ylim = c(y_min_use, y_max_use))
        width <- width + 2.2 + x_maxchars * 0.05
        height <- height + nx * nd * 0.3
    } else if (isTRUE(stack)) {
        facet_ncol <- facet_ncol %||% 1
        strip_position <- "right"
        p <- p + theme(
            panel.spacing.y = unit(-1, "pt"),
            strip.text.y = element_text(angle = 0),
            panel.grid.major.y = element_line(color = "grey", linetype = 2),
        ) + coord_cartesian(ylim = c(y_min_use, y_max_use))
        height <- height + 4 + x_maxchars * 0.05
        width <- width + nx * nd * 0.3
    } else {
        strip_position <- "top"
        p <- p + theme(
            strip.text.x = element_text(angle = 0),
            panel.grid.major.x = element_line(color = "grey", linetype = 2),
        ) + coord_cartesian(ylim = c(y_min_use, y_max_use))
        height <- height + 2 + x_maxchars * 0.05
        width <- width + nx * nd * 0.3
    }
    attr(p, "height") <- height
    attr(p, "width") <- width

    facet_plot(p, facet_by, facet_scales, facet_nrow, facet_ncol, facet_byrow,
        strip.position = strip_position, legend.position = legend.position,
        legend.direction = legend.direction)
}

#' Box/Violin plot
#'
#' @rdname BoxViolinPlot-internal
#' @inheritParams common_args
#' @inheritParams BoxViolinPlotAtomic
#' @return A combined ggplot object or wrap_plots object or a list of ggplot objects
#' @keywords internal
#' @importFrom rlang %||%
BoxViolinPlot <- function(
    data, x, x_sep = "_", y, base = c("box", "violin"), intype = c("long", "wide"),
    split_by = NULL, split_by_sep = "_",
    sort_x = c("none", "mean_asc", "mean_desc", "mean", "median_asc", "median_desc", "median"),
    flip = FALSE, keep_empty = FALSE, dodge_by = NULL, dodge_by_sep = "_", dodge_name = NULL,
    x_text_angle = ifelse(isTRUE(flip) && isTRUE(stack), 90, 45),
    fill_mode = ifelse(!is.null(dodge_by), "dodge", "x"), fill_reverse = FALSE,
    theme = "theme_this", theme_args = list(), palette = "Paired", palcolor = NULL, alpha = 1,
    aspect.ratio = NULL, legend.position = "right", legend.direction = "vertical",
    add_point = FALSE, point_color = "grey30", point_size = NULL, point_alpha = 1,
    jitter_width = 0.5, jitter_height = 0.1, stack = FALSE, y_max = NULL, y_min = NULL,
    add_box = FALSE, box_color = "black", box_width = 0.1, box_ptsize = 2.5,
    add_trend = FALSE, trend_color = "black", trend_linewidth = 1, trend_ptsize = 2,
    add_stat = NULL, stat_name = NULL, stat_color = "black", stat_size = 1, stat_stroke = 1, stat_shape = 25,
    add_bg = FALSE, bg_palette = "stripe", bg_palcolor = NULL, bg_alpha = 0.2,
    add_line = NULL, line_color = "red2", line_size = .6, line_type = 2,
    highlight = NULL, highlight_color = "red2", highlight_size = 1, highlight_alpha = 1,
    comparisons = NULL, ref_group = NULL, pairwise_method = "wilcox.test",
    multiplegroup_comparisons = FALSE, multiple_method = "kruskal.test",
    sig_label = c("p.signif", "p.format"), sig_labelsize = 3.5,
    facet_by = NULL, facet_scales = "fixed", facet_ncol = NULL, facet_nrow = NULL, facet_byrow = TRUE,
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL, seed = 8525,
    combine = TRUE, nrow = NULL, ncol = NULL, byrow = TRUE, ...) {

    validate_common_args(seed)
    split_by <- check_columns(data, split_by, force_factor = TRUE, allow_multi = TRUE, concat_multi = TRUE, concat_sep = split_by_sep)

    if (!is.null(split_by)) {
        datas <- split(data, data[[split_by]])
        # keep the order of levels
        datas <- datas[levels(data[[split_by]])]
    } else {
        datas <- list(data)
        names(datas) <- "..."
    }

    stat_name <- stat_name %||% paste0(y, " (", deparse(substitute(add_stat)), ")")

    plots <- lapply(
        names(datas), function(nm) {
            default_title <- if (length(datas) == 1 && identical(nm, "...")) NULL else nm
            if (is.function(title)) {
                title <- title(default_title)
            } else {
                title <- title %||% default_title
            }
            BoxViolinPlotAtomic(datas[[nm]],
                x = x, x_sep = x_sep, y = y, base = base, intype = intype,
                sort_x = sort_x, flip = flip, keep_empty = keep_empty, dodge_by = dodge_by, dodge_by_sep = dodge_by_sep, dodge_name = dodge_name,
                x_text_angle = x_text_angle, fill_mode = fill_mode, fill_reverse = fill_reverse,
                theme = theme, theme_args = theme_args, palette = palette, palcolor = palcolor, alpha = alpha,
                aspect.ratio = aspect.ratio, legend.position = legend.position, legend.direction = legend.direction,
                add_point = add_point, point_color = point_color, point_size = point_size, point_alpha = point_alpha,
                jitter_width = jitter_width, jitter_height = jitter_height, stack = stack, y_max = y_max, y_min = y_min,
                add_box = add_box, box_color = box_color, box_width = box_width, box_ptsize = box_ptsize,
                add_trend = add_trend, trend_color = trend_color, trend_linewidth = trend_linewidth, trend_ptsize = trend_ptsize,
                add_stat = add_stat, stat_name = stat_name, stat_color = stat_color, stat_size = stat_size, stat_stroke = stat_stroke, stat_shape = stat_shape,
                add_bg = add_bg, bg_palette = bg_palette, bg_palcolor = bg_palcolor, bg_alpha = bg_alpha,
                add_line = add_line, line_color = line_color, line_size = line_size, line_type = line_type,
                highlight = highlight, highlight_color = highlight_color, highlight_size = highlight_size, highlight_alpha = highlight_alpha,
                comparisons = comparisons, ref_group = ref_group, pairwise_method = pairwise_method,
                multiplegroup_comparisons = multiplegroup_comparisons, multiple_method = multiple_method,
                sig_label = sig_label, sig_labelsize = sig_labelsize,
                facet_by = facet_by, facet_scales = facet_scales, facet_ncol = facet_ncol, facet_nrow = facet_nrow, facet_byrow = facet_byrow,
                title = title, subtitle = subtitle, xlab = xlab, ylab = ylab, seed = seed, ...
            )
        }
    )

    combine_plots(plots, combine = combine, nrow = nrow, ncol = ncol, byrow = byrow)
}

#' Box / Violin Plot
#'
#' @description
#'  Box plot or violin plot with optional jitter points, trend line, statistical test, background, line, and highlight.
#' @rdname boxviolinplot
#' @export
#' @inheritParams BoxViolinPlot
#' @examples
#' set.seed(8525)
#' data <- data.frame(
#'     x = rep(LETTERS[1:8], 40),
#'     y = rnorm(320),
#'     group1 = sample(c("g1", "g2"), 320, replace = TRUE),
#'     group2 = sample(c("h1", "h2", "h3", "h4"), 320, replace = TRUE)
#' )
#'
#' BoxPlot(data, x = "x", y = "y")
#' BoxPlot(data, x = "x", y = "y",
#'         stack = TRUE, flip = TRUE, facet_by = "group1",
#'         add_bg = TRUE, bg_palette = "Paired")
BoxPlot <- function(
    data, x, x_sep = "_", y, intype = c("long", "wide"),
    split_by = NULL, split_by_sep = "_",
    sort_x = c("none", "mean_asc", "mean_desc", "mean", "median_asc", "median_desc", "median"),
    flip = FALSE, keep_empty = FALSE, dodge_by = NULL, dodge_by_sep = "_", dodge_name = NULL,
    x_text_angle = ifelse(isTRUE(flip) && isTRUE(stack), 90, 45),
    fill_mode = ifelse(!is.null(dodge_by), "dodge", "x"), fill_reverse = FALSE,
    theme = "theme_this", theme_args = list(), palette = "Paired", palcolor = NULL, alpha = 1,
    aspect.ratio = NULL, legend.position = "right", legend.direction = "vertical",
    add_point = FALSE, point_color = "grey30", point_size = NULL, point_alpha = 1,
    jitter_width = 0.5, jitter_height = 0.1, stack = FALSE, y_max = NULL, y_min = NULL,
    add_trend = FALSE, trend_color = "black", trend_linewidth = 1, trend_ptsize = 2,
    add_stat = NULL, stat_name = NULL, stat_color = "black", stat_size = 1, stat_stroke = 1, stat_shape = 25,
    add_bg = FALSE, bg_palette = "stripe", bg_palcolor = NULL, bg_alpha = 0.2,
    add_line = NULL, line_color = "red2", line_size = .6, line_type = 2,
    highlight = NULL, highlight_color = "red2", highlight_size = 1, highlight_alpha = 1,
    comparisons = NULL, ref_group = NULL, pairwise_method = "wilcox.test",
    multiplegroup_comparisons = FALSE, multiple_method = "kruskal.test",
    sig_label = c("p.signif", "p.format"), sig_labelsize = 3.5,
    facet_by = NULL, facet_scales = "fixed", facet_ncol = NULL, facet_nrow = NULL, facet_byrow = TRUE,
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL, seed = 8525,
    combine = TRUE, nrow = NULL, ncol = NULL, byrow = TRUE, ...) {

    stat_name <- stat_name %||% paste0(y, " (", deparse(substitute(add_stat)), ")")
    BoxViolinPlot(
        data = data, x = x, x_sep = x_sep, y = y, base = "box", intype = intype,
        split_by = split_by, split_by_sep = split_by_sep,
        sort_x = sort_x, flip = flip, keep_empty = keep_empty, dodge_by = dodge_by, dodge_by_sep = dodge_by_sep, dodge_name = dodge_name,
        x_text_angle = x_text_angle, fill_mode = fill_mode, fill_reverse = fill_reverse,
        theme = theme, theme_args = theme_args, palette = palette, palcolor = palcolor, alpha = alpha,
        aspect.ratio = aspect.ratio, legend.position = legend.position, legend.direction = legend.direction,
        add_point = add_point, point_color = point_color, point_size = point_size, point_alpha = point_alpha,
        jitter_width = jitter_width, jitter_height = jitter_height, stack = stack, y_max = y_max, y_min = y_min,
        add_trend = add_trend, trend_color = trend_color, trend_linewidth = trend_linewidth, trend_ptsize = trend_ptsize,
        add_stat = add_stat, stat_name = stat_name, stat_color = stat_color, stat_size = stat_size, stat_stroke = stat_stroke, stat_shape = stat_shape,
        add_bg = add_bg, bg_palette = bg_palette, bg_palcolor = bg_palcolor, bg_alpha = bg_alpha,
        add_line = add_line, line_color = line_color, line_size = line_size, line_type = line_type,
        highlight = highlight, highlight_color = highlight_color, highlight_size = highlight_size, highlight_alpha = highlight_alpha,
        comparisons = comparisons, ref_group = ref_group, pairwise_method = pairwise_method,
        multiplegroup_comparisons = multiplegroup_comparisons, multiple_method = multiple_method,
        sig_label = sig_label, sig_labelsize = sig_labelsize,
        facet_by = facet_by, facet_scales = facet_scales, facet_ncol = facet_ncol, facet_nrow = facet_nrow, facet_byrow = facet_byrow,
        title = title, subtitle = subtitle, xlab = xlab, ylab = ylab, seed = seed, combine = combine, nrow = nrow, ncol = ncol, byrow = byrow, ...
    )
}

#' @rdname boxviolinplot
#' @export
#' @inheritParams BoxViolinPlot
#' @examples
#' ViolinPlot(data, x = "x", y = "y")
#' ViolinPlot(data, x = "x", y = "y", add_box = TRUE)
#' ViolinPlot(data, x = "x", y = "y", add_point = TRUE)
#' ViolinPlot(data, x = "x", y = "y", add_trend = TRUE)
#' ViolinPlot(data, x = "x", y = "y", add_stat = mean)
#' ViolinPlot(data, x = "x", y = "y", add_bg = TRUE)
#' ViolinPlot(data, x = "x", y = "y", add_line = 0)
#' ViolinPlot(data, x = "x", y = "y", dodge_by = "group1")
#' ViolinPlot(data, x = "x", y = "y", dodge_by = "group1",
           #' facet_by = "group2", add_box = TRUE)
#' ViolinPlot(data, x = "x", y = "y", dodge_by = "group1",
           #' comparisons = TRUE)
#' ViolinPlot(data, x = "x", y = "y", sig_label = "p.format",
           #' facet_by = "group2", comparisons = list(c("A", "B")))
#' ViolinPlot(data, x = "x", y = "y", fill_mode = "mean",
           #' facet_by = "group2", palette = "Blues")
#' ViolinPlot(data, x = "x", y = "y", stack = TRUE,
           #' facet_by = "group2", add_box = TRUE, add_bg = TRUE,
           #' bg_palette = "Paired")
ViolinPlot <- function(
    data, x, x_sep = "_", y, intype = c("long", "wide"),
    split_by = NULL, split_by_sep = "_",
    sort_x = c("none", "mean_asc", "mean_desc", "mean", "median_asc", "median_desc", "median"),
    flip = FALSE, keep_empty = FALSE, dodge_by = NULL, dodge_by_sep = "_", dodge_name = NULL,
    x_text_angle = ifelse(isTRUE(flip) && isTRUE(stack), 90, 45),
    fill_mode = ifelse(!is.null(dodge_by), "dodge", "x"), fill_reverse = FALSE,
    theme = "theme_this", theme_args = list(), palette = "Paired", palcolor = NULL, alpha = 1,
    aspect.ratio = NULL, legend.position = "right", legend.direction = "vertical",
    add_point = FALSE, point_color = "grey30", point_size = NULL, point_alpha = 1,
    jitter_width = 0.5, jitter_height = 0.1, stack = FALSE, y_max = NULL, y_min = NULL,
    add_box = FALSE, box_color = "black", box_width = 0.1, box_ptsize = 2.5,
    add_trend = FALSE, trend_color = "black", trend_linewidth = 1, trend_ptsize = 2,
    add_stat = NULL, stat_name = NULL, stat_color = "black", stat_size = 1, stat_stroke = 1, stat_shape = 25,
    add_bg = FALSE, bg_palette = "stripe", bg_palcolor = NULL, bg_alpha = 0.2,
    add_line = NULL, line_color = "red2", line_size = .6, line_type = 2,
    highlight = NULL, highlight_color = "red2", highlight_size = 1, highlight_alpha = 1,
    comparisons = NULL, ref_group = NULL, pairwise_method = "wilcox.test",
    multiplegroup_comparisons = FALSE, multiple_method = "kruskal.test",
    sig_label = c("p.signif", "p.format"), sig_labelsize = 3.5,
    facet_by = NULL, facet_scales = "fixed", facet_ncol = NULL, facet_nrow = NULL, facet_byrow = TRUE,
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL, seed = 8525,
    combine = TRUE, nrow = NULL, ncol = NULL, byrow = TRUE, ...) {

    stat_name <- stat_name %||% paste0(y, " (", deparse(substitute(add_stat)), ")")
    BoxViolinPlot(
        data = data, x = x, x_sep = x_sep, y = y, base = "violin", intype = intype,
        split_by = split_by, split_by_sep = split_by_sep,
        sort_x = sort_x, flip = flip, keep_empty = keep_empty, dodge_by = dodge_by, dodge_by_sep = dodge_by_sep, dodge_name = dodge_name,
        x_text_angle = x_text_angle, fill_mode = fill_mode, fill_reverse = fill_reverse,
        theme = theme, theme_args = theme_args, palette = palette, palcolor = palcolor, alpha = alpha,
        aspect.ratio = aspect.ratio, legend.position = legend.position, legend.direction = legend.direction,
        add_point = add_point, point_color = point_color, point_size = point_size, point_alpha = point_alpha,
        jitter_width = jitter_width, jitter_height = jitter_height, stack = stack, y_max = y_max, y_min = y_min,
        add_box = add_box, box_color = box_color, box_width = box_width, box_ptsize = box_ptsize,
        add_trend = add_trend, trend_color = trend_color, trend_linewidth = trend_linewidth, trend_ptsize = trend_ptsize,
        add_stat = add_stat, stat_name = stat_name, stat_color = stat_color, stat_size = stat_size, stat_stroke = stat_stroke, stat_shape = stat_shape,
        add_bg = add_bg, bg_palette = bg_palette, bg_palcolor = bg_palcolor, bg_alpha = bg_alpha,
        add_line = add_line, line_color = line_color, line_size = line_size, line_type = line_type,
        highlight = highlight, highlight_color = highlight_color, highlight_size = highlight_size, highlight_alpha = highlight_alpha,
        comparisons = comparisons, ref_group = ref_group, pairwise_method = pairwise_method,
        multiplegroup_comparisons = multiplegroup_comparisons, multiple_method = multiple_method,
        sig_label = sig_label, sig_labelsize = sig_labelsize,
        facet_by = facet_by, facet_scales = facet_scales, facet_ncol = facet_ncol, facet_nrow = facet_nrow, facet_byrow = facet_byrow,
        title = title, subtitle = subtitle, xlab = xlab, ylab = ylab, seed = seed, combine = combine, nrow = nrow, ncol = ncol, byrow = byrow, ...
    )
}
