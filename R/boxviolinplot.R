#' Atomic Box/Violin plot
#'
#' @inheritParams common_args
#' @param x A character string of the column name to plot on the x-axis.
#'  A character/factor column is expected. If multiple columns are provided, the columns will be concatenated with `x_sep`.
#' @param x_sep A character string to concatenate the columns in `x`, if multiple columns are provided.
#'  When `in_form` is "wide", `x` columns will not be concatenated.
#' @param y A character string of the column name to plot on the y-axis. A numeric column is expected.
#'  When `in_form` is "wide", `y` is not required. The values under `x` columns will be used as y-values.
#' @param base A character string to specify the base plot type. Either "box" or "violin".
#' @param in_form A character string to specify the input data type. Either "long" or "wide".
#' @param sort_x A character string to specify the sorting of x-axis. Either "none", "mean_asc", "mean_desc", "mean", "median_asc", "median_desc", "median".
#' @param flip A logical value to flip the plot.
#' @param keep_empty A logical value to keep the empty levels in the x-axis.
#' @param group_by A character string of the column name to dodge the boxes/violins
#' @param group_by_sep A character string to concatenate the columns in `group_by`, if multiple columns are provided.
#' @param group_name A character string to name the legend of dodge.
#' @param fill_mode A character string to specify the fill mode. Either "dodge", "x", "mean", "median".
#' @param fill_reverse A logical value to reverse the fill colors for gradient fill (mean/median).
#' @param add_point A logical value to add (jitter) points to the plot.
#' @param pt_color A character string to specify the color of the points.
#' @param pt_size A numeric value to specify the size of the points.
#' @param pt_alpha A numeric value to specify the transparency of the points.
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
#' @param line_width A numeric value to specify the size of the line.
#' @param line_type A numeric value to specify the type of the line.
#' @param highlight A vector of character strings to highlight the points.
#'   It should be a subset of the row names of the data.
#'   If TRUE, it will highlight all points.
#' @param highlight_color A character string to specify the color of the highlighted points.
#' @param highlight_size A numeric value to specify the size of the highlighted points.
#' @param highlight_alpha A numeric value to specify the transparency of the highlighted points.
#' @param comparisons A logical value or a list of vectors to perform pairwise comparisons.
#' If `TRUE`, it will perform pairwise comparisons for all pairs.
#' @param ref_group A character string to specify the reference group for comparisons.
#' @param pairwise_method A character string to specify the pairwise comparison method.
#' @param multiplegroup_comparisons A logical value to perform multiple group comparisons.
#' @param multiple_method A character string to specify the multiple group comparison method.
#' @param sig_label A character string to specify the label of the significance test.
#' @param sig_labelsize A numeric value to specify the size of the significance test label.
#' @return A ggplot object
#' @keywords internal
#' @importFrom utils combn
#' @importFrom stats median quantile
#' @importFrom rlang sym syms parse_expr
#' @importFrom dplyr mutate ungroup first
#' @importFrom ggplot2 geom_boxplot geom_violin geom_jitter geom_point geom_line geom_hline geom_vline
#' @importFrom ggplot2 scale_fill_manual scale_color_manual scale_shape_manual scale_linetype_manual stat_summary
#' @importFrom ggplot2 labs theme element_line element_text position_dodge position_jitter coord_flip layer_scales
#' @importFrom ggplot2 position_jitterdodge scale_shape_identity scale_size_manual scale_alpha_manual scale_y_continuous
BoxViolinPlotAtomic <- function(
    data, x, x_sep = "_", y = NULL, base = c("box", "violin"), in_form = c("long", "wide"),
    sort_x = c("none", "mean_asc", "mean_desc", "mean", "median_asc", "median_desc", "median"),
    flip = FALSE, keep_empty = FALSE, group_by = NULL, group_by_sep = "_", group_name = NULL,
    x_text_angle = ifelse(isTRUE(flip) && isTRUE(stack), 90, 45),
    fill_mode = ifelse(!is.null(group_by), "dodge", "x"), fill_reverse = FALSE,
    theme = "theme_this", theme_args = list(), palette = "Paired", palcolor = NULL, alpha = 1,
    aspect.ratio = NULL, legend.position = "right", legend.direction = "vertical",
    add_point = FALSE, pt_color = "grey30", pt_size = NULL, pt_alpha = 1, y_nbreaks = 4,
    jitter_width = 0.5, jitter_height = 0.1, stack = FALSE, y_max = NULL, y_min = NULL, y_trans = "identity",
    add_box = FALSE, box_color = "black", box_width = 0.1, box_ptsize = 2.5,
    add_trend = FALSE, trend_color = NULL, trend_linewidth = 1, trend_ptsize = 2,
    add_stat = NULL, stat_name = NULL, stat_color = "black", stat_size = 1, stat_stroke = 1, stat_shape = 25,
    add_bg = FALSE, bg_palette = "stripe", bg_palcolor = NULL, bg_alpha = 0.2,
    add_line = NULL, line_color = "red2", line_width = .6, line_type = 2,
    highlight = NULL, highlight_color = "red2", highlight_size = 1, highlight_alpha = 1,
    comparisons = NULL, ref_group = NULL, pairwise_method = "wilcox.test",
    multiplegroup_comparisons = FALSE, multiple_method = "kruskal.test",
    sig_label = c("p.signif", "p.format"), sig_labelsize = 3.5,
    facet_by = NULL, facet_scales = "fixed", facet_ncol = NULL, facet_nrow = NULL, facet_byrow = TRUE,
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL, seed = 8525, ...) {
    set.seed(seed)
    ggplot <- if (getOption("plotthis.gglogger.enabled", FALSE)) {
        gglogger::ggplot
    } else {
        ggplot2::ggplot
    }
    in_form <- match.arg(in_form)
    if (in_form == "wide") {
        data <- data %>% pivot_longer(cols = x, names_to = ".x", values_to = ".y")
        x <- ".x"
        y <- ".y"
    }
    x <- check_columns(data, x, force_factor = TRUE, allow_multi = TRUE, concat_multi = TRUE, concat_sep = x_sep)
    y <- check_columns(data, y)

    group_by <- check_columns(data, group_by,
        force_factor = TRUE,
        allow_multi = TRUE, concat_multi = TRUE, concat_sep = group_by_sep
    )
    facet_by <- check_columns(data, facet_by, force_factor = TRUE, allow_multi = TRUE)
    if (isTRUE(comparisons) && is.null(group_by)) {
        # stop("'group_by' must be provided to when 'comparisons' is TRUE.")
        comparisons <- combn(levels(data[[x]]), 2, simplify = FALSE)
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
        data <- data %>% unite(".compares_group", !!!syms(unique(c(x, group_by, facet_by))), sep = "_", remove = FALSE)
    }
    sort_x <- match.arg(sort_x)
    if (sort_x != "none" && !is.null(facet_by)) {
        stop("Cannot sort x-axis when facet_by is provided.")
    }
    data <- data %>%
        dplyr::group_by(!!!syms(unique(c(x, group_by, facet_by)))) %>%
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
        } else if (is.numeric(highlight)) {
            data$.highlight <- 1:nrow(data) %in% highlight
        } else if (is.character(highlight) && length(highlight) == 1) {
            data <- mutate(data, .highlight = !!parse_expr(highlight))
        } else if (is.null(rownames(data))) {
            stop("No row names in the data, please provide a vector of indexes to highlight.")
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
        fill_by <- group_by
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
            # There is a bug in ggplot2 with preserve = "single" for violin plots
            # See https://github.com/tidyverse/ggplot2/issues/2801
            # There is a fix but not yet released
            position = position_dodge(width = 0.9), scale = "width", trim = TRUE,
            alpha = alpha, width = 0.8,
        )
    }
    if (fill_mode == "dodge") {
        p <- p + scale_fill_manual(
            name = group_name %||% group_by,
            values = palette_this(levels(data[[group_by]]), palette = palette, palcolor = palcolor)
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
            group_use <- names(which(rowSums(table(data[[x]], data[[group_by]]) >= 2) >= 2))
            if (any(rowSums(table(data[[x]], data[[group_by]]) >= 2) >= 3)) {
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
            scale_color_manual(values = c("TRUE" = highlight_color, "FALSE" = pt_color)) +
            scale_size_manual(values = c("TRUE" = highlight_size, "FALSE" = pt_size %||% min(3000 / nrow(data), 0.6))) +
            scale_alpha_manual(values = c("TRUE" = highlight_alpha, "FALSE" = pt_alpha))
    }

    if (isTRUE(add_trend)) {
        if (is.null(trend_color)) {
            p <- p + stat_summary(
                fun = first, geom = "line", mapping = if (!is.null(group_by)) {
                    aes(y = !!sym(".y_median"), group = !!sym(group_by), color = !!sym(group_by))
                } else {
                    aes(y = !!sym(".y_median"), group = 1)
                },
                position = position_dodge(width = 0.9), linewidth = trend_linewidth
            )
            if (!is.null(group_by)) {
                p <- p + scale_color_manual(
                    values = palette_this(levels(data[[group_by]]), palette = palette, palcolor = palcolor),
                    guide = "none"
                )
            }
        } else {
            p <- p + stat_summary(
                fun = first, geom = "line", mapping = if (!is.null(group_by)) {
                    aes(y = !!sym(".y_median"), group = !!sym(group_by))
                } else {
                    aes(y = !!sym(".y_median"), group = 1)
                },
                position = position_dodge(width = 0.9), color = "black", linewidth = trend_linewidth
            )
        }

        p <- p + stat_summary(
            fun = first, geom = "point", mapping = if (!is.null(group_by)) {
                aes(y = !!sym(".y_median"), group = !!sym(group_by))
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
            color = line_color, linetype = line_type, linewidth = line_width
        )
    }

    if (!is.null(add_stat)) {
        p <- p + stat_summary(
            fun = add_stat, geom = "point", mapping = if (!is.null(group_by)) {
                aes(shape = !!sym("stat_shape"), group = !!sym(group_by))
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
        labs(title = title, subtitle = subtitle, x = xlab %||% x, y = ylab %||% y)

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
    nd <- ifelse(is.null(group_by), 1, nlevels(data[[group_by]]))
    facet_free <- !is.null(facet_by) && (
        identical(facet_scales, "free") ||
            (!flip && identical(facet_scales, "free_y")) ||
            (flip && identical(facet_scales, "free_x"))
    )
    if (isTRUE(flip) && isTRUE(stack)) {
        facet_nrow <- facet_nrow %||% 1
        strip_position <- "top"
        p <- p + ggplot2::theme(
            strip.text.x = element_text(angle = 90),
            panel.grid.major.x = element_line(color = "grey", linetype = 2),
            panel.spacing.x = unit(-1, "pt")
        )
        if (facet_free) {
            p <- p + coord_flip()
        } else {
            p <- p + coord_flip(ylim = c(y_min_use, y_max_use))
        }
        width <- max(3, width + 2 + x_maxchars * 0.05)
        height <- height + nx * nd * 0.3
    } else if (isTRUE(flip) && isFALSE(stack)) {
        strip_position <- "top"
        p <- p + ggplot2::theme(
            strip.text.y = element_text(angle = 0),
            panel.grid.major.x = element_line(color = "grey", linetype = 2),
        )
        if (facet_free) {
            p <- p + coord_flip()
        } else {
            p <- p + coord_flip(ylim = c(y_min_use, y_max_use))
        }
        width <- max(3, width + 2.2 + x_maxchars * 0.05)
        height <- height + nx * nd * 0.3
    } else if (isTRUE(stack)) {
        facet_ncol <- facet_ncol %||% 1
        strip_position <- "right"
        p <- p + ggplot2::theme(
            panel.spacing.y = unit(-1, "pt"),
            strip.text.y = element_text(angle = 0),
            panel.grid.major.y = element_line(color = "grey", linetype = 2),
        )
        if (!facet_free) {
            p <- p + coord_cartesian(ylim = c(y_min_use, y_max_use))
        }
        height <- height + 4 + x_maxchars * 0.05
        width <- width + nx * nd * 0.3
    } else {
        strip_position <- "top"
        p <- p + ggplot2::theme(
            strip.text.x = element_text(angle = 0),
            panel.grid.major.x = element_line(color = "grey", linetype = 2),
        )
        if (!facet_free) {
            p <- p + coord_cartesian(ylim = c(y_min_use, y_max_use))
        }
        height <- max(3, height + 2 + x_maxchars * 0.05)
        width <- width + nx * nd * 0.3
    }

    p <- p +
        do.call(theme, theme_args) +
        ggplot2::theme(
            aspect.ratio = aspect.ratio,
            axis.text.x = element_text(angle = x_text_angle, hjust = just$h, vjust = just$v),
            legend.position = legend.position,
            legend.direction = legend.direction,
        )

    attr(p, "height") <- height
    attr(p, "width") <- max(width, height)

    facet_plot(p, facet_by, facet_scales, facet_nrow, facet_ncol, facet_byrow,
        strip.position = strip_position, legend.position = legend.position,
        legend.direction = legend.direction
    )
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
    data, x, x_sep = "_", y = NULL, base = c("box", "violin"), in_form = c("long", "wide"),
    split_by = NULL, split_by_sep = "_",
    sort_x = c("none", "mean_asc", "mean_desc", "mean", "median_asc", "median_desc", "median"),
    flip = FALSE, keep_empty = FALSE, group_by = NULL, group_by_sep = "_", group_name = NULL,
    x_text_angle = ifelse(isTRUE(flip) && isTRUE(stack), 90, 45),
    fill_mode = ifelse(!is.null(group_by), "dodge", "x"), fill_reverse = FALSE,
    theme = "theme_this", theme_args = list(), palette = "Paired", palcolor = NULL, alpha = 1,
    aspect.ratio = NULL, legend.position = "right", legend.direction = "vertical",
    add_point = FALSE, pt_color = "grey30", pt_size = NULL, pt_alpha = 1,
    jitter_width = 0.5, jitter_height = 0.1, stack = FALSE, y_max = NULL, y_min = NULL,
    add_box = FALSE, box_color = "black", box_width = 0.1, box_ptsize = 2.5,
    add_trend = FALSE, trend_color = NULL, trend_linewidth = 1, trend_ptsize = 2,
    add_stat = NULL, stat_name = NULL, stat_color = "black", stat_size = 1, stat_stroke = 1, stat_shape = 25,
    add_bg = FALSE, bg_palette = "stripe", bg_palcolor = NULL, bg_alpha = 0.2,
    add_line = NULL, line_color = "red2", line_width = .6, line_type = 2,
    highlight = NULL, highlight_color = "red2", highlight_size = 1, highlight_alpha = 1,
    comparisons = NULL, ref_group = NULL, pairwise_method = "wilcox.test",
    multiplegroup_comparisons = FALSE, multiple_method = "kruskal.test",
    sig_label = c("p.signif", "p.format"), sig_labelsize = 3.5,
    facet_by = NULL, facet_scales = "fixed", facet_ncol = NULL, facet_nrow = NULL, facet_byrow = TRUE,
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL, seed = 8525,
    combine = TRUE, nrow = NULL, ncol = NULL, byrow = TRUE,
    axes = NULL, axis_titles = axes, guides = NULL, design = NULL, ...) {
    validate_common_args(seed)
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
                x = x, x_sep = x_sep, y = y, base = base, in_form = in_form,
                sort_x = sort_x, flip = flip, keep_empty = keep_empty, group_by = group_by, group_by_sep = group_by_sep, group_name = group_name,
                x_text_angle = x_text_angle, fill_mode = fill_mode, fill_reverse = fill_reverse,
                theme = theme, theme_args = theme_args, palette = palette[[nm]], palcolor = palcolor[[nm]], alpha = alpha,
                aspect.ratio = aspect.ratio, legend.position = legend.position[[nm]], legend.direction = legend.direction[[nm]],
                add_point = add_point, pt_color = pt_color, pt_size = pt_size, pt_alpha = pt_alpha,
                jitter_width = jitter_width, jitter_height = jitter_height, stack = stack, y_max = y_max, y_min = y_min,
                add_box = add_box, box_color = box_color, box_width = box_width, box_ptsize = box_ptsize,
                add_trend = add_trend, trend_color = trend_color, trend_linewidth = trend_linewidth, trend_ptsize = trend_ptsize,
                add_stat = add_stat, stat_name = stat_name, stat_color = stat_color, stat_size = stat_size, stat_stroke = stat_stroke, stat_shape = stat_shape,
                add_bg = add_bg, bg_palette = bg_palette, bg_palcolor = bg_palcolor, bg_alpha = bg_alpha,
                add_line = add_line, line_color = line_color, line_width = line_width, line_type = line_type,
                highlight = highlight, highlight_color = highlight_color, highlight_size = highlight_size, highlight_alpha = highlight_alpha,
                comparisons = comparisons, ref_group = ref_group, pairwise_method = pairwise_method,
                multiplegroup_comparisons = multiplegroup_comparisons, multiple_method = multiple_method,
                sig_label = sig_label, sig_labelsize = sig_labelsize,
                facet_by = facet_by, facet_scales = facet_scales, facet_ncol = facet_ncol, facet_nrow = facet_nrow, facet_byrow = facet_byrow,
                title = title, subtitle = subtitle, xlab = xlab, ylab = ylab, seed = seed, ...
            )
        }
    )

    combine_plots(plots, combine = combine, nrow = nrow, ncol = ncol, byrow = byrow,
        axes = axes, axis_titles = axis_titles, guides = guides, design = design)
}

#' Box / Violin Plot
#'
#' @description
#'  Box plot or violin plot with optional jitter points, trend line, statistical test, background, line, and highlight.
#' @rdname boxviolinplot
#' @return The Box / Violin plot(s).
#'  When `split_by` is not provided, it returns a ggplot object.
#'  When `split_by` is provided, it returns a object of plots wrapped by `patchwork::wrap_plots` if `combine = TRUE`;
#'  otherwise, it returns a list of ggplot objects.
#' @export
#' @inheritParams BoxViolinPlot
#' @examples
#' \donttest{
#' set.seed(8525)
#' data <- data.frame(
#'     x = rep(LETTERS[1:8], 40),
#'     y = rnorm(320),
#'     group1 = sample(c("g1", "g2"), 320, replace = TRUE),
#'     group2 = sample(c("h1", "h2", "h3", "h4"), 320, replace = TRUE)
#' )
#'
#' BoxPlot(data, x = "x", y = "y")
#' BoxPlot(data,
#'     x = "x", y = "y",
#'     stack = TRUE, flip = TRUE, facet_by = "group1",
#'     add_bg = TRUE, bg_palette = "Paired"
#' )
#' BoxPlot(data,
#'     x = "x", y = "y",
#'     stack = TRUE, flip = TRUE, split_by = "group1",
#'     add_bg = TRUE, bg_palette = "Paired",
#'     palcolor = list(g1 = c("red", "blue"), g2 = c("blue", "red"))
#' )
#'
#' # wide form data
#' data_wide <- data.frame(
#'     A = rnorm(100),
#'     B = rnorm(100),
#'     C = rnorm(100)
#' )
#' BoxPlot(data_wide, x = c("A", "B", "C"), in_form = "wide")
#' }
BoxPlot <- function(
    data, x, x_sep = "_", y = NULL, in_form = c("long", "wide"),
    split_by = NULL, split_by_sep = "_",
    sort_x = c("none", "mean_asc", "mean_desc", "mean", "median_asc", "median_desc", "median"),
    flip = FALSE, keep_empty = FALSE, group_by = NULL, group_by_sep = "_", group_name = NULL,
    x_text_angle = ifelse(isTRUE(flip) && isTRUE(stack), 90, 45),
    fill_mode = ifelse(!is.null(group_by), "dodge", "x"), fill_reverse = FALSE,
    theme = "theme_this", theme_args = list(), palette = "Paired", palcolor = NULL, alpha = 1,
    aspect.ratio = NULL, legend.position = "right", legend.direction = "vertical",
    add_point = FALSE, pt_color = "grey30", pt_size = NULL, pt_alpha = 1,
    jitter_width = 0.5, jitter_height = 0.1, stack = FALSE, y_max = NULL, y_min = NULL,
    add_trend = FALSE, trend_color = NULL, trend_linewidth = 1, trend_ptsize = 2,
    add_stat = NULL, stat_name = NULL, stat_color = "black", stat_size = 1, stat_stroke = 1, stat_shape = 25,
    add_bg = FALSE, bg_palette = "stripe", bg_palcolor = NULL, bg_alpha = 0.2,
    add_line = NULL, line_color = "red2", line_width = .6, line_type = 2,
    highlight = NULL, highlight_color = "red2", highlight_size = 1, highlight_alpha = 1,
    comparisons = NULL, ref_group = NULL, pairwise_method = "wilcox.test",
    multiplegroup_comparisons = FALSE, multiple_method = "kruskal.test",
    sig_label = c("p.signif", "p.format"), sig_labelsize = 3.5,
    facet_by = NULL, facet_scales = "fixed", facet_ncol = NULL, facet_nrow = NULL, facet_byrow = TRUE,
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL, seed = 8525,
    combine = TRUE, nrow = NULL, ncol = NULL, byrow = TRUE,
    axes = NULL, axis_titles = axes, guides = NULL, ...) {
    stat_name <- stat_name %||% paste0(y, " (", deparse(substitute(add_stat)), ")")
    BoxViolinPlot(
        data = data, x = x, x_sep = x_sep, y = y, base = "box", in_form = in_form,
        split_by = split_by, split_by_sep = split_by_sep,
        sort_x = sort_x, flip = flip, keep_empty = keep_empty, group_by = group_by, group_by_sep = group_by_sep, group_name = group_name,
        x_text_angle = x_text_angle, fill_mode = fill_mode, fill_reverse = fill_reverse,
        theme = theme, theme_args = theme_args, palette = palette, palcolor = palcolor, alpha = alpha,
        aspect.ratio = aspect.ratio, legend.position = legend.position, legend.direction = legend.direction,
        add_point = add_point, pt_color = pt_color, pt_size = pt_size, pt_alpha = pt_alpha,
        jitter_width = jitter_width, jitter_height = jitter_height, stack = stack, y_max = y_max, y_min = y_min,
        add_trend = add_trend, trend_color = trend_color, trend_linewidth = trend_linewidth, trend_ptsize = trend_ptsize,
        add_stat = add_stat, stat_name = stat_name, stat_color = stat_color, stat_size = stat_size, stat_stroke = stat_stroke, stat_shape = stat_shape,
        add_bg = add_bg, bg_palette = bg_palette, bg_palcolor = bg_palcolor, bg_alpha = bg_alpha,
        add_line = add_line, line_color = line_color, line_width = line_width, line_type = line_type,
        highlight = highlight, highlight_color = highlight_color, highlight_size = highlight_size, highlight_alpha = highlight_alpha,
        comparisons = comparisons, ref_group = ref_group, pairwise_method = pairwise_method,
        multiplegroup_comparisons = multiplegroup_comparisons, multiple_method = multiple_method,
        sig_label = sig_label, sig_labelsize = sig_labelsize,
        facet_by = facet_by, facet_scales = facet_scales, facet_ncol = facet_ncol, facet_nrow = facet_nrow, facet_byrow = facet_byrow,
        title = title, subtitle = subtitle, xlab = xlab, ylab = ylab, seed = seed, combine = combine, nrow = nrow, ncol = ncol, byrow = byrow,
        axes = axes, axis_titles = axis_titles, guides = guides, ...
    )
}

#' @rdname boxviolinplot
#' @export
#' @inheritParams BoxViolinPlot
#' @examples
#' \donttest{
#' ViolinPlot(data, x = "x", y = "y")
#' ViolinPlot(data, x = "x", y = "y", add_box = TRUE)
#' ViolinPlot(data, x = "x", y = "y", add_point = TRUE)
#' ViolinPlot(data, x = "x", y = "y", add_trend = TRUE)
#' ViolinPlot(data, x = "x", y = "y", add_stat = mean)
#' ViolinPlot(data, x = "x", y = "y", add_bg = TRUE)
#' ViolinPlot(data, x = "x", y = "y", add_line = 0)
#' ViolinPlot(data, x = "x", y = "y", group_by = "group1")
#' ViolinPlot(data,
#'     x = "x", y = "y", group_by = "group1",
#'     facet_by = "group2", add_box = TRUE
#' )
#' ViolinPlot(data, x = "x", y = "y", add_point = TRUE, highlight = 'group1 == "g1"',
#'     alpha = 0.8, highlight_size = 1.5, pt_size = 1, add_box = TRUE)
#' ViolinPlot(data,
#'     x = "x", y = "y", group_by = "group1",
#'     comparisons = TRUE
#' )
#' ViolinPlot(data,
#'     x = "x", y = "y", sig_label = "p.format",
#'     facet_by = "group2", comparisons = list(c("A", "B"))
#' )
#' ViolinPlot(data,
#'     x = "x", y = "y", fill_mode = "mean",
#'     facet_by = "group2", palette = "Blues"
#' )
#' ViolinPlot(data,
#'     x = "x", y = "y", fill_mode = "mean",
#'     split_by = "group1", palette = c(g1 = "Blues", g2 = "Reds")
#' )
#' ViolinPlot(data,
#'     x = "x", y = "y", stack = TRUE,
#'     facet_by = "group2", add_box = TRUE, add_bg = TRUE,
#'     bg_palette = "Paired"
#' )
#' }
ViolinPlot <- function(
    data, x, x_sep = "_", y = NULL, in_form = c("long", "wide"),
    split_by = NULL, split_by_sep = "_",
    sort_x = c("none", "mean_asc", "mean_desc", "mean", "median_asc", "median_desc", "median"),
    flip = FALSE, keep_empty = FALSE, group_by = NULL, group_by_sep = "_", group_name = NULL,
    x_text_angle = ifelse(isTRUE(flip) && isTRUE(stack), 90, 45),
    fill_mode = ifelse(!is.null(group_by), "dodge", "x"), fill_reverse = FALSE,
    theme = "theme_this", theme_args = list(), palette = "Paired", palcolor = NULL, alpha = 1,
    aspect.ratio = NULL, legend.position = "right", legend.direction = "vertical",
    add_point = FALSE, pt_color = "grey30", pt_size = NULL, pt_alpha = 1,
    jitter_width = 0.5, jitter_height = 0.1, stack = FALSE, y_max = NULL, y_min = NULL,
    add_box = FALSE, box_color = "black", box_width = 0.1, box_ptsize = 2.5,
    add_trend = FALSE, trend_color = NULL, trend_linewidth = 1, trend_ptsize = 2,
    add_stat = NULL, stat_name = NULL, stat_color = "black", stat_size = 1, stat_stroke = 1, stat_shape = 25,
    add_bg = FALSE, bg_palette = "stripe", bg_palcolor = NULL, bg_alpha = 0.2,
    add_line = NULL, line_color = "red2", line_width = .6, line_type = 2,
    highlight = NULL, highlight_color = "red2", highlight_size = 1, highlight_alpha = 1,
    comparisons = NULL, ref_group = NULL, pairwise_method = "wilcox.test",
    multiplegroup_comparisons = FALSE, multiple_method = "kruskal.test",
    sig_label = c("p.signif", "p.format"), sig_labelsize = 3.5,
    facet_by = NULL, facet_scales = "fixed", facet_ncol = NULL, facet_nrow = NULL, facet_byrow = TRUE,
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL, seed = 8525,
    combine = TRUE, nrow = NULL, ncol = NULL, byrow = TRUE,
    axes = NULL, axis_titles = axes, guides = NULL, ...) {
    stat_name <- stat_name %||% paste0(y, " (", deparse(substitute(add_stat)), ")")
    BoxViolinPlot(
        data = data, x = x, x_sep = x_sep, y = y, base = "violin", in_form = in_form,
        split_by = split_by, split_by_sep = split_by_sep,
        sort_x = sort_x, flip = flip, keep_empty = keep_empty, group_by = group_by, group_by_sep = group_by_sep, group_name = group_name,
        x_text_angle = x_text_angle, fill_mode = fill_mode, fill_reverse = fill_reverse,
        theme = theme, theme_args = theme_args, palette = palette, palcolor = palcolor, alpha = alpha,
        aspect.ratio = aspect.ratio, legend.position = legend.position, legend.direction = legend.direction,
        add_point = add_point, pt_color = pt_color, pt_size = pt_size, pt_alpha = pt_alpha,
        jitter_width = jitter_width, jitter_height = jitter_height, stack = stack, y_max = y_max, y_min = y_min,
        add_box = add_box, box_color = box_color, box_width = box_width, box_ptsize = box_ptsize,
        add_trend = add_trend, trend_color = trend_color, trend_linewidth = trend_linewidth, trend_ptsize = trend_ptsize,
        add_stat = add_stat, stat_name = stat_name, stat_color = stat_color, stat_size = stat_size, stat_stroke = stat_stroke, stat_shape = stat_shape,
        add_bg = add_bg, bg_palette = bg_palette, bg_palcolor = bg_palcolor, bg_alpha = bg_alpha,
        add_line = add_line, line_color = line_color, line_width = line_width, line_type = line_type,
        highlight = highlight, highlight_color = highlight_color, highlight_size = highlight_size, highlight_alpha = highlight_alpha,
        comparisons = comparisons, ref_group = ref_group, pairwise_method = pairwise_method,
        multiplegroup_comparisons = multiplegroup_comparisons, multiple_method = multiple_method,
        sig_label = sig_label, sig_labelsize = sig_labelsize,
        facet_by = facet_by, facet_scales = facet_scales, facet_ncol = facet_ncol, facet_nrow = facet_nrow, facet_byrow = facet_byrow,
        title = title, subtitle = subtitle, xlab = xlab, ylab = ylab, seed = seed, combine = combine, nrow = nrow, ncol = ncol, byrow = byrow,
        axes = axes, axis_titles = axis_titles, guides = guides, ...
    )
}
