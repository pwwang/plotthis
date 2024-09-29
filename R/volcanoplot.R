#' Atomic volcano plot
#'
#' @inheritParams common_args
#' @param ytrans A function to transform the y-axis values.
#' @param color_by A character vector of column names to color the points by.
#'  If NULL, the points will be filled by the x and y cutoff value.
#' @param color_name A character string to name the legend of color.
#' @param flip_negatives A logical value to flip the y-axis for negative x values.
#' @param x_cutoff A numeric value to set the x-axis cutoff.
#'  Both negative and positive of this value will be used.
#' @param y_cutoff A numeric value to set the y-axis cutoff.
#'  Note that the y-axis cutoff will be transformed by `ytrans`.
#'  So you should provide the original value.
#' @param x_cutoff_name A character string to name the x-axis cutoff.
#'  If "none", the legend for the x-axis cutoff will not be shown.
#' @param y_cutoff_name A character string to name the y-axis cutoff.
#'  If "none", the legend for the y-axis cutoff will not be shown.
#' @param x_cutoff_color A character string to color the x-axis cutoff line.
#' @param y_cutoff_color A character string to color the y-axis cutoff line.
#' @param x_cutoff_linetype A character string to set the x-axis cutoff line type.
#' @param y_cutoff_linetype A character string to set the y-axis cutoff line type.
#' @param x_cutoff_linewidth A numeric value to set the x-axis cutoff line size.
#' @param y_cutoff_linewidth A numeric value to set the y-axis cutoff line size.
#' @param pt_size A numeric value to set the point size.
#' @param pt_alpha A numeric value to set the point transparency.
#' @param nlabel A numeric value to set the number of labels to show.
#'  The points will be ordered by the distance to the origin. Top `nlabel` points will be labeled.
#' @param labels A character vector of row names or indexes to label the points.
#' @param label_size A numeric value to set the label size.
#' @param label_fg A character string to set the label color.
#' @param label_bg A character string to set the label background color.
#' @param label_bg_r A numeric value specifying the radius of the background of the label.
#' @param highlight A character vector of row names or indexes to highlight the points.
#' @param highlight_color A character string to set the highlight color.
#' @param highlight_size A numeric value to set the highlight size.
#' @param highlight_alpha A numeric value to set the highlight transparency.
#' @param highlight_stroke A numeric value to set the highlight stroke size.
#'
#' @return A ggplot object
#' @keywords internal
#' @importFrom dplyr %>% case_when mutate arrange row_number group_by ungroup desc filter
VolcanoPlotAtomic <- function(
    data, x, y, ytrans = function(n) -log10(n), color_by = NULL, color_name = NULL,
    flip_negatives = FALSE, x_cutoff = NULL, y_cutoff = 0.05,
    x_cutoff_name = NULL, y_cutoff_name = NULL, x_cutoff_color = "red2", y_cutoff_color = "blue2",
    x_cutoff_linetype = "dashed", y_cutoff_linetype = "dashed", x_cutoff_linewidth = 0.5, y_cutoff_linewidth = 0.5,
    pt_size = 2, pt_alpha = 0.5, nlabel = 5, labels = NULL, label_size = 3, label_fg = "black", label_bg = "white",
    label_bg_r = 0.1, highlight = NULL, highlight_color = "red", highlight_size = 2, highlight_alpha = 1,
    highlight_stroke = 0.5,
    facet_by = NULL, facet_scales = "fixed", facet_ncol = NULL, facet_nrow = NULL, facet_byrow = TRUE,
    theme = "theme_this", theme_args = list(), palette = "Spectral", palcolor = NULL,
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL,
    aspect.ratio = 1, legend.position = "right", legend.direction = "vertical", seed = 8525,
    ...
) {
    x <- check_columns(data, x)
    y <- check_columns(data, y)
    color_by <- check_columns(data, color_by)
    facet_by <- check_columns(data, facet_by, force_factor = TRUE, allow_multi = TRUE)

    data[[y]] <- ytrans(data[[y]])
    x_cutoff <- x_cutoff %||% 0
    y_cutoff <- ytrans(y_cutoff)

    if (!is.null(y_cutoff)) {
        data <- data %>% mutate(
            .category = factor(
                case_when(
                    !!sym(x) > x_cutoff & !!sym(y) > y_cutoff ~ "sig_pos_x",
                    !!sym(x) < -x_cutoff & !!sym(y) > y_cutoff ~ "sig_neg_x",
                    TRUE ~ "insig"
                ),
                levels = c("sig_neg_x", "insig", "sig_pos_x")
            )
        )
    } else {
        data <- data %>% mutate(
            .category = factor(
                case_when(
                    !!sym(x) > x_cutoff ~ "sig_pos_x",
                    !!sym(x) < -x_cutoff ~ "sig_neg_x",
                    TRUE ~ "insig"
                ),
                levels = c("sig_neg_x", "insig", "sig_pos_x")
            )
        )
    }

    if (is.null(color_by)) {
        color_by <- ".category"
        color_type <- "discrete"
    } else if (is.null(color_by)) {
        color_by <- ".category"
        color_type <- "discrete"
    } else {
        color_type <- "continuous"
    }

    if (flip_negatives) {
        data[data[[x]] < 0, y] <- -data[data[[x]] < 0, y]
    }
    data$.label <- rownames(data)
    data$.show_label <- FALSE
    if (!is.null(labels)) {
        data$.show_label[labels] <- TRUE
    } else {
        # calculate the distance to the origin
        data$.distance <- sqrt(data[[x]]^2 + data[[y]]^2)
    }

    x_upper <- quantile(data[[x]][is.finite(data[[x]])], c(0.99, 1))
    x_lower <- quantile(data[[x]][is.finite(data[[x]])], c(0.01, 0))
    x_upper <- ifelse(x_upper[1] > 0, x_upper[1], x_upper[2])
    x_lower <- ifelse(x_lower[1] < 0, x_lower[1], x_lower[2])
    if (x_upper > 0 & x_lower < 0) {
        value_range <- min(abs(c(x_upper, x_lower)), na.rm = TRUE)
        x_upper <- value_range
        x_lower <- -value_range
    }
    data$.outlier <- data[[x]] > x_upper | data[[x]] < x_lower
    data[[x]][data[[x]] > x_upper] <- x_upper
    data[[x]][data[[x]] < x_lower] <- x_lower

    if (!is.null(facet_by)) {
        data <- data %>% group_by(!!!syms(facet_by), sign(data[[x]])) %>%
            arrange(desc(!!sym(".distance"))) %>%
            mutate(.show_label = row_number() <= nlabel) %>%
            ungroup()
    } else {
        data <- data %>% group_by(sign(data[[x]])) %>%
            arrange(desc(!!sym(".distance"))) %>%
            mutate(.show_label = row_number() <= nlabel) %>%
            ungroup()
    }
    data <- data %>% mutate(.show_label = !!sym(".show_label") & !!sym(".category") != "insig") %>%
        as.data.frame()
    rownames(data) <- data$.label

    # x_nudge is not an aesthetic, we can't specify it separately for negative and positive values
    # so we plot the points in two layers
    # one for negative values and one for positive values
    # and set the x_nudge for each layer
    pos_data <- data[data[[x]] >= 0, , drop = FALSE]
    neg_data <- data[data[[x]] < 0, , drop = FALSE]

    outlier_data <- data[data$.outlier, , drop = FALSE]

    # x_nudges will be the same for all facets
    pos_x_nudge <- -diff(range(pos_data[[x]])) * 0.05
    neg_x_nudge <- diff(range(neg_data[[x]])) * 0.05
    jitter <- position_jitter(width = 0.2, height = 0.2, seed = seed)

    p <- ggplot(mapping = aes(x = !!sym(x), y = !!sym(y), color = !!sym(color_by))) +
        geom_point(data = pos_data, size = pt_size, alpha = pt_alpha) +
        geom_point(data = neg_data, size = pt_size, alpha = pt_alpha) +
        geom_point(data = outlier_data, size = pt_size, alpha = pt_alpha, position = jitter)

    if (color_type == "discrete") {
        colors <- palette_this(levels(data[[color_by]]), palette = palette, palcolor = palcolor)
        if (is.null(palcolor)) {
            colors['insig'] <- "grey"
        }
        p <- p + scale_color_manual(values = colors, guide = "none")
    } else {
        p <- p + scale_color_gradientn(
            colors = palette_this(palette = palette, palcolor = palcolor),
            values = scales::rescale(unique(c(
                min(c(unlist(data[[color_by]]), 0), na.rm = TRUE), 0,
                max(unlist(data[[color_by]]), na.rm = TRUE)))),
            guide = guide_colorbar(frame.colour = "black", ticks.colour = "black", title.hjust = 0, order = 1)
        )
    }

    if (!is.null(highlight)) {
        p <- p + geom_point(
            data = data[highlight, , drop = FALSE] %>% filter(!!sym(".outlier") == FALSE),
            color = highlight_color, size = highlight_size, alpha = highlight_alpha, stroke = highlight_stroke
        ) + geom_point(
            data = data[highlight, , drop = FALSE] %>% filter(!!sym(".outlier")),
            color = highlight_color, size = highlight_size, alpha = highlight_alpha, stroke = highlight_stroke, position = jitter
        )
    }

    if (!is.null(x_cutoff) && x_cutoff != 0) {
        if (identical(x_cutoff_name, "none")) {
            guide <- guide_none()
        } else {
            guide <- guide_legend(override.aes = list(alpha = 0.8, size = 5), order = 2,
                theme = ggplot2::theme(legend.margin = margin(0, 0, -10, 4.5)))
        }
        vline_df <- data.frame(xintercept = c(-x_cutoff, x_cutoff))
        if (!is.null(facet_by)) {
            vline_df <- expand_grid(vline_df, data[, facet_by, drop = FALSE] %>% distinct())
        }
        p <- p +
            new_scale_color() +
            geom_vline(
                data = vline_df,
                mapping = aes(
                    xintercept = !!sym("xintercept"),
                    color = x_cutoff_name %||% paste0(x, " = +/-", scales::number(x_cutoff))),
                alpha = 0.4, linetype = x_cutoff_linetype, size = x_cutoff_linewidth,
            ) +
            scale_color_manual(name = NULL, values = x_cutoff_color, guide = guide)
    }

    if (!is.null(y_cutoff)) {
        if (isTRUE(flip_negatives)) {
            yintercept <- c(-y_cutoff, y_cutoff)
        } else {
            yintercept <- y_cutoff
        }
        if (identical(y_cutoff_name, "none")) {
            guide <- guide_none()
        } else {
            guide <- guide_legend(override.aes = list(alpha = 0.8, size = 5), order = 3)
        }
        hline_df <- data.frame(yintercept = yintercept)
        if (!is.null(facet_by)) {
            hline_df <- expand_grid(hline_df, data[, facet_by, drop = FALSE] %>% distinct())
        }
        p <- p +
            new_scale_color() +
            geom_hline(
                data = hline_df,
                mapping = aes(yintercept = !!sym("yintercept"),
                    color = y_cutoff_name %||% paste0(ylab %||% y, " = ", scales::number(y_cutoff, accuracy = 0.01))),
                alpha = 0.4, linetype = y_cutoff_linetype, size = y_cutoff_linewidth
            ) +
            scale_color_manual(
                name = NULL,
                values = y_cutoff_color,
                guide = guide
            )
    }

    if (isTRUE(flip_negatives)) {
        p <- p +
            geom_hline(yintercept = 0, color = "black", linetype = 1) +
            scale_y_continuous(labels = abs)
    }

    p <- p +
        geom_vline(xintercept = 0, color = "grey80", linetype = 2) +
        geom_text_repel(
            data = pos_data[pos_data$.show_label, , drop = FALSE], aes(label = !!sym(".label")), nudge_x = pos_x_nudge, color = label_fg,
            bg.color = label_bg, bg.r = label_bg_r, size = label_size, min.segment.length = 0, segment.color = "grey40",
            max.overlaps = 100
        ) +
        geom_text_repel(
            data = neg_data[neg_data$.show_label, , drop = FALSE], aes(label = !!sym(".label")), nudge_x = neg_x_nudge, color = label_fg,
            bg.color = label_bg, bg.r = label_bg_r, size = label_size, min.segment.length = 0, segment.color = "grey40",
            max.overlaps = 100
        ) +
        labs(title = title, subtitle = subtitle, x = xlab %||% x, y = ylab %||% y) +
        coord_cartesian(clip = "off")

    p <- p +
        do.call(theme, theme_args) +
        ggplot2::theme(
            aspect.ratio = aspect.ratio,
            legend.position = legend.position,
            legend.direction = legend.direction
        )

    height <- 5
    width <- 5
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

#' Volcano plot
#'
#' @description A volcano plot is a type of scatter plot that shows statistical significance (usually on the y-axis) versus magnitude of change (usually on the x-axis).
#' @inheritParams common_args
#' @inheritParams VolcanoPlotAtomic
#' @return A list of ggplot objects or a wrap_plots object
#' @export
#' @examples
#' set.seed(8525)
#' # Obtained by Seurat::FindMakers for the first cluster of pbmc_small
#' data <- data.frame(
#'    avg_log2FC = c(
#'      -3.69, -4.10, -2.68, -3.51, -3.09, -2.52, -3.53, -3.35, -2.82, -2.71, -3.16, -2.24,
#'      -5.62, -3.10, -3.42, -2.72, -3.23, -3.25, -4.68, 3.67, -2.66, 4.79, -2.99, 10.14,
#'      -1.78, -2.67, -2.26, -2.59, -3.39, 5.36, 4.56, 4.62, -2.94, -9.47, -9.12, -1.63,
#'      -2.77, 3.31, -1.53, -3.89, -4.21, 4.72, -2.98, -2.29, -1.41, -9.48, -4.30, 3.01,
#'      -1.19, -4.83, -1.35, -1.68, -1.63, -2.70, 3.86, 3.81, 7.23, -1.45, -0.92, -2.45,
#'      3.91, -4.45, -9.33, 3.56, 2.27, -1.60, -1.15, 11.40, -9.77, -8.32, 2.61, -1.25,
#'      -1.72, 10.61, 11.34, 10.02, 2.78, -3.48, -1.98, 5.86, 5.57, 4.57, 9.75, 9.97,
#'      10.90, 9.19, 2.93, 5.10, -1.52, -3.93, -1.95, -2.46, -0.64, 4.60, -1.82, -0.80,
#'      9.34, 7.51, 6.45, 5.23, 4.41, 3.60, -1.94, -1.15),
#'    p_val_adj = c(
#'      3.82e-09, 1.52e-07, 1.79e-07, 4.68e-07, 4.83e-07, 6.26e-07, 2.61e-06, 1.33e-05,
#'      1.79e-05, 3.71e-05, 5.21e-05, 5.36e-05, 5.83e-05, 6.66e-05, 8.22e-05, 2.89e-04,
#'      3.00e-04, 4.94e-04, 7.62e-04, 8.93e-04, 9.55e-04, 9.61e-04, 1.12e-03, 1.47e-03,
#'      1.66e-03, 1.95e-03, 2.06e-03, 3.01e-03, 3.26e-03, 4.35e-03, 4.85e-03, 5.12e-03,
#'      5.40e-03, 7.18e-03, 7.18e-03, 1.04e-02, 1.24e-02, 1.90e-02, 1.94e-02, 1.97e-02,
#'      2.09e-02, 2.13e-02, 2.25e-02, 2.61e-02, 3.18e-02, 3.27e-02, 3.69e-02, 3.80e-02,
#'      4.95e-02, 5.73e-02, 5.77e-02, 6.10e-02, 6.22e-02, 6.31e-02, 6.72e-02, 9.23e-02,
#'      9.85e-02, 1.06e-01, 1.07e-01, 1.11e-01, 1.31e-01, 1.38e-01, 1.40e-01, 1.43e-01,
#'      2.00e-01, 2.39e-01, 2.49e-01, 2.57e-01, 2.86e-01, 2.86e-01, 2.98e-01, 3.32e-01,
#'      4.15e-01, 4.91e-01, 4.91e-01, 4.91e-01, 5.97e-01, 7.11e-01, 7.59e-01, 8.38e-01,
#'      9.20e-01, 9.20e-01, 9.29e-01, 9.29e-01, 9.29e-01, 9.29e-01, 9.34e-01, 9.68e-01,
#'      1.00e+00, 1.00e+00, 1.00e+00, 1.00e+00, 1.00e+00, 1.00e+00, 1.00e+00, 1.00e+00,
#'      1.00e+00, 1.00e+00, 1.00e+00, 1.00e+00, 1.00e+00, 1.00e+00, 1.00e+00, 1.00e+00),
#'    gene = c(
#'      "HLA-DPB1", "LYZ", "HLA-DRA", "TYMP", "HLA-DPA1", "HLA-DRB1", "CST3", "HLA-DQB1",
#'      "HLA-DRB5", "LST1", "HLA-DQA1", "AIF1", "S100A8", "IFITM3", "HLA-DMB", "FCGRT",
#'      "SERPINA1", "IFI30", "S100A9", "CCL5", "GRN", "LCK", "HLA-DMA", "MS4A6A", "CTSS",
#'      "CFP", "FCN1", "BID", "CFD", "CD3D", "CD7", "CD3E", "LGALS2", "CD14", "SMCO4",
#'      "LINC00936", "HCK", "CTSW", "LGALS1", "HLA-DQA2", "LRRC25", "GZMM", "RNF130",
#'      "LGALS3", "S100A11", "C5AR1", "IL1B", "GZMA", "FCER1G", "MPEG1", "TYROBP", "TSPO",
#'      "GSTP1", "CTSB", "IL32", "CD247", "GNLY", "COTL1", "NFKBIA", "NUP214", "LAMP1",
#'      "FPR1", "CLEC10A", "CST7", "PRF1", "BLVRA", "PSAP", "GZMH", "EAF2", "ASGR1",
#'      "RARRES3", "SAT1", "LY86", "GP9", "TUBB1", "NGFRAP1", "XBP1", "SCO2", "RGS2", "GZMB",
#'      "HIST1H2AC", "KLRD1", "PGRMC1", "AKR1C3", "PTGDR", "IL2RB", "GYPC", "CCL4", "CD68",
#'      "FCER1A", "CD79B", "MS4A7", "CARD16", "ACAP1", "CD79A", "ANXA2", "TMEM40", "PF4",
#'      "GNG11", "CLU", "CD9", "FGFBP2", "TNFRSF1B", "IFI6"),
#'   pct_diff = c(
#'      -0.752, -0.457, -0.460, -0.671, -0.626, -0.701, -0.502, -0.619, -0.623, -0.598,
#'      -0.566, -0.626, -0.543, -0.566, -0.541, -0.542, -0.515, -0.489, -0.444, 0.428,
#'      -0.517, 0.461, -0.491, -0.410, -0.480, -0.491, -0.521, -0.491, -0.438, 0.411,
#'      0.411, 0.409, -0.438, -0.359, -0.359, -0.440, -0.386, 0.385, -0.332, -0.361, -0.361,
#'      0.364, -0.387, -0.415, -0.454, -0.308, -0.335, 0.364, -0.454, -0.309, -0.379, -0.427,
#'      -0.377, -0.389, 0.335, 0.315, 0.313, -0.284, -0.502, -0.309, 0.313, -0.284, -0.256,
#'      0.309, 0.313, -0.364, -0.406, 0.244, -0.231, -0.231, 0.281, -0.311, -0.312, 0.220,
#'      0.220, 0.220, 0.261, -0.232, -0.367, 0.240, 0.218, 0.218, 0.195, 0.195, 0.195, 0.195,
#'      0.262, 0.218, -0.288, -0.207, -0.290, -0.233, -0.367, 0.217, -0.233, -0.403, 0.171,
#'      0.194, 0.194, 0.194, 0.194, 0.213, -0.235, -0.292),
#'   group = sample(LETTERS[1:2], 104, replace = TRUE)
#' )
#' rownames(data) <- data$gene
#'
#' VolcanoPlot(data, x = "avg_log2FC", y = "p_val_adj", color_by = "pct_diff",
#'    y_cutoff_name = "-log10(0.05)")
#' VolcanoPlot(data, x = "avg_log2FC", y = "p_val_adj", y_cutoff_name = "none",
#'    flip_negatives = TRUE)
#' VolcanoPlot(data, x = "avg_log2FC", y = "p_val_adj", y_cutoff_name = "none",
#'    flip_negatives = TRUE, facet_by = "group")
#' VolcanoPlot(data, x = "avg_log2FC", y = "p_val_adj", y_cutoff_name = "none",
#'    flip_negatives = TRUE, split_by = "group")
#' VolcanoPlot(data, x = "avg_log2FC", y = "p_val_adj", y_cutoff_name = "none",
#'    highlight = c("ANXA2", "TMEM40", "PF4", "GNG11", "CLU", "CD9", "FGFBP2",
#'    "TNFRSF1B", "IFI6"))
VolcanoPlot <- function(
    data, x, y, ytrans = function(n) -log10(n), color_by = NULL, color_name = NULL,
    flip_negatives = FALSE, x_cutoff = NULL, y_cutoff = 0.05, split_by = NULL, split_by_sep = "_",
    x_cutoff_name = NULL, y_cutoff_name = NULL, x_cutoff_color = "red2", y_cutoff_color = "blue2",
    x_cutoff_linetype = "dashed", y_cutoff_linetype = "dashed", x_cutoff_linewidth = 0.5, y_cutoff_linewidth = 0.5,
    pt_size = 2, pt_alpha = 0.5, nlabel = 5, labels = NULL, label_size = 3, label_fg = "black", label_bg = "white",
    label_bg_r = 0.1, highlight = NULL, highlight_color = "red", highlight_size = 2, highlight_alpha = 1,
    highlight_stroke = 0.5,
    facet_by = NULL, facet_scales = "fixed", facet_ncol = NULL, facet_nrow = NULL, facet_byrow = TRUE,
    theme = "theme_this", theme_args = list(), palette = "Spectral", palcolor = NULL,
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL,
    aspect.ratio = 1, legend.position = "right", legend.direction = "vertical", seed = 8525,
    combine = TRUE, nrow = NULL, ncol = NULL, byrow = TRUE, ...
) {
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
            VolcanoPlotAtomic(datas[[nm]],
                x = x, y = y, ytrans = ytrans, color_by = color_by, color_name = color_name,
                flip_negatives = flip_negatives, x_cutoff = x_cutoff, y_cutoff = y_cutoff,
                x_cutoff_name = x_cutoff_name, y_cutoff_name = y_cutoff_name, x_cutoff_color = x_cutoff_color, y_cutoff_color = y_cutoff_color,
                x_cutoff_linetype = x_cutoff_linetype, y_cutoff_linetype = y_cutoff_linetype, x_cutoff_linewidth = x_cutoff_linewidth, y_cutoff_linewidth = y_cutoff_linewidth,
                pt_size = pt_size, pt_alpha = pt_alpha, nlabel = nlabel, labels = labels, label_size = label_size, label_fg = label_fg, label_bg = label_bg,
                label_bg_r = label_bg_r, highlight = highlight, highlight_color = highlight_color, highlight_size = highlight_size, highlight_alpha = highlight_alpha,
                highlight_stroke = highlight_stroke,
                facet_by = facet_by, facet_scales = facet_scales, facet_ncol = facet_ncol, facet_nrow = facet_nrow, facet_byrow = facet_byrow,
                theme = theme, theme_args = theme_args, palette = palette, palcolor = palcolor,
                title = title, subtitle = subtitle, xlab = xlab, ylab = ylab,
                aspect.ratio = aspect.ratio, legend.position = legend.position, legend.direction = legend.direction, seed = seed, ...
            )
        }
    )

    combine_plots(plots, combine, nrow, ncol, byrow)
}
