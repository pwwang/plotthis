#' Atomic Dimension Reduction Plot without splitting the data
#'
#' @inheritParams common_args
#' @param dims A character vector of the column names to plot on the x and y axes or a numeric vector of the column indices.
#' @param features A character vector of the column names to plot as features.
#' @param lower_quantile,upper_quantile,lower_cutoff,upper_cutoff Vector of minimum and maximum cutoff values or quantile values for each feature.
#' @param group_by A character string of the column name to group the data.
#'  A character/factor column is expected. If multiple columns are provided, the columns will be concatenated with `group_by_sep`.
#' @param group_by_sep A character string to concatenate the columns in `group_by`, if multiple columns are provided.
#' @param pt_size A numeric value of the point size. If NULL, the point size will be calculated based on the number of data points.
#' @param pt_alpha A numeric value of the point transparency. Default is 1.
#' @param bg_color A character string of the background or NA points. Default is "grey80".
#' @param label_insitu Whether to place the raw labels (group names) in the center of the points with the corresponding group. Default is FALSE, which using numbers instead of raw labels.
#' @param show_stat Whether to show the number of points in the subtitle. Default is TRUE.
#' @param label Whether to show the labels of groups. Default is FALSE.
#' @param label_size A numeric value of the label size. Default is 4.
#' @param label_fg A character string of the label foreground color. Default is "white".
#' @param label_bg A character string of the label background color. Default is "black".
#' @param label_bg_r A numeric value of the background ratio of the labels. Default is 0.1.
#' @param label_repel Whether to repel the labels. Default is FALSE.
#' @param label_repulsion A numeric value of the label repulsion. Default is 20.
#' @param label_pt_size A numeric value of the label point size. Default is 1.
#' @param label_pt_color A character string of the label point color. Default is "black".
#' @param label_segment_color A character string of the label segment color. Default is "black".
#' @param highlight A character vector of the row names to highlight. Default is NULL.
#' @param highlight_alpha A numeric value of the highlight transparency. Default is 1.
#' @param highlight_size A numeric value of the highlight size. Default is 1.
#' @param highlight_color A character string of the highlight color. Default is "black".
#' @param highlight_stroke A numeric value of the highlight stroke. Default is 0.5.
#' @param add_mark Whether to add mark to the plot. Default is FALSE.
#' @param mark_type A character string of the mark type. Default is "hull".
#' @param mark_expand A unit value of the mark expand. Default is 3mm.
#' @param mark_alpha A numeric value of the mark transparency. Default is 0.1.
#' @param mark_linetype A numeric value of the mark line type. Default is 1.
#' @param stat_by A character string of the column name to calculate the statistics. Default is NULL.
#' @param stat_plot_type A character string of the statistic plot type. Default is "pie".
#' @param stat_plot_size A numeric value of the statistic plot size. Default is 0.1.
#' @param stat_args A list of additional arguments to the statistic plot. Default is list(palette = "Set1").
#' @param graph A character string of column names or the indexes in the data for the graph data. Default is NULL.
#'   If "@graph" is provided, the graph data will be extracted from the data attribute 'graph'.
#' @param edge_size A numeric vector of the edge size range. Default is c(0.05, 0.5).
#' @param edge_alpha A numeric value of the edge transparency. Default is 0.1.
#' @param edge_color A character string of the edge color. Default is "grey40".
#' @param add_density Whether to add density plot. Default is FALSE.
#' @param density_color A character string of the density color. Default is "grey80".
#' @param density_filled Whether to fill the density plot. Default is FALSE.
#' @param density_filled_palette A character string of the filled density palette. Default is "Greys".
#' @param density_filled_palcolor A character vector of the filled density palette colors. Default is NULL.
#' @param lineages A character vector of the column names for lineages. Default is NULL.
#' @param lineages_trim A numeric vector of the trim range for lineages. Default is c(0.01, 0.99).
#' @param lineages_span A numeric value of the lineages span. Default is 0.75.
#' @param lineages_palette A character string of the lineages palette. Default is "Dark2".
#' @param lineages_palcolor A character vector of the lineages palette colors. Default is NULL.
#' @param lineages_arrow An arrow object for the lineages. Default is arrow(length = unit(0.1, "inches")).
#' @param lineages_linewidth A numeric value of the lineages line width. Default is 1.
#' @param lineages_line_bg A character string of the lineages line background color. Default is "white".
#' @param lineages_line_bg_stroke A numeric value of the lineages line background stroke. Default is 0.5.
#' @param lineages_whiskers Whether to add whiskers to the lineages. Default is FALSE.
#' @param lineages_whiskers_linewidth A numeric value of the lineages whiskers line width. Default is 0.5.
#' @param lineages_whiskers_alpha A numeric value of the lineages whiskers transparency. Default is 0.5.
#' @param raster Whether to raster the plot. Default is NULL.
#' @param raster_dpi A numeric vector of the raster dpi. Default is c(512, 512).
#' @param hex Whether to use hex plot. Default is FALSE.
#' @param hex_linewidth A numeric value of the hex line width. Default is 0.5.
#' @param hex_count Whether to count the hex.
#' @param hex_bins A numeric value of the hex bins. Default is 50.
#' @param hex_binwidth A numeric value of the hex bin width. Default is NULL.
#' @param bg_cutoff A numeric value to be used a cutoff to set the feature values to NA. Default is NULL.
#' @param color_name A character string of the color legend name. Default is "".
#' @return A ggplot object
#' @keywords internal
#' @importFrom scales rescale
#' @importFrom stats loess formula na.omit aggregate
#' @importFrom dplyr %>% group_by group_map pull filter
#' @importFrom tidyr pivot_longer
#' @importFrom ggplot2 scale_linewidth_continuous geom_density_2d stat_density_2d geom_hex
#' @importFrom ggplot2 ggplotGrob theme_void as_labeller stat_summary_hex
#' @importFrom cowplot get_plot_component
DimPlotAtomic <- function(
    data, dims = 1:2, group_by = NULL, group_by_sep = "_", features = NULL,
    lower_quantile = 0, upper_quantile = 0.99, lower_cutoff = NULL, upper_cutoff = NULL,
    pt_size = NULL, pt_alpha = 1, bg_color = "grey80", bg_cutoff = NULL, color_name = "",
    label_insitu = FALSE, show_stat = !identical(theme, "theme_blank"),
    label = FALSE, label_size = 4, label_fg = "white", label_bg = "black", label_bg_r = 0.1,
    label_repel = FALSE, label_repulsion = 20, label_pt_size = 1, label_pt_color = "black",
    label_segment_color = "black",
    highlight = NULL, highlight_alpha = 1, highlight_size = 1, highlight_color = "black", highlight_stroke = 0.8,
    add_mark = FALSE, mark_type = c("hull", "ellipse", "rect", "circle"), mark_expand = unit(3, "mm"),
    mark_alpha = 0.1, mark_linetype = 1,
    stat_by = NULL, stat_plot_type = c("pie", "ring", "bar", "line"), stat_plot_size = 0.1,
    stat_palette = "Set1", stat_args = list(),
    graph = NULL, edge_size = c(0.05, 0.5), edge_alpha = 0.1, edge_color = "grey40",
    add_density = FALSE, density_color = "grey80", density_filled = FALSE,
    density_filled_palette = "Greys", density_filled_palcolor = NULL,
    lineages = NULL, lineages_trim = c(0.01, 0.99), lineages_span = 0.75,
    lineages_palette = "Dark2", lineages_palcolor = NULL, lineages_arrow = ggplot2::arrow(length = unit(0.1, "inches")),
    lineages_linewidth = 1, lineages_line_bg = "white", lineages_line_bg_stroke = 0.5,
    lineages_whiskers = FALSE, lineages_whiskers_linewidth = 0.5, lineages_whiskers_alpha = 0.5,
    facet_by = NULL, facet_scales = "fixed", facet_nrow = NULL, facet_ncol = NULL, facet_byrow = TRUE,
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL,
    theme = "theme_this", theme_args = list(), aspect.ratio = 1, legend.position = "right", legend.direction = "vertical",
    raster = NULL, raster_dpi = c(512, 512),
    hex = FALSE, hex_linewidth = 0.5, hex_count = !is.null(group_by), hex_bins = 50, hex_binwidth = NULL,
    palette = ifelse(is.null(features), "Paired", "Spectral"), palcolor = NULL, seed = 8525, ...) {
    ggplot <- if (getOption("plotthis.gglogger.enabled", FALSE)) {
        gglogger::ggplot
    } else {
        ggplot2::ggplot
    }
    ## Setting up the parameters
    if (is.numeric(dims)) {
        dims <- colnames(data)[dims]
    }
    if (length(dims) != 2) {
        stop("Only 2 dimensions are allowed for dimension reduction plot.")
    }
    dims <- check_columns(data, dims, allow_multi = TRUE)
    if (length(raster_dpi) == 1) {
        raster_dpi <- rep(raster_dpi, 2)
    }

    group_by <- check_columns(data, group_by,
        force_factor = TRUE,
        allow_multi = TRUE, concat_multi = TRUE, concat_sep = group_by_sep
    )
    features <- check_columns(data, features, allow_multi = TRUE)
    if (is.null(group_by) && is.null(features)) {
        stop("Either 'group_by' or 'features' should be specified.")
    }
    facet_by <- check_columns(data, facet_by, force_factor = TRUE)
    if (length(features) > 1 && !is.null(facet_by)) {
        stop("Cannot specify 'facet_by' with multiple features. The plot will be faceted by features.")
    }
    pt_size <- pt_size %||% min(3000 / nrow(data), 0.6)
    raster <- raster %||% (nrow(data) > 1e5)
    xlab <- xlab %||% dims[1]
    ylab <- ylab %||% dims[2]
    if (identical(theme, "theme_blank")) {
        theme_args[["xlab"]] <- xlab
        theme_args[["ylab"]] <- ylab
    }
    if (isTRUE(label_repel) && !isTRUE(label)) {
        message("Forcing label to be TRUE when label_repel is TRUE.")
        label <- TRUE
    }

    facet_labeller <- "label_value"
    if (!is.null(group_by)) {
        colors <- palette_this(levels(data[[group_by]]), palette = palette, palcolor = palcolor, NA_keep = TRUE)
        labels_tb <- table(data[[group_by]])
        labels_tb <- labels_tb[labels_tb != 0]
        if (isTRUE(label_insitu)) {
            if (isTRUE(show_stat)) {
                label_use <- paste0(names(labels_tb), "(", labels_tb, ")")
            } else {
                label_use <- paste0(names(labels_tb))
            }
        } else {
            if (isTRUE(label)) {
                if (isTRUE(show_stat)) {
                    label_use <- paste0(seq_along(labels_tb), ": ", names(labels_tb), "(", labels_tb, ")")
                } else {
                    label_use <- paste0(seq_along(labels_tb), ": ", names(labels_tb))
                }
            } else {
                if (isTRUE(show_stat)) {
                    label_use <- paste0(names(labels_tb), "(", labels_tb, ")")
                } else {
                    label_use <- paste0(names(labels_tb))
                }
            }
        }
        if (isTRUE(show_stat)) {
            if (is.null(facet_by)) {
                subtitle <- subtitle %||% paste0("N = ", sum(!is.na(data[[group_by]])))
            } else if (length(facet_by) == 1) {
                facet_labeller <- as_labeller(function(values) {
                    sapply(values, function(v) {
                        data_sub <- data[data[[facet_by]] == v, , drop = FALSE]
                        paste0(v, " (N = ", sum(!is.na(data_sub[[group_by]])), ")")
                    })
                })
            }
        }
    }

    ## Making long data for features when length(features) > 1
    multifeats <- length(features) > 1
    if (multifeats) {
        data <- data %>% pivot_longer(cols = features, names_to = ".feature", values_to = ".value")
        data$.feature <- factor(data$.feature, levels = features)
        facet_by <- ".feature"
        features <- ".value"
    }
    if (!is.null(features)) {
        if (!is.null(bg_cutoff)) {
            data[[features]][data[[features]] <= bg_cutoff] <- NA
        }
        if (all(is.na(data[[features]]))) {
            feat_colors_value <- rep(0, 100)
        } else {
            lower_cutoff <- lower_cutoff %||% quantile(data[[features]][is.finite(data[[features]])], lower_quantile, na.rm = TRUE)
            upper_cutoff <- upper_cutoff %||% quantile(data[[features]][is.finite(data[[features]])], upper_quantile, na.rm = TRUE)
            if (upper_cutoff == lower_cutoff) {
                if (upper_cutoff == 0) {
                    upper_cutoff <- 1e-3
                } else {
                    upper_cutoff <- upper_cutoff + upper_cutoff * 1e-3
                }
            }

            feat_colors_value <- seq(lower_cutoff, upper_cutoff, length.out = 100)
        }
        data[[features]][data[[features]] > max(feat_colors_value, na.rm = TRUE)] <- max(feat_colors_value, na.rm = TRUE)
        data[[features]][data[[features]] < min(feat_colors_value, na.rm = TRUE)] <- min(feat_colors_value, na.rm = TRUE)
    }
    colorby <- ifelse(is.null(features), group_by, features)

    # Do we have fill scale?
    has_fill <- FALSE
    p <- ggplot(data)

    ## Adding the mark
    if (isTRUE(add_mark)) {
        if (is.null(group_by)) {
            stop("Cannot add mark without 'group_by'.")
        }
        # if (!requireNamespace("ggforce", quietly = TRUE)) {
        #     stop("'ggforce' package is required for adding mark to the plot.")
        # }
        mark_type <- match.arg(mark_type)
        mark_fun <- switch(mark_type,
            hull = ggforce::geom_mark_hull,
            ellipse = ggforce::geom_mark_ellipse,
            rect = ggforce::geom_mark_rect,
            circle = ggforce::geom_mark_circle
        )
        p <- p + mark_fun(
            data = data[!is.na(data[[group_by]]), , drop = FALSE],
            mapping = aes(x = !!sym(dims[1]), y = !!sym(dims[2]), color = !!sym(group_by), fill = !!sym(group_by)),
            expand = mark_expand, alpha = mark_alpha, linetype = mark_linetype,
            show.legend = FALSE
        ) +
            scale_fill_manual(values = colors[names(labels_tb)]) +
            scale_color_manual(values = colors[names(labels_tb)]) +
            new_scale_fill() +
            new_scale_color()
    }

    ## Adding the graph/network
    if (!is.null(graph)) {
        if (is.character(graph) && length(graph) == 1 && startsWith(graph, "@")) {
            graph <- substring(graph, 2)
            net_mat <- attr(data, graph)
        } else if (inherits(graph, "Graph")) {  # SeuratObject Graph
            net_mat <- as.matrix(graph)
        } else if (is.matrix(graph) || is.data.frame(graph)) {
            net_mat <- graph
        } else if (is.numeric(graph)) {
            graph <- colnames(data)[graph]
            net_mat <- data[graph]
        } else if (is.character(graph)) {
            net_mat <- data[graph]
        } else {
            stop("The 'graph' should be a matrix, data.frame, Graph object, indexes, or column names.")
        }

        if (!is.matrix(net_mat)) {
            net_mat <- as.matrix(net_mat)
        }

        if (!is.null(rownames(net_mat)) && !is.null(colnames(net_mat))) {
            net_mat <- net_mat[rownames(data), rownames(data)]
        } else if (is.null(rownames(net_mat))) {
            stop("The graph matrix should have rownames and colnames.")
        } else if (is.null(rownames(data))) {
            stop("'graph' requires the input data to have rownames.")
        }
        net_mat[net_mat == 0] <- NA
        net_mat[upper.tri(net_mat)] <- NA

        handle_single_facet_value <- function(mat) {
            net_df <- reshape2::melt(mat, na.rm = TRUE, stringsAsFactors = FALSE)
            net_df$value <- as.numeric(net_df$value)
            net_df$Var1 <- as.character(net_df$Var1)
            net_df$Var2 <- as.character(net_df$Var2)
            net_df$x <- data[net_df$Var1, dims[1]]
            net_df$y <- data[net_df$Var1, dims[2]]
            net_df$xend <- data[net_df$Var2, dims[1]]
            net_df$yend <- data[net_df$Var2, dims[2]]
            return(net_df)
        }

        if (!is.null(facet_by)) {
            net_df <- do.call(rbind, lapply(split(data, data[, facet_by]), function(d) {
                d <- handle_single_facet_value(net_mat[rownames(d), rownames(d)])
                d[, facet_by] <- d[1, facet_by]
                d
            }))
        } else {
            net_df <- handle_single_facet_value(net_mat)
        }

        p <- p + geom_segment(
            data = net_df, mapping = aes(x = !!sym("x"), y = !!sym("y"), xend = !!sym("xend"),
                yend = !!sym("yend"), linewidth = !!sym("value")),
            color = edge_color, alpha = edge_alpha, show.legend = FALSE
        ) + scale_linewidth_continuous(range = edge_size)
    }

    ## Adding the density plot
    if (isTRUE(add_density)) {
        if (isTRUE(density_filled)) {
            filled_color <- palette_this(palette = density_filled_palette, palcolor = density_filled_palcolor)
            p <- p +
                stat_density_2d(
                    geom = "raster", aes(x = !!sym(dims[1]), y = !!sym(dims[2]), fill = after_stat(!!sym("density"))),
                    contour = FALSE, inherit.aes = FALSE, show.legend = FALSE
                ) +
                scale_fill_gradientn(name = "Density", colours = filled_color) +
                new_scale_fill()
        } else {
            p <- p + geom_density_2d(
                aes(x = !!sym(dims[1]), y = !!sym(dims[2])),
                color = density_color, inherit.aes = FALSE, show.legend = FALSE
            )
        }
    }

    ## Setting up the theme
    x_min <- min(data[[dims[1]]], na.rm = TRUE)
    x_max <- max(data[[dims[1]]], na.rm = TRUE)
    y_min <- min(data[[dims[2]]], na.rm = TRUE)
    y_max <- max(data[[dims[2]]], na.rm = TRUE)
    p <- p +
        labs(title = title, subtitle = subtitle, x = xlab, y = ylab) +
        scale_x_continuous(limits = c(x_min, x_max)) +
        scale_y_continuous(limits = c(y_min, y_max)) +
        do.call(theme, theme_args) +
        ggplot2::theme(
            aspect.ratio = aspect.ratio,
            legend.position = legend.position,
            legend.direction = legend.direction
        )

    ## Adding the background points for each facet
    if (!is.null(facet_by) && isFALSE(multifeats)) {
        # Add the points from other subplots as background
        all_fc_values <- distinct(data, !!!syms(facet_by))
        colnames(all_fc_values) <- paste0(".facet_", colnames(all_fc_values))
        fc_data <- expand_grid(data, all_fc_values)
        # Remove the points inside the facet
        fc_indicator <- TRUE
        for (fc in facet_by) {
            fc_indicator <- fc_indicator & (fc_data[[fc]] == fc_data[[paste0(".facet_", fc)]])
        }
        fc_data <- fc_data[!fc_indicator, , drop = FALSE]
        fc_data[, facet_by, drop = FALSE] <- fc_data[, paste0(".facet_", facet_by), drop = FALSE]

        if (isTRUE(raster)) {
            p <- p + scattermore::geom_scattermore(
                data = fc_data, mapping = aes(x = !!sym(dims[1]), y = !!sym(dims[2])),
                size = pt_size, alpha = pt_alpha / 2, color = bg_color, pixels = raster_dpi
            )
        } else if (isTRUE(hex)) {
            p <- p + geom_hex(
                data = fc_data, mapping = aes(x = !!sym(dims[1]), y = !!sym(dims[2])),
                linewidth = hex_linewidth, bins = hex_bins, binwidth = hex_binwidth,
                fill = bg_color, alpha = pt_alpha / 2
            )
        } else {
            p <- p + geom_point(
                data = fc_data, mapping = aes(x = !!sym(dims[1]), y = !!sym(dims[2])),
                size = pt_size, alpha = pt_alpha / 2, color = bg_color
            )
        }
        rm(fc_data)
    }

    ## Raserting the plot/Adding the points
    if (isTRUE(raster)) {
        # if (!requireNamespace("scattermore", quietly = TRUE)) {
        #     stop("'scattermore' package is required to raster the plot.")
        # }
        if (!is.null(group_by)) {
            p <- p + scattermore::geom_scattermore(
                data = data[is.na(data[[group_by]]), , drop = FALSE],
                mapping = aes(x = !!sym(dims[1]), y = !!sym(dims[2])), color = bg_color,
                pointsize = ceiling(pt_size), alpha = pt_alpha, pixels = raster_dpi
            ) + scattermore::geom_scattermore(
                data = data[!is.na(data[[group_by]]), , drop = FALSE],
                mapping = aes(x = !!sym(dims[1]), y = !!sym(dims[2]), color = !!sym(group_by)),
                pointsize = ceiling(pt_size), alpha = pt_alpha, pixels = raster_dpi
            )
        } else {  # features
            p <- p + scattermore::geom_scattermore(
                mapping = aes(x = !!sym(dims[1]), y = !!sym(dims[2])), color = bg_color,
                pointsize = ceiling(pt_size), alpha = pt_alpha, pixels = raster_dpi
            ) + scattermore::geom_scattermore(
                mapping = aes(x = !!sym(dims[1]), y = !!sym(dims[2]), color = !!sym(colorby)),
                pointsize = ceiling(pt_size), alpha = pt_alpha, pixels = raster_dpi
            )
        }
    } else if (isTRUE(hex)) {
        # if (!requireNamespace("hexbin", quietly = TRUE)) {
        #     stop("'hexbin' package is required to add hexgons the plot.")
        # }
        has_fill <- TRUE
        if (isTRUE(hex_count)) {
            if (!is.null(features)) {
                stop("Don't know how to count for the hex when 'group_by' is not provided.")
            }
            p <- p + geom_hex(
                mapping = aes(x = !!sym(dims[1]), y = !!sym(dims[2]), color = !!sym(group_by), fill = !!sym(group_by),
                    alpha = after_stat(!!sym("count"))),
                linewidth = hex_linewidth, bins = hex_bins, binwidth = hex_binwidth
            )
        } else if (!is.null(group_by)) {
            p <- p + geom_hex(
                mapping = aes(x = !!sym(dims[1]), y = !!sym(dims[2]), color = !!sym(group_by), fill = !!sym(group_by)),
                linewidth = hex_linewidth, bins = hex_bins, binwidth = hex_binwidth
            )
        } else {  # features
            data_na <- data[is.na(data[[features]]), , drop = FALSE]
            if (nrow(data_na) > 0) {
                p <- p + geom_hex(
                    data = data_na, mapping = aes(x = !!sym(dims[1]), y = !!sym(dims[2])),
                    fill = bg_color, linewidth = hex_linewidth, bins = hex_bins,
                    binwidth = hex_binwidth, alpha = pt_alpha / 2
                )
            }
            p <- p + stat_summary_hex(
                data = data[!is.na(data[[features]]), , drop = FALSE],
                mapping = aes(x = !!sym(dims[1]), y = !!sym(dims[2]), z = !!sym(colorby)),
                linewidth = hex_linewidth, bins = hex_bins, binwidth = hex_binwidth, alpha = pt_alpha
            )
        }
    } else {
        p <- p + geom_point(
            mapping = aes(x = !!sym(dims[1]), y = !!sym(dims[2]), color = !!sym(colorby)),
            size = pt_size, alpha = pt_alpha
        )
    }

    ## Adding the highlight
    if (!is.null(highlight)) {
        if (isTRUE(hex)) {
            stop("Highlight is not supported for hex plot.")
        }
        if (isTRUE(highlight)) {
            hi_df <- data
        } else if (length(highlight) == 1 && is.character(highlight)) {
            hi_df <- eval(parse(text = paste0('filter(data, ', highlight, ')')))
        } else {
            all_inst <- rownames(data) %||% 1:nrow(data)
            if (!any(highlight %in% all_inst)) {
                stop("No highlight items found in the data (rownames).")
            }
            if (!all(highlight %in% all_inst)) {
                warning("Not all highlight items found in the data (rownames).", immediate. = TRUE)
            }
            hi_df <- data[intersect(highlight, all_inst), , drop = FALSE]
            rm(all_inst)
        }
        if (nrow(hi_df) > 0) {
            if (isTRUE(raster)) {
                p <- p + scattermore::geom_scattermore(
                    data = hi_df, aes(x = !!sym(dims[1]), y = !!sym(dims[2])), color = highlight_color,
                    pointsize = floor(highlight_size) + highlight_stroke, alpha = highlight_alpha, pixels = raster_dpi
                ) +
                    scattermore::geom_scattermore(
                        data = hi_df, aes(x = !!sym(dims[1]), y = !!sym(dims[2]), color = !!sym(colorby)),
                        pointsize = floor(highlight_size), alpha = highlight_alpha, pixels = raster_dpi
                    )
            } else {
                p <- p + geom_point(
                    data = hi_df, aes(x = !!sym(dims[1]), y = !!sym(dims[2])), color = highlight_color,
                    size = highlight_size + highlight_stroke, alpha = highlight_alpha
                ) +
                    geom_point(
                        data = hi_df, aes(x = !!sym(dims[1]), y = !!sym(dims[2]), color = !!sym(colorby)),
                        size = highlight_size, alpha = highlight_alpha
                    )
            }
        }
    }

    if (!is.null(group_by)) {
        p <- p + scale_color_manual(
            values = colors[names(labels_tb)],
            labels = label_use,
            na.value = bg_color,
            guide = guide_legend(
                title.hjust = 0,
                order = 1,
                override.aes = list(size = 4, alpha = 1)
            )
        )
        if (has_fill) {
            p <- p + scale_fill_manual(
                values = colors[names(labels_tb)],
                labels = label_use,
                na.value = bg_color,
                guide = guide_legend(
                    title.hjust = 0,
                    order = 1
                )
            )
        }
    } else {  # features
        p <- p + scale_color_gradientn(
            name = color_name,
            colors = palette_this(palette = palette, palcolor = palcolor, type = "continuous"),
            values = rescale(feat_colors_value),
            limits = range(feat_colors_value),
            na.value = bg_color,
            guide = guide_colorbar(frame.colour = "black", ticks.colour = "black", title.hjust = 0)
        )
        if (has_fill) {
            p <- p + scale_fill_gradientn(
                name = color_name,
                colors = palette_this(palette = palette, palcolor = palcolor, type = "continuous"),
                values = rescale(feat_colors_value),
                limits = range(feat_colors_value),
                na.value = bg_color,
                guide = guide_colorbar(frame.colour = "black", ticks.colour = "black", title.hjust = 0)
            )
        }
    }

    legend_base <- get_plot_component(
        p + theme_this(legend.position = "bottom", legend.direction = legend.direction),
        "guide-box-bottom"
    )

    legend_list <- list()
    ## Adding the lineages
    if (!is.null(lineages)) {
        if (!is.null(facet_by)) {
            stop("'lineages' is not supported when 'facet_by' is not NULL.")
        }
        lineages <- unique(check_columns(data, lineages, force_factor = FALSE, allow_multi = TRUE))
        lineage_colors <- palette_this(lineages, palette = lineages_palette, palcolor = lineages_palcolor)
        lineage_layers <- lapply(lineages, function(l) {
            trim_pass <- (
                data[[l]] > quantile(data[[l]], lineages_trim[1], na.rm = TRUE) &
                    data[[l]] < quantile(data[[l]], lineages_trim[2], na.rm = TRUE))
            na_pass <- !is.na(data[[l]])
            index <- which(trim_pass & na_pass)
            index <- index[order(data[index, l])]
            dat_sub <- data[index, , drop = FALSE]
            # if (is.null(weights)) {
            weights_used <- rep(1, nrow(dat_sub))
            # } else {
            # weights_used <- dat_sub[[weights]]
            # }
            fitted <- lapply(dims, function(x) {
                loess(formula(paste(x, l, sep = "~")), weights = weights_used, data = dat_sub, span = lineages_span, degree = 2)$fitted
            })
            names(fitted) <- dims
            fitted[["index"]] <- index

            dat_smooth <- as.data.frame(fitted)
            colnames(dat_smooth) <- c(paste0("Axis_", 1:(ncol(dat_smooth) - 1)), "index")
            dat_smooth[, "Lineages"] <- factor(l, levels = lineages)
            dat_smooth <- unique(na.omit(dat_smooth))
            curve <- list()
            if (isTRUE(lineages_whiskers)) {
                dat_smooth[, "raw_Axis_1"] <- data[dat_smooth[, "index"], dims[1]]
                dat_smooth[, "raw_Axis_2"] <- data[dat_smooth[, "index"], dims[2]]
                curve <- c(curve, geom_segment(
                    data = dat_smooth,
                    mapping = aes(x = !!sym("Axis_1"), y = !!sym("Axis_2"),
                        xend = !!sym("raw_Axis_1"), yend = !!sym("raw_Axis_2"), color = !!sym("Lineages")),
                    linewidth = lineages_whiskers_linewidth, alpha = lineages_whiskers_alpha,
                    show.legend = TRUE, inherit.aes = FALSE
                ))
            }
            curve <- c(
                curve,
                geom_path(
                    data = dat_smooth, mapping = aes(x = !!sym("Axis_1"), y = !!sym("Axis_2")), color = lineages_line_bg,
                    linewidth = lineages_linewidth + lineages_line_bg_stroke, arrow = lineages_arrow,
                    show.legend = TRUE, inherit.aes = FALSE
                ),
                geom_path(
                    data = dat_smooth, mapping = aes(x = !!sym("Axis_1"), y = !!sym("Axis_2"), color = !!sym("Lineages")),
                    linewidth = lineages_linewidth, arrow = lineages_arrow,
                    show.legend = TRUE, inherit.aes = FALSE
                )
            )
            return(curve)
        })
        lineage_layers[[length(lineage_layers) + 1]] <- scale_color_manual(values = lineage_colors)

        suppressMessages({
            legend_list$lineages <- get_plot_component(
                ggplot() +
                    lineage_layers +
                    theme_this(
                        legend.position = "bottom",
                        legend.direction = legend.direction
                    ),
                "guide-box-bottom"
            )
        })

        p <- suppressWarnings({
            p + new_scale_color() + lineage_layers + ggplot2::theme(legend.position = "none")
        })
        if (is.null(legend_list[["lineages"]])) {
            legend_list["lineages"] <- list(NULL)
        }
    }

    ## Adding the statistic plots
    if (!is.null(stat_by)) {
        if (!is.null(facet_by)) {
            stop("'stat_by' is not supported when 'facet_by' is not NULL.")
        }
        if (!is.null(features)) {
            stop("'stat_by' is not supported without 'group_by'.")
        }
        stat_by <- check_columns(data, stat_by, force_factor = TRUE)
        stat_plot_type <- match.arg(stat_plot_type)
        stat_args$data <- data
        stat_args$split_by <- group_by
        stat_args$legend.position <- "bottom"
        stat_args$legend.direction <- legend.direction
        stat_args$combine <- FALSE
        stat_args$theme <- theme
        stat_args$theme_args <- theme_args
        stat_args$title <- character(0)
        stat_args$keep_empty <- TRUE
        stat_args$palette <- stat_palette
        if (stat_plot_type == "pie") {
            stat_args$x <- stat_by
            stat_plots <- do.call(PieChart, stat_args)
        } else if (stat_plot_type == "ring") {
            stat_args$group_by <- stat_by
            stat_plots <- do.call(RingPlot, stat_args)
        } else if (stat_plot_type == "bar") {
            stat_args$x <- stat_by
            stat_plots <- do.call(BarPlot, stat_args)
        } else if (stat_plot_type == "line") {
            stat_args$x <- stat_by
            stat_plots <- do.call(LinePlot, stat_args)
        }

        coord_df <- aggregate(p$data[, dims], by = list(p$data[[group_by]]), FUN = median)
        colnames(coord_df)[1] <- group_by
        x_range <- diff(layer_scales(p)$x$range$range)
        y_range <- diff(layer_scales(p)$y$range$range)

        stat_plot_list <- list()
        for (i in seq_len(nrow(coord_df))) {
            stat_plot_list[[i]] <- annotation_custom(
                ggplotGrob(stat_plots[[coord_df[i, group_by]]] + theme_void() + ggplot2::theme(legend.position = "none")),
                xmin = coord_df[i, dims[1]] - x_range * stat_plot_size / 2, ymin = coord_df[i, dims[2]] - y_range * stat_plot_size / 2,
                xmax = coord_df[i, dims[1]] + x_range * stat_plot_size / 2, ymax = coord_df[i, dims[2]] + y_range * stat_plot_size / 2
            )
        }
        p <- p + stat_plot_list
        legend_list$stat_by <- get_plot_component(
            stat_plots[[coord_df[i, group_by]]] + ggplot2::theme(legend.position = "bottom"),
            "guide-box-bottom"
        )
    }

    ## Adding the labels
    if (isTRUE(label)) {
        if (!is.null(features)) {
            stop("Adding labels is not supported when 'features' is specified.")
        }
        if (!is.null(facet_by)) {
            label_df <- aggregate(data[, dims], by = list(data[[group_by]], data[[facet_by]]), FUN = median)
            colnames(label_df)[2:length(facet_by) + 1] <- facet_by
        } else {
            label_df <- aggregate(data[, dims], by = list(data[[group_by]]), FUN = median)
        }
        colnames(label_df)[1] <- ".label"
        label_df <- label_df[!is.na(label_df[, ".label"]), , drop = FALSE]
        if (!isTRUE(label_insitu)) {
            label_df[, ".label"] <- seq_len(nrow(label_df))
        }
        if (isTRUE(label_repel)) {
            p <- p + geom_point(
                data = label_df, mapping = aes(x = !!sym(dims[1]), y = !!sym(dims[2])),
                color = label_pt_color, size = label_pt_size
            ) + geom_text_repel(
                data = label_df, aes(x = !!sym(dims[1]), y = !!sym(dims[2]), label = !!sym(".label")),
                point.size = label_pt_size, max.overlaps = 100, force = label_repulsion,
                color = label_fg, bg.color = label_bg, bg.r = label_bg_r, size = label_size, inherit.aes = FALSE
            )
        } else {
            p <- p + geom_text_repel(
                data = label_df, aes(x = !!sym(dims[1]), y = !!sym(dims[2]), label = !!sym(".label")),
                fontface = "bold", min.segment.length = 0, segment.color = label_segment_color,
                point.size = NA, max.overlaps = 100, force = 0,
                color = label_fg, bg.color = label_bg, bg.r = label_bg_r, size = label_size, inherit.aes = FALSE
            )
        }
    }

    ## Posing all legends
    if (length(legend_list) > 0) {
        legend_list <- legend_list[!sapply(legend_list, is.null)]
        if (legend.direction == "vertical") {
            legend <- do.call(cbind, c(list(base = legend_base), legend_list))
        } else {
            legend <- do.call(rbind, c(list(base = legend_base), legend_list))
        }
        gtable <- ggplotGrob(p + ggplot2::theme(legend.position = "none"))
        gtable <- add_grob(gtable, legend, legend.position)
        p <- wrap_plots(gtable)
    }

    ## Putting reasonable height and width
    height <- width <- 5.5
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

    if (length(legend_list) == 0) {
        p <- facet_plot(p, facet_by, facet_scales, facet_nrow, facet_ncol, facet_byrow,
                        labeller = facet_labeller, legend.position = legend.position,
                        legend.direction = legend.direction)
    }
    p
}

#' DimPLot / FeatureDimPlot
#'
#' @description
#'  Visualizing the dimension reduction data.
#'  `FeatureDimPlot` is used to plot the feature numeric values on the dimension reduction plot.
#'
#' @rdname dimplot
#' @inheritParams common_args
#' @inheritParams DimPlotAtomic
#' @return A ggplot object or wrap_plots object or a list of ggplot objects
#' @export
#' @examples
#' \donttest{
#' # generate a PCA dimension data for DimPlot
#' set.seed(8525)
#' df <- matrix(c(rnorm(333), rnorm(334, 0.2), rnorm(333, .4)), ncol = 10)
#' # run PCA
#' pca <- prcomp(df)
#' # get coordinates
#' data <- pca$x[, 1:2]
#' # kmeans clustering
#' km <- kmeans(data, 3)
#' data <- as.data.frame(data)
#' data$cluster <- factor(paste0("C", km$cluster))
#' data$group <- sample(c("A", "B"), nrow(data), replace = TRUE)
#'
#' graph <- rnorm(nrow(data) * nrow(data))
#' graph[sample(1:(nrow(data) * nrow(data)), 5000)] <- NA
#' graph <- matrix(graph, nrow = nrow(data))
#' rownames(graph) <- colnames(graph) <- rownames(data)
#'
#' attr(data, "graph") <- graph
#'
#' data$L1 <- rnorm(nrow(data), 0, 0.1)
#' data$L2 <- rnorm(nrow(data), 1, 0.2)
#' data$L3 <- rnorm(nrow(data), 2, 0.3)
#'
#' DimPlot(data, group_by = "cluster")
#' DimPlot(data, group_by = "cluster", theme = "theme_blank")
#' DimPlot(data, group_by = "cluster", theme = ggplot2::theme_classic,
#'         theme_args = list(base_size = 16), palette = "seurat")
#' DimPlot(data, group_by = "cluster", raster = TRUE, raster_dpi = 30)
#' DimPlot(data, group_by = "cluster", highlight = 1:20,
#'         highlight_color = "red2", highlight_stroke = 0.8)
#' DimPlot(data, group_by = "cluster", highlight = TRUE, facet_by = "group",
#'         theme = "theme_blank")
#' DimPlot(data, group_by = "cluster", label = TRUE)
#' DimPlot(data, group_by = "cluster", label = TRUE, label_fg = "red",
#'         label_bg = "yellow", label_size = 5)
#' DimPlot(data, group_by = "cluster", label = TRUE, label_insitu = TRUE)
#' DimPlot(data, group_by = "cluster", add_mark = TRUE)
#' DimPlot(data, group_by = "cluster", add_mark = TRUE, mark_linetype = 2)
#' DimPlot(data, group_by = "cluster", add_mark = TRUE, mark_type = "ellipse")
#' DimPlot(data, group_by = "cluster", add_density = TRUE)
#' DimPlot(data, group_by = "cluster", add_density = TRUE, density_filled = TRUE)
#' DimPlot(data, group_by = "cluster", add_density = TRUE, density_filled = TRUE,
#'         density_filled_palette = "Blues", highlight = TRUE)
#' DimPlot(data, group_by = "cluster", stat_by = "group")
#' DimPlot(data, group_by = "cluster", stat_by = "group", stat_plot_type = "bar")
#' DimPlot(data, group_by = "cluster", hex = TRUE)
#' DimPlot(data, group_by = "cluster", hex = TRUE, hex_bins = 20)
#' DimPlot(data, group_by = "cluster", hex = TRUE, hex_count = FALSE)
#' DimPlot(data, group_by = "cluster", graph = "@graph", edge_color = "grey80")
#' DimPlot(data, group_by = "cluster", lineages = c("L1", "L2", "L3"))
#' DimPlot(data, group_by = "cluster", lineages = c("L1", "L2", "L3"),
#'         lineages_whiskers = TRUE)
#' DimPlot(data, group_by = "cluster", lineages = c("L1", "L2", "L3"),
#'         lineages_span = 1)
#' DimPlot(data, group_by = "cluster",  split_by = "cluster",
#'         palcolor = list("C1" = "red", "C2" = "blue", "C3" = "green"))
#' }
DimPlot <- function(
    data, dims = 1:2, group_by, group_by_sep = "_", split_by = NULL, split_by_sep = "_",
    pt_size = NULL, pt_alpha = 1, bg_color = "grey80",
    label_insitu = FALSE, show_stat = !identical(theme, "theme_blank"),
    label = FALSE, label_size = 4, label_fg = "white", label_bg = "black", label_bg_r = 0.1,
    label_repel = FALSE, label_repulsion = 20, label_pt_size = 1, label_pt_color = "black",
    label_segment_color = "black",
    highlight = NULL, highlight_alpha = 1, highlight_size = 1, highlight_color = "black", highlight_stroke = 0.8,
    add_mark = FALSE, mark_type = c("hull", "ellipse", "rect", "circle"), mark_expand = unit(3, "mm"),
    mark_alpha = 0.1, mark_linetype = 1,
    stat_by = NULL, stat_plot_type = c("pie", "ring", "bar", "line"), stat_plot_size = 0.1, stat_args = list(palette = "Set1"),
    graph = NULL, edge_size = c(0.05, 0.5), edge_alpha = 0.1, edge_color = "grey40",
    add_density = FALSE, density_color = "grey80", density_filled = FALSE,
    density_filled_palette = "Greys", density_filled_palcolor = NULL,
    lineages = NULL, lineages_trim = c(0.01, 0.99), lineages_span = 0.75,
    lineages_palette = "Dark2", lineages_palcolor = NULL, lineages_arrow = arrow(length = unit(0.1, "inches")),
    lineages_linewidth = 1, lineages_line_bg = "white", lineages_line_bg_stroke = 0.5,
    lineages_whiskers = FALSE, lineages_whiskers_linewidth = 0.5, lineages_whiskers_alpha = 0.5,
    facet_by = NULL, facet_scales = "fixed", facet_nrow = NULL, facet_ncol = NULL, facet_byrow = TRUE,
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL,
    theme = "theme_this", theme_args = list(), aspect.ratio = 1, legend.position = "right", legend.direction = "vertical",
    raster = NULL, raster_dpi = c(512, 512),
    hex = FALSE, hex_linewidth = 0.5, hex_count = TRUE, hex_bins = 50, hex_binwidth = NULL,
    palette = "Paired", palcolor = NULL, seed = 8525,
    combine = TRUE, nrow = NULL, ncol = NULL, byrow = TRUE, ...) {

    validate_common_args(seed, facet_by = facet_by)
    theme <- process_theme(theme)
    split_by <- check_columns(data, split_by, force_factor = TRUE, allow_multi = TRUE,
        concat_multi = TRUE, concat_sep = split_by_sep)

    if (!is.null(split_by)) {
        datas <- split(data, data[[split_by]])
        # keep the order of levels
        datas <- datas[levels(data[[split_by]])]
        if (is.character(graph) && length(graph) == 1 && startsWith(graph, "@")) {
            # split the graph as well
            datas <- lapply(datas, function(d) {
                gh <- attr(data, substring(graph, 2))[rownames(d), rownames(d)]
                attr(d, substring(graph, 2)) <- gh
                d
            })
        }
        palette <- check_palette(palette, names(datas))
        palcolor <- check_palcolor(palcolor, names(datas))
    } else {
		datas <- list(data)
        palette <- list(palette)
        names(datas) <- "..."
        names(palette) <- "..."
        if (!is.null(palcolor)) {
	        palcolor <- list(palcolor)
            palcolor <- check_palcolor(palcolor, "...")
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
            DimPlotAtomic(datas[[nm]],
                dims = dims, group_by = group_by, group_by_sep = group_by_sep,
                pt_size = pt_size, pt_alpha = pt_alpha, bg_color = bg_color,
                label_insitu = label_insitu, show_stat = show_stat,
                label = label, label_size = label_size, label_fg = label_fg, label_bg = label_bg, label_bg_r = label_bg_r,
                label_repel = label_repel, label_repulsion = label_repulsion, label_pt_size = label_pt_size, label_pt_color = label_pt_color,
                label_segment_color = label_segment_color,
                highlight = highlight, highlight_alpha = highlight_alpha, highlight_size = highlight_size, highlight_color = highlight_color, highlight_stroke = highlight_stroke,
                add_mark = add_mark, mark_type = mark_type, mark_expand = mark_expand, mark_alpha = mark_alpha, mark_linetype = mark_linetype,
                stat_by = stat_by, stat_plot_type = stat_plot_type, stat_plot_size = stat_plot_size, stat_args = stat_args,
                graph = graph, edge_size = edge_size, edge_alpha = edge_alpha, edge_color = edge_color,
                add_density = add_density, density_color = density_color, density_filled = density_filled,
                density_filled_palette = density_filled_palette, density_filled_palcolor = density_filled_palcolor,
                lineages = lineages, lineages_trim = lineages_trim, lineages_span = lineages_span,
                lineages_palette = lineages_palette, lineages_palcolor = lineages_palcolor, lineages_arrow = lineages_arrow,
                lineages_linewidth = lineages_linewidth, lineages_line_bg = lineages_line_bg, lineages_line_bg_stroke = lineages_line_bg_stroke,
                lineages_whiskers = lineages_whiskers, lineages_whiskers_linewidth = lineages_whiskers_linewidth, lineages_whiskers_alpha = lineages_whiskers_alpha,
                facet_by = facet_by, facet_scales = facet_scales, facet_nrow = facet_nrow, facet_ncol = facet_ncol, facet_byrow = facet_byrow,
                title = title, subtitle = subtitle, xlab = xlab, ylab = ylab,
                theme = theme, theme_args = theme_args, aspect.ratio = aspect.ratio, legend.position = legend.position, legend.direction = legend.direction,
                raster = raster, raster_dpi = raster_dpi,
                hex = hex, hex_linewidth = hex_linewidth, hex_count = hex_count, hex_bins = hex_bins, hex_binwidth = hex_binwidth,
                palette = palette[[nm]], palcolor = palcolor[[nm]], seed = seed, ...
            )
        }
    )

    combine_plots(plots, combine = combine, nrow = nrow, ncol = ncol, byrow = byrow)
}

#' @export
#' @rdname dimplot
#' @inheritParams common_args
#' @inheritParams DimPlotAtomic
#' @param split_by A character vector of column names to split the data and plot separately
#'   If TRUE, we will split the data by the `features`. Each feature will be plotted separately.
#' @return A ggplot object or wrap_plots object or a list of ggplot objects
#' @export
#' @examples
#' \donttest{
#' # Feature Dim Plot
#' FeatureDimPlot(data, features = "L1", pt_size = 2)
#' FeatureDimPlot(data, features = "L1", pt_size = 2, bg_cutoff = -Inf)
#' FeatureDimPlot(data, features = "L1", raster = TRUE, raster_dpi = 30)
#' FeatureDimPlot(data, features = c("L1", "L2"), pt_size = 2)
#' FeatureDimPlot(data, features = c("L1"), pt_size = 2, facet_by = "group")
#' # Can't facet multiple features
#' FeatureDimPlot(data, features = c("L1", "L2", "L3"), pt_size = 2)
#' # We can use split_by
#' FeatureDimPlot(data, features = c("L1", "L2", "L3"), split_by = "group", nrow = 2)
#' FeatureDimPlot(data, features = c("L1", "L2", "L3"), highlight = TRUE)
#' FeatureDimPlot(data, features = c("L1", "L2", "L3"), hex = TRUE, hex_bins = 15)
#' FeatureDimPlot(data, features = c("L1", "L2", "L3"), hex = TRUE, hex_bins = 15,
#'                split_by = "cluster", palcolor = list("C1" = "red", "C2" = "blue", "C3" = "green"))
#' }
FeatureDimPlot <- function(
    data, dims = 1:2, features, split_by = NULL, split_by_sep = "_",
    lower_quantile = 0, upper_quantile = 0.99, lower_cutoff = NULL, upper_cutoff = NULL,
    pt_size = NULL, pt_alpha = 1, bg_color = "grey80", bg_cutoff = NULL,
    label_insitu = FALSE, show_stat = !identical(theme, "theme_blank"), color_name = "",
    label = FALSE, label_size = 4, label_fg = "white", label_bg = "black", label_bg_r = 0.1,
    label_repel = FALSE, label_repulsion = 20, label_pt_size = 1, label_pt_color = "black",
    label_segment_color = "black",
    highlight = NULL, highlight_alpha = 1, highlight_size = 1, highlight_color = "black", highlight_stroke = 0.8,
    add_mark = FALSE, mark_type = c("hull", "ellipse", "rect", "circle"), mark_expand = unit(3, "mm"),
    mark_alpha = 0.1, mark_linetype = 1,
    stat_by = NULL, stat_plot_type = c("pie", "ring", "bar", "line"), stat_plot_size = 0.1, stat_args = list(palette = "Set1"),
    graph = NULL, edge_size = c(0.05, 0.5), edge_alpha = 0.1, edge_color = "grey40",
    add_density = FALSE, density_color = "grey80", density_filled = FALSE,
    density_filled_palette = "Greys", density_filled_palcolor = NULL,
    lineages = NULL, lineages_trim = c(0.01, 0.99), lineages_span = 0.75,
    lineages_palette = "Dark2", lineages_palcolor = NULL, lineages_arrow = arrow(length = unit(0.1, "inches")),
    lineages_linewidth = 1, lineages_line_bg = "white", lineages_line_bg_stroke = 0.5,
    lineages_whiskers = FALSE, lineages_whiskers_linewidth = 0.5, lineages_whiskers_alpha = 0.5,
    facet_by = NULL, facet_scales = "fixed", facet_nrow = NULL, facet_ncol = NULL, facet_byrow = TRUE,
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL,
    theme = "theme_this", theme_args = list(), aspect.ratio = 1, legend.position = "right", legend.direction = "vertical",
    raster = NULL, raster_dpi = c(512, 512),
    hex = FALSE, hex_linewidth = 0.5, hex_count = FALSE, hex_bins = 50, hex_binwidth = NULL,
    palette = "Spectral", palcolor = NULL, seed = 8525,
    combine = TRUE, nrow = NULL, ncol = NULL, byrow = TRUE, ...) {

    validate_common_args(seed, facet_by = facet_by)

    if (isTRUE(split_by)) {
        plots <- lapply(
            features, function(feature) DimPlotAtomic(
                data, dims = dims,
                lower_quantile = lower_quantile, upper_quantile = upper_quantile, lower_cutoff = lower_cutoff, upper_cutoff = upper_cutoff,
                pt_size = pt_size, pt_alpha = pt_alpha, bg_color = bg_color, color_name = color_name,
                label_insitu = label_insitu, show_stat = show_stat, features = feature, bg_cutoff = bg_cutoff,
                label = label, label_size = label_size, label_fg = label_fg, label_bg = label_bg, label_bg_r = label_bg_r,
                label_repel = label_repel, label_repulsion = label_repulsion, label_pt_size = label_pt_size, label_pt_color = label_pt_color,
                label_segment_color = label_segment_color,
                highlight = highlight, highlight_alpha = highlight_alpha, highlight_size = highlight_size, highlight_color = highlight_color, highlight_stroke = highlight_stroke,
                add_mark = add_mark, mark_type = mark_type, mark_expand = mark_expand, mark_alpha = mark_alpha, mark_linetype = mark_linetype,
                stat_by = stat_by, stat_plot_type = stat_plot_type, stat_plot_size = stat_plot_size, stat_args = stat_args,
                graph = graph, edge_size = edge_size, edge_alpha = edge_alpha, edge_color = edge_color,
                add_density = add_density, density_color = density_color, density_filled = density_filled,
                density_filled_palette = density_filled_palette, density_filled_palcolor = density_filled_palcolor,
                lineages = lineages, lineages_trim = lineages_trim, lineages_span = lineages_span,
                lineages_palette = lineages_palette, lineages_palcolor = lineages_palcolor, lineages_arrow = lineages_arrow,
                lineages_linewidth = lineages_linewidth, lineages_line_bg = lineages_line_bg, lineages_line_bg_stroke = lineages_line_bg_stroke,
                lineages_whiskers = lineages_whiskers, lineages_whiskers_linewidth = lineages_whiskers_linewidth, lineages_whiskers_alpha = lineages_whiskers_alpha,
                facet_by = facet_by, facet_scales = facet_scales, facet_nrow = facet_nrow, facet_ncol = facet_ncol, facet_byrow = facet_byrow,
                title = title %||% feature, subtitle = subtitle, xlab = xlab, ylab = ylab,
                theme = theme, theme_args = theme_args, aspect.ratio = aspect.ratio, legend.position = legend.position, legend.direction = legend.direction,
                raster = raster, raster_dpi = raster_dpi,
                hex = hex, hex_linewidth = hex_linewidth, hex_count = hex_count, hex_bins = hex_bins, hex_binwidth = hex_binwidth,
                palette = palette, palcolor = palcolor, seed = seed, ...
            )
        )
    } else {
        split_by <- check_columns(data, split_by, force_factor = TRUE, allow_multi = TRUE,
            concat_multi = TRUE, concat_sep = split_by_sep)

        if (!is.null(split_by)) {
            datas <- split(data, data[[split_by]])
            # keep the order of levels
            datas <- datas[levels(data[[split_by]])]
            if (is.character(graph) && length(graph) == 1 && startsWith(graph, "@")) {
                # split the graph as well
                datas <- lapply(datas, function(d) {
                    gh <- attr(data, substring(graph, 2))[rownames(d), rownames(d)]
                    attr(d, substring(graph, 2)) <- gh
                    d
                })
            }
	        palette <- check_palette(palette, names(datas))
	        palcolor <- check_palcolor(palcolor, names(datas))
	    } else {
			datas <- list(data)
	        palette <- list(palette)
	        names(datas) <- "..."
	        names(palette) <- "..."
	        if (!is.null(palcolor)) {
		        palcolor <- list(palcolor)
	            palcolor <- check_palcolor(palcolor, "...")
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
                DimPlotAtomic(
                    datas[[nm]], dims = dims, features = features,
                    lower_quantile = lower_quantile, upper_quantile = upper_quantile, lower_cutoff = lower_cutoff, upper_cutoff = upper_cutoff,
                    pt_size = pt_size, pt_alpha = pt_alpha, bg_color = bg_color, color_name = color_name,
                    label_insitu = label_insitu, show_stat = show_stat, bg_cutoff = bg_cutoff,
                    label = label, label_size = label_size, label_fg = label_fg, label_bg = label_bg, label_bg_r = label_bg_r,
                    label_repel = label_repel, label_repulsion = label_repulsion, label_pt_size = label_pt_size, label_pt_color = label_pt_color,
                    label_segment_color = label_segment_color,
                    highlight = highlight, highlight_alpha = highlight_alpha, highlight_size = highlight_size, highlight_color = highlight_color, highlight_stroke = highlight_stroke,
                    add_mark = add_mark, mark_type = mark_type, mark_expand = mark_expand, mark_alpha = mark_alpha, mark_linetype = mark_linetype,
                    stat_by = stat_by, stat_plot_type = stat_plot_type, stat_plot_size = stat_plot_size, stat_args = stat_args,
                    graph = graph, edge_size = edge_size, edge_alpha = edge_alpha, edge_color = edge_color,
                    add_density = add_density, density_color = density_color, density_filled = density_filled,
                    density_filled_palette = density_filled_palette, density_filled_palcolor = density_filled_palcolor,
                    lineages = lineages, lineages_trim = lineages_trim, lineages_span = lineages_span,
                    lineages_palette = lineages_palette, lineages_palcolor = lineages_palcolor, lineages_arrow = lineages_arrow,
                    lineages_linewidth = lineages_linewidth, lineages_line_bg = lineages_line_bg, lineages_line_bg_stroke = lineages_line_bg_stroke,
                    lineages_whiskers = lineages_whiskers, lineages_whiskers_linewidth = lineages_whiskers_linewidth, lineages_whiskers_alpha = lineages_whiskers_alpha,
                    facet_by = facet_by, facet_scales = facet_scales, facet_nrow = facet_nrow, facet_ncol = facet_ncol, facet_byrow = facet_byrow,
                    title = title, subtitle = subtitle, xlab = xlab, ylab = ylab,
                    theme = theme, theme_args = theme_args, aspect.ratio = aspect.ratio, legend.position = legend.position, legend.direction = legend.direction,
                    raster = raster, raster_dpi = raster_dpi,
                    hex = hex, hex_linewidth = hex_linewidth, hex_count = hex_count, hex_bins = hex_bins, hex_binwidth = hex_binwidth,
                    palette = palette[[nm]], palcolor = palcolor[[nm]], seed = seed, ...
                )
            }
        )
    }

    combine_plots(plots, combine = combine, nrow = nrow, ncol = ncol, byrow = byrow)
}
