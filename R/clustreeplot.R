#' Atomic function for clustree plot
#'
#' @inheritParams common_args
#' @param prefix A character string of the prefix of the columns to plot.
#'  The columns with the prefix will be used to plot the tree.
#' @param flip A logical value to flip the tree.
#' @param alpha A numeric value of the transparency of the nodes.
#'  Only used when `node_alpha` is not provided in `...`.
#' @param edge_palette A character string of the palette name to color the edges.
#' @param edge_palcolor A character vector of colors to color the edges.
#' @param ... Other arguments passed to `clustree::clustree`.
#' @importFrom stats complete.cases
#' @importFrom dplyr %>% select starts_with
#' @importFrom ggplot2 scale_color_manual coord_cartesian element_line element_blank labs coord_flip
#' @importFrom ggplot2 scale_y_reverse coord_flip
#' @importFrom ggrepel geom_text_repel
#' @keywords internal
ClustreePlotAtomic <- function(
    data, prefix, flip = FALSE, alpha = 0.85,
    palette = "Paired", palcolor = NULL, edge_palette = "Spectral", edge_palcolor = NULL,
    aspect.ratio = 1, legend.position = "right", legend.direction = "vertical",
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL, expand = c(0.1, 0.1),
    theme = "theme_this", theme_args = list(),
    ...) {
    # if (!requireNamespace("ggraph", quietly = TRUE)) {
    #     stop("ggraph is not available for ClustreePlot. Please install it first.")
    # }
    # ! Unknown guide: edge_colourbar
    # suppressMessages(suppressWarnings(suppressPackageStartupMessages(library(ggraph))))
    # if (!requireNamespace("clustree", quietly = TRUE)) {
    #     stop("clustree is not available for ClustreePlot. Please install it first.")
    # }

    data <- data %>% select(starts_with(prefix))
    data <- data[complete.cases(data), , drop = FALSE]
    expand <- norm_expansion(expand, x_type = "continuous", y_type = "continuous")

    if (ncol(data) == 0 || nrow(data) == 0) {
        stop("No data found with prefix '", prefix, "'")
    }
    resolutions <- substring(colnames(data), nchar(prefix) + 1)
    nres <- length(resolutions)
    if (all(startsWith(resolutions, "_"))) {
        prefix <- paste0(prefix, "_")
        resolutions <- substring(resolutions, 2)
    } else if (all(startsWith(resolutions, "."))) {
        prefix <- paste0(prefix, ".")
        resolutions <- substring(resolutions, 2)
    }
    resolutions <- as.numeric(resolutions)
    # make sure res.0.50 to be access by res.0.5
    colnames(data) <- paste0(prefix, resolutions)
    resolutions <- sort(resolutions)
    nres <- length(resolutions)
    max_clusters <- length(unique(data[[paste0(prefix, max(resolutions))]]))
    clustree_args <- list(...)
    clustree_args$x <- data
    clustree_args$prefix <- prefix
    clustree_args$node_alpha <- clustree_args$node_alpha %||% alpha %||% 0.85
    clustree_args$edge_width <- clustree_args$edge_width %||% 0.9
    clustree_args$show_axis <- clustree_args$show_axis %||% TRUE
    clustree_args$layout <- clustree_args$layout %||% "sugiyama"
    clustree_args$node_size_range <- clustree_args$node_size_range %||% c(6, 12)
    clustree_args$node_text_size <- clustree_args$node_text_size %||% 3
    clustree_args$node_text_colour <- clustree_args$node_text_colour %||% clustree_args$node_text_color %||% "black"
    clustree <- gglogger::register(clustree::clustree)

    p <- suppressMessages(do.call(clustree, clustree_args))

    # make glow effect
    # ggrepel::geom_text_repel with bg.colour is not working well with coord_flip
    # theta <- seq(pi / 8, 2 * pi, length.out = 16)
    # for (i in theta) {
    #     dx <- cos(i) * clustree_args$node_text_size / 10
    #     dy <- sin(i) * clustree_args$node_text_size / 10
    #     p <- p + geom_text(aes(label = cluster, x = x + dx, y = y + dy), fontface = "bold",
    #         size = clustree_args$node_text_size, color = "white")
    # }
    # p <- p + geom_text(aes(label = cluster, x = x + 0.1, y = y + 0.1),
    #     size = clustree_args$node_text_size, color = "white")
    # p <- p + geom_text(aes(label = cluster, x = x + 0.1, y = y - 0.1),
    #     size = clustree_args$node_text_size, color = "white")
    # p <- p + geom_text(aes(label = cluster, x = x - 0.1, y = y - 0.1),
    #     size = clustree_args$node_text_size, color = "white")
    # p <- p + geom_text(aes(label = cluster, x = x - 0.1, y = y - 0.1),
    #     size = clustree_args$node_text_size, color = "white")

    p <- suppressMessages({
        p +
        geom_text(aes(label = !!sym("cluster"), x = !!sym("x"), y = !!sym("y")),
            size = clustree_args$node_text_size, color = clustree_args$node_text_colour) +
        scale_color_manual(
            values = palette_this(n = nres, palette = palette, palcolor = palcolor, keep_names = FALSE),
            guide = "none") +
        ggraph::scale_edge_color_gradientn(
            name = "count",
            n.breaks = 5,
            colors = palette_this(palette = edge_palette, palcolor = edge_palcolor),
            na.value = "grey80",
            guide = ggraph::guide_edge_colorbar()) +
        do.call(theme, theme_args) +
        ggplot2::theme(
            aspect.ratio = aspect.ratio,
            axis.title.x = element_text(),
            axis.title.y = element_text(),
            legend.position = legend.position,
            legend.direction = legend.direction,
            # panel.border = element_blank()
        ) +
        labs(title = title, subtitle = subtitle, x = xlab %||% "", y = ylab %||% sub("\\.|_$", "", prefix)) +
        scale_x_continuous(expand = expand$x)
    })

    # height driven by nres (resolution rows); width driven by max_clusters (node spread)
    # keep old heuristics only as fallback
    height_val <- nres * max_clusters / 20
    width_val <- if (max_clusters > 20) 11 else if (max_clusters > 15) 9 else 7

    if (isTRUE(flip)) {
        # After flip: resolutions become x-axis columns, clusters become y-axis rows
        # Width driven by nres; height by max_clusters (or aspect.ratio applied to width)
        content_width_fl <- max(3, 0.5 + nres * 1.0)
        content_height_fl <- if (!is.null(aspect.ratio)) {
            max(3, min(content_width_fl * aspect.ratio, 12))
        } else {
            max(3, 0.5 + max_clusters * 0.5)
        }
        dims <- calculate_plot_dimensions(
            base_height = content_height_fl,
            aspect.ratio = NULL,
            n_x = nres,
            x_scale_factor = 1.0,
            legend.position = legend.position,
            legend.direction = legend.direction
        )
        attr(p, "height") <- if (is.null(dims)) width_val else dims$height
        attr(p, "width") <- if (is.null(dims)) height_val else dims$width
        p <- suppressMessages({
            p + scale_y_reverse(breaks = nres:1, labels = resolutions, expand = expand$y) +
            coord_flip(clip = "off") +
            ggplot2::theme(
                panel.grid.major.x = element_line(colour = "grey80", linetype = 2),
                panel.grid.major.y = element_blank(),
                axis.line.y = element_blank(),
                axis.text.y = element_blank(),
                axis.ticks.y = element_blank())
        })
    } else {
        # Non-flip: resolutions on y-axis (rows), clusters spread on x-axis (columns)
        # Width driven by max_clusters; height by nres (or aspect.ratio applied to width)
        content_width_nf <- max(3, 0.5 + max_clusters * 0.5)
        content_height_nf <- if (!is.null(aspect.ratio)) {
            max(3, min(content_width_nf * aspect.ratio, 12))
        } else {
            max(3, 0.5 + nres * 1.0)
        }
        dims <- calculate_plot_dimensions(
            base_height = content_height_nf,
            aspect.ratio = NULL,
            n_x = max_clusters,
            x_scale_factor = 0.5,
            legend.position = legend.position,
            legend.direction = legend.direction
        )
        attr(p, "height") <- if (is.null(dims)) height_val else dims$height
        attr(p, "width") <- if (is.null(dims)) width_val else dims$width
        p <- suppressMessages({
            p + coord_cartesian(clip = "off") +
            scale_y_continuous(breaks = nres:1, labels = resolutions, expand = expand$y) +
            ggplot2::theme(
                panel.grid.major.x = element_blank(),
                panel.grid.major.y = element_line(colour = "grey80", linetype = 2),
                axis.line.x = element_blank(),
                axis.text.x = element_blank(),
                axis.ticks.x = element_blank()
            )
        })
    }
    p
}

#' Clustree Plot
#'
#' @description A plot visualizing Clusterings at Different Resolutions
#' @inheritParams common_args
#' @inheritParams ClustreePlotAtomic
#' @return A ggplot object or wrap_plots object or a list of ggplot objects
#' @export
#' @examples
#' \donttest{
#' set.seed(8525)
#' N = 100
#' data <- data.frame(
#'     p.0.4 = sample(LETTERS[1:5], N, replace = TRUE),
#'     p.0.5 = sample(LETTERS[1:6], N, replace = TRUE),
#'     p.0.6 = sample(LETTERS[1:7], N, replace = TRUE),
#'     p.0.7 = sample(LETTERS[1:8], N, replace = TRUE),
#'     p.0.8 = sample(LETTERS[1:9], N, replace = TRUE),
#'     p.0.9 = sample(LETTERS[1:10], N, replace = TRUE),
#'     p.1 = sample(LETTERS[1:30], N, replace = TRUE),
#'     split = sample(1:2, N, replace = TRUE)
#' )
#'
#' ClustreePlot(data, prefix = "p")
#' ClustreePlot(data, prefix = "p", flip = TRUE)
#' ClustreePlot(data, prefix = "p", split_by = "split")
#' ClustreePlot(data, prefix = "p", split_by = "split",
#'              palette = c("1" = "Set1", "2" = "Paired"))
#' }
ClustreePlot <- function(
    data, prefix, flip = FALSE, split_by = NULL, split_by_sep = "_",
    palette = "Paired", palcolor = NULL, edge_palette = "Spectral", edge_palcolor = NULL,
    aspect.ratio = 1, legend.position = "right", legend.direction = "vertical",
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL, expand = c(0.1, 0.1),
    theme = "theme_this", theme_args = list(),
    combine = TRUE, nrow = NULL, ncol = NULL, byrow = TRUE, seed = 8525,
    axes = NULL, axis_titles = axes, guides = NULL, design = NULL, ...
) {
    validate_common_args(seed)
    theme <- process_theme(theme)
    split_by <- check_columns(data, split_by, force_factor = TRUE, allow_multi = TRUE, concat_multi = TRUE, concat_sep = split_by_sep)

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
            ClustreePlotAtomic(datas[[nm]],
                prefix = prefix, flip = flip, palette = palette[[nm]], palcolor = palcolor[[nm]],
                edge_palette = edge_palette, edge_palcolor = edge_palcolor, expand = expand,
                aspect.ratio = aspect.ratio, legend.position = legend.position[[nm]],
                legend.direction = legend.direction[[nm]],
                title = title, subtitle = subtitle, xlab = xlab, ylab = ylab,
                theme = theme, theme_args = theme_args, ...
            )
        }
    )

    combine_plots(plots, combine = combine, nrow = nrow, ncol = ncol, byrow = byrow,
        axes = axes, axis_titles = axis_titles, guides = guides, design = design)
}
