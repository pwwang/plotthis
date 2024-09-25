#' Atomic Sankey plot
#'
#' @description Plot a Sankey plot without splitting the data.
#' @inheritParams common_args
#' @param y A character string of the column name to plot on the y-axis.
#'   A numeric column is expected.
#'   If NULL, the count of each nodes_by column will be used.
#' @param nodes_by A character vector of column names to define the nodes.
#' @param links_by A character vector of column names to define the links.
#'  If NULL, the links_by will be the first column in nodes_by.
#' @param links_by_sep A character string to concatenate the columns in `links_by`, if multiple columns are provided.
#' @param links_name A character string to name the legend of links.
#' @param nodes_palette A character string to specify the palette of nodes.
#' @param nodes_palcolor A character vector to specify the colors of nodes.
#' @param nodes_alpha A numeric value to specify the transparency of nodes.
#' @param nodes_label A logical value to show the labels on the nodes.
#' @param links_palette A character string to specify the palette of links.
#' @param links_palcolor A character vector to specify the colors of links.
#' @param links_alpha A numeric value to specify the transparency of links.
#' @param legend.box A character string to specify the box of the legend, either "vertical" or "horizontal".
#' @return A ggplot object
#' @keywords internal
#' @importFrom rlang syms %||%
#' @importFrom dplyr %>% group_by summarise n
#' @importFrom gglogger ggplot
#' @importFrom ggnewscale new_scale_fill
#' @importFrom ggplot2 geom_col scale_fill_manual geom_label after_stat scale_x_discrete scale_y_continuous labs
SankeyPlotAtomic <- function(
    data, y = NULL, nodes_by, nodes_color = "grey30", links_by = NULL, links_by_sep = "_", links_name = NULL,
    nodes_palette = "Paired", nodes_palcolor = NULL, nodes_alpha = 1, nodes_label = FALSE,
    links_palette = "Paired", links_palcolor = NULL, links_alpha = 0.6, legend.box = "vertical",
    x_text_angle = 0, aspect.ratio = 1, legend.position = "right", legend.direction = "vertical",
    theme = "theme_this", theme_args = list(), title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL,
    ...
) {
    if (!requireNamespace("ggalluvial", quietly = TRUE)) {
        stop("ggalluvial is required to use SankeyPlot/AlluvialPlot.")
    }

    nodes_by <- check_columns(data, nodes_by, force_factor = TRUE, allow_multi = TRUE)
    if (length(nodes_by) < 2) {
        stop("'nodes_by' must have at least 2 columns.")
    }
    links_by <- check_columns(data, links_by, force_factor = TRUE, allow_multi = TRUE, concat_multi = TRUE, concat_sep = links_by_sep)
    if (is.null(links_by)) {
        links_by <- nodes_by[1]
    }

    if (is.null(y)) {
        data <- data %>%
            group_by(!!!syms(unique(c(nodes_by, links_by)))) %>%
            summarise(.y = n(), .groups = "drop")
        y <- ".y"
    }

    orig_links_by <- links_by
    if (links_by %in% nodes_by) {
        # keep the links_by column when transforming to lodes form
        new_links_by <- paste0(".sankey.", links_by)
        data[[new_links_by]] <- data[[links_by]]
        links_by <- new_links_by
    }
    all_nodes <- unique(unlist(sapply(nodes_by, function(x) levels(data[[x]]))))
    data <- ggalluvial::to_lodes_form(data, key = ".Group", value = ".GroupValue", id = ".ID", axes = nodes_by)
    data$.GroupValue <- factor(data$.GroupValue, levels = all_nodes)
    nodes_colors <- palette_this(levels(data$.GroupValue), palette = nodes_palette, palcolor = nodes_palcolor)
    # make sure links colors are consistent with nodes colors when links_by is in nodes_by
    # and the nodes_palette/nodes_palcolor are the same as links_palette/links_palcolor
    if (orig_links_by %in% nodes_by && identical(nodes_palette, links_palette) && identical(nodes_palcolor, links_palcolor)) {
        links_colors <- nodes_colors
        links_guide = "none"
    } else {
        links_colors <- palette_this(levels(data[[links_by]]), palette = links_palette, palcolor = links_palcolor)
        links_guide = guide_legend(order = 1)
    }

    just <- calc_just(x_text_angle)
    base_size <- theme_args$base_size %||% 12
    text_size_scale <- base_size / 12

    p <- ggplot(data)
    # nodes filling
    for (i in seq_along(nodes_by)) {
        gvalues <- levels(data$.GroupValue)
        gvalues <- gvalues[gvalues %in% data[data$.Group == nodes_by[i], ".GroupValue", drop = TRUE]]
        p <- p +
            geom_col(aes(x = .Group, fill = .GroupValue, y = 0), width = 0) +
            scale_fill_manual(name = nodes_by[i], values = nodes_colors, breaks = gvalues,
                              guide = guide_legend(order = i + 1)) +
            new_scale_fill()
    }

    p <- p +
        ggalluvial::geom_alluvium(
            aes(x = .Group, stratum = .GroupValue, alluvium = .ID, y = !!sym(y), fill = !!sym(links_by)),
            alpha = links_alpha) +
        scale_fill_manual(
            name = links_name %||% ifelse(orig_links_by %in% nodes_by, paste0("Links: ", orig_links_by), orig_links_by),
            values = links_colors, breaks = levels(data[[links_by]]),
            guide = links_guide) +
        new_scale_fill() +
        ggalluvial::geom_stratum(
            aes(x = .Group, stratum = .GroupValue, alluvium = .ID, y = !!sym(y), fill = .GroupValue),
            alpha = nodes_alpha, width = 0.25, color = nodes_color) +
        scale_fill_manual(values = nodes_colors, breaks = levels(data$.GroupValue),
                          guide = "none")

    if (isTRUE(nodes_label)) {
        p <- p + geom_label(
            aes(x = .Group, stratum = .GroupValue, alluvium = .ID, label = .GroupValue, y = !!sym(y)),
            stat = ggalluvial::StatStratum,
            size = text_size_scale * 3)
    }

    p <- p +
        scale_x_discrete(expand = c(0, 0)) +
        scale_y_continuous(expand = c(0, 0)) +
        do.call(theme, theme_args) +
        labs(title = title, subtitle = subtitle, x = xlab %||% "", y = xlab %||% orig_links_by) +
        ggplot2::theme(
            aspect.ratio = aspect.ratio,
            legend.position = legend.position,
            legend.direction = legend.direction,
            legend.box = legend.box,
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            axis.ticks.x = element_blank(),
            axis.text.x = element_text(angle = x_text_angle, hjust = just$h, vjust = just$v)
        )

    attr(p, "height") <- 6
    attr(p, "width") <- nlevels(data$.Group) * ifelse(nlevels(data$.Group) < 5, 2, 1.5)

    p
}


#' Sankey / Alluvial Plot
#'
#' @description A plot visualizing flow/movement/change from one state to another or one time to another.
#'  `AlluvialPlot` is an alias of `SankeyPlot`.
#' @inheritParams common_args
#' @inheritParams SankeyPlotAtomic
#' @return A ggplot object or wrap_plots object or a list of ggplot objects
#' @export
#' @rdname sankeyplot
#' @examples
#' set.seed(8525)
#' data <- data.frame(
#'     nodes1 = sample(LETTERS[1:3], 10, replace = TRUE),
#'     nodes2 = sample(letters[1:3], 10, replace = TRUE),
#'     nodes3 = sample(LETTERS[4:6], 10, replace = TRUE),
#'     y = sample(1:5, 10, replace = TRUE)
#' )
#' SankeyPlot(data, nodes_by = c("nodes1", "nodes2", "nodes3"))
#' SankeyPlot(data, nodes_by = c("nodes1", "nodes2", "nodes3"), nodes_label = TRUE)
#' SankeyPlot(data, nodes_by = c("nodes1", "nodes2", "nodes3"), links_by = "y")
#' SankeyPlot(data, nodes_by = c("nodes1", "nodes2", "nodes3"), y = "y")
SankeyPlot <- function(
    data, y = NULL, nodes_by, nodes_color = "grey30", links_by = NULL, links_by_sep = "_", links_name = NULL,
    split_by = NULL, split_by_sep = "_", palette = "Paired", palcolor = NULL, alpha = 0.6, label = FALSE,
    x_text_angle = 0, aspect.ratio = 1, legend.position = "right", legend.direction = "vertical", legend.box = "vertical",
    theme = "theme_this", theme_args = list(), title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL,
    combine = TRUE, nrow = NULL, ncol = NULL, byrow = TRUE, seed = 8525, ...
) {
    validate_common_args(seed)
    split_by <- check_columns(data, split_by, force_factor = TRUE, allow_multi = TRUE, concat_multi = TRUE, concat_sep = split_by_sep)

    if (!is.null(split_by)) {
        datas <- split(data, data[[split_by]])
        # keep the order of levels
        datas <- datas[levels(data[[split_by]])]
    } else {
        datas <- list(data)
    }

    plots <- lapply(
        datas, SankeyPlotAtomic,
        y = y, nodes_by = nodes_by, nodes_color = nodes_color,
        links_by = links_by, links_by_sep = links_by_sep, links_name = links_name,
        palette = palette, palcolor = palcolor, alpha = alpha, label = label,
        x_text_angle = x_text_angle, aspect.ratio = aspect.ratio,
        legend.position = legend.position, legend.direction = legend.direction, legend.box = legend.box,
        theme = theme, theme_args = theme_args, title = title, subtitle = subtitle,
        xlab = xlab, ylab = ylab, ...
    )

    combine_plots(plots, combine = combine, nrow = nrow, ncol = ncol, byrow = byrow)
}


#' @rdname sankeyplot
#' @export
AlluvialPlot <- SankeyPlot
