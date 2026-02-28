#' NetworkAtomic
#'
#' Plot a network graph without splitting the data.
#'
#' @inheritParams common_args
#' @param links A data frame containing the links between nodes.
#' @param nodes A data frame containing the nodes.
#'  This is optional. The names of the nodes are extracted from the links data frame.
#'  If `"@nodes"` is provided, the nodes data frame will be extracted from the attribute `nodes` of the links data frame.
#' @param from A character string specifying the column name of the links data frame for the source nodes.
#'  Default is the first column of the links data frame.
#' @param from_sep A character string to concatenate the columns in `from`, if multiple columns are provided.
#' @param to A character string specifying the column name of the links data frame for the target nodes.
#'  Default is the second column of the links data frame.
#' @param to_sep A character string to concatenate the columns in `to`, if multiple columns are provided.
#' @param node_by A character string specifying the column name of the nodes data frame for the node names.
#'  Default is the first column of the nodes data frame.
#' @param node_by_sep A character string to concatenate the columns in `node_by`, if multiple columns are provided.
#' @param link_weight_by A numeric value or a character string specifying the column name of the links data frame for the link weight.
#'  If a numeric value is provided, all links will have the same weight.
#'  This determines the width of the links.
#' @param link_weight_name A character string specifying the name of the link weight in the legend.
#' @param link_type_by A character string specifying the type of the links.
#'  This can be "solid", "dashed", "dotted", or a column name from the links data frame.
#'  It has higher priority when it is a column name.
#' @param link_type_name A character string specifying the name of the link type in the legend.
#' @param node_size_by A numeric value or a character string specifying the column name of the nodes data frame for the node size.
#'  If a numeric value is provided, all nodes will have the same size.
#' @param node_size_name A character string specifying the name of the node size in the legend.
#' @param node_color_by A character string specifying the color of the nodes.
#'  This can be a color name, a hex code, or a column name from the nodes data frame.
#'  It has higher priority when it is a column name.
#' @param node_color_name A character string specifying the name of the node color in the legend.
#' @param node_shape_by A numeric value or a character string specifying the column name of the nodes data frame for the node shape.
#' If a numeric value is provided, all nodes will have the same shape.
#' @param node_shape_name A character string specifying the name of the node shape in the legend.
#' @param node_fill_by A character string specifying the fill color of the nodes.
#' This can be a color name, a hex code, or a column name from the nodes data frame.
#' It has higher priority when it is a column name.
#' @param node_fill_name A character string specifying the name of the node fill in the legend.
#' @param link_alpha A numeric value specifying the transparency of the links.
#' @param node_alpha A numeric value specifying the transparency of the nodes.
#'  It only works when the nodes are filled.
#' @param node_stroke A numeric value specifying the stroke of the nodes.
#' @param cluster_scale A character string specifying how to scale the clusters.
#'  It can be "fill", "color", or "shape".
#' @param node_size_range A numeric vector specifying the range of the node size.
#' @param link_weight_range A numeric vector specifying the range of the link weight.
#' @param link_arrow_offset A numeric value specifying the offset of the link arrows.
#'  So that they won't overlap with the nodes.
#' @param link_curvature A numeric value specifying the curvature of the links.
#' @param link_color_by A character string specifying the colors of the link. It can be:
#'  * "from" means the color of the link is determined by the source node.
#'  * "to" means the color of the link is determined by the target node.
#'  * Otherwise, the color of the link is determined by the column name from the links data frame.
#' @param link_color_name A character string specifying the name of the link color in the legend.
#'  Only used when `link_color_by` is a column name.
#' @param palette A character string specifying the palette of the nodes.
#' @param palcolor A character vector specifying the colors of the node palette.
#' @param link_palette A character string specifying the palette of the links.
#'  When `link_color_by` is "from" or "to", the palette of the links defaults to the palette of the nodes.
#' @param link_palcolor A character vector specifying the colors of the link palette.
#'  When `link_color_by` is "from" or "to", the colors of the link palette defaults to the colors of the node palette.
#' @param directed A logical value specifying whether the graph is directed.
#' @param layout A character string specifying the layout of the graph.
#' It can be "circle", "tree", "grid", or a layout function from igraph.
#' @param cluster A character string specifying the clustering method.
#'  It can be "none", "fast_greedy", "walktrap", "edge_betweenness", "infomap", or a clustering function from igraph.
#' @param add_mark A logical value specifying whether to add mark for the clusters to the plot.
#' @param mark_expand A unit value specifying the expansion of the mark.
#' @param mark_type A character string specifying the type of the mark.
#' It can be "hull", "ellipse", "rect", "circle", or a mark function from ggforce.
#' @param mark_alpha A numeric value specifying the transparency of the mark.
#' @param mark_linetype A numeric value specifying the line type of the mark.
#' @param add_label A logical value specifying whether to add label to the nodes to the plot.
#' @param label_size A numeric value specifying the size of the label.
#' @param label_fg A character string specifying the foreground color of the label.
#' @param label_bg A character string specifying the background color of the label.
#' @param label_bg_r A numeric value specifying the background ratio of the label.
#' @param arrow An arrow object for the links.
#' @param ... Not used.
#' @return A ggplot object
#' @keywords internal
#' @importFrom utils getFromNamespace
#' @importFrom dplyr %>% rename relocate
#' @importFrom ggplot2 aes geom_curve geom_point scale_x_continuous scale_y_continuous
#' @importFrom ggplot2 scale_size_continuous scale_linetype_discrete guide_legend
#' @importFrom ggrepel geom_text_repel
#' @importFrom ggnewscale new_scale_color
NetworkAtomic <- function(
    links, nodes = NULL, from = NULL, from_sep = "_", to = NULL, to_sep = "_",
    node_by = NULL, node_by_sep = "_", link_weight_by = 2, link_weight_name = NULL,
    link_type_by = "solid", link_type_name = NULL,
    node_size_by = 15, node_size_name = NULL, node_color_by = "black", node_color_name = NULL,
    node_shape_by = 21, node_shape_name = NULL, node_fill_by = "grey20", node_fill_name = NULL,
    link_alpha = 1, node_alpha = 0.95, node_stroke = 1.5, cluster_scale = c("fill", "color", "shape"),
    node_size_range = c(5, 20), link_weight_range = c(0.5, 5), link_arrow_offset = 20,
    link_curvature = 0, link_color_by = "from", link_color_name = NULL, palette = "Paired", palcolor = NULL,
    link_palette = ifelse(link_color_by %in% c("from", "to"), palette, "Set1"),
    link_palcolor = if (link_color_by %in% c("from", "to")) palcolor else NULL,
    directed = TRUE, layout = "circle", cluster = "none", add_mark = FALSE, mark_expand = ggplot2::unit(10, "mm"),
    mark_type = c("hull", "ellipse", "rect", "circle"), mark_alpha = 0.1, mark_linetype = 1, add_label = TRUE,
    label_size = 3, label_fg = "white", label_bg = "black", label_bg_r = 0.1,
    arrow = ggplot2::arrow(type = "closed", length = ggplot2::unit(0.1, "inches")),
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL, aspect.ratio = 1,
    theme = "theme_this", theme_args = list(), legend.position = "right", legend.direction = "vertical",
    ...
) {
    cluster_scale <- match.arg(cluster_scale)

    base_size <- theme_args$base_size %||% 12
    text_size_scale <- base_size / 12

    if (is.character(nodes) && length(nodes) == 1 && startsWith(nodes, "@")) {
        nodes <- attr(links, substring(nodes, 2))
    }

    modify_list <- getFromNamespace("modify_list", "ggplot2")
    if (is.null(from)) {
        from <- "from"
        if (!from %in% colnames(links)) {
            colnames(links)[1] <- from
        }
    }
    if (is.null(to)) {
        to <- "to"
        if (!to %in% colnames(links)) {
            colnames(links)[2] <- to
        }
    }
    if (is.null(node_by) && !is.null(nodes)) {
        node_by <- "name"
        if (!node_by %in% colnames(nodes)) {
            colnames(nodes)[1] <- node_by
        }
    }

    from <- check_columns(links, from, force_factor = TRUE, allow_multi = TRUE,
        concat_multi = TRUE, concat_sep = from_sep)
    to <- check_columns(links, to, force_factor = TRUE, allow_multi = TRUE,
        concat_multi = TRUE, concat_sep = to_sep)
    links <- links %>%
        rename(from = from, to = to) %>%
        relocate(!!sym("from"), !!sym("to"))

    if (!is.null(nodes)) {
        node_by <- check_columns(nodes, node_by, force_factor = TRUE, allow_multi = TRUE,
            concat_multi = TRUE, concat_sep = node_by_sep)
        nodes <- nodes %>% rename(name = node_by) %>% relocate(!!sym("name"))
    }

    graph <- igraph::graph_from_data_frame(d = links, vertices = nodes, directed = directed)
    if (inherits(layout, "igraph_layout_spec")) {
        layout <- igraph::layout_(graph, layout)
    } else {
        if (layout %in% c("circle", "tree", "grid")) {
            layout <- switch(layout,
                "circle" = igraph::layout_in_circle(graph),
                "tree" = igraph::layout_as_tree(graph),
                "grid" = igraph::layout_on_grid(graph)
            )
        } else {
            lofun <- getFromNamespace(paste0("layout_with_", layout), "igraph")
            layout <- lofun(graph)
        }
    }

    df <- igraph::as_data_frame(graph, what = "both")
    df_nodes <- df$vertices
    df_nodes$x <- layout[, 1]
    df_nodes$y <- layout[, 2]
    df_edges <- df$edges
    rm(df)

    ## NODES
    node_layer_args <- list(mapping = list(aes(x = !!sym("x"), y = !!sym("y"))),
        stroke = node_stroke)

    if (is.numeric(node_size_by)) {
        node_layer_args$size <- node_size_by
        node_size_by_guide <- "none"
    } else {
        node_layer_args$mapping[[length(node_layer_args$mapping) + 1]] <- aes(size = !!sym(node_size_by))
        node_size_by_guide <- "guide"
    }

    if (!node_color_by %in% colnames(df_nodes)) {
        node_layer_args$color <- node_color_by
        node_color_by_guide <- "none"
    } else {
        node_color_by <- check_columns(df_nodes, node_color_by, force_factor = TRUE)
        node_layer_args$mapping[[length(node_layer_args$mapping) + 1]] <- aes(color = !!sym(node_color_by))
        node_color_by_guide <- "guide"
    }

    if (is.numeric(node_shape_by)) {
        node_layer_args$shape <- node_shape_by
        node_shape_by_guide <- "none"
    } else {
        node_shape_by <- check_columns(df_nodes, node_shape_by, force_factor = TRUE)
        node_layer_args$mapping[[length(node_layer_args$mapping) + 1]] <- aes(shape = as.factor(!!sym(node_shape_by)))
        node_shape_by_guide <- "guide"
    }

    if (!node_fill_by %in% colnames(df_nodes)) {
        node_layer_args$fill <- node_fill_by
        node_fill_by_guide <- "none"
    } else {
        node_fill_by <- check_columns(df_nodes, node_fill_by, force_factor = TRUE)
        node_layer_args$mapping[[length(node_layer_args$mapping) + 1]] <- aes(fill = !!sym(node_fill_by))
        node_fill_by_guide <- "guide"
    }

    # LINKS
    link_layer_args <- list(alpha = link_alpha, check_overlap = TRUE, mapping = list())
    if (isTRUE(directed)) {
        link_layer_args$arrow <- arrow
    }

    if (link_color_by %in% colnames(df_edges) && !link_color_by %in% c("from", "to")) {
        link_color_by_guide <- "guide"
        link_layer_args$mapping[[length(link_layer_args$mapping) + 1]] <- aes(color = !!sym(link_color_by))
    } else {
        link_color_by_guide <- "none"
        if (node_shape_by %in% 21:25) {
            if (node_fill_by_guide == "none") {
                link_layer_args$color <- node_fill_by
            } else {
                link_layer_args$mapping[[length(link_layer_args$mapping) + 1]] <- aes(color = I(!!sym(paste0("node.", node_fill_by))))
                # map node_fill_by from nodes data to links data
                df_edges[[paste0("node.", node_fill_by)]] <- palette_this(
                    levels(df_nodes[[node_fill_by]]), palette = link_palette, palcolor = link_palcolor
                )[df_nodes[df_edges[[link_color_by]], node_fill_by]]
                graph <- igraph::graph_from_data_frame(d = df_edges, vertices = df_nodes, directed = directed)
            }
        } else {
            if (node_color_by_guide == "none") {
                link_layer_args$color <- node_color_by
            } else {
                link_layer_args$mapping[[length(link_layer_args$mapping) + 1]] <- aes(color = I(!!sym(paste0("node.", node_color_by))))
                # map node_color_by from nodes data to links data
                df_edges[[paste0("node.", node_color_by)]] <- palette_this(
                    levels(df_nodes[[node_color_by]]), palette = link_palette, palcolor = link_palcolor
                )[df_nodes[df_edges[[link_color_by]], node_color_by]]
                graph <- igraph::graph_from_data_frame(d = df_edges, vertices = df_nodes, directed = directed)
            }
        }
    }

    if (is.numeric(link_weight_by)) {
        link_layer_args$linewidth <- link_weight_by
        link_weight_by_guide <- "none"
    } else {
        link_layer_args$mapping[[length(link_layer_args$mapping) + 1]] <- aes(linewidth = !!sym(link_weight_by))
        link_weight_by_guide <- "guide"
    }

    if (!link_type_by %in% colnames(df_edges)) {
        link_layer_args$linetype <- link_type_by
        link_type_by_guide <- "none"
    } else {
        if (utils::compareVersion(as.character(utils::packageVersion("ggplot2")), "4.0.0") != 0) {
            # This is fixed in ggplot2 4.0.1
            link_layer_args$mapping[[length(link_layer_args$mapping) + 1]] <- aes(linetype = factor(!!sym(link_type_by)))
            link_type_by_guide <- "guide"
        } else {
            warning("Using `link_type_by` with ggplot2 == 4.0.0 (ggraph <= 2.2.2) is not supported. Set to 'solid'. See https://github.com/thomasp85/ggraph/issues/394 for details.")
            link_layer_args$linetype <- "solid"
            link_type_by_guide <- "none"
        }
    }

    if (isTRUE(directed)) {
        link_layer_args$end_cap <- ggraph::circle(link_arrow_offset, "pt")
    }
    link_loop_layer_args <- link_layer_args
    link_loop_layer_args$mapping[[length(link_loop_layer_args$mapping) + 1]] <- aes(direction = (!!sym("from") - 1) * 360 / length(graph))
    # link_layer_args$data <- df_edges[df_edges$from != df_edges$to, , drop = FALSE]
    link_layer_args$strength <- link_curvature
    link_layer_args$mapping <- Reduce(modify_list, link_layer_args$mapping)
    link_loop_layer_args$mapping <- Reduce(modify_list, link_loop_layer_args$mapping)

    # Start building the plot
    p <- ggraph::ggraph(graph, layout = "manual", x = df_nodes$x, y = df_nodes$y)
    if (cluster != "none") {
        clfun <- getFromNamespace(paste0("cluster_", cluster), "igraph")
        clusters <- clfun(graph)
        df_nodes$cluster <- factor(
            paste0("c", clusters$membership),
            levels = paste0("c", unique(sort(clusters$membership))))
        if (cluster_scale == "fill") {
            if (!identical(node_fill_by, "grey20")) {
                warning("`cluster_scale = 'fill'` overrides `node_fill_by` when 'cluster' is enabled")
            }
            node_fill_by <- "cluster"
        } else if (cluster_scale == "color") {
            if (!identical(node_color_by, "black")) {
                warning("`cluster_scale = 'color'` overrides `node_color_by` when 'cluster' is enabled")
            }
            node_color_by <- "cluster"
        } else if (cluster_scale == "shape") {
            if (!identical(node_shape_by, 21)) {
                warning("`cluster_scale = 'shape'` overrides `node_shape_by` when 'cluster' is enabled")
            }
            node_shape_by <- "cluster"
        }

        mark_type <- match.arg(mark_type)
        mark_fun <- switch(mark_type,
            hull = ggforce::geom_mark_hull,
            ellipse = ggforce::geom_mark_ellipse,
            rect = ggforce::geom_mark_rect,
            circle = ggforce::geom_mark_circle
        )
        p <- p + mark_fun(
            data = df_nodes,
            mapping = aes(x = !!sym("x"), y = !!sym("y"), color = !!sym("cluster"), fill = !!sym("cluster")),
            expand = mark_expand, alpha = mark_alpha, linetype = mark_linetype,
            show.legend = FALSE
        ) +
            scale_fill_manual(values = palette_this(levels(df_nodes$cluster), palette = palette, palcolor = palcolor)) +
            scale_color_manual(values = palette_this(levels(df_nodes$cluster), palette = palette, palcolor = palcolor)) +
            new_scale_fill() +
            new_scale_color()
    }

    ## Combine link layer
    p <- p +
        do.call(ggraph::geom_edge_arc, link_layer_args) +
        do.call(ggraph::geom_edge_loop, link_loop_layer_args)

    if (link_weight_by_guide == "guide") {
        p <- p + ggraph::scale_edge_width_continuous(
            range = link_weight_range, breaks = scales::pretty_breaks(n = 4),
            guide = guide_legend(title = link_weight_name %||% link_weight_by, order = 10))
    }

    if (link_type_by_guide == "guide") {
        p <- p + ggraph::scale_edge_linetype_discrete(
            guide = guide_legend(title = link_type_name %||% link_type_by, order = 11)
        )
    }

    if (link_color_by_guide == "guide") {
        if (is.numeric(df_edges[[link_color_by]])) {
            p <- p + ggraph::scale_edge_color_gradientn(
                n.breaks = 5,
                colors = palette_this(palette = link_palette, palcolor = link_palcolor),
                na.value = "grey80",
                guide = ggraph::guide_edge_colorbar(
                    title = link_color_name %||% link_color_by,
                    frame.colour = "black", ticks.colour = "black", title.hjust = 0, order = 12
                )
            )
        } else {
            lc_values <- if (is.factor(df_edges[[link_color_by]])) {
                levels(df_edges[[link_color_by]])
            } else {
                unique(df_edges[[link_color_by]])
            }
            p <- p + ggraph::scale_edge_color_manual(
                values = palette_this(lc_values, palette = link_palette, palcolor = link_palcolor),
                guide = guide_legend(title = link_color_name %||% link_color_by, order = 12)
            )
        }
    }

    # Compose node layer
    node_layer_args$data <- df_nodes
    node_layer_args$mapping <- Reduce(modify_list, node_layer_args$mapping)
    p <- p + do.call(geom_point, node_layer_args)

    if (node_size_by_guide == "guide") {
        p <- p + scale_size_continuous(
            range = node_size_range, breaks = scales::pretty_breaks(n = 4),
            guide = guide_legend(title = node_size_name %||% node_size_by, order = 1,
                override.aes = list(size = scales::rescale(sort(df_nodes[[node_size_by]]), c(1, 6)))))
    }

    if (node_color_by_guide == "guide") {
        p <- p + scale_color_manual(
            values = palette_this(levels(df_nodes[[node_color_by]]), palette = palette, palcolor = palcolor),
            guide = guide_legend(title = node_color_name %||% node_color_by, order = 2,
                override.aes = list(size = 4))
        )
    }

    if (node_shape_by_guide == "guide") {
        p <- p + scale_shape_manual(
            guide = guide_legend(title = node_shape_name %||% node_shape_by, order = 3,
                override.aes = list(size = 4))
        )
    }

    if (node_fill_by_guide == "guide") {
        p <- p + scale_fill_manual(
            values = palette_this(levels(df_nodes[[node_fill_by]]), palette = palette, palcolor = palcolor, alpha = node_alpha),
            guide = guide_legend(title = node_fill_name %||% node_fill_by, order = 4,
                override.aes = list(size = 4))
        )
    }

    ## NODE LABELS
    if (isTRUE(add_label)) {
        p <- p + geom_text_repel(
            data = df_nodes, mapping = aes(x = !!sym("x"), y = !!sym("y"), label = !!sym("name")),
            segment.color = "transparent",
            point.size = NA, max.overlaps = 100, color = label_fg, bg.color = label_bg, bg.r = label_bg_r,
            size = label_size * text_size_scale
        )
    }

    p <- p +
        scale_x_continuous(expand = c(0, .4)) +
        scale_y_continuous(expand = c(0, .4)) +
        labs(title = title, subtitle = subtitle, x = xlab %||% "", y = ylab %||% "") +
        do.call(theme, theme_args) +
        ggplot2::theme(
            aspect.ratio = aspect.ratio,
            legend.position = legend.position,
            legend.direction = legend.direction,
            legend.key.size = unit(0.5, "cm"),
            legend.key.width = unit(0.8, "cm")
        )

    dims <- calculate_plot_dimensions(
        base_height = 5,
        aspect.ratio = aspect.ratio,
        legend.position = legend.position,
        legend.direction = legend.direction
    )
    if (is.null(dims)) {
        height <- width <- 5
        if (!identical(legend.position, "none")) {
            if (legend.position %in% c("right", "left")) {
                width <- width + 1
            } else if (legend.direction == "horizontal") {
                height <- height + 1
            } else {
                height <- height + 2
            }
        }
    } else {
        height <- dims$height
        width <- dims$width
    }

    attr(p, "height") <- height
    attr(p, "width") <- width

    p
}

#' Network
#'
#' Plot a network graph
#'
#' @inheritParams common_args
#' @inheritParams NetworkAtomic
#' @param split_nodes A logical value specifying whether to split the nodes data.
#'  If TRUE, the nodes data will also be split by the `split_by` column.
#' @return A ggplot object or wrap_plots object or a list of ggplot objects
#' @export
#' @examples
#' \donttest{
#' actors <- data.frame(
#'   name = c("Alice", "Bob", "Cecil", "David", "Esmeralda"),
#'   age = c(48, 33, 45, 34, 21),
#'   shape = c(21, 22, 21, 22, 23),
#'   gender = c("F", "M", "F", "M", "F")
#' )
#' relations <- data.frame(
#'   from = c("Bob", "Cecil", "Cecil", "David", "David", "Esmeralda", "Bob", "Alice",
#'      "Cecil", "David"),
#'   to = c("Alice", "Bob", "Alice", "Alice", "Bob", "Alice", "Bob", "Alice", "Cecil",
#'      "David"),
#'   friendship = c(4, 5, 5, 2, 1, 1, 2, 1, 3, 4),
#'   type = c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2)
#' )
#' Network(relations, actors)
#' Network(relations, actors, theme = "theme_blank", theme_args = list(add_coord = FALSE))
#' Network(relations, actors, link_weight_by = "friendship", node_size_by = "age",
#'  link_weight_name = "FRIENDSHIP", node_fill_by = "gender", link_color_by = "to",
#'  link_type_by = "type", node_color_by = "black", layout = "circle", link_curvature = 0.2)
#' Network(relations, actors, layout = "tree", directed = FALSE, cluster = "fast_greedy",
#'  add_mark = TRUE)
#' Network(relations, actors, split_by = "type")
#' }
Network <- function(
    links, nodes = NULL, split_by = NULL, split_by_sep = "_", split_nodes = FALSE,
    from = NULL, from_sep = "_", to = NULL, to_sep = "_",
    node_by = NULL, node_by_sep = "_", link_weight_by = 2, link_weight_name = NULL,
    link_type_by = "solid", link_type_name = NULL,
    node_size_by = 15, node_size_name = NULL, node_color_by = "black", node_color_name = NULL,
    node_shape_by = 21, node_shape_name = NULL, node_fill_by = "grey20", node_fill_name = NULL,
    link_alpha = 1, node_alpha = 0.95, node_stroke = 1.5, cluster_scale = c("fill", "color", "shape"),
    node_size_range = c(5, 20), link_weight_range = c(0.5, 5), link_arrow_offset = 20,
    link_curvature = 0, link_color_by = "from", link_color_name = NULL, palette = "Paired", palcolor = NULL,
    link_palette = ifelse(link_color_by %in% c("from", "to"), palette, "Set1"),
    link_palcolor = if (link_color_by %in% c("from", "to")) palcolor else NULL,
    directed = TRUE, layout = "circle", cluster = "none", add_mark = FALSE, mark_expand = ggplot2::unit(10, "mm"),
    mark_type = c("hull", "ellipse", "rect", "circle"), mark_alpha = 0.1, mark_linetype = 1, add_label = TRUE,
    label_size = 3, label_fg = "white", label_bg = "black", label_bg_r = 0.1,
    arrow = ggplot2::arrow(type = "closed", length = ggplot2::unit(0.1, "inches")),
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL, aspect.ratio = 1,
    theme = "theme_this", theme_args = list(), legend.position = "right", legend.direction = "vertical",
    seed = 8525, combine = TRUE, nrow = NULL, ncol = NULL, byrow = TRUE,
    axes = NULL, axis_titles = axes, guides = NULL, design = NULL, ...
) {
    validate_common_args(seed = seed)
    theme <- process_theme(theme)
    if (is.null(split_by)) { split_nodes <- FALSE }

    l_split_by <- check_columns(links, split_by, force_factor = TRUE, allow_multi = TRUE,
        concat_multi = TRUE, concat_sep = split_by_sep)
    if (!is.null(l_split_by)) {
        links[[l_split_by]] <- droplevels(links[[l_split_by]])
    }

    if (isTRUE(split_nodes) && !is.null(nodes)) {
        if (is.character(nodes) && length(nodes) == 1 && startsWith(nodes, "@")) {
            nodes <- attr(links, substring(nodes, 2))
        }
        n_split_by <- check_columns(nodes, split_by, force_factor = TRUE, allow_multi = TRUE,
            concat_multi = TRUE, concat_sep = split_by_sep)
        if (!is.null(n_split_by)) {
            nodes[[n_split_by]] <- droplevels(nodes[[n_split_by]])
        }

        if (!identical(l_split_by, n_split_by)) {
            stop("The `split_by` columns in `links` and `nodes` must be the same.")
        }
    }

    if (!is.null(split_by)) {
        linkss <- split(links, links[[l_split_by]])
        if (isTRUE(split_nodes) && !is.null(nodes)) {
            nodess <- split(nodes, nodes[[n_split_by]])
            nms <- names(nodess)
            linkss < lapply(nms, function(nm) {
                dat <- linkss[[nm]]
                attr(dat, "nodes") <- nodess[[nm]]
                dat
            })
            names(linkss) <- nms
        } else {
            linkss <- linkss[levels(links[[l_split_by]])]
        }
    } else {
        linkss <- list(links)
        names(linkss) <- "..."
    }

    if (isTRUE(split_nodes) && !is.null(nodes)) {
        nodes <- "@nodes"
    }

    plots <- lapply(names(linkss), function(nm) {
        default_title <- if (length(linkss) == 1 && identical(nm, "...")) NULL else nm
        if (is.function(title)) {
            title <- title(default_title)
        } else {
            title <- title %||% default_title
        }

        NetworkAtomic(
            links = linkss[[nm]], nodes = nodes, from = from, from_sep = from_sep,
            to = to, to_sep = to_sep, node_by = node_by, node_by_sep = node_by_sep,
            link_weight_by = link_weight_by, link_weight_name = link_weight_name,
            link_type_by = link_type_by, link_type_name = link_type_name,
            node_size_by = node_size_by, node_size_name = node_size_name,
            node_color_by = node_color_by, node_color_name = node_color_name,
            node_shape_by = node_shape_by, node_shape_name = node_shape_name,
            node_fill_by = node_fill_by, node_fill_name = node_fill_name,
            link_alpha = link_alpha, node_alpha = node_alpha, node_stroke = node_stroke,
            cluster_scale = cluster_scale, node_size_range = node_size_range,
            link_weight_range = link_weight_range, link_arrow_offset = link_arrow_offset,
            link_curvature = link_curvature, link_color_by = link_color_by, link_color_name = link_color_name,
            palette = palette, palcolor = palcolor, link_palette = link_palette, link_palcolor = link_palcolor,
            directed = directed, layout = layout, cluster = cluster, add_mark = add_mark, mark_expand = mark_expand,
            mark_type = mark_type, mark_alpha = mark_alpha, mark_linetype = mark_linetype,
            add_label = add_label, label_size = label_size, label_fg = label_fg,
            label_bg = label_bg, label_bg_r = label_bg_r, arrow = arrow,
            title = title, subtitle = subtitle, xlab = xlab, ylab = ylab,
            aspect.ratio = aspect.ratio, theme = theme, theme_args = theme_args,
            legend.position = legend.position, legend.direction = legend.direction, ...
        )
    })

    combine_plots(plots, combine = combine, nrow = nrow, ncol = ncol, byrow = byrow,
        axes = axes, axis_titles = axis_titles, guides = guides, design = design)
}
