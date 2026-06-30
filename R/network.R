#' NetworkAtomic
#'
#' @description
#' Core implementation for rendering a single network graph from a links
#' (edge list) data frame and an optional nodes (vertex metadata) data frame.
#' This is the workhorse behind the exported \code{\link{Network}} function --
#' it takes a **single** pair of links/nodes data frames (no
#' \code{split_by} support) and returns a \code{ggplot} object.
#'
#' The graph is constructed via \code{\link[igraph]{graph_from_data_frame}},
#' laid out using igraph layout algorithms (\code{"circle"}, \code{"tree"},
#' \code{"grid"}, or any named igraph layout such as \code{"fr"} or
#' \code{"kk"}), and rendered with \code{\link[ggraph]{ggraph}} using
#' \code{\link[ggraph]{geom_edge_arc}} for links and
#' \code{\link[ggplot2]{geom_point}} for nodes.
#'
#' Key features include:
#' \itemize{
#'   \item \strong{Directed / undirected graphs} with optional arrow heads.
#'   \item \strong{Link styling}: variable width (weight), linetype, and
#'         colour, each constant or mapped from a column. Edge colours
#'         can follow source nodes, target nodes, or a dedicated column.
#'   \item \strong{Node styling}: variable size, shape, colour, and fill,
#'         each constant or column-mapped.
#'   \item \strong{Community detection} via igraph clustering algorithms
#'         with convex hull, ellipse, rectangle, or circle marks from
#'         \code{\link[ggforce]{ggforce}}.
#'   \item \strong{Automatic labels} via
#'         \code{\link[ggrepel]{geom_text_repel}}.
#'   \item \strong{Self-loop edges} drawn as direction-sensitive arcs.
#' }
#'
#' @section Architecture:
#' \enumerate{
#'   \item \strong{Column resolution} -- Default column names for
#'         \code{from}, \code{to}, and \code{node_by} are assigned when
#'         \code{NULL}, using the first or second column of the respective
#'         data frame. Each is validated via \code{\link{check_columns}};
#'         multi-column inputs are concatenated with their respective
#'         separator (\code{from_sep}, \code{to_sep}, \code{node_by_sep})
#'         and force-converted to factors. Resolved columns are renamed to
#'         the canonical names \code{"from"}, \code{"to"}, and \code{"name"}.
#'   \item \strong{Graph construction} -- An \code{\link[igraph]{igraph}}
#'         object is built via \code{\link[igraph]{graph_from_data_frame}}
#'         using the links data frame and (optionally) the nodes data frame.
#'         If \code{nodes} is a string starting with \code{"@"}, it is
#'         extracted from the corresponding attribute on the links object.
#'   \item \strong{Layout computation} -- The layout is resolved:
#'         \code{"circle"} -> \code{\link[igraph]{layout_in_circle}},
#'         \code{"tree"} -> \code{\link[igraph]{layout_as_tree}},
#'         \code{"grid"} -> \code{\link[igraph]{layout_on_grid}}.
#'         Any other character string is prefixed with \code{"layout_with_"}
#'         and looked up in the igraph namespace (e.g. \code{"fr"} ->
#'         \code{layout_with_fr}). If \code{layout} is already an
#'         \code{igraph_layout_spec} object it is applied directly via
#'         \code{\link[igraph]{layout_}}.
#'   \item \strong{Data extraction} -- Vertex and edge data frames are
#'         extracted from the igraph object via
#'         \code{\link[igraph]{as_data_frame}}. Layout coordinates are
#'         added as \code{x} and \code{y} columns to the vertex data.
#'   \item \strong{Node aesthetic assembly} -- Each of \code{size},
#'         \code{color}, \code{shape}, and \code{fill} is resolved as
#'         either a constant value (numeric or character) or a mapping to
#'         a column of the node data. A per-aesthetic \code{"guide"} /
#'         \code{"none"} flag is tracked for legend construction.
#'   \item \strong{Link aesthetic assembly} -- \code{linewidth} (weight),
#'         \code{linetype}, and \code{color} are resolved similarly.
#'         \itemize{
#'           \item When \code{link_color_by = "from"}, edge colours are
#'                 derived from the source node's fill (if node shape is
#'                 filled, 21--25) or colour; when \code{"to"}, from the
#'                 target node.
#'           \item When \code{link_color_by} names a column in the links
#'                 data frame (other than \code{"from"}/\code{"to"}), it
#'                 is mapped directly.
#'         }
#'         If \code{directed = TRUE}, a \code{\link[ggplot2]{arrow}} is
#'         added and \code{end_cap} is set via \code{\link[ggraph]{circle}}
#'         to prevent arrow overlap with nodes. Self-loop edges receive a
#'         \code{direction} aesthetic.
#'   \item \strong{Plot initialisation} -- A \code{\link[ggraph]{ggraph}}
#'         plot is created with \code{layout = "manual"} and vertex
#'         coordinates from the layout.
#'   \item \strong{Clustering and marks} -- When \code{cluster != "none"},
#'         the specified igraph community-detection algorithm is run
#'         (\code{cluster_fast_greedy}, \code{cluster_walktrap},
#'         \code{cluster_edge_betweenness}, \code{cluster_infomap}, or a
#'         custom function). Membership overrides the aesthetic specified
#'         by \code{cluster_scale} (\code{"fill"}, \code{"color"}, or
#'         \code{"shape"}) with a warning that the previous setting is
#'         discarded. If \code{add_mark = TRUE}, a mark enclosure
#'         (hull / ellipse / rect / circle) is drawn per cluster via the
#'         corresponding \code{\link[ggforce]{ggforce}} geom.
#'   \item \strong{Link rendering} -- \code{\link[ggraph]{geom_edge_arc}}
#'         draws the edges (with configurable \code{link_curvature}) and
#'         \code{\link[ggraph]{geom_edge_loop}} handles self-loop edges.
#'   - \item \strong{Link scales} -- Conditional on their guide status,
#'         \code{scale_edge_width_continuous},
#'         \code{scale_edge_linetype_discrete}, and
#'         \code{scale_edge_color_manual} /
#'         \code{scale_edge_color_gradientn} are added for weight,
#'         linetype, and colour legends respectively.
#'   \item \strong{Node rendering} -- \code{\link[ggplot2]{geom_point}}
#'         draws the nodes with the assembled aesthetics.
#'   \item \strong{Node scales} -- Conditional scale additions:
#'         \code{scale_size_continuous} (range from
#'         \code{node_size_range}), \code{scale_color_manual},
#'         \code{scale_shape_manual}, and \code{scale_fill_manual}, each
#'         with their legend title and guide overrides.
#'   \item \strong{Labels} -- When \code{add_label = TRUE}, node
#'         identifiers are rendered via
#'         \code{\link[ggrepel]{geom_text_repel}} using
#'         \code{label_fg}, \code{label_bg}, and \code{label_bg_r}.
#'   \item \strong{Final theme and dimensions} -- Coordinate expansion,
#'         axis labels, theme application, and legend positioning are
#'         applied. Plot height and width are computed via
#'         \code{\link{calculate_plot_dimensions}} and stored as
#'         attributes.
#' }
#'
#' @inheritParams common_args
#' @param links A data frame containing the edge list. Must contain the
#'  \code{from} and \code{to} columns specifying source and target node
#'  identifiers. Additional columns can be referenced by other parameters
#'  (e.g., \code{link_weight_by}, \code{link_type_by},
#'  \code{link_color_by}).
#' @param nodes An optional data frame of node metadata. When provided,
#'  columns such as \code{node_size_by}, \code{node_color_by},
#'  \code{node_shape_by}, and \code{node_fill_by} can reference its
#'  columns. When \code{NULL}, the node set is inferred from the unique
#'  values in the \code{from} and \code{to} columns. If a single character
#'  string starting with \code{"@"}, the nodes data frame is extracted
#'  from the corresponding attribute of \code{links} (e.g.
#'  \code{"@nodes"} extracts \code{attr(links, "nodes")}).
#' @param from A character string specifying the column name in
#'  \code{links} for the source node identifiers. Defaults to
#'  \code{"from"}, or the first column of \code{links} if that column
#'  name does not exist. Multiple columns can be provided; they are
#'  concatenated with \code{from_sep}.
#' @param from_sep A character string to join multiple \code{from}
#'  columns. Default \code{"_"}. Ignored when \code{from} is a single
#'  column.
#' @param to A character string specifying the column name in
#'  \code{links} for the target node identifiers. Defaults to
#'  \code{"to"}, or the second column of \code{links} if that column
#'  name does not exist. Multiple columns can be provided; they are
#'  concatenated with \code{to_sep}.
#' @param to_sep A character string to join multiple \code{to} columns.
#'  Default \code{"_"}. Ignored when \code{to} is a single column.
#' @param node_by A character string specifying the column name in
#'  \code{nodes} for the node identifiers. These must match the values
#'  in the \code{from} / \code{to} columns of \code{links}. Defaults to
#'  \code{"name"}, or the first column of \code{nodes} if that column
#'  name does not exist. Multiple columns can be provided; they are
#'  concatenated with \code{node_by_sep}.
#' @param node_by_sep A character string to join multiple
#'  \code{node_by} columns. Default \code{"_"}. Ignored when
#'  \code{node_by} is a single column.
#' @param link_weight_by A numeric value or a character string. If
#'  numeric, all edges receive that constant line width. If a column
#'  name, the edge line width is mapped to that column. Default
#'  \code{2}.
#' @param link_weight_name A character string for the link weight legend
#'  title. When \code{NULL} (default), the column name from
#'  \code{link_weight_by} is used. Only relevant when
#'  \code{link_weight_by} is a column name.
#' @param link_type_by A character string or a column name specifying
#'  the edge linetype. Can be \code{"solid"}, \code{"dashed"},
#'  \code{"dotted"}, etc. If a column name from \code{links} is
#'  supplied, the linetype is mapped to that column (with a version
#'  check for ggplot2 4.0.0, where mapping is unsupported and a
#'  warning is issued). Default \code{"solid"}.
#' @param link_type_name A character string for the link linetype legend
#'  title. When \code{NULL} (default), the column name from
#'  \code{link_type_by} is used. Only relevant when
#'  \code{link_type_by} is a column name.
#' @param node_size_by A numeric value or a character string. If
#'  numeric, all nodes receive that constant point size. If a column
#'  name, the size is mapped to that column. Default \code{15}.
#' @param node_size_name A character string for the node size legend
#'  title. When \code{NULL} (default), the column name from
#'  \code{node_size_by} is used. Only relevant when
#'  \code{node_size_by} is a column name.
#' @param node_color_by A character string specifying the node colour.
#'  If a colour name or hex code (e.g. \code{"black"}), all nodes
#'  receive that constant colour. If a column name from \code{nodes} is
#'  supplied, the colour is mapped to that column. Default
#'  \code{"black"}.
#' @param node_color_name A character string for the node colour legend
#'  title. When \code{NULL} (default), the column name from
#'  \code{node_color_by} is used. Only relevant when
#'  \code{node_color_by} is a column name.
#' @param node_shape_by A numeric value or a character string. If
#'  numeric, all nodes receive that constant shape (see
#'  \code{\link[ggplot2]{shape}}). If a column name, the shape is
#'  mapped to that column (cast to factor). Default \code{21} (filled
#'  circle with border).
#' @param node_shape_name A character string for the node shape legend
#'  title. When \code{NULL} (default), the column name from
#'  \code{node_shape_by} is used. Only relevant when
#'  \code{node_shape_by} is a column name.
#' @param node_fill_by A character string specifying the node fill
#'  colour. If a colour name or hex code (e.g. \code{"grey20"}), all
#'  nodes receive that constant fill. If a column name from
#'  \code{nodes} is supplied, the fill is mapped to that column.
#'  Default \code{"grey20"}.
#' @param node_fill_name A character string for the node fill legend
#'  title. When \code{NULL} (default), the column name from
#'  \code{node_fill_by} is used. Only relevant when
#'  \code{node_fill_by} is a column name.
#' @param link_alpha A numeric value specifying the transparency
#'  (alpha) of the edge lines. Between \code{0} (invisible) and
#'  \code{1} (opaque). Default \code{1}.
#' @param node_alpha A numeric value specifying the fill transparency
#'  of the nodes. Only applies when \code{node_shape_by} is one of the
#'  filled shapes (21--25). Default \code{0.95}.
#' @param node_stroke A numeric value specifying the border stroke
#'  width of the node points. Default \code{1.5}.
#' @param cluster_scale A character string specifying which node
#'  aesthetic is overridden by cluster membership. One of
#'  \code{"fill"}, \code{"color"}, or \code{"shape"}. The value is
#'  matched via \code{\link{match.arg}}; default is \code{"fill"}.
#' @param node_size_range A numeric vector of length 2 giving the
#'  minimum and maximum node size (in ggplot2 point units) when
#'  \code{node_size_by} is a column name. Default \code{c(5, 20)}.
#' @param link_weight_range A numeric vector of length 2 giving the
#'  minimum and maximum edge line width (in mm) when
#'  \code{link_weight_by} is a column name. Default
#'  \code{c(0.5, 5)}.
#' @param link_arrow_offset A numeric value (in points) specifying the
#'  offset distance for the arrow end cap from the target node.
#'  Prevents arrow heads from overlapping the node points. Only
#'  relevant when \code{directed = TRUE}. Default \code{20}.
#' @param link_curvature A numeric value controlling the curvature of
#'  the edges. \code{0} (default) produces straight edges; positive
#'  values curve them away from the direct path.
#' @param link_color_by A character string controlling how edge colour
#'  is determined. Options:
#'  \itemize{
#'    \item \code{"from"} (default) -- colour follows the source node's
#'          fill or colour aesthetic.
#'    \item \code{"to"} -- colour follows the target node's fill or
#'          colour.
#'    \item A column name from \code{links} -- colour is mapped
#'          directly to that column.
#'  }
#' @param link_color_name A character string for the edge colour legend
#'  title. Only used when \code{link_color_by} is a column name (not
#'  \code{"from"} or \code{"to"}). When \code{NULL} (default), the
#'  column name is used.
#' @param link_palette A character string specifying the palette for
#'  edge colours when they are mapped. When \code{link_color_by} is
#'  \code{"from"} or \code{"to"}, defaults to the node
#'  \code{palette}. Otherwise defaults to \code{"Set1"}.
#' @param link_palcolor A character vector specifying custom colours
#'  for the edge palette. When \code{link_color_by} is \code{"from"}
#'  or \code{"to"}, defaults to the node \code{palcolor}. Otherwise
#'  defaults to \code{NULL}.
#' @param directed A logical value. When \code{TRUE}, edges are drawn
#'  with arrow heads and an end-cap offset. Default \code{TRUE}.
#' @param layout A character string or an \code{igraph_layout_spec}
#'  object specifying the node placement algorithm. Built-in shortcuts:
#'  \code{"circle"} (circular layout), \code{"tree"} (hierarchical
#'  tree), \code{"grid"} (grid layout). Any other string is prefixed
#'  with \code{"layout_with_"} and called as an igraph function (e.g.
#'  \code{"fr"} for Fruchterman--Reingold, \code{"kk"} for
#'  Kamada--Kawai). Default \code{"circle"}.
#' @param cluster A character string specifying the community detection
#'  algorithm. One of \code{"none"}, \code{"fast_greedy"},
#'  \code{"walktrap"}, \code{"edge_betweenness"}, \code{"infomap"}, or
#'  a custom clustering function from igraph. When not \code{"none"},
#'  cluster membership overrides the aesthetic selected by
#'  \code{cluster_scale}. Default \code{"none"}.
#' @param add_mark A logical value. When \code{TRUE} (and
#'  \code{cluster != "none"}), an enclosure mark is drawn around each
#'  cluster's nodes. Default \code{FALSE}.
#' @param mark_expand A \code{\link[grid]{unit}} object specifying the
#'  extra space around points within a cluster mark. Default
#'  \code{unit(10, "mm")}.
#' @param mark_type A character string specifying the mark geometry.
#'  One of \code{"hull"}, \code{"ellipse"}, \code{"rect"}, or
#'  \code{"circle"}, corresponding to ggforce's \code{geom_mark_hull},
#'  \code{geom_mark_ellipse}, \code{geom_mark_rect}, and
#'  \code{geom_mark_circle}. The value is matched via
#'  \code{\link{match.arg}}; default is \code{"hull"}.
#' @param mark_alpha A numeric value for the fill transparency of
#'  cluster marks. Default \code{0.1}.
#' @param mark_linetype A numeric or character value specifying the
#'  border line type of the cluster marks. Default \code{1} (solid).
#' @param add_label A logical value. When \code{TRUE} (default), node
#'  identifiers are drawn as repulsive text labels via
#'  \code{\link[ggrepel]{geom_text_repel}}.
#' @param label_size A numeric value for the font size of node labels.
#'  Scaled by the theme base size. Default \code{3}.
#' @param label_fg A character string specifying the text colour of
#'  node labels. Default \code{"white"}.
#' @param label_bg A character string specifying the background colour
#'  of node labels. Default \code{"black"}.
#' @param label_bg_r A numeric value specifying the background box
#'  radius (as a fraction of label height). Passed to
#'  \code{\link[ggrepel]{geom_text_repel}}'s \code{bg.r} argument.
#'  Default \code{0.1}.
#' @param arrow A \code{\link[ggplot2]{arrow}} object for the link
#'  arrow heads. Only used when \code{directed = TRUE}. Default is
#'  \code{arrow(type = "closed", length = unit(0.1, "inches"))}.
#' @param ... Not used.
#' @return A \code{ggplot} object with \code{height} and \code{width}
#'  attributes (in inches).
#' @keywords internal
#' @importFrom utils getFromNamespace
#' @importFrom dplyr %>% rename relocate
#' @importFrom ggplot2 aes geom_point scale_x_continuous scale_y_continuous
#' @importFrom ggplot2 scale_size_continuous guide_legend
#' @importFrom ggrepel geom_text_repel
#' @importFrom ggnewscale new_scale_color
NetworkAtomic <- function(
    links,
    nodes = NULL,
    from = NULL,
    from_sep = "_",
    to = NULL,
    to_sep = "_",
    node_by = NULL,
    node_by_sep = "_",
    link_weight_by = 2,
    link_weight_name = NULL,
    link_type_by = "solid",
    link_type_name = NULL,
    node_size_by = 15,
    node_size_name = NULL,
    node_color_by = "black",
    node_color_name = NULL,
    node_shape_by = 21,
    node_shape_name = NULL,
    node_fill_by = "grey20",
    node_fill_name = NULL,
    link_alpha = 1,
    node_alpha = 0.95,
    node_stroke = 1.5,
    cluster_scale = c("fill", "color", "shape"),
    node_size_range = c(5, 20),
    link_weight_range = c(0.5, 5),
    link_arrow_offset = 20,
    link_curvature = 0,
    link_color_by = "from",
    link_color_name = NULL,
    palette = "Paired",
    palcolor = NULL,
    palreverse = FALSE,
    link_palette = ifelse(link_color_by %in% c("from", "to"), palette, "Set1"),
    link_palcolor = if (link_color_by %in% c("from", "to")) palcolor else NULL,
    directed = TRUE,
    layout = "circle",
    cluster = "none",
    add_mark = FALSE,
    mark_expand = ggplot2::unit(10, "mm"),
    mark_type = c("hull", "ellipse", "rect", "circle"),
    mark_alpha = 0.1,
    mark_linetype = 1,
    add_label = TRUE,
    label_size = 3,
    label_fg = "white",
    label_bg = "black",
    label_bg_r = 0.1,
    arrow = ggplot2::arrow(
        type = "closed",
        length = ggplot2::unit(0.1, "inches")
    ),
    title = NULL,
    subtitle = NULL,
    xlab = NULL,
    ylab = NULL,
    aspect.ratio = 1,
    theme = "theme_this",
    theme_args = list(),
    legend.position = "right",
    legend.direction = "vertical",
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

    from <- check_columns(
        links,
        from,
        force_factor = TRUE,
        allow_multi = TRUE,
        concat_multi = TRUE,
        concat_sep = from_sep
    )
    to <- check_columns(
        links,
        to,
        force_factor = TRUE,
        allow_multi = TRUE,
        concat_multi = TRUE,
        concat_sep = to_sep
    )
    links <- links %>%
        rename(from = from, to = to) %>%
        relocate(!!sym("from"), !!sym("to"))

    if (!is.null(nodes)) {
        node_by <- check_columns(
            nodes,
            node_by,
            force_factor = TRUE,
            allow_multi = TRUE,
            concat_multi = TRUE,
            concat_sep = node_by_sep
        )
        nodes <- nodes %>% rename(name = node_by) %>% relocate(!!sym("name"))
    }

    graph <- igraph::graph_from_data_frame(
        d = links,
        vertices = nodes,
        directed = directed
    )
    if (inherits(layout, "igraph_layout_spec")) {
        layout <- igraph::layout_(graph, layout)
    } else {
        if (layout %in% c("circle", "tree", "grid")) {
            layout <- switch(
                layout,
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
    node_layer_args <- list(
        mapping = list(aes(x = !!sym("x"), y = !!sym("y"))),
        stroke = node_stroke
    )

    if (is.numeric(node_size_by)) {
        node_layer_args$size <- node_size_by
        node_size_by_guide <- "none"
    } else {
        node_layer_args$mapping[[length(node_layer_args$mapping) + 1]] <- aes(
            size = !!sym(node_size_by)
        )
        node_size_by_guide <- "guide"
    }

    if (!node_color_by %in% colnames(df_nodes)) {
        node_layer_args$color <- node_color_by
        node_color_by_guide <- "none"
    } else {
        node_color_by <- check_columns(
            df_nodes,
            node_color_by,
            force_factor = TRUE
        )
        node_layer_args$mapping[[length(node_layer_args$mapping) + 1]] <- aes(
            color = !!sym(node_color_by)
        )
        node_color_by_guide <- "guide"
    }

    if (is.numeric(node_shape_by)) {
        node_layer_args$shape <- node_shape_by
        node_shape_by_guide <- "none"
    } else {
        node_shape_by <- check_columns(
            df_nodes,
            node_shape_by,
            force_factor = TRUE
        )
        node_layer_args$mapping[[length(node_layer_args$mapping) + 1]] <- aes(
            shape = as.factor(!!sym(node_shape_by))
        )
        node_shape_by_guide <- "guide"
    }

    if (!node_fill_by %in% colnames(df_nodes)) {
        node_layer_args$fill <- node_fill_by
        node_fill_by_guide <- "none"
    } else {
        node_fill_by <- check_columns(
            df_nodes,
            node_fill_by,
            force_factor = TRUE
        )
        node_layer_args$mapping[[length(node_layer_args$mapping) + 1]] <- aes(
            fill = !!sym(node_fill_by)
        )
        node_fill_by_guide <- "guide"
    }

    # LINKS
    link_layer_args <- list(
        alpha = link_alpha,
        check_overlap = TRUE,
        mapping = list()
    )
    if (isTRUE(directed)) {
        link_layer_args$arrow <- arrow
    }

    if (
        link_color_by %in%
            colnames(df_edges) &&
            !link_color_by %in% c("from", "to")
    ) {
        link_color_by_guide <- "guide"
        link_layer_args$mapping[[length(link_layer_args$mapping) + 1]] <- aes(
            color = !!sym(link_color_by)
        )
    } else {
        link_color_by_guide <- "none"
        if (node_shape_by %in% 21:25) {
            if (node_fill_by_guide == "none") {
                link_layer_args$color <- node_fill_by
            } else {
                link_layer_args$mapping[[
                    length(link_layer_args$mapping) + 1
                ]] <- aes(color = I(!!sym(paste0("node.", node_fill_by))))
                # map node_fill_by from nodes data to links data
                df_edges[[paste0("node.", node_fill_by)]] <- palette_this(
                    levels(df_nodes[[node_fill_by]]),
                    palette = link_palette,
                    palcolor = link_palcolor,
                    reverse = palreverse
                )[df_nodes[df_edges[[link_color_by]], node_fill_by]]
                graph <- igraph::graph_from_data_frame(
                    d = df_edges,
                    vertices = df_nodes,
                    directed = directed
                )
            }
        } else {
            if (node_color_by_guide == "none") {
                link_layer_args$color <- node_color_by
            } else {
                link_layer_args$mapping[[
                    length(link_layer_args$mapping) + 1
                ]] <- aes(color = I(!!sym(paste0("node.", node_color_by))))
                # map node_color_by from nodes data to links data
                df_edges[[paste0("node.", node_color_by)]] <- palette_this(
                    levels(df_nodes[[node_color_by]]),
                    palette = link_palette,
                    palcolor = link_palcolor,
                    reverse = palreverse
                )[df_nodes[df_edges[[link_color_by]], node_color_by]]
                graph <- igraph::graph_from_data_frame(
                    d = df_edges,
                    vertices = df_nodes,
                    directed = directed
                )
            }
        }
    }

    if (is.numeric(link_weight_by)) {
        link_layer_args$linewidth <- link_weight_by
        link_weight_by_guide <- "none"
    } else {
        link_layer_args$mapping[[length(link_layer_args$mapping) + 1]] <- aes(
            linewidth = !!sym(link_weight_by)
        )
        link_weight_by_guide <- "guide"
    }

    if (!link_type_by %in% colnames(df_edges)) {
        link_layer_args$linetype <- link_type_by
        link_type_by_guide <- "none"
    } else {
        if (
            utils::compareVersion(
                as.character(utils::packageVersion("ggplot2")),
                "4.0.0"
            ) !=
                0
        ) {
            # This is fixed in ggplot2 4.0.1
            link_layer_args$mapping[[
                length(link_layer_args$mapping) + 1
            ]] <- aes(linetype = factor(!!sym(link_type_by)))
            link_type_by_guide <- "guide"
        } else {
            warning(
                "Using `link_type_by` with ggplot2 == 4.0.0 (ggraph <= 2.2.2) is not supported. Set to 'solid'. See https://github.com/thomasp85/ggraph/issues/394 for details."
            )
            link_layer_args$linetype <- "solid"
            link_type_by_guide <- "none"
        }
    }

    if (isTRUE(directed)) {
        link_layer_args$end_cap <- ggraph::circle(link_arrow_offset, "pt")
    }
    link_loop_layer_args <- link_layer_args
    link_loop_layer_args$mapping[[
        length(link_loop_layer_args$mapping) + 1
    ]] <- aes(direction = (!!sym("from") - 1) * 360 / length(graph))
    # link_layer_args$data <- df_edges[df_edges$from != df_edges$to, , drop = FALSE]
    link_layer_args$strength <- link_curvature
    link_layer_args$mapping <- Reduce(modify_list, link_layer_args$mapping)
    link_loop_layer_args$mapping <- Reduce(
        modify_list,
        link_loop_layer_args$mapping
    )

    # Start building the plot
    p <- ggraph::ggraph(
        graph,
        layout = "manual",
        x = df_nodes$x,
        y = df_nodes$y
    )
    if (cluster != "none") {
        clfun <- getFromNamespace(paste0("cluster_", cluster), "igraph")
        clusters <- clfun(graph)
        df_nodes$cluster <- factor(
            paste0("c", clusters$membership),
            levels = paste0("c", unique(sort(clusters$membership)))
        )
        if (cluster_scale == "fill") {
            if (!identical(node_fill_by, "grey20")) {
                warning(
                    "`cluster_scale = 'fill'` overrides `node_fill_by` when 'cluster' is enabled"
                )
            }
            node_fill_by <- "cluster"
        } else if (cluster_scale == "color") {
            if (!identical(node_color_by, "black")) {
                warning(
                    "`cluster_scale = 'color'` overrides `node_color_by` when 'cluster' is enabled"
                )
            }
            node_color_by <- "cluster"
        } else if (cluster_scale == "shape") {
            if (!identical(node_shape_by, 21)) {
                warning(
                    "`cluster_scale = 'shape'` overrides `node_shape_by` when 'cluster' is enabled"
                )
            }
            node_shape_by <- "cluster"
        }

        mark_type <- match.arg(mark_type)
        mark_fun <- switch(
            mark_type,
            hull = ggforce::geom_mark_hull,
            ellipse = ggforce::geom_mark_ellipse,
            rect = ggforce::geom_mark_rect,
            circle = ggforce::geom_mark_circle
        )
        p <- p +
            mark_fun(
                data = df_nodes,
                mapping = aes(
                    x = !!sym("x"),
                    y = !!sym("y"),
                    color = !!sym("cluster"),
                    fill = !!sym("cluster")
                ),
                expand = mark_expand,
                alpha = mark_alpha,
                linetype = mark_linetype,
                show.legend = FALSE
            ) +
            scale_fill_manual(
                values = palette_this(
                    levels(df_nodes$cluster),
                    palette = palette,
                    palcolor = palcolor,
                    reverse = palreverse
                )
            ) +
            scale_color_manual(
                values = palette_this(
                    levels(df_nodes$cluster),
                    palette = palette,
                    palcolor = palcolor,
                    reverse = palreverse
                )
            ) +
            new_scale_fill() +
            new_scale_color()
    }

    ## Combine link layer
    p <- p +
        do_call(ggraph::geom_edge_arc, link_layer_args) +
        do_call(ggraph::geom_edge_loop, link_loop_layer_args)

    if (link_weight_by_guide == "guide") {
        p <- p +
            ggraph::scale_edge_width_continuous(
                range = link_weight_range,
                breaks = scales::pretty_breaks(n = 4),
                guide = guide_legend(
                    title = link_weight_name %||% link_weight_by,
                    order = 10
                )
            )
    }

    if (link_type_by_guide == "guide") {
        p <- p +
            ggraph::scale_edge_linetype_discrete(
                guide = guide_legend(
                    title = link_type_name %||% link_type_by,
                    order = 11
                )
            )
    }

    if (link_color_by_guide == "guide") {
        if (is.numeric(df_edges[[link_color_by]])) {
            p <- p +
                ggraph::scale_edge_color_gradientn(
                    n.breaks = 5,
                    colors = palette_this(
                        palette = link_palette,
                        palcolor = link_palcolor,
                        reverse = palreverse
                    ),
                    na.value = "grey80",
                    guide = ggraph::guide_edge_colorbar(
                        title = link_color_name %||% link_color_by,
                        frame.colour = "black",
                        ticks.colour = "black",
                        title.hjust = 0,
                        order = 12
                    )
                )
        } else {
            lc_values <- if (is.factor(df_edges[[link_color_by]])) {
                levels(df_edges[[link_color_by]])
            } else {
                unique(df_edges[[link_color_by]])
            }
            p <- p +
                ggraph::scale_edge_color_manual(
                    values = palette_this(
                        lc_values,
                        palette = link_palette,
                        palcolor = link_palcolor,
                        reverse = palreverse
                    ),
                    guide = guide_legend(
                        title = link_color_name %||% link_color_by,
                        order = 12
                    )
                )
        }
    }

    # Compose node layer
    node_layer_args$data <- df_nodes
    node_layer_args$mapping <- Reduce(modify_list, node_layer_args$mapping)
    p <- p + do_call(geom_point, node_layer_args)

    if (node_size_by_guide == "guide") {
        p <- p +
            scale_size_continuous(
                range = node_size_range,
                breaks = scales::pretty_breaks(n = 4),
                guide = guide_legend(
                    title = node_size_name %||% node_size_by,
                    order = 1,
                    override.aes = list(
                        size = scales::rescale(
                            sort(df_nodes[[node_size_by]]),
                            c(1, 6)
                        )
                    )
                )
            )
    }

    if (node_color_by_guide == "guide") {
        p <- p +
            scale_color_manual(
                values = palette_this(
                    levels(df_nodes[[node_color_by]]),
                    palette = palette,
                    palcolor = palcolor,
                    reverse = palreverse
                ),
                guide = guide_legend(
                    title = node_color_name %||% node_color_by,
                    order = 2,
                    override.aes = list(size = 4)
                )
            )
    }

    if (node_shape_by_guide == "guide") {
        p <- p +
            scale_shape_manual(
                guide = guide_legend(
                    title = node_shape_name %||% node_shape_by,
                    order = 3,
                    override.aes = list(size = 4)
                )
            )
    }

    if (node_fill_by_guide == "guide") {
        p <- p +
            scale_fill_manual(
                values = palette_this(
                    levels(df_nodes[[node_fill_by]]),
                    palette = palette,
                    palcolor = palcolor,
                    alpha = node_alpha,
                    reverse = palreverse
                ),
                guide = guide_legend(
                    title = node_fill_name %||% node_fill_by,
                    order = 4,
                    override.aes = list(size = 4)
                )
            )
    }

    ## NODE LABELS
    if (isTRUE(add_label)) {
        p <- p +
            geom_text_repel(
                data = df_nodes,
                mapping = aes(
                    x = !!sym("x"),
                    y = !!sym("y"),
                    label = !!sym("name")
                ),
                segment.color = "transparent",
                point.size = NA,
                max.overlaps = 100,
                color = label_fg,
                bg.color = label_bg,
                bg.r = label_bg_r,
                size = label_size * text_size_scale
            )
    }

    p <- p +
        scale_x_continuous(expand = c(0, .4)) +
        scale_y_continuous(expand = c(0, .4)) +
        labs(
            title = title,
            subtitle = subtitle,
            x = xlab %||% "",
            y = ylab %||% ""
        ) +
        do_call(theme, theme_args) +
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

    attr(p, "height") <- dims$height
    attr(p, "width") <- dims$width

    p
}

#' Network
#'
#' @description
#' Draws a network graph from a links (edge list) data frame and an optional
#' nodes (vertex metadata) data frame. The graph is constructed via
#' \code{\link[igraph]{igraph}}, laid out with igraph layout algorithms, and
#' rendered with \code{\link[ggraph]{ggraph}}. Supports directed or
#' undirected edges, variable link widths/linetypes/colours, node
#' sizes/shapes/colours/fills, community detection with enclosure marks,
#' automatic node labels, and a wide range of layout options.
#'
#' When \code{links} (and optionally \code{nodes}) contain a
#' \code{split_by} column, separate sub-plots are generated for each split
#' level and combined via \code{\link[patchwork]{patchwork}}. Unlike most
#' other plot types, \code{Network} operates on two data frames; splitting
#' may affect both.
#'
#' @section split_by workflow:
#' When \code{split_by} is provided:
#' \enumerate{
#'   \item \strong{Column validation} -- The \code{split_by} column is
#'         validated in \code{links} via \code{\link{check_columns}},
#'         force-converted to a factor, and empty levels are dropped.
#'   \item \strong{Node split} -- If \code{split_nodes = TRUE} and
#'         \code{nodes} is provided, the same \code{split_by} column is
#'         validated in \code{nodes}. It must be identical in name to the
#'         links \code{split_by} or an error is raised. Empty levels are
#'         also dropped.
#'   \item \strong{Data splitting} -- The \code{links} data frame is
#'         split by the \code{split_by} levels into a named list,
#'         preserving factor level order.
#'   \item \strong{Attach node splits} -- If \code{split_nodes = TRUE},
#'         the \code{nodes} data frame is split identically. Each split's
#'         node data is attached as the \code{"nodes"} attribute on the
#'         corresponding links split.
#'   \item \strong{Dispatch to atomic} -- \code{\link{NetworkAtomic}} is
#'         called for each split. The \code{nodes} argument is passed as
#'         \code{"@nodes"} when \code{split_nodes = TRUE} so that it is
#'         extracted from the attribute. If \code{title} is a function, it
#'         receives the split level name for dynamic title generation.
#'   \item \strong{Combination} -- Results are combined via
#'         \code{\link{combine_plots}()} (when \code{combine = TRUE}) or
#'         returned as a named list of \code{ggplot} objects.
#' }
#'
#' @inheritParams common_args
#' @inheritParams NetworkAtomic
#' @param split_nodes A logical value. When \code{TRUE} and
#'  \code{split_by} is provided, the \code{nodes} data frame is split by
#'  the same \code{split_by} column in addition to the links. Both data
#'  frames must have a column with the same name as \code{split_by}.
#'  Default \code{FALSE}.
#' @return A \code{ggplot} object (no \code{split_by}), a
#'  \code{patchwork} object (\code{combine = TRUE}), or a named list of
#'  \code{ggplot} objects (\code{combine = FALSE}), each with
#'  \code{height} and \code{width} attributes in inches.
#' @export
#' @examples
#' \donttest{
#' # Create example data
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
#'
#' # Basic network
#' Network(relations, actors)
#'
#' # Blank theme with no coordinate axes
#' Network(relations, actors, theme = "theme_blank",
#'         theme_args = list(add_coord = FALSE))
#'
#' # Mapped aesthetics with custom layout
#' Network(relations, actors,
#'         link_weight_by = "friendship",
#'         node_size_by = "age",
#'         link_weight_name = "FRIENDSHIP",
#'         node_fill_by = "gender",
#'         link_color_by = "to",
#'         link_type_by = "type",
#'         node_color_by = "black",
#'         layout = "circle",
#'         link_curvature = 0.2)
#'
#' # Tree layout with clustering and marks
#' Network(relations, actors, layout = "tree",
#'         directed = FALSE, cluster = "fast_greedy",
#'         add_mark = TRUE)
#'
#' # Split by a column
#' Network(relations, actors, split_by = "type")
#' }
Network <- function(
    links,
    nodes = NULL,
    split_by = NULL,
    split_by_sep = "_",
    split_nodes = FALSE,
    from = NULL,
    from_sep = "_",
    to = NULL,
    to_sep = "_",
    node_by = NULL,
    node_by_sep = "_",
    link_weight_by = 2,
    link_weight_name = NULL,
    link_type_by = "solid",
    link_type_name = NULL,
    node_size_by = 15,
    node_size_name = NULL,
    node_color_by = "black",
    node_color_name = NULL,
    node_shape_by = 21,
    node_shape_name = NULL,
    node_fill_by = "grey20",
    node_fill_name = NULL,
    link_alpha = 1,
    node_alpha = 0.95,
    node_stroke = 1.5,
    cluster_scale = c("fill", "color", "shape"),
    node_size_range = c(5, 20),
    link_weight_range = c(0.5, 5),
    link_arrow_offset = 20,
    link_curvature = 0,
    link_color_by = "from",
    link_color_name = NULL,
    palette = "Paired",
    palcolor = NULL,
    palreverse = FALSE,
    link_palette = ifelse(link_color_by %in% c("from", "to"), palette, "Set1"),
    link_palcolor = if (link_color_by %in% c("from", "to")) palcolor else NULL,
    directed = TRUE,
    layout = "circle",
    cluster = "none",
    add_mark = FALSE,
    mark_expand = ggplot2::unit(10, "mm"),
    mark_type = c("hull", "ellipse", "rect", "circle"),
    mark_alpha = 0.1,
    mark_linetype = 1,
    add_label = TRUE,
    label_size = 3,
    label_fg = "white",
    label_bg = "black",
    label_bg_r = 0.1,
    arrow = ggplot2::arrow(
        type = "closed",
        length = ggplot2::unit(0.1, "inches")
    ),
    title = NULL,
    subtitle = NULL,
    xlab = NULL,
    ylab = NULL,
    aspect.ratio = 1,
    theme = "theme_this",
    theme_args = list(),
    legend.position = "right",
    legend.direction = "vertical",
    seed = 8525,
    combine = TRUE,
    nrow = NULL,
    ncol = NULL,
    byrow = TRUE,
    axes = NULL,
    axis_titles = axes,
    guides = NULL,
    design = NULL,
    ...
) {
    validate_common_args(seed = seed)
    theme <- process_theme(theme)
    if (is.null(split_by)) {
        split_nodes <- FALSE
    }

    l_split_by <- check_columns(
        links,
        split_by,
        force_factor = TRUE,
        allow_multi = TRUE,
        concat_multi = TRUE,
        concat_sep = split_by_sep
    )
    if (!is.null(l_split_by)) {
        links[[l_split_by]] <- droplevels(links[[l_split_by]])
    }

    if (isTRUE(split_nodes) && !is.null(nodes)) {
        if (
            is.character(nodes) && length(nodes) == 1 && startsWith(nodes, "@")
        ) {
            nodes <- attr(links, substring(nodes, 2))
        }
        n_split_by <- check_columns(
            nodes,
            split_by,
            force_factor = TRUE,
            allow_multi = TRUE,
            concat_multi = TRUE,
            concat_sep = split_by_sep
        )
        if (!is.null(n_split_by)) {
            nodes[[n_split_by]] <- droplevels(nodes[[n_split_by]])
        }

        if (!identical(l_split_by, n_split_by)) {
            stop(
                "The `split_by` columns in `links` and `nodes` must be the same."
            )
        }
    }

    if (!is.null(split_by)) {
        linkss <- split(links, links[[l_split_by]])
        if (isTRUE(split_nodes) && !is.null(nodes)) {
            nodess <- split(nodes, nodes[[n_split_by]])
            nms <- names(nodess)
            linkss <
                lapply(nms, function(nm) {
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
        split_by <- names(linkss) <- "..."
    }

    if (isTRUE(split_nodes) && !is.null(nodes)) {
        nodes <- "@nodes"
    }

    plots <- lapply(names(linkss), function(nm) {
        default_title <- if (length(linkss) == 1 && identical(nm, "...")) {
            NULL
        } else {
            nm
        }
        if (is.function(title)) {
            title <- title(default_title)
        } else {
            title <- title %||% default_title
        }

        NetworkAtomic(
            links = linkss[[nm]],
            nodes = nodes,
            from = from,
            from_sep = from_sep,
            to = to,
            to_sep = to_sep,
            node_by = node_by,
            node_by_sep = node_by_sep,
            link_weight_by = link_weight_by,
            link_weight_name = link_weight_name,
            link_type_by = link_type_by,
            link_type_name = link_type_name,
            node_size_by = node_size_by,
            node_size_name = node_size_name,
            node_color_by = node_color_by,
            node_color_name = node_color_name,
            node_shape_by = node_shape_by,
            node_shape_name = node_shape_name,
            node_fill_by = node_fill_by,
            node_fill_name = node_fill_name,
            link_alpha = link_alpha,
            node_alpha = node_alpha,
            node_stroke = node_stroke,
            cluster_scale = cluster_scale,
            node_size_range = node_size_range,
            link_weight_range = link_weight_range,
            link_arrow_offset = link_arrow_offset,
            link_curvature = link_curvature,
            link_color_by = link_color_by,
            link_color_name = link_color_name,
            palette = palette,
            palcolor = palcolor,
            palreverse = palreverse,
            link_palette = link_palette,
            link_palcolor = link_palcolor,
            directed = directed,
            layout = layout,
            cluster = cluster,
            add_mark = add_mark,
            mark_expand = mark_expand,
            mark_type = mark_type,
            mark_alpha = mark_alpha,
            mark_linetype = mark_linetype,
            add_label = add_label,
            label_size = label_size,
            label_fg = label_fg,
            label_bg = label_bg,
            label_bg_r = label_bg_r,
            arrow = arrow,
            title = title,
            subtitle = subtitle,
            xlab = xlab,
            ylab = ylab,
            aspect.ratio = aspect.ratio,
            theme = theme,
            theme_args = theme_args,
            legend.position = legend.position,
            legend.direction = legend.direction,
            ...
        )
    })

    names(plots) <- names(linkss)

    combine_plots(
        plots,
        combine = combine,
        split_by = split_by,
        nrow = nrow,
        ncol = ncol,
        byrow = byrow,
        axes = axes,
        axis_titles = axis_titles,
        guides = guides,
        design = design
    )
}
