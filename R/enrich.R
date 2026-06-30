#' Adjust_network_layout
#'
#' @keywords internal
#' @importFrom stats dist
adjust_network_layout <- function(
    graph,
    layout,
    width,
    height = 2,
    scale = 100,
    iter = 100
) {
    w <- width / 2
    layout[, 1] <- layout[, 1] / diff(range(layout[, 1])) * scale
    layout[, 2] <- layout[, 2] / diff(range(layout[, 2])) * scale

    adjusted <- c()
    # for (i in seq_len(iter)) {
    for (v in order(igraph::degree(graph), decreasing = TRUE)) {
        adjusted <- c(adjusted, v)
        neighbors <- as.numeric(igraph::neighbors(graph, igraph::V(graph)[v]))
        neighbors <- setdiff(neighbors, adjusted)
        x <- layout[v, 1]
        y <- layout[v, 2]
        r <- w[v]
        for (neighbor in neighbors) {
            nx <- layout[neighbor, 1]
            ny <- layout[neighbor, 2]
            ndist <- sqrt((nx - x)^2 + (ny - y)^2)
            nr <- w[neighbor]
            expect <- r + nr
            if (ndist < expect) {
                dx <- (x - nx) * (expect - ndist) / ndist
                dy <- (y - ny) * (expect - ndist) / ndist
                layout[neighbor, 1] <- nx - dx
                layout[neighbor, 2] <- ny - dy
                adjusted <- c(adjusted, neighbor)
            }
        }
    }
    # }

    for (i in seq_len(iter)) {
        dist_matrix <- as.matrix(dist(layout))
        nearest_neighbors <- apply(
            dist_matrix,
            2,
            function(x) which(x == min(x[x > 0])),
            simplify = FALSE
        )
        # nearest_neighbors <- apply(dist_matrix, 2, function(x) {
        #   head(order(x), 3)[-1]
        # }, simplify = FALSE)
        for (v in sample(seq_len(nrow(layout)))) {
            neighbors <- unique(nearest_neighbors[[v]])
            x <- layout[v, 1]
            y <- layout[v, 2]
            r <- w[v]
            for (neighbor in neighbors) {
                nx <- layout[neighbor, 1]
                ny <- layout[neighbor, 2]
                nr <- w[neighbor]
                if (abs(nx - x) < (r + nr) && abs(ny - y) < height) {
                    dx <- r + nr - (nx - x)
                    dy <- height - (ny - y)
                    if (sample(c(1, 0), 1) == 1) {
                        dx <- 0
                    } else {
                        dy <- 0
                    }
                    layout[neighbor, 1] <- nx - dx
                    layout[neighbor, 2] <- ny - dy
                }
            }
        }
    }
    return(layout)
}

#' Atomic enrichment map (internal)
#'
#' @description
#' Core implementation for drawing a single enrichment map -- a gene-set
#' similarity network where nodes represent enriched terms, node fill colour
#' encodes cluster membership, node size encodes the number of genes per
#' term, and edge thickness encodes the number of overlapping genes between
#' pairs of terms.  This is the workhorse behind the exported
#' \code{\link{EnrichMap}} function -- it takes a **single** data frame (no
#' \code{split_by} support) and returns a \code{ggplot} object.
#'
#' The function constructs an undirected graph from the term-term gene
#' overlap matrix, computes a force-directed layout, detects term clusters
#' via igraph community detection, and renders the result as a labelled
#' network plot with ggforce hull annotations around each cluster.
#'
#' @section Architecture:
#' \enumerate{
#'   \item \strong{ggplot dispatch} -- selects \code{gglogger::ggplot} or
#'         \code{ggplot2::ggplot} based on
#'         \code{getOption("plotthis.gglogger.enabled")}.
#'   \item \strong{Data format conversion} -- if \code{data} inherits from
#'         \code{"enrichResult"}, converts via \code{as.data.frame()}; if
#'         \code{in_form == "enrichr"}, calls
#'         \code{\link{prepare_enrichr_result}()} to rename columns and
#'         infer GeneRatio/BgRatio.
#'   \item \strong{Column validation} -- \code{\link{check_columns}()}
#'         verifies that \code{Description}, \code{GeneRatio},
#'         \code{pvalue}, \code{p.adjust}, and \code{geneID} are present.
#'   \item \strong{Top-term selection} -- when \code{top_term} is not
#'         \code{NULL}, selects the top N terms by the chosen \code{metric}
#'         via \code{dplyr::slice_min()}.
#'   \item \strong{Metric transformation} -- computes
#'         \eqn{-\log_{10}(metric)} as the node scoring variable.  Splits
#'         \code{geneID} on \code{"/"} to obtain per-term gene lists.
#'   \item \strong{ID assignment} -- uses the \code{ID} column if present;
#'         otherwise falls back to row names or generates synthetic IDs
#'         (\code{"GS1"}, \code{"GS2"}, \dots).
#'   \item \strong{Edge construction} -- creates all pairwise combinations
#'         (\code{utils::combn()}) of term IDs and computes the overlap
#'         count (intersection of geneID elements) as edge weight.  Edges
#'         with weight 0 are dropped.
#'   \item \strong{igraph graph construction} --
#'         \code{igraph::graph_from_data_frame()} builds an undirected
#'         graph from the edge list, with node attributes from the term
#'         data.
#'   \item \strong{Layout computation} -- dispatches to the chosen igraph
#'         layout function (\code{layout_with_*}) or one of the built-in
#'         shortcuts (\code{"circle"}, \code{"tree"}, \code{"grid"}).
#'   \item \strong{Cluster detection} -- runs the chosen igraph clustering
#'         algorithm (\code{cluster_*}) to group related terms into
#'         modules.
#'   \item \strong{Node coordinates} -- extracts vertex positions and
#'         cluster assignments into a data frame with \code{dim1},
#'         \code{dim2}, and \code{clusters} columns.
#'   \item \strong{Keyword extraction} -- when \code{show_keyword = TRUE},
#'         tokenizes \code{Description} text, filters words by
#'         \code{minchar} and \code{words_excluded}, scores remaining
#'         words by summed \code{metric} per cluster, and keeps the top
#'         \code{nlabel} keywords.  Otherwise, the term \code{Description}
#'         texts themselves are used as keywords.
#'   \item \strong{Gene keyword extraction} -- always computes per-cluster
#'         gene-level keywords (top \code{nlabel} genes by summed
#'         \code{metric}) for the feature legend mode.
#'   \item \strong{Mark layer} -- \code{ggforce::geom_mark_*()} (ellipse,
#'         rect, circle, or text) draws cluster hulls with labels
#'         containing the cluster ID and either term or feature keywords.
#'   \item \strong{Edge rendering} -- \code{ggplot2::geom_segment()} draws
#'         edges with line width proportional to overlap weight.
#'   \item \strong{Node rendering} -- \code{ggplot2::geom_point(shape = 21)}
#'         draws nodes sized by \code{Count} and filled by cluster
#'         membership.
#'   \item \strong{Scale configuration} -- \code{scale_size()} for node
#'         size, \code{scale_linewidth()} for edge width,
#'         \code{scale_fill_manual()} for cluster colours with custom
#'         legend labels (term keywords or gene keywords depending on
#'         \code{label}).
#'   \item \strong{Theme and dimensions} -- applies the resolved theme,
#'         sets aspect ratio and legend position, then calls
#'         \code{\link{calculate_plot_dimensions}()} to attach
#'         \code{height}/\code{width} attributes.
#' }
#'
#' @inheritParams common_args
#' @param data A data frame containing enrichment results in clusterProfiler
#'  format with at least the columns: \code{Description}, \code{GeneRatio},
#'  \code{pvalue}, \code{p.adjust}, and \code{geneID}.
#'  \itemize{
#'    \item \code{ID}, \code{qvalue}, \code{BgRatio}, and \code{Count} are
#'          optional.
#'    \item \code{Description} is the term description, displayed as the
#'          default keyword.
#'    \item \code{GeneRatio} is the fraction of input genes annotated to
#'          the term (e.g. \code{"10/500"}).
#'    \item \code{BgRatio} is the fraction of background genes annotated
#'          to the term (e.g. \code{"50/20000"}).
#'    \item \code{Count}, if given, must equal the numerator of
#'          \code{GeneRatio}.
#'    \item \code{geneID} contains gene symbols separated by \code{"/"}.
#'  }
#' @param in_form A character string specifying the input format.
#'  When \code{"auto"} (default), the function infers the format from the
#'  column names: clusterProfiler columns (\code{pvalue}, \code{p.adjust},
#'  \code{qvalue}) or Enrichr columns (\code{P.value},
#'  \code{Adjusted.P.value}).  Other options are \code{"clusterProfiler"}
#'  and \code{"enrichr"}.
#' @param top_term An integer specifying the maximum number of terms to
#'  include.  Terms are ranked by \code{metric} (ascending).  Default
#'  \code{100}.
#' @param metric A character string specifying the significance metric
#'  used for top-term selection and node scoring: \code{"p.adjust"}
#'  (default) or \code{"pvalue"}.  The value is transformed as
#'  \eqn{-\log_{10}(metric)}.
#' @param layout A character string naming the igraph layout algorithm.
#'  Built-in shortcuts: \code{"circle"}, \code{"tree"}, \code{"grid"}.
#'  Otherwise, the suffix passed to \code{layout_with_<layout>} in igraph
#'  (e.g. \code{"fr"} for Fruchterman-Reingold, \code{"kk"} for
#'  Kamada-Kawai).  Default \code{"fr"}.
#' @param minchar An integer specifying the minimum character length for
#'  words to be included as keywords when \code{show_keyword = TRUE}.
#'  Default \code{2}.
#' @param cluster A character string naming the igraph community detection
#'  algorithm.  The suffix passed to \code{cluster_<cluster>} in igraph
#'  (e.g. \code{"fast_greedy"}, \code{"walktrap"},
#'  \code{"edge_betweenness"}, \code{"infomap"}).  Default
#'  \code{"fast_greedy"}.
#' @param show_keyword A logical value.  When \code{TRUE}, the
#'  \code{Description} text is tokenized and the most significant words
#'  per cluster are shown as keywords.  When \code{FALSE} (default), the
#'  original term descriptions are used as labels.
#' @param nlabel An integer specifying the number of keywords or term
#'  descriptions to show per cluster in the legend labels.  Default
#'  \code{4}.
#' @param character_width An integer specifying the maximum width (in
#'  characters) at which keyword labels are wrapped via
#'  \code{strwrap(width = character_width)}.  Default \code{50}.
#' @param mark A character string naming the ggforce hull function.
#'  One of \code{"ellipse"} (default), \code{"rect"}, \code{"circle"}, or
#'  \code{"text"} -- passed as the suffix to \code{geom_mark_<mark>}.
#' @param label A character string specifying what information to display
#'  in the legend labels.  Either \code{"term"} (default; shows top term
#'  descriptions/keywords per cluster) or \code{"feature"} (shows top
#'  gene symbols per cluster).
#' @param labelsize A numeric value specifying the font size of the
#'  cluster labels drawn by the ggforce mark layer.  Default \code{5}.
#' @param expand A numeric vector of length 2 (or 4) specifying the
#'  expansion of the x and y axes, processed via
#'  \code{\link{norm_expansion}()}.  Default \code{c(0.4, 0.4)}.
#' @param words_excluded A character vector of words to exclude from
#'  keyword extraction when \code{show_keyword = TRUE}.  Defaults to
#'  \code{plotthis::words_excluded}.
#' @return A \code{ggplot} object with \code{height} and \code{width}
#'  attributes (in inches) attached.
#' @keywords internal
#' @importFrom utils combn
#' @importFrom dplyr slice_min slice_head reframe distinct group_by arrange reframe desc
#' @importFrom scales breaks_extended
#' @importFrom ggplot2 geom_segment geom_point labs scale_size guides scale_linewidth scale_fill_manual scale_x_continuous
#' @importFrom ggplot2 scale_y_continuous
EnrichMapAtomic <- function(
    data,
    in_form = "clusterProfiler",
    top_term = 100,
    metric = "p.adjust",
    layout = "fr",
    minchar = 2,
    cluster = "fast_greedy",
    show_keyword = FALSE,
    nlabel = 4,
    character_width = 50,
    words_excluded = plotthis::words_excluded,
    mark = "ellipse",
    label = c("term", "feature"),
    labelsize = 5,
    expand = c(0.4, 0.4),
    theme = "theme_this",
    theme_args = list(),
    palette = "Paired",
    palcolor = NULL,
    palreverse = FALSE,
    alpha = 1,
    aspect.ratio = 1,
    legend.position = "right",
    legend.direction = "vertical",
    title = NULL,
    subtitle = NULL,
    xlab = NULL,
    ylab = NULL,
    seed = 8525,
    ...
) {
    # if (!requireNamespace("igraph", quietly = TRUE)) {
    #     stop("The 'igraph' package is required 'EnrichMap'. Please install it first.")
    # }
    # if (!requireNamespace("ggforce", quietly = TRUE)) {
    #     stop("The 'ggforce' package is required 'EnrichMap'. Please install it first.")
    # }
    ggplot <- if (getOption("plotthis.gglogger.enabled", FALSE)) {
        gglogger::ggplot
    } else {
        ggplot2::ggplot
    }
    if (inherits(data, "enrichResult")) {
        data <- as.data.frame(data)
    }
    if (in_form == "enrichr") {
        data <- prepare_enrichr_result(data)
    }

    # Check the columns
    check_columns(data, "Description", force_factor = TRUE)
    check_columns(data, "GeneRatio")
    check_columns(data, "pvalue")
    check_columns(data, "p.adjust")
    check_columns(data, "geneID")
    label <- match.arg(label)
    expand <- norm_expansion(
        expand,
        x_type = "continuous",
        y_type = "continuous"
    )

    if (!is.null(top_term)) {
        data <- slice_min(data, !!sym(metric), n = top_term, with_ties = FALSE)
    }

    data$metric <- -log10(data[[metric]])
    data$geneID <- strsplit(data$geneID, "/")
    if (is.null(data$ID) && is.null(rownames(data))) {
        data$ID <- paste0("GS", seq_len(nrow(data)))
    } else if (is.null(data$ID)) {
        data$ID <- rownames(data)
    }

    nodes <- data
    edges <- as.data.frame(t(combn(nodes$ID, 2)))
    colnames(edges) <- c("from", "to")
    edges$weight <- mapply(
        function(x, y) {
            length(intersect(
                unlist(data[x, "geneID"]),
                unlist(data[y, "geneID"])
            ))
        },
        edges$from,
        edges$to
    )
    edges <- edges[edges$weight > 0, , drop = FALSE]
    nodes <- nodes[c("ID", setdiff(colnames(nodes), "ID"))]
    graph <- igraph::graph_from_data_frame(
        d = edges,
        vertices = nodes,
        directed = FALSE
    )
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

    clfun <- getFromNamespace(paste0("cluster_", cluster), "igraph")
    clusters <- clfun(graph)

    df_graph <- igraph::as_data_frame(graph, what = "both")
    df_nodes <- df_graph$vertices
    df_nodes$dim1 <- layout[, 1]
    df_nodes$dim2 <- layout[, 2]
    df_nodes$clusters <- factor(
        paste0("C", clusters$membership),
        paste0("C", unique(sort(clusters$membership)))
    )

    if (isTRUE(show_keyword)) {
        df_keyword1 <- df_nodes %>%
            mutate(
                keyword = strsplit(
                    tolower(as.character(!!sym("Description"))),
                    "\\s|\\n",
                    perl = TRUE
                )
            ) %>%
            unnest(cols = "keyword") %>%
            group_by(!!sym("keyword"), !!sym("clusters")) %>%
            reframe(
                keyword = tools::toTitleCase(!!sym("keyword")),
                score = sum(!!sym("metric")),
                count = n(),
                .groups = "keep"
            ) %>%
            filter(!grepl(pattern = "\\[.*\\]", x = !!sym("keyword"))) %>%
            filter(nchar(!!sym("keyword")) >= minchar) %>%
            filter(!tolower(!!sym("keyword")) %in% tolower(words_excluded)) %>%
            distinct() %>%
            group_by(!!sym("clusters")) %>%
            arrange(desc(!!sym("score"))) %>%
            slice_head(n = nlabel) %>%
            reframe(keyword = paste0(!!sym("keyword"), collapse = "/")) %>%
            as.data.frame()
        rownames(df_keyword1) <- as.character(df_keyword1$clusters)
        df_keyword1$keyword <- str_wrap(
            df_keyword1$keyword,
            width = character_width
        )
        df_keyword1$label <- paste0(
            df_keyword1$clusters,
            ":\n",
            df_keyword1$keyword
        )
    } else {
        # if (label == "term") {
        df_nodes$Description <- paste0(
            "- ",
            str_wrap(df_nodes$Description, width = character_width, exdent = 2)
        )
        # }
        df_keyword1 <- df_nodes %>%
            group_by(!!sym("clusters")) %>%
            arrange(desc(!!sym("metric"))) %>%
            reframe(keyword = !!sym("Description")) %>%
            distinct() %>%
            group_by(!!sym("clusters")) %>%
            slice_head(n = nlabel) %>%
            reframe(keyword = paste0(!!sym("keyword"), collapse = "\n")) %>%
            as.data.frame()

        rownames(df_keyword1) <- as.character(df_keyword1$clusters)
        df_keyword1$label <- paste0(
            df_keyword1$clusters,
            ":\n",
            df_keyword1$keyword
        )
    }

    df_keyword2 <- df_nodes %>%
        mutate(keyword = !!sym("geneID")) %>%
        unnest(cols = "keyword") %>%
        group_by(!!sym("keyword"), !!sym("clusters")) %>%
        reframe(
            keyword = !!sym("keyword"),
            score = sum(!!sym("metric")),
            count = n(),
            .groups = "keep"
        ) %>%
        distinct() %>%
        group_by(!!sym("clusters")) %>%
        arrange(desc(!!sym("score"))) %>%
        slice_head(n = nlabel) %>%
        reframe(keyword = paste0(!!sym("keyword"), collapse = "|")) %>%
        as.data.frame()

    rownames(df_keyword2) <- as.character(df_keyword2$clusters)
    df_keyword2$keyword <- str_wrap(
        df_keyword2$keyword,
        width = character_width
    )
    df_keyword2$label <- paste0(
        df_keyword2$clusters,
        ":\n",
        df_keyword2$keyword
    )

    df_nodes$keyword1 <- df_keyword1[as.character(df_nodes$clusters), "keyword"]
    df_nodes$keyword2 <- df_keyword2[as.character(df_nodes$clusters), "keyword"]

    df_edges <- df_graph$edges
    df_edges$from_dim1 <- df_nodes[df_edges$from, "dim1"]
    df_edges$from_dim2 <- df_nodes[df_edges$from, "dim2"]
    df_edges$to_dim1 <- df_nodes[df_edges$to, "dim1"]
    df_edges$to_dim2 <- df_nodes[df_edges$to, "dim2"]

    markfun <- getFromNamespace(paste0("geom_mark_", mark), "ggforce")
    mark_layer <- markfun(
        data = df_nodes,
        aes(
            x = !!sym("dim1"),
            y = !!sym("dim2"),
            color = !!sym("clusters"),
            fill = !!sym("clusters"),
            label = !!sym("clusters"),
            description = !!sym(ifelse(label == "term", "keyword1", "keyword2"))
        ),
        expand = unit(3, "mm"),
        alpha = 0.1,
        label.margin = margin(1, 1, 1, 1, "mm"),
        label.fontsize = labelsize * 2,
        label.fill = "grey95",
        label.minwidth = unit(character_width, "in"),
        label.buffer = unit(0, "mm"),
        con.size = 1,
        con.cap = 0
    )

    p <- ggplot() +
        mark_layer +
        geom_segment(
            data = df_edges,
            aes(
                x = !!sym("from_dim1"),
                y = !!sym("from_dim2"),
                xend = !!sym("to_dim1"),
                yend = !!sym("to_dim2"),
                linewidth = !!sym("weight")
            ),
            alpha = 0.1,
            lineend = "round"
        ) +
        geom_point(
            data = df_nodes,
            aes(
                x = !!sym("dim1"),
                y = !!sym("dim2"),
                size = !!sym("Count"),
                fill = !!sym("clusters")
            ),
            color = "black",
            shape = 21
        ) +
        labs(
            title = title,
            subtitle = subtitle,
            x = xlab %||% "",
            y = ylab %||% ""
        ) +
        scale_size(name = "Count", range = c(2, 6), breaks_extended(n = 4)) +
        guides(
            size = guide_legend(
                override.aes = list(fill = "grey30", shape = 21),
                order = 1
            )
        ) +
        scale_linewidth(
            name = "Intersection",
            range = c(0.3, 3),
            breaks_extended(n = 4)
        ) +
        guides(
            linewidth = guide_legend(
                override.aes = list(alpha = 1, color = "grey"),
                order = 2
            )
        ) +
        scale_fill_manual(
            name = switch(label, "term" = "Feature", "feature" = "Term"),
            values = palette_this(
                levels(df_nodes$clusters),
                palette = palette,
                palcolor = palcolor,
                reverse = palreverse
            ),
            labels = if (label == "term") {
                df_keyword2[levels(df_nodes$clusters), "label"]
            } else {
                df_keyword1[levels(df_nodes$clusters), "label"]
            },
            na.value = "grey80",
            aesthetics = c("colour", "fill")
        ) +
        guides(
            fill = guide_legend(
                override.aes = list(alpha = 1, color = "black", shape = NA),
                byrow = TRUE,
                theme = ggplot2::theme(legend.key.spacing.y = unit(0.1, "cm")),
                order = 3
            )
        ) +
        guides(color = guide_none()) +
        scale_x_continuous(expand = expand$x) +
        scale_y_continuous(expand = expand$y) +
        do_call(theme, theme_args) +
        ggplot2::theme(
            aspect.ratio = aspect.ratio,
            legend.position = legend.position,
            legend.direction = legend.direction
        )

    dims <- calculate_plot_dimensions(
        base_height = 8,
        aspect.ratio = aspect.ratio,
        legend.position = legend.position,
        legend.direction = legend.direction
    )
    attr(p, "height") <- dims$height
    attr(p, "width") <- dims$width

    p
}

#' Atomic enrichment network (internal)
#'
#' @description
#' Core implementation for drawing a single enrichment network -- a
#' term-gene bipartite graph where enriched terms and their member genes
#' are shown as interconnected nodes.  Term nodes are displayed as numbered
#' filled circles with a colour-coded legend; gene nodes are displayed as
#' labelled rectangles coloured by a blend of the term colours they belong
#' to.  This is the workhorse behind the exported \code{\link{EnrichNetwork}}
#' function -- it takes a **single** data frame (no \code{split_by} support)
#' and returns a \code{ggplot} object.
#'
#' The function constructs a bipartite graph between terms and genes,
#' computes a force-directed layout, optionally adjusts node positions to
#' reduce overlap, blends term colours for shared genes, and renders the
#' result as a labelled network.
#'
#' @section Architecture:
#' \enumerate{
#'   \item \strong{ggplot dispatch} -- selects \code{gglogger::ggplot} or
#'         \code{ggplot2::ggplot} based on
#'         \code{getOption("plotthis.gglogger.enabled")}.
#'   \item \strong{Data format conversion} -- if \code{data} inherits from
#'         \code{"enrichResult"}, converts via \code{as.data.frame()}.
#'   \item \strong{Column validation} -- \code{\link{check_columns}()}
#'         verifies that \code{Description}, \code{GeneRatio},
#'         \code{pvalue}, \code{p.adjust}, and \code{geneID} are present.
#'   \item \strong{Top-term selection} -- when \code{top_term} is not
#'         \code{NULL}, selects the top N terms by \code{metric} via
#'         \code{dplyr::slice_min()}.
#'   \item \strong{Metric and description preparation} -- computes
#'         \eqn{-\log_{10}(metric)} as the scoring variable.  Wraps
#'         \code{Description} text to \code{character_width} and parses
#'         \code{geneID} by splitting on \code{"/"}.
#'   \item \strong{Gene-term unnesting} -- unnests the \code{geneID}
#'         column to produce a gene-term mapping table.
#'   \item \strong{Bipartite node construction} -- creates nodes for both
#'         terms (\code{class = "term"}) and genes (\code{class = "gene"}),
#'         carrying the \code{Database} attribute if present.
#'   \item \strong{Bipartite edge construction} -- edges connect each term
#'         to its member genes with uniform weight (\code{weight = 1}).
#'   \item \strong{igraph graph construction} --
#'         \code{igraph::graph_from_data_frame()} builds an undirected
#'         bipartite graph.
#'   \item \strong{Layout computation} -- dispatches to the chosen igraph
#'         layout function (\code{layout_with_*}) or built-in shortcuts
#'         (\code{"circle"}, \code{"tree"}, \code{"grid"}).
#'   \item \strong{Layout adjustment} -- when \code{layoutadjust = TRUE}
#'         (default), calls \code{\link{adjust_network_layout}()} to push
#'         overlapping nodes apart based on label width and a repulsion
#'         simulation.
#'   \item \strong{Node coordinates} -- extracts vertex positions into
#'         \code{dim1} and \code{dim2} columns.
#'   \item \strong{Colour computation} -- palette colours are assigned to
#'         term nodes.  Gene node colours are computed by blending the
#'         colours of all connected terms via the specified
#'         \code{blendmode} using \code{blend_colors()}.
#'   \item \strong{Label colour} -- each node's text colour is chosen as
#'         black or white based on the luminance of its fill colour (sum
#'         of RGB channels > 510).
#'   \item \strong{Numeric labels} -- term nodes receive sequential integer
#'         labels; gene nodes display their gene symbol.
#'   \item \strong{Custom legend key} -- \code{draw_key_cust()} renders
#'         term legend entries as numbered circles via
#'         \code{ggrepel::shadowtextGrob()}.
#'   \item \strong{Edge rendering} -- \code{ggplot2::geom_segment()} draws
#'         edges coloured by the source term (legend suppressed).
#'   \item \strong{Gene node rendering} -- \code{ggplot2::geom_label()}
#'         displays gene symbols with fill colour blended from connected
#'         terms.
#'   \item \strong{Term node rendering} -- two \code{geom_point()} layers
#'         draw black-outlined circles filled with the term colour, with
#'         \code{draw_key_cust} as the key glyph.
#'   \item \strong{Term labels} -- \code{ggrepel::geom_text_repel()}
#'         places the numeric term labels with white text on a black
#'         background.
#'   \item \strong{Scale configuration} -- \code{scale_color_identity()}
#'         and \code{scale_fill_identity()} with a manual legend mapping
#'         term colours to term descriptions.
#'   \item \strong{Theme and dimensions} -- applies the resolved theme,
#'         sets aspect ratio and legend position, then calls
#'         \code{\link{calculate_plot_dimensions}()} to attach
#'         \code{height}/\code{width} attributes.
#' }
#'
#' @inheritParams common_args
#' @param data A data frame containing enrichment results in clusterProfiler
#'  format (see \code{\link{EnrichMapAtomic}} for the expected columns).
#' @param top_term An integer specifying the maximum number of terms to
#'  include.  Terms are ranked by \code{metric} (ascending).  Default
#'  \code{6}.
#' @param metric A character string specifying the significance metric for
#'  top-term selection: \code{"p.adjust"} (default) or \code{"pvalue"}.
#' @param character_width An integer specifying the maximum width (in
#'  characters) at which term descriptions are wrapped via
#'  \code{strwrap(width = character_width)}.  Default \code{50}.
#' @param layout A character string naming the igraph layout algorithm.
#'  Built-in shortcuts: \code{"circle"}, \code{"tree"}, \code{"grid"}.
#'  Otherwise, the suffix passed to \code{layout_with_<layout>} in igraph.
#'  Default \code{"fr"}.
#' @param layoutadjust A logical value.  When \code{TRUE} (default),
#'  applies \code{\link{adjust_network_layout}()} after the initial layout
#'  to reduce node overlap based on label width and a repulsion simulation.
#' @param adjscale A numeric value controlling the scale of the layout
#'  adjustment.  Passed as the \code{scale} argument to
#'  \code{\link{adjust_network_layout}()}.  Default \code{60}.
#' @param adjiter A numeric value controlling the number of iterations for
#'  the layout adjustment.  Passed as the \code{iter} argument to
#'  \code{\link{adjust_network_layout}()}.  Default \code{100}.
#' @param blendmode A character string specifying how gene colours are
#'  computed from the colours of the terms they belong to.  One of
#'  \code{"blend"} (default), \code{"average"}, \code{"multiply"}, or
#'  \code{"screen"}.  Passed to \code{blend_colors()}.
#' @param labelsize A numeric value specifying the font size of the
#'  numeric term labels displayed via \code{ggrepel::geom_text_repel()}.
#'  Default \code{5}.
#' @return A \code{ggplot} object with \code{height} and \code{width}
#'  attributes (in inches) attached.
#' @keywords internal
#' @importFrom utils getFromNamespace
#' @importFrom grDevices col2rgb
#' @importFrom ggplot2 scale_color_identity scale_fill_identity guides guide_legend draw_key_point .pt element_text
EnrichNetworkAtomic <- function(
    data,
    top_term = 6,
    metric = "p.adjust",
    character_width = 50,
    layout = "fr",
    layoutadjust = TRUE,
    adjscale = 60,
    adjiter = 100,
    blendmode = "blend",
    labelsize = 5,
    theme = "theme_this",
    theme_args = list(),
    palette = "Paired",
    palcolor = NULL,
    palreverse = FALSE,
    alpha = 1,
    aspect.ratio = 1,
    legend.position = "right",
    legend.direction = "vertical",
    title = NULL,
    subtitle = NULL,
    xlab = NULL,
    ylab = NULL,
    seed = 8525,
    ...
) {
    ggplot <- if (getOption("plotthis.gglogger.enabled", FALSE)) {
        gglogger::ggplot
    } else {
        ggplot2::ggplot
    }
    # if (!requireNamespace("igraph", quietly = TRUE)) {
    #     stop("The 'igraph' package is required 'EnrichMap'. Please install it first.")
    # }
    # if (!requireNamespace("ggforce", quietly = TRUE)) {
    #     stop("The 'ggforce' package is required 'EnrichMap'. Please install it first.")
    # }
    if (inherits(data, "enrichResult")) {
        data <- as.data.frame(data)
    }
    # Check the columns
    check_columns(data, "Description", force_factor = TRUE)
    check_columns(data, "GeneRatio")
    check_columns(data, "pvalue")
    check_columns(data, "p.adjust")
    check_columns(data, "geneID")

    if (!is.null(top_term)) {
        data <- slice_min(data, !!sym(metric), n = top_term, with_ties = FALSE)
    }

    data$metric <- -log10(data[[metric]])
    data$Description <- str_wrap(data$Description, width = character_width)
    data$Description <- factor(
        data$Description,
        levels = unique(data$Description)
    )
    data$geneID <- strsplit(data$geneID, "/")
    df_unnest <- unnest(data, cols = "geneID")

    nodes <- rbind(
        data.frame(
            "ID" = data$Description,
            class = "term",
            metric = data$metric
        ),
        data.frame("ID" = unique(df_unnest$geneID), class = "gene", metric = 0)
    )
    nodes$Database <- data$Database[1]
    edges <- as.data.frame(df_unnest[, c("Description", "geneID")])
    colnames(edges) <- c("from", "to")
    edges$weight <- 1
    graph <- igraph::graph_from_data_frame(
        d = edges,
        vertices = nodes,
        directed = FALSE
    )
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
    df_graph <- igraph::as_data_frame(graph, what = "both")

    df_nodes <- df_graph$vertices
    if (isTRUE(layoutadjust)) {
        width <- nchar(df_nodes$name)
        width[df_nodes$class == "term"] <- 8
        layout <- adjust_network_layout(
            graph = graph,
            layout = layout,
            width = width,
            height = 2,
            scale = adjscale,
            iter = adjiter
        )
    }
    df_nodes$dim1 <- layout[, 1]
    df_nodes$dim2 <- layout[, 2]

    df_edges <- df_graph$edges
    df_edges$from_dim1 <- df_nodes[df_edges$from, "dim1"]
    df_edges$from_dim2 <- df_nodes[df_edges$from, "dim2"]
    df_edges$to_dim1 <- df_nodes[df_edges$to, "dim1"]
    df_edges$to_dim2 <- df_nodes[df_edges$to, "dim2"]

    colors <- palette_this(
        levels(data$Description),
        palette = palette,
        palcolor = palcolor,
        reverse = palreverse
    )
    df_edges$color <- colors[df_edges$from]
    node_colors <- aggregate(
        df_unnest$Description,
        by = list(df_unnest$geneID),
        FUN = function(x) blend_colors(colors = colors[x], mode = blendmode)
    )
    colors <- c(colors, setNames(node_colors[, 2], node_colors[, 1]))
    label_colors <- ifelse(colSums(col2rgb(colors)) > 255 * 2, "black", "white")
    df_nodes$color <- colors[df_nodes$name]
    df_nodes$label_color <- label_colors[df_nodes$name]
    df_nodes$label <- NA
    df_nodes[levels(data$Description), "label"] <- seq_len(nlevels(
        data$Description
    ))

    draw_key_cust <- function(df, params, size) {
        data_text <- df
        data_text$label <- which(
            levels(data$Description) %in%
                names(colors)[colors == data_text$fill]
        )
        data_text$colour <- "black"
        data_text$alpha <- 1
        data_text$size <- 11 / .pt
        grid::grobTree(
            draw_key_point(df, list(color = "white", shape = 21)),
            getFromNamespace("shadowtextGrob", "ggrepel")(
                label = data_text$label,
                bg.colour = "black",
                bg.r = 0.1,
                gp = grid::gpar(col = "white", fontface = "bold")
            )
        )
    }

    p <- ggplot() +
        geom_segment(
            data = df_edges,
            aes(
                x = !!sym("from_dim1"),
                y = !!sym("from_dim2"),
                xend = !!sym("to_dim1"),
                yend = !!sym("to_dim2"),
                color = !!sym("color")
            ),
            alpha = 1,
            lineend = "round",
            show.legend = FALSE
        ) +
        geom_label(
            data = df_nodes[df_nodes$class == "gene", ],
            aes(
                x = !!sym("dim1"),
                y = !!sym("dim2"),
                label = !!sym("name"),
                fill = !!sym("color"),
                color = !!sym("label_color")
            ),
            size = 3,
            show.legend = FALSE
        ) +
        geom_point(
            data = df_nodes[df_nodes$class == "term", ],
            aes(x = !!sym("dim1"), y = !!sym("dim2")),
            size = 8,
            color = "black",
            fill = "black",
            stroke = 1,
            shape = 21,
            show.legend = FALSE
        ) +
        geom_point(
            data = df_nodes[df_nodes$class == "term", ],
            aes(x = !!sym("dim1"), y = !!sym("dim2"), fill = !!sym("color")),
            size = 7,
            color = "white",
            stroke = 1,
            shape = 21,
            key_glyph = draw_key_cust
        ) +
        geom_text_repel(
            data = df_nodes[df_nodes$class == "term", ],
            aes(x = !!sym("dim1"), y = !!sym("dim2"), label = !!sym("label")),
            fontface = "bold",
            min.segment.length = 0,
            segment.color = "black",
            point.size = NA,
            max.overlaps = 100,
            force = 0,
            color = "white",
            bg.color = "black",
            bg.r = 0.1,
            size = labelsize
        ) +
        scale_color_identity(guide = "none") +
        scale_fill_identity(
            name = "Term",
            guide = "legend",
            labels = levels(data$Description),
            breaks = colors[levels(data$Description)]
        ) +
        guides(
            color = guide_legend(override.aes = list(color = "transparent"))
        ) +
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
            legend.direction = legend.direction
        )

    dims <- calculate_plot_dimensions(
        base_height = 8,
        aspect.ratio = aspect.ratio,
        legend.position = legend.position,
        legend.direction = legend.direction
    )
    attr(p, "height") <- dims$height
    attr(p, "width") <- dims$width

    p
}

#' Process the enrichment results from Enrichr
#'
#' @param data A data frame containing the result by Enrichr.
#' @param dbname A character string specifying the name of the database column.
#' @param n_input An integer specifying the number of input genes.
#' Enrichr result doesn't ship with the number of input genes.
#' You can either provide the number directly or we will infer it. See details.
#' @return A data frame that can be used in `EnrichMap`.
#' @keywords internal
#' @details
#' In order to use the `EnrichMap` and `EnrichNetwork` functions and other visualization functions in `plotthis`,
#' the enrichment results from Enrichr need to be processed by the `prepare_enrichr_result` function.
#' The following columns are renamed:
#' * `Term` -> `Description`
#' * `Genes` -> `geneID` (separated replaced by `/`)
#' * `P.value` -> `pvalue`
#' * `Adjusted.P.value` -> `p.adjust`
#' Additionally, GeneRatio and BgRatio columns are inferred.
#' From [enrichr's documentation](https://maayanlab.cloud/Enrichr/help#background), the oddsRatio is defined as:
#' `oddsRatio = (A * (D - B - C + A) / max((B - A) * (C - A), 1)`, where A is the overlapping genes; B is the total genes in the gene set;
#' C (n_input) is the genes in input list; D is the total genes in the background.
#' D is not provided by Enrichr. To infer it, `D = oddsRatio * max((B - A) * (C - A), 1) / A + B + C - A`.
#' * `Overlap = A / B` (from Enrichr)
#' * `GeneRatio = A / C` (from ClusterProfiler)
#' * `BgRatio = B / D` (from ClusterProfiler)
#' `C (n_input)`, if not provided, will be inferred when `D` for all terms are equal.
#' When starting inferrence, the minimum value to try will be unique genes in `data$Genes`/`data$geneID`.
prepare_enrichr_result <- function(data, dbname = "Database", n_input = NULL) {
    if (inherits(data, "list")) {
        data <- lapply(names(data), function(x) {
            df <- prepare_enrichr_result(data[[x]])
            df[[dbname]] <- x
            df
        })
        return(do_call(rbind, data))
    }
    # Calculate GeneRatio and BgRatio
    A_B <- strsplit(data$Overlap, "/", fixed = TRUE)
    A <- as.numeric(sapply(A_B, `[`, 1))
    B <- as.numeric(sapply(A_B, `[`, 2))
    data$geneID <- data$Genes
    data$Genes <- NULL
    data$geneID <- gsub(";", "/", data$geneID, fixed = TRUE)
    if (is.null(n_input)) {
        C <- length(unique(unlist(strsplit(
            unlist(data$geneID),
            "/",
            fixed = TRUE
        ))))
        while (TRUE) {
            D <- data$Odds.Ratio * pmax((B - A) * (C - A), 1) / A + B + C - A
            if (isTRUE(all.equal(D, rep(D[1], length(D))))) {
                break
            }
            if (D[1] > 100000) {
                stop(
                    "Failed to infer the number of input genes. Please provide it manually."
                )
            }
            C <- C + 1
        }
    } else {
        C <- n_input
        D <- data$Odds.Ratio * pmax((B - A) * (C - A), 1) / A + B + C - A
    }
    D <- round(D)
    data$Count <- A
    data$GeneRatio <- paste(A, C, sep = "/")
    data$BgRatio <- paste(B, D, sep = "/")
    data$Overlap <- NULL
    data$Odds.Ratio <- NULL
    data$Description <- data$Term
    data$Term <- NULL
    data$pvalue <- data[["P.value"]]
    data[["P.value"]] <- NULL
    data$p.adjust <- data[["Adjusted.P.value"]]
    data[["Adjusted.P.value"]] <- NULL

    data
}

#' Enrichment Map and Enrichment Network
#'
#' @description
#' \code{EnrichMap} draws an enrichment map -- a gene-set similarity network
#' where each node is an enriched term, node size encodes the number of
#' associated genes, node fill colour encodes cluster membership (detected
#' via igraph community detection), and edge thickness encodes the number
#' of overlapping genes between term pairs.  The plot uses a force-directed
#' layout to arrange terms, and ggforce hull annotations group terms into
#' clusters.  Keyword or term-description labels appear in the legend.
#'
#' \code{EnrichNetwork} draws an enrichment network -- a term-gene bipartite
#' graph where term nodes are shown as numbered circles and gene nodes as
#' labelled rectangles.  Gene node colours are blended from the colours of
#' all terms they belong to.  A force-directed layout positions the nodes,
#' with optional overlap adjustment for better readability.
#'
#' Both functions accept enrichment results from clusterProfiler or Enrichr
#' (the latter is auto-detected and preprocessed via
#' \code{\link{prepare_enrichr_result}()}).
#'
#' @section split_by Workflow (EnrichMap):
#'
#' When \code{split_by} is provided, \code{EnrichMap()} executes the
#' following pipeline:
#' \enumerate{
#'   \item \strong{Argument validation} --
#'         \code{\link{validate_common_args}()} checks the seed.
#'   \item \strong{Input format detection} -- \code{match.arg()} resolves
#'         \code{in_form}; \code{"auto"} mode infers the format from
#'         column names.
#'   \item \strong{Enrichr preprocessing} -- when format is \code{"enrichr"},
#'         calls \code{\link{prepare_enrichr_result}()} to rename columns
#'         and infer GeneRatio/BgRatio.
#'   \item \strong{Split column resolution} --
#'         \code{\link{check_columns}()} validates \code{split_by}
#'         (force_factor, allow_multi, concat_multi).
#'   \item \strong{Data splitting} -- splits \code{data} by
#'         \code{split_by} levels, preserving factor level order.
#'   \item \strong{Per-split palette/colour} --
#'         \code{\link{check_palette}()} and
#'         \code{\link{check_palcolor}()} resolve per-split palette and
#'         colour overrides.
#'   \item \strong{Per-split legend} -- \code{\link{check_legend}()}
#'         resolves \code{legend.position} and \code{legend.direction}
#'         per split.
#'   \item \strong{Per-split title} -- when \code{title} is a function,
#'         it receives the default title (the split level name); otherwise
#'         \code{title \%||\% split_level} is used.
#'   \item \strong{Dispatch} -- each split subset is passed to
#'         \code{\link{EnrichMapAtomic}} with its resolved parameters.
#'   \item \strong{Combination} -- \code{\link{combine_plots}()} assembles
#'         the list of plots via \code{patchwork::wrap_plots}, honouring
#'         \code{nrow}/\code{ncol}/\code{byrow}/\code{axes}/
#'         \code{axis_titles}/\code{guides}/\code{design}.
#' }
#'
#' @rdname enrichmap1
#' @inheritParams common_args
#' @inheritParams EnrichMapAtomic
#' @param data A data frame containing enrichment results in clusterProfiler
#'  format with at least the columns: \code{Description}, \code{GeneRatio},
#'  \code{pvalue}, \code{p.adjust}, and \code{geneID}.
#'  \itemize{
#'    \item \code{ID}, \code{qvalue}, \code{BgRatio}, and \code{Count} are
#'          optional.
#'    \item \code{Description} is the term description.
#'    \item \code{GeneRatio} is the fraction of input genes annotated to
#'          the term (e.g. \code{"10/500"}).
#'    \item \code{BgRatio} is the fraction of background genes annotated
#'          to the term (e.g. \code{"50/20000"}).
#'    \item \code{Count}, if given, must equal the numerator of
#'          \code{GeneRatio}.
#'  }
#'  If you have enrichment results from multiple databases, you can combine
#'  them into one data frame and add a column (e.g. \code{Database}) to
#'  indicate the source.  Use \code{split_by = "Database"} to plot them
#'  side by side.
#' @param in_form A character string specifying the input format.
#'  When \code{"auto"} (default), the function infers the format from the
#'  column names: clusterProfiler columns (\code{pvalue}, \code{p.adjust},
#'  \code{qvalue}) or Enrichr columns (\code{P.value},
#'  \code{Adjusted.P.value}).  Other options are \code{"clusterProfiler"},
#'  \code{"clusterprofiler"}, and \code{"enrichr"}.
#' @return A \code{ggplot} object (single plot), a \code{patchwork} /
#'  \code{wrap_plots} object (when \code{split_by} is provided and
#'  \code{combine = TRUE}), or a list of \code{ggplot} objects (when
#'  \code{split_by} is provided and \code{combine = FALSE}).
#' @export
#' @examples
#' \donttest{
#' data(enrich_example)
#' EnrichMap(enrich_example)
#' EnrichMap(enrich_example, label = "feature")
#' EnrichMap(enrich_example, show_keyword = TRUE, label = "term")
#' EnrichMap(enrich_example, show_keyword = TRUE, label = "feature")
#'
#' data(enrich_multidb_example)
#' EnrichMap(enrich_multidb_example, split_by = "Database")
#' EnrichMap(enrich_multidb_example, split_by = "Database",
#'           palette = list(DB1 = "Paired", DB2 = "Set1"))
#' }
EnrichMap <- function(
    data,
    in_form = c("auto", "clusterProfiler", "clusterprofiler", "enrichr"),
    split_by = NULL,
    split_by_sep = "_",
    top_term = 10,
    metric = "p.adjust",
    layout = "fr",
    minchar = 2,
    cluster = "fast_greedy",
    show_keyword = FALSE,
    nlabel = 4,
    character_width = 50,
    mark = "ellipse",
    label = c("term", "feature"),
    labelsize = 5,
    expand = c(0.4, 0.4),
    theme = "theme_this",
    theme_args = list(),
    palette = "Paired",
    palcolor = NULL,
    palreverse = FALSE,
    alpha = 1,
    aspect.ratio = 1,
    legend.position = "right",
    legend.direction = "vertical",
    title = NULL,
    subtitle = NULL,
    xlab = NULL,
    ylab = NULL,
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
    validate_common_args(seed)
    theme <- process_theme(theme)
    in_form <- match.arg(in_form)
    in_form <- tolower(in_form)
    if (in_form == "auto") {
        if (all(c("pvalue", "p.adjust", "qvalue") %in% colnames(data))) {
            in_form <- "clusterProfiler"
        } else if (all(c("P.value", "Adjusted.P.value") %in% colnames(data))) {
            in_form <- "enrichr"
        } else {
            stop("Cannot infer the input format. Please provide it manually.")
        }
    }
    if (in_form == "enrichr") {
        data <- prepare_enrichr_result(data)
    }

    split_by <- check_columns(
        data,
        split_by,
        force_factor = TRUE,
        allow_multi = TRUE,
        concat_multi = TRUE,
        concat_sep = split_by_sep
    )

    if (!is.null(split_by)) {
        data[[split_by]] <- droplevels(data[[split_by]])
        datas <- split(data, data[[split_by]])
        # keep the order of levels
        datas <- datas[levels(data[[split_by]])]
    } else {
        datas <- list(data)
        split_by <- names(datas) <- "..."
    }
    palette <- check_palette(palette, names(datas))
    palcolor <- check_palcolor(palcolor, names(datas))
    legend.direction <- check_legend(
        legend.direction,
        names(datas),
        "legend.direction"
    )
    legend.position <- check_legend(
        legend.position,
        names(datas),
        "legend.position"
    )

    plots <- lapply(
        names(datas),
        function(nm) {
            default_title <- if (length(datas) == 1 && identical(nm, "...")) {
                NULL
            } else {
                nm
            }
            if (is.function(title)) {
                title <- title(default_title)
            } else {
                title <- title %||% default_title
            }
            EnrichMapAtomic(
                datas[[nm]],
                top_term = top_term,
                metric = metric,
                layout = layout,
                minchar = minchar,
                cluster = cluster,
                show_keyword = show_keyword,
                nlabel = nlabel,
                character_width = character_width,
                mark = mark,
                label = label,
                labelsize = labelsize,
                expand = expand,
                theme = theme,
                theme_args = theme_args,
                palette = palette[[nm]],
                palcolor = palcolor[[nm]],
                palreverse = palreverse,
                alpha = alpha,
                aspect.ratio = aspect.ratio,
                legend.position = legend.position[[nm]],
                legend.direction = legend.direction[[nm]],
                title = title,
                subtitle = subtitle,
                xlab = xlab,
                ylab = ylab,
                seed = seed,
                ...
            )
        }
    )

    names(plots) <- names(datas)

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

#' @section split_by Workflow (EnrichNetwork):
#'
#' When \code{split_by} is provided, \code{EnrichNetwork()} executes the
#' same pipeline as \code{EnrichMap()} above, but dispatches each split
#' subset to \code{\link{EnrichNetworkAtomic}}.
#'
#' @rdname enrichmap1
#' @inheritParams common_args
#' @inheritParams EnrichNetworkAtomic
#' @param data A data frame containing enrichment results in clusterProfiler
#'  format (see \code{\link{EnrichMap}} for the expected columns).
#'  If you have enrichment results from multiple databases, you can combine
#'  them into one data frame and add a column (e.g. \code{Database}) to
#'  indicate the source.  Use \code{split_by = "Database"} to plot them
#'  side by side.
#' @param in_form A character string specifying the input format.
#'  When \code{"auto"} (default), the function infers the format from the
#'  column names.  Other options are \code{"clusterProfiler"},
#'  \code{"clusterprofiler"}, and \code{"enrichr"}.
#' @return A \code{ggplot} object (single plot), a \code{patchwork} /
#'  \code{wrap_plots} object (when \code{split_by} is provided and
#'  \code{combine = TRUE}), or a list of \code{ggplot} objects (when
#'  \code{split_by} is provided and \code{combine = FALSE}).
#' @export
#' @examples
#' \donttest{
#' EnrichNetwork(enrich_example, top_term = 5)
#' }
EnrichNetwork <- function(
    data,
    in_form = c("auto", "clusterProfiler", "clusterprofiler", "enrichr"),
    split_by = NULL,
    split_by_sep = "_",
    top_term = 10,
    metric = "p.adjust",
    character_width = 50,
    layout = "fr",
    layoutadjust = TRUE,
    adjscale = 60,
    adjiter = 100,
    blendmode = "blend",
    labelsize = 5,
    theme = "theme_this",
    theme_args = list(),
    palette = "Paired",
    palcolor = NULL,
    palreverse = FALSE,
    alpha = 1,
    aspect.ratio = 1,
    legend.position = "right",
    legend.direction = "vertical",
    title = NULL,
    subtitle = NULL,
    xlab = NULL,
    ylab = NULL,
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
    validate_common_args(seed)
    theme <- process_theme(theme)
    in_form <- match.arg(in_form)
    in_form <- tolower(in_form)
    if (in_form == "auto") {
        if (all(c("pvalue", "p.adjust", "qvalue") %in% colnames(data))) {
            in_form <- "clusterProfiler"
        } else if (all(c("P.value", "Adjusted.P.value") %in% colnames(data))) {
            in_form <- "enrichr"
        } else {
            stop("Cannot infer the input format. Please provide it manually.")
        }
    }
    if (in_form == "enrichr") {
        data <- prepare_enrichr_result(data)
    }

    split_by <- check_columns(
        data,
        split_by,
        force_factor = TRUE,
        allow_multi = TRUE,
        concat_multi = TRUE,
        concat_sep = split_by_sep
    )

    if (!is.null(split_by)) {
        data[[split_by]] <- droplevels(data[[split_by]])
        datas <- split(data, data[[split_by]])
        # keep the order of levels
        datas <- datas[levels(data[[split_by]])]
    } else {
        datas <- list(data)
        split_by <- names(datas) <- "..."
    }
    palette <- check_palette(palette, names(datas))
    palcolor <- check_palcolor(palcolor, names(datas))
    legend.direction <- check_legend(
        legend.direction,
        names(datas),
        "legend.direction"
    )
    legend.position <- check_legend(
        legend.position,
        names(datas),
        "legend.position"
    )

    plots <- lapply(
        names(datas),
        function(nm) {
            default_title <- if (length(datas) == 1 && identical(nm, "...")) {
                NULL
            } else {
                nm
            }
            if (is.function(title)) {
                title <- title(default_title)
            } else {
                title <- title %||% default_title
            }
            EnrichNetworkAtomic(
                datas[[nm]],
                top_term = top_term,
                metric = metric,
                character_width = character_width,
                layout = layout,
                layoutadjust = layoutadjust,
                adjscale = adjscale,
                adjiter = adjiter,
                blendmode = blendmode,
                labelsize = labelsize,
                theme = theme,
                theme_args = theme_args,
                palette = palette[[nm]],
                palcolor = palcolor[[nm]],
                palreverse = palreverse,
                alpha = alpha,
                aspect.ratio = aspect.ratio,
                legend.position = legend.position[[nm]],
                legend.direction = legend.direction[[nm]],
                title = title,
                subtitle = subtitle,
                xlab = xlab,
                ylab = ylab,
                seed = seed,
                ...
            )
        }
    )

    names(plots) <- names(datas)

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
