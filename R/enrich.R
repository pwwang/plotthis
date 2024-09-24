#' Adjust_network_layout
#'
#' @keywords internal
adjust_network_layout <- function(graph, layout, width, height = 2, scale = 100, iter = 100) {
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
        nearest_neighbors <- apply(dist_matrix, 2, function(x) which(x == min(x[x > 0])), simplify = FALSE)
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

#' Atomic Enrichment Map
#'
#' @inheritParams common_args
#' @param data A data frame containing the data to be plotted.
#'  It should be in the format of clusterProfiler enrichment result,
#'  which includes the columns: ID, Description, GeneRatio, BgRatio, pvalue, p.adjust,
#'  qvalue, geneID and Count.
#'  * The `ID`, `qvalue`, `BgRatio`, and `Count` columns are optional.
#'  * The `Description` is the description of the term.
#'  * The `GeneRatio` is the number of genes in the term divided by the total number of genes in the input list.
#'  * The `BgRatio` is the number of genes in the term divided by the total number of genes in the background list (all terms).
#'  * The `Count` column, if given, should be the same as the first number in GeneRatio.
#' @param top_term An integer specifying the number of top terms to show.
#' @param metric A character string specifying the metric to use for the size of the nodes.
#'  Either "pvalue" or "p.adjust". The default is "p.adjust".
#' @param cutoff A numeric value specifying the cutoff of the metric to filter the terms.
#'  The records will be filtered by both the `top_term` and `cutoff` if provided.
#'  To skip either/both of them, set it/them to `NULL`.
#' @param layout A character string specifying the layout of the graph.
#'  Either "circle", "tree", "grid" or other layout functions in `igraph`.
#' @param minchar An integer specifying the minimum number of characters to show in the keyword.
#' @param cluster A character string specifying the clustering method.
#'  Either "fast_greedy", "walktrap", "edge_betweenness", "infomap" or other clustering functions in `igraph`.
#' @param show_keyword A logical value specifying whether to show the keyword instead of Description/Term in the plot.
#' @param nlabel An integer specifying the number of labels to show in each cluster.
#' @param character_width The width of the characters used to wrap the keyword.
#' @param mark A character string specifying the mark to use for the nodes.
#'  Either "ellipse", "rect", "circle", "text" or other mark functions in `ggforce`.
#' @param label A character string specifying the label to show in the legend.
#'  Either "term" or "feature". The default is "term".
#' @param labelsize A numeric value specifying the size of the label.
#' @param expand A numeric vector of length 2 specifying the expansion of the x and y axis.
#' @return A ggplot object
#' @keywords internal
#' @importFrom dplyr slice_min slice_head reframe distinct group_by arrange reframe
#' @importFrom scales breaks_extended
#' @importFrom gglogger ggplot
#' @importFrom ggplot2 geom_segment geom_point labs scale_size guides scale_linewidth scale_fill_manual scale_x_continuous
#' @importFrom ggplot2 scale_y_continuous
EnrichMapAtomic <- function(
    data, top_term = 10, metric = "p.adjust", cutoff = 0.05, layout = "fr", minchar = 2,
    cluster = "fast_greedy", show_keyword = FALSE, nlabel = 4, character_width = 50,
    mark = "ellipse", label = c("term", "feature"), labelsize = 5, expand = c(0.4, 0.4),
    theme = "theme_this", theme_args = list(), palette = "Paired", palcolor = NULL, alpha = 1,
    aspect.ratio = 1, legend.position = "right", legend.direction = "vertical",
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL, seed = 8525, ...
) {
    if (!requireNamespace("igraph", quietly = TRUE)) {
        stop("The 'igraph' package is required 'EnrichMap'. Please install it first.")
    }
    if (!requireNamespace("ggforce", quietly = TRUE)) {
        stop("The 'ggforce' package is required 'EnrichMap'. Please install it first.")
    }
    if (inherits(data, "enrichResult")) {
        data <- as.data.frame(data)
    }
    # Check the columns
    check_columns(data, "Description", force_factor = TRUE)
    check_columns(data, "GeneRatio")
    check_columns(data, "pvalue")
    check_columns(data, "p.adjust")
    check_columns(data, "geneID")
    label <- match.arg(label)
    expand <- norm_expansion(expand, x_type = "continuous", y_type = "continuous")
    if (!is.null(cutoff)) {
        data <- data[data[[metric]] <= cutoff, , drop = FALSE]
    }
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
    edges$weight <- mapply(function(x, y) length(intersect(data[x, "geneID"], data[y, "geneID"])), edges$from, edges$to)
    edges <- edges[edges$weight > 0, , drop = FALSE]
    nodes <- nodes[c("ID", setdiff(colnames(nodes), "ID"))]
    graph <- igraph::graph_from_data_frame(d = edges, vertices = nodes, directed = FALSE)
    if (layout %in% c("circle", "tree", "grid")) {
        layout <- switch(enrichmap_layout,
            "circle" = layout_in_circle(graph),
            "tree" = layout_as_tree(graph),
            "grid" = layout_on_grid(graph)
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
        paste0("C", unique(sort(clusters$membership))))

    if (isTRUE(show_keyword)) {
        df_keyword1 <- df_nodes %>%
            mutate(keyword = strsplit(tolower(as.character(Description)), "\\s|\\n", perl = TRUE)) %>%
            unnest(cols = "keyword") %>%
            group_by(keyword, clusters) %>%
            reframe(
                keyword = tools::toTitleCase(keyword),
                score = sum(metric),
                count = n(),
                .groups = "keep"
            ) %>%
            filter(!grepl(pattern = "\\[.*\\]", x = keyword)) %>%
            filter(nchar(keyword) >= minchar) %>%
            filter(!tolower(keyword) %in% tolower(words_excluded)) %>%
            distinct() %>%
            group_by(clusters) %>%
            arrange(desc(score)) %>%
            slice_head(n = nlabel) %>%
            reframe(keyword = paste0(keyword, collapse = "/")) %>%
            as.data.frame()
        rownames(df_keyword1) <- as.character(df_keyword1$clusters)
        df_keyword1$keyword <- str_wrap(df_keyword1$keyword, width = character_width)
        df_keyword1$label <- paste0(df_keyword1$clusters, ":\n", df_keyword1$keyword)
    } else {
        # if (label == "term") {
            df_nodes$Description <- paste0("· ", str_wrap(df_nodes$Description, width = character_width, exdent = 2))
        # }
        df_keyword1 <- df_nodes %>%
            group_by(clusters) %>%
            arrange(desc(metric)) %>%
            reframe(keyword = Description) %>%
            distinct() %>%
            group_by(clusters) %>%
            slice_head(n = nlabel) %>%
            reframe(keyword = paste0(keyword, collapse = "\n")) %>%
            as.data.frame()

        rownames(df_keyword1) <- as.character(df_keyword1$clusters)
        df_keyword1$label <- paste0(df_keyword1$clusters, ":\n", df_keyword1$keyword)
    }

    df_keyword2 <- df_nodes %>%
        mutate(keyword = geneID) %>%
        unnest(cols = "keyword") %>%
        group_by(keyword, clusters) %>%
        reframe(
            keyword = keyword,
            score = sum(metric),
            count = n(),
            .groups = "keep"
        ) %>%
        distinct() %>%
        group_by(clusters) %>%
        arrange(desc(score)) %>%
        slice_head(n = nlabel) %>%
        reframe(keyword = paste0(keyword, collapse = "|")) %>%
        as.data.frame()

    rownames(df_keyword2) <- as.character(df_keyword2$clusters)
    df_keyword2$keyword <- str_wrap(df_keyword2$keyword, width = character_width)
    df_keyword2$label <- paste0(df_keyword2$clusters, ":\n", df_keyword2$keyword)

    df_nodes$keyword1 <- df_keyword1[as.character(df_nodes$clusters), "keyword"]
    df_nodes$keyword2 <- df_keyword2[as.character(df_nodes$clusters), "keyword"]

    df_edges <- df_graph$edges
    df_edges$from_dim1 <- df_nodes[df_edges$from, "dim1"]
    df_edges$from_dim2 <- df_nodes[df_edges$from, "dim2"]
    df_edges$to_dim1 <- df_nodes[df_edges$to, "dim1"]
    df_edges$to_dim2 <- df_nodes[df_edges$to, "dim2"]

    markfun <- getFromNamespace(paste0("geom_mark_", mark), "ggforce")
    mark_layer <- markfun(
        data = df_nodes, aes(
            x = dim1, y = dim2, color = clusters, fill = clusters,
            label = clusters, description = if (label == "term") keyword1 else keyword2
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
        geom_segment(data = df_edges, aes(x = from_dim1, y = from_dim2, xend = to_dim1, yend = to_dim2, linewidth = weight), alpha = 0.1, lineend = "round") +
        geom_point(data = df_nodes, aes(x = dim1, y = dim2, size = Count, fill = clusters), color = "black", shape = 21) +
        labs(title = title, subtitle = subtitle, x = xlab %||% "", y = ylab %||% "") +
        scale_size(name = "Count", range = c(2, 6), breaks_extended(n = 4)) +
        guides(size = guide_legend(override.aes = list(fill = "grey30", shape = 21), order = 1)) +
        scale_linewidth(name = "Intersection", range = c(0.3, 3), breaks_extended(n = 4)) +
        guides(linewidth = guide_legend(override.aes = list(alpha = 1, color = "grey"), order = 2)) +
        scale_fill_manual(
            name = switch(label, "term" = "Feature", "feature" = "Term"),
            values = palette_this(levels(df_nodes$clusters), palette = palette, palcolor = palcolor),
            labels = if (label == "term") df_keyword2[levels(df_nodes$clusters), "label"] else df_keyword1[levels(df_nodes$clusters), "label"],
            na.value = "grey80",
            aesthetics = c("colour", "fill")
        ) +
        guides(fill = guide_legend(
            override.aes = list(alpha = 1, color = "black", shape = NA),
            byrow = TRUE,
            theme = ggplot2::theme(legend.key.spacing.y = unit(0.1, "cm")),
            order = 3)) +
        guides(color = guide_none()) +
        scale_x_continuous(expand = expand$x) +
        scale_y_continuous(expand = expand$y) +
        do.call(theme, theme_args) +
        ggplot2::theme(
            aspect.ratio = aspect.ratio,
            legend.position = legend.position,
            legend.direction = legend.direction
        )

    height <- width <- 8
    if (legend.position != "none") {
        if (legend.position %in% c("right", "left")) {
            width <- width + 2
        } else if (legend.direction == "horizontal") {
            height <- height + 1
        } else {
            width <- width + 4
        }
    }
    attr(p, "height") <- height
    attr(p, "width") <- width
    p
}

#' Atomic Enrichment Network
#'
#' @inheritParams common_args
#' @param data A data frame containing the data to be plotted.
#' @return A ggplot object
#' @keywords internal
#' @importFrom ggplot2 scale_color_identity scale_fill_identity guides guide_legend draw_key_point .pt element_text
EnrichNetworkAtomic <- function(
    data, top_term = 10, metric = "p.adjust", cutoff = 0.05, character_width = 50,
    layout = "fr", layoutadjust = TRUE, adjscale = 60, adjiter = 100, blendmode = "blend", labelsize = 5,
    theme = "theme_this", theme_args = list(), palette = "Paired", palcolor = NULL, alpha = 1,
    aspect.ratio = 1, legend.position = "right", legend.direction = "vertical",
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL, seed = 8525,
    ...
) {
    if (!requireNamespace("igraph", quietly = TRUE)) {
        stop("The 'igraph' package is required 'EnrichMap'. Please install it first.")
    }
    if (!requireNamespace("ggforce", quietly = TRUE)) {
        stop("The 'ggforce' package is required 'EnrichMap'. Please install it first.")
    }
    if (inherits(data, "enrichResult")) {
        data <- as.data.frame(data)
    }
    # Check the columns
    check_columns(data, "Description", force_factor = TRUE)
    check_columns(data, "GeneRatio")
    check_columns(data, "pvalue")
    check_columns(data, "p.adjust")
    check_columns(data, "geneID")

    if (!is.null(cutoff)) {
        data <- data[data[[metric]] < cutoff, , drop = FALSE]
    }
    if (!is.null(top_term)) {
        data <- slice_min(data, !!sym(metric), n = top_term, with_ties = FALSE)
    }

    data$metric <- -log10(data[[metric]])
    data$Description <- str_wrap(data$Description, width = character_width)
    data$Description <- factor(data$Description, levels = unique(data$Description))
    data$geneID <- strsplit(data$geneID, "/")
    df_unnest <- unnest(data, cols = "geneID")

    nodes <- rbind(
        data.frame("ID" = data$Description, class = "term", metric = data$metric),
        data.frame("ID" = unique(df_unnest$geneID), class = "gene", metric = 0)
    )
    nodes$Database <- data$Database[1]
    edges <- as.data.frame(df_unnest[, c("Description", "geneID")])
    colnames(edges) <- c("from", "to")
    edges$weight <- 1
    graph <- igraph::graph_from_data_frame(d = edges, vertices = nodes, directed = FALSE)
    if (layout %in% c("circle", "tree", "grid")) {
        layout <- switch(layout,
            "circle" = layout_in_circle(graph),
            "tree" = layout_as_tree(graph),
            "grid" = layout_on_grid(graph)
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
            graph = graph, layout = layout, width = width, height = 2,
            scale = adjscale, iter = adjiter
        )
    }
    df_nodes$dim1 <- layout[, 1]
    df_nodes$dim2 <- layout[, 2]

    df_edges <- df_graph$edges
    df_edges$from_dim1 <- df_nodes[df_edges$from, "dim1"]
    df_edges$from_dim2 <- df_nodes[df_edges$from, "dim2"]
    df_edges$to_dim1 <- df_nodes[df_edges$to, "dim1"]
    df_edges$to_dim2 <- df_nodes[df_edges$to, "dim2"]

    colors <- palette_this(levels(data$Description), palette = palette, palcolor = palcolor)
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
    df_nodes[levels(data$Description), "label"] <- seq_len(nlevels(data$Description))

    draw_key_cust <- function(df, params, size) {
        data_text <- df
        data_text$label <- which(levels(data$Description) %in% names(colors)[colors == data_text$fill])
        data_text$colour <- "black"
        data_text$alpha <- 1
        data_text$size <- 11 / .pt
        grid::grobTree(
            draw_key_point(df, list(color = "white", shape = 21)),
            ggrepel:::shadowtextGrob(label = data_text$label, bg.colour = "black", bg.r = 0.1, gp = grid::gpar(col = "white", fontface = "bold"))
        )
    }

    p <- ggplot() +
        geom_segment(data = df_edges, aes(x = from_dim1, y = from_dim2, xend = to_dim1, yend = to_dim2, color = color), alpha = 1, lineend = "round", show.legend = FALSE) +
        geom_label(data = df_nodes[df_nodes$class == "gene", ], aes(x = dim1, y = dim2, label = name, fill = color, color = label_color), size = 3, show.legend = FALSE) +
        geom_point(data = df_nodes[df_nodes$class == "term", ], aes(x = dim1, y = dim2), size = 8, color = "black", fill = "black", stroke = 1, shape = 21, show.legend = FALSE) +
        geom_point(data = df_nodes[df_nodes$class == "term", ], aes(x = dim1, y = dim2, fill = color), size = 7, color = "white", stroke = 1, shape = 21, key_glyph = draw_key_cust) +
        geom_text_repel(
            data = df_nodes[df_nodes$class == "term", ], aes(x = dim1, y = dim2, label = label),
            fontface = "bold", min.segment.length = 0, segment.color = "black",
            point.size = NA, max.overlaps = 100, force = 0, color = "white", bg.color = "black", bg.r = 0.1, size = labelsize
        ) +
        scale_color_identity(guide = "none") +
        scale_fill_identity(
            name = "Term", guide = "legend",
            labels = levels(data$Description),
            breaks = colors[levels(data$Description)]
        ) +
        guides(color = guide_legend(override.aes = list(color = "transparent"))) +
        labs(title = title, subtitle = subtitle, x = xlab %||% "", y = ylab %||% "") +
        do.call(theme, theme_args) +
        ggplot2::theme(
            aspect.ratio = aspect.ratio,
            legend.position = legend.position,
            legend.direction = legend.direction
        )

    height <- width <- 8
    if (legend.position != "none") {
        if (legend.position %in% c("right", "left")) {
            width <- width + 2
        } else if (legend.direction == "horizontal") {
            height <- height + 1
        } else {
            width <- width + 4
        }
    }
    attr(p, "height") <- height
    attr(p, "width") <- width
    p
}

#' Enrichment Map/Network
#'
#' `PrepareEnrichrResult` is a function to process the enrichment results from Enrichr.
#' `EnrichMap` is a function to plot the enrichment map.
#' `EnrichNetwork` is a function to plot the enrichment network.
#'
#' @rdname enrichmap
#' @param data A data frame containing the result by Enrichr.
#' @return A data frame that can be used in `EnrichMap`.
#' @export
PrepareEnrichrResult <- function(data) {
    data$GeneRatio <- data$Overlap
    data$Overlap <- NULL
    data$Description <- data$Term
    data$Term <- NULL
    data$geneID <- data$Genes
    data$Genes <- NULL
    data$geneID <- gsub(";", "/", data$geneID)
    data$pvalue <- data[["P-value"]]
    data[["P-value"]] <- NULL
    data$p.adjust <- data[["Adjusted P-value"]]
    data[["Adjusted P-value"]] <- NULL

    data
}

#' @rdname enrichmap
#' @inheritParams common_args
#' @inheritParams EnrichMapAtomic
#' @param data A data frame containing the data to be plotted.
#'  It should be in the format of clusterProfiler enrichment result,
#'  which includes the columns: ID, Description, GeneRatio, BgRatio, pvalue, p.adjust,
#'  qvalue, geneID and Count.
#'  * The `ID`, `qvalue` and `Count` columns are optional.
#'  * The `Description` is the description of the term.
#'  * The `GeneRatio` is the number of genes in the term divided by the total number of genes in the input list.
#'  * The `BgRatio` is the number of genes in the term divided by the total number of genes in the background list (all terms).
#'  * The `Count` column, if given, should be the same as the first number in GeneRatio.
#'
#'  If you have enrichment results from multiple databases, you can combine them into one data frame and add a column (e.g. Database)
#'  to indicate the database.
#'  You can plot them in a single plot using the `split_by` argument (e.g. `split_by = "Database"`).
#' @export
#' @examples
#' data(enrich_example)
#' EnrichMap(enrich_example)
#' EnrichMap(enrich_example, label = "feature")
#' EnrichMap(enrich_example, show_keyword = TRUE, label = "term")
#' EnrichMap(enrich_example, show_keyword = TRUE, label = "feature")
#'
#' enrich_example$Database <- "DB1"
#' enrich_example2 <- enrich_example
#' enrich_example2$Database <- "DB2"
#' EnrichMap(rbind(enrich_example, enrich_example2), split_by = "Database")
EnrichMap <- function(
    data, split_by = NULL, split_by_sep = "_",
    top_term = 10, metric = "p.adjust", cutoff = 0.05, layout = "fr", minchar = 2,
    cluster = "fast_greedy", show_keyword = FALSE, nlabel = 4, character_width = 50,
    mark = "ellipse", label = c("term", "feature"), labelsize = 5, expand = c(0.4, 0.4),
    theme = "theme_this", theme_args = list(), palette = "Paired", palcolor = NULL, alpha = 1,
    aspect.ratio = 1, legend.position = "right", legend.direction = "vertical",
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL, seed = 8525,
    combine = TRUE, nrow = NULL, ncol = NULL, byrow = TRUE, ...
) {
    validate_common_args(seed)
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
        names(datas),
        function(nm) {
            if (!identical(nm, "...")) {
                title <- title %||% nm
            }
            EnrichMapAtomic(
                datas[[nm]],
                top_term = top_term, metric = metric, cutoff = cutoff, layout = layout, minchar = minchar,
                cluster = cluster, show_keyword = show_keyword, nlabel = nlabel, character_width = character_width,
                mark = mark, label = label, labelsize = labelsize, expand = expand,
                theme = theme, theme_args = theme_args, palette = palette, palcolor = palcolor, alpha = alpha,
                aspect.ratio = aspect.ratio, legend.position = legend.position, legend.direction = legend.direction,
                title = title, subtitle = subtitle, xlab = xlab, ylab = ylab, seed = seed, ...
            )
        }
    )

    combine_plots(plots, combine = combine, nrow = nrow, ncol = ncol, byrow = byrow)
}

#' @rdname enrichmap
#' @inheritParams common_args
#' @inheritParams EnrichNetworkAtomic
#' @param data A data frame containing the data to be plotted.
#'  It should be in the format of clusterProfiler enrichment result,
#'  which includes the columns: ID, Description, GeneRatio, BgRatio, pvalue, p.adjust,
#'  qvalue, geneID and Count.
#'  * The `ID`, `qvalue` and `Count` columns are optional.
#'  * The `Description` is the description of the term.
#'  * The `GeneRatio` is the number of genes in the term divided by the total number of genes in the input list.
#'  * The `BgRatio` is the number of genes in the term divided by the total number of genes in the background list (all terms).
#'  * The `Count` column, if given, should be the same as the first number in GeneRatio.
#'
#'  If you have enrichment results from multiple databases, you can combine them into one data frame and add a column (e.g. Database)
#'  to indicate the database.
#'  You can plot them in a single plot using the `split_by` argument (e.g. `split_by = "Database"`).
#' @return A ggplot object or wrap_plots object or a list of ggplot objects
#' @export
#' @examples
#' EnrichNetwork(enrich_example, top_term = 5)
EnrichNetwork <- function(
    data, split_by = NULL, split_by_sep = "_",
    top_term = 10, metric = "p.adjust", cutoff = 0.05, character_width = 50,
    layout = "fr", layoutadjust = TRUE, adjscale = 60, adjiter = 100, blendmode = "blend", labelsize = 5,
    theme = "theme_this", theme_args = list(), palette = "Paired", palcolor = NULL, alpha = 1,
    aspect.ratio = 1, legend.position = "right", legend.direction = "vertical",
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL, seed = 8525,
    combine = TRUE, nrow = NULL, ncol = NULL, byrow = TRUE, ...
) {
    validate_common_args(seed)
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
        names(datas),
        function(nm) {
            if (!identical(nm, "...")) {
                title <- title %||% nm
            }
            EnrichNetworkAtomic(
                datas[[nm]],
                top_term = top_term, metric = metric, cutoff = cutoff, character_width = character_width,
                layout = layout, layoutadjust = layoutadjust, adjscale = adjscale, adjiter = adjiter, blendmode = blendmode,
                labelsize = labelsize, theme = theme, theme_args = theme_args, palette = palette, palcolor = palcolor, alpha = alpha,
                aspect.ratio = aspect.ratio, legend.position = legend.position, legend.direction = legend.direction,
                title = title, subtitle = subtitle, xlab = xlab, ylab = ylab, seed = seed, ...
            )
        }
    )

    combine_plots(plots, combine = combine, nrow = nrow, ncol = ncol, byrow = byrow)
}
