set.seed(8525)
# Network plot expects a links data frame with from/to columns
net_links <- data.frame(
    from = c("A", "A", "B", "C", "C", "D", "E", "E"),
    to   = c("B", "C", "D", "D", "E", "E", "A", "B"),
    weight = c(2, 3, 1, 4, 2, 1, 3, 2),
    type = factor(c("strong", "weak", "strong", "strong", "weak", "weak", "strong", "weak")),
    group = factor(c("G1", "G1", "G1", "G2", "G2", "G2", "G1", "G2"))
)

test_that("Network basic usage works", {
    p <- Network(net_links, from = "from", to = "to")
    expect_s3_class(p, "ggplot")
    expect_true(!is.null(attr(p, "height")))
    expect_true(!is.null(attr(p, "width")))
})

test_that("Network with title and subtitle works", {
    p <- Network(net_links, from = "from", to = "to",
                 title = "Network Title", subtitle = "Subtitle")
    expect_s3_class(p, "ggplot")
    expect_equal(p$labels$title, "Network Title")
    expect_equal(p$labels$subtitle, "Subtitle")
})

test_that("Network with link_weight_by works", {
    p <- Network(net_links, from = "from", to = "to", link_weight_by = "weight")
    expect_s3_class(p, "ggplot")
})

test_that("Network with link_color_by = 'to' works", {
    p <- Network(net_links, from = "from", to = "to", link_color_by = "to")
    expect_s3_class(p, "ggplot")
})

test_that("Network with layout = 'fr' works", {
    p <- Network(net_links, from = "from", to = "to", layout = "fr")
    expect_s3_class(p, "ggplot")
})

test_that("Network with split_by returns patchwork", {
    p <- Network(net_links, from = "from", to = "to",
                 split_by = "group", combine = TRUE)
    expect_true(inherits(p, "patchwork") || inherits(p, "gg"))
})

test_that("Network with combine = FALSE returns list", {
    plots <- Network(net_links, from = "from", to = "to",
                     split_by = "group", combine = FALSE)
    expect_true(is.list(plots))
    expect_length(plots, 2)
    expect_s3_class(plots[[1]], "ggplot")
})

test_that("Network with multiple split_by columns returns a list with one plot per combination", {
    plots <- suppressMessages(Network(net_links, from = "from", to = "to",
                                      split_by = c("group", "type"), combine = FALSE))
    expect_true(is.list(plots))
    expect_length(plots, 4)
    expect_s3_class(plots[[1]], "ggplot")
})

test_that("Network with multiple split_by columns returns combined plot", {
    p <- suppressMessages(Network(net_links, from = "from", to = "to",
                                  split_by = c("group", "type"), combine = TRUE))
    expect_true(inherits(p, "patchwork") || inherits(p, "gg"))
})

test_that("Network split_by keeps link endpoints aligned with nodes when splits differ", {
    # G1 has 5 nodes (A, B, C, D, E) but G2 only 4 (A missing): links in
    # the combined plot must stay anchored to each split's own node
    # positions instead of resolving against the first split's layout.
    p <- Network(net_links, from = "from", to = "to",
                 split_by = "group", combine = TRUE)
    # The combined plot exposes the layout of every split, each row
    # tagged with its split level, plus a combined graph carrying all
    # nodes and links.
    expect_true("group" %in% colnames(p$data))
    expect_equal(nrow(p$data), 9) # 5 nodes (G1) + 4 nodes (G2)
    expect_equal(sort(unique(p$data$group)), c("G1", "G2"))
    g <- attr(p$data, "graph")
    expect_equal(igraph::vcount(g), 9)
    expect_equal(igraph::ecount(g), 8)
    # The combined links table is exposed too, tagged with the split
    # column.
    eg <- attr(p$data, "edges")
    expect_equal(nrow(eg), 8)
    expect_true(all(c("from", "to", "weight", "group") %in% colnames(eg)))
    expect_equal(sort(unique(eg$group)), c("G1", "G2"))
    b <- ggplot2::ggplot_build(p)
    # Expected node positions for the last split (G2): layout_in_circle
    # of the graph with nodes C, D, E, B.
    g2 <- igraph::graph_from_data_frame(net_links[net_links$group == "G2", c("from", "to")])
    pos <- igraph::layout_in_circle(g2)
    check_endpoint <- function(x, y) {
        any(apply(pos, 1, function(v) sqrt(sum((c(x, y) - v)^2))) < 1e-6)
    }
    # The edge layer is the one carrying xend/yend (interpolated arcs).
    edge_layer <- Filter(function(d) "xend" %in% colnames(d) && nrow(d) > 0, b$data)
    expect_length(edge_layer, 1)
    edges <- edge_layer[[1]]
    expect_equal(nrow(edges), 400) # 4 edges x 100 interpolated points
    for (g in unique(edges$group)) {
        d <- edges[edges$group == g, ]
        expect_true(check_endpoint(d$x[1], d$y[1]), info = paste("edge", g, "start"))
        expect_true(check_endpoint(d$xend[1], d$yend[1]), info = paste("edge", g, "end"))
    }
})

test_that("Network with directed = FALSE works", {
    p <- Network(net_links, from = "from", to = "to", directed = FALSE)
    expect_s3_class(p, "ggplot")
})

test_that("Network with add_label = FALSE works", {
    p <- Network(net_links, from = "from", to = "to", add_label = FALSE)
    expect_s3_class(p, "ggplot")
})
