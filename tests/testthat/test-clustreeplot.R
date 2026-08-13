set.seed(8525)
# ClustreePlot expects data with columns named with a common prefix
# followed by resolution values. E.g., "res.0.1", "res.0.3", "res.0.5"
n_cells <- 100
clustree_data <- data.frame(
    res_0.1 = sample(c("A", "B"), n_cells, replace = TRUE),
    res_0.3 = sample(c("A", "B", "C"), n_cells, replace = TRUE),
    res_0.5 = sample(c("A", "B", "C", "D"), n_cells, replace = TRUE),
    res_0.8 = sample(c("A", "B", "C", "D", "E"), n_cells, replace = TRUE),
    group = factor(rep(c("G1", "G2"), each = n_cells / 2))
)

test_that("ClustreePlot basic usage works", {
    skip_if_not_installed("clustree")
    p <- ClustreePlot(clustree_data, prefix = "res_")
    expect_s3_class(p, "ggplot")
    expect_true(!is.null(attr(p, "height")))
    expect_true(!is.null(attr(p, "width")))
})

test_that("ClustreePlot with title and subtitle works", {
    skip_if_not_installed("clustree")
    p <- ClustreePlot(clustree_data, prefix = "res_",
                      title = "Clustree Title", subtitle = "Subtitle")
    expect_s3_class(p, "ggplot")
    expect_equal(p$labels$title, "Clustree Title")
    expect_equal(p$labels$subtitle, "Subtitle")
})

test_that("ClustreePlot with flip = TRUE works", {
    skip_if_not_installed("clustree")
    p <- ClustreePlot(clustree_data, prefix = "res_", flip = TRUE)
    expect_s3_class(p, "ggplot")
})

test_that("ClustreePlot with split_by returns patchwork", {
    skip_if_not_installed("clustree")
    p <- ClustreePlot(clustree_data, prefix = "res_",
                      split_by = "group", combine = TRUE)
    expect_true(inherits(p, "patchwork") || inherits(p, "gg"))
})

test_that("ClustreePlot with combine = FALSE returns list", {
    skip_if_not_installed("clustree")
    plots <- ClustreePlot(clustree_data, prefix = "res_",
                          split_by = "group", combine = FALSE)
    expect_true(is.list(plots))
    expect_length(plots, 2)
    expect_s3_class(plots[[1]], "ggplot")
})

test_that("ClustreePlot with multiple split_by columns returns a list with one plot per combination", {
    skip_if_not_installed("clustree")
    data2 <- clustree_data
    data2$split2 <- factor(rep(c("s1", "s2"), 50))
    plots <- suppressMessages(ClustreePlot(data2, prefix = "res_",
                                           split_by = c("group", "split2"), combine = FALSE))
    expect_true(is.list(plots))
    expect_length(plots, 4)
    expect_s3_class(plots[[1]], "ggplot")
})

test_that("ClustreePlot with multiple split_by columns returns combined plot", {
    skip_if_not_installed("clustree")
    data2 <- clustree_data
    data2$split2 <- factor(rep(c("s1", "s2"), 50))
    p <- suppressMessages(ClustreePlot(data2, prefix = "res_",
                                       split_by = c("group", "split2"), combine = TRUE))
    expect_true(inherits(p, "gg") || inherits(p, "patchwork"))
})

test_that("ClustreePlot split_by keeps link endpoints aligned with nodes when splits differ", {
    skip_if_not_installed("clustree")
    # ggraph-based plots (same class as Network) carry their layout in
    # the plot data: links in the combined plot must stay anchored to
    # each split's own node positions instead of resolving against the
    # first split's layout.
    p <- ClustreePlot(clustree_data, prefix = "res_",
                      split_by = "group", combine = TRUE)
    # The combined plot exposes the layout of every split, each row
    # tagged with its split level, and a combined graph attribute.
    expect_true("group" %in% colnames(p$data))
    expect_equal(sort(unique(p$data$group)), c("G1", "G2"))
    expect_false(is.null(attr(p$data, "graph")))
    expect_true("group" %in% colnames(attr(p$data, "edges")))
    b <- ggplot2::ggplot_build(p)
    # The edge layer is the one carrying the interpolation index; the
    # node layer is the one with point shape/colour aesthetics.
    edge_layer <- Filter(function(d) "index" %in% colnames(d), b$data)
    node_layer <- Filter(function(d) "shape" %in% colnames(d), b$data)
    expect_length(edge_layer, 1)
    expect_length(node_layer, 1)
    pos <- as.matrix(node_layer[[1]][, c("x", "y")])
    check_endpoint <- function(x, y) {
        any(apply(pos, 1, function(v) sqrt(sum((c(x, y) - v)^2))) < 1e-6)
    }
    edges <- edge_layer[[1]]
    for (g in unique(edges$group)) {
        d <- edges[edges$group == g, ]
        expect_true(check_endpoint(d$x[1], d$y[1]), info = paste("edge", g, "start"))
        expect_true(check_endpoint(d$x[nrow(d)], d$y[nrow(d)]), info = paste("edge", g, "end"))
    }
})

test_that("ClustreePlot with invalid prefix errors informatively", {
    skip_if_not_installed("clustree")
    expect_error(ClustreePlot(clustree_data, prefix = "nonexistent_"), "nonexistent_")
})
