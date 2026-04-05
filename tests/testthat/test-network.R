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

test_that("Network with directed = FALSE works", {
    p <- Network(net_links, from = "from", to = "to", directed = FALSE)
    expect_s3_class(p, "ggplot")
})

test_that("Network with add_label = FALSE works", {
    p <- Network(net_links, from = "from", to = "to", add_label = FALSE)
    expect_s3_class(p, "ggplot")
})
