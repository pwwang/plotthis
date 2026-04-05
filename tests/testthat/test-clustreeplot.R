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

test_that("ClustreePlot with invalid prefix errors informatively", {
    skip_if_not_installed("clustree")
    expect_error(ClustreePlot(clustree_data, prefix = "nonexistent_"), "nonexistent_")
})
