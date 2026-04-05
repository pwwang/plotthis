set.seed(8525)
data <- data.frame(
    x = factor(rep(c("A", "B", "C", "D"), each = 25), levels = c("A", "B", "C", "D")),
    y = rnorm(100),
    group1 = factor(rep(c("g1", "g2"), 50)),
    group2 = factor(rep(c("f1", "f2"), each = 50))
)

# BoxPlot tests
test_that("BoxPlot returns a ggplot for basic usage", {
    p <- BoxPlot(data, x = "x", y = "y")
    expect_s3_class(p, "ggplot")
    expect_true(!is.null(attr(p, "height")))
    expect_true(!is.null(attr(p, "width")))
})

test_that("BoxPlot sets title and labels", {
    p <- BoxPlot(data, x = "x", y = "y", title = "Box", xlab = "Groups", ylab = "Value")
    expect_equal(p$labels$title, "Box")
    expect_equal(p$labels$x, "Groups")
    expect_equal(p$labels$y, "Value")
})

test_that("BoxPlot works with group_by", {
    p <- BoxPlot(data, x = "x", y = "y", group_by = "group1")
    expect_s3_class(p, "ggplot")
})

test_that("BoxPlot with split_by returns combined plot", {
    p <- BoxPlot(data, x = "x", y = "y", split_by = "group2", combine = TRUE)
    expect_true(inherits(p, "gg") || inherits(p, "patchwork"))
})

test_that("BoxPlot with split_by and combine = FALSE returns list", {
    plots <- BoxPlot(data, x = "x", y = "y", split_by = "group2", combine = FALSE)
    expect_true(is.list(plots))
    expect_length(plots, 2)
    expect_s3_class(plots[[1]], "ggplot")
})

test_that("BoxPlot with flip = TRUE works", {
    p <- BoxPlot(data, x = "x", y = "y", flip = TRUE)
    expect_s3_class(p, "ggplot")
})

test_that("BoxPlot with facet_by works", {
    p <- BoxPlot(data, x = "x", y = "y", facet_by = "group2")
    expect_s3_class(p, "ggplot")
})

test_that("BoxPlot with add_point = TRUE works", {
    p <- BoxPlot(data, x = "x", y = "y", add_point = TRUE)
    expect_s3_class(p, "ggplot")
})

test_that("BoxPlot with fill_mode = 'x' works", {
    p <- BoxPlot(data, x = "x", y = "y", fill_mode = "x")
    expect_s3_class(p, "ggplot")
})

test_that("BoxPlot with fill_mode = 'mean' works", {
    p <- BoxPlot(data, x = "x", y = "y", fill_mode = "mean")
    expect_s3_class(p, "ggplot")
})

test_that("BoxPlot with add_trend = TRUE works", {
    p <- BoxPlot(data, x = "x", y = "y", add_trend = TRUE)
    expect_s3_class(p, "ggplot")
})

test_that("BoxPlot with sort_x works", {
    p <- BoxPlot(data, x = "x", y = "y", sort_x = "mean(y)")
    expect_s3_class(p, "ggplot")
})

test_that("BoxPlot base = 'bar' works", {
    p <- BoxPlot(data, x = "x", y = "y", base = "bar")
    expect_s3_class(p, "ggplot")
})

# ViolinPlot tests
test_that("ViolinPlot returns a ggplot for basic usage", {
    p <- ViolinPlot(data, x = "x", y = "y")
    expect_s3_class(p, "ggplot")
    expect_true(!is.null(attr(p, "height")))
    expect_true(!is.null(attr(p, "width")))
})

test_that("ViolinPlot with add_box = TRUE works", {
    p <- ViolinPlot(data, x = "x", y = "y", add_box = TRUE)
    expect_s3_class(p, "ggplot")
})

test_that("ViolinPlot with add_point = TRUE works", {
    p <- ViolinPlot(data, x = "x", y = "y", add_point = TRUE)
    expect_s3_class(p, "ggplot")
})

test_that("ViolinPlot with group_by works", {
    p <- ViolinPlot(data, x = "x", y = "y", group_by = "group1")
    expect_s3_class(p, "ggplot")
})

test_that("ViolinPlot with split_by and combine = FALSE returns list", {
    plots <- ViolinPlot(data, x = "x", y = "y", split_by = "group2", combine = FALSE)
    expect_true(is.list(plots))
    expect_length(plots, 2)
})

test_that("ViolinPlot with facet_by works", {
    p <- ViolinPlot(data, x = "x", y = "y", facet_by = "group2")
    expect_s3_class(p, "ggplot")
})

test_that("ViolinPlot with fill_mode = 'mean' works", {
    p <- ViolinPlot(data, x = "x", y = "y", fill_mode = "mean")
    expect_s3_class(p, "ggplot")
})

test_that("ViolinPlot sets title and labels", {
    p <- ViolinPlot(data, x = "x", y = "y", title = "Violin", xlab = "X", ylab = "Y")
    expect_equal(p$labels$title, "Violin")
})

# BeeswarmPlot tests
test_that("BeeswarmPlot errors when add_violin = TRUE", {
    expect_error(
        BeeswarmPlot(data, x = "x", y = "y", add_violin = TRUE),
        "Adding violin"
    )
})

test_that("BeeswarmPlot returns a ggplot for basic usage", {
    skip_if_not_installed("ggbeeswarm")
    p <- BeeswarmPlot(data, x = "x", y = "y")
    expect_s3_class(p, "ggplot")
})
