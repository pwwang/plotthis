set.seed(8525)
data <- data.frame(
    x = rnorm(100),
    y = rnorm(100),
    size_col = runif(100, 1, 10),
    color_col = factor(rep(c("A", "B", "C", "D"), 25)),
    color_num = rnorm(100),
    group = factor(rep(c("g1", "g2"), 50))
)

test_that("ScatterPlot returns a ggplot for basic usage", {
    p <- ScatterPlot(data, x = "x", y = "y")
    expect_s3_class(p, "ggplot")
    expect_true(!is.null(attr(p, "height")))
    expect_true(!is.null(attr(p, "width")))
})

test_that("ScatterPlot works with color_by (discrete)", {
    p <- ScatterPlot(data, x = "x", y = "y", color_by = "color_col")
    expect_s3_class(p, "ggplot")
})

test_that("ScatterPlot works with color_by (continuous)", {
    p <- ScatterPlot(data, x = "x", y = "y", color_by = "color_num")
    expect_s3_class(p, "ggplot")
})

test_that("ScatterPlot works with size_by column", {
    p <- ScatterPlot(data, x = "x", y = "y", size_by = "size_col")
    expect_s3_class(p, "ggplot")
})

test_that("ScatterPlot works with numeric size_by value", {
    p <- ScatterPlot(data, x = "x", y = "y", size_by = 3)
    expect_s3_class(p, "ggplot")
})

test_that("ScatterPlot sets title and labels", {
    p <- ScatterPlot(data, x = "x", y = "y", title = "Scatter", xlab = "X", ylab = "Y")
    expect_s3_class(p, "ggplot")
    expect_equal(p$labels$title, "Scatter")
    expect_equal(p$labels$x, "X")
    expect_equal(p$labels$y, "Y")
})

test_that("ScatterPlot with split_by returns combined or list", {
    p_combined <- ScatterPlot(data, x = "x", y = "y", split_by = "group", combine = TRUE)
    expect_true(inherits(p_combined, "gg") || inherits(p_combined, "patchwork"))

    p_list <- ScatterPlot(data, x = "x", y = "y", split_by = "group", combine = FALSE)
    expect_true(is.list(p_list))
    expect_length(p_list, 2)
})

test_that("ScatterPlot works with highlight as index vector", {
    p <- ScatterPlot(data, x = "x", y = "y", highlight = 1:10)
    expect_s3_class(p, "ggplot")
})

test_that("ScatterPlot works with highlight as expression string", {
    p <- ScatterPlot(data, x = "x", y = "y", highlight = "x > 0")
    expect_s3_class(p, "ggplot")
})

test_that("ScatterPlot works with facet_by", {
    p <- ScatterPlot(data, x = "x", y = "y", facet_by = "group")
    expect_s3_class(p, "ggplot")
})

test_that("ScatterPlot works with different shapes", {
    p <- ScatterPlot(data, x = "x", y = "y", shape = 16)
    expect_s3_class(p, "ggplot")
    p2 <- ScatterPlot(data, x = "x", y = "y", shape = 21)
    expect_s3_class(p2, "ggplot")
})

test_that("ScatterPlot width attribute is numeric", {
    p <- ScatterPlot(data, x = "x", y = "y")
    expect_true(is.numeric(attr(p, "width")))
    expect_true(is.numeric(attr(p, "height")))
})

test_that("ScatterPlot works with alpha", {
    p <- ScatterPlot(data, x = "x", y = "y", alpha = 0.5)
    expect_s3_class(p, "ggplot")
})
