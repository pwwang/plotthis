set.seed(8525)
data <- data.frame(
    x = c(rnorm(500, mean = 5, sd = 2), rnorm(500, mean = 8, sd = 1.5)),
    group = factor(rep(c("A", "B"), each = 500)),
    facet = factor(rep(c("F1", "F2"), 500))
)

# DensityPlot tests
test_that("DensityPlot returns a ggplot for basic usage", {
    p <- DensityPlot(data, x = "x")
    expect_s3_class(p, "ggplot")
    expect_true(!is.null(attr(p, "height")))
    expect_true(!is.null(attr(p, "width")))
})

test_that("DensityPlot with group_by works", {
    p <- DensityPlot(data, x = "x", group_by = "group")
    expect_s3_class(p, "ggplot")
})

test_that("DensityPlot sets title and labels", {
    p <- DensityPlot(data, x = "x", title = "Density", xlab = "Value")
    expect_equal(p$labels$title, "Density")
})

test_that("DensityPlot with split_by returns combined", {
    p <- DensityPlot(data, x = "x", split_by = "facet", combine = TRUE)
    expect_true(inherits(p, "gg") || inherits(p, "patchwork"))
})

test_that("DensityPlot with split_by and combine = FALSE returns list", {
    plots <- DensityPlot(data, x = "x", split_by = "facet", combine = FALSE)
    expect_true(is.list(plots))
    expect_length(plots, 2)
})

test_that("DensityPlot with facet_by works", {
    p <- DensityPlot(data, x = "x", facet_by = "facet")
    expect_s3_class(p, "ggplot")
})

test_that("DensityPlot with add_bars = TRUE works", {
    p <- DensityPlot(data, x = "x", group_by = "group", add_bars = TRUE)
    expect_s3_class(p, "ggplot")
})

test_that("DensityPlot with flip = TRUE works", {
    p <- DensityPlot(data, x = "x", flip = TRUE)
    expect_s3_class(p, "ggplot")
})

test_that("DensityPlot width/height attributes are numeric", {
    p <- DensityPlot(data, x = "x")
    expect_true(is.numeric(attr(p, "width")))
    expect_true(is.numeric(attr(p, "height")))
})

# Histogram tests
test_that("Histogram returns a ggplot for basic usage", {
    p <- Histogram(data, x = "x")
    expect_s3_class(p, "ggplot")
    expect_true(!is.null(attr(p, "height")))
    expect_true(!is.null(attr(p, "width")))
})

test_that("Histogram with group_by works", {
    p <- Histogram(data, x = "x", group_by = "group")
    expect_s3_class(p, "ggplot")
})

test_that("Histogram with binwidth works", {
    p <- Histogram(data, x = "x", binwidth = 0.5)
    expect_s3_class(p, "ggplot")
})

test_that("Histogram with bins works", {
    p <- Histogram(data, x = "x", bins = 20)
    expect_s3_class(p, "ggplot")
})

test_that("Histogram with add_bars = TRUE works", {
    p <- Histogram(data, x = "x", group_by = "group", add_bars = TRUE)
    expect_s3_class(p, "ggplot")
})

test_that("Histogram with add_trend = TRUE works", {
    p <- Histogram(data, x = "x", group_by = "group", add_trend = TRUE)
    expect_s3_class(p, "ggplot")
})

test_that("Histogram with split_by and combine = FALSE returns list", {
    plots <- Histogram(data, x = "x", split_by = "facet", combine = FALSE)
    expect_true(is.list(plots))
    expect_length(plots, 2)
})

test_that("Histogram sets title", {
    p <- Histogram(data, x = "x", title = "Histogram Title")
    expect_equal(p$labels$title, "Histogram Title")
})

test_that("Histogram width/height attributes are numeric", {
    p <- Histogram(data, x = "x")
    expect_true(is.numeric(attr(p, "width")))
    expect_true(is.numeric(attr(p, "height")))
})

# RidgePlot tests
test_that("RidgePlot returns a ggplot for basic usage", {
    skip_if_not_installed("ggridges")
    p <- RidgePlot(data, x = "x", group_by = "group")
    expect_s3_class(p, "ggplot")
    expect_true(!is.null(attr(p, "height")))
    expect_true(!is.null(attr(p, "width")))
})

test_that("RidgePlot sets title", {
    skip_if_not_installed("ggridges")
    p <- RidgePlot(data, x = "x", group_by = "group", title = "Ridge Title")
    expect_equal(p$labels$title, "Ridge Title")
})
