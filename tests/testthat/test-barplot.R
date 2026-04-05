set.seed(8525)
data <- data.frame(
    x = factor(rep(c("A", "B", "C", "D"), each = 25), levels = c("A", "B", "C", "D")),
    y = rnorm(100),
    group1 = factor(rep(c("g1", "g2"), 50)),
    group2 = factor(rep(c("f1", "f2"), each = 50))
)

test_that("BarPlot returns a ggplot object for basic usage", {
    p <- BarPlot(data, x = "x")
    expect_s3_class(p, "ggplot")
    expect_true(!is.null(attr(p, "height")))
    expect_true(!is.null(attr(p, "width")))
})

test_that("BarPlot works with y column", {
    p <- BarPlot(data, x = "x", y = "y")
    expect_s3_class(p, "ggplot")
})

test_that("BarPlot works with group_by", {
    p <- BarPlot(data, x = "x", y = "y", group_by = "group1")
    expect_s3_class(p, "ggplot")
})

test_that("BarPlot with split_by returns patchwork when combine = TRUE", {
    p <- BarPlot(data, x = "x", y = "y", split_by = "group2", combine = TRUE)
    expect_s3_class(p, c("patchwork", "gg"), exact = FALSE)
    expect_true(inherits(p, "patchwork") || inherits(p, "gg"))
})

test_that("BarPlot with split_by and combine = FALSE returns a list", {
    plots <- BarPlot(data, x = "x", y = "y", split_by = "group2", combine = FALSE)
    expect_true(is.list(plots))
    expect_length(plots, 2)
    expect_s3_class(plots[[1]], "ggplot")
    expect_s3_class(plots[[2]], "ggplot")
})

test_that("BarPlot sets title and subtitle", {
    p <- BarPlot(data, x = "x", title = "Test Title", subtitle = "Test Subtitle")
    expect_s3_class(p, "ggplot")
    labels <- p$labels
    expect_equal(labels$title, "Test Title")
    expect_equal(labels$subtitle, "Test Subtitle")
})

test_that("BarPlot sets xlab and ylab", {
    p <- BarPlot(data, x = "x", xlab = "X Label", ylab = "Y Label")
    expect_s3_class(p, "ggplot")
    labels <- p$labels
    expect_equal(labels$x, "X Label")
    expect_equal(labels$y, "Y Label")
})

test_that("BarPlot works with flip = TRUE", {
    p <- BarPlot(data, x = "x", y = "y", flip = TRUE)
    expect_s3_class(p, "ggplot")
})

test_that("BarPlot works with facet_by", {
    p <- BarPlot(data, x = "x", y = "y", facet_by = "group2")
    expect_s3_class(p, "ggplot")
})

test_that("BarPlot works with position = 'stack'", {
    p <- BarPlot(data, x = "x", group_by = "group1", position = "stack")
    expect_s3_class(p, "ggplot")
})

test_that("BarPlot works with fill_by = FALSE", {
    p <- BarPlot(data, x = "x", fill_by = FALSE)
    expect_s3_class(p, "ggplot")
})

test_that("BarPlot works with add_line parameter", {
    p <- BarPlot(data, x = "x", y = "y", add_line = 0)
    expect_s3_class(p, "ggplot")
})

test_that("BarPlot works with add_trend = TRUE", {
    p <- BarPlot(data, x = "x", y = "y", add_trend = TRUE)
    expect_s3_class(p, "ggplot")
})

test_that("BarPlot works with custom palette", {
    p <- BarPlot(data, x = "x", palette = "Set1")
    expect_s3_class(p, "ggplot")
})

test_that("BarPlot width attribute is numeric", {
    p <- BarPlot(data, x = "x")
    expect_true(is.numeric(attr(p, "width")))
    expect_true(is.numeric(attr(p, "height")))
})

test_that("BarPlot works with multiple split levels", {
    data2 <- data
    data2$group3 <- factor(rep(c("h1", "h2", "h3"), length.out = nrow(data2)))
    plots <- BarPlot(data2, x = "x", split_by = "group3", combine = FALSE)
    expect_true(is.list(plots))
    expect_length(plots, 3)
})

test_that("LollipopPlot returns a ggplot", {
    p <- LollipopPlot(data, x = "y", y = "x")
    expect_s3_class(p, "ggplot")
})

test_that("WaterfallPlot returns a ggplot", {
    wf_data <- data.frame(
        x = c(0.5, -0.3, 0.8, -1.2, 0.4),
        y = c("GeneA", "GeneB", "GeneC", "GeneD", "GeneE")
    )
    p <- WaterfallPlot(wf_data, x = "x", y = "y")
    expect_s3_class(p, "ggplot")
})

test_that("SplitBarPlot returns a ggplot", {
    wf_data <- data.frame(
        x = c(0.5, -0.3, 0.8, -1.2, 0.4),
        y = c("GeneA", "GeneB", "GeneC", "GeneD", "GeneE")
    )
    p <- SplitBarPlot(wf_data, x = "x", y = "y")
    expect_s3_class(p, "ggplot")
})
