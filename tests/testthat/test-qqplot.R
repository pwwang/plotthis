set.seed(8525)
qq_data <- data.frame(
    val = c(rnorm(80), rnorm(80, mean = 0.5)),
    group = factor(rep(c("A", "B"), each = 80))
)

test_that("QQPlot basic usage works", {
    p <- QQPlot(qq_data, val = "val")
    expect_s3_class(p, "ggplot")
    expect_true(!is.null(attr(p, "height")))
    expect_true(!is.null(attr(p, "width")))
})

test_that("QQPlot with type = 'pp' works", {
    p <- QQPlot(qq_data, val = "val", type = "pp")
    expect_s3_class(p, "ggplot")
})

test_that("QQPlot with title and subtitle works", {
    p <- QQPlot(qq_data, val = "val", title = "QQ Title", subtitle = "QQ Subtitle")
    expect_s3_class(p, "ggplot")
    expect_equal(p$labels$title, "QQ Title")
    expect_equal(p$labels$subtitle, "QQ Subtitle")
})

test_that("QQPlot with split_by returns patchwork", {
    p <- QQPlot(qq_data, val = "val", split_by = "group", combine = TRUE)
    expect_true(inherits(p, "patchwork") || inherits(p, "gg"))
})

test_that("QQPlot with split_by and combine = FALSE returns list", {
    plots <- QQPlot(qq_data, val = "val", split_by = "group", combine = FALSE)
    expect_true(is.list(plots))
    expect_length(plots, 2)
    expect_s3_class(plots[[1]], "ggplot")
})

test_that("QQPlot with xlab and ylab works", {
    p <- QQPlot(qq_data, val = "val", xlab = "Theory", ylab = "Sample")
    expect_s3_class(p, "ggplot")
    expect_equal(p$labels$x, "Theory")
    expect_equal(p$labels$y, "Sample")
})

test_that("QQPlot with band (qqplotr) works", {
    skip_if_not_installed("qqplotr")
    p <- QQPlot(qq_data, val = "val", band = list())
    expect_s3_class(p, "ggplot")
})
