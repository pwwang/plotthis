set.seed(8525)
# TrendPlot expects: x = factor column for x-axis steps, group_by = fill group
trend_data <- data.frame(
    timepoint = factor(rep(c("T1", "T2", "T3", "T4"), times = 60),
                       levels = c("T1", "T2", "T3", "T4")),
    cell_type = factor(rep(rep(c("TypeA", "TypeB", "TypeC"), each = 4), 20)),
    sample = factor(rep(c("S1", "S2"), each = 120))
)

test_that("TrendPlot basic usage works", {
    p <- TrendPlot(trend_data, x = "timepoint")
    expect_s3_class(p, "ggplot")
    expect_true(!is.null(attr(p, "height")))
    expect_true(!is.null(attr(p, "width")))
})

test_that("TrendPlot with group_by works", {
    p <- TrendPlot(trend_data, x = "timepoint", group_by = "cell_type")
    expect_s3_class(p, "ggplot")
})

test_that("TrendPlot with title and subtitle works", {
    p <- TrendPlot(trend_data, x = "timepoint", group_by = "cell_type",
                   title = "Trend Title", subtitle = "Trend Subtitle")
    expect_s3_class(p, "ggplot")
    expect_equal(p$labels$title, "Trend Title")
    expect_equal(p$labels$subtitle, "Trend Subtitle")
})

test_that("TrendPlot with split_by returns patchwork", {
    p <- TrendPlot(trend_data, x = "timepoint", group_by = "cell_type",
                   split_by = "sample", combine = TRUE)
    expect_true(inherits(p, "patchwork") || inherits(p, "gg"))
})

test_that("TrendPlot with combine = FALSE returns list", {
    plots <- TrendPlot(trend_data, x = "timepoint", group_by = "cell_type",
                       split_by = "sample", combine = FALSE)
    expect_true(is.list(plots))
    expect_length(plots, 2)
    expect_s3_class(plots[[1]], "ggplot")
})

test_that("TrendPlot with scale_y = TRUE works", {
    p <- TrendPlot(trend_data, x = "timepoint", group_by = "cell_type", scale_y = TRUE)
    expect_s3_class(p, "ggplot")
})

test_that("TrendPlot with facet_by works", {
    p <- TrendPlot(trend_data, x = "timepoint", group_by = "cell_type",
                   facet_by = "sample")
    expect_s3_class(p, "ggplot")
})

test_that("TrendPlot with x_text_angle works", {
    p <- TrendPlot(trend_data, x = "timepoint", group_by = "cell_type",
                   x_text_angle = 45)
    expect_s3_class(p, "ggplot")
})
