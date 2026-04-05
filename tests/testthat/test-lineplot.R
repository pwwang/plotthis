set.seed(8525)
data <- data.frame(
    x = factor(rep(c("A", "B", "C", "D", "E"), each = 20), levels = c("A", "B", "C", "D", "E")),
    y = rnorm(100),
    group1 = factor(rep(c("g1", "g2"), 50)),
    group2 = factor(rep(c("f1", "f2"), each = 50))
)

test_that("LinePlot returns a ggplot for basic usage", {
    p <- LinePlot(data, x = "x", y = "y")
    expect_s3_class(p, "ggplot")
    expect_true(!is.null(attr(p, "height")))
    expect_true(!is.null(attr(p, "width")))
})

test_that("LinePlot works without explicit y (aggregates count)", {
    p <- LinePlot(data, x = "x")
    expect_s3_class(p, "ggplot")
})

test_that("LinePlot with group_by works", {
    p <- LinePlot(data, x = "x", y = "y", group_by = "group1")
    expect_s3_class(p, "ggplot")
})

test_that("LinePlot sets title and labels", {
    p <- LinePlot(data, x = "x", y = "y", title = "Line", xlab = "X Axis", ylab = "Y Axis")
    expect_s3_class(p, "ggplot")
    expect_equal(p$labels$title, "Line")
    expect_equal(p$labels$x, "X Axis")
    expect_equal(p$labels$y, "Y Axis")
})

test_that("LinePlot with split_by returns patchwork", {
    p <- LinePlot(data, x = "x", y = "y", split_by = "group2", combine = TRUE)
    expect_true(inherits(p, "gg") || inherits(p, "patchwork"))
})

test_that("LinePlot with split_by and combine = FALSE returns list", {
    plots <- LinePlot(data, x = "x", y = "y", split_by = "group2", combine = FALSE)
    expect_true(is.list(plots))
    expect_length(plots, 2)
    expect_s3_class(plots[[1]], "ggplot")
})

test_that("LinePlot with facet_by works", {
    p <- LinePlot(data, x = "x", y = "y", facet_by = "group2")
    expect_s3_class(p, "ggplot")
})

test_that("LinePlot with add_errorbars works when columns provided", {
    data2 <- data
    data2$ymin <- data2$y - 0.5
    data2$ymax <- data2$y + 0.5
    p <- LinePlot(data2, x = "x", y = "y", group_by = "group1",
                  add_errorbars = TRUE, errorbar_min = "ymin", errorbar_max = "ymax")
    expect_s3_class(p, "ggplot")
})

test_that("LinePlot with add_hline works", {
    p <- LinePlot(data, x = "x", y = "y", add_hline = 0)
    expect_s3_class(p, "ggplot")
})

test_that("LinePlot with flip = TRUE works", {
    p <- LinePlot(data, x = "x", y = "y", flip = TRUE)
    expect_s3_class(p, "ggplot")
})

test_that("LinePlot width/height attributes are numeric", {
    p <- LinePlot(data, x = "x", y = "y")
    expect_true(is.numeric(attr(p, "width")))
    expect_true(is.numeric(attr(p, "height")))
})
