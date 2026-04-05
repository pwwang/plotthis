set.seed(8525)
data <- data.frame(
    category = factor(sample(c("A", "B", "C", "D"), 100, replace = TRUE)),
    value = runif(100, 1, 10),
    group = factor(rep(c("g1", "g2"), 50))
)

# PieChart tests
test_that("PieChart returns a ggplot for basic usage", {
    p <- PieChart(data, x = "category")
    expect_s3_class(p, "ggplot")
    expect_true(!is.null(attr(p, "height")))
    expect_true(!is.null(attr(p, "width")))
})

test_that("PieChart works with y column for values", {
    p <- PieChart(data, x = "category", y = "value")
    expect_s3_class(p, "ggplot")
})

test_that("PieChart sets title", {
    p <- PieChart(data, x = "category", title = "Pie Title")
    expect_equal(p$labels$title, "Pie Title")
})

test_that("PieChart with split_by returns combined plot", {
    p <- PieChart(data, x = "category", split_by = "group", combine = TRUE)
    expect_true(inherits(p, "gg") || inherits(p, "patchwork"))
})

test_that("PieChart with split_by and combine = FALSE returns list", {
    plots <- PieChart(data, x = "category", split_by = "group", combine = FALSE)
    expect_true(is.list(plots))
    expect_length(plots, 2)
    expect_s3_class(plots[[1]], "ggplot")
})

test_that("PieChart with facet_by works", {
    p <- PieChart(data, x = "category", facet_by = "group")
    expect_s3_class(p, "ggplot")
})

test_that("PieChart with clockwise = FALSE works", {
    p <- PieChart(data, x = "category", clockwise = FALSE)
    expect_s3_class(p, "ggplot")
})

test_that("PieChart with custom palette works", {
    p <- PieChart(data, x = "category", palette = "Set2")
    expect_s3_class(p, "ggplot")
})

test_that("PieChart width/height attributes are numeric", {
    p <- PieChart(data, x = "category")
    expect_true(is.numeric(attr(p, "width")))
    expect_true(is.numeric(attr(p, "height")))
})

# RingPlot tests
test_that("RingPlot returns a ggplot for basic usage", {
    p <- RingPlot(data, x = "category")
    expect_s3_class(p, "ggplot")
    expect_true(!is.null(attr(p, "height")))
    expect_true(!is.null(attr(p, "width")))
})

test_that("RingPlot works with y column", {
    p <- RingPlot(data, x = "category", y = "value")
    expect_s3_class(p, "ggplot")
})

test_that("RingPlot sets title", {
    p <- RingPlot(data, x = "category", title = "Ring Title")
    expect_equal(p$labels$title, "Ring Title")
})

test_that("RingPlot with split_by and combine = FALSE returns list", {
    plots <- RingPlot(data, x = "category", split_by = "group", combine = FALSE)
    expect_true(is.list(plots))
    expect_length(plots, 2)
})

test_that("RingPlot width/height attributes are numeric", {
    p <- RingPlot(data, x = "category")
    expect_true(is.numeric(attr(p, "width")))
    expect_true(is.numeric(attr(p, "height")))
})
