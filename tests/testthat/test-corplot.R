set.seed(8525)
cor_data <- data.frame(
    v1 = rnorm(80),
    v2 = rnorm(80),
    v3 = rnorm(80),
    group = factor(rep(c("A", "B"), 40)),
    facet = factor(rep(c("f1", "f2"), each = 40))
)
cor_data$v2 <- cor_data$v1 * 0.8 + rnorm(80, sd = 0.3)
cor_data$v3 <- cor_data$v1 * 0.4 + rnorm(80)

test_that("CorPlot returns a ggplot for basic usage", {
    p <- CorPlot(cor_data, x = "v1", y = "v2")
    expect_s3_class(p, "ggplot")
    expect_true(!is.null(attr(p, "height")))
    expect_true(!is.null(attr(p, "width")))
})

test_that("CorPlot with group_by works", {
    p <- CorPlot(cor_data, x = "v1", y = "v2", group_by = "group")
    expect_s3_class(p, "ggplot")
})

test_that("CorPlot sets title and labels", {
    p <- CorPlot(cor_data, x = "v1", y = "v2", title = "Correlation", xlab = "V1", ylab = "V2")
    expect_equal(p$labels$title, "Correlation")
    expect_equal(p$labels$x, "V1")
    expect_equal(p$labels$y, "V2")
})

test_that("CorPlot with split_by and combine = FALSE returns list", {
    plots <- CorPlot(cor_data, x = "v1", y = "v2", split_by = "facet", combine = FALSE)
    expect_true(is.list(plots))
    expect_length(plots, 2)
    expect_s3_class(plots[[1]], "ggplot")
})

test_that("CorPlot with facet_by works", {
    p <- CorPlot(cor_data, x = "v1", y = "v2", facet_by = "facet")
    expect_s3_class(p, "ggplot")
})

test_that("CorPlot with add_smooth = FALSE works", {
    p <- CorPlot(cor_data, x = "v1", y = "v2", add_smooth = FALSE)
    expect_s3_class(p, "ggplot")
})

test_that("CorPlot with different anno_items works", {
    p <- CorPlot(cor_data, x = "v1", y = "v2", anno_items = c("r2", "p"))
    expect_s3_class(p, "ggplot")
})

test_that("CorPlot width/height attributes are numeric", {
    p <- CorPlot(cor_data, x = "v1", y = "v2")
    expect_true(is.numeric(attr(p, "width")))
    expect_true(is.numeric(attr(p, "height")))
})

# CorPairsPlot tests
test_that("CorPairsPlot returns a ggplot for basic usage", {
    p <- CorPairsPlot(cor_data, columns = c("v1", "v2", "v3"))
    expect_s3_class(p, "ggplot")
    expect_true(!is.null(attr(p, "height")))
    expect_true(!is.null(attr(p, "width")))
})

test_that("CorPairsPlot with group_by works", {
    p <- CorPairsPlot(cor_data, columns = c("v1", "v2", "v3"), group_by = "group")
    expect_s3_class(p, "ggplot")
})

test_that("CorPairsPlot with split_by and combine = FALSE returns list", {
    plots <- CorPairsPlot(cor_data, columns = c("v1", "v2", "v3"),
                          split_by = "facet", combine = FALSE)
    expect_true(is.list(plots))
    expect_length(plots, 2)
})

test_that("CorPairsPlot with title does not error", {
    # CorPairsPlot returns a patchwork::wrap_elements object, not a plain ggplot
    p <- CorPairsPlot(cor_data, columns = c("v1", "v2", "v3"), title = "Pairs")
    # Should be some plot-like object with height/width attributes set
    expect_true(!is.null(p))
    expect_true(!is.null(attr(p, "height")))
})
