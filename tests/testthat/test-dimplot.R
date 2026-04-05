data("dim_example", package = "plotthis")

test_that("DimPlot basic usage works", {
    p <- DimPlot(dim_example, group_by = "clusters")
    expect_s3_class(p, "ggplot")
    expect_true(!is.null(attr(p, "height")))
    expect_true(!is.null(attr(p, "width")))
})

test_that("DimPlot with title and subtitle works", {
    p <- DimPlot(dim_example, group_by = "clusters", title = "DimPlot Title", subtitle = "Subtitle")
    expect_s3_class(p, "ggplot")
    expect_equal(p$labels$title, "DimPlot Title")
    expect_equal(p$labels$subtitle, "Subtitle")
})

test_that("DimPlot with label = TRUE works", {
    p <- DimPlot(dim_example, group_by = "clusters", label = TRUE)
    expect_s3_class(p, "ggplot")
})

test_that("DimPlot with split_by returns patchwork when combine = TRUE", {
    p <- DimPlot(dim_example, group_by = "clusters", split_by = "group", combine = TRUE)
    expect_true(inherits(p, "patchwork") || inherits(p, "gg"))
})

test_that("DimPlot with split_by and combine = FALSE returns a list", {
    plots <- DimPlot(dim_example, group_by = "clusters", split_by = "group", combine = FALSE)
    expect_true(is.list(plots))
    expect_s3_class(plots[[1]], "ggplot")
})

test_that("DimPlot with specific dims works", {
    p <- DimPlot(dim_example, dims = c(1, 2), group_by = "clusters")
    expect_s3_class(p, "ggplot")
})

test_that("DimPlot with facet_by works", {
    p <- DimPlot(dim_example, group_by = "clusters", facet_by = "group")
    expect_s3_class(p, "ggplot")
})

test_that("DimPlot with add_density = TRUE works", {
    p <- DimPlot(dim_example, group_by = "clusters", add_density = TRUE)
    expect_s3_class(p, "ggplot")
})

test_that("DimPlot with add_mark = TRUE works", {
    p <- DimPlot(dim_example, group_by = "clusters", add_mark = TRUE)
    expect_s3_class(p, "ggplot")
})

test_that("FeatureDimPlot basic usage works", {
    p <- FeatureDimPlot(dim_example, features = "stochasticbasis_1")
    expect_s3_class(p, "ggplot")
    expect_true(!is.null(attr(p, "height")))
    expect_true(!is.null(attr(p, "width")))
})

test_that("FeatureDimPlot with split_by = TRUE returns patchwork per feature", {
    p <- FeatureDimPlot(dim_example, features = c("stochasticbasis_1", "stochasticbasis_2"),
                        split_by = TRUE, combine = TRUE)
    expect_true(inherits(p, "patchwork") || inherits(p, "gg"))
})

test_that("FeatureDimPlot with split_by = TRUE and combine = FALSE returns list per feature", {
    plots <- FeatureDimPlot(dim_example, features = c("stochasticbasis_1", "stochasticbasis_2"),
                            split_by = TRUE, combine = FALSE)
    expect_true(is.list(plots))
    expect_length(plots, 2)
    expect_s3_class(plots[[1]], "ggplot")
})

test_that("FeatureDimPlot with title works", {
    p <- FeatureDimPlot(dim_example, features = "stochasticbasis_1", title = "Feature Plot")
    expect_s3_class(p, "ggplot")
    expect_equal(p$labels$title, "Feature Plot")
})

test_that("FeatureDimPlot with upper/lower quantile works", {
    p <- FeatureDimPlot(dim_example, features = "stochasticbasis_1",
                        lower_quantile = 0.05, upper_quantile = 0.95)
    expect_s3_class(p, "ggplot")
})
