set.seed(8525)
# Matrix-form heatmap data
mat <- matrix(rnorm(50), nrow = 10, ncol = 5,
    dimnames = list(paste0("Gene", 1:10), paste0("Sample", 1:5))
)

# Long-form heatmap data
long_data <- data.frame(
    gene = rep(paste0("Gene", 1:8), each = 4),
    sample = rep(paste0("S", 1:4), 8),
    value = rnorm(32),
    group = rep(c("grp1", "grp2"), 16)
)

test_that("Heatmap returns an object for matrix input", {
    skip_if_not_installed("ComplexHeatmap")
    p <- Heatmap(mat)
    expect_true(!is.null(p))
    # Should have width/height attributes
    expect_true(!is.null(attr(p, "height")) || inherits(p, "HeatmapList") || inherits(p, "Heatmap"))
})

test_that("Heatmap works with long-form data", {
    skip_if_not_installed("ComplexHeatmap")
    p <- Heatmap(long_data, rows_by = "gene", columns_by = "sample", values_by = "value")
    expect_true(!is.null(p))
})

test_that("Heatmap with split_by and combine = FALSE returns list", {
    skip_if_not_installed("ComplexHeatmap")
    plots <- Heatmap(long_data, rows_by = "gene", columns_by = "sample",
                     values_by = "value", split_by = "group", combine = FALSE)
    expect_true(is.list(plots))
    expect_length(plots, 2)
})

test_that("Heatmap with flip = TRUE works", {
    skip_if_not_installed("ComplexHeatmap")
    p <- Heatmap(mat, flip = TRUE)
    expect_true(!is.null(p))
})

test_that("Heatmap with cluster_rows = FALSE works", {
    skip_if_not_installed("ComplexHeatmap")
    p <- Heatmap(mat, cluster_rows = FALSE)
    expect_true(!is.null(p))
})

test_that("Heatmap with cluster_columns = FALSE works", {
    skip_if_not_installed("ComplexHeatmap")
    p <- Heatmap(mat, cluster_columns = FALSE)
    expect_true(!is.null(p))
})

test_that("Heatmap with custom palette works", {
    skip_if_not_installed("ComplexHeatmap")
    p <- Heatmap(mat, palette = "Spectral")
    expect_true(!is.null(p))
})
