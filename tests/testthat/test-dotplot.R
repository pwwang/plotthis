set.seed(8525)
# DotPlot data: x = genes (factor), y = cell types (factor), fill_by = numeric, size_by = numeric
data <- data.frame(
    gene = factor(rep(paste0("Gene", 1:5), each = 4)),
    celltype = factor(rep(paste0("CellType", 1:4), 5)),
    expression = runif(20, 0, 10),
    pct = runif(20, 0, 1),
    group = factor(rep(c("g1", "g2"), 10))
)

test_that("DotPlot returns a ggplot for basic usage", {
    p <- DotPlot(data, x = "gene", y = "celltype", fill_by = "expression", size_by = "pct")
    expect_s3_class(p, "ggplot")
    expect_true(!is.null(attr(p, "height")))
    expect_true(!is.null(attr(p, "width")))
})

test_that("DotPlot works without size_by", {
    p <- suppressWarnings(DotPlot(data, x = "gene", y = "celltype", fill_by = "expression"))
    expect_s3_class(p, "ggplot")
})

test_that("DotPlot works with flip = TRUE", {
    p <- suppressWarnings(DotPlot(data, x = "gene", y = "celltype", fill_by = "expression", flip = TRUE))
    expect_s3_class(p, "ggplot")
})

test_that("DotPlot sets title and labels", {
    p <- DotPlot(data, x = "gene", y = "celltype",
                 title = "Dot Title", xlab = "Genes", ylab = "Cell Types")
    expect_equal(p$labels$title, "Dot Title")
    expect_equal(p$labels$x, "Genes")
    expect_equal(p$labels$y, "Cell Types")
})

test_that("DotPlot with split_by returns patchwork", {
    p <- suppressWarnings(DotPlot(data, x = "gene", y = "celltype", fill_by = "expression",
                 split_by = "group", combine = TRUE))
    expect_true(inherits(p, "gg") || inherits(p, "patchwork"))
})

test_that("DotPlot with split_by and combine = FALSE returns list", {
    plots <- suppressWarnings(DotPlot(data, x = "gene", y = "celltype", fill_by = "expression",
                     split_by = "group", combine = FALSE))
    expect_true(is.list(plots))
    expect_length(plots, 2)
})

test_that("DotPlot with facet_by works", {
    p <- suppressWarnings(DotPlot(data, x = "gene", y = "celltype", fill_by = "expression",
                 facet_by = "group"))
    expect_s3_class(p, "ggplot")
})

test_that("DotPlot with fill_cutoff works", {
    p <- suppressWarnings(DotPlot(data, x = "gene", y = "celltype", fill_by = "expression",
                 fill_cutoff = "< 0.05"))
    expect_s3_class(p, "ggplot")
})

test_that("DotPlot width/height attributes are numeric", {
    p <- suppressWarnings(DotPlot(data, x = "gene", y = "celltype", fill_by = "expression"))
    expect_true(is.numeric(attr(p, "width")))
    expect_true(is.numeric(attr(p, "height")))
})

test_that("DotPlot with lower/upper quantile works", {
    p <- suppressWarnings(DotPlot(data, x = "gene", y = "celltype", fill_by = "expression",
                 lower_quantile = 0.1, upper_quantile = 0.9))
    expect_s3_class(p, "ggplot")
})

test_that("DotPlot with explicit cutoffs works", {
    p <- suppressWarnings(DotPlot(data, x = "gene", y = "celltype", fill_by = "expression",
                 lower_cutoff = 0, upper_cutoff = 5))
    expect_s3_class(p, "ggplot")
})

test_that("DotPlot quantile/cutoff works with lollipop", {
    data_lolli <- data.frame(
        expression = c(0.1, 0.5, 0.8, 1.2, 2.0),
        celltype = paste0("CellType", 1:5)
    )
    p <- suppressWarnings(LollipopPlot(data_lolli, x = "expression", y = "celltype",
        fill_by = "expression",
        lower_quantile = 0, upper_quantile = 0.95))
    expect_s3_class(p, "ggplot")
})
