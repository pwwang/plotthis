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
    p <- DotPlot(data, x = "gene", y = "celltype", fill_by = "expression")
    expect_s3_class(p, "ggplot")
})

test_that("DotPlot works with flip = TRUE", {
    p <- DotPlot(data, x = "gene", y = "celltype", fill_by = "expression", flip = TRUE)
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
    p <- DotPlot(data, x = "gene", y = "celltype", fill_by = "expression",
                 split_by = "group", combine = TRUE)
    expect_true(inherits(p, "gg") || inherits(p, "patchwork"))
})

test_that("DotPlot with split_by and combine = FALSE returns list", {
    plots <- DotPlot(data, x = "gene", y = "celltype", fill_by = "expression",
                     split_by = "group", combine = FALSE)
    expect_true(is.list(plots))
    expect_length(plots, 2)
})

test_that("DotPlot with facet_by works", {
    p <- DotPlot(data, x = "gene", y = "celltype", fill_by = "expression",
                 facet_by = "group")
    expect_s3_class(p, "ggplot")
})

test_that("DotPlot with fill_cutoff works", {
    p <- DotPlot(data, x = "gene", y = "celltype", fill_by = "expression",
                 fill_cutoff = 0.05)
    expect_s3_class(p, "ggplot")
})

test_that("DotPlot width/height attributes are numeric", {
    p <- DotPlot(data, x = "gene", y = "celltype", fill_by = "expression")
    expect_true(is.numeric(attr(p, "width")))
    expect_true(is.numeric(attr(p, "height")))
})
