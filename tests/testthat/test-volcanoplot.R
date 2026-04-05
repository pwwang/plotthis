set.seed(8525)
n <- 200
data <- data.frame(
    logFC = rnorm(n, mean = 0, sd = 2),
    pvalue = runif(n, min = 1e-10, max = 1),
    gene = paste0("gene", seq_len(n)),
    group = factor(rep(c("g1", "g2"), each = n / 2))
)
rownames(data) <- data$gene

test_that("VolcanoPlot returns a ggplot for basic usage", {
    p <- VolcanoPlot(data, x = "logFC", y = "pvalue", nlabel = 0)
    expect_s3_class(p, "ggplot")
    expect_true(!is.null(attr(p, "height")))
    expect_true(!is.null(attr(p, "width")))
})

test_that("VolcanoPlot sets title and labels", {
    p <- VolcanoPlot(data, x = "logFC", y = "pvalue", nlabel = 0,
                     title = "Volcano", xlab = "log2FC", ylab = "-log10(p)")
    expect_equal(p$labels$title, "Volcano")
    expect_equal(p$labels$x, "log2FC")
    expect_equal(p$labels$y, "-log10(p)")
})

test_that("VolcanoPlot with x_cutoff and y_cutoff works", {
    p <- VolcanoPlot(data, x = "logFC", y = "pvalue", nlabel = 0,
                     x_cutoff = 1, y_cutoff = 0.05)
    expect_s3_class(p, "ggplot")
})

test_that("VolcanoPlot with nlabel > 0 works", {
    p <- VolcanoPlot(data, x = "logFC", y = "pvalue", nlabel = 5)
    expect_s3_class(p, "ggplot")
})

test_that("VolcanoPlot with specific labels works", {
    p <- VolcanoPlot(data, x = "logFC", y = "pvalue", nlabel = 0,
                     labels = rownames(data)[1:3])
    expect_s3_class(p, "ggplot")
})

test_that("VolcanoPlot with label_by works", {
    p <- VolcanoPlot(data, x = "logFC", y = "pvalue", nlabel = 3, label_by = "gene")
    expect_s3_class(p, "ggplot")
})

test_that("VolcanoPlot with color_by (discrete) works", {
    p <- VolcanoPlot(data, x = "logFC", y = "pvalue", nlabel = 0, color_by = "group")
    expect_s3_class(p, "ggplot")
})

test_that("VolcanoPlot with split_by returns combined plot", {
    p <- VolcanoPlot(data, x = "logFC", y = "pvalue", nlabel = 0,
                     split_by = "group", combine = TRUE)
    expect_true(inherits(p, "gg") || inherits(p, "patchwork"))
})

test_that("VolcanoPlot with split_by and combine = FALSE returns list", {
    plots <- VolcanoPlot(data, x = "logFC", y = "pvalue", nlabel = 0,
                         split_by = "group", combine = FALSE)
    expect_true(is.list(plots))
    expect_length(plots, 2)
})

test_that("VolcanoPlot with facet_by works", {
    p <- VolcanoPlot(data, x = "logFC", y = "pvalue", nlabel = 0,
                     facet_by = "group")
    expect_s3_class(p, "ggplot")
})

test_that("VolcanoPlot with trim works", {
    p <- VolcanoPlot(data, x = "logFC", y = "pvalue", nlabel = 0,
                     trim = c(0.01, 0.99))
    expect_s3_class(p, "ggplot")
})

test_that("VolcanoPlot errors on invalid trim", {
    expect_error(
        VolcanoPlot(data, x = "logFC", y = "pvalue", trim = c(0, 1, 0.5)),
        "trim"
    )
})

test_that("VolcanoPlot with flip_negatives works", {
    p <- VolcanoPlot(data, x = "logFC", y = "pvalue", nlabel = 0,
                     flip_negatives = TRUE)
    expect_s3_class(p, "ggplot")
})

test_that("VolcanoPlot with highlight works", {
    p <- VolcanoPlot(data, x = "logFC", y = "pvalue", nlabel = 0,
                     highlight = rownames(data)[1:5])
    expect_s3_class(p, "ggplot")
})

test_that("VolcanoPlot width/height attributes are numeric", {
    p <- VolcanoPlot(data, x = "logFC", y = "pvalue", nlabel = 0)
    expect_true(is.numeric(attr(p, "width")))
    expect_true(is.numeric(attr(p, "height")))
})
