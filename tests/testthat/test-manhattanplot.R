set.seed(8525)
# Manhattan plot needs chr, pos, pval columns
# Using a small but realistic dataset with multiple chromosomes
n_snps <- 200
mhtn_data <- data.frame(
    chr = sample(paste0("chr", 1:5), n_snps, replace = TRUE),
    pos = sample(1:1e6, n_snps),
    pval = runif(n_snps, min = 1e-10, max = 1),
    label = paste0("SNP", seq_len(n_snps)),
    group = factor(sample(c("A", "B"), n_snps, replace = TRUE))
)

test_that("ManhattanPlot basic usage works", {
    p <- ManhattanPlot(mhtn_data, chr_by = "chr", pos_by = "pos", pval_by = "pval")
    expect_s3_class(p, "ggplot")
    expect_true(!is.null(attr(p, "height")))
    expect_true(!is.null(attr(p, "width")))
})

test_that("ManhattanPlot with title and subtitle works", {
    p <- ManhattanPlot(mhtn_data, chr_by = "chr", pos_by = "pos", pval_by = "pval",
                       title = "Manhattan Title", subtitle = "Subtitle")
    expect_s3_class(p, "ggplot")
    expect_equal(p$labels$title, "Manhattan Title")
    expect_equal(p$labels$subtitle, "Subtitle")
})

test_that("ManhattanPlot with split_by returns patchwork", {
    p <- ManhattanPlot(mhtn_data, chr_by = "chr", pos_by = "pos", pval_by = "pval",
                       split_by = "group", combine = TRUE)
    expect_true(inherits(p, "patchwork") || inherits(p, "gg"))
})

test_that("ManhattanPlot with split_by and combine = FALSE returns list", {
    plots <- ManhattanPlot(mhtn_data, chr_by = "chr", pos_by = "pos", pval_by = "pval",
                           split_by = "group", combine = FALSE)
    expect_true(is.list(plots))
    expect_length(plots, 2)
    expect_s3_class(plots[[1]], "ggplot")
})

test_that("ManhattanPlot with specific chromosomes works", {
    p <- ManhattanPlot(mhtn_data, chr_by = "chr", pos_by = "pos", pval_by = "pval",
                       chromosomes = c("chr1", "chr2", "chr3"))
    expect_s3_class(p, "ggplot")
})

test_that("ManhattanPlot with label_by works", {
    p <- ManhattanPlot(mhtn_data, chr_by = "chr", pos_by = "pos", pval_by = "pval",
                       label_by = "label")
    expect_s3_class(p, "ggplot")
})

test_that("ManhattanPlot with highlight works", {
    highlights <- mhtn_data$pval < 1e-5
    if (any(highlights)) {
        p <- ManhattanPlot(mhtn_data, chr_by = "chr", pos_by = "pos", pval_by = "pval",
                           highlight = which(highlights))
        expect_s3_class(p, "ggplot")
    }
})

test_that("ManhattanPlot with preserve_position = FALSE works", {
    p <- ManhattanPlot(mhtn_data, chr_by = "chr", pos_by = "pos", pval_by = "pval",
                       preserve_position = FALSE)
    expect_s3_class(p, "ggplot")
})
