data("gsea_example", package = "plotthis")

test_that("GSEAPlot basic usage works", {
    p <- GSEAPlot(gsea_example)
    expect_s3_class(p, "ggplot")
    expect_true(!is.null(attr(p, "height")))
    expect_true(!is.null(attr(p, "width")))
})

test_that("GSEAPlot with title works", {
    gs <- gsea_example$ID[1]
    p <- GSEAPlot(gsea_example, gs = gs, title = "GSEA Title")
    expect_s3_class(p, "ggplot")
    expect_true(!is.null(attr(p, "height")))
})

test_that("GSEAPlot with specific gene sets works", {
    gs_ids <- gsea_example$ID[1:2]
    p <- GSEAPlot(gsea_example, gs = gs_ids, combine = TRUE)
    expect_true(inherits(p, "patchwork") || inherits(p, "gg"))
})

test_that("GSEAPlot with combine = FALSE returns list", {
    gs_ids <- gsea_example$ID[1:3]
    plots <- GSEAPlot(gsea_example, gs = gs_ids, combine = FALSE)
    expect_true(is.list(plots))
    expect_length(plots, 3)
    expect_s3_class(plots[[1]], "ggplot")
})

test_that("GSEAPlot with n_coregenes works", {
    p <- GSEAPlot(gsea_example, gs = gsea_example$ID[1], n_coregenes = 5)
    expect_s3_class(p, "ggplot")
})

test_that("GSEASummaryPlot basic usage works", {
    p <- GSEASummaryPlot(gsea_example)
    expect_s3_class(p, "ggplot")
    expect_true(!is.null(attr(p, "height")))
    expect_true(!is.null(attr(p, "width")))
})

test_that("GSEASummaryPlot with title works", {
    p <- GSEASummaryPlot(gsea_example, title = "GSEA Summary Title")
    expect_s3_class(p, "ggplot")
    expect_equal(p$labels$title, "GSEA Summary Title")
})

test_that("GSEASummaryPlot with top_term works", {
    p <- GSEASummaryPlot(gsea_example, top_term = 5)
    expect_s3_class(p, "ggplot")
})

test_that("GSEASummaryPlot with line_by = 'running_score' works", {
    p <- GSEASummaryPlot(gsea_example, line_by = "running_score")
    expect_s3_class(p, "ggplot")
})

test_that("GSEASummaryPlot with in_form = 'dose' works", {
    p <- GSEASummaryPlot(gsea_example, in_form = "dose")
    expect_s3_class(p, "ggplot")
})
