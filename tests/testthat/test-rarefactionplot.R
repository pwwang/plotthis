set.seed(8525)
# RarefactionPlot requires iNEXT package
# Input: a list of abundance vectors per sample/group
rarefy_data <- list(
    GroupA = c(10, 5, 3, 8, 2, 1, 7, 4, 6, 9, 3, 2, 5, 1, 4),
    GroupB = c(8, 4, 2, 6, 3, 1, 5, 7, 3, 2, 4, 9, 6, 1, 3)
)

test_that("RarefactionPlot basic usage with list input works", {
    skip_if_not_installed("iNEXT")
    p <- RarefactionPlot(rarefy_data)
    expect_s3_class(p, "ggplot")
    expect_true(!is.null(attr(p, "height")))
    expect_true(!is.null(attr(p, "width")))
})

test_that("RarefactionPlot with type = 2 (Shannon diversity) works", {
    skip_if_not_installed("iNEXT")
    p <- RarefactionPlot(rarefy_data, type = 2)
    expect_s3_class(p, "ggplot")
})

test_that("RarefactionPlot with title and subtitle works", {
    skip_if_not_installed("iNEXT")
    p <- RarefactionPlot(rarefy_data, title = "Rarefaction Title", subtitle = "Subtitle")
    expect_s3_class(p, "ggplot")
    expect_equal(p$labels$title, "Rarefaction Title")
    expect_equal(p$labels$subtitle, "Subtitle")
})

test_that("RarefactionPlot with group_by = 'q' and split_by = 'group' works", {
    skip_if_not_installed("iNEXT")
    p <- RarefactionPlot(rarefy_data, group_by = "group", split_by = "q", combine = TRUE)
    expect_true(inherits(p, "patchwork") || inherits(p, "gg"))
})

test_that("RarefactionPlot with combine = FALSE returns list when split_by = 'q'", {
    skip_if_not_installed("iNEXT")
    plots <- RarefactionPlot(rarefy_data, group_by = "group", split_by = "q", combine = FALSE)
    expect_true(is.list(plots))
    expect_s3_class(plots[[1]], "ggplot")
})

test_that("RarefactionPlot with facet_by = 'q' works", {
    skip_if_not_installed("iNEXT")
    p <- RarefactionPlot(rarefy_data, group_by = "group", facet_by = "q")
    expect_s3_class(p, "ggplot")
})

test_that("RarefactionPlot with iNEXT object input works", {
    skip_if_not_installed("iNEXT")
    inext_obj <- iNEXT::iNEXT(rarefy_data)
    p <- RarefactionPlot(inext_obj)
    expect_s3_class(p, "ggplot")
})
