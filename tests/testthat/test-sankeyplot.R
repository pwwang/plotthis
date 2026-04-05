set.seed(8525)
# SankeyPlot / AlluvialPlot test data
# Wide format: each row is an entity, columns are levels in the flow
sankey_wide <- data.frame(
    Year1 = factor(sample(c("A", "B", "C"), 60, replace = TRUE)),
    Year2 = factor(sample(c("X", "Y", "Z"), 60, replace = TRUE)),
    Year3 = factor(sample(c("P", "Q"), 60, replace = TRUE)),
    group = factor(rep(c("G1", "G2"), each = 30))
)

# Long/alluvia format: stratum column defines nodes, alluvium defines flow paths
n <- 120
sankey_long <- data.frame(
    time   = factor(rep(c("T1", "T2", "T3"), each = n / 3)),
    status = factor(c(
        sample(c("A", "B"), n / 3, replace = TRUE),
        sample(c("A", "B", "C"), n / 3, replace = TRUE),
        sample(c("B", "C"), n / 3, replace = TRUE)
    )),
    id = rep(seq_len(n / 3), 3)
)

test_that("SankeyPlot with wide-format data works", {
    skip_if_not_installed("ggalluvial")
    p <- SankeyPlot(sankey_wide, x = c("Year1", "Year2", "Year3"),
                    in_form = "wide", links_fill_by = "Year1")
    expect_s3_class(p, "ggplot")
    expect_true(!is.null(attr(p, "height")))
    expect_true(!is.null(attr(p, "width")))
})

test_that("SankeyPlot with title and subtitle works", {
    skip_if_not_installed("ggalluvial")
    p <- SankeyPlot(sankey_wide, x = c("Year1", "Year2", "Year3"),
                    in_form = "wide", links_fill_by = "Year1",
                    title = "Sankey Title", subtitle = "Subtitle")
    expect_s3_class(p, "ggplot")
    expect_equal(p$labels$title, "Sankey Title")
    expect_equal(p$labels$subtitle, "Subtitle")
})

test_that("SankeyPlot with split_by returns patchwork", {
    skip_if_not_installed("ggalluvial")
    p <- SankeyPlot(sankey_wide, x = c("Year1", "Year2", "Year3"),
                    in_form = "wide", links_fill_by = "Year1",
                    split_by = "group", combine = TRUE)
    expect_true(inherits(p, "patchwork") || inherits(p, "gg"))
})

test_that("SankeyPlot with split_by combine = FALSE returns list", {
    skip_if_not_installed("ggalluvial")
    plots <- SankeyPlot(sankey_wide, x = c("Year1", "Year2", "Year3"),
                        in_form = "wide", links_fill_by = "Year1",
                        split_by = "group", combine = FALSE)
    expect_true(is.list(plots))
    expect_length(plots, 2)
    expect_s3_class(plots[[1]], "ggplot")
})

test_that("SankeyPlot with flip = TRUE works", {
    skip_if_not_installed("ggalluvial")
    p <- SankeyPlot(sankey_wide, x = c("Year1", "Year2", "Year3"),
                    in_form = "wide", links_fill_by = "Year1", flip = TRUE)
    expect_s3_class(p, "ggplot")
})

test_that("SankeyPlot in long/lodes format works", {
    skip_if_not_installed("ggalluvial")
    p <- SankeyPlot(sankey_long, in_form = "long",
                    x = "time", stratum = "status", alluvium = "id")
    expect_s3_class(p, "ggplot")
})

test_that("AlluvialPlot is an alias for SankeyPlot", {
    skip_if_not_installed("ggalluvial")
    p <- AlluvialPlot(sankey_wide, x = c("Year1", "Year2", "Year3"),
                      in_form = "wide", links_fill_by = "Year1")
    expect_s3_class(p, "ggplot")
})

test_that("SankeyPlot with links_fill_by works", {
    skip_if_not_installed("ggalluvial")
    p <- SankeyPlot(sankey_wide, x = c("Year1", "Year2", "Year3"),
                    in_form = "wide", links_fill_by = "Year1")
    expect_s3_class(p, "ggplot")
})
