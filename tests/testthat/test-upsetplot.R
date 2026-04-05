set.seed(8525)
# UpsetPlot accepts list input or data.frame (long/wide)
upset_list <- list(
    SetA = c("gene1", "gene2", "gene3", "gene4", "gene5", "gene6"),
    SetB = c("gene3", "gene4", "gene5", "gene7", "gene8", "gene9"),
    SetC = c("gene2", "gene4", "gene6", "gene8", "gene10", "gene11"),
    SetD = c("gene1", "gene5", "gene7", "gene9", "gene11", "gene12")
)

# Long-format for upset
upset_long <- data.frame(
    id    = c("g1", "g2", "g3", "g4", "g5", "g3", "g4", "g5", "g6", "g7"),
    group = c(rep("SetA", 5), rep("SetB", 5))
)

test_that("UpsetPlot with list input works", {
    skip_if_not_installed("ggupset")
    p <- UpsetPlot(upset_list)
    expect_s3_class(p, "ggplot")
    expect_true(!is.null(attr(p, "height")))
    expect_true(!is.null(attr(p, "width")))
})

test_that("UpsetPlot with title works", {
    skip_if_not_installed("ggupset")
    p <- UpsetPlot(upset_list, title = "Upset Title")
    expect_s3_class(p, "ggplot")
    expect_equal(p$labels$title, "Upset Title")
})

test_that("UpsetPlot with long data frame input works", {
    skip_if_not_installed("ggupset")
    p <- UpsetPlot(upset_long, in_form = "long", id_by = "id", group_by = "group")
    expect_s3_class(p, "ggplot")
})

test_that("UpsetPlot with label works", {
    skip_if_not_installed("ggupset")
    p <- UpsetPlot(upset_list, label = "count")
    expect_s3_class(p, "ggplot")
})

test_that("UpsetPlot with split_by returns patchwork", {
    skip_if_not_installed("ggupset")
    upset_split <- data.frame(
        id    = c("g1","g2","g3","g4","g5","g3","g4","g5","g6","g7",
                  "g1","g2","g8","g9","g3","g8","g9","g3","g10","g11"),
        group = c(rep("SetA", 5), rep("SetB", 5), rep("SetA", 5), rep("SetB", 5)),
        split = factor(rep(c("S1", "S2"), each = 10))
    )
    p <- UpsetPlot(upset_split, in_form = "long", id_by = "id",
                   group_by = "group", split_by = "split", combine = TRUE)
    expect_true(inherits(p, "patchwork") || inherits(p, "gg"))
})

test_that("UpsetPlot with combine = FALSE returns list", {
    skip_if_not_installed("ggupset")
    upset_split <- data.frame(
        id    = c("g1","g2","g3","g4","g5","g3","g4","g5","g6","g7",
                  "g1","g2","g8","g9","g3","g8","g9","g3","g10","g11"),
        group = c(rep("SetA", 5), rep("SetB", 5), rep("SetA", 5), rep("SetB", 5)),
        split = factor(rep(c("S1", "S2"), each = 10))
    )
    plots <- UpsetPlot(upset_split, in_form = "long", id_by = "id",
                       group_by = "group", split_by = "split", combine = FALSE)
    expect_true(is.list(plots))
    expect_length(plots, 2)
    expect_s3_class(plots[[1]], "ggplot")
})
