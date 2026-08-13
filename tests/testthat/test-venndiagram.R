set.seed(8525)
# VennDiagram accepts list input or data.frame (long/wide)
venn_list <- list(
    SetA = c("gene1", "gene2", "gene3", "gene4", "gene5"),
    SetB = c("gene3", "gene4", "gene5", "gene6", "gene7"),
    SetC = c("gene2", "gene4", "gene6", "gene8", "gene9")
)

# Long-format data frame
venn_long <- data.frame(
    id    = c("gene1", "gene2", "gene3", "gene4", "gene5",
               "gene3", "gene4", "gene5", "gene6", "gene7"),
    group = c(rep("SetA", 5), rep("SetB", 5))
)

test_that("VennDiagram with list input works", {
    skip_if_not_installed("ggVennDiagram")
    p <- VennDiagram(venn_list)
    expect_s3_class(p, "ggplot")
    expect_true(!is.null(attr(p, "height")))
    expect_true(!is.null(attr(p, "width")))
})

test_that("VennDiagram with title works", {
    skip_if_not_installed("ggVennDiagram")
    p <- VennDiagram(venn_list, title = "Venn Title")
    expect_s3_class(p, "ggplot")
    expect_equal(p$labels$title, "Venn Title")
})

test_that("VennDiagram with long data frame input works", {
    skip_if_not_installed("ggVennDiagram")
    p <- VennDiagram(venn_long, in_form = "long", id_by = "id", group_by = "group")
    expect_s3_class(p, "ggplot")
})

test_that("VennDiagram with fill_mode = 'count' works", {
    skip_if_not_installed("ggVennDiagram")
    p <- VennDiagram(venn_list, fill_mode = "count")
    expect_s3_class(p, "ggplot")
})

test_that("VennDiagram with fill_mode = 'set' works", {
    skip_if_not_installed("ggVennDiagram")
    p <- VennDiagram(venn_list, fill_mode = "set")
    expect_s3_class(p, "ggplot")
})

test_that("VennDiagram with label works", {
    skip_if_not_installed("ggVennDiagram")
    p <- VennDiagram(venn_list, label = "count")
    expect_s3_class(p, "ggplot")
})

test_that("VennDiagram with split_by returns patchwork", {
    skip_if_not_installed("ggVennDiagram")
    # Two separate Venn diagrams using split_by
    venn_split_data <- data.frame(
        id    = c("g1", "g2", "g3", "g4", "g2", "g3", "g5", "g6",
                  "g1", "g2", "g3", "g7", "g2", "g3", "g8", "g9"),
        group = c(rep("SetA", 4), rep("SetB", 4), rep("SetA", 4), rep("SetB", 4)),
        split = factor(rep(c("S1", "S2"), each = 8))
    )
    p <- VennDiagram(venn_split_data, in_form = "long", id_by = "id",
                     group_by = "group", split_by = "split", combine = TRUE)
    expect_true(inherits(p, "patchwork") || inherits(p, "gg"))
})

test_that("VennDiagram with multiple split_by columns returns combined plot", {
    skip_if_not_installed("ggVennDiagram")
    data2 <- data.frame(
        id    = c("g1", "g2", "g3", "g4", "g2", "g3", "g5", "g6",
                  "g1", "g2", "g3", "g7", "g2", "g3", "g8", "g9"),
        group = c(rep("SetA", 4), rep("SetB", 4), rep("SetA", 4), rep("SetB", 4)),
        split = factor(rep(c("S1", "S2"), each = 8))
    )
    data2$split2 <- factor(rep(c("s1", "s2"), 8))
    p <- suppressMessages(VennDiagram(data2, in_form = "long", id_by = "id",
                                      group_by = "group",
                                      split_by = c("split", "split2"), combine = TRUE))
    expect_true(inherits(p, "patchwork") || inherits(p, "gg"))
})

test_that("VennDiagram with multiple split_by columns returns a list with one plot per combination", {
    skip_if_not_installed("ggVennDiagram")
    data2 <- data.frame(
        id    = c("g1", "g2", "g3", "g4", "g2", "g3", "g5", "g6",
                  "g1", "g2", "g3", "g7", "g2", "g3", "g8", "g9"),
        group = c(rep("SetA", 4), rep("SetB", 4), rep("SetA", 4), rep("SetB", 4)),
        split = factor(rep(c("S1", "S2"), each = 8))
    )
    data2$split2 <- factor(rep(c("s1", "s2"), 8))
    plots <- suppressMessages(VennDiagram(data2, in_form = "long", id_by = "id",
                                          group_by = "group",
                                          split_by = c("split", "split2"), combine = FALSE))
    expect_true(is.list(plots))
    expect_length(plots, 4)
    expect_s3_class(plots[[1]], "ggplot")
})
