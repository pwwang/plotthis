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

test_that("Heatmap with multiple split_by columns returns a list with one plot per combination", {
    skip_if_not_installed("ComplexHeatmap")
    data2 <- long_data
    data2$split2 <- factor(rep(c("s1", "s2"), each = 16))
    plots <- suppressMessages(Heatmap(data2, rows_by = "gene", columns_by = "sample",
                                      values_by = "value", split_by = c("group", "split2"),
                                      combine = FALSE))
    expect_true(is.list(plots))
    expect_length(plots, 4)
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

test_that("Heatmap with rownames split annotation works", {
    skip_if_not_installed("ComplexHeatmap")
    rows_data <- data.frame(
        rows = paste0("Gene", 1:10),
        group = rep(c("grp1", "grp2"), each = 5)
    )
    p <- Heatmap(mat, rows_data = rows_data, rows_split_by = "group",
        row_annotation = list(.row.split = list(
            type = "rownames",
            params = list(sep = ", ", wrap_by = 3)
        )))
    expect_true(!is.null(p))
})

test_that("Heatmap with rownames split annotation works with defaults", {
    skip_if_not_installed("ComplexHeatmap")
    rows_data <- data.frame(
        rows = paste0("Gene", 1:10),
        group = rep(c("grp1", "grp2"), each = 5)
    )
    p <- Heatmap(mat, rows_data = rows_data, rows_split_by = "group",
        row_annotation = list(.row.split = list(type = "rownames")))
    expect_true(!is.null(p))
})

test_that("Heatmap with colnames split annotation works for columns", {
    skip_if_not_installed("ComplexHeatmap")
    columns_data <- data.frame(
        columns = paste0("Sample", 1:5),
        batch = rep(c("b1", "b2"), length.out = 5)
    )
    p <- Heatmap(mat, columns_data = columns_data, columns_split_by = "batch",
        column_annotation = list(.col.split = list(
            type = "colnames",
            params = list(wrap_by = 2)
        )))
    expect_true(!is.null(p))
})

test_that("Heatmap with names/dimnames aliases for split annotations works", {
    skip_if_not_installed("ComplexHeatmap")
    rows_data <- data.frame(
        rows = paste0("Gene", 1:10),
        group = rep(c("grp1", "grp2"), each = 5)
    )
    p <- Heatmap(mat, rows_data = rows_data, rows_split_by = "group",
        row_annotation = list(.row.split = list(type = "names")))
    expect_true(!is.null(p))
    columns_data <- data.frame(
        columns = paste0("Sample", 1:5),
        batch = rep(c("b1", "b2"), length.out = 5)
    )
    p <- Heatmap(mat, columns_data = columns_data, columns_split_by = "batch",
        column_annotation = list(.col.split = list(type = "dimnames")))
    expect_true(!is.null(p))
})

test_that("Heatmap with show_row_names display modes works", {
    skip_if_not_installed("ComplexHeatmap")
    p <- Heatmap(long_data, rows_by = "gene", columns_by = "sample",
        values_by = "value", show_row_names = c("inplace", "legend"))
    expect_true(!is.null(p))
    p <- Heatmap(long_data, rows_by = "gene", columns_by = "sample",
        values_by = "value", show_row_names = "simple")
    expect_true(!is.null(p))
    p <- Heatmap(long_data, rows_by = "gene", columns_by = "sample",
        values_by = "value", show_row_names = "none")
    expect_true(!is.null(p))
})

test_that("Heatmap with anno display mode works with and without by", {
    skip_if_not_installed("ComplexHeatmap")
    # without a grouping column
    p <- Heatmap(mat, show_row_names = "anno")
    expect_true(!is.null(p))
    # with a grouping column
    p <- Heatmap(long_data, rows_by = "gene", columns_by = "sample",
        values_by = "value", show_row_names = "anno")
    expect_true(!is.null(p))
    # columns
    p <- Heatmap(long_data, rows_by = "gene", columns_by = "sample",
        values_by = "value", show_column_names = "annotation")
    expect_true(!is.null(p))
    # flip
    p <- Heatmap(mat, show_row_names = "anno", flip = TRUE)
    expect_true(!is.null(p))
})

test_that("Heatmap with explicit row_annotation overrides display modes", {
    skip_if_not_installed("ComplexHeatmap")
    # "none" mode + explicit .row config: the annotation still shows
    p <- Heatmap(long_data, rows_by = "gene", columns_by = "sample",
        values_by = "value", show_row_names = "none",
        row_annotation = list(.row = list(type = "label")))
    expect_true(!is.null(p))
})

test_that("Heatmap with split title display modes works", {
    skip_if_not_installed("ComplexHeatmap")
    rows_data <- data.frame(
        rows = paste0("Gene", 1:10),
        group = rep(c("grp1", "grp2"), each = 5)
    )
    columns_data <- data.frame(
        columns = paste0("Sample", 1:5),
        batch = rep(c("b1", "b2"), length.out = 5)
    )
    p <- Heatmap(mat, rows_data = rows_data, rows_split_by = "group",
        row_title = "legend")
    expect_true(!is.null(p))
    p <- Heatmap(mat, rows_data = rows_data, rows_split_by = "group",
        row_title = "none")
    expect_true(!is.null(p))
    p <- Heatmap(mat, rows_data = rows_data, rows_split_by = "group",
        row_title = "anno")
    expect_true(!is.null(p))
    p <- Heatmap(mat, rows_data = rows_data, rows_split_by = "group",
        row_title = c("inplace", "legend"))
    expect_true(!is.null(p))
    p <- Heatmap(mat, columns_data = columns_data, columns_split_by = "batch",
        column_title = "anno")
    expect_true(!is.null(p))
})

test_that("Heatmap errors on invalid display modes", {
    skip_if_not_installed("ComplexHeatmap")
    expect_error(Heatmap(mat, show_row_names = c("none", "legend")),
        "'none' cannot be combined")
    expect_error(Heatmap(mat, show_row_names = c("legend", "simple")),
        "'legend' and 'simple' cannot be combined")
    expect_error(Heatmap(mat, show_row_names = "bogus"),
        "Unknown display mode")
})

test_that("Heatmap annotation `name` aliases display name and legend title", {
    skip_if_not_installed("ComplexHeatmap")
    p <- Heatmap(long_data, rows_by = "gene", columns_by = "sample",
        values_by = "value",
        show_row_names = FALSE, show_column_names = FALSE,
        row_annotation = list(.row = list(name = "Gene Group")),
        column_annotation = list(.col = list(name = "Sample Group")),
        combine = FALSE, return_ht = TRUE)
    x <- p[[1]]
    # the legend title is the first text grob in the rendered legend
    legend_title <- function(key) {
        attr(x, "legends")[[key]]@grob$children[[1]]$label
    }
    expect_identical(
        slot(x, "left_annotation")@anno_list[["gene"]]@name_param$label,
        "Gene Group")
    expect_true(slot(x, "left_annotation")@anno_list[["gene"]]@name_param$show)
    expect_identical(
        slot(x, "top_annotation")@anno_list[["sample"]]@name_param$label,
        "Sample Group")
    expect_identical(legend_title("gene"), "Gene Group")
    expect_identical(legend_title("sample"), "Sample Group")
})

test_that("Heatmap annotation `name = FALSE` hides the displayed name", {
    skip_if_not_installed("ComplexHeatmap")
    p <- Heatmap(long_data, rows_by = "gene", columns_by = "sample",
        values_by = "value",
        show_row_names = FALSE, show_column_names = FALSE,
        row_annotation = list(.row = list(name = FALSE)),
        combine = FALSE, return_ht = TRUE)
    x <- p[[1]]
    expect_false(
        slot(x, "left_annotation")@anno_list[["gene"]]@name_param$show)
    expect_identical(
        attr(x, "legends")[["gene"]]@grob$children[[1]]$label, "gene")
})

test_that("Heatmap annotation `name` inherits from .default", {
    skip_if_not_installed("ComplexHeatmap")
    p <- Heatmap(long_data, rows_by = "gene", columns_by = "sample",
        values_by = "value",
        show_row_names = FALSE, show_column_names = FALSE,
        row_annotation = list(.default = list(name = "Group")),
        combine = FALSE, return_ht = TRUE)
    x <- p[[1]]
    expect_identical(
        slot(x, "left_annotation")@anno_list[["gene"]]@name_param$label,
        "Group")
})

test_that("show_row_names = 'none' keeps annotation configured via `name`", {
    skip_if_not_installed("ComplexHeatmap")
    p <- Heatmap(long_data, rows_by = "gene", columns_by = "sample",
        values_by = "value", show_row_names = "none",
        row_annotation = list(.default = list(name = "Group")),
        combine = FALSE, return_ht = TRUE)
    expect_true(!is.null(slot(p[[1]], "left_annotation")))
})

test_that("Heatmap user-defined annotation `name` works", {
    skip_if_not_installed("ComplexHeatmap")
    rows_data <- data.frame(
        rows = paste0("Gene", 1:10),
        group = rep(c("g1", "g2"), each = 5)
    )
    p <- Heatmap(mat, rows_data = rows_data,
        row_annotation = list(Group = list(col = "group",
            name = "My Group", palette = "Spectral", agg = dplyr::first)),
        combine = FALSE, return_ht = TRUE)
    x <- p[[1]]
    expect_identical(
        slot(x, "left_annotation")@anno_list[["Group"]]@name_param$label,
        "My Group")
    expect_identical(
        attr(x, "legends")[["row.Group"]]@grob$children[[1]]$label,
        "My Group")
})

test_that("Deprecated *_name args warn but keep working", {
    skip_if_not_installed("ComplexHeatmap")
    expect_warning(p <- Heatmap(mat, rows_name = "Features"), "deprecated")
    expect_true(!is.null(p))
    expect_warning(Heatmap(mat, columns_split_name = "S"), "deprecated")
    # LinkedHeatmap: the deprecation warning fires even though the
    # column rename itself is broken downstream (pre-existing, also
    # broken in 0.13.2)
    expect_warning(
        expect_error(
            LinkedHeatmap(long_data, values_by = "value", rows_by = "gene",
                columns_by = "sample", left_rows_name = "Genes"),
            "not found"),
        "deprecated")
    # matrix-form internal default must not warn
    ws <- character(0)
    withCallingHandlers(Heatmap(mat),
        warning = function(w) {
            ws <<- c(ws, conditionMessage(w))
            invokeRestart("muffleWarning")
        })
    expect_false(any(grepl("is deprecated", ws)))
})

# Bars cell type: unbalanced fixture, column counts 10 vs 4 (gcd 2 -> k = c(5, 2))
bars_data <- data.frame(
    gene = rep(c("G1", "G2"), each = 7),
    sample = rep(rep(c("S1", "S2"), c(5, 2)), 2),
    value = rnorm(14)
)

test_that("Heatmap bars: default proportional column widths", {
    skip_if_not_installed("ComplexHeatmap")
    p <- Heatmap(bars_data, rows_by = "gene", columns_by = "sample",
        values_by = "value", cell_type = "bars", return_ht = TRUE)
    expect_identical(ncol(p@matrix), 7L)
    expect_identical(unname(lengths(p@column_order_list)), c(5L, 2L))
})

test_that("Heatmap bars: numeric bars_sample gives equal column widths", {
    skip_if_not_installed("ComplexHeatmap")
    p <- Heatmap(bars_data, rows_by = "gene", columns_by = "sample",
        values_by = "value", cell_type = "bars", bars_sample = 2,
        return_ht = TRUE)
    expect_identical(ncol(p@matrix), 2L)
    # uniform counts: no replication, single unsplit column block
    expect_identical(unname(lengths(p@column_order_list)), 2L)
})

test_that("Heatmap bars: fraction bars_sample samples per cell", {
    skip_if_not_installed("ComplexHeatmap")
    # column counts 10 vs 3; without sampling gcd(10, 3) = 1 -> 13 columns
    # (all four cells present: G1-S1 5, G2-S1 5, G1-S2 2, G2-S2 1)
    data3 <- data.frame(
        gene = c(rep("G1", 7), rep("G2", 6)),
        sample = c(rep("S1", 5), rep("S2", 2), rep("S1", 5), "S2"),
        value = rnorm(13)
    )
    p_full <- Heatmap(data3, rows_by = "gene", columns_by = "sample",
        values_by = "value", cell_type = "bars", return_ht = TRUE)
    expect_identical(ncol(p_full@matrix), 13L)
    # ceil(5*0.5)=3 + 3 vs ceil(3*0.5)=2 -> counts 6 vs 2 -> k = c(3, 1)
    p <- Heatmap(data3, rows_by = "gene", columns_by = "sample",
        values_by = "value", cell_type = "bars", bars_sample = 0.5,
        return_ht = TRUE)
    expect_identical(ncol(p@matrix), 4L)
    expect_identical(unname(lengths(p@column_order_list)), c(3L, 1L))
})

test_that("Heatmap bars: flip replicates rows", {
    skip_if_not_installed("ComplexHeatmap")
    p <- Heatmap(bars_data, rows_by = "gene", columns_by = "sample",
        values_by = "value", cell_type = "bars", flip = TRUE,
        return_ht = TRUE)
    expect_identical(nrow(p@matrix), 7L)
    expect_identical(unname(lengths(p@row_order_list)), c(5L, 2L))
})

test_that("Heatmap bars: invalid bars_sample errors", {
    skip_if_not_installed("ComplexHeatmap")
    for (bad in list(0, -1, 1.5, "a")) {
        expect_error(
            Heatmap(bars_data, rows_by = "gene", columns_by = "sample",
                values_by = "value", cell_type = "bars",
                bars_sample = bad, return_ht = TRUE),
            "bars_sample"
        )
    }
})

test_that("Heatmap bars: proportional widths work with user column annotations", {
    skip_if_not_installed("ComplexHeatmap")
    adata <- bars_data
    adata$ann <- rep(c("x", "y"), 7)
    p <- Heatmap(adata, rows_by = "gene", columns_by = "sample",
        values_by = "value", cell_type = "bars",
        column_annotation = "ann", return_ht = TRUE)
    expect_identical(ncol(p@matrix), 7L)
    expect_identical(unname(lengths(p@column_order_list)), c(5L, 2L))
})

test_that("Heatmap bars: smoke render and rows_split_by", {
    skip_if_not_installed("ComplexHeatmap")
    p <- Heatmap(bars_data, rows_by = "gene", columns_by = "sample",
        values_by = "value", cell_type = "bars")
    expect_true(!is.null(p))
    p2 <- Heatmap(bars_data, rows_by = "gene", columns_by = "sample",
        rows_split_by = "gene", values_by = "value", cell_type = "bars",
        return_ht = TRUE)
    expect_identical(ncol(p2@matrix), 7L)
    p3 <- Heatmap(bars_data, rows_by = "gene", columns_by = "sample",
        rows_split_by = "gene", values_by = "value", cell_type = "bars")
    expect_true(!is.null(p3))
})
