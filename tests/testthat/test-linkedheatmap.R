# ──────────────────────────────────────────────────────────────────────
# LinkedHeatmap tests
# ──────────────────────────────────────────────────────────────────────

test_that("LinkedHeatmap basic functionality", {
    set.seed(8525)
    ligands <- paste0("L", 1:5)
    receptors <- paste0("R", 1:8)
    sources <- paste0("S", 1:3)
    targets <- paste0("T", 1:4)
    data <- expand.grid(
        ligand = ligands, receptor = receptors,
        source = sources, target = targets,
        stringsAsFactors = FALSE
    )
    data$le <- runif(480, 0, 10)
    data$re <- runif(480, 0, 10)
    data$int <- runif(480, 0, 1)

    p <- LinkedHeatmap(data,
        left  = list(rows_by = "ligand", columns_by = "source",
                     values_by = "le", name = "L"),
        right = list(rows_by = "receptor", columns_by = "target",
                     values_by = "re", name = "R"),
        link_by = c("ligand", "receptor"),
        cluster_rows = FALSE)

    expect_s3_class(p, "patchwork")
    expect_true(is.numeric(attr(p, "height")))
    expect_true(is.numeric(attr(p, "width")))
    expect_true(attr(p, "height") > 0)
    expect_true(attr(p, "width") > 0)
})

test_that("LinkedHeatmap with clustering", {
    set.seed(8525)
    ligands <- paste0("L", 1:5)
    receptors <- paste0("R", 1:8)
    sources <- paste0("S", 1:3)
    targets <- paste0("T", 1:4)
    data <- expand.grid(
        ligand = ligands, receptor = receptors,
        source = sources, target = targets,
        stringsAsFactors = FALSE
    )
    data$le <- runif(480, 0, 10)
    data$re <- runif(480, 0, 10)

    p <- LinkedHeatmap(data,
        left  = list(rows_by = "ligand", columns_by = "source",
                     values_by = "le", name = "L"),
        right = list(rows_by = "receptor", columns_by = "target",
                     values_by = "re", name = "R"),
        link_by = c("ligand", "receptor"),
        cluster_rows = TRUE)

    expect_s3_class(p, "patchwork")
})

test_that("LinkedHeatmap with split_by and combine=FALSE", {
    set.seed(8525)
    ligands <- paste0("L", 1:5)
    receptors <- paste0("R", 1:8)
    sources <- paste0("S", 1:3)
    targets <- paste0("T", 1:4)
    data <- expand.grid(
        ligand = ligands, receptor = receptors,
        source = sources, target = targets,
        stringsAsFactors = FALSE
    )
    data$le <- runif(480, 0, 10)
    data$re <- runif(480, 0, 10)
    data$group <- sample(c("A", "B"), 480, replace = TRUE)

    p <- LinkedHeatmap(data,
        left  = list(rows_by = "ligand", columns_by = "source",
                     values_by = "le", name = "L"),
        right = list(rows_by = "receptor", columns_by = "target",
                     values_by = "re", name = "R"),
        link_by = c("ligand", "receptor"),
        split_by = "group", combine = FALSE)

    expect_type(p, "list")
    expect_length(p, 2)
})

test_that("LinkedHeatmap with split_by and combine=TRUE", {
    set.seed(8525)
    ligands <- paste0("L", 1:5)
    receptors <- paste0("R", 1:8)
    sources <- paste0("S", 1:3)
    targets <- paste0("T", 1:4)
    data <- expand.grid(
        ligand = ligands, receptor = receptors,
        source = sources, target = targets,
        stringsAsFactors = FALSE
    )
    data$le <- runif(480, 0, 10)
    data$re <- runif(480, 0, 10)
    data$group <- sample(c("A", "B"), 480, replace = TRUE)

    p <- LinkedHeatmap(data,
        left  = list(rows_by = "ligand", columns_by = "source",
                     values_by = "le", name = "L"),
        right = list(rows_by = "receptor", columns_by = "target",
                     values_by = "re", name = "R"),
        link_by = c("ligand", "receptor"),
        split_by = "group", combine = TRUE)

    expect_s3_class(p, "patchwork")
})

test_that("LinkedHeatmap with link_width_by", {
    set.seed(8525)
    ligands <- paste0("L", 1:5)
    receptors <- paste0("R", 1:8)
    sources <- paste0("S", 1:3)
    targets <- paste0("T", 1:4)
    data <- expand.grid(
        ligand = ligands, receptor = receptors,
        source = sources, target = targets,
        stringsAsFactors = FALSE
    )
    data$le <- runif(480, 0, 10)
    data$re <- runif(480, 0, 10)
    data$int <- runif(480, 0, 1)

    p <- LinkedHeatmap(data,
        left  = list(rows_by = "ligand", columns_by = "source",
                     values_by = "le", name = "L"),
        right = list(rows_by = "receptor", columns_by = "target",
                     values_by = "re", name = "R"),
        link_by = c("ligand", "receptor"),
        link_width_by = "int",
        cluster_rows = FALSE)

    expect_s3_class(p, "patchwork")
})

test_that("LinkedHeatmap debug mode", {
    set.seed(8525)
    ligands <- paste0("L", 1:3)
    receptors <- paste0("R", 1:4)
    sources <- paste0("S", 1:2)
    targets <- paste0("T", 1:2)
    data <- expand.grid(
        ligand = ligands, receptor = receptors,
        source = sources, target = targets,
        stringsAsFactors = FALSE
    )
    data$le <- runif(nrow(data), 0, 10)
    data$re <- runif(nrow(data), 0, 10)

    p <- LinkedHeatmap(data,
        left  = list(rows_by = "ligand", columns_by = "source",
                     values_by = "le", name = "L"),
        right = list(rows_by = "receptor", columns_by = "target",
                     values_by = "re", name = "R"),
        link_by = c("ligand", "receptor"),
        cluster_rows = FALSE, debug = TRUE)

    expect_s3_class(p, "patchwork")
})

test_that("LinkedHeatmap with custom palette", {
    set.seed(8525)
    ligands <- paste0("L", 1:5)
    receptors <- paste0("R", 1:8)
    sources <- paste0("S", 1:3)
    targets <- paste0("T", 1:4)
    data <- expand.grid(
        ligand = ligands, receptor = receptors,
        source = sources, target = targets,
        stringsAsFactors = FALSE
    )
    data$le <- runif(480, 0, 10)
    data$re <- runif(480, 0, 10)

    p <- LinkedHeatmap(data,
        left  = list(rows_by = "ligand", columns_by = "source",
                     values_by = "le", name = "L", palette = "viridis"),
        right = list(rows_by = "receptor", columns_by = "target",
                     values_by = "re", name = "R", palette = "magma"),
        link_by = c("ligand", "receptor"),
        cluster_rows = FALSE)

    expect_s3_class(p, "patchwork")
})

test_that("LinkedHeatmap validates required args", {
    set.seed(8525)
    data <- data.frame(a = 1:3, b = 4:6, c = 7:9, d = 10:12)

    expect_error(
        LinkedHeatmap(data,
            left  = list(rows_by = "a", columns_by = "b",
                         values_by = "c"),
            right = list(rows_by = "a", columns_by = "b",
                         values_by = "d", name = "R"),
            link_by = c("a", "a")),
        "missing"
    )
})

test_that("LinkedHeatmap legend defaults", {
    set.seed(8525)
    ligands <- paste0("L", 1:5)
    receptors <- paste0("R", 1:8)
    sources <- paste0("S", 1:3)
    targets <- paste0("T", 1:4)
    data <- expand.grid(
        ligand = ligands, receptor = receptors,
        source = sources, target = targets,
        stringsAsFactors = FALSE
    )
    data$le <- runif(480, 0, 10)
    data$re <- runif(480, 0, 10)

    # Default: legend should be shown on the right
    p_def <- LinkedHeatmap(data,
        left  = list(rows_by = "ligand", columns_by = "source",
                     values_by = "le", name = "L"),
        right = list(rows_by = "receptor", columns_by = "target",
                     values_by = "re", name = "R"),
        link_by = c("ligand", "receptor"),
        cluster_rows = FALSE)
    expect_s3_class(p_def, "patchwork")
    w_def <- attr(p_def, "width")

    # legend.position = "none": should be narrower
    p_none <- LinkedHeatmap(data,
        left  = list(rows_by = "ligand", columns_by = "source",
                     values_by = "le", name = "L"),
        right = list(rows_by = "receptor", columns_by = "target",
                     values_by = "re", name = "R"),
        link_by = c("ligand", "receptor"),
        cluster_rows = FALSE,
        legend.position = "none")
    expect_s3_class(p_none, "patchwork")
    w_none <- attr(p_none, "width")
    expect_true(w_def > w_none)
})

test_that("LinkedHeatmap legend with split_by", {
    set.seed(8525)
    ligands <- paste0("L", 1:5)
    receptors <- paste0("R", 1:8)
    sources <- paste0("S", 1:3)
    targets <- paste0("T", 1:4)
    data <- expand.grid(
        ligand = ligands, receptor = receptors,
        source = sources, target = targets,
        stringsAsFactors = FALSE
    )
    data$le <- runif(480, 0, 10)
    data$re <- runif(480, 0, 10)
    data$group <- sample(c("A", "B"), 480, replace = TRUE)

    # Per-split legend: one with, one without
    p <- LinkedHeatmap(data,
        left  = list(rows_by = "ligand", columns_by = "source",
                     values_by = "le", name = "L"),
        right = list(rows_by = "receptor", columns_by = "target",
                     values_by = "re", name = "R"),
        link_by = c("ligand", "receptor"),
        split_by = "group", combine = TRUE,
        legend.position = list(A = "none", B = "right"),
        cluster_rows = FALSE)
    expect_s3_class(p, "patchwork")

    # combine = FALSE with per-split legends
    p_list <- LinkedHeatmap(data,
        left  = list(rows_by = "ligand", columns_by = "source",
                     values_by = "le", name = "L"),
        right = list(rows_by = "receptor", columns_by = "target",
                     values_by = "re", name = "R"),
        link_by = c("ligand", "receptor"),
        split_by = "group", combine = FALSE,
        legend.position = list(A = "none", B = "right"),
        cluster_rows = FALSE)
    expect_type(p_list, "list")
    expect_length(p_list, 2)
})

test_that("LinkedHeatmap legend positions: top, bottom, left", {
    set.seed(8525)
    ligands <- paste0("L", 1:5)
    receptors <- paste0("R", 1:8)
    sources <- paste0("S", 1:3)
    targets <- paste0("T", 1:4)
    data <- expand.grid(
        ligand = ligands, receptor = receptors,
        source = sources, target = targets,
        stringsAsFactors = FALSE
    )
    data$le <- runif(480, 0, 10)
    data$re <- runif(480, 0, 10)

    # legend.position = "top": taller than "none"
    p_top <- LinkedHeatmap(data,
        left  = list(rows_by = "ligand", columns_by = "source",
                     values_by = "le", name = "L"),
        right = list(rows_by = "receptor", columns_by = "target",
                     values_by = "re", name = "R"),
        link_by = c("ligand", "receptor"),
        cluster_rows = FALSE,
        legend.position = "top")
    expect_s3_class(p_top, "patchwork")

    p_none <- LinkedHeatmap(data,
        left  = list(rows_by = "ligand", columns_by = "source",
                     values_by = "le", name = "L"),
        right = list(rows_by = "receptor", columns_by = "target",
                     values_by = "re", name = "R"),
        link_by = c("ligand", "receptor"),
        cluster_rows = FALSE,
        legend.position = "none")
    expect_true(attr(p_top, "height") > attr(p_none, "height"))

    # legend.position = "bottom"
    p_bottom <- LinkedHeatmap(data,
        left  = list(rows_by = "ligand", columns_by = "source",
                     values_by = "le", name = "L"),
        right = list(rows_by = "receptor", columns_by = "target",
                     values_by = "re", name = "R"),
        link_by = c("ligand", "receptor"),
        cluster_rows = FALSE,
        legend.position = "bottom")
    expect_s3_class(p_bottom, "patchwork")
    expect_true(attr(p_bottom, "height") > attr(p_none, "height"))

    # legend.position = "left": wider than "none"
    p_left <- LinkedHeatmap(data,
        left  = list(rows_by = "ligand", columns_by = "source",
                     values_by = "le", name = "L"),
        right = list(rows_by = "receptor", columns_by = "target",
                     values_by = "re", name = "R"),
        link_by = c("ligand", "receptor"),
        cluster_rows = FALSE,
        legend.position = "left")
    expect_s3_class(p_left, "patchwork")
    expect_true(attr(p_left, "width") > attr(p_none, "width"))
})
