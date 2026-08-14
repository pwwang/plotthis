set.seed(8525)
# Linked heatmap data: ligand-receptor pairs across sources/targets
pairs_df <- data.frame(
    ligand = rep(paste0("Ligand", 1:4), each = 2),
    receptor = paste0("Receptor", 1:8),
    stringsAsFactors = FALSE
)
sources <- paste0("Source", 1:3)
targets <- paste0("Target", 1:4)
link_data <- merge(
    merge(pairs_df, data.frame(source = sources, stringsAsFactors = FALSE)),
    data.frame(target = targets, stringsAsFactors = FALSE)
)
link_data$split <- sample(c("A", "B"), nrow(link_data), replace = TRUE)
link_data$ligand_expr <- runif(nrow(link_data), 0, 10)
link_data$receptor_expr <- runif(nrow(link_data), 0, 10)
link_data$intensity <- runif(nrow(link_data), 0, 1)

test_that("LinkedHeatmap returns an object", {
    skip_if_not_installed("ComplexHeatmap")
    p <- LinkedHeatmap(
        link_data,
        left_rows_by = "ligand",
        left_columns_by = "source",
        left_values_by = "ligand_expr",
        left_name = "Ligand",
        right_rows_by = "receptor",
        right_columns_by = "target",
        right_values_by = "receptor_expr",
        right_name = "Receptor",
        link_width_by = "intensity"
    )
    expect_true(!is.null(p))
})

test_that("LinkedHeatmap works with name and split title display modes", {
    skip_if_not_installed("ComplexHeatmap")
    p <- LinkedHeatmap(
        link_data,
        left_rows_by = "ligand",
        left_columns_by = "source",
        left_values_by = "ligand_expr",
        left_name = "Ligand",
        right_rows_by = "receptor",
        right_columns_by = "target",
        right_values_by = "receptor_expr",
        right_name = "Receptor",
        link_width_by = "intensity",
        rows_split_by = "split",
        show_row_names = "legend",
        row_title = c("inplace", "legend")
    )
    expect_true(!is.null(p))
})
