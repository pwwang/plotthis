#' Join the meta data to the main data frame for heatmap
#'
#' @param data A data frame containing the main data for the heatmap.
#' @param meta_data A data frame containing the meta data to be joined.
#' @param by A character string specifying the column name in `meta_data` to join on.
#' Either `rows_by` or `columns_by` should be specified in `data`.
#' @param cr_split_by A character string specifying the column name in `data` to join on.
#' Either `rows_split_by` or `columns_split_by` should be specified in `data`.
#' @param split_by A character string specifying the column name in `data` to join on.
#' Used to split the data into multiple heatmaps.
#' @param which A character string specifying whether to join on rows or columns.
#' Can be either `"row"` or `"column"`.
#' @importFrom dplyr left_join
#' @return A data frame with the meta data joined to the main data.
#' @keywords internal
join_heatmap_meta <- function(data, meta_data, by, cr_split_by, split_by, which) {
    if (!by %in% colnames(meta_data)) {
        stop(sprintf("[Heatmap] '%ss_by' (%s) must be a column in '%ss_data'.", which, by, which))
    }

    join_by <- by
    if (!is.null(cr_split_by) && cr_split_by %in% colnames(data) &&
        cr_split_by %in% colnames(meta_data)) {
        # if split_by is in both data and meta_data, we join by both
        join_by <- c(by, cr_split_by)
    }
    if (!is.null(split_by) && split_by %in% colnames(data) &&
        split_by %in% colnames(meta_data)) {
        # if split_by is in both data and meta_data, we join by both
        join_by <- c(join_by, split_by)
    }

    out <- dplyr::left_join(data, meta_data, by = join_by, suffix = c("", paste0(".", which)))
    # factor lost when joining, so we need to restore the levels
    out[[by]] <- factor(out[[by]], levels = levels(data[[by]]))
    if (!is.null(cr_split_by) && cr_split_by %in% join_by) {
        if (is.factor(data[[cr_split_by]])) {
            out[[cr_split_by]] <- factor(out[[cr_split_by]], levels = levels(data[[cr_split_by]]))
        } else if (is.factor(meta_data[[cr_split_by]])) {
            # if the split_by is a factor in meta_data, we need to restore the levels
            out[[cr_split_by]] <- factor(out[[cr_split_by]], levels = levels(meta_data[[cr_split_by]]))
        } else {
            # if the split_by is not a factor, we convert it to a factor
            out[[cr_split_by]] <- factor(out[[cr_split_by]], levels = unique(out[[cr_split_by]]))
        }
    }
    if (!is.null(split_by) && split_by %in% join_by) {
        if (is.factor(data[[split_by]])) {
            out[[split_by]] <- factor(out[[split_by]], levels = levels(data[[split_by]]))
        } else if (is.factor(meta_data[[split_by]])) {
            # if the split_by is a factor in meta_data, we need to restore the levels
            out[[split_by]] <- factor(out[[split_by]], levels = levels(meta_data[[split_by]]))
        } else {
            # if the split_by is not a factor, we convert it to a factor
            out[[split_by]] <- factor(out[[split_by]], levels = unique(out[[split_by]]))
        }
    }

    out
}

#' Process/normalize data passed to [Heatmap()]
#'
#' This function is used to process the data passed to [Heatmap()].
#' @param data A data frame or matrix containing the data to be plotted.
#' Based on the `in_form`, the data can have the following formats:
#' * `matrix`: A matrix with rows and columns directly representing the heatmap.
#' * `long`: A data frame in long format with columns for values, rows, and columns.
#' * `wide-rows`: A data frame in wide format with columns for heatmap rows and values,
#'    and a single column for heatmap columns.
#' * `wide-columns`: A data frame in wide format with columns for heatmap columns and values,
#'    and a single column for heatmap rows.
#' * `auto`: Automatically inferred from the data format.
#'    When `data` is a matrix, `in_form` is set to `"matrix"`. When `columns_by` has more than one column,
#'    `in_form` is set to `"wide-columns"`. When `rows_by` has more than one column,
#'    `in_form` is set to `"wide-rows"`. Otherwise, it is set to `"long"`.
#' @param in_form The format of the data. Can be one of `"matrix"`, `"long"`, `"wide-rows"`, `"wide-columns"`, or `"auto"`.
#' Defaults to `"auto"`.
#' @param values_by A character of column name in `data` that contains the values to be plotted.
#' This is required when `in_form` is `"long"`. For other formats, the values are pivoted into a column named by `values_by`.
#' @param name A character string to name the heatmap (will be used to rename `values_by`).
#' @param split_by A character of column name in `data` that contains the split information to split into multiple heatmaps.
#' This is used to create a list of heatmaps, one for each level of the split.
#' Defaults to `NULL`, meaning no split.
#' @param split_by_sep A character string to concat multiple columns in `split_by`.
#' @param rows_by A vector of column names in `data` that contains the row information.
#' This is used to create the rows of the heatmap.
#' When `in_form` is `"long"` or `"wide-columns"`, this is requied, and multiple columns can be specified,
#' which will be concatenated by `rows_by_sep` into a single column.
#' @param rows_by_sep A character string to concat multiple columns in `rows_by`.
#' @param rows_name A character string to rename the column created by `rows_by`, which will be reflected in the name of the annotation or legend.
#' @param rows_split_by A character of column name in `data` that contains the split information for rows.
#' @param rows_split_by_sep A character string to concat multiple columns in `rows_split_by`.
#' @param rows_split_name A character string to rename the column created by `rows_split_by`, which will be reflected in the name of the annotation or legend.
#' @param columns_by A vector of column names in `data` that contains the column information.
#' This is used to create the columns of the heatmap.
#' When `in_form` is `"long"` or `"wide-rows"`, this is required, and multiple columns can be specified,
#' which will be concatenated by `columns_by_sep` into a single column.
#' @param columns_by_sep A character string to concat multiple columns in `columns_by`.
#' @param columns_name A character string to rename the column created by `columns_by`, which will be reflected in the name of the annotation or legend.
#' @param columns_split_by A character of column name in `data` that contains the split information for columns.
#' @param columns_split_by_sep A character string to concat multiple columns in `columns_split_by`.
#' @param columns_split_name A character string to rename the column created by `columns_split_by`, which will be reflected in the name of the annotation or legend.
#' @param rows_orderby A expression (in character) to specify how to order rows. It will be evaluated in the context of the data frame used for rows (after grouping by rows_split_by and rows_by). The expression should return a vector of the same length as the number of rows in the data frame. The default is NULL, which means no specific ordering.
#' Can't be used with cluster_rows = TRUE.
#' This is applied before renaming rows_by to rows_name.
#' @param columns_orderby A expression (in character) to specify how to order columns. It will be evaluated in the context of the data frame used for columns (after grouping by columns
#' split_by and columns_by). The expression should return a vector of the same length as the number of rows in the data frame. The default is NULL, which means no specific ordering.
#' Can't be used with cluster_columns = TRUE.
#' This is applied before renaming columns_by to columns_name.
#' @param pie_group_by A character of column name in `data` that contains the group information for pie charts.
#' This is used to create pie charts in the heatmap when `cell_type` is `"pie"`.
#' @param pie_group_by_sep A character string to concat multiple columns in `pie_group_by`.
#' @param pie_name A character string to rename the column created by `pie_group_by`, which will be reflected in the name of the annotation or legend.
#' @param rows_data A data frame containing additional data for rows, which can be used to add annotations to the heatmap.
#' It will be joined to the main data by `rows_by` and `split_by` if `split_by` exists in `rows_data`.
#' This is useful for adding additional information to the rows of the heatmap.
#' @param columns_data A data frame containing additional data for columns, which can be used to add annotations to the heatmap.
#' It will be joined to the main data by `columns_by` and `split_by` if `split_by` exists in `columns_data`.
#' This is useful for adding additional information to the columns of the heatmap.
#' @param keep_na Whether we should keep NA groups in rows, columns and split_by variables. Default is FALSE.
#' FALSE to remove NA groups; TRUE to keep NA groups.
#' A vector of column names can also be provided to specify which columns to keep NA groups.
#' Note that the record will be removed if any of the grouping columns has NA and is not specified to keep NA.
#' @return A list containing the processed data and metadata:
#' * `data`: A list of data frames, one for each level of `split_by`. If no `split_by` is provided, the name will be `"..."`.
#'    Each data frame is in the long format.
#' * `values_by`: The name of the column containing the values to be plotted.
#' * `rows_by`: The name of the column containing the row information.
#' * `rows_split_by`: The name of the column containing the row split information.
#' * `columns_by`: The name of the column containing the column information.
#' * `columns_split_by`: The name of the column containing the column split information.
#' * `pie_group_by`: The name of the column containing the pie group information.
#' @importFrom rlang sym syms %||% parse_expr
#' @importFrom dplyr arrange group_by summarise pull all_of
#' @keywords internal
process_heatmap_data <- function(
    data, in_form, values_by, name,
    split_by, split_by_sep, rows_orderby, columns_orderby,
    rows_by, rows_by_sep, rows_name,
    rows_split_by, rows_split_by_sep, rows_split_name,
    columns_by, columns_by_sep, columns_name,
    columns_split_by, columns_split_by_sep, columns_split_name,
    pie_group_by, pie_group_by_sep, pie_name,
    rows_data, columns_data, keep_na, keep_empty
) {
    if (identical(rows_by, columns_by) && !is.null(rows_by)) {
        stop("[Heatmap] 'rows_by' and 'columns_by' can not be the same.")
    }
    stopifnot("[Heatmap] no data is presented (nrow == 0)." = nrow(data) > 0)
    # Infer in_form
    if (in_form == "auto") {
        if (is.matrix(data)) {
            in_form <- "matrix"
        } else if (length(rows_by) > 1) {
            in_form <- "wide-rows"
        } else if (length(columns_by) > 1) {
            in_form <- "wide-columns"
        } else {
            in_form <- "long"
        }
    }


    if (in_form == "matrix") {
        stopifnot("[Heatmap] 'split_by' is not supported when 'in_form = \"matrix\"'." = is.null(split_by))
        stopifnot("[Heatmap] 'rows_by' is not supported when 'in_form = \"matrix\"'." = is.null(rows_by))
        stopifnot("[Heatmap] 'columns_by' is not supported when 'in_form = \"matrix\"'." = is.null(columns_by))
        stopifnot("[Heatmap] 'pie_group_by' is not supported when 'in_form = \"matrix\"'." = is.null(pie_group_by))
        stopifnot("[Heatmap] 'keep_na' is not supported when 'in_form = \"matrix\"'." = isFALSE(keep_na))

        rows_name <- rows_name %||% "rows"
        data <- as.data.frame(data)
        columns_by <- colnames(data)
        data[rows_name] <- rownames(data)
        rows_by <- rows_name

        in_form <- "wide-columns"
    }

    if (identical(rows_name %||% rows_by, columns_name %||% columns_by)) {
        if (!is.null(columns_name)) {
            # consider flip and names_side?
            columns_name <- paste0(columns_name, " ")
        } else {
            rows_name <- paste0(" ", rows_name)
        }
    }

    # pie_group_by should be always in the main data
    pie_group_by <- check_columns(
        data, pie_group_by,
        force_factor = TRUE, allow_multi = TRUE, concat_multi = TRUE, concat_sep = pie_group_by_sep
    )

    split_by <- check_columns(
        data, split_by,
        force_factor = TRUE, allow_multi = TRUE, concat_multi = TRUE, concat_sep = split_by_sep
    )

    apply_orderby <- function(df, orderby, by, sby, sby_levels) {
        if (!is.null(orderby)) {
            by_levels <- df %>%
                group_by(!!sym(by)) %>%
                summarise(.orderby = !!parse_expr(orderby)) %>%
                arrange(!!sym(".orderby")) %>%
                pull(!!sym(by)) %>%
                as.character() %>%
                unique()
            df[[by]] <- factor(df[[by]], levels = by_levels)

            if (!is.null(sby) && is.null(sby_levels)) {
                sby_levels <- df %>%
                    group_by(!!sym(sby)) %>%
                    summarise(.orderby = !!parse_expr(orderby)) %>%
                    arrange(!!sym(".orderby")) %>%
                    pull(!!sym(sby)) %>%
                    as.character() %>%
                    unique()
                df[[sby]] <- factor(df[[sby]], levels = sby_levels)
            }
        }
        df
    }

    if (in_form == "long") {
        # values_by
        values_by <- check_columns(data, values_by)
        stopifnot("[Heatmap] 'values_by' must be specified when 'in_form = \"long\"'." = !is.null(values_by))

        # rows_by/rows_split_by
        rows_by <- check_columns(
            data, rows_by,
            force_factor = TRUE, allow_multi = TRUE, concat_multi = TRUE, concat_sep = rows_by_sep
        )
        stopifnot("[Heatmap] 'rows_by' must be specified when 'in_form = \"long\"'." = !is.null(rows_by))

        # columns_by/columns_split_by
        columns_by <- check_columns(
            data, columns_by,
            force_factor = TRUE, allow_multi = TRUE, concat_multi = TRUE, concat_sep = columns_by_sep
        )
        stopifnot("[Heatmap] 'columns_by' must be specified when 'in_form = \"long\"'." = !is.null(columns_by))

        # join rows_data/columns_data
        if (!is.null(rows_data)) {
            data <- join_heatmap_meta(
                data, rows_data, by = rows_by, cr_split_by = rows_split_by,
                split_by = split_by, which = "row"
            )
        }
        rows_split_levels <- if (length(rows_split_by) == 1 && is.factor(data[[rows_split_by]])) {
            levels(data[[rows_split_by]])
        } else {
            NULL
        }
        rows_split_by <- check_columns(
            data, rows_split_by,
            force_factor = TRUE, allow_multi = TRUE, concat_multi = TRUE, concat_sep = rows_split_by_sep
        )

        if (!is.null(columns_data)) {
            data <- join_heatmap_meta(
                data, columns_data, by = columns_by, cr_split_by = columns_split_by,
                split_by = split_by, which = "column"
            )
        }
        columns_split_leves <- if (length(columns_split_by) == 1 && is.factor(data[[columns_split_by]])) {
            levels(data[[columns_split_by]])
        } else {
            NULL
        }
        columns_split_by <- check_columns(
            data, columns_split_by,
            force_factor = TRUE, allow_multi = TRUE, concat_multi = TRUE, concat_sep = columns_split_by_sep
        )

        data <- apply_orderby(data, rows_orderby, rows_by, rows_split_by, rows_split_levels)
        data <- apply_orderby(data, columns_orderby, columns_by, columns_split_by, columns_split_leves)

        # rename
        if (!is.null(rows_name)) {
            if (identical(rows_name, "")) {
                rows_name <- " "
            }
            data <- dplyr::rename(data, !!sym(rows_name) := rows_by)
            rows_by <- rows_name
        }
        if (!is.null(columns_name)) {
            if (identical(columns_name, "")) {
                columns_name <- " "
            }
            data <- dplyr::rename(data, !!sym(columns_name) := columns_by)
            columns_by <- columns_name
        }
    } else if (in_form == "wide-rows") {
        # columns_split_by columns_by pie_group_by rows1 rows2 ...
        # csb1             cb1        pgb1         0.1   0.2   ...
        # csb2             cb2        pgb2         0.3   0.4   ...
        #                    ... ...
        # rows_by
        rows_by <- rows_by %||% setdiff(colnames(data), c(columns_by, columns_split_by, pie_group_by))
        rows_name <- rows_name %||% ifelse("rows" %in% colnames(data), "rows.1", "rows")
        values_by <- values_by %||% ifelse("value" %in% colnames(data), "value.1", "value")
        data <- tidyr::pivot_longer(data, cols = all_of(rows_by), names_to = rows_name, values_to = values_by)
        data[[rows_name]] <- factor(data[[rows_name]], levels = unique(rows_by))
        data <- data[order(data[[rows_name]]), , drop = FALSE]
        rows_by <- rows_name

        # columns_by/columns_split_by
        columns_by <- check_columns(
            data, columns_by,
            force_factor = TRUE, allow_multi = TRUE, concat_multi = TRUE, concat_sep = columns_by_sep
        )
        stopifnot("[Heatmap] 'columns_by' must be specified when 'in_form = \"wide-rows\"'." = !is.null(columns_by))

        # rows_data/columns_data
        if (!is.null(rows_data)) {
            data <- join_heatmap_meta(
                data, rows_data, by = rows_by, cr_split_by = rows_split_by,
                split_by = split_by, which = "row"
            )
        }
        rows_split_levels <- if (length(rows_split_by) == 1 && is.factor(data[[rows_split_by]])) {
            levels(data[[rows_split_by]])
        } else {
            NULL
        }
        rows_split_by <- check_columns(
            data, rows_split_by,
            force_factor = TRUE, allow_multi = TRUE, concat_multi = TRUE, concat_sep = rows_split_by_sep
        )

        if (!is.null(columns_data)) {
            data <- join_heatmap_meta(
                data, columns_data, by = columns_by, cr_split_by = columns_split_by,
                split_by = split_by, which = "column"
            )
        }
        columns_split_levels <- if (length(columns_split_by) == 1 && is.factor(data[[columns_split_by]])) {
            levels(data[[columns_split_by]])
        } else {
            NULL
        }
        columns_split_by <- check_columns(
            data, columns_split_by,
            force_factor = TRUE, allow_multi = TRUE, concat_multi = TRUE, concat_sep = columns_split_by_sep
        )

        data <- apply_orderby(data, rows_orderby, rows_by, rows_split_by, rows_split_levels)
        data <- apply_orderby(data, columns_orderby, columns_by, columns_split_by, columns_split_levels)

        if (!is.null(columns_name)) {
            if (identical(columns_name, "")) {
                columns_name <- " "
            }
            data <- dplyr::rename(data, !!sym(columns_name) := columns_by)
            columns_by <- columns_name
        }
    } else {  # wide-columns
        # rows_split_by rows_by pie_group_by columns1 columns2 ...
        # rsb1          cb1        pgb1         0.1   0.2   ...
        # rsb2          cb2        pgb2         0.3   0.4   ...
        #                    ... ...
        # rows_by/rows_split_by
        rows_by <- check_columns(
            data, rows_by,
            force_factor = TRUE, allow_multi = TRUE, concat_multi = TRUE, concat_sep = rows_by_sep
        )
        stopifnot("[Heatmap] 'rows_by' must be specified when 'in_form = \"wide-columns\"'." = !is.null(rows_by))

        # columns_by
        columns_by <- columns_by %||% setdiff(colnames(data), c(rows_by, rows_split_by, pie_group_by))
        columns_name <- columns_name %||% ifelse("columns" %in% colnames(data), "columns.1", "columns")
        values_by <- values_by %||% ifelse("value" %in% colnames(data), "value.1", "value")
        data <- tidyr::pivot_longer(data, cols = all_of(columns_by), names_to = columns_name, values_to = values_by)
        data[[columns_name]] <- factor(data[[columns_name]], levels = columns_by)
        data <- data[order(data[[columns_name]]), , drop = FALSE]
        columns_by <- columns_name

        # rows_data/columns_data
        if (!is.null(rows_data)) {
            data <- join_heatmap_meta(
                data, rows_data, by = rows_by, cr_split_by = rows_split_by,
                split_by = split_by, which = "row"
            )
        }
        rows_split_levels <- if (length(rows_split_by) == 1 && is.factor(data[[rows_split_by]])) {
            levels(data[[rows_split_by]])
        } else {
            NULL
        }
        rows_split_by <- check_columns(
            data, rows_split_by,
            force_factor = TRUE, allow_multi = TRUE, concat_multi = TRUE, concat_sep = rows_split_by_sep
        )

        if (!is.null(columns_data)) {
            data <- join_heatmap_meta(
                data, columns_data, by = columns_by, cr_split_by = columns_split_by,
                split_by = split_by, which = "column"
            )
        }
        columns_split_leves <- if (length(columns_split_by) == 1 && is.factor(data[[columns_split_by]])) {
            levels(data[[columns_split_by]])
        } else {
            NULL
        }
        columns_split_by <- check_columns(
            data, columns_split_by,
            force_factor = TRUE, allow_multi = TRUE, concat_multi = TRUE, concat_sep = columns_split_by_sep
        )

        data <- apply_orderby(data, rows_orderby, rows_by, rows_split_by, rows_split_levels)
        data <- apply_orderby(data, columns_orderby, columns_by, columns_split_by, columns_split_leves)

        if (!is.null(rows_name)) {
            if (identical(rows_name, "")) {
                rows_name <- " "
            }
            data <- dplyr::rename(data, !!sym(rows_name) := rows_by)
            rows_by <- rows_name
        }
    }

    if (!is.null(rows_split_name) && !is.null(rows_split_by)) {
        if (identical(rows_split_name, "")) {
            rows_split_name <- " "
        }
        data <- dplyr::rename(data, !!sym(rows_split_name) := rows_split_by)
        rows_split_by <- rows_split_name
    }
    if (!is.null(columns_split_name) && !is.null(columns_split_by)) {
        if (identical(columns_split_name, "")) {
            columns_split_name <- " "
        }
        data <- dplyr::rename(data, !!sym(columns_split_name) := columns_split_by)
        columns_split_by <- columns_split_name
    }
    if (!is.null(pie_name) && !is.null(pie_group_by)) {
        if (identical(pie_name, "")) {
            pie_name <- " "
        }
        data <- dplyr::rename(data, !!sym(pie_name) := pie_group_by)
        pie_group_by <- pie_name
    }
    if (!is.null(name)) {
        if (identical(name, "")) {
            name <- " "
        }
        data <- dplyr::rename(data, !!sym(name) := !!sym(values_by))
        values_by <- name
    }
    keep_na <- check_keep_na(keep_na, c(split_by, rows_by, rows_split_by, columns_by, columns_split_by))
    if (!isFALSE(keep_empty)) {
        warning("[Heatmap] 'keep_empty != FALSE' is not supported yet, and unused levels will be always dropped.")
    }
    data <- process_keep_na_empty(data, keep_na, list())

    list(
        data = if (is.null(split_by)) {
            stats::setNames(list(data), "...")
        } else {
            split(select(data, -!!sym(split_by)), data[[split_by]])
        },
        values_by = values_by,
        rows_by = rows_by,
        rows_split_by = rows_split_by,
        columns_by = columns_by,
        columns_split_by = columns_split_by,
        pie_group_by = pie_group_by,
        keep_na = keep_na,
        keep_empty = keep_empty
    )
}

#' Resolve annotation aliases:
#'   .row         -> rows_by
#'   .rows.split  -> rows_split_by
#'   .col/.column -> columns_by
#'   .col.split/.column.split -> columns_split_by
#' @param lst A list of annotations, which may contain aliases for rows_by, rows_split_by, columns_by, and columns_split_by.
#' @param row_key The actual key for rows_by in the list.
#' @param rsplit_key The actual key for rows_split_by in the list.
#' @param col_key The actual key for columns_by in the list.
#' @param csplit_key The actual key for columns_split_by in the list.
#' @return A list of annotations with aliases resolved to the actual keys.
#' @keywords internal
.resolve_anno_aliases <- function(lst, row_key, rsplit_key, col_key, csplit_key) {
    if (is.null(lst) || !is.list(lst)) return(lst)
    alias_map <- list(
        ".row"           = row_key,
        ".rows"          = row_key,
        ".rows.split"    = rsplit_key,
        ".row.split"     = rsplit_key,
        ".col"           = col_key,
        ".cols"          = col_key,
        ".column"        = col_key,
        ".columns"       = col_key,
        ".col.split"     = csplit_key,
        ".cols.split"    = csplit_key,
        ".column.split"  = csplit_key,
        ".columns.split" = csplit_key
    )
    for (alias in names(alias_map)) {
        real <- alias_map[[alias]]
        if (!is.null(real) && alias %in% names(lst) && !real %in% names(lst)) {
            lst[[real]] <- lst[[alias]]
            lst[[alias]] <- NULL
        } else if (!is.null(real) && alias %in% names(lst)) {
            lst[[alias]] <- NULL  # real key wins; drop alias
        } else if (is.null(real) && alias %in% names(lst)) {
            lst[[alias]] <- NULL  # alias maps to NULL; drop
        }
    }
    lst
}

#' Get the grid.draw-able ggplot grob
#' The output from ggplotGrob can not be directly used in grid.draw, the position can not be set.
#' This function extracts the gTree from the ggplot grob.
#' @param p A ggplot object
#' @param void If TRUE, the theme_void will be added to the ggplot object
#' @param nolegend If TRUE, the legend will be removed from the ggplot object
#' @return A gTree object
#' @importFrom ggplot2 ggplotGrob theme_void theme
#' @keywords internal
.gggrob <- function(p, void = TRUE, nolegend = TRUE) {
    if (isTRUE(void)) {
        p <- p + theme_void()
    }
    if (isTRUE(nolegend)) {
        p <- p + theme(legend.position = "none")
    }
    for (g in ggplotGrob(p)$grobs) {
        if (inherits(g, "gTree") && !inherits(g, "zeroGrob") && !inherits(g, "absoluteGrob")) {
            return(g)
        }
    }
    stop("No gTree found in the ggplot grob.")
}

#' Ensure to convert numeric to grid::unit with mm
#' @noRd
#' @keywords internal
.ensure_unit <- function(param, keys = c("width", "height")) {
    for (key in keys) {
        if (!is.null(param[[key]]) && is.numeric(param[[key]]) && !is.unit(param[[key]])) {
            param[[key]] <- unit(param[[key]], "mm")
        }
    }
    return(param)
}

#' Reorder annotation list so split annotation (split_by) is farthest from heatmap body
#' and name annotation (by) is closest. User annotations stay in between.
#' For all annotation sides, ComplexHeatmap renders first-element = farthest,
#' last-element = closest to the heatmap body.
#' @param x A list of annotations
#' @param by The name of the annotation used for row/column names
#' @param split_by The name of the annotation used for splitting rows/columns
#' @param side The annotation side ("top", "bottom", "left", "right")
#' @return A reordered list of annotations
#' @keywords internal
.reorder_anno_side <- function(x, by, split_by, side) {
    keys <- setdiff(names(x), c("annotation_name_side", "show_annotation_name"))
    name_key <- if (!is.null(by) && by %in% keys) by else character(0)
    split_key <- if (!is.null(split_by) && split_by %in% keys) split_by else character(0)
    if (side %in% c("top", "left")) {
        new_order <- c(split_key, setdiff(keys, c(name_key, split_key)), name_key)
    } else {
        new_order <- c(name_key, setdiff(keys, c(name_key, split_key)), split_key)
    }
    if (!length(new_order)) return(x)
    result <- x[c("annotation_name_side", "show_annotation_name", new_order)]
    if (!is.null(x$show_annotation_name)) {
        result$show_annotation_name <- x$show_annotation_name[
            intersect(new_order, names(x$show_annotation_name))]
    }
    result
}

#' Unified annotation builder for built-in (split/name) and user-defined annotations
#' @param which The annotation direction ("row" or "column")
#' @param names_side The side to place the row/column name annotation ("top", "bottom", "left", "right")
#' @param anno_title The title of the annotation (for split)
#' @param show_names A logical value indicating whether to show row/column names
#' @param annotation A list of user-defined annotations, where names are annotation names and values are annotation objects or parameters to build annotation objects
#' @param annotation_type A list of annotation types, where names are annotation names and values are annotation types ("simple", "label", "block", "ggcat", "ggseries", or "auto")
#' @param annotation_side A list of annotation sides, where names are annotation names and values are annotation sides ("top", "bottom", "left", "right")
#' @param annotation_palette A list of annotation palettes, where names are annotation names and values are palette names or color vectors
#' @param annotation_palcolor A list of annotation palette colors, where names are annotation names and values are color vectors to override the palette
#' @param annotation_agg A list of functions to aggregate the original data for each annotation, where names are annotation names and values are functions that take a vector of values in the cell and return an aggregated value
#' @param annotation_params A list of additional parameters for each annotation, where names are annotation names and values are lists of parameters to pass to the annotation constructor
#' @param split_by The name of the column used for split annotation
#' @param splits A factor vector of splits for the split annotation
#' @param by The name of the column used for name annotation
#' @param by_labels A factor vector of labels for the name annotation
#' @param flip A logical value indicating whether to flip the annotation (for ggseries annotations)
#' @param legend.direction The direction of the legend ("vertical" or "horizontal")
#' @param legend.position The position of the legend ("right", "left", "top", "bottom")
#' @param data A data frame used for ggcat and ggseries annotations, where each row corresponds to a cell in the heatmap and contains the original values before aggregation
#' @return A list of annotations and legends to be passed to ComplexHeatmap.
#' @keywords internal
.setup_annos <- function(
    which, names_side, anno_title, show_names,
    annotation, annotation_type, annotation_side, annotation_palette, annotation_palcolor,
    annotation_agg, annotation_params,
    split_by, splits, by, by_labels,
    flip, legend.direction, legend.position, data
) {
    .legends <- list()
    # --- 1. Normalize inputs ---
    if (is.null(annotation)) annotation <- list()
    if (!is.list(annotation)) {
        annotation <- as.list(annotation)
        names(annotation) <- unlist(annotation)
    }
    if (!is.list(annotation_palette)) {
        annotation_palette <- as.list(rep(
            if (is.character(annotation_palette)) annotation_palette else "Paired",
            length(annotation)))
        names(annotation_palette) <- names(annotation)
    }
    if (!is.list(annotation_palcolor)) {
        annotation_palcolor <- rep(list(annotation_palcolor), length(annotation))
        names(annotation_palcolor) <- names(annotation)
    }
    if (is.character(annotation_type) && !identical(annotation_type, "auto")) {
        annotation_type <- as.list(rep(annotation_type, length(annotation)))
        names(annotation_type) <- names(annotation)
    } else if (identical(annotation_type, "auto")) {
        annotation_type <- as.list(rep("auto", length(annotation)))
        names(annotation_type) <- names(annotation)
    } else if (!is.list(annotation_type)) {
        annotation_type <- as.list(rep("simple", length(annotation)))
        names(annotation_type) <- names(annotation)
    }

    # Add built-in split annotation
    if (!is.null(split_by)) {
        annotation[[split_by]] <- split_by
        annotation_type[[split_by]] <- annotation_type[[split_by]] %||% "simple"
        annotation_palette[[split_by]] <- annotation_palette[[split_by]] %||% "simspec"
        annotation_palcolor[[split_by]] <- annotation_palcolor[[split_by]]
    }
    # Add built-in name annotation
    by_name_annotation <- !is.null(by) && (
        !isFALSE(annotation_params[[by]] %||% TRUE) ||
        annotation_type[[by]] %||% "simple" == "label"
    )
    if (by_name_annotation) {
        annotation[[by]] <- by
        annotation_type[[by]] <- annotation_type[[by]] %||% "simple"
        annotation_palette[[by]] <- annotation_palette[[by]] %||% "Paired"
        annotation_palcolor[[by]] <- annotation_palcolor[[by]]
    }
    annotation_agg <- annotation_agg %||% list()
    annotation_params <- annotation_params %||% list()

    annos <- list(
        annotation_name_side = names_side,
        show_annotation_name = list()
    )

    # --- 2. Process each annotation ---
    for (aname in names(annotation)) {
        if (aname %in% formalArgs(ComplexHeatmap::HeatmapAnnotation)) {
            annos[[aname]] <- annotation[[aname]]
            next
        }
        if (isFALSE(annotation_params[[aname]])) {
            annos[[aname]] <- NULL
            next
        }
        side <- annotation_side[[aname]] %||% annotation_side[[".default"]] %||% ifelse(which == "row", "left", "top")
        is_split <- identical(aname, split_by)
        is_builtin <- is_split || identical(aname, by)
        annotype <- annotation_type[[aname]] %||% "simple"
        param <- annotation_params[[aname]] %||% list()
        param <- .ensure_unit(param)

        if (is_builtin) {
            is_label <- annotype == "label"
            # Built-in: use pre-computed splits/by_labels
            param$x <- if (identical(aname, split_by)) splits else by_labels
            param$title <- aname
            param$which <- ifelse(flip, setdiff(c("column", "row"), which), which)
            worh <- ifelse(param$which == "row", "width", "height")
            param$palette <- annotation_palette[[aname]]
            param$palcolor <- annotation_palcolor[[aname]]

            if (is_label && is_split) {
                # Start from user params so extra arguments (label_rot, etc.) pass through
                block_param <- param
                block_param$x <- levels(param$x)
                block_param$which <- param$which
                block_param$side <- side
                anno_legend <- do.call(anno_block, block_param)
                # annos$show_annotation_name[[aname]] <- FALSE
                param$show_legend = FALSE
            } else {
                param$border <- param$border %||% TRUE
                param$legend.direction <- legend.direction
                show_legend <- !identical(legend.position, "none") && annotype != "label"
                if (is_split) {
                    show_legend <- show_legend && isFALSE(anno_title)
                } else {
                    show_legend <- show_legend && !show_names
                }
                param$show_legend = param$show_legend %||% show_legend
                param$side <- side
                if (annotype == "label") {
                    anno_legend <- do.call(anno_text, param)
                } else {
                    param[[worh]] <- param[[worh]] %||% unit(2.5, "mm")
                    anno_legend <- do.call(anno_simple, param)
                }
            }
            annos[[aname]] <- anno_legend$anno
            if (isTRUE(param$show_legend))
                .legends[[aname]] <- anno_legend$legend
        } else {
            # User-defined: extract and aggregate data
            annocol <- annotation[[aname]]
            annoagg <- annotation_agg[[aname]]
            annodata <- param$x %||% data
            annocol <- check_columns(annodata, annocol)
            if (annotype == "auto") {
                all_ones <- annodata %>%
                    group_by(!!!syms(unique(c(split_by, by)))) %>%
                    summarise(n = n(), .groups = "drop") %>%
                    pull("n")
                all_ones <- all(all_ones == 1)
                if (is.character(annodata[[annocol]]) || is.factor(annodata[[annocol]]) || is.logical(annodata[[annocol]])) {
                    annotype <- ifelse(all_ones, "pie", "simple")
                } else if (is.numeric(annodata[[annocol]])) {
                    annotype <- ifelse(all_ones, "points", "violin")
                } else {
                    stop("[Heatmap] Don't know how to handle ", which, " annotation type for column: ", annocol)
                }
            }
            if (annotype %in% c("simple", "points", "lines") && is.null(annoagg)) {
                warning("[Heatmap] Assuming '", which, "_annotation_agg[\"", aname, "\"] = dplyr::first' for the simple annotation")
                annoagg <- dplyr::first
            }
            if (annotype == "label" && is.null(annoagg)) {
                warning("[Heatmap] Assuming '", which, "_annotation_agg[\"", aname, "\"] = function(x) paste(unique(x), collapse = \",\")' for the label annotation")
                annoagg <- function(x) paste(unique(x), collapse = ", ")
            }
            if (is.null(annoagg)) {
                annodata <- annodata %>% select(!!!syms(unique(c(split_by, by, annocol))))
            } else {
                annodata <- annodata %>%
                    group_by(!!!syms(unique(c(split_by, by)))) %>%
                    summarise(!!sym(annocol) := annoagg(!!sym(annocol)), .groups = "drop")
            }
            param$x <- annodata
            param$split_by <- split_by
            param$group_by <- by
            param$column <- annocol
            param$title <- aname
            param$side <- side
            param$which <- ifelse(flip, setdiff(c("column", "row"), which), which)
            param$palette <- annotation_palette[[aname]] %||% "Paired"
            param$palcolor <- annotation_palcolor[[aname]]
            param$legend.direction <- legend.direction
            if (annotype == "simple") {
                worh <- ifelse(param$which == "row", "width", "height")
                param[[worh]] <- param[[worh]] %||% unit(2.5, "mm")
            }
            if (flip) {
                pheight <- param$height; param$height <- param$width; param$width <- pheight
            }
            if (legend.position == "none") param$show_legend <- FALSE
            if (annotype == "label") annotype <- "text"
            if (!exists(paste0("anno_", annotype)))
                stop("[Heatmap] Unsupported annotation type: ", annotype)
            anno <- do.call(paste0("anno_", annotype), param)
            annos[[aname]] <- anno$anno
            .legends[[paste0(which, ".", aname)]] <- anno$legend
        }
    }

    list(annos = annos, legends = .legends)
}


#' Heatmap annotation function for categorical data
#' @param x A data frame
#' @param split_by A character string of the column name to split the data
#' @param group_by A character string of the column name to group the data
#' @param column A character string of the column name to plot
#' @param title A character string to name the legend
#' @param side A character string showing where the annotation is.
#' @param which A character string specifying the direction of the annotation. Default is "row".
#'  Other options are "column".
#' @param palette A character string specifying the palette of the annotation
#' @param palcolor A character vector of colors to override the palette
#' @param border A logical value indicating whether to draw the border of the annotation
#' @param legend.direction A character string specifying the direction of the legend. Default is "vertical".
#'  Other options are "horizontal".
#' @param show_legend A logical value indicating whether to show the legend
#' @param .plotting A function to create the plot for each split and each group
#' @param ... Other arguments passed to `ComplexHeatmap::AnnotationFunction`
#' @keywords internal
#' @importFrom grid grid.draw grid.lines viewport gpar
#' @importFrom tidyr unite
.anno_ggcat <- function(x, split_by = NULL, group_by, column, title, side = "left", which = "row", palette,
                        palcolor = NULL, border = TRUE, legend.direction, show_legend = TRUE, .plotting, ...) {
    column <- check_columns(x, column, force_factor = TRUE)
    clevels <- levels(x[[column]])

    if (!is.null(split_by)) {
        x <- x %>% dplyr::arrange(!!!syms(c(split_by, group_by)))
        x <- x %>% unite("..split", split_by, group_by, remove = FALSE)
        plots <- .plotting(data = x, column = column, group_by = "..split", palette = palette, palcolor = palcolor)
        plots <- lapply(plots, .gggrob)
    } else {
        plots <- .plotting(data = x, column = column, group_by = group_by, palette = palette, palcolor = palcolor)
        plots <- lapply(plots, .gggrob)
    }
    # add legend
    if (isTRUE(show_legend)) {
        lgd <- ComplexHeatmap::Legend(
            title = title,
            labels = clevels,
            legend_gp = gpar(fill = palette_this(clevels, palette = palette, palcolor = palcolor)),
            border = TRUE, nrow = if (legend.direction == "horizontal") 1 else NULL
        )
    } else {
        lgd <- NULL
    }
    anno <- ComplexHeatmap::AnnotationFunction(
        fun = function(index, k, n) {
            if (which == "row") {
                index <- rev(index)
            }
            grobs <- grobs[index]
            total <- length(index)
            # draw border
            grid.lines(x = c(0, 0), y = c(0, 1), gp = gpar(col = "black", lwd = 1))
            grid.lines(x = c(1, 1), y = c(0, 1), gp = gpar(col = "black", lwd = 1))
            grid.lines(x = c(0, 1), y = c(0, 0), gp = gpar(col = "black", lwd = 1))
            grid.lines(x = c(0, 1), y = c(1, 1), gp = gpar(col = "black", lwd = 1))
            for (i in seq_along(grobs)) {
                if (which == "row") {
                    if (side == "left") {
                        grobs[[i]]$vp <- viewport(x = 0.5, y = (i - 1) * 1 / total + 1 / (2 * total), width = 0.95, height = 1 / total, angle = 180)
                    } else {
                        grobs[[i]]$vp <- viewport(x = 0.5, y = (i - 1) * 1 / total + 1 / (2 * total), width = 0.95, height = 1 / total)
                    }
                } else {
                    grobs[[i]]$vp <- viewport(x = (i - 1) * 1 / total + 1 / (2 * total), y = 0.5, width = 1 / total, height = 1)
                }
                grid.draw(grobs[[i]])
            }
        },
        var_import = list(grobs = plots, which = which, side = side),
        n = length(plots),
        which = which,
        subsettable = TRUE,
        ...
    )
    list(anno = anno, legend = lgd)
}

#' Heatmap annotation functions
#'
#' @rdname heatmap-anno
#' @param x A data frame
#' @param split_by A character string of the column name to split the data
#' @param group_by A character string of the column name to group the data
#' @param column A character string of the column name to plot
#' @param title A character string to name the legend
#' @param which A character string specifying the direction of the annotation. Default is "row".
#'  Other options are "column".
#' @param palette A character string specifying the palette of the annotation
#' @param palcolor A character vector of colors to override the palette
#' @param border A logical value indicating whether to draw the border of the annotation
#' @param legend.direction A character string specifying the direction of the legend. Default is "vertical".
#'  Other options are "horizontal".
#' @param show_legend A logical value indicating whether to show the legend
#' @param .plotting A function to create the plot for each split and each group
#' @param ... Other arguments passed to `ComplexHeatmap::AnnotationFunction`
#' @keywords internal
#' @importFrom grid grid.draw grid.lines viewport gpar
#' @importFrom tidyr unite
.anno_ggseries <- function(x, split_by = NULL, group_by, column, title, side = "left", which = "row", palette,
                           palcolor = NULL, border = TRUE, legend.direction, show_legend = TRUE, .plotting, ...) {
    x$.x <- x[[group_by]]
    glevels <- levels(x[[group_by]])
    colors <- palette_this(glevels, palette = palette, palcolor = palcolor)
    if (!is.null(split_by)) {
        x <- x %>% dplyr::arrange(!!!syms(c(split_by, group_by)))
        x <- x %>% unite("..split", split_by, group_by, remove = FALSE)
        plots <- .plotting(data = x, column = column, group_by = "..split", palette = palette, palcolor = as.list(colors))
        plots <- lapply(plots, .gggrob)
    } else {
        plots <- .plotting(data = x, column = column, group_by = group_by, palette = palette, palcolor = as.list(colors))
        plots <- lapply(plots, .gggrob)
    }

    # add legend
    if (isTRUE(show_legend)) {
        lgd <- ComplexHeatmap::Legend(
            title = title,
            labels = glevels,
            legend_gp = gpar(fill = palette_this(glevels, palette = palette, palcolor = colors)),
            border = TRUE, nrow = if (legend.direction == "horizontal") 1 else NULL
        )
    } else {
        lgd <- NULL
    }

    anno <- ComplexHeatmap::AnnotationFunction(
        fun = function(index, k, n) {
            if (identical(which, "row")) {
                index <- rev(index)
            }
            grobs <- grobs[index]
            total <- length(index)
            # draw border
            grid.lines(x = c(0, 0), y = c(0, 1), gp = gpar(col = "black", lwd = 1))
            grid.lines(x = c(1, 1), y = c(0, 1), gp = gpar(col = "black", lwd = 1))
            grid.lines(x = c(0, 1), y = c(0, 0), gp = gpar(col = "black", lwd = 1))
            grid.lines(x = c(0, 1), y = c(1, 1), gp = gpar(col = "black", lwd = 1))
            for (i in seq_along(grobs)) {
                if (which == "row") {
                    if (side == "left") {
                        grobs[[i]]$vp <- viewport(x = 0.5, y = (i - 1) * 1 / total + 1 / (2 * total), width = 0.95, height = 1 / total, angle = 180)
                    } else {
                        grobs[[i]]$vp <- viewport(x = 0.5, y = (i - 1) * 1 / total + 1 / (2 * total), width = 0.95, height = 1 / total)
                    }
                } else {
                    grobs[[i]]$vp <- viewport(x = (i - 1) * 1 / total + 1 / (2 * total), y = 0.5, width = 1 / total, height = 0.95)
                }
                grid.draw(grobs[[i]])
            }
        },
        var_import = list(grobs = plots, which = which, side = side),
        n = length(plots),
        which = which,
        subsettable = TRUE,
        ...
    )

    list(anno = anno, legend = lgd)
}

#' @rdname heatmap-anno
#' @param x A data frame
#' @param split_by A character string of the column name to split the data (heatmap)
#' @param group_by A character string of the column name to group the data (rows or columns of the heatmap)
#' @param column A character string of the column name of the data `x` to plot
#' @param title A character string to name the legend
#' @param side A character string specifying the side of the annotation. Default is "left".
#' @param which A character string specifying the direction of the annotation. Default is "row".
#'  Other options are "column".
#' @param palette A character string specifying the palette of the annotation
#' @param palcolor A character vector of colors to override the palette
#' @param border A logical value indicating whether to draw the border of the annotation
#' @param legend.direction A character string specifying the direction of the legend. Default is "vertical".
#'  Other options are "horizontal".
#' @param show_legend A logical value indicating whether to show the legend
#' @param ... Other arguments passed to `ComplexHeatmap::AnnotationFunction`
#'  The parameters passed to `row_annotation_params` and `column_annotation_params` will be passed here.
anno_pie <- function(x, split_by = NULL, group_by, column, title, which = "row", palette, side = "left",
                     palcolor = NULL, border = TRUE, legend.direction, show_legend = TRUE, ...) {
    .anno_ggcat(
        x = x, split_by = split_by, group_by = group_by, column = column, title = title, which = which, side = side,
        palette = palette, palcolor = palcolor, border = border, legend.direction = legend.direction, show_legend = show_legend,
        .plotting = function(data, column, group_by, palette, palcolor) {
            PieChart(data, x = column, split_by = group_by, palette = palette, palcolor = palcolor, combine = FALSE)
        }, ...
    )
}

#' @rdname heatmap-anno
anno_ring <- function(x, split_by = NULL, group_by, column, title, which = "row", palette, side = "left",
                      palcolor = NULL, border = TRUE, legend.direction, show_legend = TRUE, ...) {
    .anno_ggcat(
        x = x, split_by = split_by, group_by = group_by, column = column, which = which, title = title, side = side,
        palette = palette, palcolor = palcolor, border = border, legend.direction = legend.direction, show_legend = show_legend,
        .plotting = function(data, column, group_by, palette, palcolor) {
            RingPlot(data, group_by = column, split_by = group_by, palette = palette, palcolor = palcolor, combine = FALSE)
        }, ...
    )
}

#' @rdname heatmap-anno
anno_bar <- function(x, split_by = NULL, group_by, column, title, which = "row", palette, side = "left",
                     palcolor = NULL, border = TRUE, legend.direction, show_legend = TRUE, ...) {
    .anno_ggcat(
        x = x, split_by = split_by, group_by = group_by, column = column, which = which, title = title, side = side,
        palette = palette, palcolor = palcolor, border = border, legend.direction = legend.direction, show_legend = show_legend,
        .plotting = function(data, column, group_by, palette, palcolor) {
            BarPlot(data,
                x = column, split_by = group_by, expand = c(0.05, 1),
                palette = palette, palcolor = palcolor, combine = FALSE
            )
        }, ...
    )
}

#' @rdname heatmap-anno
anno_violin <- function(x, split_by = NULL, group_by, column, title, which = "row", palette, side = "left",
                        palcolor = NULL, border = TRUE, legend.direction, show_legend = TRUE, ...) {
    .anno_ggseries(
        x = x, split_by = split_by, group_by = group_by, column = column, which = which, title = title, side = side,
        palette = palette, palcolor = palcolor, border = border, legend.direction = legend.direction, show_legend = show_legend,
        .plotting = function(data, column, group_by, palette, palcolor) {
            ViolinPlot(data,
                x = ".x", y = column, split_by = group_by, combine = FALSE,
                palette = palette, palcolor = palcolor, flip = which == "row"
            )
        }, ...
    )
}

#' @rdname heatmap-anno
anno_boxplot <- function(x, split_by = NULL, group_by, column, title, which = "row", palette, side = "left",
                         palcolor = NULL, border = TRUE, legend.direction, show_legend = TRUE, ...) {
    .anno_ggseries(
        x = x, split_by = split_by, group_by = group_by, column = column, which = which, title = title, side = side,
        palette = palette, palcolor = palcolor, border = border, legend.direction = legend.direction, show_legend = show_legend,
        .plotting = function(data, column, group_by, palette, palcolor) {
            BoxPlot(data,
                x = ".x", y = column, split_by = group_by, combine = FALSE,
                palette = palette, palcolor = palcolor, flip = which == "row"
            )
        }, ...
    )
}

#' @rdname heatmap-anno
anno_density <- function(x, split_by = NULL, group_by, column, title, which = "row", palette, side = "left",
                         palcolor = NULL, border = TRUE, legend.direction, show_legend = TRUE, ...) {
    .anno_ggseries(
        x = x, split_by = split_by, group_by = group_by, column = column, title = title, which = which, side = side,
        palette = palette, palcolor = palcolor, border = border, legend.direction = legend.direction, show_legend = show_legend,
        .plotting = function(data, column, group_by, palette, palcolor) {
            DensityPlot(data,
                x = column, split_by = group_by, combine = FALSE,
                palette = palette, palcolor = palcolor, flip = which == "row"
            )
        }, ...
    )
}

#' @rdname heatmap-anno
#' @param alpha A numeric value between 0 and 1 specifying the transparency of the annotation
anno_simple <- function(
    x, split_by = NULL, group_by = NULL, column = NULL, title, which = "row", side = "left",
    palette, palcolor = NULL, border = TRUE, legend.direction, show_legend = TRUE,
    alpha = 1, ...) {
    if (!is.null(split_by)) {
        x <- do.call(rbind, split(x, x[[split_by]]))
    }
    if (!is.null(column)) {
        x <- x[[column]]
    }
    is_cont <- is.numeric(x)
    if (isFALSE(is_cont) && !is.factor(x)) {
        x <- factor(x, levels = unique(x))
    }
    # add legend
    if (is_cont) {
        col_fun <- colorRamp2(
            seq(min(x), max(x), length = 100),
            palette_this(palette = palette, palcolor = palcolor, alpha = alpha)
        )
        lgd <- if (isTRUE(show_legend)) ComplexHeatmap::Legend(
            title = title,
            col_fun = col_fun,
            border = TRUE, direction = legend.direction
        )
        anno <- ComplexHeatmap::anno_simple(x, col = col_fun, which = which, border = border, ...)
    } else {
        x_levels <- levels(x)
        colors <- palette_this(x_levels, palette = palette, palcolor = palcolor, alpha = alpha, NA_keep = TRUE)
        lgd <- if (isTRUE(show_legend)) ComplexHeatmap::Legend(
            title = title,
            labels = x_levels,
            legend_gp = gpar(fill = colors),
            border = TRUE, nrow = if (legend.direction == "horizontal") 1 else NULL
        )
        anno <- ComplexHeatmap::anno_simple(as.character(x), col = colors, which = which, border = border, ...)
    }

    list(anno = anno, legend = lgd)
}

#' @rdname heatmap-anno
anno_points <- function(x, split_by = NULL, group_by, column, title, which = "row", palette, side = "left",
                        palcolor = NULL, border = TRUE, legend.direction, show_legend = TRUE, alpha = 1, ...) {
    if (!is.null(split_by)) {
        x <- do.call(rbind, split(x, x[[split_by]]))
    }

    anno <- ComplexHeatmap::anno_points(
        x[[column]],
        which = which, border = border, ...
    )
    list(anno = anno, legend = NULL)
}

#' @rdname heatmap-anno
#' @param add_points A logical value indicating whether to add points to the annotation
anno_lines <- function(x, split_by = NULL, group_by, column, title, which = "row", palette, side = "left",
                       palcolor = NULL, border = TRUE, legend.direction, show_legend = TRUE, alpha = 1, add_points = TRUE, ...) {
    anno <- ComplexHeatmap::anno_lines(
        x[[column]],
        which = which, border = border, add_points = add_points, ...
    )
    list(anno = anno, legend = NULL)
}

#' @rdname heatmap-anno
#' @keywords internal
anno_block <- function(
    x, split_by = NULL, group_by = NULL, column, title, which = "row", side = "left",
    palette, palcolor = NULL, border = TRUE, legend.direction, show_legend = FALSE, alpha = 1,
    ...
) {
    args <- list(
        labels = x,
        which = which,
        ...
    )
    args$gp <- args$gp %||% gpar()
    if (isFALSE(border)) {
        args$gp$col <- "transparent"
    }
    if (is.null(args$gp$fill)) {
        args$gp$fill <- palette_this(x, palette = palette, palcolor = palcolor, alpha = alpha, NA_keep = TRUE)
    }
    args$labels_rot <- args$labels_rot %||% (
        if (side == "left") 90
        else if (side == "right") -90
        else if (side == "top") 0
        else if (side == "bottom") 0
        else 0
    )
    max_height <- ComplexHeatmap::max_text_height(x, gp = args$gp)
    max_width <- ComplexHeatmap::max_text_width(x, gp = args$gp)
    if (which == "row" && is.null(args$width)) {
        max_height <- max_height + max(max_height * 0.6, max_width * abs(sin((args$labels_rot + 90) / 180 * pi)))
        args$width <- args$width %||% max_height * 1.2
    } else if (which == "column" && is.null(args$height)) {
        max_height <- max_height + max(max_height * 0.6, max_width * abs(sin(args$labels_rot / 180 * pi)))
        args$height <- args$height %||% max_height * 1.2
    }
    anno <- do.call(ComplexHeatmap::anno_block, args)
    list(anno = anno, legend = NULL)
}

#' @rdname heatmap-anno
#' @keywords internal
anno_text <- function(
    x, split_by = NULL, group_by, column = NULL, title, which = "row", palette, side = "left",
    palcolor = NULL, border = TRUE, legend.direction, show_legend = FALSE, alpha = 1,
    ...
) {
    if (!is.null(split_by)) {
        x <- do.call(rbind, split(x, x[[split_by]]))
    }
    if (!is.null(column)) {
        x <- x[[column]]
    }
    if (!is.factor(x)) {
        x <- factor(x, levels = unique(x))
    }
    x_levels <- levels(x)
    colors <- palette_this(x_levels, palette = palette, palcolor = palcolor, alpha = alpha, NA_keep = TRUE)
    args <- list(...)
    args$x <- x_levels
    args$which <- which
    args$gp <- args$gp %||% args$labels_gp %||% gpar()
    args$labels_gp <- NULL
    args$rot <- args$rot %||% args$labels_rot %||% (
        if (side == "left") 90
        else if (side == "right") -90
        else if (side == "top") 0
        else if (side == "bottom") 0
        else 0
    )
    args$labels_rot <- NULL
    if (is.null(args$gp$fill)) {
        args$gp$fill <- colors
    }
    if (is.null(args$gp$border)) {
        args$gp$border <- if (border) "black" else "transparent"
    }
    max_height <- ComplexHeatmap::max_text_height(x_levels, gp = args$gp)
    max_width <- ComplexHeatmap::max_text_width(x_levels, gp = args$gp)
    if (which == "row" && is.null(args$width)) {
        max_height <- max_height + max(max_height * 0.6, max_width * abs(sin((args$rot + 90) / 180 * pi)))
        args$width <- args$width %||% max_height * 1.2
    } else if (which == "column" && is.null(args$height)) {
        max_height <- max_height + max(max_height * 0.6, max_width * abs(sin(args$rot / 180 * pi)))
        args$height <- args$height %||% max_height * 1.2
    }
    args$location <- args$location %||% max_height * 0.55
    args$just <- args$just %||% c("center", "center")
    anno <- do.call(ComplexHeatmap::anno_text, args)
    list(anno = anno, legend = NULL)
}

#' Heatmap layer functions used to draw on the heatmap cells
#'
#' @rdname heatmap-layer
#' @param j An integer specifying the column index
#' @param i An integer specifying the row index
#' @param x A numeric vector specifying the x position
#' @param y A numeric vector specifying the y position
#' @param w A numeric vector specifying the width
#' @param h A numeric vector specifying the height
#' @param fill A character vector specifying the fill color
#' @keywords internal
layer_white_bg <- function(j, i, x, y, w, h, fill) {
    grid.rect(x = x, y = y, width = w, height = h, gp = gpar(fill = "white", col = "white", lwd = 1))
}

#' @rdname heatmap-layer
#' @param alpha A numeric value between 0 and 1 specifying the transparency of the fill color
#' @keywords internal
layer_bg <- function(j, i, x, y, w, h, fill, alpha) {
    grid.rect(x = x, y = y, width = w, height = h, gp = gpar(col = fill, lwd = 1, fill = adjcolors(fill, alpha)))
}

#' @rdname heatmap-layer
#' @param color A character vector specifying the color of the reticle
#' @keywords internal
layer_reticle <- function(j, i, x, y, w, h, fill, color) {
    ind_mat <- ComplexHeatmap::restore_matrix(j, i, x, y)
    ind_top <- ind_mat[1, ]
    ind_left <- ind_mat[, 1]
    for (col in seq_len(ncol(ind_mat))) {
        grid.lines(
            x = unit(rep(x[ind_top[col]], each = 2), "npc"), y = unit(c(0, 1), "npc"),
            gp = gpar(col = color, lwd = 1.5)
        )
    }
    for (row in seq_len(nrow(ind_mat))) {
        grid.lines(
            x = unit(c(0, 1), "npc"), y = unit(rep(y[ind_left[row]], each = 2), "npc"),
            gp = gpar(col = color, lwd = 1.5)
        )
    }
}

#' @rdname heatmap-layer
#' @param data A dataframe used to create the annotation. Different from the data used to
#'  create the heatmap itself, which is aggregated data. This dataframe is the original data,
#'  where each cell could have multiple values.
#' @param dot_size A numeric value specifying the size of the dot or a function to calculate the size
#'  from the values in the cell. The function can take 1, 3, or 5 arguments: the first argument is
#'  the values in the cell before aggregation; the 2nd and 3rd arguments are the row and column
#'  indices; the 4th and 5th arguments are the row and column names.
#' @param row_names Row names from the heatmap matrix.
#' @param col_names Column names from the heatmap matrix.
#' @keywords internal
layer_dot <- function(j, i, x, y, w, h, fill, data, dot_size, alpha, row_names = NULL, col_names = NULL) {
    if (is.numeric(dot_size) && length(dot_size) == 1) {
        # Simple numeric size
        grid.points(x, y,
            pch = 21, size = unit(dot_size, "mm"),
            gp = gpar(col = "black", lwd = 1, fill = adjcolors(fill, alpha))
        )
    } else {
        # dot_size is a pre-computed vector/list or will be computed from function
        grid.points(x, y,
            pch = 21, size = unit(scales::rescale(unlist(dot_size), to = c(.5, 12)), "mm"),
            gp = gpar(col = "black", lwd = 1, fill = adjcolors(fill, alpha))
        )
    }
}

#' @rdname heatmap-layer
#' @param col_fun A function to calculate the color of the bars
#' @keywords internal
layer_bars <- function(j, i, x, y, w, h, fill, flip, col_fun, data, alpha) {
    # colors be like [1,1.9]: '#A6CEE3' (1.9,2.8]: '#1F78B4' (2.8,3.7]: '#B2DF8A'
    indices <- paste(i, j, sep = "-")
    data <- data[indices]
    ns <- lengths(data)
    if (flip) {
        # rep(w / ns, ns) can't keep the unit
        bw <- rep(w, lengths(data))
        bh <- rep(sapply(seq_along(h), function(m) h[m] / ns[m]), ns)
        by <- unlist(lapply(seq_along(y), function(m) {
            y[m] - h[m] / 2 + seq_along(data[[m]]) * h[m] / length(data[[m]])
        })) - bh / 2
        bx <- rep(x, lengths(data))
    } else {
        # rep(w / ns, ns) can't keep the unit
        bw <- rep(sapply(seq_along(w), function(m) w[m] / ns[m]), ns)
        bh <- rep(h, lengths(data))
        bx <- unlist(lapply(seq_along(x), function(m) {
            x[m] - w[m] / 2 + seq_along(data[[m]]) * w[m] / length(data[[m]])
        })) - bw / 2
        by <- rep(y, lengths(data))
    }
    bf <- unlist(lapply(data, col_fun))
    grid.rect(x = bx, y = by, width = bw, height = bh, gp = gpar(fill = bf, col = "transparent"))
}

#' @rdname heatmap-layer
#' @keywords internal
layer_pie <- function(j, i, x, y, w, h, fill, palette, palcolor, data, pie_size) {
    indices <- paste(i, j, sep = "-")
    data <- data[indices]
    pies <- lapply(indices, function(idx) {
        p <- PieChart(data[[idx]], x = "Var", y = "Freq", label = NULL, palette = palette, palcolor = palcolor)
        ggplotGrob(p + theme_void() + theme(legend.position = "none"))
    })

    if (!is.function(pie_size)) {
        pie_sizes <- rep(pie_size %||% 1, length(pies))
    } else {
        pie_sizes <- sapply(data, function(d) pie_size(sum(d$Freq, na.rm = TRUE)))
        pie_sizes <- scales::rescale(pie_sizes, to = c(0.2, 1))
    }
    idx <- which(sapply(pies[[1]]$grobs, function(g) inherits(g, "gTree") && !inherits(g, "zeroGrob") && !inherits(g, "absoluteGrob")))[1]
    for (m in seq_along(pies)) {
        pies[[m]]$grobs[[idx]]$vp <- viewport(x = x[m], y = y[m], width = pie_sizes[m] * w[m], height = pie_sizes[m] * h[m])
        grid.draw(pies[[m]]$grobs[[idx]])
    }
}

#' @rdname heatmap-layer
#' @param colors A character vector specifying the fill color of the violin plot.
#'  If not provided, the fill color of row/column annotation will be used
#' @keywords internal
layer_boxviolin <- function(j, i, x, y, w, h, fill, flip, data, colors, fn) {
    vlndata <- data[paste(i, j, sep = "-")]
    vlnplots <- lapply(seq_along(vlndata), function(m) {
        p <- fn(data.frame(x = 1, y = vlndata[[m]]), x = "x", y = "y", palcolor = colors %||% fill[m], flip = flip)
        ggplotGrob(p + theme_void() + theme(legend.position = "none"))
    })
    idx <- which(sapply(vlnplots[[1]]$grobs, function(g) inherits(g, "gTree") && !inherits(g, "zeroGrob") && !inherits(g, "absoluteGrob")))[1]
    for (m in seq_along(vlnplots)) {
        wm <- if (flip) w[m] * 0.95 else w[m]
        hm <- if (flip) h[m] * 0.95 else h[m]
        vlnplots[[m]]$grobs[[idx]]$vp <- viewport(x = x[m], y = y[m], width = wm, height = hm)
        grid.draw(vlnplots[[m]]$grobs[[idx]])
    }
}
