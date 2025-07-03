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
#' @return A list containing the processed data and metadata:
#' * `data`: A list of data frames, one for each level of `split_by`. If no `split_by` is provided, the name will be `"..."`.
#'    Each data frame is in the long format.
#' * `values_by`: The name of the column containing the values to be plotted.
#' * `rows_by`: The name of the column containing the row information.
#' * `rows_split_by`: The name of the column containing the row split information.
#' * `columns_by`: The name of the column containing the column information.
#' * `columns_split_by`: The name of the column containing the column split information.
#' * `pie_group_by`: The name of the column containing the pie group information.
#' @importFrom rlang sym %||%
#' @keywords internal
process_heatmap_data <- function(
    data, in_form, values_by, name,
    split_by, split_by_sep,
    rows_by, rows_by_sep, rows_name,
    rows_split_by, rows_split_by_sep, rows_split_name,
    columns_by, columns_by_sep, columns_name,
    columns_split_by, columns_split_by_sep, columns_split_name,
    pie_group_by, pie_group_by_sep, pie_name,
    rows_data, columns_data
) {
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

        rows_name <- rows_name %||% "rows"
        data <- as.data.frame(data)
        columns_by <- colnames(data)
        data[rows_name] <- rownames(data)
        rows_by <- rows_name

        in_form <- "wide-columns"
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
        rows_split_by <- check_columns(
            data, rows_split_by,
            force_factor = TRUE, allow_multi = TRUE, concat_multi = TRUE, concat_sep = rows_split_by_sep
        )

        # columns_by/columns_split_by
        columns_by <- check_columns(
            data, columns_by,
            force_factor = TRUE, allow_multi = TRUE, concat_multi = TRUE, concat_sep = columns_by_sep
        )
        stopifnot("[Heatmap] 'columns_by' must be specified when 'in_form = \"long\"'." = !is.null(columns_by))
        columns_split_by <- check_columns(
            data, columns_split_by,
            force_factor = TRUE, allow_multi = TRUE, concat_multi = TRUE, concat_sep = columns_split_by_sep
        )

        # join rows_data/columns_data
        if (!is.null(rows_data)) {
            join_by <- if (is.null(split_by) || !split_by %in% colnames(rows_data)) {
                rows_by
            } else {
                c(rows_by, split_by)
            }
            data <- dplyr::left_join(data, rows_data, by = join_by, suffix = c("", ".x"))
        }
        if (!is.null(columns_data)) {
            join_by <- if (is.null(split_by) || !split_by %in% colnames(columns_data)) {
                columns_by
            } else {
                c(columns_by, split_by)
            }
            data <- dplyr::left_join(data, columns_data, by = join_by, suffix = c("", ".y"))
        }

        # rename
        if (!is.null(rows_name)) {
            data <- dplyr::rename(data, !!sym(rows_name) := rows_by)
            rows_by <- rows_name
        }
        if (!is.null(columns_name)) {
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
        data <- tidyr::pivot_longer(data, cols = rows_by, names_to = rows_name, values_to = values_by)
        data[[rows_name]] <- factor(data[[rows_name]], levels = rows_by)
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
            join_by <- if (is.null(split_by) || !split_by %in% colnames(rows_data)) {
                rows_by
            } else {
                c(rows_by, split_by)
            }
            data <- dplyr::left_join(data, rows_data, by = join_by, suffix = c("", ".x"))
        }
        row_split_by <- check_columns(
            data, rows_split_by,
            force_factor = TRUE, allow_multi = TRUE, concat_multi = TRUE, concat_sep = rows_split_by_sep
        )
        if (!is.null(columns_data)) {
            join_by <- if (is.null(split_by) || !split_by %in% colnames(columns_data)) {
                columns_by
            } else {
                c(columns_by, split_by)
            }
            data <- dplyr::left_join(data, columns_data, by = join_by, suffix = c("", ".y"))
        }
        columns_split_by <- check_columns(
            data, columns_split_by,
            force_factor = TRUE, allow_multi = TRUE, concat_multi = TRUE, concat_sep = columns_split_by_sep
        )

        if (!is.null(columns_name)) {
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
        data <- tidyr::pivot_longer(data, cols = columns_by, names_to = columns_name, values_to = values_by)
        data[[columns_name]] <- factor(data[[columns_name]], levels = columns_by)
        data <- data[order(data[[columns_name]]), , drop = FALSE]
        columns_by <- columns_name

        # rows_data/columns_data
        if (!is.null(rows_data)) {
            join_by <- if (is.null(split_by) || !split_by %in% colnames(rows_data)) {
                rows_by
            } else {
                c(rows_by, split_by)
            }
            data <- dplyr::left_join(data, rows_data, by = join_by, suffix = c("", ".x"))
        }
        rows_split_by <- check_columns(
            data, rows_split_by,
            force_factor = TRUE, allow_multi = TRUE, concat_multi = TRUE, concat_sep = rows_split_by_sep
        )
        if (!is.null(columns_data)) {
            join_by <- if (is.null(split_by) || !split_by %in% colnames(columns_data)) {
                columns_by
            } else {
                c(columns_by, split_by)
            }
            data <- dplyr::left_join(data, columns_data, by = join_by, suffix = c("", ".y"))
        }
        columns_split_by <- check_columns(
            data, columns_split_by,
            force_factor = TRUE, allow_multi = TRUE, concat_multi = TRUE, concat_sep = columns_split_by_sep
        )

        if (!is.null(rows_name)) {
            data <- dplyr::rename(data, !!sym(rows_name) := rows_by)
            rows_by <- rows_name
        }
    }

    if (!is.null(rows_split_name) && !is.null(rows_split_by)) {
        data <- dplyr::rename(data, !!sym(rows_split_name) := rows_split_by)
        rows_split_by <- rows_split_name
    }
    if (!is.null(columns_split_name) && !is.null(columns_split_by)) {
        data <- dplyr::rename(data, !!sym(columns_split_name) := columns_split_by)
        columns_split_by <- columns_split_name
    }
    if (!is.null(pie_name) && !is.null(pie_group_by)) {
        data <- dplyr::rename(data, !!sym(pie_name) := pie_group_by)
        pie_group_by <- pie_name
    }
    if (!is.null(name)) {
        data <- dplyr::rename(data, !!sym(name) := !!sym(values_by))
        values_by <- name
    }

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
        pie_group_by = pie_group_by
    )
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
gggrob <- function(p, void = TRUE, nolegend = TRUE) {
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

#' Heatmap annotation function for categorical data
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
.anno_ggcat <- function(x, split_by = NULL, group_by, column, title, which = "row", palette,
                        palcolor = NULL, border = TRUE, legend.direction, show_legend = TRUE, .plotting, ...) {
    column <- check_columns(x, column, force_factor = TRUE)
    clevels <- levels(x[[column]])

    if (!is.null(split_by)) {
        x <- x %>% unite("..split", split_by, group_by, remove = FALSE)
        plots <- .plotting(data = x, column = column, group_by = "..split", palette = palette, palcolor = palcolor)
        plots <- lapply(plots, gggrob)
    } else {
        plots <- .plotting(data = x, column = column, group_by = group_by, palette = palette, palcolor = palcolor)
        plots <- lapply(plots, gggrob)
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
                    grobs[[i]]$vp <- viewport(x = 0.5, y = (i - 1) * 1 / total + 1 / (2 * total), width = 0.95, height = 1 / total)
                } else {
                    grobs[[i]]$vp <- viewport(x = (i - 1) * 1 / total + 1 / (2 * total), y = 0.5, width = 1 / total, height = 1)
                }
                grid.draw(grobs[[i]])
            }
        },
        var_import = list(grobs = plots, which = which),
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
.anno_ggseries <- function(x, split_by = NULL, group_by, column, title, which = "row", palette,
                           palcolor = NULL, border = TRUE, legend.direction, show_legend = TRUE, .plotting, ...) {
    x$.x <- x[[group_by]]
    glevels <- levels(x[[group_by]])
    colors <- palette_this(glevels, palette = palette, palcolor = palcolor)
    if (!is.null(split_by)) {
        x <- x %>% unite("..split", split_by, group_by, remove = FALSE)
        # We need to use show the original group_by in the legend
        # but here we have united the split_by and group_by
        x <- x %>% mutate(..palcolors = colors[as.numeric(x[[group_by]])])
        palcolors <- x$..palcolors
        names(palcolors) <- x$..split
        plots <- .plotting(data = x, column = column, group_by = "..split", palette = palette, palcolor = as.list(palcolors))
        plots <- lapply(plots, gggrob)
    } else {
        plots <- .plotting(data = x, column = column, group_by = group_by, palette = palette, palcolor = as.list(colors))
        plots <- lapply(plots, gggrob)
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
                    grobs[[i]]$vp <- viewport(x = 0.5, y = (i - 1) * 1 / total + 1 / (2 * total), width = 0.95, height = 1 / total)
                } else {
                    grobs[[i]]$vp <- viewport(x = (i - 1) * 1 / total + 1 / (2 * total), y = 0.5, width = 1 / total, height = 0.95)
                }
                grid.draw(grobs[[i]])
            }
        },
        var_import = list(grobs = plots, which = which),
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
anno_pie <- function(x, split_by = NULL, group_by, column, title, which = "row", palette,
                     palcolor = NULL, border = TRUE, legend.direction, show_legend = TRUE, ...) {
    .anno_ggcat(
        x = x, split_by = split_by, group_by = group_by, column = column, title = title, which = which,
        palette = palette, palcolor = palcolor, border = border, legend.direction = legend.direction, show_legend = show_legend,
        .plotting = function(data, column, group_by, palette, palcolor) {
            PieChart(data, x = column, split_by = group_by, palette = palette, palcolor = palcolor, combine = FALSE)
        }, ...
    )
}

#' @rdname heatmap-anno
anno_ring <- function(x, split_by = NULL, group_by, column, title, which = "row", palette,
                      palcolor = NULL, border = TRUE, legend.direction, show_legend = TRUE, ...) {
    .anno_ggcat(
        x = x, split_by = split_by, group_by = group_by, column = column, which = which, title = title,
        palette = palette, palcolor = palcolor, border = border, legend.direction = legend.direction, show_legend = show_legend,
        .plotting = function(data, column, group_by, palette, palcolor) {
            RingPlot(data, group_by = column, split_by = group_by, palette = palette, palcolor = palcolor, combine = FALSE)
        }, ...
    )
}

#' @rdname heatmap-anno
anno_bar <- function(x, split_by = NULL, group_by, column, title, which = "row", palette,
                     palcolor = NULL, border = TRUE, legend.direction, show_legend = TRUE, ...) {
    .anno_ggcat(
        x = x, split_by = split_by, group_by = group_by, column = column, which = which, title = title,
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
anno_violin <- function(x, split_by = NULL, group_by, column, title, which = "row", palette,
                        palcolor = NULL, border = TRUE, legend.direction, show_legend = TRUE, ...) {
    .anno_ggseries(
        x = x, split_by = split_by, group_by = group_by, column = column, which = which, title = title,
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
anno_boxplot <- function(x, split_by = NULL, group_by, column, title, which = "row", palette,
                         palcolor = NULL, border = TRUE, legend.direction, show_legend = TRUE, ...) {
    .anno_ggseries(
        x = x, split_by = split_by, group_by = group_by, column = column, which = which, title = title,
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
anno_density <- function(x, split_by = NULL, group_by, column, title, which = "row", palette,
                         palcolor = NULL, border = TRUE, legend.direction, show_legend = TRUE, ...) {
    .anno_ggseries(
        x = x, split_by = split_by, group_by = group_by, column = column, title = title, which = which,
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
anno_simple <- function(x, split_by = NULL, group_by, column, title, which = "row", palette,
                        palcolor = NULL, border = TRUE, legend.direction, show_legend = TRUE, alpha = 1, ...) {
    if (!is.null(split_by)) {
        x <- do.call(rbind, split(x, x[[split_by]]))
    }
    is_cont <- is.numeric(x[[column]])
    if (isFALSE(is_cont) && !is.factor(x[[column]])) {
        x[[column]] <- factor(x[[column]], levels = unique(x[[column]]))
    }
    # add legend
    if (isTRUE(show_legend)) {
        if (is_cont) {
            col_fun <- colorRamp2(
                seq(min(x[[column]]), max(x[[column]]), length = 100),
                palette_this(palette = palette, palcolor = palcolor, alpha = alpha)
            )
            lgd <- ComplexHeatmap::Legend(
                title = title,
                col_fun = col_fun,
                border = TRUE, direction = legend.direction
            )
            anno <- ComplexHeatmap::anno_simple(x[[column]], col = col_fun, which = which, border = border, ...)
        } else {
            colors <- palette_this(levels(x[[column]]), palette = palette, palcolor = palcolor, alpha = alpha)
            lgd <- ComplexHeatmap::Legend(
                title = title,
                labels = levels(x[[column]]),
                legend_gp = gpar(fill = colors),
                border = TRUE, nrow = if (legend.direction == "horizontal") 1 else NULL
            )
            anno <- ComplexHeatmap::anno_simple(as.character(x[[column]]), col = colors, which = which, border = border, ...)
        }
    } else {
        lgd <- NULL
    }
    list(anno = anno, legend = lgd)
}

#' @rdname heatmap-anno
anno_points <- function(x, split_by = NULL, group_by, column, title, which = "row", palette,
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
anno_lines <- function(x, split_by = NULL, group_by, column, title, which = "row", palette,
                       palcolor = NULL, border = TRUE, legend.direction, show_legend = TRUE, alpha = 1, add_points = TRUE, ...) {
    anno <- ComplexHeatmap::anno_lines(
        x[[column]],
        which = which, border = border, add_points = add_points, ...
    )
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
#'  from the values in the cell or a function to calculate the size from the values in the cell.
#' @keywords internal
layer_dot <- function(j, i, x, y, w, h, fill, data, dot_size, alpha) {
    if (is.function(dot_size)) {
        s <- unlist(data[paste(i, j, sep = "-")])
        s <- scales::rescale(s, to = c(0.1, 1))
        grid.points(x, y,
            pch = 21, size = unit(10, "mm") * s,
            gp = gpar(col = "black", lwd = 1, fill = adjcolors(fill, alpha))
        )
    } else {
        grid.points(x, y,
            pch = 21, size = unit(unlist(dot_size), "mm"),
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

    for (m in seq_along(pies)) {
        pies[[m]]$grobs[[5]]$vp <- viewport(x = x[m], y = y[m], width = pie_sizes[m] * w[m], height = pie_sizes[m] * h[m])
        grid.draw(pies[[m]]$grobs[[5]])
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
    for (m in seq_along(vlnplots)) {
        wm <- if (flip) w[m] * 0.95 else w[m]
        hm <- if (flip) h[m] * 0.95 else h[m]
        vlnplots[[m]]$grobs[[5]]$vp <- viewport(x = x[m], y = y[m], width = wm, height = hm)
        grid.draw(vlnplots[[m]]$grobs[[5]])
    }
}

#' Atomic heatmap without split
#'
#' @inheritParams process_heatmap_data
#' @inheritParams common_args
#' @param data A data frame used to create the heatmap.
#'  The data should be in a long form where each row represents a instance in the heatmap.
#' @param values_fill A value to fill in the missing values in the heatmap.
#' When there is missing value in the data, the cluster_rows and cluster_columns will fail.
#' @param border A logical value indicating whether to draw the border of the heatmap.
#'  If TRUE, the borders of the slices will be also drawn.
#' @param title The global (column) title of the heatmap
#' @param lower_quantile,upper_quantile,lower_cutoff,upper_cutoff Vector of minimum and maximum cutoff values or quantile values for each feature.
#'  It's applied to aggregated values when aggregated values are used (e.g. plot_type tile, label, etc).
#'  It's applied to raw values when raw values are used (e.g. plot_type bars, etc).
#' @param rows_palette A character string specifying the palette of the row group annotation.
#'  The default is "Paired".
#' @param rows_palcolor A character vector of colors to override the palette of the row group annotation.
#' @param columns_palette A character string specifying the palette of the column group annotation.
#'  The default is "Paired".
#' @param columns_palcolor A character vector of colors to override the palette of the column group annotation.
#' @param columns_split_palette A character string specifying the palette of the column split annotation.
#'  The default is "simspec".
#' @param columns_split_palcolor A character vector of colors to override the palette of the column split annotation.
#' @param rows_split_palette A character string specifying the palette of the row split annotation.
#'  The default is "simspec".
#' @param rows_split_palcolor A character vector of colors to override the palette of the row split annotation.
#' @param cluster_columns A logical value indicating whether to cluster the columns.
#'  If TRUE and columns_split_by is provided, the clustering will only be applied to the columns within the same split.
#' @param cluster_rows A logical value indicating whether to cluster the rows.
#'  If TRUE and rows_split_by is provided, the clustering will only be applied to the rows within the same split.
#' @param legend_items A numeric vector with names to specifiy the items in the main legend.
#'  The names will be working as the labels of the legend items.
#' @param legend_discrete A logical value indicating whether the main legend is discrete.
#' @param show_row_names A logical value indicating whether to show the row names.
#'  If TRUE, the legend of the row group annotation will be hidden.
#' @param show_column_names A logical value indicating whether to show the column names.
#'  If TRUE, the legend of the column group annotation will be hidden.
#' @param column_title A character string/vector of the column name(s) to use as the title of the column group annotation.
#' @param row_title A character string/vector of the column name(s) to use as the title of the row group annotation.
#' @param na_col A character string specifying the color for missing values.
#'  The default is "grey85".
#' @param row_names_side A character string specifying the side of the row names.
#'  The default is "right".
#' @param column_names_side A character string specifying the side of the column names.
#'  The default is "bottom".
#' @param bars_sample An integer specifying the number of samples to draw the bars.
#' @param flip A logical value indicating whether to flip the heatmap.
#' @param pie_palette A character string specifying the palette of the pie chart.
#' @param pie_palcolor A character vector of colors to override the palette of the pie chart.
#' @param pie_values A function or character that can be converted to a function by [match.arg()]
#' to calculate the values for the pie chart. Default is "length".
#' The function should take a vector of values as the argument and return a single value, for each
#' group in `pie_group_by`.
#' @param pie_size A numeric value or a function specifying the size of the pie chart.
#'  If it is a function, the function should take `count` as the argument and return the size.
#' @param pie_size_name A character string specifying the name of the legend for the pie size.
#' @param label_size A numeric value specifying the size of the labels when `cell_type = "label"`.
#' @param label A function to calculate the labels for the heatmap cells.
#' It takes the aggregated values as the argument and returns the labels to be shown in the heatmap.
#' No labels will be shown for the NA the returned values.
#' @param layer_fun_callback A function to add additional layers to the heatmap.
#'  The function should have the following arguments: `j`, `i`, `x`, `y`, `w`, `h`, `fill`, `sr` and `sc`.
#'  Please also refer to the `layer_fun` argument in `ComplexHeatmap::Heatmap`.
#' @param cell_type A character string specifying the type of the heatmap cells.
#'  The default is values. Other options are "bars", "label", "dot", "violin", "boxplot".
#'  Note that for pie chart, the values under columns specified by `rows` will not be used directly. Instead, the values
#'  will just be counted in different `pie_group_by` groups. `NA` values will not be counted.
#' @param cell_agg A function to aggregate the values in the cell, for the cell type "tile" and "label".
#'  The default is `mean`.
#' @param add_bg A logical value indicating whether to add a background to the heatmap.
#'  Does not work with `cell_type = "bars"` or `cell_type = "tile"`.
#' @param bg_alpha A numeric value between 0 and 1 specifying the transparency of the background.
#' @param violin_fill A character vector of colors to override the fill color of the violin plot.
#'  If NULL, the fill color will be the same as the annotion.
#' @param boxplot_fill A character vector of colors to override the fill color of the boxplot.
#'  If NULL, the fill color will be the same as the annotion.
#' @param dot_size A numeric value specifying the size of the dot or a function to calculate the size
#'  from the values in the cell or a function to calculate the size from the values in the cell.
#' @param dot_size_name A character string specifying the name of the legend for the dot size.
#' If NULL, the dot size legend will not be shown.
#' @param column_name_annotation A logical value indicating whether to add the column annotation for the column names.
#'  which is a simple annotaion indicating the column names.
#' @param column_name_legend A logical value indicating whether to show the legend of the column name annotation.
#' @param column_annotation A character string/vector of the column name(s) to use as the column annotation.
#'  Or a list with the keys as the names of the annotation and the values as the column names.
#' @param column_annotation_side A character string specifying the side of the column annotation.
#'  Could be a list with the keys as the names of the annotation and the values as the sides.
#' @param column_annotation_palette A character string specifying the palette of the column annotation.
#'  The default is "Paired".
#'  Could be a list with the keys as the names of the annotation and the values as the palettes.
#' @param column_annotation_palcolor A character vector of colors to override the palette of the column annotation.
#'  Could be a list with the keys as the names of the annotation and the values as the palcolors.
#' @param column_annotation_type A character string specifying the type of the column annotation.
#'  The default is "auto". Other options are "simple", "pie", "ring", "bar", "violin", "boxplot", "density".
#'  Could be a list with the keys as the names of the annotation and the values as the types.
#'  If the type is "auto", the type will be determined by the type and number of the column data.
#' @param column_annotation_params A list of parameters passed to the annotation function.
#'  Could be a list with the keys as the names of the annotation and the values as the parameters.
#' @param column_annotation_agg A function to aggregate the values in the column annotation.
#' @param row_name_annotation A logical value indicating whether to add the row annotation for the row names.
#'  which is a simple annotaion indicating the row names.
#' @param row_name_legend A logical value indicating whether to show the legend of the row name annotation.
#' @param row_annotation A character string/vector of the column name(s) to use as the row annotation.
#' Or a list with the keys as the names of the annotation and the values as the column names.
#' @param row_annotation_side A character string specifying the side of the row annotation.
#' Could be a list with the keys as the names of the annotation and the values as the sides.
#' @param row_annotation_palette A character string specifying the palette of the row annotation.
#' The default is "Paired".
#' Could be a list with the keys as the names of the annotation and the values as the palettes.
#' @param row_annotation_palcolor A character vector of colors to override the palette of the row annotation.
#' Could be a list with the keys as the names of the annotation and the values as the palcolors.
#' @param row_annotation_type A character string specifying the type of the row annotation.
#' The default is "auto". Other options are "simple", "pie", "ring", "bar", "violin", "boxplot", "density".
#' Could be a list with the keys as the names of the annotation and the values as the types.
#' If the type is "auto", the type will be determined by the type and number of the row data.
#' @param row_annotation_params A list of parameters passed to the annotation function.
#' Could be a list with the keys as the names of the annotation and the values as the parameters.
#' @param row_annotation_agg A function to aggregate the values in the row annotation.
#' @param add_reticle A logical value indicating whether to add a reticle to the heatmap.
#' @param reticle_color A character string specifying the color of the reticle.
#' @param palette A character string specifying the palette of the heatmap cells.
#' @param palcolor A character vector of colors to override the palette of the heatmap cells.
#' @param alpha A numeric value between 0 and 1 specifying the transparency of the heatmap cells.
#' @param return_grob A logical value indicating whether to return the grob object of the heatmap.
#'  This is useful when merging multiple heatmaps using patchwork.
#' @param ... Other arguments passed to [ComplexHeatmap::Heatmap()]
#' When `row_names_max_width` is passed, a unit is expected. But you can also pass a numeric values,
#' with a default unit "inches", or a string like "5inches" to specify the number and unit directly.
#' @importFrom circlize colorRamp2
#' @importFrom dplyr group_by across ungroup %>% all_of summarise first slice_sample everything group_map
#' @importFrom tidyr pivot_longer pivot_wider unite expand_grid
#' @importFrom ggplot2 ggplotGrob theme_void
#' @importFrom grid grid.rect grid.text grid.lines grid.points viewport gpar unit grid.draw
#' @importFrom grid convertUnit grid.grabExpr
#' @return A drawn HeatmapList object if `return_grob = FALSE`. Otherwise, a grob/gTree object.
#' @keywords internal
HeatmapAtomic <- function(
    data, values_by, values_fill = NA,
    # data definition
    rows_by = NULL, rows_split_by = NULL,
    columns_by = NULL, columns_split_by = NULL,
    # palettes
    palette = "RdBu", palcolor = NULL,
    rows_palette = "Paired", rows_palcolor = NULL, rows_split_palette = "simspec", rows_split_palcolor = NULL,
    columns_palette = "Paired", columns_palcolor = NULL, columns_split_palette = "simspec", columns_split_palcolor = NULL,
    # cell_type: pies
    pie_size_name = "size", pie_size = NULL, pie_values = "length",
    pie_group_by = NULL, pie_palette = "Spectral", pie_palcolor = NULL,
    # cell_type: bars
    bars_sample = 100,
    # cell_type: label
    label = identity, label_size = 10,
    # cell_type: violin
    violin_fill = NULL,
    # cell_type: boxplot
    boxplot_fill = NULL,
    # cell_type: dot
    dot_size = 8, dot_size_name = "size",
    # legend
    legend_items = NULL, legend_discrete = FALSE,
    legend.position = "right", legend.direction = "vertical",
    # values
    lower_quantile = 0, upper_quantile = 0.99, lower_cutoff = NULL, upper_cutoff = NULL,
    # bg
    add_bg = FALSE, bg_alpha = 0.5,
    # reticle
    add_reticle = FALSE, reticle_color = "grey",
    # passed to ComplexHeatmap::Heatmap
    column_name_annotation = TRUE, column_name_legend = isFALSE(show_column_names) && !identical(legend.position, "none"),
    row_name_annotation = TRUE, row_name_legend = isFALSE(show_row_names) && !identical(legend.position, "none"),
    cluster_columns = TRUE, cluster_rows = TRUE, show_row_names = !row_name_annotation, show_column_names = !column_name_annotation,
    border = TRUE, title = NULL, column_title = character(0), row_title = character(0), na_col = "grey85",
    row_names_side = "right", column_names_side = "bottom",
    column_annotation = NULL, column_annotation_side = "top", column_annotation_palette = "Paired", column_annotation_palcolor = NULL,
    column_annotation_type = "auto", column_annotation_params = list(), column_annotation_agg = NULL,
    row_annotation = NULL, row_annotation_side = "left", row_annotation_palette = "Paired", row_annotation_palcolor = NULL,
    row_annotation_type = "auto", row_annotation_params = list(), row_annotation_agg = NULL,
    # misc
    flip = FALSE, alpha = 1, seed = 8525, return_grob = FALSE,
    # cell customization
    layer_fun_callback = NULL, cell_type = "tile", cell_agg = NULL,
    ...
) {
    # Data was validated in `plotthis::Heatmap()`

    # Convert to the format that ComplexHeatmap::Heatmap can understand
    if (isFALSE(column_title)) column_title <- NULL
    if (isFALSE(row_title)) row_title <- NULL
    if (isTRUE(column_title)) column_title <- character(0)
    if (isTRUE(row_title)) row_title <- character(0)

    get_col_fun <- function(lower, upper, a = alpha) {
        # If the lower and upper cutoff are the same, we need to adjust the upper cutoff
        if (upper == lower) {
            if (upper == 0) {
                upper <- 1e-3
            } else {
                upper <- upper + upper * 1e-3
            }
        }
        colorRamp2(
            seq(lower, upper, length = 100),
            palette_this(palette = palette, palcolor = palcolor, alpha = a, transparent = FALSE)
        )
    }

    # Initialize the heatmap arguments
    hmargs <- list(
        # name = name,   # error when name has irregular characters (e.g. "-")
        heatmap_legend_param = list(title = values_by),
        border = border, na_col = na_col,
        cluster_columns = cluster_columns, cluster_rows = cluster_rows,
        cluster_column_slices = FALSE, cluster_row_slices = FALSE, show_heatmap_legend = FALSE,
        show_row_names = show_row_names, show_column_names = show_column_names,
        row_names_side = row_names_side, column_names_side = column_names_side,
        column_title = column_title, row_title = row_title,
        ...
    )

    # Set the row_names_max_width based on the length of the row names
    if (!is.null(hmargs$row_names_max_width)) {
        if (is.character(hmargs$row_names_max_width)) {
            if (grepl("^[0-9]+(\\.[0-9]+)?$", hmargs$row_names_max_width)) {
                hmargs$row_names_max_width <- unit(as.numeric(hmargs$row_names_max_width), "inches")
            }
            stopifnot(
                "[Heatmap] 'row_names_max_width' should be in a format of '2inches' or '2cm' if given as a string." =
                grepl("^[0-9]+(\\.[0-9]+\\s*)?(npc|cm|centimetre|centimeter|in|inch|inches|mm|points|picas|bigpts|cicero|scalepts|lines|char|native|snpc|strwidth|strheight|grobwidth|grobheight|null)$", hmargs$row_names_max_width)
            )

            # convert to grid::unit
            hmargs$row_names_max_width <- unit(
                as.numeric(gsub("[^0-9.]", "", hmargs$row_names_max_width)),
                gsub("[0-9.]", "", hmargs$row_names_max_width)
            )
        } else if (is.numeric(hmargs$row_names_max_width)) {
            hmargs$row_names_max_width <- unit(hmargs$row_names_max_width, "inches")
        }
    } else if (flip) {
        hmargs$row_names_max_width <- ComplexHeatmap::max_text_width(levels(data[[columns_by]]))
    } else {
        hmargs$row_names_max_width <- ComplexHeatmap::max_text_width(levels(data[[rows_by]]))
    }

    # Collect the legends
    legends <- list()

    # Set the default cell aggregation function for pie chart (will be plotted as the background)
    cell_agg <- cell_agg %||% (
        if (cell_type == "pie") {
            function(x) sum(x, na.rm = TRUE)
        } else {
            function(x) mean(x, na.rm = TRUE)
        }
    )
    if (is.character(cell_agg)) {
        if (startsWith(cell_agg, "nan")) {
            fn <- match.fun(substring(cell_agg, 4))
            cell_agg <- function(x) fn(x[is.finite(x)])
        } else {
            cell_agg <- match.fun(cell_agg)
        }
    }

    # Extract the matrix for the heatmap (aggregated values, for e.g. tile, label, pie background, etc)
    # We also need it for bars, because ComplexHeatmap::Heatmap need the matrix to plot anyway
    # rows_split_by  rows_by  columns_split_by1::columns_by1 columns_split_by2::columns_by2 ...
    # rows_split_by1 rows_by1 0.1                            0.2                            ...
    # rows_split_by2 rows_by2 0.3                            0.4                            ...
    # ...
    # Make sure the rows/columns are in order
    data <- arrange(data, !!!syms(unique(c(rows_split_by, rows_by, columns_split_by, columns_by, pie_group_by))))
    hmargs$matrix <- data %>%
        group_by(!!!syms(unique(c(rows_split_by, rows_by, columns_split_by, columns_by))), .drop = FALSE) %>%
        summarise(.value = cell_agg(!!sym(values_by)), .groups = "drop") %>%
        unite(".columns", !!!syms(unique(c(columns_split_by, columns_by))), sep = " // ") %>%
        unite(".rows", !!!syms(unique(c(rows_split_by, rows_by))), sep = " // ") %>%
        pivot_wider(
            names_from = ".columns",
            values_from = ".value",
            values_fill = values_fill
        ) %>%
        as.data.frame()

    rownames(hmargs$matrix) <- hmargs$matrix$.rows
    hmargs$matrix$.rows <- NULL
    hmargs$matrix <- as.matrix(hmargs$matrix)
    hmargs$matrix[is.na(hmargs$matrix)] <- values_fill
    if (flip) {
        hmargs$matrix <- t(hmargs$matrix)
    }

    if (!is.null(rows_split_by)) {
        row_split_labels <- strsplit(rownames(hmargs$matrix), " // ", fixed = TRUE)
        hmargs$row_split <- sapply(row_split_labels, `[`, 1)
        hmargs$row_labels <- sapply(row_split_labels, `[`, 2)
    } else {
        hmargs$row_labels <- rownames(hmargs$matrix)
    }

    if (!is.null(columns_split_by)) {
        column_split_labels <- strsplit(colnames(hmargs$matrix), " // ", fixed = TRUE)
        hmargs$column_split <- sapply(column_split_labels, `[`, 1)
        hmargs$column_labels <- sapply(column_split_labels, `[`, 2)
    } else {
        hmargs$column_labels <- colnames(hmargs$matrix)
    }

    if (cell_type == "bars") { # where multiple values are used, operating on data
        lower_cutoff <- lower_cutoff %||% quantile(data[[values_by]][is.finite(data[[values_by]])], lower_quantile, na.rm = TRUE)
        upper_cutoff <- upper_cutoff %||% quantile(data[[values_by]][is.finite(data[[values_by]])], upper_quantile, na.rm = TRUE)
        data[[values_by]][data[[values_by]] < lower_cutoff] <- lower_cutoff
        data[[values_by]][data[[values_by]] > upper_cutoff] <- upper_cutoff
    } else { # where aggregated values are used
        lower_cutoff <- lower_cutoff %||% quantile(hmargs$matrix[is.finite(hmargs$matrix)], lower_quantile, na.rm = TRUE)
        upper_cutoff <- upper_cutoff %||% quantile(hmargs$matrix[is.finite(hmargs$matrix)], upper_quantile, na.rm = TRUE)
    }

    # Set the color function for the heatmap cells
    hmargs$col <- get_col_fun(lower_cutoff, upper_cutoff)

    # Indices for data in layer_fun
    indices <- if (flip) {
        # 1-1, 2-1, 1-2, 2-2, ...
        expand.grid(1:nrow(hmargs$matrix), 1:ncol(hmargs$matrix))
    } else {
        # 1-1, 1-2, 2-1, 2-2, ...
        expand_grid(1:nrow(hmargs$matrix), 1:ncol(hmargs$matrix))
    }
    indices <- paste(indices[[1]], indices[[2]], sep = "-")

    # Compose the main legend
    get_main_legend <- function(allow_discreate = TRUE) {
        if (identical(legend.position, "none")) {
            return(NULL)
        }
        if (!allow_discreate && isTRUE(legend_discrete)) {
            stop("[Heatmap] 'legend_discrete = TRUE' is not allowed.")
        }

        if (isTRUE(legend_discrete)) {
            if (is.null(legend_items)) {
                lgd_items <- sort(unique(as.vector(hmargs$matrix)), decreasing = TRUE)
                names(lgd_items) <- as.character(lgd_items)
            } else {
                lgd_items <- unlist(legend_items)
            }
            ComplexHeatmap::Legend(
                title = values_by, at = lgd_items, labels = names(lgd_items),
                legend_gp = grid::gpar(fill = hmargs$col(lgd_items)), border = TRUE, direction = legend.direction
            )
        } else {
            ComplexHeatmap::Legend(title = values_by, col_fun = hmargs$col, border = TRUE, direction = legend.direction)
        }
    }

    nrow_multiplier <- ncol_multiplier <- 1
    if (cell_type == "pie") {
        if (is.null(pie_group_by)) {
            stop("[Heatmap] Please provide 'pie_group_by' to use 'cell_type = 'pie'.")
        }
        pie_values <- pie_values %||% "length"
        if (is.character(pie_values)) {
            if (startsWith(pie_values, "nan")) {
                fn <- match.fun(substring(pie_values, 4))
                pie_values <- function(x) fn(x[is.finite(x)])
            } else {
                pie_values <- match.fun(pie_values)
            }
        }

        pie_group_levels <- levels(data[[pie_group_by]])
        pie_data <- data %>%
            group_by(!!!syms(unique(c(rows_split_by, rows_by, columns_split_by, columns_by, pie_group_by))), .drop = FALSE) %>%
            summarise(.value = pie_values(!!sym(values_by)), .groups = "drop") %>%
            group_by(!!!syms(unique(c(rows_split_by, rows_by, columns_split_by, columns_by)))) %>%
            group_map(
                ~ data.frame(Var = .x[[pie_group_by]], Freq = .x$.value)
            )
        names(pie_data) <- indices

        pie_colors <- palette_this(pie_group_levels, palette = pie_palette, palcolor = pie_palcolor)
        if (is.character((pie_size))) {
            if (startsWith(pie_size, "nan")) {
                fn <- match.fun(substring(pie_size, 4))
                pie_size <- function(x) fn(x[is.finite(x)])
            } else {
                pie_size <- match.fun(pie_size)
            }
        }
        hmargs$layer_fun <- function(j, i, x, y, w, h, fill, sr, sc) {
            layer_white_bg(j, i, x, y, w, h, fill)
            if (isTRUE(add_bg)) {
                layer_bg(j, x, x, y, w, h, fill, alpha = bg_alpha)
            }
            if (isTRUE(add_reticle)) {
                layer_reticle(j, i, x, y, w, h, fill, color = reticle_color)
            }
            layer_pie(
                j, i, x, y, w, h, fill,
                palette = pie_palette, palcolor = pie_palcolor, data = pie_data, pie_size = pie_size
            )
            if (is.function(layer_fun_callback)) {
                layer_fun_callback(j, i, x, y, w, h, fill, sr, sc)
            }
        }

        if (!identical(legend.position, "none") && is.function(pie_size)) {
            pie_sizes <- sapply(pie_data, function(d) pie_size(sum(d$Freq, na.rm = TRUE)))
            pie_size_min <- min(pie_sizes, na.rm = TRUE)
            pie_size_max <- max(pie_sizes, na.rm = TRUE)
            legends$.pie_size <- ComplexHeatmap::Legend(
                title = pie_size_name,
                labels = scales::number((seq(pie_size_min, pie_size_max, length.out = ifelse(pie_size_max > pie_size_min, 4, 1)))),
                type = "points",
                pch = 21,
                size = unit(8, "mm") * seq(0.2, 1, length.out = 4),
                grid_height = unit(8, "mm") * seq(0.2, 1, length.out = 4) * 0.8,
                grid_width = unit(8, "mm"),
                legend_gp = gpar(fill = "grey30"),
                border = FALSE,
                background = "transparent",
                direction = legend.direction
            )
        }
        if (isTRUE(add_bg)) {
            legends$.heatmap <- get_main_legend()
        }
        if (!identical(legend.position, "none")) {
            legends$.pie <- ComplexHeatmap::Legend(
                title = pie_group_by, direction = legend.direction,
                border = TRUE, labels = pie_group_levels, legend_gp = gpar(fill = pie_colors)
            )
        }
        nrow_multiplier <- ifelse(flip, 6, 4)
        ncol_multiplier <- ifelse(flip, 4, 6)
    }
    else if (cell_type == "bars") {
        if (isTRUE(add_bg)) {
            stop("[Heatmap] Cannot use 'add_bg' with 'cell_type = 'bars'.")
        }
        if (isTRUE(add_reticle)) {
            stop("[Heatmap] Cannot use 'add_reticle' with 'cell_type = 'bars'.")
        }

        bars_data <- data %>%
            group_by(!!!syms(unique(c(rows_split_by, rows_by, columns_split_by, columns_by))), .drop = FALSE) %>%
            group_map(~ .x[[values_by]])

        names(bars_data) <- indices

        # plot bars in each cell
        hmargs$layer_fun <- function(j, i, x, y, w, h, fill, sr, sc) {
            layer_white_bg(j, i, x, y, w, h, fill)
            layer_bars(
                j, i, x, y, w, h, fill, flip = flip,
                col_fun = hmargs$col, data = bars_data, alpha = alpha
            )
            if (is.function(layer_fun_callback)) {
                layer_fun_callback(j, i, x, y, w, h, fill, sr, sc)
            }
        }
        # Override the main legend
        legends$.heatmap <- get_main_legend(FALSE)
        nrow_multiplier <- 0.5
    }
    else if (cell_type == "dot") {
        if (is.character(dot_size)) {
            if (startsWith(dot_size, "nan")) {
                fn <- match.fun(substring(dot_size, 4))
                dot_size <- function(x) fn(x[is.finite(x)])
            } else {
                dot_size <- match.fun(dot_size)
            }
        }
        dot_data <- data %>%
            group_by(!!!syms(unique(c(rows_split_by, rows_by, columns_split_by, columns_by))), .drop = FALSE) %>%
            group_map(~ if (is.function(dot_size)) dot_size(.x[[values_by]]) else dot_size)

        names(dot_data) <- indices

        if (!identical(legend.position, "none") && is.function(dot_size) && !is.null(dot_size_name)) {
            dot_size_min <- min(unlist(dot_data), na.rm = TRUE)
            dot_size_max <- max(unlist(dot_data), na.rm = TRUE)
            legends$.dot_size <- ComplexHeatmap::Legend(
                title = dot_size_name,
                labels = scales::number((seq(dot_size_min, dot_size_max, length.out = ifelse(dot_size_max > dot_size_min, 4, 1)))),
                type = "points",
                pch = 21,
                size = unit(8, "mm") * seq(0.2, 1, length.out = 4),
                grid_height = unit(8, "mm") * seq(0.2, 1, length.out = 4) * 0.8,
                grid_width = unit(8, "mm"),
                legend_gp = gpar(fill = "grey30"),
                border = FALSE,
                background = "transparent",
                direction = legend.direction
            )
        }
        hmargs$layer_fun <- function(j, i, x, y, w, h, fill, sr, sc) {
            layer_white_bg(j, i, x, y, w, h, fill)
            if (isTRUE(add_bg)) {
                layer_bg(j, i, x, y, w, h, fill, alpha = bg_alpha)
            }
            if (isTRUE(add_reticle)) {
                layer_reticle(j, i, x, y, w, h, fill, color = reticle_color)
            }
            layer_dot(
                j, i, x, y, w, h, fill,
                data = dot_data, dot_size = dot_size, alpha = alpha
            )
            if (is.function(layer_fun_callback)) {
                layer_fun_callback(j, i, x, y, w, h, fill, sr, sc)
            }
        }
        # Override the main legend
        legends$.heatmap <- get_main_legend()
    }
    else if (cell_type %in% c("violin", "boxplot")) {
        # df with multiple values in each cell
        vdata <- data %>%
            group_by(!!!syms(unique(c(rows_split_by, rows_by, columns_split_by, columns_by))), .drop = FALSE) %>%
            group_map(~ .x[[values_by]])

        names(vdata) <- indices
        vcolors <- if (cell_type == "violin") violin_fill else boxplot_fill

        hmargs$layer_fun <- function(j, i, x, y, w, h, fill, sr, sc) {
            layer_white_bg(j, i, x, y, w, h, fill)
            if (isTRUE(add_bg)) {
                layer_bg(j, i, x, y, w, h, fill, alpha = bg_alpha)
            }
            if (isTRUE(add_reticle)) {
                layer_reticle(j, i, x, y, w, h, fill, color = reticle_color)
            }
            layer_fn <- if (cell_type == "violin") ViolinPlot else BoxPlot
            layer_boxviolin(
                j, i, x, y, w, h, fill,
                flip = flip, data = vdata, colors = vcolors, fn = layer_fn
            )
            if (is.function(layer_fun_callback)) {
                layer_fun_callback(j, i, x, y, w, h, fill, sr, sc)
            }
        }
        if (!identical(legend.position, "none")) {
            if (!is.null(legend_items)) {
                stop("[Heatmap] Cannot use 'legend_items' with 'cell_type = 'violin' or 'boxplot'.")
            }
            if (isTRUE(legend_discrete)) {
                stop("[Heatmap] Cannot use 'legend_discrete = TRUE' with 'cell_type = 'violin' or 'boxplot'.")
            }
            if (is.null(vcolors)) {
                legends$.heatmap <- ComplexHeatmap::Legend(
                    title = values_by, col_fun = hmargs$col, border = TRUE,
                    direction = legend.direction
                )
            } else if (isTRUE(add_bg)) {
                legends$.heatmap <- ComplexHeatmap::Legend(
                    title = values_by, col_fun = get_col_fun(lower_cutoff, upper_cutoff, bg_alpha),
                    border = TRUE, direction = legend.direction
                )
            }
        }
    }
    else if (cell_type == "tile") {
        if (isTRUE(add_bg)) {
            stop("[Heatmap] Cannot use 'add_bg' with 'cell_type = 'tile'.")
        }
        if (isTRUE(add_reticle)) {
            stop("[Heatmap] Cannot use 'add_reticle' with 'cell_type = 'tile'.")
        }
        hmargs$rect_gp <- gpar(col = "grey80", lwd = 0.1)
        hmargs$layer_fun <- layer_fun_callback
        legends$.heatmap <- get_main_legend()
    }
    else if (cell_type == "label") {
        if (isTRUE(add_bg)) {
            stop("[Heatmap] Cannot use 'add_bg' with 'cell_type = 'tile'.")
        }
        if (isTRUE(add_reticle)) {
            stop("[Heatmap] Cannot use 'add_reticle' with 'cell_type = 'tile'.")
        }
        hmargs$layer_fun <- function(j, i, x, y, w, h, fill, sr, sc) {
            labels <- ComplexHeatmap::pindex(hmargs$matrix, i, j)
            if (is.function(label)) {
                labels <- label(labels)
            }
            inds <- !is.na(labels)
            if (any(inds)) {
                theta <- seq(pi / 8, 2 * pi, length.out = 16)
                lapply(theta, function(a) {
                    x_out <- x[inds] + unit(cos(a) * label_size / 30, "mm")
                    y_out <- y[inds] + unit(sin(a) * label_size / 30, "mm")
                    grid.text(labels[inds], x = x_out, y = y_out, gp = gpar(fontsize = label_size, col = "white"))
                })
                grid.text(labels[inds], x[inds], y[inds], gp = gpar(fontsize = label_size, col = "black"))
            }
            if (is.function(layer_fun_callback)) {
                layer_fun_callback(j, i, x, y, w, h, fill, sr, sc)
            }
        }
        legends$.heatmap <- get_main_legend()
    }

    ncols <- nlevels(data[[columns_by]])
    if (!is.null(columns_split_by)) {
        ncols <- ncols * nlevels(data[[columns_split_by]])
    }
    ncols <- ncols * ncol_multiplier
    nrows <- nlevels(data[[rows_by]])
    if (!is.null(rows_split_by)) {
        nrows <- nrows * nlevels(data[[rows_split_by]])
    }
    nrows <- nrows * nrow_multiplier

    ## Set up the top annotations
    setup_name_annos <- function(
        names_side, anno_title,
        split_by, splits, split_palette, split_palcolor,
        by, by_name_annotation, by_labels, by_palette, by_palcolor, by_name_legend
    ) {
        annos <- list(
            border = TRUE, col = list(),
            annotation_name_side = names_side,
            show_annotation_name = list(), show_legend = FALSE
        )

        if (!is.null(split_by)) {
            annos[[split_by]] <- splits
            annos$col[[split_by]] <- palette_this(
                unique(splits), palette = split_palette, palcolor = split_palcolor
            )
            annos$show_annotation_name[[split_by]] <- TRUE
            if (!isTRUE(anno_title) && !identical(legend.position, "none")) {
                legends[[paste0("name.", split_by)]] <<- ComplexHeatmap::Legend(
                    title = split_by,
                    labels = unique(splits),
                    legend_gp = gpar(fill = annos$col[[split_by]]),
                    border = TRUE, nrow = if (legend.direction == "horizontal") 1 else NULL
                )
            }
        }

        if (!is.null(by) && by_name_annotation) {
            annos[[by]] <- by_labels
            annos$col[[by]] <- palette_this(
                unique(by_labels), palette = by_palette, palcolor = by_palcolor
            )
            annos$show_annotation_name[[by]] <- TRUE

            if (isTRUE(by_name_legend)) {
                legends[[paste0("name.", by)]] <<- ComplexHeatmap::Legend(
                    title = by,
                    labels = unique(by_labels),
                    legend_gp = gpar(fill = annos$col[[by]]),
                    border = TRUE, nrow = if (legend.direction == "horizontal") 1 else NULL
                )
            }
        }

        return(annos)
    }

    setup_annos <- function(
        which, names_side,
        annotation, annotation_type,
        annotation_palette, annotation_palcolor,
        annotation_agg, annotation_params,
        split_by, by
    ) {
        annos <- list()
        if (!is.list(annotation)) {
            annotation <- as.list(annotation)
            names(annotation) <- unlist(annotation)
        }
        if (is.character(annotation_type) && identical(annotation_type, "auto")) {
            annotation_type <- as.list(rep("auto", length(annotation)))
            names(annotation_type) <- names(annotation)
        }
        if (is.character(annotation_palette) && length(annotation_palette) == 1) {
            annotation_palette <- as.list(rep(annotation_palette, length(annotation)))
            names(annotation_palette) <- names(annotation)
        }
        if (!is.list(annotation_palcolor)) {
            annotation_palcolor <- list(annotation_palcolor)
            annotation_palcolor <- rep(annotation_palcolor, length(annotation))
            names(annotation_palcolor) <- names(annotation)
        }
        annotation_agg <- annotation_agg %||% list()
        annotation_params <- annotation_params %||% list()
        for (aname in names(annotation)) {
            annocol <- annotation[[aname]]
            annoagg <- annotation_agg[[aname]]
            annotype <- annotation_type[[aname]] %||% "auto"
            param <- annotation_params[[aname]] %||% list()
            annodata <- param$x %||% data # TODO: order in param$x
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
            param$which <- ifelse(flip, setdiff(c("column", "row"), which), which)
            param$palette <- annotation_palette[[aname]] %||% "Paired"
            param$palcolor <- annotation_palcolor[[aname]]
            param$legend.direction <- legend.direction
            # swap width and height if flip is TRUE
            if (flip) {
                pheight <- param$height
                param$height <- param$width
                param$width <- pheight
            }
            if (legend.position == "none") {
                param$show_legend <- FALSE
            }
            if (!exists(paste0("anno_", annotype))) {
                stop("[Heatmap] Unsupported annotation type: ", annotype)
            }
            anno <- do.call(paste0("anno_", annotype), param)
            annos[[aname]] <- anno$anno
            legends[[paste0(which, ".", aname)]] <<- anno$legend
        }

        if (length(annos) > 0) {
            annos$annotation_name_side <- names_side
        }

        return(annos)
    }

    ncol_annos <- sum(cluster_columns, show_column_names) * 4
    ncol_annos <- ncol_annos +
        ifelse(is.null(columns_split_by), 0, 1) +
        ifelse(is.null(columns_by) || !column_name_annotation, 0, 1)
    top_annos <- setup_name_annos(
        names_side = ifelse(flip, column_names_side, row_names_side), anno_title = column_title,
        split_by = columns_split_by, splits = if (flip) hmargs$row_split else hmargs$column_split,
        split_palette = columns_split_palette, split_palcolor = columns_split_palcolor,
        by = columns_by, by_name_annotation = column_name_annotation,
        by_labels = if (flip) hmargs$row_labels else hmargs$column_labels, by_palette = columns_palette,
        by_palcolor = columns_palcolor, by_name_legend = column_name_legend
    )
    column_annos <- setup_annos(
        which = "column", names_side = ifelse(flip, column_names_side, row_names_side),
        annotation = column_annotation, annotation_type = column_annotation_type,
        annotation_palette = column_annotation_palette, annotation_palcolor = column_annotation_palcolor,
        annotation_agg = column_annotation_agg, annotation_params = column_annotation_params,
        split_by = columns_split_by, by = columns_by
    )

    if (column_annotation_side == "top") {
        column_annos$annotation_name_side <- NULL
        top_annos <- c(top_annos, column_annos)
    } else if (length(column_annos) > 0) {
        if (isTRUE(flip)) {
            hmargs$right_annotation <- do.call(ComplexHeatmap::rowAnnotation, column_annos)
        } else {
            hmargs$bottom_annotation <- do.call(ComplexHeatmap::HeatmapAnnotation, column_annos)
        }
        rm(column_annos)
    }
    if (length(top_annos$show_annotation_name) > 0) {
        if (isTRUE(flip)) {
            hmargs$left_annotation <- do.call(ComplexHeatmap::rowAnnotation, top_annos)
        } else {
            hmargs$top_annotation <- do.call(ComplexHeatmap::HeatmapAnnotation, top_annos)
        }
    }
    rm(top_annos)

    nrow_annos <- sum(cluster_rows, show_row_names) * 4
    nrow_annos <- nrow_annos +
        ifelse(is.null(rows_split_by), 0, 1) +
        ifelse(is.null(rows_by) || !row_name_annotation, 0, 1)
    left_annos <- setup_name_annos(
        names_side = ifelse(flip, row_names_side, column_names_side), anno_title = row_title,
        split_by = rows_split_by, splits = if (flip) hmargs$column_split else hmargs$row_split,
        split_palette = rows_split_palette, split_palcolor = rows_split_palcolor,
        by = rows_by, by_name_annotation = row_name_annotation,
        by_labels = if (flip) hmargs$column_labels else hmargs$row_labels,
        by_palette = rows_palette, by_palcolor = rows_palcolor, by_name_legend = row_name_legend
    )
    row_annos <- setup_annos(
        which = "row", names_side = ifelse(flip, row_names_side, column_names_side),
        annotation = row_annotation, annotation_type = row_annotation_type,
        annotation_palette = row_annotation_palette, annotation_palcolor = row_annotation_palcolor,
        annotation_agg = row_annotation_agg, annotation_params = row_annotation_params,
        split_by = rows_split_by, by = rows_by
    )

    if (row_annotation_side == "left") {
        row_annos$annotation_name_side <- NULL
        left_annos <- c(left_annos, row_annos)
    } else if (length(row_annos) > 0) {
        if (isTRUE(flip)) {
            hmargs$bottom_annotation <- do.call(ComplexHeatmap::HeatmapAnnotation, row_annos)
        } else {
            hmargs$right_annotation <- do.call(ComplexHeatmap::rowAnnotation, row_annos)
        }
        rm(row_annos)
    }
    if (length(left_annos$show_annotation_name) > 0) {
        if (isTRUE(flip)) {
            hmargs$top_annotation <- do.call(ComplexHeatmap::HeatmapAnnotation, left_annos)
        } else {
            hmargs$left_annotation <- do.call(ComplexHeatmap::rowAnnotation, left_annos)
        }
    }
    rm(left_annos)

    ## Set up the heatmap
    rownames_width <- convertUnit(hmargs$row_names_max_width, "inches", valueOnly = TRUE) * 0.5 - 0.2
    rownames_width <- max(rownames_width, 0)
    if (isTRUE(flip)) {
        width <- nrows * 0.25 + ncol_annos * 0.5 + rownames_width
        # How about column name length (nchars)?
        height <- ncols * 0.25 + nrow_annos * 0.5
    } else {
        width <- ncols * 0.25 + nrow_annos * 0.5 + rownames_width
        height <- nrows * 0.25 + ncol_annos * 0.5
    }
    if (cell_type == "pie") {
        width <- max(width, height)
        height <- max(width, height)
    }
    if (!identical(legend.position, "none")) {
        if (legend.position %in% c("right", "left")) {
            if (legend.direction == "horizontal") {
                width <- width + 3
            } else {
                width <- width + 1.5
            }
        } else if (legend.direction == "horizontal") {
            height <- height + 3
        } else {
            height <- height + 1.5
        }
    }

    p <- do.call(ComplexHeatmap::Heatmap, hmargs)
    if (isTRUE(return_grob)) {
        if (identical(legend.position, "none")) {
            p <- grid.grabExpr(ComplexHeatmap::draw(p,
                annotation_legend_list = legends,
                show_annotation_legend = FALSE, column_title = title
            ))
        } else {
            p <- grid.grabExpr(ComplexHeatmap::draw(p,
                annotation_legend_list = legends,
                annotation_legend_side = legend.position, column_title = title
            ))
        }
    } else {
        if (identical(legend.position, "none")) {
            p <- ComplexHeatmap::draw(p,
                annotation_legend_list = legends,
                show_annotation_legend = FALSE, column_title = title
            )
        } else {
            p <- ComplexHeatmap::draw(p,
                annotation_legend_list = legends,
                annotation_legend_side = legend.position, column_title = title
            )
        }
    }

    attr(p, "height") <- max(height, 4)
    attr(p, "width") <- max(width, 4)
    p
}

#' Heatmap
#'
#' @description Heatmap is a popular way to visualize data in matrix format. It is widely used in biology to visualize gene expression data in microarray and RNA-seq data. The heatmap is a matrix where rows represent the samples and columns represent the features. The color of each cell represents the value of the feature in the sample. The color can be continuous or discrete. The heatmap can be split by the columns or rows to show the subgroups in the data. The heatmap can also be annotated by the columns or rows to show the additional information of the samples or features.
#' @inheritParams process_heatmap_data
#' @inheritParams HeatmapAtomic
#' @inheritParams common_args
#' @export
#' @importFrom patchwork wrap_plots
#' @seealso \code{\link{anno_simple}}, \code{\link{anno_points}}, \code{\link{anno_lines}}, \code{\link{anno_pie}}, \code{\link{anno_violin}}, \code{\link{anno_boxplot}}, \code{\link{anno_density}}
#' @examples
#' \donttest{
#' set.seed(8525)
#'
#' matrix_data <- matrix(rnorm(60), nrow = 6, ncol = 10)
#' rownames(matrix_data) <- paste0("R", 1:6)
#' colnames(matrix_data) <- paste0("C", 1:10)
#' if (requireNamespace("cluster", quietly = TRUE)) {
#'     Heatmap(matrix_data)
#' }
#' if (requireNamespace("cluster", quietly = TRUE)) {
#'     # use a different color palette
#'     # change the main legend title
#'     # show row names (legend will be hidden)
#'     # show column names
#'     # change the row name annotation name and side
#'     # change the column name annotation name
#'     Heatmap(matrix_data, palette = "viridis", values_by = "z-score",
#'        show_row_names = TRUE, show_column_names = TRUE,
#'        rows_name = "Features", row_names_side = "left",
#'        columns_name = "Samples")
#' }
#' if (requireNamespace("cluster", quietly = TRUE)) {
#'     # flip the heatmap
#'     Heatmap(matrix_data, palette = "viridis", values_by = "z-score",
#'        show_row_names = TRUE, show_column_names = TRUE,
#'        rows_name = "Features", row_names_side = "left",
#'        columns_name = "Samples", flip = TRUE)
#' }
#' if (requireNamespace("cluster", quietly = TRUE)) {
#'     # add annotations to the heatmap
#'     rows_data <- data.frame(
#'        rows = paste0("R", 1:6),
#'        group = sample(c("X", "Y", "Z"), 6, replace = TRUE)
#'     )
#'     Heatmap(matrix_data, rows_data = rows_data,
#'         row_annotation = list(Group = "group"),
#'         row_annotation_type = list(Group = "simple"),
#'         row_annotation_palette = list(Group = "Spectral")
#'     )
#' }
#' if (requireNamespace("cluster", quietly = TRUE)) {
#'     Heatmap(matrix_data, rows_data = rows_data,
#'         rows_split_by = "group"
#'     )
#' }
#' if (requireNamespace("cluster", quietly = TRUE)) {
#'     # add labels to the heatmap
#'     Heatmap(matrix_data, rows_data = rows_data,
#'         rows_split_by = "group", cell_type = "label",
#'         label = function(x) ifelse(
#'             x > 0, scales::number(x, accuracy = 0.01), NA
#'         )
#'     )
#' }
#' if (requireNamespace("cluster", quietly = TRUE)) {
#'     # quickly simulate a GO board
#'     go <- matrix(sample(c(0, 1, NA), 81, replace = TRUE), ncol = 9)
#'
#'     Heatmap(
#'         go,
#'         # Do not cluster rows and columns and hide the annotations
#'         cluster_rows = FALSE, cluster_columns = FALSE,
#'         row_name_annotation = FALSE, column_name_annotation = FALSE,
#'         show_row_names = FALSE, show_column_names = FALSE,
#'         # Set the legend items
#'         values_by = "Players", legend_discrete = TRUE,
#'         legend_items = c("Player 1" = 0, "Player 2" = 1),
#'         # Set the pawns
#'         cell_type = "dot", dot_size = function(x) ifelse(is.na(x), 0, 1),
#'         dot_size_name = NULL,  # hide the dot size legend
#'         palcolor = c("white", "black"),
#'         # Set the board
#'         add_reticle = TRUE,
#'         # Set the size of the board
#'         width = ggplot2::unit(105, "mm"), height = ggplot2::unit(105, "mm"))
#' }
#'
#' # Use long form data
#' N <- 500
#' data <- data.frame(
#'     value = rnorm(N),
#'     c = sample(letters[1:8], N, replace = TRUE),
#'     r = sample(LETTERS[1:5], N, replace = TRUE),
#'     p = sample(c("x", "y"), N, replace = TRUE),
#'     q = sample(c("X", "Y", "Z"), N, replace = TRUE),
#'     a = as.character(sample(1:5, N, replace = TRUE)),
#'     p1 = runif(N),
#'     p2 = runif(N)
#' )
#'
#' if (requireNamespace("cluster", quietly = TRUE)) {
#'     Heatmap(data, rows_by = "r", columns_by = "c", values_by = "value",
#'         rows_split_by = "p", columns_split_by = "q", show_column_names = TRUE)
#' }
#' if (requireNamespace("cluster", quietly = TRUE)) {
#'     # split into multiple heatmaps
#'     Heatmap(data,
#'         values_by = "value", columns_by = "c", rows_by = "r", split_by = "p",
#'         upper_cutoff = 2, lower_cutoff = -2, legend.position = c("none", "right"),
#'         design = "AAAAAA#BBBBBBB"
#'     )
#' }
#' if (requireNamespace("cluster", quietly = TRUE)) {
#'     # cell_type = "bars" (default is "tile")
#'     Heatmap(data, values_by = "value", rows_by = "r", columns_by = "c",
#'         cell_type = "bars")
#' }
#' if (requireNamespace("cluster", quietly = TRUE)) {
#'     Heatmap(data, values_by = "value", rows_by = "r", columns_by = "c",
#'         cell_type = "dot", dot_size = length, dot_size_name = "data points",
#'         add_bg = TRUE, add_reticle = TRUE)
#' }
#' if (requireNamespace("cluster", quietly = TRUE)) {
#'     Heatmap(data, values_by = "value", rows_by = "r", columns_by = "c",
#'         cell_type = "pie", pie_group_by = "q", pie_size = sqrt,
#'         add_bg = TRUE, add_reticle = TRUE)
#' }
#' if (requireNamespace("cluster", quietly = TRUE)) {
#'     Heatmap(data, values_by = "value", rows_by = "r", columns_by = "c",
#'         cell_type = "violin", add_bg = TRUE, add_reticle = TRUE)
#' }
#' if (requireNamespace("cluster", quietly = TRUE)) {
#'     Heatmap(data, values_by = "value", rows_by = "r", columns_by = "c",
#'         cell_type = "boxplot", add_bg = TRUE, add_reticle = TRUE)
#' }
#' if (requireNamespace("cluster", quietly = TRUE)) {
#'     Heatmap(data,
#'         values_by = "value", rows_by = "r", columns_by = "c",
#'         column_annotation = list(r1 = "p", r2 = "q", r3 = "p1"),
#'         column_annotation_type = list(r1 = "ring", r2 = "bar", r3 = "violin"),
#'         column_annotation_params = list(
#'             r1 = list(height = grid::unit(10, "mm"), show_legend = FALSE),
#'             r3 = list(height = grid::unit(18, "mm"))
#'         ),
#'         row_annotation = c("q", "p2", "a"),
#'         row_annotation_side = "right",
#'         row_annotation_type = list(q = "pie", p2 = "density", a = "simple"),
#'         row_annotation_params = list(q = list(width = grid::unit(12, "mm"))),
#'         show_row_names = TRUE, show_column_names = TRUE
#'     )
#' }
#' if (requireNamespace("cluster", quietly = TRUE)) {
#'     Heatmap(data,
#'         values_by = "value", rows_by = "r", columns_by = "c",
#'         split_by = "p", palette = list(x = "Reds", y = "Blues")
#'     )
#' }
#' if (requireNamespace("cluster", quietly = TRUE)) {
#'     # implies in_form = "wide-rows"
#'     Heatmap(data, rows_by = c("p1", "p2"), columns_by = "c")
#' }
#' if (requireNamespace("cluster", quietly = TRUE)) {
#'     # implies wide-columns
#'     Heatmap(data, rows_by = "r", columns_by = c("p1", "p2"))
#' }
#' }
Heatmap <- function(
    data, values_by = NULL, values_fill = NA, name = NULL,
    # data definition
    in_form = c("auto", "matrix", "wide-columns", "wide-rows", "long"),
    split_by = NULL, split_by_sep = "_",
    rows_by = NULL, rows_by_sep = "_", rows_split_by = NULL, rows_split_by_sep = "_",
    columns_by = NULL, columns_by_sep = "_", columns_split_by = NULL, columns_split_by_sep = "_",
    rows_data = NULL, columns_data = NULL,
    # names
    columns_name = NULL, columns_split_name = NULL,
    rows_name = NULL, rows_split_name = NULL,
    # palettes
    palette = "RdBu", palcolor = NULL,
    rows_palette = "Paired", rows_palcolor = NULL, rows_split_palette = "simspec", rows_split_palcolor = NULL,
    columns_palette = "Paired", columns_palcolor = NULL, columns_split_palette = "simspec", columns_split_palcolor = NULL,
    # cell_type: pies
    pie_size_name = "size", pie_size = NULL, pie_values = "length", pie_name = NULL,
    pie_group_by = NULL, pie_group_by_sep = "_", pie_palette = "Spectral", pie_palcolor = NULL,
    # cell_type: bars
    bars_sample = 100,
    # cell_type: label
    label = identity, label_size = 10,
    # cell_type: violin
    violin_fill = NULL,
    # cell_type: boxplot
    boxplot_fill = NULL,
    # cell_type: dot
    dot_size = 8, dot_size_name = "size",
    # legend
    legend_items = NULL, legend_discrete = FALSE,
    legend.position = "right", legend.direction = "vertical",
    # values
    lower_quantile = 0, upper_quantile = 0.99, lower_cutoff = NULL, upper_cutoff = NULL,
    # bg
    add_bg = FALSE, bg_alpha = 0.5,
    # reticle
    add_reticle = FALSE, reticle_color = "grey",
    # passed to ComplexHeatmap::Heatmap
    column_name_annotation = TRUE, column_name_legend = isFALSE(show_column_names) && !identical(legend.position, "none"),
    row_name_annotation = TRUE, row_name_legend = isFALSE(show_row_names) && !identical(legend.position, "none"),
    cluster_columns = TRUE, cluster_rows = TRUE, show_row_names = !row_name_annotation, show_column_names = !column_name_annotation,
    border = TRUE, title = NULL, column_title = character(0), row_title = character(0), na_col = "grey85",
    row_names_side = "right", column_names_side = "bottom",
    column_annotation = NULL, column_annotation_side = "top", column_annotation_palette = "Paired", column_annotation_palcolor = NULL,
    column_annotation_type = "auto", column_annotation_params = list(), column_annotation_agg = NULL,
    row_annotation = NULL, row_annotation_side = "left", row_annotation_palette = "Paired", row_annotation_palcolor = NULL,
    row_annotation_type = "auto", row_annotation_params = list(), row_annotation_agg = NULL,
    # misc
    flip = FALSE, alpha = 1, seed = 8525,
    # cell customization
    layer_fun_callback = NULL, cell_type = c("tile", "bars", "label", "dot", "violin", "boxplot", "pie"), cell_agg = NULL,
    # subplots
    combine = TRUE, nrow = NULL, ncol = NULL, byrow = TRUE, axes = NULL, axis_titles = axes, guides = NULL, design = NULL,
    ...
) {
    validate_common_args(seed)
    in_form <- match.arg(in_form)
    cell_type <- match.arg(cell_type)

    hmdata <- process_heatmap_data(
        data, in_form = in_form, values_by = values_by, name = name,
        split_by = split_by, split_by_sep = split_by_sep,
        rows_by = rows_by, rows_by_sep = rows_by_sep, rows_name = rows_name,
        rows_split_by = rows_split_by, rows_split_by_sep = rows_split_by_sep, rows_split_name = rows_split_name,
        columns_by = columns_by, columns_by_sep = columns_by_sep, columns_name = columns_name,
        columns_split_by = columns_split_by, columns_split_by_sep = columns_split_by_sep, columns_split_name = columns_split_name,
        pie_group_by = pie_group_by, pie_group_by_sep = pie_group_by_sep, pie_name = pie_name,
        rows_data = rows_data, columns_data = columns_data
    )

    palette <- check_palette(palette, names(hmdata$data))
    palcolor <- check_palcolor(palcolor, names(hmdata$data))
    rows_palette <- check_palette(rows_palette, names(hmdata$data))
    rows_palcolor <- check_palcolor(rows_palcolor, names(hmdata$data))
    rows_split_palette <- check_palette(rows_split_palette, names(hmdata$data))
    rows_split_palcolor <- check_palcolor(rows_split_palcolor, names(hmdata$data))
    columns_palette <- check_palette(columns_palette, names(hmdata$data))
    columns_palcolor <- check_palcolor(columns_palcolor, names(hmdata$data))
    columns_split_palette <- check_palette(columns_split_palette, names(hmdata$data))
    columns_split_palcolor <- check_palcolor(columns_split_palcolor, names(hmdata$data))
    pie_palette <- check_palette(pie_palette, names(hmdata$data))
    pie_palcolor <- check_palcolor(pie_palcolor, names(hmdata$data))
    legend.direction <- check_legend(legend.direction, names(hmdata$data), "legend.direction")
    legend.position <- check_legend(legend.position, names(hmdata$data), "legend.position")

    plots <- lapply(
        names(hmdata$data), function(nm) {
            default_title <- if (length(hmdata$data) == 1 && identical(nm, "...")) NULL else nm
            if (is.function(title)) {
                title <- title(default_title)
            } else {
                title <- title %||% default_title
            }

            HeatmapAtomic(
                data = hmdata$data[[nm]], values_by = hmdata$values_by, values_fill = values_fill,
                rows_by = hmdata$rows_by, rows_split_by = hmdata$rows_split_by,
                columns_by = hmdata$columns_by, columns_split_by = hmdata$columns_split_by,

                palette = palette[[nm]], palcolor = palcolor[[nm]],
                rows_palette = rows_palette[[nm]], rows_palcolor = rows_palcolor[[nm]],
                rows_split_palette = rows_split_palette[[nm]], rows_split_palcolor = rows_split_palcolor[[nm]],
                columns_palette = columns_palette[[nm]], columns_palcolor = columns_palcolor[[nm]],
                columns_split_palette = columns_split_palette[[nm]], columns_split_palcolor = columns_split_palcolor[[nm]],

                pie_size_name = pie_size_name, pie_size = pie_size, pie_values = pie_values,
                pie_group_by = hmdata$pie_group_by, pie_palette = pie_palette[[nm]], pie_palcolor = pie_palcolor[[nm]],

                bars_sample = bars_sample,
                label = label, label_size = label_size,
                violin_fill = violin_fill,
                boxplot_fill = boxplot_fill,
                dot_size = dot_size, dot_size_name = dot_size_name,

                legend_items = legend_items, legend_discrete = legend_discrete,
                legend.position = legend.position[[nm]], legend.direction = legend.direction[[nm]],

                lower_quantile = lower_quantile, upper_quantile = upper_quantile,
                lower_cutoff = lower_cutoff, upper_cutoff = upper_cutoff,

                add_bg = add_bg, bg_alpha = bg_alpha,

                add_reticle = add_reticle, reticle_color = reticle_color,

                column_name_annotation = column_name_annotation, column_name_legend = column_name_legend,
                row_name_annotation = row_name_annotation, row_name_legend = row_name_legend,
                cluster_columns = cluster_columns, cluster_rows = cluster_rows, show_row_names = show_row_names, show_column_names = show_column_names,
                border = border, title = title, column_title = column_title, row_title = row_title, na_col = na_col,
                row_names_side = row_names_side, column_names_side = column_names_side,
                column_annotation = column_annotation, column_annotation_side = column_annotation_side,
                column_annotation_palette = column_annotation_palette, column_annotation_palcolor = column_annotation_palcolor,
                column_annotation_type = column_annotation_type, column_annotation_params = column_annotation_params,
                column_annotation_agg = column_annotation_agg,
                row_annotation = row_annotation, row_annotation_side = row_annotation_side,
                row_annotation_palette = row_annotation_palette, row_annotation_palcolor = row_annotation_palcolor,
                row_annotation_type = row_annotation_type, row_annotation_params = row_annotation_params,
                row_annotation_agg = row_annotation_agg,

                flip = flip, alpha = alpha, seed = seed, return_grob = TRUE,
                layer_fun_callback = layer_fun_callback, cell_type = cell_type, cell_agg = cell_agg,

                ...
            )
        }
    )

    combine_plots(plots,
        combine = combine, nrow = nrow, ncol = ncol, byrow = byrow,
        axes = axes, axis_titles = axis_titles, guides = guides, design = design
    )
}
