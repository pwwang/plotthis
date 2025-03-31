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
    if (isTRUE(void)) { p <- p + theme_void() }
    if (isTRUE(nolegend)) { p <- p + theme(legend.position = "none") }
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
    if (!is.factor(x[[column]])) {
        x[[column]] <- factor(x[[column]], levels = unique(x[[column]]))
    }
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
            grobs <- grobs[index]
            total <- length(index)
            # draw border
            grid.lines(x = c(0, 0), y = c(0, 1), gp = gpar(col = "black", lwd = 1))
            grid.lines(x = c(1, 1), y = c(0, 1), gp = gpar(col = "black", lwd = 1))
            grid.lines(x = c(0, 1), y = c(0, 0), gp = gpar(col = "black", lwd = 1))
            grid.lines(x = c(0, 1), y = c(1, 1), gp = gpar(col = "black", lwd = 1))
            for (i in seq_along(grobs)) {
                if (which == "row") {
                    grobs[[i]]$vp <- viewport(x = 0.5, y = (i - 1) * 1/total + 1/(2*total), width = 0.95, height = 1/total)
                } else {
                    grobs[[i]]$vp <- viewport(x = (i - 1) * 1/total + 1/(2*total), y = 0.5, width = 1/total, height = 1)
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
        plots <- .plotting(data = x, column = column, group_by = "..split", palette = palette, palcolor = colors)
        plots <- lapply(plots, gggrob)
    } else {
        plots <- .plotting(data = x, column = column, group_by = group_by, palette = palette, palcolor = colors)
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
            grobs <- grobs[index]
            total <- length(index)
            # draw border
            grid.lines(x = c(0, 0), y = c(0, 1), gp = gpar(col = "black", lwd = 1))
            grid.lines(x = c(1, 1), y = c(0, 1), gp = gpar(col = "black", lwd = 1))
            grid.lines(x = c(0, 1), y = c(0, 0), gp = gpar(col = "black", lwd = 1))
            grid.lines(x = c(0, 1), y = c(1, 1), gp = gpar(col = "black", lwd = 1))
            for (i in seq_along(grobs)) {
                if (which == "row") {
                    grobs[[i]]$vp <- viewport(x = 0.5, y = (i - 1) * 1/total + 1/(2*total), width = 0.95, height = 1/total)
                } else {
                    grobs[[i]]$vp <- viewport(x = (i - 1) * 1/total + 1/(2*total), y = 0.5, width = 1/total, height = 0.95)
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
            BarPlot(data, x = column, split_by = group_by, expand = c(0.05, 1),
                    palette = palette, palcolor = palcolor, combine = FALSE)
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
            ViolinPlot(data, x = ".x", y = column, split_by = group_by, combine = FALSE,
                       palette = palette, palcolor = palcolor, flip = which == "row")
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
            BoxPlot(data, x = ".x", y = column, split_by = group_by, combine = FALSE,
                    palette = palette, palcolor = palcolor, flip = which == "row")
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
            DensityPlot(data, x = column, split_by = group_by, combine = FALSE,
                       palette = palette, palcolor = palcolor, flip = which == "row")
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
        x[[column]], which = which, border = border, ...)
    list(anno = anno, legend = NULL)
}

#' @rdname heatmap-anno
#' @param add_points A logical value indicating whether to add points to the annotation
anno_lines <- function(x, split_by = NULL, group_by, column, title, which = "row", palette,
                        palcolor = NULL, border = TRUE, legend.direction, show_legend = TRUE, alpha = 1, add_points = TRUE, ...) {
    anno <- ComplexHeatmap::anno_lines(
        x[[column]], which = which, border = border, add_points = add_points, ...)
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
#' @param hmdf A dataframe used to create the annotation. Different from the data used to
#'  create the heatmap itself, which is aggregated data. This dataframe is the original data,
#'  where each cell could have multiple values.
#' @param dot_size A numeric value specifying the size of the dot or a function to calculate the size
#'  from the values in the cell
#' @keywords internal
layer_dot <- function(j, i, x, y, w, h, fill, hmdf, dot_size, alpha) {
    if (is.function(dot_size)) {
        s <- ComplexHeatmap::pindex(hmdf, i, j)
        s <- scales::rescale(s, to = c(0.2, 1))
        grid.points(x, y,
            pch = 21, size = unit(10, "mm") * s,
            gp = gpar(col = "black", lwd = 1, fill = adjcolors(fill, alpha))
        )
    } else {
        grid.points(x, y,
            pch = 21, size = unit(dot_size, "mm"),
            gp = gpar(col = "black", lwd = 1, fill = adjcolors(fill, alpha))
        )
    }
}

#' @rdname heatmap-layer
#' @param col_fun A function to calculate the color of the bars
#' @keywords internal
layer_bars <- function(j, i, x, y, w, h, fill, col_fun, hmdf, alpha) {
    # colors be like [1,1.9]: '#A6CEE3' (1.9,2.8]: '#1F78B4' (2.8,3.7]: '#B2DF8A'
    bdata <- ComplexHeatmap::pindex(hmdf, i, j)
    if (!is.null(names(bdata))) {  # flip = TRUE
        bdata <- unlist(bdata, recursive = FALSE)
    }
    ns <- lengths(bdata)
    # rep(w / ns, ns) can't keep the unit
    bw <- rep(sapply(seq_along(w), function(m) w[m] / ns[m]), ns)
    bx <- unlist(lapply(seq_along(x), function(m) {
        x[m] - w[m] / 2 + seq_along(bdata[[m]]) * w[m] / length(bdata[[m]])
    })) - bw / 2
    by <- rep(y, lengths(bdata))
    bh <- rep(h, lengths(bdata))
    bf <- unlist(lapply(bdata, col_fun))
    grid.rect(x = bx, y = by, width = bw, height = bh, gp = gpar(fill = bf, col = "transparent"))
}

#' @rdname heatmap-layer
#' @keywords internal
layer_pie <- function(j, i, x, y, w, h, fill, palette, palcolor, hmdf, pie_size) {
    pdata <- ComplexHeatmap::pindex(hmdf, i, j)  # not j, i
    if (!is.null(names(pdata))) {  # flip = TRUE
        pdata <- unlist(pdata, recursive = FALSE)
    }
    pies <- lapply(pdata, function(pd) {
        # [1] "table"
        # X Y Z
        # 9 2 4
        pd <- as.data.frame(pd)
        p <- PieChart(pd, x = "Var1", y = "Freq", label = NULL, palette = palette, palcolor = palcolor)
        ggplotGrob(p + theme_void() + theme(legend.position = "none"))
    })

    if (!is.function(pie_size)) {
        pie_sizes <- rep(pie_size %||% 1, length(pies))
    } else {
        pie_sizes <- sapply(pdata, function(pd) pie_size(sum(pd)))
        pie_sizes <- scales::rescale(pie_sizes, to = c(0.2, 1))
    }

    for (m in seq_along(pies)) {
        pies[[m]]$grobs[[5]]$vp <- viewport(x = x[m], y = y[m], width = pie_sizes[m] * w[m], height = pie_sizes[m] * h[m])
        grid.draw(pies[[m]]$grobs[[5]])
    }
}


#' @rdname heatmap-layer
#' @param violin_fill A character vector specifying the fill color of the violin plot.
#'  If not provided, the fill color of row/column annotation will be used
#' @keywords internal
layer_violin <- function(j, i, x, y, w, h, fill, hmdf, violin_fill) {
    vlndata <- ComplexHeatmap::pindex(hmdf, i, j)
    if (!is.null(names(vlndata))) {  # flip = TRUE
        vlndata <- unlist(vlndata, recursive = FALSE)
    }
    vlnplots <- lapply(seq_along(vlndata), function(m) {
        p <- ViolinPlot(data.frame(x = 1, y = vlndata[[m]]), x = "x", y = "y", palcolor = violin_fill %||% fill[m])
        ggplotGrob(p + theme_void() + theme(legend.position = "none"))
    })
    for (m in seq_along(vlnplots)) {
        vlnplots[[m]]$grobs[[5]]$vp <- viewport(x = x[m], y = y[m], width = w[m], height = h[m] * 0.95)
        grid.draw(vlnplots[[m]]$grobs[[5]])
    }
}

#' @rdname heatmap-layer
#' @param boxplot_fill A character vector specifying the fill color of the boxplot.
#'  If not provided, the fill color of row/column annotation will be used
#' @keywords internal
layer_boxplot <- function(j, i, x, y, w, h, fill, hmdf, boxplot_fill) {
    bpdata <- ComplexHeatmap::pindex(hmdf, i, j)
    if (!is.null(names(bpdata))) {  # flip = TRUE
        bpdata <- unlist(bpdata, recursive = FALSE)
    }
    bpplots <- lapply(seq_along(bpdata), function(m) {
        p <- BoxPlot(data.frame(x = 1, y = bpdata[[m]]), x = "x", y = "y", palcolor = boxplot_fill %||% fill[m])
        ggplotGrob(p + theme_void() + theme(legend.position = "none"))
    })
    for (m in seq_along(bpplots)) {
        bpplots[[m]]$grobs[[5]]$vp <- viewport(x = x[m], y = y[m], width = w[m], height = h[m] * 0.95)
        grid.draw(bpplots[[m]]$grobs[[5]])
    }
}

#' Atomic heatmap
#'
#' @inheritParams common_args
#' @param data A data frame used to create the heatmap.
#'  The data should be in a long form where each row represents a instance in the heatmap.
#'  The `rows` should be multiple columns if you want to plot as rows, which you can refer as "features".
#' @param rows A character string/vector of the column name(s) to plot for the rows
#'  Multiple columns in the data frame can be used as the rows.
#' @param rows_name A character string specifying the name of rows, which will be shown in the
#'  row group annotation and the legend of it.
#' @param columns_by A character string of the column name to plot for the columns
#'  A character/factor column is expected.
#'  Multiple columns are also supported. If so, `NA`s can be used to exclude rows in that group.
#'  If there is a single value in each group (other than `NA`), the group name, rather than the value,
#'  will be shown in the column group annotation.
#' @param columns_name A character string specifying the name of columns, which will be shown in the
#'  column group annotation and the legend of it.
#'  Only used when `columns_by` has multiple columns.
#' @param name A character string specifying the name of the main legend of the heatmap
#' @param border A logical value indicating whether to draw the border of the heatmap.
#'  If TRUE, the borders of the slices will be also drawn.
#' @param title The global (column) title of the heatmap
#' @param lower_quantile,upper_quantile,lower_cutoff,upper_cutoff Vector of minimum and maximum cutoff values or quantile values for each feature.
#'  It's applied to aggregated values when aggregated values are used (e.g. plot_type tile, label, etc).
#'  It's applied to raw values when raw values are used (e.g. plot_type bars, etc).
#' @param rows_palette A character string specifying the palette of the row group annotation.
#'  The default is "Paired".
#' @param rows_palcolor A character vector of colors to override the palette of the row group annotation.
#' @param columns_by_sep A character string to concatenate the columns in `columns_by` if there are multiple columns.
#' @param columns_split_by A character string of the column name to split the heatmap columns into slices.
#'  A character/factor column or multiple columns are expected.
#' @param columns_split_name A character string specifying the name of the column split annotation.
#' @param columns_palette A character string specifying the palette of the column group annotation.
#'  The default is "Paired".
#' @param columns_palcolor A character vector of colors to override the palette of the column group annotation.
#' @param columns_split_by_sep A character string to concatenate the columns in `columns_split_by` if there are multiple columns.
#' @param columns_split_palette A character string specifying the palette of the column split annotation.
#'  The default is "simspec".
#' @param columns_split_palcolor A character vector of colors to override the palette of the column split annotation.
#' @param rows_data A character string of the column name to use as the data for the row group annotation.
#'  If it starts with "@", it will be treated as an attribute of the data.
#' @param rows_split_by A character string of the column name to split the heatmap rows into slices.
#'  A character/factor column or multiple columns are expected.
#' @param rows_split_by_sep A character string to concatenate the columns in `rows_split_by` if there are multiple columns.
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
#' @param pie_group_by A character string of the column name to group the data for the pie chart.
#' @param pie_group_by_sep A character string to concatenate the columns in `pie_group_by` if there are multiple columns.
#' @param pie_palette A character string specifying the palette of the pie chart.
#' @param pie_palcolor A character vector of colors to override the palette of the pie chart.
#' @param pie_values How to calculate the values for the pie chart. Default is "count".
#'  Other options are "value" and a function.
#'  * "count": Count the number of instances in `pie_group_by`.
#'  * "value": Use the values in the columns specified by `rows` as the values.
#'    If there are multiple values for a single group in `pie_group_by`, a function should be provided to aggregate the values.
#'    If not, the sum of the values will be used and a warning will be shown.
#'  * A function: The function should take a vector of values as the argument and return a single value, for each
#'    group in `pie_group_by`.
#' @param pie_size A numeric value or a function specifying the size of the pie chart.
#'  If it is a function, the function should take `count` as the argument and return the size.
#' @param pie_name A character string specifying the name of the legend for the pie chart.
#' @param pie_size_name A character string specifying the name of the legend for the pie size.
#' @param label_size A numeric value specifying the size of the labels when `cell_type = "label"`.
#' @param label_cutoff A numeric value specifying the cutoff to show the labels when `cell_type = "label"`.
#' @param label_accuracy A numeric value specifying the accuracy of the labels when `cell_type = "label"`.
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
#'  from the values in the cell.
#' @param dot_size_name A character string specifying the name of the legend for the dot size.
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
#' @importFrom circlize colorRamp2
#' @importFrom dplyr group_by across ungroup %>% all_of summarise first slice_sample everything
#' @importFrom tidyr pivot_longer
#' @importFrom ggplot2 ggplotGrob theme_void
#' @importFrom grid grid.rect grid.text grid.lines grid.points viewport gpar unit grid.draw grid.grabExpr
#' @return A drawn HeatmapList object if `return_grob = FALSE`. Otherwise, a grob/gTree object.
#' @keywords internal
HeatmapAtomic <- function(
    data, rows, columns_by, rows_name = "rows", rows_split_name = "rows_split", columns_name = "columns", name = "value",
    border = TRUE, rows_palette = "Paired", rows_palcolor = NULL, pie_group_by = NULL, pie_group_by_sep = "_",
    pie_palette = "Spectral", pie_palcolor = NULL, pie_size = NULL, pie_name = NULL, pie_size_name = "size", pie_values = "count",
    columns_by_sep = "_", columns_split_by = NULL, columns_split_name = NULL, columns_palette = "Paired", columns_palcolor = NULL,
    columns_split_by_sep = "_", columns_split_palette = "simspec", columns_split_palcolor = NULL,
    lower_quantile = 0, upper_quantile = 0.99, lower_cutoff = NULL, upper_cutoff = NULL,
    rows_data = NULL, rows_split_by = NULL, rows_split_by_sep = "_", rows_split_palette = "simspec", rows_split_palcolor = NULL,
    column_name_annotation = TRUE, column_name_legend = isFALSE(show_column_names) && !identical(legend.position, "none"),
    row_name_annotation = TRUE, row_name_legend = isFALSE(show_row_names) && !identical(legend.position, "none"),
    cluster_columns = TRUE, cluster_rows = TRUE, legend_items = NULL, legend_discrete = FALSE,
    show_row_names = !row_name_annotation, show_column_names = !column_name_annotation,
    column_title = character(0), row_title = character(0), na_col = "grey85", title = NULL,
    row_names_side = "right", column_names_side = "bottom", bars_sample = 100, flip = FALSE,
    label_size = 10, label_cutoff = NULL, label_accuracy = 0.01, layer_fun_callback = NULL,
    cell_type = c("tile", "bars", "label", "dot", "violin", "boxplot", "pie"), cell_agg = mean,
    add_bg = FALSE, bg_alpha = 0.5, violin_fill = NULL, boxplot_fill = NULL, dot_size = 8, dot_size_name = "size",
    column_annotation = NULL, column_annotation_side = "top", column_annotation_palette = "Paired", column_annotation_palcolor = NULL,
    column_annotation_type = "auto", column_annotation_params = list(), column_annotation_agg = NULL,
    row_annotation = NULL, row_annotation_side = "left", row_annotation_palette = "Paired", row_annotation_palcolor = NULL,
    row_annotation_type = "auto", row_annotation_params = list(), row_annotation_agg = NULL,
    add_reticle = FALSE, reticle_color = "grey", return_grob = FALSE,
    palette = "RdBu", palcolor = NULL, alpha = 1, legend.position = "right", legend.direction = "vertical",
    ...) {
    if (is.character(rows_data) && length(rows_data) == 1 && startsWith(rows_data, "@")) {
        rows_data <- attr(data, substring(rows_data, 2))
    }
    if (is.list(rows)) {
        if (!is.null(rows_split_by)) {
            stop("When a list is provided for 'rows', 'rows_split_by' should not be provided. The rows will be split by the names of the list.")
        }
        if (is.null(names(rows))) {
            stop("When a list is provided for 'rows', the names of the list should be provided.")
        }
        rs <- lapply(names(rows), function(rn) rep(rn, length(rows[[rn]])))
        rows <- unname(unlist(rows))
        rd <- data.frame(.rows = rows, rows_split = unlist(rs))
        colnames(rd)[2] <- rows_split_name
        if (!is.null(rows_data)) {
            # Add the names of the list to the rows_data, with rows_split_name
            colnames(rows_data)[1] <- ".rows"
            rows_data <- merge(rows_data, rd, by = ".rows", all.x = TRUE)
        } else {
            rows_data <- rd
        }
        rows_split_by <- rows_split_name
    }

    rows <- check_columns(data, rows, allow_multi = TRUE)
    columns_by <- check_columns(data, columns_by, force_factor = TRUE, allow_multi = TRUE)
    pie_group_by <- check_columns(data, pie_group_by, force_factor = TRUE, allow_multi = TRUE,
        concat_multi = TRUE, concat_sep = pie_group_by_sep)
    for (column_by in columns_by) {
        data[[column_by]] <- droplevels(data[[column_by]])
    }

    if (length(columns_by) > 1) {
        if (!is.null(columns_split_by)) {
            stop("Cannot use 'columns_split_by' with multiple columns in 'columns_by'. The heatmap columns will be split by the multiple 'columns_by'.")
        }
        pval_col <- paste(columns_by, collapse = columns_by_sep)
        data <- data %>% pivot_longer(columns_by, names_to = columns_name, values_to = pval_col)
        data <- data[!is.na(data[[pval_col]]), , drop = FALSE]
        data[[columns_name]] <- factor(data[[columns_name]], levels = columns_by)

        df_splits <- data %>% distinct(!!sym(columns_name), !!sym(pval_col))
        if (all(table(df_splits[[columns_name]]) == 1)) {
            columns_split_by <- NULL
            columns_by <- columns_name
        } else {
            columns_split_by <- columns_name
            columns_by <- pval_col
        }
    }

    columns_split_by <- check_columns(data, columns_split_by,
        force_factor = TRUE, allow_multi = TRUE,
        concat_multi = TRUE, concat_sep = columns_split_by_sep
    )
    if (!is.null(columns_split_by)) {
        data <- data[order(data[[columns_split_by]], data[[columns_by]]), , drop = FALSE]
    } else {
        data <- data[order(data[[columns_by]]), , drop = FALSE]
    }

    cell_type <- match.arg(cell_type)
    if (isFALSE(column_title)) column_title <- NULL
    if (isFALSE(row_title)) row_title <- NULL
    if (isTRUE(column_title)) column_title <- character(0)
    if (isTRUE(row_title)) row_title <- character(0)

    # limits <- limits %||% quantile(data[, rows], c(0.01, 0.99), na.rm = TRUE)
    get_col_fun <- function(lower, upper, a = alpha) {
        colorRamp2(
            seq(lower, upper, length = 100),
            palette_this(palette = palette, palcolor = palcolor, alpha = a, transparent = FALSE)
        )
    }
    ## Initialize the heatmap arguments
    hmargs <- list(
        name = name, border = border, na_col = na_col,
        cluster_columns = cluster_columns, cluster_rows = cluster_rows,
        cluster_column_slices = FALSE, cluster_row_slices = FALSE, show_heatmap_legend = FALSE,
        show_row_names = show_row_names, show_column_names = show_column_names,
        row_names_side = row_names_side, column_names_side = column_names_side,
        column_title = column_title, row_title = row_title,
        ...
    )

    legends <- list()
    if (cell_type == "pie") { cell_agg = function(x) sum(!is.na(x)) }
    hmargs$matrix <- data %>%
        group_by(!!!syms(unique(c(columns_by, columns_split_by)))) %>%
        summarise(across(all_of(rows), ~ cell_agg(.x))) %>%
        ungroup() %>%
        as.data.frame()

    if (!is.null(columns_split_by)) {
        hmargs$matrix <- hmargs$matrix[order(hmargs$matrix[[columns_split_by]], hmargs$matrix[[columns_by]]), , drop = FALSE]
    } else {
        hmargs$matrix <- hmargs$matrix[order(hmargs$matrix[[columns_by]]), , drop = FALSE]
    }

    if (cell_type != "bars") {  # where aggregated values are used
        values <- as.matrix(hmargs$matrix[, rows])
        lower_cutoff <- lower_cutoff %||% quantile(values[is.finite(values)], lower_quantile, na.rm = TRUE)
        upper_cutoff <- upper_cutoff %||% quantile(values[is.finite(values)], upper_quantile, na.rm = TRUE)
    } else { # where multiple values are used
        values <- as.matrix(data[, rows])
        lower_cutoff <- lower_cutoff %||% quantile(values[is.finite(values)], lower_quantile, na.rm = TRUE)
        upper_cutoff <- upper_cutoff %||% quantile(values[is.finite(values)], upper_quantile, na.rm = TRUE)
        values[values < lower_cutoff] <- lower_cutoff
        values[values > upper_cutoff] <- upper_cutoff
        data[, rows] <- values
    }
    if (upper_cutoff == lower_cutoff) {
        if (upper_cutoff == 0) {
            upper_cutoff <- 1e-3
        } else {
            upper_cutoff <- upper_cutoff + upper_cutoff * 1e-3
        }
    }
    hmargs$col <- get_col_fun(lower_cutoff, upper_cutoff)

    get_main_legend <- function(allow_discreate = TRUE) {
        if (identical(legend.position, "none")) {
            return(NULL)
        }
        if (!allow_discreate && isTRUE(legend_discrete)) {
            stop("'legend_discrete = TRUE' is not allowed.")
        }

        if (isTRUE(legend_discrete)) {
            if (is.null(legend_items)) {
                lgd_items <- sort(unique(as.vector(as.matrix(hmargs$matrix[, unlist(rows)]))), decreasing = TRUE)
                names(lgd_items) <- as.character(lgd_items)
            } else {
                lgd_items <- unlist(legend_items)
            }
            ComplexHeatmap::Legend(
                title = name, at = lgd_items, labels = names(lgd_items),
                legend_gp = grid::gpar(fill = hmargs$col(lgd_items)), border = TRUE, direction = legend.direction)
        } else {
            ComplexHeatmap::Legend(title = name, col_fun = hmargs$col, border = TRUE, direction = legend.direction)
        }
    }

    nrow_multiplier <- ncol_multiplier <- 1
    if (cell_type == "pie") {
        if (is.null(pie_group_by)) {
            stop("Please provide 'pie_group_by' to use 'cell_type = 'pie'.")
        }

        pie_group_levels <- levels(data[[pie_group_by]])
        hmdf <- data %>%
            select(!!!syms(unique(c(columns_by, columns_split_by, pie_group_by, rows)))) %>%
            group_by(!!!syms(unique(c(columns_by, columns_split_by)))) %>%
            summarise(across(
                all_of(rows),
                function(.x, .g) {
                    if (identical(pie_values, "count")) {
                        list(table(.g[!is.na(.x)]))
                    } else {
                        .d <- lapply(split(.x[!is.na(.x)], .g[!is.na(.x)]), function(.v) {
                            if (length(.v) > 1) {
                                if (is.function(pie_values)) {
                                    pie_values(.v)
                                } else {
                                    warning("Multiple values in a group in 'pie_group_by'. Using the sum of the values.")
                                    sum(.v)
                                }
                            } else {
                                .v
                            }
                        })
                        list(as.table(unlist(.d)))
                    }
                },
                !!sym(pie_group_by)
            ), .groups = "drop")
        # match the dimensions of the heatmap matrix
        if (!is.null(columns_split_by)) hmdf[[columns_split_by]] <- NULL
        if (!is.null(columns_by)) hmdf[[columns_by]] <- NULL
        if (!flip) hmdf <- t(hmdf)

        pie_colors <- palette_this(levels(data[[pie_group_by]]), palette = pie_palette, palcolor = pie_palcolor)
        hmargs$layer_fun <- function(j, i, x, y, w, h, fill, sr, sc) {
            layer_white_bg(j, i, x, y, w, h, fill)
            if (isTRUE(add_bg)) {
                layer_bg(j, x, x, y, w, h, fill, alpha = bg_alpha)
            }
            if (isTRUE(add_reticle)) {
                layer_reticle(j, i, x, y, w, h, fill, color = reticle_color)
            }
            layer_pie(j, i, x, y, w, h, fill, palette = pie_palette, palcolor = pie_palcolor, hmdf, pie_size)
            if (is.function(layer_fun_callback)) {
                layer_fun_callback(j, i, x, y, w, h, fill, sr, sc)
            }
        }

        if (!identical(legend.position, "none") && is.function(pie_size)) {
            pie_size_min <- min(hmargs$matrix[, rows], na.rm = TRUE)
            pie_size_max <- max(hmargs$matrix[, rows], na.rm = TRUE)
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
        if (isTRUE(add_bg)) { legends$.heatmap <- get_main_legend() }
        if (!identical(legend.position, "none")) {
            legends$.pie <- ComplexHeatmap::Legend(title = pie_name %||% pie_group_by, direction = legend.direction,
                border = TRUE, labels = levels(data[[pie_group_by]]), legend_gp = gpar(fill = pie_colors))
        }
        nrow_multiplier <- ifelse(flip, 6, 4)
        ncol_multilier <- ifelse(flip, 4, 6)
    } else if (cell_type == "bars") {
        if (isTRUE(add_bg)) {
            stop("Cannot use 'add_bg' with 'cell_type = 'bars'.")
        }
        if (isTRUE(add_reticle)) {
            stop("Cannot use 'add_reticle' with 'cell_type = 'bars'.")
        }

        hmdf <- data %>%
            select(!!!syms(unique(c(columns_by, columns_split_by, rows)))) %>%
            group_by(!!!syms(unique(c(columns_by, columns_split_by)))) %>%
            slice_sample(n = bars_sample) %>%
            summarise(across(all_of(rows), ~ list(.x)), .groups = "drop")

        if (!is.null(columns_split_by)) hmdf[[columns_split_by]] <- NULL
        if (!is.null(columns_by)) hmdf[[columns_by]] <- NULL
        if (!flip) hmdf <- t(hmdf)
        # plot bars in each cell
        hmargs$layer_fun <- function(j, i, x, y, w, h, fill, sr, sc) {
            layer_white_bg(j, i, x, y, w, h, fill)
            layer_bars(j, i, x, y, w, h, fill, col_fun = hmargs$col, hmdf = hmdf, alpha = alpha)
            if (is.function(layer_fun_callback)) {
                layer_fun_callback(j, i, x, y, w, h, fill, sr, sc)
            }
        }
        legends$.heatmap <- get_main_legend(FALSE)
        nrow_multiplier <- 0.5
    } else if (cell_type == "dot") {
        if (is.function(dot_size)) {
            hmdf <- data %>%
                group_by(!!!syms(unique(c(columns_by, columns_split_by)))) %>%
                summarise(across(all_of(rows), ~ dot_size(.x)), .groups = "drop")
        } else {
            hmdf <- data %>%
                group_by(!!!syms(unique(c(columns_by, columns_split_by)))) %>%
                summarise(across(all_of(rows), ~ dot_size), .groups = "drop")
        }
        if (!is.null(columns_by)) hmdf[[columns_by]] <- NULL
        if (!is.null(columns_split_by)) hmdf[[columns_split_by]] <- NULL
        hmdf <- as.matrix(hmdf)
        if (!flip) hmdf <- t(hmdf)

        if (!identical(legend.position, "none") && is.function(dot_size)) {
            dot_size_min <- min(hmdf, na.rm = TRUE)
            dot_size_max <- max(hmdf, na.rm = TRUE)
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
            layer_dot(j, i, x, y, w, h, fill, hmdf = hmdf, dot_size = dot_size, alpha = alpha)
            if (is.function(layer_fun_callback)) {
                layer_fun_callback(j, i, x, y, w, h, fill, sr, sc)
            }
        }
        legends$.heatmap <- get_main_legend()
    } else if (cell_type %in% c("violin", "boxplot")) {
        # df with multiple values in each cell
        hmdf <- data %>%
            group_by(!!!syms(unique(c(columns_by, columns_split_by)))) %>%
            summarise(across(all_of(rows), ~ list(.x)), .groups = "drop")
        if (!is.null(columns_by)) hmdf[[columns_by]] <- NULL
        if (!is.null(columns_split_by)) hmdf[[columns_split_by]] <- NULL
        if (!flip) hmdf <- t(hmdf)

        hmargs$layer_fun <- function(j, i, x, y, w, h, fill, sr, sc) {
            layer_white_bg(j, i, x, y, w, h, fill)
            if (isTRUE(add_bg)) {
                layer_bg(j, i, x, y, w, h, fill, alpha = bg_alpha)
            }
            if (isTRUE(add_reticle)) {
                layer_reticle(j, i, x, y, w, h, fill)
            }
            if (cell_type == "violin") {
                layer_violin(j, i, x, y, w, h, fill, hmdf, violin_fill = violin_fill)
            } else {
                layer_boxplot(j, i, x, y, w, h, fill, hmdf, boxplot_fill = boxplot_fill)
            }
            if (is.function(layer_fun_callback)) {
                layer_fun_callback(j, i, x, y, w, h, fill, sr, sc)
            }
        }
        if (!identical(legend.position, "none")) {
            if (!is.null(legend_items)) {
                stop("Cannot use 'legend_items' with 'cell_type = 'violin' or 'boxplot'.")
            }
            if (isTRUE(legend_discrete)) {
                stop("Cannot use 'legend_discrete = TRUE' with 'cell_type = 'violin' or 'boxplot'.")
            }
            if ((cell_type == "violin" && is.null(violin_fill)) || (cell_type == "boxplot" && is.null(boxplot_fill))) {
                legends$.heatmap <- ComplexHeatmap::Legend(title = name, col_fun = hmargs$col, border = TRUE,
                    direction = legend.direction)
            } else if (isTRUE(add_bg)) {
                legends$.heatmap <- ComplexHeatmap::Legend(title = name, col_fun = get_col_fun(lower_cutoff, upper_cutoff, bg_alpha),
                    border = TRUE, direction = legend.direction)
            }
        }
    } else if (cell_type == "tile") {
        if (isTRUE(add_bg)) {
            stop("Cannot use 'add_bg' with 'cell_type = 'tile'.")
        }
        if (isTRUE(add_reticle)) {
            stop("Cannot use 'add_reticle' with 'cell_type = 'tile'.")
        }
        hmargs$rect_gp <- gpar(col = "grey80", lwd = 0.1)
        hmargs$layer_fun <- layer_fun_callback
        legends$.heatmap <- get_main_legend()
    } else if (cell_type == "label") {
        if (isTRUE(add_bg)) {
            stop("Cannot use 'add_bg' with 'cell_type = 'tile'.")
        }
        if (isTRUE(add_reticle)) {
            stop("Cannot use 'add_reticle' with 'cell_type = 'tile'.")
        }
        hmargs$layer_fun <- function(j, i, x, y, w, h, fill, sr, sc) {
            labels <- ComplexHeatmap::pindex(hmargs$matrix, i, j)
            inds <- !is.na(labels) & (if (is.null(label_cutoff)) TRUE else labels >= label_cutoff)
            inds[is.na(inds)] <- FALSE
            labels[inds] <- scales::number(labels[inds], accuracy = label_accuracy)
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
    nrows <- length(rows) * nrow_multiplier

    ## Set up the top annotations
    top_annos <- list(
        border = TRUE, col = list(), annotation_name_side = ifelse(isTRUE(flip), "top", "left"),
        show_annotation_name = list(), show_legend = FALSE
    )
    ncol_annos <- sum(cluster_columns, show_column_names) * 4
    if (!is.null(columns_split_by)) {
        columns_split_name <- columns_split_name %||% columns_split_by
        ncol_annos <- ncol_annos + 1
        top_annos[[columns_split_name]] <- hmargs$matrix[[columns_split_by]]
        top_annos$col[[columns_split_name]] <- palette_this(
            levels(hmargs$matrix[[columns_split_by]]),
            palette = columns_split_palette, palcolor = columns_split_palcolor
        )
        top_annos$show_annotation_name[[columns_split_name]] <- TRUE
        # top_annos$show_legend <- c(top_annos$show_legend, is.null(column_title))
        if (is.null(column_title) && !identical(legend.position, "none")) {
            legends$.column_split <- ComplexHeatmap::Legend(
                title = columns_split_name,
                labels = levels(hmargs$matrix[[columns_split_by]]),
                legend_gp = gpar(fill = top_annos$col[[columns_split_name]]),
                border = TRUE, nrow = if (legend.direction == "horizontal") 1 else NULL
            )
        }
        if (isTRUE(flip)) {
            hmargs$row_split <- hmargs$matrix[[columns_split_by]]
        } else {
            hmargs$column_split <- hmargs$matrix[[columns_split_by]]
        }
    }
    if (!is.null(columns_by)) {
        if (isTRUE(flip)) {
            hmargs$row_labels <- hmargs$matrix[[columns_by]]
        } else {
            hmargs$column_labels <- hmargs$matrix[[columns_by]]
        }
        if (column_name_annotation) {
            ncol_annos <- ncol_annos + 1
            top_annos[[columns_by]] <- hmargs$matrix[[columns_by]]
            top_annos$col[[columns_by]] <- palette_this(
                unique(hmargs$matrix[[columns_by]]),
                palette = columns_palette, palcolor = columns_palcolor
            )
            top_annos$show_annotation_name[[columns_by]] <- TRUE
            # top_annos$show_legend <- c(top_annos$show_legend, isFALSE(show_column_names))
            if (isTRUE(column_name_legend)) {
                legends$.columns_by <- ComplexHeatmap::Legend(
                    title = columns_by,
                    labels = levels(hmargs$matrix[[columns_by]]),
                    legend_gp = gpar(fill = top_annos$col[[columns_by]]),
                    border = TRUE, nrow = if (legend.direction == "horizontal") 1 else NULL
                )
            }
        }
    }

    ## Set up the column annotations
    column_annos <- list()
    if (!is.list(column_annotation)) {
        column_annotation <- as.list(column_annotation)
        names(column_annotation) <- unlist(column_annotation)
    }
    if (is.character(column_annotation_type) && identical(column_annotation_type, "auto")) {
        column_annotation_type <- as.list(rep("auto", length(column_annotation)))
        names(column_annotation_type) <- names(column_annotation)
    }
    if (is.character(column_annotation_palette) && length(column_annotation_palette) == 1) {
        column_annotation_palette <- as.list(rep(column_annotation_palette, length(column_annotation)))
        names(column_annotation_palette) <- names(column_annotation)
    }
    if (!is.list(column_annotation_palcolor)) {
        column_annotation_palcolor <- list(column_annotation_palcolor)
        column_annotation_palcolor <- rep(column_annotation_palcolor, length(column_annotation))
        names(column_annotation_palcolor) <- names(column_annotation)
    }
    column_annotation_agg <- column_annotation_agg %||% list()
    column_annotation_params <- column_annotation_params %||% list()
    for (caname in names(column_annotation)) {
        annocol <- column_annotation[[caname]]
        annoagg <- column_annotation_agg[[caname]]
        annotype <- column_annotation_type[[caname]]
        param <- column_annotation_params[[caname]] %||% list()
        annodata <- param$x %||% data  # TODO: order in param$x
        annocol <- check_columns(annodata, annocol)
        if (annotype == "auto") {
            all_ones <- annodata %>% group_by(!!!syms(unique(c(columns_split_by, columns_by)))) %>%
                summarise(n = n(), .groups = "drop") %>% pull(n)
            all_ones <- all(all_ones == 1)
            if (is.character(annodata[[annocol]]) || is.factor(annodata[[annocol]]) || is.logical(annodata[[annocol]])) {
                annotype <- ifelse(all_ones, "pie", "simple")
            } else if (is.numeric(annodata[[annocol]])) {
                annotype <- ifelse(all_ones, "points", "violin")
            } else {
                stop("Don't know how to handle column annotation type for column: ", annocol)
            }
        }
        if (annotype %in% c("simple", "points", "lines") && is.null(annoagg)) {
            warning("Assuming 'column_annotation_agg[\"", caname, "\"] = dplyr::first' for the simple column annotation")
            annoagg <- dplyr::first
        }
        if (is.null(annoagg)) {
            annodata <- annodata %>% select(!!!syms(unique(c(columns_split_by, columns_by, annocol))))
        } else {
            annodata <- annodata %>% group_by(!!!syms(unique(c(columns_split_by, columns_by)))) %>%
                summarise(!!sym(annocol) := annoagg(!!sym(annocol)), .groups = "drop")
        }
        param$x <- annodata
        param$split_by <- columns_split_by
        param$group_by <- columns_by
        param$column <- annocol
        param$title <- caname
        param$which <- ifelse(isTRUE(flip), "row", "column")
        param$palette <- column_annotation_palette[[caname]] %||% "Paired"
        param$palcolor <- column_annotation_palcolor[[caname]]
        param$legend.direction <- legend.direction
        if (legend.position == "none") {
            param$show_legend <- FALSE
        }
        if (!exists(paste0("anno_", annotype))) {
            stop("Unsupported annotation type: ", annotype)
        }
        anno <- do.call(paste0("anno_", annotype), param)
        column_annos[[caname]] <- anno$anno
        legends[[paste0("column.", caname)]] <- anno$legend
        ncol_annos <- ncol_annos + 1
    }

    if (column_annotation_side == "top") {
        top_annos <- c(top_annos, column_annos)
    } else {
        if (isTRUE(flip)) {
            hmargs$right_annotation <- do.call(ComplexHeatmap::rowAnnotation, column_annos)
        } else {
            hmargs$bottom_annotation <- do.call(ComplexHeatmap::HeatmapAnnotation, column_annos)
        }
    }
    rm(column_annos)
    if (length(top_annos$show_annotation_name) > 0) {
        if (isTRUE(flip)) {
            hmargs$left_annotation <- do.call(ComplexHeatmap::rowAnnotation, top_annos)
        } else {
            hmargs$top_annotation <- do.call(ComplexHeatmap::HeatmapAnnotation, top_annos)
        }
    }
    rm(top_annos)

    if (!is.null(columns_by)) hmargs$matrix[[columns_by]] <- NULL
    if (!is.null(columns_split_by)) hmargs$matrix[[columns_split_by]] <- NULL

    ## Set up the left annotations
    left_annos <- list(
        border = TRUE, col = list(), annotation_name_side = ifelse(isTRUE(flip), "right", "bottom"),
        show_annotation_name = list(), show_legend = FALSE
    )
    nrow_annos <- sum(cluster_rows, show_row_names) * 4
    if (!is.null(rows_split_by)) {
        if (is.null(rows_data)) {
            stop("'rows_data' must be provided for 'rows_split_by.'")
        }
        if (length(setdiff(rows, rows_data[, 1])) > 0) {
            stop("The first column of 'rows_data' must contain all the unique values of 'rows'.")
        }
        # select the rows from rows_data
        rows_data <- rows_data[rows_data[, 1] %in% rows, , drop = FALSE]
        nrow_annos <- nrow_annos + 1
        rows_by <- colnames(rows_data)[1]
        rows_split_by <- check_columns(rows_data, rows_split_by,
            force_factor = TRUE, allow_multi = TRUE,
            concat_multi = TRUE, concat_sep = rows_split_by_sep
        )
        rows_by <- check_columns(rows_data, rows_by, force_factor = TRUE)
        # align rows_data with rows
        if (!is.null(rows_split_by)) {
            rows_data <- rows_data[order(rows_data[[rows_split_by]], rows_data[[rows_by]]), , drop = FALSE]
        } else {
            rows_data <- rows_data[order(rows_data[[rows_by]]), , drop = FALSE]
        }

        left_annos[[rows_split_by]] <- rows_data %>%
            distinct(!!sym(rows_by), .keep_all = TRUE) %>%
            pull(!!sym(rows_split_by))

        left_annos$col[[rows_split_by]] <- palette_this(
            levels(rows_data[[rows_split_by]]),
            palette = rows_split_palette, palcolor = rows_split_palcolor
        )
        left_annos$show_annotation_name[[rows_split_by]] <- TRUE
        if (isTRUE(flip)) {
            hmargs$column_split <- left_annos[[rows_split_by]]
        } else {
            hmargs$row_split <- left_annos[[rows_split_by]]
        }

        if (is.null(row_title) && !identical(legend.position, "none")) {
            legends$.rows_split <- ComplexHeatmap::Legend(
                title = rows_split_by,
                labels = levels(rows_data[[rows_split_by]]),
                legend_gp = gpar(fill = left_annos$col[[rows_split_by]]),
                border = TRUE, nrow = if (legend.direction == "horizontal") 1 else NULL
            )
        }
    }

    if (row_name_annotation) {
        left_annos[[rows_name]] <- colnames(hmargs$matrix)
        left_annos$col[[rows_name]] <- palette_this(
            colnames(hmargs$matrix),
            palette = rows_palette, palcolor = rows_palcolor
        )
        nrow_annos <- nrow_annos + 1
        left_annos$show_annotation_name[[rows_name]] <- TRUE
        # left_annos$show_legend <- c(left_annos$show_legend, isFALSE(show_row_names))
        if (isTRUE(row_name_legend)) {
            legends$.rows <- ComplexHeatmap::Legend(
                title = rows_name,
                labels = colnames(hmargs$matrix),
                legend_gp = gpar(fill = left_annos$col[[rows_name]]),
                border = TRUE, nrow = if (legend.direction == "horizontal") 1 else NULL
            )
        }
    }

    ## Set up the row annotations
    row_annos <- list()
    if (!is.list(row_annotation)) {
        row_annotation <- as.list(row_annotation)
        names(row_annotation) <- unlist(row_annotation)
    }
    if (is.character(row_annotation_type) && identical(row_annotation_type, "auto")) {
        row_annotation_type <- as.list(rep("auto", length(row_annotation)))
        names(row_annotation_type) <- names(row_annotation)
    }
    if (is.character(row_annotation_palette) && length(row_annotation_palette) == 1) {
        row_annotation_palette <- as.list(rep(row_annotation_palette, length(row_annotation)))
        names(row_annotation_palette) <- names(row_annotation)
    }
    if (!is.list(row_annotation_palcolor)) {
        row_annotation_palcolor <- list(row_annotation_palcolor)
        row_annotation_palcolor <- rep(row_annotation_palcolor, length(row_annotation))
        names(row_annotation_palcolor) <- names(row_annotation)
    }
    row_annotation_agg <- row_annotation_agg %||% list()
    row_annotation_params <- row_annotation_params %||% list()
    for (raname in names(row_annotation)) {
        annocol <- row_annotation[[raname]]
        annoagg <- row_annotation_agg[[raname]]
        annotype <- row_annotation_type[[raname]]
        param <- row_annotation_params[[raname]] %||% list()
        if (identical(param$x, "data")) {
            param$x <- data %>%
                select(!!!syms(unique(c(rows_split_by, rows_by, rows)))) %>%
                pivot_longer(cols = -c(rows_split_by, rows_by), names_to = ".rows", values_to = annocol) %>%
                select(!!sym(".rows"), everything())
            rows_by <- ".rows"
        } else if (identical(param$x, "rows_data") || is.null(param$x)) {
            param$x <- rows_data
            rows_by <- colnames(rows_data)[1]
        } else {
            rows_by <- colnames(param$x)[1]
        }
        param$x[[1]] <- factor(param$x[[1]], levels = rows)
        param$x <- param$x[order(param$x[[1]]), , drop = FALSE]

        annocol <- check_columns(param$x, annocol)
        if (annotype == "auto") {
            all_ones <- param$x %>% group_by(!!!syms(unique(c(rows_split_by, rows_by)))) %>%
                summarise(n = n(), .groups = "drop") %>% pull(n)
            all_ones <- all(all_ones == 1)
            if (is.character(param$x[[annocol]]) || is.factor(param$x[[annocol]]) || is.logical(param$x[[annocol]])) {
                annotype <- ifelse(all_ones, "pie", "simple")
            } else if (is.numeric(param$x[[annocol]])) {
                annotype <- ifelse(all_ones, "points", "violin")
            } else {
                stop("Don't know how to handle row annotation type for column: ", annocol)
            }
        }
        if (annotype %in% c("simple", "points", "lines") && is.null(annoagg)) {
            warning("Assuming 'row_annotation_agg[\"", raname, "\"] = dplyr::first' for the simple column annotation")
            annoagg <- dplyr::first
        }
        if (is.null(annoagg)) {
            param$x <- param$x %>% select(!!!syms(unique(c(rows_split_by, rows_by, annocol))))
        } else {
            param$x <- param$x %>% group_by(!!!syms(unique(c(rows_split_by, rows_by)))) %>%
                summarise(!!sym(annocol) := annoagg(!!sym(annocol)), .groups = "drop")
        }
        param$split_by <- rows_split_by
        param$group_by <- rows_by
        param$column <- annocol
        param$title <- raname
        param$which <- ifelse(isTRUE(flip), "column", "row")
        param$palette <- row_annotation_palette[[raname]] %||% "Paired"
        param$palcolor <- row_annotation_palcolor[[raname]]
        param$legend.direction <- legend.direction
        if (legend.position == "none") {
            param$show_legend <- FALSE
        }
        if (!exists(paste0("anno_", annotype))) {
            stop("Unsupported annotation type: ", annotype)
        }
        anno <- do.call(paste0("anno_", annotype), param)
        row_annos[[raname]] <- anno$anno
        legends[[paste0("row", raname)]] <- anno$legend
        nrow_annos <- nrow_annos + 1
    }

    if (row_annotation_side == "left") {
        left_annos <- c(left_annos, row_annos)
    } else {
        if (isTRUE(flip)) {
            hmargs$bottom_annotation <- do.call(ComplexHeatmap::HeatmapAnnotation, row_annos)
        } else {
            hmargs$right_annotation <- do.call(ComplexHeatmap::rowAnnotation, row_annos)
        }
    }
    rm(row_annos)
    if (length(left_annos$show_annotation_name) > 0) {
        if (isTRUE(flip)) {
            hmargs$top_annotation <- do.call(ComplexHeatmap::HeatmapAnnotation, left_annos)
        } else {
            hmargs$left_annotation <- do.call(ComplexHeatmap::rowAnnotation, left_annos)
        }
    }
    rm(left_annos)

    ## Set up the heatmap
    if (isTRUE(flip)) {
        hmargs$matrix <- as.matrix(hmargs$matrix)
        width <- nrows * 0.25 + ncol_annos * 0.5
        height <- ncols * 0.25 + nrow_annos * 0.5
    } else {
        hmargs$matrix <- t(as.matrix(hmargs$matrix))
        width <- ncols * 0.25 + nrow_annos * 0.5
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
            p <- grid.grabExpr(ComplexHeatmap::draw(p, annotation_legend_list = legends,
            show_annotation_legend = FALSE, column_title = title))
        } else {
        p <- grid.grabExpr(ComplexHeatmap::draw(p, annotation_legend_list = legends,
            annotation_legend_side = legend.position, column_title = title))
        }
    } else {
        if (identical(legend.position, "none")) {
            p <- ComplexHeatmap::draw(p, annotation_legend_list = legends,
                show_annotation_legend = FALSE, column_title = title)
    } else {
        p <- ComplexHeatmap::draw(p, annotation_legend_list = legends,
            annotation_legend_side = legend.position, column_title = title)
        }
    }

    attr(p, "height") <- max(height, 4)
    attr(p, "width") <- max(width, 4)
    p
}

#' Heatmap
#'
#' @description Heatmap is a popular way to visualize data in matrix format. It is widely used in biology to visualize gene expression data in microarray and RNA-seq data. The heatmap is a matrix where rows represent the samples and columns represent the features. The color of each cell represents the value of the feature in the sample. The color can be continuous or discrete. The heatmap can be split by the columns or rows to show the subgroups in the data. The heatmap can also be annotated by the columns or rows to show the additional information of the samples or features.
#' @inheritParams common_args
#' @inheritParams HeatmapAtomic
#' @param data A data frame used to create the heatmap.
#'  The data should be in a long form where each row represents a instance in the heatmap.
#'  The `rows` should be multiple columns if you want to plot as rows, which you can refer as "features".
#' @param split_rows_data A logical value indicating whether to split the rows data as well using 'split_by' and 'split_by_sep'.
#' @return The heatmap(s).
#'  When `split_by` is not provided, the function returns a single heatmap ([ComplexHeatmap::Heatmap]-class object).
#'  When `split_by` is provided, the function returns a combined plot of multiple heatmaps wrapped by [patchwork::wrap_plots]
#'  if `combine = TRUE`, otherwise, it returns a list of heatmaps.
#' @export
#' @importFrom patchwork wrap_plots
#' @seealso \code{\link{anno_simple}}, \code{\link{anno_points}}, \code{\link{anno_lines}}, \code{\link{anno_pie}}, \code{\link{anno_violin}}, \code{\link{anno_boxplot}}, \code{\link{anno_density}}
#' @examples
#' \donttest{
#' set.seed(8525)
#' data <- data.frame(
#'    F1 = rnorm(100, 0.1),
#'    F2 = rnorm(100, 0.2),
#'    F3 = rnorm(100),
#'    F4 = rnorm(100, 0.3),
#'    F5 = rnorm(100, -0.1),
#'    F6 = rnorm(100, -0.2),
#'    c = sample(letters[1:8], 100, replace = TRUE),
#'    s = sample(LETTERS[1:2], 100, replace = TRUE),
#'    p = sample(c("X", "Y", "Z"), 100, replace = TRUE),
#'    a = sample(1:5, 100, replace = TRUE),
#'    p1 = c(sample(c(1, NA), 100, replace = TRUE)),
#'    p2 = c(sample(c(1, NA), 100, replace = TRUE)),
#'    p3 = c(sample(c(1, NA), 100, replace = TRUE))
#' )
#' rows <- c("F1", "F2", "F3", "F4", "F5", "F6")
#' rows_data <- data.frame(
#'    rows = rep(c("F1", "F2", "F3", "F4", "F5", "F6"), each = 10),
#'    rows1 = rep(c("F1", "F2", "F3", "F4", "F5", "F6"), each = 10),
#'    rs = rep(letters[1:2], each = 30),
#'    rp = sample(c("X", "Y", "Z"), 60, replace = TRUE),
#'    rv = rnorm(60, 0.5)
#' )
#'
#' Heatmap(data, rows = rows, columns_by = "c")
#' Heatmap(data, rows = list(RG1 = c("F1", "F2", "F3"), RG2 = c("F4", "F5", "F6")),
#'    columns_by = "c")
#' # Multiple columns_by, each as a split
#' Heatmap(data, rows = rows, columns_by = c("c", "s"), columns_by_sep = "/")
#' Heatmap(data, rows = rows, columns_by = c("p1", "p2", "p3"))
#' Heatmap(data, rows = rows, columns_by = "c", split_by = "s")
#' Heatmap(data, rows = rows, columns_by = "c", columns_split_by = "s")
#' Heatmap(data, rows = rows, columns_by = "c", columns_split_by = "s",
#'         rows_data = rows_data, rows_split_by = "rs")
#' Heatmap(data, rows = rows, columns_by = "c", columns_split_by = "s",
#'         rows_data = rows_data, rows_split_by = "rs", flip = TRUE)
#' Heatmap(data, rows = rows, columns_by = "c", cell_type = "bars")
#' Heatmap(data, rows = rows, columns_by = "c", cell_type = "bars",
#'         bars_sample = 3)
#' Heatmap(data, rows = rows, columns_by = "c", cell_type = "label")
#' Heatmap(data, rows = rows, columns_by = "c", cell_type = "label",
#'         label_cutoff = 0)
#' Heatmap(data, rows = rows, columns_by = "c", cell_type = "dot")
#' Heatmap(data, rows = rows, columns_by = "c", cell_type = "dot",
#'         dot_size = mean)
#' Heatmap(data, rows = rows, columns_by = "c", cell_type = "dot",
#'         dot_size = mean, row_name_annotation = FALSE, column_name_annotation = FALSE,
#'         row_names_side = "left", cluster_rows = FALSE, cluster_columns = FALSE)
#' Heatmap(data, rows = rows, columns_by = "c", cell_type = "dot",
#'         dot_size = mean, add_bg = TRUE)
#' Heatmap(data, rows = rows, columns_by = "c", cell_type = "dot",
#'         dot_size = mean, add_reticle = TRUE)
#' Heatmap(data, cluster_rows = FALSE, cluster_columns = FALSE, columns_by = "p",
#'    rows = c("p1", "p2", "p3"), name = "Category", pie_group_by = "c",
#'    cell_type = "pie")
#' Heatmap(data, cluster_rows = FALSE, cluster_columns = FALSE, columns_by = "p",
#'    rows = c("p1", "p2", "p3"), name = "Category", pie_group_by = "c",
#'    cell_type = "pie", pie_size = sqrt, add_bg = TRUE)
#' Heatmap(data, rows = rows, columns_by = "c", cell_type = "violin")
#' Heatmap(data, rows = rows, columns_by = "c", cell_type = "boxplot")
#' Heatmap(data, rows = rows, columns_by = "c", rows_data = rows_data,
#'   column_annotation = list(p1 = "p", p2 = "p", F1 = "F1"),
#'   column_annotation_type = list(p1 = "ring", p2 = "bar", F1 = "violin"),
#'   column_annotation_params = list(
#'      p1 = list(height = grid::unit(10, "mm"), show_legend = FALSE),
#'      F1 = list(height = grid::unit(18, "mm"))),
#'   rows_split_by = "rs",
#'   row_annotation = c("rp", "rv", "rows1"),
#'   row_annotation_side = "right",
#'   row_annotation_type = list(rp = "pie", rv = "density", rows1 = "simple"),
#'   row_annotation_params = list(rp = list(width = grid::unit(12, "mm"))),
#'   show_row_names = TRUE, show_column_names = TRUE)
#' Heatmap(data, rows = rows, columns_by = "c", rows_data = rows_data,
#'   column_annotation = list(p1 = "p", p2 = "p", F1 = "F1"),
#'   column_annotation_type = list(p1 = "ring", p2 = "bar", F1 = "violin"),
#'   column_annotation_params = list(
#'      p1 = list(height = grid::unit(10, "mm"), show_legend = FALSE),
#'      F1 = list(height = grid::unit(18, "mm"))),
#'   rows_split_by = "rs",
#'   row_annotation = c("rp", "rv", "rows1"),
#'   row_annotation_side = "right",
#'   row_annotation_type = list(rp = "pie", rv = "density", rows1 = "simple"),
#'   row_annotation_params = list(rp = list(width = grid::unit(12, "mm"))),
#'   show_row_names = TRUE, show_column_names = TRUE, flip = TRUE)
#' Heatmap(data, rows = rows, columns_by = "c", split_by = "p",
#'         palette = list(X = "Reds", Y = "Blues", Z = "Purp"))
#' }
Heatmap <- function(
    data, rows, columns_by, rows_name = "rows", columns_name = "columns", split_by = NULL, split_by_sep = "_", split_rows_data = FALSE,
    name = "value", border = TRUE, rows_palette = "Paired", rows_palcolor = NULL, title = NULL,
    pie_group_by = NULL, pie_group_by_sep = "_", pie_palette = "Spectral", pie_palcolor = NULL, pie_size = NULL,
    pie_name = NULL, pie_size_name = "size", pie_values = "count", legend_items = NULL, legend_discrete = FALSE,
    lower_quantile = 0, upper_quantile = 0.99, lower_cutoff = NULL, upper_cutoff = NULL,
    columns_by_sep = "_", columns_split_by = NULL, columns_split_name = NULL, columns_palette = "Paired", columns_palcolor = NULL,
    columns_split_by_sep = "_", columns_split_palette = "simspec", columns_split_palcolor = NULL,
    rows_data = NULL, rows_split_by = NULL, rows_split_by_sep = "_", rows_split_palette = "simspec", rows_split_palcolor = NULL,
    column_name_annotation = TRUE, column_name_legend = isFALSE(show_column_names) && !identical(legend.position, "none"),
    row_name_annotation = TRUE, row_name_legend = isFALSE(show_row_names) && !identical(legend.position, "none"),
    cluster_columns = TRUE, cluster_rows = TRUE, show_row_names = !row_name_annotation, show_column_names = !column_name_annotation,
    column_title = character(0), row_title = character(0), na_col = "grey85",
    row_names_side = "right", column_names_side = "bottom", bars_sample = 100, flip = FALSE,
    label_size = 10, label_cutoff = NULL, label_accuracy = 0.01, layer_fun_callback = NULL,
    cell_type = c("tile", "bars", "label", "dot", "violin", "boxplot", "pie"), cell_agg = mean,
    add_bg = FALSE, bg_alpha = 0.5, violin_fill = NULL, boxplot_fill = NULL, dot_size = 8, dot_size_name = "size",
    column_annotation = NULL, column_annotation_side = "top", column_annotation_palette = "Paired", column_annotation_palcolor = NULL,
    column_annotation_type = "auto", column_annotation_params = list(), column_annotation_agg = NULL,
    row_annotation = NULL, row_annotation_side = "left", row_annotation_palette = "Paired", row_annotation_palcolor = NULL,
    row_annotation_type = "auto", row_annotation_params = list(), row_annotation_agg = NULL,
    add_reticle = FALSE, reticle_color = "grey",
    palette = "RdBu", palcolor = NULL, alpha = 1, legend.position = "right", legend.direction = "vertical",
    seed = 8525, combine = TRUE, nrow = NULL, ncol = NULL, byrow = TRUE, axes = NULL, axis_titles = axes, guides = NULL, ...
) {
    validate_common_args(seed)
    if (is.null(split_by)) { split_rows_data <- FALSE }

    d_split_by <- check_columns(data, split_by, force_factor = TRUE, allow_multi = TRUE, concat_multi = TRUE, concat_sep = split_by_sep)
    if (isTRUE(split_rows_data) && !is.null(rows_data)) {
        if (is.character(rows_data) && length(rows_data) == 1 && startsWith(rows_data, "@")) {
            rows_data <- attr(data, substring(rows_data, 2))
        }
        r_split_by <- check_columns(rows_data, split_by, force_factor = TRUE, allow_multi = TRUE, concat_multi = TRUE, concat_sep = rows_split_by_sep)
        if (!identical(d_split_by, r_split_by)) {
            stop("The 'split_by' for 'data' and 'rows_data' must be the same.")
        }
    }

    if (!is.null(split_by)) {
        columns_by <- check_columns(data, columns_by, force_factor = TRUE, allow_multi = TRUE,
            concat_multi = TRUE, concat_sep = columns_by_sep)
        datas <- split(data, data[[d_split_by]])

        if (isTRUE(split_rows_data) && !is.null(rows_data)) {
            rows_datas <- split(rows_data, rows_data[[d_split_by]])
            nms <- names(datas)
            datas <- lapply(nms, function(nm) {
                dat <- datas[[nm]]
                attr(dat, "rows_data") <- rows_datas[[nm]]
                dat
            })
            names(datas) <- nms
        } else {
            # keep the order of levels
            datas <- datas[levels(data[[d_split_by]])]
        }
    } else {
        datas <- list(data)
        names(datas) <- "..."
    }
    palette <- check_palette(palette, names(datas))
    palcolor <- check_palcolor(palcolor, names(datas))

    if (isTRUE(split_rows_data) && !is.null(rows_data)) {
        rows_data <- "@rows_data"
    }

    plots <- lapply(
        names(datas), function(nm) {
            default_title <- if (length(datas) == 1 && identical(nm, "...")) NULL else nm
            if (is.function(title)) {
                title <- title(default_title)
            } else {
                title <- title %||% default_title
            }

            HeatmapAtomic(datas[[nm]],
                rows = rows, columns_by = columns_by, rows_name = rows_name, columns_name = columns_name, name = name, border = border, rows_palette = rows_palette, rows_palcolor = rows_palcolor,
                lower_quantile = lower_quantile, upper_quantile = upper_quantile, lower_cutoff = lower_cutoff, upper_cutoff = upper_cutoff,
                pie_group_by = pie_group_by, pie_group_by_sep = pie_group_by_sep, pie_palette = pie_palette, pie_palcolor = pie_palcolor,
                pie_size = pie_size, pie_name = pie_name, pie_size_name = pie_size_name, pie_values = pie_values, legend_items = legend_items, legend_discrete = legend_discrete,
                columns_by_sep = columns_by_sep, columns_split_by = columns_split_by, columns_split_name = columns_split_name, columns_palette = columns_palette,
                columns_palcolor = columns_palcolor, columns_split_by_sep = columns_split_by_sep, columns_split_palette = columns_split_palette,
                columns_split_palcolor = columns_split_palcolor, column_name_legend = column_name_legend, row_name_legend = row_name_legend,
                rows_data = rows_data, rows_split_by = rows_split_by, rows_split_by_sep = rows_split_by_sep, rows_split_palette = rows_split_palette, rows_split_palcolor = rows_split_palcolor,
                column_name_annotation = column_name_annotation, row_name_annotation = row_name_annotation,
                cluster_columns = cluster_columns, cluster_rows = cluster_rows, show_row_names = show_row_names, show_column_names = show_column_names,
                column_title = column_title, row_title = row_title, na_col = na_col, return_grob = TRUE, title = title,
                row_names_side = row_names_side, column_names_side = column_names_side, bars_sample = bars_sample, flip = flip,
                label_size = label_size, label_cutoff = label_cutoff, label_accuracy = label_accuracy, layer_fun_callback = layer_fun_callback,
                cell_type = cell_type, cell_agg = cell_agg, add_bg = add_bg, bg_alpha = bg_alpha, violin_fill = violin_fill, boxplot_fill = boxplot_fill,
                dot_size = dot_size, dot_size_name = dot_size_name, column_annotation = column_annotation, column_annotation_side = column_annotation_side,
                column_annotation_palette = column_annotation_palette, column_annotation_palcolor = column_annotation_palcolor,
                column_annotation_type = column_annotation_type, column_annotation_params = column_annotation_params, column_annotation_agg = column_annotation_agg,
                row_annotation = row_annotation, row_annotation_side = row_annotation_side, row_annotation_palette = row_annotation_palette,
                row_annotation_palcolor = row_annotation_palcolor, row_annotation_type = row_annotation_type, row_annotation_params = row_annotation_params,
                row_annotation_agg = row_annotation_agg, add_reticle = add_reticle, reticle_color = reticle_color,
                palette = palette[[nm]], palcolor = palcolor[[nm]], alpha = alpha, legend.position = legend.position, legend.direction = legend.direction,
                ...
            )
        }
    )

    combine_plots(plots, combine = combine, nrow = nrow, ncol = ncol, byrow = byrow,
        axes = axes, axis_titles = axis_titles, guides = guides)
}
