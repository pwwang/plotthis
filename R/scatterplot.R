
#' Scatter Plot Atomic
#'
#' @inheritParams common_args
#' @param x A character vector specifying the column to use for the x-axis.
#'  A numeric column is expected.
#' @param y A character vector specifying the column to use for the y-axis.
#'  A numeric column is expected.
#' @param size_by Which column to use as the size of the dots. It must be a numeric column.
#'  Or it can be a numeric value to specify the size of the dots.
#' @param size_name A character vector specifying the name for the size legend.
#' @param color_by Which column to use as the color of the dots.
#'  It could be a numeric column or a factor/character column.
#'  For shapes 21-25, the color is applied to the fill color.
#' @param color_name A character vector specifying the name for the color legend.
#' @param color_reverse A logical value indicating whether to reverse the color direction. Default is FALSE.
#' @param border_color A character vector specifying the color for the border of the points.
#'  Or TRUE to use the fill color as the border color.
#' @param highlight A vector of indexes or rownames to select the points to highlight.
#'  It could also be an expression (in string) to filter the data.
#' @param highlight_shape A numeric value specifying the shape of the highlighted points. Default is 16.
#' @param highlight_size A numeric value specifying the size of the highlighted points. Default is 3.
#' @param highlight_color A character vector specifying the color of the highlighted points. Default is "red".
#' @param highlight_alpha A numeric value specifying the transparency of the highlighted points. Default is 1.
#' @param alpha A numeric value specifying the transparency of the dots. Default is 1.
#'  For shapes 21-25, the transparency is applied to the fill color.
#' @param shape A numeric value specifying the shape of the points. Default is 21.
#' @param xtrans A character vector specifying the transformation of the x-axis. Default is "identity".
#' @param ytrans A character vector specifying the transformation of the y-axis. Default is "identity".
#' @return A ggplot object
#' @keywords internal
#' @importFrom utils getFromNamespace
#' @importFrom dplyr filter
#' @importFrom ggplot2 aes geom_point scale_size_area scale_fill_gradientn scale_color_gradientn labs
#' @importFrom ggplot2 guide_colorbar guide_legend guides guide_none scale_size
ScatterPlotAtomic <- function(
    data, x, y, size_by = 2, size_name = NULL, color_by = NULL, color_name = NULL, color_reverse = FALSE,
    theme = "theme_this", theme_args = list(), alpha = ifelse(shape %in% 21:25, 0.65, 1),
    shape = 21, border_color = "black", xtrans = "identity", ytrans = "identity",
    highlight = NULL, highlight_shape = 16, highlight_size = 3, highlight_color = "red", highlight_alpha = 1,
    palette = ifelse(!is.null(color_by) && !is.numeric(data[[color_by]]), "Paired", "Spectral"), palcolor = NULL,
    facet_by = NULL, facet_scales = "fixed", facet_ncol = NULL, facet_nrow = NULL, facet_byrow = TRUE,
    aspect.ratio = 1, legend.position = "right", legend.direction = "vertical",
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL, ...
) {
    ggplot <- if (getOption("plotthis.gglogger.enabled", FALSE)) {
        gglogger::ggplot
    } else {
        ggplot2::ggplot
    }
    facet_by <- check_columns(data, facet_by, force_factor = TRUE, allow_multi = TRUE)
    if (!is.numeric(size_by)) {
        size_by <- check_columns(data, size_by)
    }
    color_by <- check_columns(data, color_by)
    if (is.null(color_by)) {
        data$.color_by <- ""
        color_by <- ".color_by"
        color_legend <- FALSE
    } else {
        color_legend <- TRUE
    }
    if (!is.numeric(data[[color_by]])) {
        color_by <- check_columns(data, color_by, force_factor = TRUE)
    }

    hidata <- NULL
    if (!is.null(highlight)) {
        if (isTRUE(highlight)) {
            hidata <- data
        } else if (is.numeric(highlight)) {
            hidata <- data[highlight, , drop = FALSE]
        } else if (is.character(highlight) && length(highlight) == 1) {
            hidata <- filter(data, !!parse_expr(highlight))
        } else if (is.null(rownames(data))) {
            stop("No row names in the data, please provide a vector of indexes to highlight.")
        } else {
            hidata <- data[highlight, , drop = FALSE]
        }
    }

    shape_has_fill <- shape %in% 21:25
    hishape_has_fill <- highlight_shape %in% 21:25

    p <- ggplot(data, aes(x = !!sym(x), y = !!sym(y)))
    mapping <- list()
    point_layer_args <- list(shape = shape)
    if (shape_has_fill) {
        mapping[[length(mapping) + 1]] <- aes(fill = !!sym(color_by))
        if (isTRUE(border_color)) {
            mapping[[length(mapping) + 1]] <- aes(color = !!sym(color_by))
        } else {
            point_layer_args$color <- border_color
        }
    } else {
        mapping[[length(mapping) + 1]] <- aes(color = !!sym(color_by))
    }
    if (is.numeric(size_by)) {
        point_layer_args$size <- size_by
    } else {
        mapping[[length(mapping) + 1]] <- aes(size = !!sym(size_by))
    }
    modify_list <- getFromNamespace("modify_list", "ggplot2")
    point_layer_args$mapping <- Reduce(modify_list, mapping)
    point_layer <- do.call(geom_point, point_layer_args)

    p <- p + point_layer
    if (!is.numeric(size_by)) {
        p <- p + scale_size_area(max_size = 6, n.breaks = 4) +
            guides(size = guide_legend(
                title = size_name %||% size_by,
                override.aes = list(fill = "grey30", shape = 21), order = 1))
    }
    if (shape_has_fill) {
        if (is.numeric(data[[color_by]])) {
            p <- p + scale_fill_gradientn(
                n.breaks = 5,
                colors = palette_this(data[[color_by]], palette = palette, palcolor = palcolor, reverse = color_reverse, alpha = alpha),
                na.value = "grey80",
                guide = if (isTRUE(border_color) || isFALSE(color_legend)) {
                    # legend for border color will be added later
                    # so we don't need to show the legend for fill color
                    guide_none()
                } else {
                    guide_colorbar(
                        title = color_name %||% color_by,
                        frame.colour = "black", ticks.colour = "black", title.hjust = 0, order = 2)
                }
            )
            if (isTRUE(border_color)) {
                p <- p + scale_color_gradientn(
                    n.breaks = 5,
                    colors = palette_this(palette = palette, palcolor = palcolor, reverse = color_reverse),
                    na.value = "grey80",
                    guide = if (isTRUE(border_color) && isTRUE(color_legend)) {
                        guide_colorbar(
                            title = color_name %||% color_by,
                            frame.colour = "black", ticks.colour = "black", title.hjust = 0, order = 2)
                    } else {
                        guide_none()
                    }
                )
            }
        } else {  # factor/character
            p <- p + scale_fill_manual(
                values = palette_this(levels(data[[color_by]]), palette = palette, palcolor = palcolor, alpha = alpha),
                na.value = "grey80",
                guide = if (isTRUE(border_color) || isFALSE(color_legend)) {
                    guide_none()
                } else {
                    guide_legend(
                        title = color_name %||% color_by,
                        override.aes = list(size = 4, alpha = 1),
                        order = 3
                    )
                }
            )
            if (isTRUE(border_color)) {
                p <- p + scale_color_manual(
                    values = palette_this(levels(data[[color_by]]), palette = palette, palcolor = palcolor),
                    na.value = "black",
                    guide = if (isTRUE(border_color) && isTRUE(color_legend)) {
                        guide_legend(title = color_name %||% color_by, order = 3)
                    } else {
                        guide_none()
                    }
                )
            }
        }
    } else {  # shape has no fill
        if (is.numeric(data[[color_by]])) {
            p <- p + scale_color_gradientn(
                n.breaks = 5,
                colors = palette_this(data[[color_by]], palette = palette, palcolor = palcolor, reverse = color_reverse, alpha = alpha),
                na.value = "grey80",
                guide = if (isTRUE(color_legend)) {
                    guide_colorbar(
                        title = color_name %||% color_by,
                        frame.colour = "black", ticks.colour = "black", title.hjust = 0, order = 2)
                } else {
                    guide_none()
                }
            )
        } else {  # factor/character
            p <- p + scale_color_manual(
                values = palette_this(levels(data[[color_by]]), palette = palette, palcolor = palcolor, alpha = alpha),
                na.value = "grey80",
                guide = if (isTRUE(color_legend)) {
                    guide_legend(
                        title = color_name %||% color_by,
                        override.aes = list(size = 4, alpha = 1),
                        order = 3
                    )
                } else {
                    guide_none()
                }
            )
        }
    }

    if (!is.null(hidata)) {
        if (hishape_has_fill) {
            p <- p + geom_point(data = hidata,
                shape = highlight_shape, fill = highlight_color, color = "transparent",
                size = highlight_size, alpha = highlight_alpha)
        } else {
            p <- p + geom_point(data = hidata,
                shape = highlight_shape, color = highlight_color,
                size = highlight_size, alpha = highlight_alpha)
        }
    }

    p <- p +
        scale_x_continuous(trans = xtrans) +
        scale_y_continuous(trans = ytrans) +
        labs(title = title, subtitle = subtitle, x = xlab %||% x, y = ylab %||% y) +
        do.call(theme, theme_args) +
        ggplot2::theme(
            aspect.ratio = aspect.ratio,
            legend.position = legend.position,
            legend.direction = legend.direction,
            panel.grid.major = element_line(colour = "grey80", linetype = 2)
        )

    height <- width <- 5
    if (!identical(legend.position, "none")) {
        if (legend.position %in% c("right", "left")) {
            width <- width + 1
        } else if (legend.direction == "horizontal") {
            height <- height + 1
        } else {
            width <- width + 2
        }
    }

    attr(p, "height") <- height
    attr(p, "width") <- width

    facet_plot(p, facet_by, facet_scales, facet_nrow, facet_ncol, facet_byrow,
        legend.position = legend.position, legend.direction = legend.direction)
}


#' Scatter Plot
#'
#' @inheritParams common_args
#' @inheritParams ScatterPlotAtomic
#' @return A ggplot object or wrap_plots object or a list of ggplot objects
#' @export
#' @examples
#' set.seed(8525)
#'
#' data <- data.frame(
#'    x = rnorm(20),
#'    y = rnorm(20),
#'    w = abs(rnorm(20)),
#'    t = sample(c("A", "B"), 20, replace = TRUE)
#' )
#' ScatterPlot(data, x = "x", y = "y")
#'
#' # highlight points
#' ScatterPlot(data, x = "x", y = "y", highlight = 'x > 0')
#'
#' # size_by is a numeric column
#' ScatterPlot(data, x = "x", y = "y", size_by = "w")
#'
#' # color_by is a numeric column
#' ScatterPlot(data, x = "x", y = "y", color_by = "w")
#'
#' # color_by is a factor/character column and set a border_color
#' ScatterPlot(data, x = "x", y = "y", size_by = "w", color_by = "t",
#'  border_color = "red")
#'
#' # Same border_color as the fill color
#' ScatterPlot(data, x = "x", y = "y", size_by = "w", color_by = "t",
#'  border_color = TRUE)
#'
#' # Shape doesn't have fill color
#' ScatterPlot(data, x = "x", y = "y", size_by = "w", color_by = "t",
#'  shape = 1, palette = "Set1")
#'
#' # Change color per plot
#' ScatterPlot(data, x = "x", y = "y", split_by = "t",
#'             palcolor = list(A = "blue", B = "red"))
ScatterPlot <- function(
    data, x, y, size_by = 2, size_name = NULL, color_by = NULL, color_name = NULL, color_reverse = FALSE,
    split_by = NULL, split_by_sep = "_", shape = 21, alpha = ifelse(shape %in% 21:25, 0.65, 1), border_color = "black",
    highlight = NULL, highlight_shape = 16, highlight_size = 3, highlight_color = "red", highlight_alpha = 1,
    theme = "theme_this", theme_args = list(),
    palette = ifelse(!is.null(color_by) && !is.numeric(data[[color_by]]), "Paired", "Spectral"), palcolor = NULL,
    facet_by = NULL, facet_scales = "fixed", facet_ncol = NULL, facet_nrow = NULL, facet_byrow = TRUE,
    aspect.ratio = 1, legend.position = "right", legend.direction = "vertical",
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL,
    combine = TRUE, nrow = NULL, ncol = NULL, byrow = TRUE, seed = 8525,
    axes = NULL, axis_titles = axes, guides = NULL, design = NULL, ...
) {
    validate_common_args(seed, facet_by = facet_by)
    theme <- process_theme(theme)
    split_by <- check_columns(data, split_by, force_factor = TRUE, allow_multi = TRUE, concat_multi = TRUE,
        concat_sep = split_by_sep)

    if (!is.null(split_by)) {
        datas <- split(data, data[[split_by]])
        # keep the order of levels
        datas <- datas[levels(data[[split_by]])]
    } else {
        datas <- list(data)
        names(datas) <- "..."
    }
    palette <- check_palette(palette, names(datas))
    palcolor <- check_palcolor(palcolor, names(datas))
    legend.direction <- check_legend(legend.direction, names(datas), "legend.direction")
    legend.position <- check_legend(legend.position, names(datas), "legend.position")

    plots <- lapply(
        names(datas), function(nm) {
            default_title <- if (length(datas) == 1 && identical(nm, "...")) NULL else nm
            if (is.function(title)) {
                title <- title(default_title)
            } else {
                title <- title %||% default_title
            }
            ScatterPlotAtomic(
                datas[[nm]], x = x, y = y, size_by = size_by, size_name = size_name, color_by = color_by,
                highlight = highlight, highlight_shape = highlight_shape, highlight_size = highlight_size,
                highlight_color = highlight_color, highlight_alpha = highlight_alpha,
                color_name = color_name, color_reverse = color_reverse, theme = theme, theme_args = theme_args,
                alpha = alpha, shape = shape, border_color = border_color, palette = palette[[nm]], palcolor = palcolor[[nm]],
                facet_by = facet_by, facet_scales = facet_scales, facet_ncol = facet_ncol, facet_nrow = facet_nrow, facet_byrow = facet_byrow,
                aspect.ratio = aspect.ratio, legend.position = legend.position[[nm]], legend.direction = legend.direction[[nm]],
                title = title, subtitle = subtitle, xlab = xlab, ylab = ylab, ...
            )
        }
    )

    combine_plots(plots, combine = combine, nrow = nrow, ncol = ncol, byrow = byrow,
        axes = axes, axis_titles = axis_titles, guides = guides, design = design)
}
