
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
#' @param alpha A numeric value specifying the transparency of the dots. Default is 1.
#'  For shapes 21-25, the transparency is applied to the fill color.
#' @param xtrans A character vector specifying the transformation of the x-axis. Default is "identity".
#' @param ytrans A character vector specifying the transformation of the y-axis. Default is "identity".
#' @return A ggplot object
#' @keywords internal
#' @importFrom gglogger ggplot
#' @importFrom ggplot2 aes geom_point scale_size_area scale_fill_gradientn scale_color_gradientn labs
#' @importFrom ggplot2 guide_colorbar guide_legend guides guide_none scale_size
ScatterPlotAtomic <- function(
    data, x, y, size_by = 2, size_name = NULL, color_by = NULL, color_name = NULL, color_reverse = FALSE,
    theme = "theme_this", theme_args = list(), alpha = ifelse(shape %in% 21:25, 0.65, 1),
    shape = 21, border_color = "black", xtrans = "identity", ytrans = "identity",
    palette = ifelse(!is.null(color_by) && !is.numeric(data[[color_by]]), "Paired", "Spectral"), palcolor = NULL,
    facet_by = NULL, facet_scales = "fixed", facet_ncol = NULL, facet_nrow = NULL, facet_byrow = TRUE,
    aspect.ratio = 1, legend.position = "right", legend.direction = "vertical",
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL, ...
) {
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

    shape_has_fill <- shape %in% 21:25

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
    point_layer_args$mapping <- Reduce(ggplot2:::modify_list, mapping)
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
                guide = if (isFALSE(color_legend)) {
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
                    guide = if (isTRUE(color_legend)) {
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
            if (isTRUE(border_color)) {
                p <- p + scale_color_manual(
                    values = palette_this(levels(data[[color_by]]), palette = palette, palcolor = palcolor),
                    na.value = "black",
                    guide = if (isTRUE(color_legend)) {
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
                guide = if (isFALSE(color_legend)) {
                    guide_none()
                } else {
                    guide_colorbar(
                        title = color_name %||% color_by,
                        frame.colour = "black", ticks.colour = "black", title.hjust = 0, order = 2)
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
ScatterPlot <- function(
    data, x, y, size_by = 2, size_name = NULL, color_by = NULL, color_name = NULL, color_reverse = FALSE,
    split_by = NULL, split_by_sep = "_", shape = 21, alpha = ifelse(shape %in% 21:25, 0.65, 1), border_color = "black",
    theme = "theme_this", theme_args = list(),
    palette = ifelse(!is.null(color_by) && !is.numeric(data[[color_by]]), "Paired", "Spectral"), palcolor = NULL,
    facet_by = NULL, facet_scales = "fixed", facet_ncol = NULL, facet_nrow = NULL, facet_byrow = TRUE,
    aspect.ratio = 1, legend.position = "right", legend.direction = "vertical",
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL,
    combine = TRUE, nrow = NULL, ncol = NULL, byrow = TRUE, seed = 8525, ...
) {
    validate_common_args(seed, facet_by = facet_by)

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
                color_name = color_name, color_reverse = color_reverse, theme = theme, theme_args = theme_args,
                alpha = alpha, shape = shape, border_color = border_color, palette = palette, palcolor = palcolor,
                facet_by = facet_by, facet_scales = facet_scales, facet_ncol = facet_ncol, facet_nrow = facet_nrow, facet_byrow = facet_byrow,
                aspect.ratio = aspect.ratio, legend.position = legend.position, legend.direction = legend.direction,
                title = title, subtitle = subtitle, xlab = xlab, ylab = ylab, ...
            )
        }
    )

    combine_plots(plots, combine = combine, nrow = nrow, ncol = ncol, byrow = byrow)
}
