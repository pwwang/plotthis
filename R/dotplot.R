#' Dot Plot without splitting the data
#'
#' @inheritParams common_args
#' @param x A character vector specifying the column to use for the x-axis.
#'  Could be either numeric or factor/character. When multiple columns are provided, they will be concatenated with 'x_sep'.
#' @param y A character vector specifying the column to use for the y-axis.
#'  Could be either numeric or factor/character. When multiple columns are provided, they will be concatenated with 'y_sep'.
#' @param x_sep A character vector to concatenate multiple columns in x. Default is "_".
#' @param y_sep A character vector to concatenate multiple columns in y. Default is "_".
#' @param size_by Which column to use as the size of the dots. It must be a numeric column.
#'   If not provided, the size will be the count of the instances for each 'y' in 'x'.
#' @param fill_by Which column to use as the fill the dots. It must be a numeric column.
#'   If not provided, all dots will be filled with the same color at the middle of the palette.
#' @param fill_cutoff A numeric value specifying the cutoff for the fill column.
#' @param fill_reverse A logical value indicating whether to reverse the fill direction. Default is FALSE.
#'   By default, the fill direction is "up". If TRUE, the fill direction is "down".
#'   When the direction is "up", the values less than the cutoff will be filled with grey.
#'   When the direction is "down", the values greater than the cutoff will be filled with grey.
#' @param size_name A character vector specifying the name for the size legend.
#' @param fill_name A character vector specifying the name for the fill legend.
#' @param fill_cutoff_name A character vector specifying the name for the fill cutoff legend.
#' @param flip A logical value indicating whether to flip the x and y axes. Default is FALSE.
#' @param add_bg A logical value indicating whether to add a background color to the plot. Default is FALSE.
#' @param bg_palette A character vector specifying the palette for the background color. Default is "stripe".
#' @param bg_palcolor A character vector specifying the color for the background color.
#' @param bg_alpha A numeric value specifying the alpha for the background color. Default is 0.2.
#' @param bg_direction A character vector specifying the direction for the background color. Default is "vertical".
#'   Other options are "horizontal". "h" and "v" are also accepted.
#' @param lollipop A logical value indicating whether to make it a lolipop plot. Default is FALSE.
#'   When TRUE, 'x' should be a numeric column and 'y' should be a factor/character column.
#' @return A ggplot object
#' @keywords internal
#' @importFrom dplyr %>% group_by summarise n first
#' @importFrom gglogger ggplot
#' @importFrom ggplot2 geom_point scale_y_discrete scale_size_area scale_fill_gradientn scale_color_gradientn labs
#' @importFrom ggplot2 coord_flip guide_colorbar guide_legend guides guide_none scale_size geom_segment
#' @importFrom ggnewscale new_scale_color
DotPlotAtomic <- function(
    data, x, y, x_sep = "_", y_sep = "_", flip = FALSE, lollipop = FALSE,
    size_by = NULL, fill_by = NULL, fill_cutoff = NULL, fill_reverse = FALSE,
    size_name = NULL, fill_name = NULL, fill_cutoff_name = NULL,
    theme = "theme_this", theme_args = list(), palette = "Spectral", palcolor = NULL, alpha = 1,
    facet_by = NULL, facet_scales = "fixed", facet_ncol = NULL, facet_nrow = NULL, facet_byrow = TRUE,
    x_text_angle = 0, aspect.ratio = 1, legend.position = "right", legend.direction = "vertical",
    add_bg = FALSE, bg_palette = "stripe", bg_palcolor = NULL, bg_alpha = 0.2, bg_direction = c("vertical", "horizontal", "v", "h"),
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL, keep_empty = FALSE, ...
) {
    bg_direction <- match.arg(bg_direction)
    if (bg_direction %in% c("h", "horizontal")) {
        bg_direction <- "horizontal"
    } else {
        bg_direction <- "vertical"
    }
    x_is_numeric <- !is.character(data[[x]]) && !is.factor(data[[x]]) && length(x) == 1
    y_is_numeric <- !is.character(data[[y]]) && !is.factor(data[[y]]) && length(y) == 1
    if (!x_is_numeric) {
        x <- check_columns(data, x, force_factor = TRUE, allow_multi = TRUE, concat_multi = TRUE, concat_sep = x_sep)
    }
    if (!y_is_numeric) {
        y <- check_columns(data, y, force_factor = TRUE, allow_multi = TRUE, concat_multi = TRUE, concat_sep = y_sep)
    }

    if (!is.null(fill_cutoff) && is.null(fill_by)) {
        stop("'fill_by' must be provided when 'fill_cutoff' is specified.")
    }

    facet_by <- check_columns(data, facet_by, force_factor = TRUE, allow_multi = TRUE)
    size_by <- check_columns(data, size_by)
    if (is.null(size_by)) {
        if (x_is_numeric || y_is_numeric) {
            stop("'size_by' must be provided when 'x' or 'y' is numeric.")
        }
        if (is.null(fill_by)) {
            data <- data %>%
                group_by(!!!syms(unique(c(x, y, facet_by)))) %>%
                summarise(.size = n(), .groups = "drop")
        } else {
            warning("Using the first value of fill_by as size_by is calculated from the count of instances.", immediate. = TRUE)
            data <- data %>%
                group_by(!!!syms(unique(c(x, y, facet_by)))) %>%
                summarise(!!sym(fill_by) := first(!!sym(fill_by)), .size = n(), .groups = "drop")
        }
        size_by <- ".size"
    }

    fill_by <- check_columns(data, fill_by)
    if (!is.null(fill_by) && !is.null(fill_cutoff)) {
        # Add a column to indicate the fill cutoff
        if (isFALSE(fill_reverse)) {
            fill_cutoff_label <- paste0(fill_by, " < ", fill_cutoff)
            data[[fill_by]][data[[fill_by]] < fill_cutoff] <- NA
        } else {
            fill_cutoff_label <- paste0(fill_by, " > ", fill_cutoff)
            data[[fill_by]][data[[fill_by]] > fill_cutoff] <- NA
        }
    }
    if (is.null(fill_by)) {
        data$.fill_by <- 1
        fill_by <- ".fill_by"
        fill_legend <- FALSE
    } else {
        fill_legend <- TRUE
    }

    just <- calc_just(x_text_angle)
    p <- ggplot(data, aes(x = !!sym(x), y = !!sym(y)))
    if (add_bg) {
        if (bg_direction == "vertical") {
            if (x_is_numeric) {
                stop("Vertical 'bg_direction' is not supported when 'x' is numeric.")
            }
            p <- p + bg_layer(data, x, bg_palette, bg_palcolor, bg_alpha, keep_empty, facet_by, bg_direction)
        } else {
            if (y_is_numeric) {
                stop("Horizontal 'bg_direction' is not supported when 'y' is numeric.")
            }
            p <- p + bg_layer(data, y, bg_palette, bg_palcolor, bg_alpha, keep_empty, facet_by, bg_direction)
        }
    }

    if (!x_is_numeric) {
        p <- p + scale_x_discrete(drop = !keep_empty)
    }
    if (!y_is_numeric) {
        p <- p + scale_y_discrete(drop = !keep_empty)
    }

    if (isTRUE(lollipop)) {
        p <- p +
            geom_segment(aes(x = 0, xend = !!sym(x), yend = !!sym(y)), color = "black", linewidth = 2) +
            geom_segment(aes(x = 0, xend = !!sym(x), yend = !!sym(y), color = !!sym(fill_by)), linewidth = 1) +
            scale_x_continuous(expand = c(0, 0, 0.05, 0)) +
            scale_color_gradientn(
                n.breaks = 3,
                colors = palette_this(palette = palette, palcolor = palcolor, reverse = fill_reverse),
                na.value = "grey80",
                guide = "none"
            ) +
            new_scale_color()
    }

    p <- p + geom_point(aes(size = !!sym(size_by), fill = !!sym(fill_by), color = ""), shape = 21, alpha = alpha) +
        scale_size_area(max_size = 6, n.breaks = 4) +
        guides(size = guide_legend(
            title = size_name %||% size_by,
            override.aes = list(fill = "grey30", shape = 21), order = 1)) +
        scale_fill_gradientn(
            n.breaks = 3,
            colors = palette_this(palette = palette, palcolor = palcolor, reverse = fill_reverse),
            na.value = "grey80",
            guide = if (isTRUE(fill_legend)) {
                guide_colorbar(
                    title = fill_name %||% fill_by,
                    frame.colour = "black", ticks.colour = "black", title.hjust = 0, order = 2)
            } else {
                guide_none()
            }
        ) +
        labs(title = title, subtitle = subtitle, x = xlab %||% x, y = ylab %||% y) +
        do.call(theme, theme_args) +
        theme(
            aspect.ratio = aspect.ratio,
            legend.position = legend.position,
            legend.direction = legend.direction,
            panel.grid.major = element_line(colour = "grey80", linetype = 2),
            axis.text.x = element_text(angle = x_text_angle, hjust = just$h, vjust = just$v)
        )

    p <- p + scale_color_manual(values = NA, na.value = "black", guide = "none")
    if (!is.null(fill_by) && !is.null(fill_cutoff)) {
        p <- p + guides(color = guide_legend(
            title = fill_cutoff_name %||% fill_cutoff_label,
            override.aes = list(colour = "black", fill = "grey80", size = 3),
            order = 3
        ))
    }

    if (isTRUE(flip)) {
        p <- p + coord_flip()
    }

    if (x_is_numeric) {
        nx <- 5
    } else if (keep_empty) {
        nx <- nlevels(data[[x]])
    } else {
        nx <- nlevels(droplevels(data[[x]]))
    }
    if (y_is_numeric) {
        ny <- 5
    } else if (keep_empty) {
        ny <- nlevels(data[[y]])
    } else {
        ny <- nlevels(droplevels(data[[y]]))
    }

    height = ny * 0.5
    width = nx * 0.8
    if (legend.position %in% c("right", "left")) {
        width <- width + 1
    } else if (legend.direction == "horizontal") {
        height <- height + 1
    } else {
        width <- width + 2
    }
    height <- max(height, 3)

    if (isTRUE(flip)) {
        attr(p, "height") <- width
        attr(p, "width") <- height
    } else {
        attr(p, "height") <- height
        attr(p, "width") <- width
    }

    facet_plot(p, facet_by, facet_scales, facet_nrow, facet_ncol, facet_byrow)
}

#' Dot Plot / Scatter Plot / Lollipop Plot
#'
#' @rdname dotplot
#' @description For `DotPlot`, X-axis and Y-axis could be either numeric or factor/character.
#'   When x-axis and y-axis are both numeric, the plot works as a scatter plot.
#'   `LollipopPlot` is an alias of `DotPlot` when `lollipop` = TRUE.
#'
#' @inheritParams DotPlotAtomic
#' @inheritParams common_args
#' @return A ggplot object or wrap_plots object or a list of ggplot objects
#' @export
#' @examples
#' mtcars <- datasets::mtcars
#' mtcars$carb <- factor(mtcars$carb)
#' mtcars$gear <- factor(mtcars$gear)
#' DotPlot(mtcars, x = "carb", y = "gear", size_by = "wt",
#'         fill_by = "mpg", fill_cutoff = 18)
#' DotPlot(mtcars, x = "carb", y = "gear", size_by = "wt",
#'         fill_by = "mpg", fill_cutoff = 18, add_bg = TRUE)
#' DotPlot(mtcars, x = "carb", y = "gear", size_by = "wt",
#'         fill_by = "mpg", fill_cutoff = 18, add_bg = TRUE,
#'         bg_direction = "h")
#' DotPlot(mtcars, x = "carb", y = "gear", size_by = "wt",
#'         fill_by = "mpg", fill_cutoff = 18, facet_by = "cyl")
#' DotPlot(mtcars, x = "carb", y = "gear", size_by = "wt",
#'         fill_by = "mpg", fill_cutoff = 18, facet_by = "cyl",
#'         facet_scales = "free_x")
#' DotPlot(mtcars, x = "carb", y = "gear", size_by = "wt",
#'         fill_by = "mpg", fill_cutoff = 18, split_by = "cyl")
#' # works as a scatter plot
#' DotPlot(mtcars, x = "qsec", y = "drat", size_by = "wt",
#'         fill_by = "mpg", fill_cutoff = 18, fill_cutoff_name = "Small mpgs")
DotPlot <- function(
    data, x, y, x_sep = "_", y_sep = "_", flip = FALSE,
    split_by = NULL, split_by_sep = "_", size_name = NULL, fill_name = NULL, fill_cutoff_name = NULL,
    add_bg = FALSE, bg_palette = "stripe", bg_palcolor = NULL, bg_alpha = 0.2, bg_direction = c("vertical", "horizontal", "v", "h"),
    size_by = NULL, fill_by = NULL, fill_cutoff = NULL, fill_reverse = FALSE,
    theme = "theme_this", theme_args = list(), palette = "Spectral", palcolor = NULL, alpha = 1,
    facet_by = NULL, facet_scales = "fixed", facet_ncol = NULL, facet_nrow = NULL, facet_byrow = TRUE,
    x_text_angle = 0, seed = 8525, aspect.ratio = 1, legend.position = "right", legend.direction = "vertical",
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL, keep_empty = FALSE,
    combine = TRUE, nrow = NULL, ncol = NULL, byrow = TRUE, ...
) {
    validate_common_args(seed, facet_by = facet_by)
    split_by <- check_columns(data, split_by, force_factor = TRUE, allow_multi = TRUE, concat_multi = TRUE, concat_sep = split_by_sep)

    if (!is.null(split_by)) {
        datas <- split(data, data[[split_by]])
        # keep the order of levels
        datas <- datas[levels(data[[split_by]])]
    } else {
        datas <- list(data)
    }

    plots <- lapply(
        datas, DotPlotAtomic,
        x = x, y = y, x_sep = x_sep, y_sep = y_sep, flip = flip, bg_direction = bg_direction,
        size_by = size_by, fill_by = fill_by, fill_cutoff = fill_cutoff, fill_reverse = fill_reverse,
        theme = theme, theme_args = theme_args, palette = palette, palcolor = palcolor, alpha = alpha,
        facet_by = facet_by, facet_scales = facet_scales, facet_ncol = facet_ncol, facet_nrow = facet_nrow, facet_byrow = facet_byrow,
        x_text_angle = x_text_angle, size_name = size_name, fill_name = fill_name, fill_cutoff_name = fill_cutoff_name,
        add_bg = add_bg, bg_palette = bg_palette, bg_palcolor = bg_palcolor, bg_alpha = bg_alpha,
        aspect.ratio = aspect.ratio, legend.position = legend.position, legend.direction = legend.direction,
        title = title, subtitle = subtitle, xlab = xlab, ylab = ylab, keep_empty = keep_empty, ...
    )

    combine_plots(plots, combine = combine, nrow = nrow, ncol = ncol, byrow = byrow)
}

#' @rdname dotplot
#' @inheritParams DotPlot
#' @inheritParams common_args
#' @param x A character vector specifying the column to use for the x-axis.
#'  Must be a numeric column.
#' @param y A character vector specifying the column to use for the y-axis.
#'  Must be a numeric column.
#' @return A ggplot object or wrap_plots object or a list of ggplot objects
#' @export
#' @examples
#' ScatterPlot(mtcars, x = "qsec", y = "drat", size_by = "wt",
#'             fill_by = "mpg")
#' ScatterPlot(mtcars, x = "qsec", y = "drat", size_by = "wt",
#'             fill_by = "mpg", fill_cutoff = 18)
ScatterPlot <- function(
    data, x, y, flip = FALSE,
    split_by = NULL, split_by_sep = "_", size_name = NULL, fill_name = NULL, fill_cutoff_name = NULL,
    size_by = NULL, fill_by = NULL, fill_cutoff = NULL, fill_reverse = FALSE,
    theme = "theme_this", theme_args = list(), palette = "Spectral", palcolor = NULL, alpha = 1,
    facet_by = NULL, facet_scales = "fixed", facet_ncol = NULL, facet_nrow = NULL, facet_byrow = TRUE,
    x_text_angle = 0, seed = 8525, aspect.ratio = 1, legend.position = "right", legend.direction = "vertical",
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL, keep_empty = FALSE,
    combine = TRUE, nrow = NULL, ncol = NULL, byrow = TRUE, ...
) {
    validate_common_args(seed, facet_by = facet_by)
    split_by <- check_columns(data, split_by, force_factor = TRUE, allow_multi = TRUE, concat_multi = TRUE, concat_sep = split_by_sep)
    x <- check_columns(data, x)
    y <- check_columns(data, y)
    if (!is.numeric(data[[x]]) || !is.numeric(data[[y]])) {
        stop("Both 'x' and 'y' must be numeric.")
    }

    if (!is.null(split_by)) {
        datas <- split(data, data[[split_by]])
        # keep the order of levels
        datas <- datas[levels(data[[split_by]])]
    } else {
        datas <- list(data)
    }

    plots <- lapply(
        datas, DotPlotAtomic,
        x = x, y = y, x_sep = x_sep, y_sep = y_sep, flip = flip,
        size_by = size_by, fill_by = fill_by, fill_cutoff = fill_cutoff, fill_reverse = fill_reverse,
        theme = theme, theme_args = theme_args, palette = palette, palcolor = palcolor, alpha = alpha,
        facet_by = facet_by, facet_scales = facet_scales, facet_ncol = facet_ncol, facet_nrow = facet_nrow, facet_byrow = facet_byrow,
        x_text_angle = x_text_angle, size_name = size_name, fill_name = fill_name, fill_cutoff_name = fill_cutoff_name,
        aspect.ratio = aspect.ratio, legend.position = legend.position, legend.direction = legend.direction,
        title = title, subtitle = subtitle, xlab = xlab, ylab = ylab, keep_empty = keep_empty, ...
    )

    combine_plots(plots, combine = combine, nrow = nrow, ncol = ncol, byrow = byrow)
}

#' @rdname dotplot
#' @inheritParams DotPlot
#' @inheritParams common_args
#' @param x A character vector specifying the column to use for the x-axis.
#'  A numeric column is expected.
#' @param y A character vector specifying the column to use for the y-axis.
#'  A factor/character column is expected.
#' @return A ggplot object or wrap_plots object or a list of ggplot objects
#' @export
#' @examples
#' LollipopPlot(mtcars, x = "qsec", y = "drat", size_by = "wt",
#'              fill_by = "mpg")
#' LollipopPlot(mtcars, x = "qsec", y = "drat", size_by = "wt",
#'              fill_by = "mpg", fill_cutoff = 18, facet_by = "cyl",
#'              facet_scales = "free_y")
LollipopPlot <- function(
    data, x, y, flip = FALSE,
    split_by = NULL, split_by_sep = "_", size_name = NULL, fill_name = NULL, fill_cutoff_name = NULL,
    size_by = NULL, fill_by = NULL, fill_cutoff = NULL, fill_reverse = FALSE,
    theme = "theme_this", theme_args = list(), palette = "Spectral", palcolor = NULL, alpha = 1,
    facet_by = NULL, facet_scales = "fixed", facet_ncol = NULL, facet_nrow = NULL, facet_byrow = TRUE,
    x_text_angle = 0, seed = 8525, aspect.ratio = 1, legend.position = "right", legend.direction = "vertical",
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL, keep_empty = FALSE,
    combine = TRUE, nrow = NULL, ncol = NULL, byrow = TRUE, ...
) {
    validate_common_args(seed, facet_by = facet_by)
    split_by <- check_columns(data, split_by, force_factor = TRUE, allow_multi = TRUE, concat_multi = TRUE, concat_sep = split_by_sep)

    if (!is.null(split_by)) {
        datas <- split(data, data[[split_by]])
        # keep the order of levels
        datas <- datas[levels(data[[split_by]])]
    } else {
        datas <- list(data)
    }

    plots <- lapply(
        datas, DotPlotAtomic, lollipop = TRUE,
        x = x, y = y, x_sep = x_sep, y_sep = y_sep, flip = flip,
        size_by = size_by, fill_by = fill_by, fill_cutoff = fill_cutoff, fill_reverse = fill_reverse,
        theme = theme, theme_args = theme_args, palette = palette, palcolor = palcolor, alpha = alpha,
        facet_by = facet_by, facet_scales = facet_scales, facet_ncol = facet_ncol, facet_nrow = facet_nrow, facet_byrow = facet_byrow,
        x_text_angle = x_text_angle, size_name = size_name, fill_name = fill_name, fill_cutoff_name = fill_cutoff_name,
        aspect.ratio = aspect.ratio, legend.position = legend.position, legend.direction = legend.direction,
        title = title, subtitle = subtitle, xlab = xlab, ylab = ylab, keep_empty = keep_empty, ...
    )

    combine_plots(plots, combine = combine, nrow = nrow, ncol = ncol, byrow = byrow)
}
