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
#'   For 'ScatterPlot', it can be a single numeric value to specify the size of the dots.
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
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL, keep_na = FALSE, keep_empty = FALSE, ...
) {
    ggplot <- if (getOption("plotthis.gglogger.enabled", FALSE)) {
        gglogger::ggplot
    } else {
        ggplot2::ggplot
    }
    bg_direction <- match.arg(bg_direction)
    if (bg_direction %in% c("h", "horizontal")) {
        bg_direction <- "horizontal"
    } else {
        bg_direction <- "vertical"
    }
    x_is_numeric <- length(x) == 1 && !is.character(data[[x]]) && !is.factor(data[[x]])
    y_is_numeric <- length(y) == 1 && !is.character(data[[y]]) && !is.factor(data[[y]])
    if (!x_is_numeric) {
        x <- check_columns(data, x, force_factor = TRUE, allow_multi = TRUE, concat_multi = TRUE, concat_sep = x_sep)
    }
    if (!y_is_numeric) {
        y <- check_columns(data, y, force_factor = TRUE, allow_multi = TRUE, concat_multi = TRUE, concat_sep = y_sep)
    }
    fill_by <- check_columns(data, fill_by)
    facet_by <- check_columns(data, facet_by, force_factor = TRUE, allow_multi = TRUE)

    data <- process_keep_na_empty(data, keep_na, keep_empty)
    keep_empty_x <- keep_empty[[x]]
    keep_empty_y <- keep_empty[[y]]
    keep_empty_facet <- if (!is.null(facet_by)) keep_empty[[facet_by[1]]] else NULL
    if (length(facet_by) > 1) {
        stopifnot("[DotPlot/LillipopPlot] `keep_empty` for `facet_by` variables must be identical." =
            identical(keep_empty_facet, keep_empty[[facet_by[2]]]))
    }

    if (!is.null(fill_cutoff) && is.null(fill_by)) {
        stop("[DotPlot/LollipopPlot]'fill_by' must be provided when 'fill_cutoff' is specified.")
    }

    if (!is.numeric(size_by)) {
        size_by <- check_columns(data, size_by)
    }
    if (is.null(size_by)) {
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
        # keep the levels of x, y, and facet_by
        for (col in unique(c(x, y, facet_by))) {
            if (is.factor(data[[col]])) {
                data[[col]] <- factor(data[[col]], levels = levels(data[[col]]))
            }
        }
        size_by <- ".size"
    }

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
            p <- p + bg_layer(data, x, isTRUE(keep_empty_x), bg_palette, bg_palcolor, bg_alpha, facet_by, bg_direction)
        } else {
            if (y_is_numeric) {
                stop("Horizontal 'bg_direction' is not supported when 'y' is numeric.")
            }
            p <- p + bg_layer(data, y, isTRUE(keep_empty_y), bg_palette, bg_palcolor, bg_alpha, facet_by, bg_direction)
        }
    }
    if (!x_is_numeric) {
        p <- p + scale_x_discrete(drop = !isTRUE(keep_empty_x))
    }
    if (!y_is_numeric) {
        p <- p + scale_y_discrete(drop = !isTRUE(keep_empty_y))
    }

    if (isTRUE(lollipop)) {
        p <- p +
            geom_segment(aes(x = 0, xend = !!sym(x), yend = !!sym(y)), color = "black", linewidth = 2) +
            geom_segment(aes(x = 0, xend = !!sym(x), yend = !!sym(y), color = !!sym(fill_by)), linewidth = 1) +
            scale_x_continuous(expand = c(0, 0, 0.05, 0)) +
            scale_color_gradientn(
                n.breaks = 5,
                colors = palette_this(palette = palette, palcolor = palcolor, reverse = fill_reverse),
                na.value = "grey80",
                guide = "none"
            ) +
            new_scale_color()
    }
    if (is.numeric(size_by)) {
        p <- p + geom_point(aes(fill = !!sym(fill_by), color = ""), size = size_by, shape = 21, alpha = alpha)
    } else {
        p <- p + geom_point(aes(size = !!sym(size_by), fill = !!sym(fill_by), color = ""), shape = 21, alpha = alpha) +
            scale_size_area(max_size = 6, n.breaks = 4) +
            guides(size = guide_legend(
                title = size_name %||% size_by,
                override.aes = list(fill = "transparent", shape = 21), order = 1))
    }

    p <- p +
        scale_fill_gradientn(
            n.breaks = 5,
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
        ggplot2::theme(
            aspect.ratio = aspect.ratio,
            legend.position = legend.position,
            legend.direction = legend.direction,
            panel.grid.major = element_line(colour = "grey80", linetype = 2),
            axis.text.x = element_text(angle = x_text_angle, hjust = just$h, vjust = just$v)
        )

    p <- p + scale_color_manual(values = "black", na.value = "black", guide = "none")
    if (!is.null(fill_by) && !is.null(fill_cutoff) && anyNA(data[[fill_by]])) {
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
    } else if (isTRUE(keep_empty_x)) {
        nx <- nlevels(data[[x]])
    } else {
        nx <- nlevels(droplevels(data[[x]]))
    }
    if (y_is_numeric) {
        ny <- 5
    } else if (isTRUE(keep_empty_y)) {
        ny <- nlevels(data[[y]])
    } else {
        ny <- nlevels(droplevels(data[[y]]))
    }

    if (ny / nx > 10) {
        if (aspect.ratio <= 1) {
            message("Two many terms than groups, you may want to set a larger 'aspect.ratio'.")
        }
        height = ny * 0.2
        width = nx * 2
    } else if (ny / nx < 0.1) {
        if (aspect.ratio >= 1) {
            message("Two many groups than terms, you may want to set a smaller 'aspect.ratio'.")
        }
        height = ny * 2
        width = nx * 0.4
    } else {
        height = ny * 0.5
        width = nx * 0.8
    }

    if (!y_is_numeric) {
        y_label_len <- max(sapply(strsplit(levels(data[[y]]), "\n"), function(x) max(nchar(x))))
        width <- width + y_label_len * 0.1
    }
    width <- max(width, 3)
    height <- max(height, 3)

    # Apply aspect.ratio coupling: derive the smaller dimension from the larger one
    ar <- if (isTRUE(flip)) 1 / aspect.ratio else aspect.ratio
    coupled_height <- width * ar
    if (abs(coupled_height - height) / max(height, 1) < 0.5) {
        # Only apply coupling if the adjustment is less than 50% change
        height <- coupled_height
        height <- max(height, 3)
    }

    if (!identical(legend.position, "none")) {
        if (legend.position %in% c("right", "left")) {
            width <- width + 1
        } else if (legend.direction == "horizontal") {
            height <- height + 1
        } else {
            width <- width + 2
        }
    }
    height <- max(height, 3)

    if (isTRUE(flip)) {
        attr(p, "height") <- width
        attr(p, "width") <- height
    } else {
        attr(p, "height") <- height
        attr(p, "width") <- width
    }

    facet_plot(p, facet_by, facet_scales, facet_nrow, facet_ncol, facet_byrow,
        legend.position = legend.position, legend.direction = legend.direction,
        drop = !isTRUE(keep_empty_facet))
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
#' \donttest{
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
#' DotPlot(mtcars, x = "carb", y = "gear", size_by = "wt",
#'         fill_by = "mpg", fill_cutoff = 18, split_by = "cyl",
#'         palette = list("4" = "Set1", "6" = "Paired", "8" = "Reds"))
#' # works as a scatter plot
#' DotPlot(mtcars, x = "qsec", y = "drat", size_by = "wt",
#'         fill_by = "mpg", fill_cutoff = 18, fill_cutoff_name = "Small mpgs")
#' # keep_na and keep_empty
#' mtcars$carb[mtcars$carb == "1"] <- NA
#' mtcars$gear[mtcars$gear == "3"] <- NA
#' DotPlot(mtcars, x = "carb", y = "gear", size_by = "wt",
#'         fill_by = "mpg", fill_cutoff = 18,
#'         keep_na = TRUE, keep_empty = TRUE)
#' }
DotPlot <- function(
    data, x, y, x_sep = "_", y_sep = "_", flip = FALSE,
    split_by = NULL, split_by_sep = "_", size_name = NULL, fill_name = NULL, fill_cutoff_name = NULL,
    add_bg = FALSE, bg_palette = "stripe", bg_palcolor = NULL, bg_alpha = 0.2, bg_direction = c("vertical", "horizontal", "v", "h"),
    size_by = NULL, fill_by = NULL, fill_cutoff = NULL, fill_reverse = FALSE,
    theme = "theme_this", theme_args = list(), palette = "Spectral", palcolor = NULL, alpha = 1,
    facet_by = NULL, facet_scales = "fixed", facet_ncol = NULL, facet_nrow = NULL, facet_byrow = TRUE,
    x_text_angle = 0, seed = 8525, aspect.ratio = 1, legend.position = "right", legend.direction = "vertical",
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL, keep_na = FALSE, keep_empty = FALSE,
    combine = TRUE, nrow = NULL, ncol = NULL, byrow = TRUE, axes = NULL, axis_titles = axes, guides = NULL, design = NULL,
    ...
) {
    validate_common_args(seed, facet_by = facet_by)
    keep_na <- check_keep_na(keep_na, c(x, y, split_by, fill_by, facet_by))
    keep_empty <- check_keep_empty(keep_empty, c(x, y, split_by, fill_by, facet_by))
    theme <- process_theme(theme)
    split_by <- check_columns(data, split_by, force_factor = TRUE, allow_multi = TRUE, concat_multi = TRUE, concat_sep = split_by_sep)

    if (!is.null(split_by)) {
        data <- process_keep_na_empty(data, keep_na, keep_empty, col = split_by)
        keep_na[[split_by]] <- NULL
        keep_empty[[split_by]] <- NULL
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
            DotPlotAtomic(datas[[nm]],
                x = x, y = y, x_sep = x_sep, y_sep = y_sep, flip = flip, bg_direction = bg_direction,
                size_by = size_by, fill_by = fill_by, fill_cutoff = fill_cutoff, fill_reverse = fill_reverse,
                theme = theme, theme_args = theme_args, palette = palette[[nm]], palcolor = palcolor[[nm]], alpha = alpha,
                facet_by = facet_by, facet_scales = facet_scales, facet_ncol = facet_ncol, facet_nrow = facet_nrow, facet_byrow = facet_byrow,
                x_text_angle = x_text_angle, size_name = size_name, fill_name = fill_name, fill_cutoff_name = fill_cutoff_name,
                add_bg = add_bg, bg_palette = bg_palette, bg_palcolor = bg_palcolor, bg_alpha = bg_alpha,
                aspect.ratio = aspect.ratio, legend.position = legend.position[[nm]], legend.direction = legend.direction[[nm]],
                title = title, subtitle = subtitle, xlab = xlab, ylab = ylab, keep_na = keep_na, keep_empty = keep_empty, ...
            )
        }
    )

    combine_plots(plots, combine = combine, nrow = nrow, ncol = ncol, byrow = byrow,
        axes = axes, axis_titles = axis_titles, guides = guides, design = design)
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
#' LollipopPlot(mtcars, x = "qsec", y = "drat", size_by = "wt",
#'              split_by = "vs", palette = list("0" = "Reds", "1" = "Blues"))
LollipopPlot <- function(
    data, x, y, y_sep = NULL, flip = FALSE,
    split_by = NULL, split_by_sep = "_", size_name = NULL, fill_name = NULL, fill_cutoff_name = NULL,
    size_by = NULL, fill_by = NULL, fill_cutoff = NULL, fill_reverse = FALSE,
    theme = "theme_this", theme_args = list(), palette = "Spectral", palcolor = NULL, alpha = 1,
    facet_by = NULL, facet_scales = "fixed", facet_ncol = NULL, facet_nrow = NULL, facet_byrow = TRUE,
    x_text_angle = 0, seed = 8525, aspect.ratio = 1, legend.position = "right", legend.direction = "vertical",
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL, keep_na = FALSE, keep_empty = FALSE,
    combine = TRUE, nrow = NULL, ncol = NULL, byrow = TRUE, axes = NULL, axis_titles = axes, guides = NULL, design = NULL,
    ...
) {
    validate_common_args(seed, facet_by = facet_by)
    keep_na <- check_keep_na(keep_na, c(y, split_by, fill_by, facet_by))
    keep_empty <- check_keep_empty(keep_empty, c(y, split_by, fill_by, facet_by))
    theme <- process_theme(theme)
    split_by <- check_columns(data, split_by, force_factor = TRUE, allow_multi = TRUE, concat_multi = TRUE, concat_sep = split_by_sep)

    if (!is.null(split_by)) {
        data <- process_keep_na_empty(data, keep_na, keep_empty, col = split_by)
        keep_na[[split_by]] <- NULL
        keep_empty[[split_by]] <- NULL
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
            DotPlotAtomic(datas[[nm]], lollipop = TRUE,
                x = x, y = y, x_sep = NULL, y_sep = y_sep, flip = flip,
                size_by = size_by, fill_by = fill_by, fill_cutoff = fill_cutoff, fill_reverse = fill_reverse,
                theme = theme, theme_args = theme_args, palette = palette[[nm]], palcolor = palcolor[[nm]], alpha = alpha,
                facet_by = facet_by, facet_scales = facet_scales, facet_ncol = facet_ncol, facet_nrow = facet_nrow, facet_byrow = facet_byrow,
                x_text_angle = x_text_angle, size_name = size_name, fill_name = fill_name, fill_cutoff_name = fill_cutoff_name,
                aspect.ratio = aspect.ratio, legend.position = legend.position[[nm]], legend.direction = legend.direction[[nm]],
                title = title, subtitle = subtitle, xlab = xlab, ylab = ylab, keep_na = keep_na, keep_empty = keep_empty, ...
            )
        }
    )

    combine_plots(plots, combine = combine, nrow = nrow, ncol = ncol, byrow = byrow,
        axes = axes, axis_titles = axis_titles, guides = guides, design = design)
}
