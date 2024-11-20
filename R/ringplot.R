#' RingPlotAtomic
#'
#' Ring plot for atomic data
#'
#' @inheritParams common_args
#' @param x A character vector specifying the column as the rings of the plot.
#' @param y A character vector specifying the column as the y axis of the plot.
#'   Default is NULL, meaning the y axis is the count of the data.
#' @param group_by A character vector specifying the column as the group_by of the plot.
#'   How the ring is divided.
#' @param group_by_sep A character string to concatenate the columns in `group_by`, if multiple columns are provided.
#' @param group_name A character string to specify the name of the group_by in the legend.
#' @param label A logical value indicating whether to show the labels on the rings.
#'   The labels should be the values of group_by. Default is NULL, meaning no labels for one ring and
#'   showing the labels for multiple rings.
#' @param clockwise A logical value to draw the ring plot clockwise or not.
#' @return A ggplot object
#' @importFrom rlang sym syms
#' @importFrom dplyr %>% summarise n
#' @importFrom ggplot2 geom_col scale_fill_manual scale_x_discrete geom_label
#' @keywords internal
RingPlotAtomic <- function(
    data, x = NULL, y = NULL, group_by = NULL, group_by_sep = "_",  group_name = NULL,
    label = NULL, clockwise = TRUE,
    facet_by = NULL, facet_scales = "free_y", facet_ncol = NULL, facet_nrow = NULL, facet_byrow = TRUE,
    theme = "theme_this", theme_args = list(), palette = "Paired", palcolor = NULL,
    alpha = 1, aspect.ratio = 1, legend.position = "right", legend.direction = "vertical",
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL, keep_empty = FALSE,
    seed = 8525, ...
) {
    ggplot <- if (getOption("plotthis.gglogger.enabled", FALSE)) {
        gglogger::ggplot
    } else {
        ggplot2::ggplot
    }
    base_size <- theme_args$base_size %||% 12
    text_size_scale <- base_size / 12

    if (is.null(x)) {
        x <- ".x"
        data$.x <- 1
    }
    x <- check_columns(data, x, force_factor = TRUE)
    group_by <- check_columns(data, group_by, force_factor = TRUE, allow_multi = TRUE, concat_multi = TRUE, concat_sep = group_by_sep)
    facet_by <- check_columns(data, facet_by, force_factor = TRUE, allow_multi = TRUE)

    if (is.null(y)) {
        data <- data %>% dplyr::group_by(!!!syms(unique(c(x, group_by, facet_by)))) %>% summarise(.y = n(), .groups = "drop")
        y <- ".y"
    } else {
        y <- check_columns(data, y)
    }
    # make sure each ring sums to 1
    data <- data %>% dplyr::group_by(!!!syms(unique(c(x, facet_by)))) %>% mutate(!!sym(y) := !!sym(y) / sum(!!sym(y)))

    rings <- rev(levels(data[[x]]))
    if (length(rings) == 1 && is.null(label)) {
        label <- FALSE
    } else if (length(rings) > 1 && is.null(label)) {
        label <- TRUE
    }
    if (isFALSE(keep_empty)) {
        data[[x]] <- droplevels(data[[x]])
    } else {
        # fill y with 0 for empty group_by. 'drop' with scale_fill_* doesn't have color for empty group_by
        fill_list <- list(0)
        names(fill_list) <- y
        data <- data %>%
            dplyr::group_by(!!!syms(unique(c(x, facet_by)))) %>%
            complete(!!sym(group_by), fill = fill_list)
    }

    if (isTRUE(clockwise)) {
        data[[group_by]] <- factor(data[[group_by]], levels = rev(levels(data[[group_by]])))
        colors <- palette_this(rev(levels(data[[group_by]])), palette = palette, palcolor = palcolor)
    } else {
        colors <- palette_this(levels(data[[group_by]]), palette = palette, palcolor = palcolor)
    }
    data <- data[order(data[[group_by]]), , drop = FALSE]

    p = ggplot(data, aes(x = !!sym(x), y = !!sym(y), fill = !!sym(group_by))) +
        geom_col(width = 0.9, color = "white", alpha = alpha) +
        coord_polar("y", start = 0) +
        scale_fill_manual(name = group_name %||% group_by, values = colors, guide = guide_legend(reverse = clockwise)) +
        scale_x_discrete(limits = c(" ", rings)) +
        do.call(theme, theme_args) +
        ggplot2::theme(
            aspect.ratio = aspect.ratio,
            legend.position = legend.position,
            legend.direction = legend.direction,
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            axis.title = element_blank(),
            panel.border = element_blank()
        ) +
        labs(title = title, subtitle = subtitle, x = xlab %||% "", y = ylab %||% "")

    if (label) {
        p <- p + geom_label(
            aes(label = !!sym(x), x = !!sym(x), y = 0),
            # data = label_df,
            inherit.aes = FALSE,
            color = "grey20",
            size = text_size_scale * 3
        )
    }

    height <- 4.5
    width <- 4.5
    if (!inherits(legend.position, "none")) {
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
    facet_plot(p, facet_by, facet_scales, facet_nrow, facet_ncol, facet_byrow)
}

#' Ring Plot
#'
#' @description A ring plot is like pie chart but with multiple rings.
#' @seealso \code{\link{PieChart}}
#' @inheritParams RingPlotAtomic
#' @inheritParams common_args
#' @return A ggplot object or wrap_plots object or a list of ggplot objects
#' @export
#' @examples
#' RingPlot(datasets::iris, group_by = "Species")
#'
#' data <- data.frame(
#'   x = c("A", "B", "C", "A", "B", "C"),
#'   y = c(1, 2, 3, 4, 5, 6),
#'   group = c("a", "a", "a", "b", "b", "b")
#' )
#' RingPlot(data, x = "x", y = "y", group_by = "group")
#' RingPlot(datasets::mtcars, x = "cyl", group_by = "carb", facet_by = "vs")
#' RingPlot(datasets::mtcars, x = "cyl", group_by = "carb", split_by = "vs")
RingPlot <- function(
    data, x = NULL, y = NULL, group_by = NULL, group_by_sep = "_", group_name = NULL,
    label = NULL, split_by = NULL, split_by_sep = "_",
    facet_by = NULL, facet_scales = "free_y", facet_ncol = NULL, facet_nrow = NULL, facet_byrow = TRUE,
    theme = "theme_this", theme_args = list(), palette = "Paired", palcolor = NULL,
    alpha = 1, aspect.ratio = 1,
    legend.position = "right", legend.direction = "vertical",
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL, keep_empty = FALSE,
    combine = TRUE, nrow = NULL, ncol = NULL, byrow = TRUE,
    seed = 8525, ...
) {
    validate_common_args(seed, facet_by = facet_by)
    theme <- process_theme(theme)
    split_by <- check_columns(data, split_by, force_factor = TRUE, allow_multi = TRUE,
        concat_multi = TRUE, concat_sep = split_by_sep)

    if (!is.null(split_by)) {
        datas <- split(data, data[[split_by]])
        # keep the order of levels
        datas <- datas[levels(data[[split_by]])]
        palette <- check_palette(palette, names(datas))
        palcolor <- check_palcolor(palcolor, names(datas))
    } else {
		datas <- list(data)
        palette <- list(palette)
        names(datas) <- "..."
        names(palette) <- "..."
        if (!is.null(palcolor)) {
	        palcolor <- list(palcolor)
            palcolor <- check_palcolor(palcolor, "...")
        }
    }

    plots <- lapply(
        names(datas), function(nm) {
            default_title <- if (length(datas) == 1 && identical(nm, "...")) NULL else nm
            if (is.function(title)) {
                title <- title(default_title)
            } else {
                title <- title %||% default_title
            }
            RingPlotAtomic(datas[[nm]],
                x = x, y = y, label = label, group_by = group_by, group_by_sep = group_by_sep, group_name = group_name,
                facet_by = facet_by, facet_scales = facet_scales, facet_ncol = facet_ncol, facet_nrow = facet_nrow, facet_byrow = facet_byrow,
                theme = theme, theme_args = theme_args, palette = palette[[nm]], palcolor = palcolor[[nm]],
                alpha = alpha, aspect.ratio = aspect.ratio,
                legend.position = legend.position, legend.direction = legend.direction,
                title = title, subtitle = subtitle, xlab = xlab, ylab = ylab, keep_empty = keep_empty,
                seed = seed, ...
            )
        }
    )

    combine_plots(plots, combine = combine, nrow = nrow, ncol = ncol, byrow = byrow)
}