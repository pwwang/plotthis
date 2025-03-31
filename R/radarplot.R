#' Atomic Radar plot
#'
#' @inheritParams common_args
#' @param x A character string of the column name to plot on the x-axis/circles.
#'  A character/factor column is expected.
#' @param x_sep A character string to concatenate the columns in `x`, if multiple columns are provided.
#' @param group_by A character string of the column name(s) to group the data (the lines) by.
#'  Character/factor column(s) is expected.
#' @param group_by_sep A character string to concatenate the columns in `group_by`, if multiple columns are provided.
#' @param y A character string of the column name to plot on the y-axis.
#'  A numeric column is expected.
#'  If NULL, the count of the x-axis column in each group will be used.
#' @param group_name A character string to name the legend of group.
#' @param scale_y How should the y-axis be scaled? Default is "group".
#'  Other options are "global", "x" and "none".
#'  * If "group", the y-axis will be scaled to the fraction within each group.
#'  * If "global", the y-axis will be scaled to the fraction of the total.
#'  * If "x", the y-axis will be scaled to the fraction of the total within each x-axis group.
#'  * If "none", the y-axis will be scaled to the count of each x-axis group.
#' @param y_min A numeric value to set the minimum value of the y-axis.
#' @param y_max A numeric value to set the maximum value of the y-axis.
#' @param y_nbreaks A numeric value to set the number of breaks in the y-axis.
#' @param polygon A logical value to draw the polygons instead of the circles as panel grid.
#' @param fill A logical value to fill the polygons with colors.
#' @param linewidth A numeric value to set the width of the lines.
#' @param pt_size A numeric value to set the size of the points.
#' @param max_charwidth A numeric value to set the maximum character width for the x labels.
#' @return A ggplot object
#' @keywords internal
#' @importFrom rlang syms
#' @importFrom dplyr summarise n %>% distinct ungroup
#' @importFrom tidyr complete
#' @importFrom ggplot2 geom_polygon geom_point geom_text scale_y_continuous scale_x_discrete scale_fill_manual
#' @importFrom ggplot2 scale_color_manual coord_polar labs theme element_blank element_line element_text element_rect
#' @importFrom ggplot2 ggproto CoordPolar waiver
RadarPlotAtomic <- function(
    data, x, x_sep = "_", group_by = NULL, group_by_sep = "_", y = NULL, group_name = NULL,
    scale_y = c("group", "global", "x", "none"), y_min = 0, y_max = NULL, y_nbreaks = 4,
    polygon = FALSE, fill = TRUE, linewidth = 1, pt_size = 4, max_charwidth = 16,
    theme = "theme_this", theme_args = list(), palette = "Paired", palcolor = NULL,
    facet_by = NULL, facet_scales = "fixed", facet_ncol = NULL, facet_nrow = NULL, facet_byrow = TRUE,
    alpha = 0.2, aspect.ratio = 1, legend.position = waiver(), legend.direction = "vertical",
    title = NULL, subtitle = NULL, ...) {
    ggplot <- if (getOption("plotthis.gglogger.enabled", FALSE)) {
        gglogger::ggplot
    } else {
        ggplot2::ggplot
    }
    x <- check_columns(data, x, force_factor = TRUE, allow_multi = TRUE, concat_multi = TRUE, concat_sep = x_sep)
    data[[x]] <- droplevels(data[[x]])
    y <- check_columns(data, y)
    group_by <- check_columns(data, group_by, force_factor = TRUE, allow_multi = TRUE, concat_multi = TRUE, concat_sep = group_by_sep)
    facet_by <- check_columns(data, facet_by, force_factor = TRUE, allow_multi = TRUE)

    if (is.null(y)) {
        y <- ".count"
        data <- data %>%
            dplyr::group_by(!!!syms(unique(c(x, group_by, facet_by)))) %>%
            summarise(.count = n(), .groups = "drop")
    }

    if (is.null(group_by)) {
        data$.group <- factor("")
        group_by <- ".group"
        legend.position <- ifelse(inherits(legend.position, "waiver"), "none", "right")
    } else {
        legend.position <- ifelse(inherits(legend.position, "waiver"), "right", legend.position)
    }

    scale_y <- match.arg(scale_y)
    if (scale_y == "group") {
        data <- data %>%
            dplyr::group_by(!!!syms(unique(c(group_by, facet_by)))) %>%
            mutate(!!sym(y) := !!sym(y) / sum(!!sym(y)))
    } else if (scale_y == "global") {
        if (!is.null(facet_by)) {
            data <- data %>%
                dplyr::group_by(!!!syms(unique(facet_by))) %>%
                mutate(!!sym(y) := !!sym(y) / sum(!!sym(y)))
        } else {
            data <- data %>%
                mutate(!!sym(y) := !!sym(y) / sum(!!sym(y)))
        }
    } else if (scale_y == "x") {
        data <- data %>%
            dplyr::group_by(!!!syms(unique(c(x, facet_by)))) %>%
            mutate(!!sym(y) := !!sym(y) / sum(!!sym(y)))
    }

    # We need all levels of x in each group and facet
    # fill the y value with 0 if the level is missing
    complete_fill = list(0)
    names(complete_fill) = y
    data <- data %>%
        dplyr::group_by(!!!syms(unique(c(group_by, facet_by)))) %>%
        complete(!!sym(x), fill = complete_fill) %>%
        ungroup()

    coord_radar <- function(theta = "x", start = 0, direction = 1) {
        theta <- match.arg(theta, c("x", "y"))
        r <- if (theta == "x") "y" else "x"
        ggproto(
            "CoordRadar", CoordPolar,
            theta = theta, r = r, start = start, clip = "off",
            direction = sign(direction),
            is_linear = function(coord) TRUE
        )
    }

    y_min <- y_min %||% min(data[[y]])
    y_max <- y_max %||% max(data[[y]])
    breaks <- seq(y_min, y_max, length.out = y_nbreaks)

    p <- ggplot(data, aes(x = !!sym(x), y = !!sym(y), group = !!sym(group_by), color = !!sym(group_by)))
    if (isTRUE(polygon)) {
        for (i in seq_along(breaks)) {
            p <- p + geom_polygon(y = breaks[i], fill = "transparent", color = "grey80", linetype = 2)
        }
    }

    if (isTRUE(fill)) {
        p <- p + geom_polygon(aes(fill = !!sym(group_by)), alpha = alpha, linewidth = linewidth)
    } else {
        p <- p + geom_polygon(alpha = alpha, linewidth = linewidth, fill = "transparent")
    }

    if (!is.null(facet_by)) {
        labels_df <- data %>% select(!!!syms(facet_by)) %>% distinct()
        labels_df <- expand_grid(labels_df, breaks)
    } else {
        labels_df <- data.frame(breaks = breaks)
    }

    mc <- min(max(nchar(levels(data[[x]]))), max_charwidth)
    panel.spacing <- theme_args$panel.spacing %||% unit(mc * 2, "points")
    theme_args$panel.spacing <- NULL
    p <- p +
        geom_point(aes(colour = !!sym(group_by), fill = !!sym(group_by)), size = pt_size, shape = 21) +
        geom_text(
            data = labels_df,
            mapping = aes(y = !!sym("breaks"),
                label = if (scale_y == "none") scales::number(!!sym("breaks")) else scales::percent(!!sym("breaks"))),
            x = pi / (length(breaks) - 0.88), color = "grey20", inherit.aes = FALSE) +
        scale_y_continuous(limits = c(y_min, y_max), breaks = breaks, expand = c(.1, 0, 0, 0)) +
        scale_x_discrete(labels = scales::label_wrap(max_charwidth)) +
        scale_fill_manual(
            name = group_name %||% group_by,
            values = palette_this(levels(data[[group_by]]), palette = palette, palcolor = palcolor)
        ) +
        scale_color_manual(
            values = palette_this(levels(data[[group_by]]), palette = palette, palcolor = palcolor)
        ) +
        coord_radar(start = -pi / nlevels(data[[x]])) +
        labs(title = title, subtitle = subtitle) +
        do.call(theme, theme_args) +
        ggplot2::theme(
            axis.text.y = element_blank(),
            axis.ticks = element_blank(),
            axis.title = element_blank(),
            panel.grid = element_blank(),
            panel.border = element_blank(),
            panel.spacing = panel.spacing,
            panel.grid.major.x = element_line(colour = "grey80", linetype = 1)
        )

    if (isFALSE(polygon)) {
        p <- p + ggplot2::theme(panel.grid.major.y = element_line(
            colour = c(rep("grey80", length(breaks)), NA), linetype = 2))
    }

    height <- 4.5
    width <- 4.5
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

    facet_plot(p, facet_by, facet_scales, facet_nrow, facet_ncol, facet_byrow)
}

#' Radar plot / Spider plot
#'
#' @description Create a radar plot or spider plot for a series of data.
#'   Radar plot uses circles as the plot grid and Spider plot uses polygons.
#' @rdname radarplot
#' @inheritParams common_args
#' @inheritParams RadarPlotAtomic
#' @importFrom ggplot2 waiver
#' @return A ggplot object or wrap_plots object or a list of ggplot objects
#' @export
RadarPlot <- function(
    data, x, x_sep = "_", group_by = NULL, group_by_sep = "_", y = NULL, group_name = NULL,
    scale_y = c("group", "global", "x", "none"), y_min = 0, y_max = NULL, y_nbreaks = 4,
    fill = TRUE, linewidth = 1, pt_size = 4, max_charwidth = 16, split_by = NULL, split_by_sep = "_",
    theme = "theme_this", theme_args = list(), palette = "Paired", palcolor = NULL,
    facet_by = NULL, facet_scales = "fixed", facet_ncol = NULL, facet_nrow = NULL, facet_byrow = TRUE,
    alpha = 0.2, aspect.ratio = 1, legend.position = waiver(), legend.direction = "vertical",
    title = NULL, subtitle = NULL, seed = 8525, combine = TRUE, nrow = NULL, ncol = NULL, byrow = TRUE,
    axes = NULL, axis_titles = axes, guides = NULL, ...) {

    validate_common_args(seed, facet_by = facet_by)
    theme <- process_theme(theme)
    split_by <- check_columns(data, split_by, force_factor = TRUE, allow_multi = TRUE, concat_multi = TRUE, concat_sep = split_by_sep)

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
            RadarPlotAtomic(datas[[nm]],
                x = x, x_sep = x_sep, group_by = group_by, group_by_sep = group_by_sep, y = y, group_name = group_name,
                scale_y = scale_y, y_min = y_min, y_max = y_max, y_nbreaks = y_nbreaks, polygon = FALSE,
                fill = fill, linewidth = linewidth, pt_size = pt_size, max_charwidth = max_charwidth,
                theme = theme, theme_args = theme_args, palette = palette[[nm]], palcolor = palcolor[[nm]],
                facet_by = facet_by, facet_scales = facet_scales, facet_ncol = facet_ncol, facet_nrow = facet_nrow, facet_byrow = facet_byrow,
                alpha = alpha, aspect.ratio = aspect.ratio, legend.position = legend.position[[nm]], legend.direction = legend.direction[[nm]],
                title = title, subtitle = subtitle, ...
            )
        }
    )

    combine_plots(plots, combine = combine, nrow = nrow, ncol = ncol, byrow = byrow,
        axes = axes, axis_titles = axis_titles, guides = guides)
}

#' @rdname radarplot
#' @inheritParams RadarPlot
#' @export
#' @importFrom ggplot2 waiver
#' @examples
#' # use the count
#' data <- data.frame(
#'    x = c(rep("A", 2), rep("B", 3), rep("C", 3), rep("D", 4), rep("E", 5)),
#'    group = sample(paste0("G", 1:4), 17, replace = TRUE)
#' )
#' RadarPlot(data, x = "x")
#' RadarPlot(data, x = "x", scale_y = "none")
#' RadarPlot(data, x = "x", group_by = "group")
#' SpiderPlot(data, x = "x")
#' SpiderPlot(data, x = "x", group_by = "group")
#'
#' # use the y value
#' data <- data.frame(
#'    x = rep(LETTERS[1:5], 2),
#'    y = c(1, 3, 6, 4, 2, 5, 7, 8, 9, 10),
#'    group = rep(c("G1", "G2"), each = 5)
#' )
#' RadarPlot(data, x = "x", y = "y", scale_y = "none", group_by = "group")
#' RadarPlot(data, x = "x", y = "y", facet_by = "group")
#' RadarPlot(data, x = "x", y = "y", split_by = "group")
#' RadarPlot(data, x = "x", y = "y", split_by = "group",
#'           palette = c(G1 = "Set1", G2 = "Paired"))
SpiderPlot <- function(
    data, x, x_sep = "_", group_by = NULL, group_by_sep = "_", y = NULL, group_name = NULL,
    scale_y = c("group", "global", "x", "none"), y_min = 0, y_max = NULL, y_nbreaks = 4,
    fill = TRUE, linewidth = 1, pt_size = 4, max_charwidth = 16, split_by = NULL, split_by_sep = "_",
    theme = "theme_this", theme_args = list(), palette = "Paired", palcolor = NULL,
    facet_by = NULL, facet_scales = "fixed", facet_ncol = NULL, facet_nrow = NULL, facet_byrow = TRUE,
    alpha = 0.2, aspect.ratio = 1, legend.position = waiver(), legend.direction = "vertical",
    title = NULL, subtitle = NULL, seed = 8525, combine = TRUE, nrow = NULL, ncol = NULL, byrow = TRUE,
    axes = NULL, axis_titles = axes, guides = NULL, ...) {

    validate_common_args(seed, facet_by = facet_by)
    theme <- process_theme(theme)
    split_by <- check_columns(data, split_by, force_factor = TRUE, allow_multi = TRUE, concat_multi = TRUE, concat_sep = split_by_sep)

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
            RadarPlotAtomic(datas[[nm]],
                x = x, x_sep = x_sep, group_by = group_by, group_by_sep = group_by_sep, y = y, group_name = group_name,
                scale_y = scale_y, y_min = y_min, y_max = y_max, y_nbreaks = y_nbreaks, polygon = TRUE,
                fill = fill, linewidth = linewidth, pt_size = pt_size, max_charwidth = max_charwidth,
                theme = theme, theme_args = theme_args, palette = palette[[nm]], palcolor = palcolor[[nm]],
                facet_by = facet_by, facet_scales = facet_scales, facet_ncol = facet_ncol, facet_nrow = facet_nrow, facet_byrow = facet_byrow,
                alpha = alpha, aspect.ratio = aspect.ratio, legend.position = legend.position[[nm]], legend.direction = legend.direction[[nm]],
                title = title, subtitle = subtitle, ...
            )
        }
    )

    combine_plots(plots, combine = combine, nrow = nrow, ncol = ncol, byrow = byrow,
        axes = axes, axis_titles = axis_titles, guides = guides)
}
