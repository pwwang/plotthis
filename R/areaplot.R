#' Atomic area plot
#'
#' @inheritParams common_args
#' @param x A character string of the column name to plot on the x-axis.
#'  A character/factor column is expected.
#' @param x_sep A character string to concatenate the columns in `x`, if multiple columns are provided.
#' @param y A character string of the column name to plot on the y-axis.
#'  A numeric column is expected.
#'  If NULL, the count of the x-axis column will be used.
#' @param scale_y A logical value to scale the y-axis by the total number in each x-axis group.
#' @param group_by A character vector of column names to fill the area plot by.
#'  If NULL, the plot will be filled by the first color of the palette.
#'  If multiple columns are provided, the columns will be concatenated with
#'  `group_by_sep` and used as the fill column.
#' @param group_by_sep A character string to separate the columns in `group_by`.
#' @param group_name A character string to name the legend of fill.
#' @keywords internal
#' @return A ggplot object
#' @importFrom rlang syms :=
#' @importFrom dplyr summarise n %>%
#' @importFrom ggplot2 geom_area scale_x_discrete scale_y_continuous scale_fill_manual
#' @importFrom ggplot2 labs theme element_line element_text position_stack waiver
AreaPlotAtomic <- function(
    data, x, y = NULL, x_sep = "_", group_by = NULL, group_by_sep = "_", group_name = NULL, scale_y = FALSE,
    theme = "theme_this", theme_args = list(), palette = "Paired", palcolor = NULL, alpha = 1,
    facet_by = NULL, facet_scales = "fixed", facet_ncol = NULL, facet_nrow = NULL, facet_byrow = TRUE,
    x_text_angle = 0, aspect.ratio = 1, legend.position = waiver(), legend.direction = "vertical",
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL, keep_na = FALSE, keep_empty = FALSE, ...
) {
    ggplot <- if (getOption("plotthis.gglogger.enabled", FALSE)) {
        gglogger::ggplot
    } else {
        ggplot2::ggplot
    }
    x <- check_columns(data, x, force_factor = TRUE, allow_multi = TRUE, concat_multi = TRUE, concat_sep = x_sep)
    y <- check_columns(data, y)
    group_by <- check_columns(data, group_by, force_factor = TRUE, allow_multi = TRUE, concat_multi = TRUE, concat_sep = group_by_sep)
    facet_by <- check_columns(data, facet_by, force_factor = TRUE, allow_multi = TRUE)
    data <- process_keep_na_empty(data, keep_na, keep_empty)
    # TRUE: unused levels will be kept on X axis
    # FALSE/level: unused levels will be dropped
    keep_empty_x <- keep_empty[[x]]
    # TRUE: unused levels will be kept in group_by
    # FALSE: unused levels will be dropped
    # level: unused levels will be dropped, but the group colors will be identified using all levels
    keep_empty_group <- if (!is.null(group_by)) keep_empty[[group_by]] else NULL
    # TRUE: unused levels will be kept in facet_by
    # FALSE/level: unused levels will be dropped
    # 2-column facet_by is not supported yet
    keep_empty_facet <- if (!is.null(facet_by)) keep_empty[[facet_by[1]]] else NULL
    if (length(facet_by) > 1) {
        stopifnot("[AreaPlot] `keep_empty` for `facet_by` variables must be identical." =
            identical(keep_empty_facet, keep_empty[[facet_by[2]]]))
    }

    orig_data <- data
    if (is.null(y)) {
        y <- ".count"
        data <- data %>%
            dplyr::group_by(!!!syms(unique(c(x, group_by, facet_by)))) %>%
            summarise(.count = n(), .groups = "drop")
        # keep the levels
        for (col in unique(c(x, group_by, facet_by))) {
            data[[col]] <- factor(data[[col]], levels = levels(orig_data[[col]]))
        }
    }

    if (isTRUE(scale_y)) {
        data <- data %>%
            dplyr::group_by(!!!syms(unique(c(x, facet_by)))) %>%
            mutate(!!sym(y) := !!sym(y) / sum(!!sym(y))) %>%
            ungroup()
        # keep the levels
        for (col in unique(c(x, group_by, facet_by))) {
            data[[col]] <- factor(data[[col]], levels = levels(orig_data[[col]]))
        }
    }
    rm(orig_data)

    if (is.null(group_by)) {
        data$.fill <- factor("")
        group_by <- ".fill"
        legend.position <- ifelse(inherits(legend.position, "waiver"), "none", "right")
    } else {
        legend.position <- ifelse(inherits(legend.position, "waiver"), "right", legend.position)
    }
    if (!isTRUE(keep_empty_x)) {
        data[[x]] <- droplevels(data[[x]])
    }

    x_vals <- levels(data[[x]])
    if (anyNA(data[[x]])) x_vals <- c(x_vals, NA)

    # group_by_vals <- if (!isTRUE(keep_empty_group) || !anyNA(data[[group_by]])) {
    #     levels(data[[group_by]])
    # } else {
    #     c(levels(data[[group_by]]), NA)
    # }
    group_by_vals <- levels(data[[group_by]])
    if (anyNA(data[[group_by]])) group_by_vals <- c(group_by_vals, NA)
    group_colors <- palette_this(group_by_vals, palette = palette, palcolor = palcolor, NA_keep = TRUE)

    just <- calc_just(x_text_angle)

    # Complete all x * group_by (and facet_by) combinations so that geom_area
    # doesn't interpolate across missing groups, which would cause stacked
    # areas to exceed the correct total.
    complete_vars <- unique(c(x, group_by, facet_by))
    complete_fill <- setNames(list(0), y)
    data_complete <- data %>%
        tidyr::complete(!!!syms(complete_vars), fill = complete_fill)
    # Restore factor levels that complete() may have altered
    for (col in complete_vars) {
        if (is.factor(data[[col]])) {
            data_complete[[col]] <- factor(data_complete[[col]], levels = levels(data[[col]]))
        }
    }

    # Convert x to numeric, handling NA values by assigning them the next position
    data_complete$.x_numeric <- as.numeric(data_complete[[x]])
    if (anyNA(data_complete[[x]])) {
        data_complete$.x_numeric[is.na(data_complete[[x]])] <- length(levels(data_complete[[x]])) + 1
    }

    p <- ggplot(data_complete, aes(x = !!sym(".x_numeric"), y = !!sym(y), fill = !!sym(group_by))) +
        geom_area(alpha = alpha, color = "grey50", position = position_stack(vjust = 0.5), show.legend = TRUE) +
        scale_x_discrete(expand = c(0, 0), breaks = x_vals, limits = x_vals, drop = isFALSE(keep_empty_x)) +
        scale_y_continuous(expand = c(0, 0), labels = if (isFALSE(scale_y)) scales::number else scales::percent) +
        labs(title = title, subtitle = subtitle, x = xlab %||% x, y = ylab %||% y) +
        do.call(theme, theme_args) +
        ggplot2::theme(
            aspect.ratio = aspect.ratio,
            legend.position = legend.position,
            legend.direction = legend.direction,
            panel.grid.major = element_line(colour = "grey80", linetype = 2),
            axis.text.x = element_text(angle = x_text_angle, hjust = just$h, vjust = just$v)
        )

    if (isTRUE(keep_empty_group)) {
        p <- p +
            scale_fill_manual(
                name = group_name %||% group_by,
                values = group_colors, na.value = group_colors["NA"] %||% "grey80",
                breaks = group_by_vals, limits = group_by_vals, drop = FALSE
            )
    } else {
        p <- p +
            scale_fill_manual(
                name = group_name %||% group_by,
                values = group_colors, na.value = group_colors["NA"] %||% "grey80"
            )
    }

    # Calculate plot dimensions with aspect ratio consideration
    dims <- calculate_plot_dimensions(
        base_height = 4.5,
        aspect.ratio = aspect.ratio,
        n_x = length(x_vals),
        x_scale_factor = 0.5,
        legend.position = legend.position,
        legend.direction = legend.direction,
        flip = FALSE
    )

    # Fallback to old calculation if dimension calculator is disabled
    if (is.null(dims)) {
        height = 4.5
        width = 0.5 + length(x_vals) * 0.5
        if (!identical(legend.position, "none")) {
            if (legend.position %in% c("right", "left")) {
                width <- width + 1
            } else if (legend.direction == "horizontal") {
                height <- height + 1
            } else {
                width <- width + 2
            }
        }
    } else {
        height <- dims$height
        width <- dims$width
    }

    attr(p, "height") <- height
    attr(p, "width") <- width

    facet_plot(p, facet_by, facet_scales, facet_nrow, facet_ncol, facet_byrow,
        legend.position = legend.position, legend.direction = legend.direction,
        drop = !isTRUE(keep_empty_facet))
}

#' Area plot
#'
#' @description A plot showing how one or more groups' numeric values change over the
#'  progression of a another variable
#' @inheritParams common_args
#' @inheritParams AreaPlotAtomic
#' @return A ggplot object or wrap_plots object or a list of ggplot objects
#' @importFrom ggplot2 waiver
#' @export
#' @examples
#' \donttest{
#' set.seed(8525)
#' data <- data.frame(
#'     x = rep(c("A", "B", "C", "D"), 2),
#'     y = c(1, 3, 6, 4, 2, 5, 7, 8),
#'     group = rep(c("F1", "F2"), each = 4),
#'     split = rep(c("X", "Y"), 4)
#' )
#' AreaPlot(data, x = "x", y = "y", group_by = "group")
#' AreaPlot(data, x = "x", y = "y", group_by = "group",
#'          scale_y = TRUE)
#' AreaPlot(data, x = "x", y = "y", split_by = "group")
#' AreaPlot(data, x = "x", y = "y", split_by = "group", palette = c(F1 = "Blues", F2 = "Reds"))
#' AreaPlot(data, x = "x", y = "y", group_by = "group", split_by = "split",
#'     legend.direction = c(X = "horizontal", Y = "vertical"),
#'     legend.position = c(X = "top", Y = "right"))
#'
#' # How keep_na and keep_empty work
#' data <- data.frame(
#'     x = factor(rep(c("A", NA, "C", "D"), 3), levels = c("A", "B", "C", "D")),
#'     y = c(1, 3, 6, 4, 2, 5, 7, 8, 4, 2, 3, 5),
#'     group = factor(sample(rep(c("F1", NA, "F3"), each = 4)), levels = c("F1", "F2", "F3")),
#'     split = factor(sample(rep(c("X", "Y", NA), 4)), levels = c("X", "Y", "Z")),
#'     facet = factor(sample(rep(c("M", "N", NA), 4)), levels = c("M", "N", "O"))
#' )
#'
#' AreaPlot(data, x = "x", y = "y", group_by = "group")
#' AreaPlot(data, x = "x", y = "y", group_by = "group", keep_na = TRUE, keep_empty = TRUE)
#' AreaPlot(data, x = "x", y = "y", group_by = "group", keep_na = TRUE, keep_empty = "level")
#' AreaPlot(data, x = "x", y = "y", group_by = "group", keep_na = FALSE, keep_empty = TRUE)
#' AreaPlot(data, x = "x", y = "y", group_by = "group",
#'     keep_na = list(x = TRUE, group = FALSE), keep_empty = list(x = FALSE, group = TRUE))
#' AreaPlot(data, x = "x", y = "y", group_by = "group",
#'     keep_na = list(x = FALSE, group = TRUE), keep_empty = list(x = TRUE, group = FALSE))
#' }
AreaPlot <- function(
    data, x, y = NULL, x_sep = "_", split_by = NULL, split_by_sep = "_",
    group_by = NULL, group_by_sep = "_", group_name = NULL, scale_y = FALSE,
    theme = "theme_this", theme_args = list(), palette = "Paired", palcolor = NULL, alpha = 1,
    facet_by = NULL, facet_scales = "fixed", facet_ncol = NULL, facet_nrow = NULL, facet_byrow = TRUE,
    x_text_angle = 0, aspect.ratio = 1, legend.position = waiver(), legend.direction = "vertical",
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL, keep_na = FALSE, keep_empty = FALSE, seed = 8525,
    combine = TRUE, nrow = NULL, ncol = NULL, byrow = TRUE, axes = NULL, axis_titles = axes, guides = NULL,
    design = NULL, ...
){
    validate_common_args(seed, facet_by = facet_by)
    keep_na <- check_keep_na(keep_na, c(x, split_by, group_by, facet_by))
    keep_empty <- check_keep_empty(keep_empty, c(x, split_by, group_by, facet_by))
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
            AreaPlotAtomic(
                datas[[nm]],
                x = x, y = y, x_sep = x_sep, group_by = group_by, group_by_sep = group_by_sep, group_name = group_name, scale_y = scale_y,
                theme = theme, theme_args = theme_args, palette = palette[[nm]], palcolor = palcolor[[nm]], alpha = alpha,
                facet_by = facet_by, facet_scales = facet_scales, facet_ncol = facet_ncol, facet_nrow = facet_nrow, facet_byrow = facet_byrow,
                x_text_angle = x_text_angle, aspect.ratio = aspect.ratio, legend.position = legend.position[[nm]], legend.direction = legend.direction[[nm]],
                title = title, subtitle = subtitle, xlab = xlab, ylab = ylab, keep_na = keep_na, keep_empty = keep_empty, ...
            )
        }
    )

    combine_plots(plots, combine = combine, nrow = nrow, ncol = ncol, byrow = byrow,
        axes = axes, axis_titles = axis_titles, guides = guides, design = design)
}
