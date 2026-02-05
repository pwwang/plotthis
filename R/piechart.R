#' Pie chart without data splitting
#'
#' @inheritParams common_args
#' @param label Which column to use as the label. NULL means no label.
#' Default is the same as y. If y is NULL, you should use ".y" to specify the count as the label.
#' If TRUE, the y values will be used as the label.
#' @param y A character string of the column name to plot on the y-axis.
#'   A numeric column is expected.
#'   If NULL, the count of each x column will be used.
#' @param clockwise A logical value to draw the pie chart clockwise or not.
#' @keywords internal
#' @importFrom rlang sym
#' @importFrom dplyr lead if_else mutate %>% group_by summarise n
#' @importFrom tidyr complete replace_na
#' @importFrom ggplot2 coord_polar geom_col scale_fill_manual labs element_blank guide_legend
#' @importFrom ggrepel geom_label_repel
PieChartAtomic <- function(
    data, x, y = NULL, label = y, clockwise = TRUE, keep_na = FALSE, keep_empty = FALSE,
    theme = "theme_this", theme_args = list(), palette = "Paired", palcolor = NULL, alpha = 1,
    facet_by = NULL, facet_scales = "free_y", facet_ncol = NULL, facet_nrow = NULL, facet_byrow = TRUE,
    aspect.ratio = 1, legend.position = "right", legend.direction = "vertical",
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL, ...
) {
    ggplot <- if (getOption("plotthis.gglogger.enabled", FALSE)) {
        gglogger::ggplot
    } else {
        ggplot2::ggplot
    }
    base_size <- theme_args$base_size %||% 12
    text_size_scale <- base_size / 12

    x <- check_columns(data, x, force_factor = TRUE)
    y <- check_columns(data, y)
    orig_data <- data
    if (is.null(y)) {
        data <- data %>%
            group_by(!!!syms(unique(c(x, facet_by)))) %>%
            summarise(.y = n(), .groups = "drop")

        for (col in c(x, facet_by)) {
            data[[col]] <- factor(data[[col]], levels = levels(orig_data[[col]]))
        }
        y <- ".y"
    }
    if (isTRUE(label)) label <- y

    label <- check_columns(data, label)
    facet_by <- check_columns(data, facet_by, force_factor = TRUE, allow_multi = TRUE)

    data <- process_keep_na_empty(data, keep_na, keep_empty)
    keep_empty_x <- keep_empty[[x]]
    keep_empty_facet <- if (!is.null(facet_by)) keep_empty[[facet_by[1]]] else NULL
    if (length(facet_by) > 1) {
        stopifnot("[PieChart] `keep_empty` for `facet_by` variables must be identical." =
            identical(keep_empty_facet, keep_empty[[facet_by[2]]]))
    }

    # order the data by the levels of x
    if (isTRUE(clockwise)) {
        data[[x]] <- factor(data[[x]], levels = rev(levels(data[[x]])))
    }
    data <- data[order(data[[x]]), , drop = FALSE]

    if (!is.null(facet_by)) {
        data <- data %>%
            group_by(!!!syms(facet_by)) %>%
            mutate(
                csum = rev(cumsum(rev(!!sym(y)))),
                pos = !!sym(y) / 2 + lead(!!sym("csum"), 1),
                pos = if_else(is.na(!!sym("pos")), !!sym(y) / 2, !!sym("pos"))
            ) %>%
            ungroup()

        for (col in facet_by) {
            data[[col]] <- factor(data[[col]], levels = levels(orig_data[[col]]))
        }
    } else {
        data <- data %>%
            mutate(
                csum = rev(cumsum(rev(!!sym(y)))),
                pos = !!sym(y) / 2 + lead(!!sym("csum"), 1),
                pos = if_else(is.na(!!sym("pos")), !!sym(y) / 2, !!sym("pos"))
            )
    }
    rm(orig_data)

    x_vals <- levels(data[[x]])
    if (isTRUE(clockwise)) x_vals <- rev(x_vals)
    if (anyNA(data[[x]])) x_vals <- c(x_vals, NA)

    colors <- palette_this(x_vals, palette = palette, palcolor = palcolor, NA_keep = TRUE)

    p <- ggplot(data, aes(x = "", y = !!sym(y), fill = !!sym(x))) +
        geom_col(width = 1, alpha = alpha, color = "white", show.legend = TRUE) +
        # scale_fill_manual(name = x, drop = !keep_empty, values = colors, guide = guide_legend(reverse = clockwise)) +
        labs(title = title, subtitle = subtitle, x = xlab, y = ylab) +
        coord_polar(theta = "y") +
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
        )

    if (isTRUE(keep_empty_x)) {
        p <- p + scale_fill_manual(
            name = x, values = colors,
            breaks = rev(x_vals), limits = rev(x_vals), drop = FALSE,
            guide = guide_legend(reverse = clockwise)
        )
    } else {
        p <- p + scale_fill_manual(
            name = x, values = colors,
            breaks = rev(x_vals),
            guide = guide_legend(reverse = clockwise)
        )
    }

    if (!is.null(label)) {
        p <- p + geom_label_repel(
            aes(y = !!sym("pos"),
            label = ifelse(is.na(!!sym(label)), "NA", as.character(!!sym(label)))),
            nudge_x = 0.1,
            color = "grey20",
            fill = "#fcfcfc",
            size = text_size_scale * 3
        )
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
    facet_plot(p, facet_by, facet_scales, facet_nrow, facet_ncol, facet_byrow,
        legend.position = legend.position, legend.direction = legend.direction,
        drop = !isTRUE(keep_empty_facet))
}

#' Pie Chart
#'
#' @description Pie chart to illustrate numerical proportion of each group.
#' @export
#' @inheritParams PieChartAtomic
#' @inheritParams common_args
#' @return A ggplot object or wrap_plots object or a list of ggplot objects
#' @examples
#' \donttest{
#' data <- data.frame(
#'    x = factor(c("A", "B", "C", NA, "E", "F", "G", "H"), levels = LETTERS[1:8]),
#'    y = c(10, 8, 16, 4, 6, 12, 14, 2),
#'    group = factor(c("G1", "G1", NA, NA, "G3", "G3", "G4", "G4"),
#'        levels = c("G1", "G2", "G3", "G4")),
#'    facet = factor(c("F1", NA, "F3", "F4", "F1", NA, "F3", "F4"),
#'        levels = c("F1", "F2", "F3", "F4"))
#' )
#' PieChart(data, x = "x", y = "y")
#' PieChart(data, x = "x", y = "y", keep_na = TRUE, keep_empty = TRUE)
#' PieChart(data, x = "x", y = "y", clockwise = FALSE)
#' PieChart(data, x = "x", y = "y", clockwise = FALSE,
#'          keep_na = TRUE, keep_empty = TRUE)
#' PieChart(data, x = "x", y = "y", label = "group")
#' PieChart(data, x = "x", y = "y", facet_by = "facet")
#' PieChart(data, x = "x", y = "y", facet_by = c("facet", "group"),
#'     keep_empty = 'level')
#' PieChart(data, x = "x", y = "y", facet_by = c("facet", "group"),
#'     keep_empty = TRUE)
#' PieChart(data, x = "x", y = "y", split_by = "group")
#' PieChart(data, x = "x", y = "y", split_by = "group",
#'          palette = list(G1 = "Reds", G2 = "Blues", G3 = "Greens", G4 = "Purp"))
#'
#' # y from count
#' PieChart(data, x = "group")
#' # add label
#' PieChart(data, x = "group", label = ".y")  # or label = TRUE
#' }
PieChart <- function(
    data, x, y = NULL, label = y, split_by = NULL, split_by_sep = "_", clockwise = TRUE,
    facet_by = NULL, facet_scales = "free_y", facet_ncol = NULL, facet_nrow = NULL, facet_byrow = TRUE,
    theme = "theme_this", theme_args = list(), palette = "Paired", palcolor = NULL,
    alpha = 1, aspect.ratio = 1, keep_na = FALSE, keep_empty = FALSE,
    legend.position = "right", legend.direction = "vertical",
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL,
    combine = TRUE, nrow = NULL, ncol = NULL, byrow = TRUE, seed = 8525,
    axes = NULL, axis_titles = axes, guides = NULL, design = NULL, ...
) {
    validate_common_args(seed, facet_by = facet_by)
    keep_na <- check_keep_na(keep_na, c(x, split_by, facet_by))
    keep_empty <- check_keep_empty(keep_empty, c(x, split_by, facet_by))
    theme <- process_theme(theme)

    x <- check_columns(data, x, force_factor = TRUE)
    y <- check_columns(data, y)
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
            PieChartAtomic(datas[[nm]],
                x = x, y = y, label = label, split_by = split_by, clockwise = clockwise,
                facet_by = facet_by, facet_scales = facet_scales, facet_ncol = facet_ncol, facet_nrow = facet_nrow,
                facet_byrow = facet_byrow, keep_na = keep_na, keep_empty = keep_empty,
                theme = theme, theme_args = theme_args, palette = palette[[nm]], palcolor = palcolor[[nm]],
                alpha = alpha, aspect.ratio = aspect.ratio,
                legend.position = legend.position[[nm]], legend.direction = legend.direction[[nm]],
                title = title, subtitle = subtitle, xlab = xlab, ylab = ylab, ...
            )
        }
    )

    combine_plots(plots, combine = combine, nrow = nrow, ncol = ncol, byrow = byrow,
        axes = axes, axis_titles = axis_titles, guides = guides, design = design)
}
