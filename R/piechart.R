#' Pie chart without data splitting
#'
#' @inheritParams common_args
#' @param label Which column to use as the label. NULL means no label.
#' Default is the same as y. If y is NULL, you should use ".y" to specify the count as the label.
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
    data, x, y = NULL, label = y, clockwise = TRUE,
    theme = "theme_this", theme_args = list(), palette = "Paired", palcolor = NULL, alpha = 1,
    facet_by = NULL, facet_scales = "free_y", facet_ncol = NULL, facet_nrow = NULL, facet_byrow = TRUE,
    aspect.ratio = 1, legend.position = "right", legend.direction = "vertical",
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL, keep_empty = FALSE, ...
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
    if (is.null(y)) {
        data <- data %>%
            group_by(!!!syms(unique(c(x, facet_by)))) %>%
            summarise(.y = n(), .groups = "drop")
        y <- ".y"
    }
    label <- check_columns(data, label)
    concated_facet_by <- check_columns(data, facet_by, force_factor = TRUE, allow_multi = TRUE, concat_multi = TRUE)
    # if keep_empty is TRUE, fill the empty levels with 0
    if (isTRUE(keep_empty)) {
        .handle_one_facet <- function(df) {
            df <- df %>% complete(!!sym(x))
            df[[y]] <- df[[y]] %>% replace_na(0)
            df
        }
        if (is.null(facet_by)) {
            data <- .handle_one_facet(data)
        } else {
            dsplit <- split(data, data[[concated_facet_by]])
            dsplit <- lapply(names(dsplit), function(n) {
                df <- .handle_one_facet(dsplit[[n]])
                df[[concated_facet_by]] <- n
                df
            })
            data <- do.call(rbind, dsplit)
            rm(dsplit)
        }
    }
    # order the data by the levels of x
    if (isTRUE(clockwise)) {
        data[[x]] <- factor(data[[x]], levels = rev(levels(data[[x]])))
    }
    data <- data[order(data[[x]]), , drop = FALSE]

    .pos_df_one_facet <- function(df) {
        df %>% mutate(
            csum = rev(cumsum(rev(!!sym(y)))),
            pos = !!sym(y) / 2 + lead(!!sym("csum"), 1),
            pos = if_else(is.na(!!sym("pos")), !!sym(y) / 2, !!sym("pos"))
        )
    }
    if (is.null(facet_by)) {
        pos_df <- .pos_df_one_facet(data)
    } else {
        pos_df <- do.call(rbind, lapply(split(data, data[[concated_facet_by]]), .pos_df_one_facet))
    }
    if (isTRUE(clockwise)) {
        colors <- palette_this(rev(levels(data[[x]])), palette = palette, palcolor = palcolor)
    } else {
        colors <- palette_this(levels(data[[x]]), palette = palette, palcolor = palcolor)
    }

    p <- ggplot(data, aes(x = "", y = !!sym(y), fill = !!sym(x))) +
        geom_col(width = 1, alpha = alpha, color = "white") +
        scale_fill_manual(name = x, drop = !keep_empty, values = colors, guide = guide_legend(reverse = clockwise)) +
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

    if (!is.null(label)) {
        p <- p + geom_label_repel(
            data = pos_df,
            aes(y = !!sym("pos"), label = !!sym(label)),
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
        legend.position = legend.position, legend.direction = legend.direction)
}

#' Pie Chart
#'
#' @description Pie chart to illustrate numerical proportion of each group.
#' @export
#' @inheritParams PieChartAtomic
#' @inheritParams common_args
#' @return A ggplot object or wrap_plots object or a list of ggplot objects
#' @examples
#' data <- data.frame(
#'    x = c("A", "B", "C", "D", "E", "F", "G", "H"),
#'    y = c(10, 8, 16, 4, 6, 12, 14, 2),
#'    group = c("G1", "G1", "G2", "G2", "G3", "G3", "G4", "G4"),
#'    facet = c("F1", "F2", "F3", "F4", "F1", "F2", "F3", "F4")
#' )
#'
#' PieChart(data, x = "x", y = "y")
#' PieChart(data, x = "x", y = "y", clockwise = FALSE)
#' PieChart(data, x = "x", y = "y", label = "group")
#' PieChart(data, x = "x", y = "y", facet_by = "facet")
#' PieChart(data, x = "x", y = "y", split_by = "group")
#' PieChart(data, x = "x", y = "y", split_by = "group",
#'          palette = list(G1 = "Reds", G2 = "Blues", G3 = "Greens", G4 = "Purp"))
#'
#' # y from count
#' PieChart(data, x = "group")
#' # add label
#' PieChart(data, x = "group", label = ".y")
PieChart <- function(
    data, x, y = NULL, label = y, split_by = NULL, split_by_sep = "_", clockwise = TRUE,
    facet_by = NULL, facet_scales = "free_y", facet_ncol = NULL, facet_nrow = NULL, facet_byrow = TRUE,
    theme = "theme_this", theme_args = list(), palette = "Paired", palcolor = NULL,
    alpha = 1, aspect.ratio = 1,
    legend.position = "right", legend.direction = "vertical",
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL, keep_empty = FALSE,
    combine = TRUE, nrow = NULL, ncol = NULL, byrow = TRUE, seed = 8525,
    axes = NULL, axis_titles = axes, guides = NULL, design = NULL, ...
) {
    validate_common_args(seed, facet_by = facet_by)
    theme <- process_theme(theme)

    x <- check_columns(data, x, force_factor = TRUE)
    y <- check_columns(data, y)
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
            PieChartAtomic(datas[[nm]],
                x = x, y = y, label = label, split_by = split_by, clockwise = clockwise,
                facet_by = facet_by, facet_scales = facet_scales, facet_ncol = facet_ncol, facet_nrow = facet_nrow,
                facet_byrow = facet_byrow,
                theme = theme, theme_args = theme_args, palette = palette[[nm]], palcolor = palcolor[[nm]],
                alpha = alpha, aspect.ratio = aspect.ratio,
                legend.position = legend.position[[nm]], legend.direction = legend.direction[[nm]],
                title = title, subtitle = subtitle, xlab = xlab, ylab = ylab, keep_empty = keep_empty, ...
            )
        }
    )

    combine_plots(plots, combine = combine, nrow = nrow, ncol = ncol, byrow = byrow,
        axes = axes, axis_titles = axis_titles, guides = guides, design = design)
}
