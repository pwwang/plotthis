#' Atomic trend plot
#'
#' @inheritParams common_args
#' @param x A character string of the column name to plot on the x-axis.
#'  A character/factor column is expected.
#' @param x_sep A character string to concatenate the columns in `x`, if multiple columns are provided.
#' @param y A character string of the column name to plot on the y-axis.
#'  A numeric column is expected.
#'  If NULL, the count of the x-axis column will be used.
#' @param scale_y A logical value to scale the y-axis by the total number in each x-axis group.
#' @param fill_by A character vector of column names to fill the area plot by.
#'  If NULL, the plot will be filled by the first color of the palette.
#'  If multiple columns are provided, the columns will be concatenated with
#'  `fill_by_sep` and used as the fill column.
#' @param fill_by_sep A character string to separate the columns in `fill_by`.
#' @param fill_name A character string to name the legend of fill.
#' @return A ggplot object
#' @keywords internal
TrendPlotAtomic <- function(
    data, x, y = NULL, x_sep = "_", fill_by = NULL, fill_by_sep = "_", fill_name = NULL, scale_y = FALSE,
    theme = "theme_this", theme_args = list(), palette = "Paired", palcolor = NULL, alpha = 1,
    facet_by = NULL, facet_scales = "fixed", facet_ncol = NULL, facet_nrow = NULL, facet_byrow = TRUE,
    x_text_angle = 0, aspect.ratio = 1, legend.position = "right", legend.direction = "vertical",
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL, ...
) {
    x <- check_columns(data, x, force_factor = TRUE, allow_multi = TRUE, concat_multi = TRUE, concat_sep = x_sep)
    data[[x]] <- droplevels(data[[x]])
    y <- check_columns(data, y)
    fill_by <- check_columns(data, fill_by, force_factor = TRUE,
        allow_multi = TRUE, concat_multi = TRUE, concat_sep = fill_by_sep)

    if (is.null(y)) {
        y <- ".count"
        data <- data %>%
            group_by(!!!syms(unique(c(x, fill_by, facet_by)))) %>%
            summarise(.count = n(), .groups = "drop")
    }

    if (isTRUE(scale_y)) {
        data <- data %>%
            group_by(!!!syms(unique(c(x, facet_by)))) %>%
            mutate(!!sym(y) := !!sym(y) / sum(!!sym(y)))
    }

    if (is.null(fill_by)) {
        data$.fill <- factor("")
        fill_by <- ".fill"
        fill_guide = "none"
    } else {
        fill_guide = "legend"
    }
    xs <- levels(data[[x]])
    nr <- nrow(data)
    dat_area <- data[rep(seq_len(nr), each = 2), , drop = FALSE]
    dat_area[[x]] <- as.numeric(dat_area[[x]])
    dat_area[seq(1, nr * 2, 2), x] <- dat_area[seq(1, nr * 2, 2), x] - 0.2
    dat_area[seq(2, nr * 2, 2), x] <- dat_area[seq(2, nr * 2, 2), x] + 0.2

    position <- position_stack(vjust = 0.5)

    just <- calc_just(x_text_angle)
    p <- ggplot(data, aes(x = !!sym(x), y = !!sym(y), fill = !!sym(fill_by))) +
        geom_area(
            data = dat_area, mapping = aes(x = !!sym(x), fill = !!sym(fill_by)),
            alpha = alpha / 2, color = "grey50", position = position
        ) +
        geom_col(aes(fill = !!sym(fill_by)),
            width = 0.4,
            color = "black",
            alpha = alpha,
            position = position
        ) +
        scale_x_discrete(expand = c(0, 0)) +
        scale_y_continuous(expand = c(0, 0), labels = if (isFALSE(scale_y)) scales::number else scales::percent) +
        scale_fill_manual(
            name = fill_name %||% fill_by,
            values = palette_this(levels(data[[fill_by]]), palette = palette, palcolor = palcolor),
            guide = fill_guide) +
        labs(title = title, subtitle = subtitle, x = xlab %||% "", y = ylab %||% y) +
        do.call(theme, theme_args) +
        ggplot2::theme(
            aspect.ratio = aspect.ratio,
            legend.position = legend.position,
            legend.direction = legend.direction,
            panel.grid.major = element_line(colour = "grey80", linetype = 2),
            axis.text.x = element_text(angle = x_text_angle, hjust = just$h, vjust = just$v)
        )


    height = 4.5
    width = 0.5 + length(xs) * 0.8
    if (legend.position %in% c("right", "left")) {
        width <- width + 1
    } else if (legend.direction == "horizontal") {
        height <- height + 1
    } else {
        width <- width + 2
    }

    attr(p, "height") <- height
    attr(p, "width") <- width

    facet_plot(p, facet_by, facet_scales, facet_nrow, facet_ncol, facet_byrow)
}


#' Trend plot
#'
#' @inheritParams common_args
#' @inheritParams TrendPlotAtomic
#' @return A ggplot object or wrap_plots object or a list of ggplot objects
#' @export
#' @examples
#' data <- data.frame(
#'     x = rep(c("A", "B", "C", "D"), 2),
#'     y = c(1, 3, 6, 4, 2, 5, 7, 8),
#'     fill = rep(c("F1", "F2"), each = 4)
#' )
#' TrendPlot(data, x = "x", y = "y", fill_by = "fill")
#' TrendPlot(data, x = "x", y = "y", fill_by = "fill",
#'          scale_y = TRUE)
#' TrendPlot(data, x = "x", y = "y", split_by = "fill")
TrendPlot <- function(
    data, x, y = NULL, x_sep = "_", split_by = NULL, split_by_sep = "_",
    fill_by = NULL, fill_by_sep = "_", fill_name = NULL, scale_y = FALSE,
    theme = "theme_this", theme_args = list(), palette = "Paired", palcolor = NULL, alpha = 1,
    facet_by = NULL, facet_scales = "fixed", facet_ncol = NULL, facet_nrow = NULL, facet_byrow = TRUE,
    x_text_angle = 0, aspect.ratio = 1, legend.position = "right", legend.direction = "vertical",
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL, seed = 8525,
    combine = TRUE, nrow = NULL, ncol = NULL, byrow = TRUE, ...
){
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
        datas, TrendPlotAtomic,
        x = x, y = y, x_sep = x_sep, fill_by = fill_by, fill_by_sep = fill_by_sep, fill_name = fill_name, scale_y = scale_y,
        theme = theme, theme_args = theme_args, palette = palette, palcolor = palcolor, alpha = alpha,
        facet_by = facet_by, facet_scales = facet_scales, facet_ncol = facet_ncol, facet_nrow = facet_nrow, facet_byrow = facet_byrow,
        x_text_angle = x_text_angle, aspect.ratio = aspect.ratio, legend.position = legend.position, legend.direction = legend.direction,
        title = title, subtitle = subtitle, xlab = xlab, ylab = ylab, ...
    )

    combine_plots(plots, combine = combine, nrow = nrow, ncol = ncol, byrow = byrow)
}
