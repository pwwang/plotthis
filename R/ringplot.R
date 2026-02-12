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
#' @importFrom dplyr %>% summarise n ungroup mutate
#' @importFrom ggplot2 geom_col scale_fill_manual scale_x_discrete geom_label
#' @keywords internal
RingPlotAtomic <- function(
    data, x = NULL, y = NULL, group_by = NULL, group_by_sep = "_",  group_name = NULL,
    label = NULL, clockwise = TRUE, keep_na = FALSE, keep_empty = FALSE,
    facet_by = NULL, facet_scales = "free_y", facet_ncol = NULL, facet_nrow = NULL, facet_byrow = TRUE,
    theme = "theme_this", theme_args = list(), palette = "Paired", palcolor = NULL,
    alpha = 1, aspect.ratio = 1, legend.position = "right", legend.direction = "vertical",
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL,
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
    if (is.null(group_by)) {
        group_by <- ".group_by"
        data[[group_by]] <- factor("")
        group_guide = "none"
    } else {
        group_guide = guide_legend(reverse = clockwise)
    }

    data <- process_keep_na_empty(data, keep_na, keep_empty)
    keep_empty_x <- keep_empty[[x]]
    keep_empty_group <- if (!is.null(group_by)) keep_empty[[group_by]] else NULL
    keep_empty_facet <- if (!is.null(facet_by)) keep_empty[[facet_by[1]]] else NULL
    if (length(facet_by) > 1) {
        stopifnot("[RingPlot] `keep_empty` for `facet_by` variables must be identical." =
            identical(keep_empty_facet, keep_empty[[facet_by[2]]]))
    }

    orig_data <- data
    if (is.null(y)) {
        data <- data %>% dplyr::group_by(!!!syms(unique(c(x, group_by, facet_by)))) %>% summarise(.y = n(), .groups = "drop")
        for (col in unique(c(x, facet_by))) {
            data[[col]] <- factor(data[[col]], levels = levels(orig_data[[col]]))
        }
        y <- ".y"
    } else {
        y <- check_columns(data, y)
    }
    # make sure each ring sums to 1
    data <- data %>% dplyr::group_by(!!!syms(unique(c(x, facet_by)))) %>% mutate(!!sym(y) := !!sym(y) / sum(!!sym(y))) %>% ungroup()
    for (col in unique(x, facet_by)) {
        data[[col]] <- factor(data[[col]], levels = levels(orig_data[[col]]))
    }
    rm(orig_data)

    rings <- rev(levels(data[[x]]))
    if (anyNA(data[[x]])) rings <- c(rings, NA)
    if (length(rings) == 1 && is.null(label)) {
        label <- FALSE
    } else if (length(rings) > 1 && is.null(label)) {
        label <- TRUE
    }
    if (isTRUE(clockwise)) {
        data[[group_by]] <- factor(data[[group_by]], levels = rev(levels(data[[group_by]])))
        group_vals <- rev(levels(data[[group_by]]))
        if (anyNA(data[[group_by]])) {
            group_vals <- c(group_vals, NA)
        }
    } else {
        group_vals <- levels(data[[group_by]])
        if (anyNA(data[[group_by]])) {
            group_vals <- c(group_vals, NA)
        }
    }
    data <- data[order(data[[group_by]]), , drop = FALSE]
    colors <- palette_this(group_vals, palette = palette, palcolor = palcolor, NA_keep = TRUE)

    p = ggplot(data, aes(x = !!sym(x), y = !!sym(y), fill = !!sym(group_by))) +
        geom_col(width = 0.9, color = "white", alpha = alpha, show.legend = TRUE) +
        coord_polar("y", start = 0) +
        scale_x_discrete(limits = c(" ", rings), drop = !isTRUE(keep_empty_x)) +
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

    if (isTRUE(keep_empty_group)) {
        p <- p + scale_fill_manual(
            name = group_name %||% group_by,
            values = colors, na.value = colors["NA"] %||% "grey80",
            breaks = rev(group_vals),
            limits = rev(group_vals),
            drop = FALSE,
            guide = group_guide
        )
    } else {
        p <- p + scale_fill_manual(
            name = group_name %||% group_by,
            values = colors, na.value = colors["NA"] %||% "grey80",
            breaks = rev(group_vals),
            drop = TRUE,
            guide = group_guide
        )
    }

    if (label) {
        # Create label data frame with all levels (including empty ones)
        label_df <- data.frame(
            x = rings[!is.na(rings)],
            y = 0,
            stringsAsFactors = FALSE
        )
        if (anyNA(rings)) {
            label_df <- rbind(label_df, data.frame(x = NA, y = 0, stringsAsFactors = FALSE))
        }
        names(label_df)[1] <- x
        label_df[[x]] <- factor(label_df[[x]], levels = rings)
        label_df$label_text <- ifelse(is.na(label_df[[x]]), "NA", as.character(label_df[[x]]))

        p <- p + geom_label(
            aes(label = !!sym("label_text"), x = !!sym(x), y = 0),
            data = label_df,
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
    facet_plot(p, facet_by, facet_scales, facet_nrow, facet_ncol, facet_byrow,
        legend.position = legend.position, legend.direction = legend.direction,
        drop = !isTRUE(keep_empty_facet))
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
#' \donttest{
#' RingPlot(datasets::iris, group_by = "Species")
#' RingPlot(datasets::mtcars, x = "cyl", group_by = "carb", facet_by = "vs")
#' RingPlot(datasets::mtcars, x = "cyl", group_by = "carb", split_by = "vs",
#'         palette = c("0" = "Set1", "1" = "Paired"))
#'
#' data <- data.frame(
#'   x = factor(c("A", "B", NA, "D", "A", "B", NA, "D"), levels = c("A", "B", "C", "D")),
#'   y = c(1, 2, 5, 3, 4, 5, 2, 6),
#'   group = factor(c("a", "a", "a", NA, NA, "c", "c", "c"), levels = c("a", "b", "c"))
#' )
#' RingPlot(data, x = "x", y = "y", group_by = "group")
#' RingPlot(data, x = "x", y = "y", group_by = "group",
#'         keep_na = TRUE, keep_empty = TRUE)
#' RingPlot(data, x = "x", y = "y", group_by = "group",
#'         keep_na = TRUE, keep_empty = list(x = FALSE, group = 'level'))
#' }
RingPlot <- function(
    data, x = NULL, y = NULL, group_by = NULL, group_by_sep = "_", group_name = NULL,
    label = NULL, split_by = NULL, split_by_sep = "_",
    facet_by = NULL, facet_scales = "free_y", facet_ncol = NULL, facet_nrow = NULL, facet_byrow = TRUE,
    theme = "theme_this", theme_args = list(), palette = "Paired", palcolor = NULL,
    alpha = 1, aspect.ratio = 1, keep_na = FALSE, keep_empty = FALSE,
    legend.position = "right", legend.direction = "vertical",
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL,
    combine = TRUE, nrow = NULL, ncol = NULL, byrow = TRUE,
    seed = 8525, axes = NULL, axis_titles = axes, guides = NULL, design = NULL, ...
) {
    validate_common_args(seed, facet_by = facet_by)
    keep_na <- check_keep_na(keep_na, c(x, group_by, split_by, facet_by))
    keep_empty <- check_keep_empty(keep_empty, c(x, group_by, split_by, facet_by))
    theme <- process_theme(theme)
    split_by <- check_columns(data, split_by, force_factor = TRUE, allow_multi = TRUE,
        concat_multi = TRUE, concat_sep = split_by_sep)

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
            RingPlotAtomic(datas[[nm]],
                x = x, y = y, label = label, group_by = group_by, group_by_sep = group_by_sep, group_name = group_name,
                facet_by = facet_by, facet_scales = facet_scales, facet_ncol = facet_ncol, facet_nrow = facet_nrow, facet_byrow = facet_byrow,
                theme = theme, theme_args = theme_args, palette = palette[[nm]], palcolor = palcolor[[nm]],
                alpha = alpha, aspect.ratio = aspect.ratio, keep_na = keep_na, keep_empty = keep_empty,
                legend.position = legend.position[[nm]], legend.direction = legend.direction[[nm]],
                title = title, subtitle = subtitle, xlab = xlab, ylab = ylab,
                seed = seed, ...
            )
        }
    )

    combine_plots(plots, combine = combine, nrow = nrow, ncol = ncol, byrow = byrow,
        axes = axes, axis_titles = axis_titles, guides = guides, design = design)
}