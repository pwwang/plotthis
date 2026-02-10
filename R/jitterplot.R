#' Atomic Jitter plot
#'
#' @inheritParams common_args
#' @param x A character string of the column name to plot on the x-axis.
#'  A character/factor column is expected. If multiple columns are provided, the columns will be concatenated with `x_sep`.
#' @param x_sep A character string to concatenate the columns in `x`, if multiple columns are provided.
#'  When `in_form` is "wide", `x` columns will not be concatenated.
#' @param y A character string of the column name to plot on the y-axis. A numeric column is expected.
#'  When `in_form` is "wide", `y` is not required. The values under `x` columns will be used as y-values.
#' @param in_form A character string to specify the input data type. Either "long" or "wide".
#' @param sort_x A character string to specify the sorting of x-axis, chosen from "none", "mean_asc", "mean_desc", "mean", "median_asc", "median_desc", "median".
#' * `none` means no sorting (as-is).
#' * `mean_asc` sorts the x-axis by ascending mean of y-values.
#' * `mean_desc` sorts the x-axis by descending mean of y-values.
#' * `mean` is an alias for `mean_asc`.
#' * `median_asc` sorts the x-axis by ascending median of y-values.
#' * `median_desc` sorts the x-axis by descending median of y-values.
#' * `median` is an alias for `median_asc`.
#' @param flip A logical value to flip the plot.
#' @param group_by A character string to dodge the points.
#' @param group_by_sep A character string to concatenate the columns in `group_by`, if multiple columns are provided.
#' @param group_name A character string to name the legend of dodge.
#' @param add_bg A logical value to add background to the plot.
#' @param bg_palette A character string to specify the palette of the background.
#' @param bg_palcolor A character vector to specify the colors of the background.
#' @param bg_alpha A numeric value to specify the transparency of the background.
#' @param shape A numeric value to specify the point shape.
#'   Shapes 21–25 have borders; border behavior is controlled by `border`.
#' @param border A logical or character value to specify the border of points when the shape has border (21–25).
#'  If TRUE, border color follows the point color (same as fill). If a color string, uses that constant border color.
#'  If FALSE, no border.
#' @param size_by A numeric column name or a single numeric value for the point size.
#'  When a column, sizes are scaled (see scatter plots).
#' @param size_name Legend title for size when `size_by` is a column.
#' @param size_trans A function or a name of a global function to transform `size_by` (when `size_by` is a column).
#'  The legend shows original (untransformed) values.
#' @param alpha Point transparency.
#' @param jitter_width,jitter_height Jitter parameters.
#' @param y_max,y_min Numeric or quantile strings ("q95", "q5") for y limits computation (used for fixed coord).
#' @param y_trans,y_nbreaks Axis settings.
#' @param labels A vector of row names or indices to label the points.
#' @param label_by A character column name to use as the label text. If NULL, rownames are used.
#' @param nlabel Number of points to label per x-group when `labels` is NULL (top by y^2 + size^2).
#' @param order_by A string of expression passed to `dplyr::arrange()` to order the data to get the
#' top `nlabel` points for labeling. Default is `-({y}^2 + {size_by}^2)` (similar to VolcanoPlot).
#' @param label_size,label_fg,label_bg,label_bg_r Label aesthetics.
#' @param add_hline Add one or more horizontal reference lines at the given y-value(s).
#' @param hline_type The line type for the horizontal reference line(s).
#' @param hline_width The line width for the horizontal reference line(s).
#' @param hline_color The color for the horizontal reference line(s).
#' @param hline_alpha The alpha for the horizontal reference line(s).
#' @param highlight,highlight_color,highlight_size,highlight_alpha Highlighted point options.
#' @return A ggplot object
#' @keywords internal
#' @importFrom stats median quantile
#' @importFrom rlang sym syms parse_expr %||%
#' @importFrom dplyr mutate ungroup first
#' @importFrom ggplot2 geom_point labs theme element_line element_text coord_flip
#' @importFrom ggplot2 position_jitterdodge scale_fill_manual scale_color_manual scale_y_continuous scale_size_area guide_legend guide_colorbar
#' @importFrom ggrepel geom_text_repel
JitterPlotAtomic <- function(
    data, x, x_sep = "_", y = NULL, in_form = c("long", "wide"), keep_na = FALSE, keep_empty = FALSE,
    sort_x = c("none", "mean_asc", "mean_desc", "mean", "median_asc", "median_desc", "median"),
    flip = FALSE, group_by = NULL, group_by_sep = "_", group_name = NULL,
    x_text_angle = 0, order_by = "-({y}^2 + {size_by}^2)",
    theme = "theme_this", theme_args = list(), palette = "Paired", palcolor = NULL, alpha = 1,
    aspect.ratio = NULL, legend.position = "right", legend.direction = "vertical",
    shape = 21, border = "black",
    size_by = 2, size_name = NULL, size_trans = NULL, y_nbreaks = 4,
    jitter_width = 0.5, jitter_height = 0, y_max = NULL, y_min = NULL, y_trans = "identity",
    add_bg = FALSE, bg_palette = "stripe", bg_palcolor = NULL, bg_alpha = 0.2,
    add_hline = NULL, hline_type = "solid", hline_width = 0.5, hline_color = "black", hline_alpha = 1,
    labels = NULL, label_by = NULL, nlabel = 5, label_size = 3, label_fg = "black", label_bg = "white", label_bg_r = 0.1,
    highlight = NULL, highlight_color = "red2", highlight_size = 1, highlight_alpha = 1,
    facet_by = NULL, facet_scales = "fixed", facet_ncol = NULL, facet_nrow = NULL, facet_byrow = TRUE,
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL, seed = 8525, ...
) {
    set.seed(seed)
    ggplot <- if (getOption("plotthis.gglogger.enabled", FALSE)) gglogger::ggplot else ggplot2::ggplot
    in_form <- match.arg(in_form)
    if (in_form == "wide") {
        data <- data %>% tidyr::pivot_longer(cols = x, names_to = ".x", values_to = ".y")
        x <- ".x"; y <- ".y"
    }

    x <- check_columns(data, x, force_factor = TRUE, allow_multi = TRUE, concat_multi = TRUE, concat_sep = x_sep)
    y <- check_columns(data, y)
    group_by <- check_columns(data, group_by, force_factor = TRUE, allow_multi = TRUE, concat_multi = TRUE, concat_sep = group_by_sep)
    facet_by <- check_columns(data, facet_by, force_factor = TRUE, allow_multi = TRUE)

    data <- process_keep_na_empty(data, keep_na, keep_empty)
    keep_empty_x <- keep_empty[[x]]
    keep_empty_group <- if (!is.null(group_by)) keep_empty[[group_by]] else NULL
    keep_empty_facet <- if (!is.null(facet_by)) keep_empty[[facet_by[1]]] else NULL
    if (length(facet_by) > 1) {
        stopifnot("[JitterPlot] `keep_empty` for `facet_by` variables must be identical." =
            identical(keep_empty_facet, keep_empty[[facet_by[2]]]))
    }

    sort_x <- match.arg(sort_x)
    data <- data %>%
        dplyr::group_by(!!!syms(unique(c(x, group_by, facet_by)))) %>%
        mutate(.y_mean = mean(!!sym(y)), .y_median = median(!!sym(y))) %>%
        ungroup()

    # keep factor levels
    for (col in unique(c(x, group_by, facet_by))) {
        data[[col]] <- factor(data[[col]], levels = levels(data[[col]]))
    }

    values <- data[[y]][is.finite(data[[y]])]
    if (is.character(y_max)) {
        q_max <- as.numeric(sub("(^q)(\\d+)", "\\2", y_max)) / 100
        y_max_use <- stats::quantile(values, q_max, na.rm = TRUE)
    } else {
        y_max_use <- max(values, na.rm = TRUE)
    }
    if (is.null(y_min)) {
        y_min_use <- min(values, na.rm = TRUE)
    } else if (is.character(y_min)) {
        q_min <- as.numeric(sub("(^q)(\\d+)", "\\2", y_min)) / 100
        y_min_use <- stats::quantile(values, q_min, na.rm = TRUE)
    } else {
        y_min_use <- y_min
    }
    rm(values)

    # Highlight flag
    if (!is.null(highlight)) {
        if (isTRUE(highlight)) {
            data$.highlight <- TRUE
        } else if (is.numeric(highlight)) {
            data$.highlight <- 1:nrow(data) %in% highlight
        } else if (is.character(highlight) && length(highlight) == 1) {
            data <- dplyr::mutate(data, .highlight = !!parse_expr(highlight))
        } else if (is.null(rownames(data))) {
            stop("No row names in the data, please provide a vector of indexes to highlight.")
        } else {
            data$.highlight <- rownames(data) %in% highlight
        }
    } else {
        data$.highlight <- FALSE
    }

    # Labels (similar to VolcanoPlot)
    # Labels using distance = y^2 + size^2
    data$.label <- if (is.null(label_by)) rownames(data) else data[[label_by]]
    data$.show_label <- FALSE
    if (!is.null(labels)) {
        data[labels, ".show_label"] <- TRUE
    } else if (nlabel > 0) {
        if (!is.null(facet_by)) {
            data <- data %>% dplyr::group_by(!!!syms(facet_by), !!sym(x)) %>%
                dplyr::arrange(!!rlang::parse_expr(glue::glue(order_by))) %>%
                dplyr::mutate(.show_label = dplyr::row_number() <= nlabel) %>%
                dplyr::ungroup()
        } else {
            data <- data %>% dplyr::group_by(!!sym(x)) %>%
                dplyr::arrange(!!rlang::parse_expr(glue::glue(order_by))) %>%
                dplyr::mutate(.show_label = dplyr::row_number() <= nlabel) %>%
                dplyr::ungroup()
        }
        for (col in unique(c(x, facet_by))) {
            data[[col]] <- factor(data[[col]], levels = levels(data[[col]]))
        }
    }
    data <- as.data.frame(data)

    if (sort_x == "mean" || sort_x == "mean_asc") {
        data[[x]] <- stats::reorder(data[[x]], data$.y_mean)
    } else if (sort_x == "mean_desc") {
        data[[x]] <- stats::reorder(data[[x]], -data$.y_mean)
    } else if (sort_x == "median" || sort_x == "median_asc") {
        data[[x]] <- stats::reorder(data[[x]], data$.y_median)
    } else if (sort_x == "median_desc") {
        data[[x]] <- stats::reorder(data[[x]], -data$.y_median)
    }

    if (isTRUE(flip)) {
        data[[x]] <- factor(data[[x]], levels = rev(levels(data[[x]])))
        aspect.ratio <- 1 / aspect.ratio
        if (length(aspect.ratio) == 0 || is.na(aspect.ratio)) aspect.ratio <- NULL
    }

    color_col <- ifelse(is.null(group_by), x, group_by)
    keep_empty_col <- if (color_col == x) keep_empty_x else keep_empty_group
    col_levels <- levels(data[[color_col]])
    if (anyNA(data[[color_col]])) col_levels <- c(col_levels, NA)
    colors <- palette_this(col_levels, palette = palette, palcolor = palcolor, NA_keep = TRUE)
    if (anyNA(col_levels)) {
        # To prevent ggrepel text segments being too long
        # It seem it thinks the NA is an empty area for it to place labels
        names(colors)[is.na(names(colors))] <- "NA"
        col_levels[is.na(col_levels)] <- "NA"
        levels(data[[color_col]]) <- col_levels
        data[[color_col]][is.na(data[[color_col]])] <- "NA"
    }
    # Similar when x has NA, but color_col is group
    if (color_col != x && anyNA(data[[x]])) {
        x_levels <- c(levels(data[[x]]), "NA")
        levels(data[[x]]) <- x_levels
        data[[x]][is.na(data[[x]])] <- "NA"
    }
    # Base
    p <- ggplot(data, aes(x = !!sym(x), y = !!sym(y)))
    if (isTRUE(add_bg)) {
        p <- p + bg_layer(data, x, isTRUE(keep_empty_x), bg_palette, bg_palcolor, bg_alpha, facet_by)
    }

    # Positioner (jitter + optional dodge)
    if (is.null(group_by)) {
        pos <- position_jitter(width = jitter_width, height = jitter_height, seed = seed)
    } else {
        pos <- position_jitterdodge(
            jitter.width = jitter_width, jitter.height = jitter_height,
            dodge.width = 0.9, seed = seed
        )
    }

    # Pre-calculate jittered positions for labels (geom_text_repel doesn't respect position adjustments)
    if (any(data$.show_label)) {
        # Create a temporary plot to extract jittered positions
        temp_data <- data
        temp_mapping <- aes(x = !!sym(x), y = !!sym(y))
        if (!is.null(group_by)) {
            # position_jitterdodge requires a visual aesthetic (fill/colour), not just group
            temp_mapping$fill <- aes(fill = !!sym(group_by))$fill
        }
        temp_plot <- ggplot2::ggplot(temp_data, temp_mapping) +
            geom_point(position = pos) +
            scale_x_discrete(drop = !isTRUE(keep_empty_x))
        temp_build <- ggplot2::ggplot_build(temp_plot)
        jittered_coords <- temp_build$data[[1]]
        data$.x_jittered <- jittered_coords$x
        data$.y_jittered <- jittered_coords$y
    }

    # Build point layer, color by x
    has_fill <- shape %in% 21:25
    point_args <- list(
        shape = shape, position = pos, alpha = alpha
    )
    mapping <- list(aes())

    if (has_fill) {
        mapping[[length(mapping) + 1]] <- aes(fill = !!sym(color_col))
        # border handling
        if (isTRUE(border)) {
            mapping[[length(mapping) + 1]] <- aes(color = !!sym(color_col))
        } else if (is.character(border) && length(border) == 1) {
            point_args$color <- border
        } else {
            point_args$color <- NA
        }
    } else {
        # shapes without fill
        mapping[[length(mapping) + 1]] <- aes(color = !!sym(color_col))
    }

    # size handling
    if (is.numeric(size_by)) {
        point_args$size <- size_by
    } else {
        size_by <- check_columns(data, size_by)
        # transform size values while keeping original values for legend labels
        f <- if (is.null(size_trans)) {
            identity
        } else if (is.function(size_trans)) {
            size_trans
        } else {
            get(as.character(size_trans), inherits = TRUE)
        }
        data$.size_raw <- data[[size_by]]
        data$.size_mapped <- f(data$.size_raw)
        mapping[[length(mapping) + 1]] <- aes(size = !!sym(".size_mapped"))
    }

    # Group for dodging only (no legend)
    if (!is.null(group_by)) {
        mapping[[length(mapping) + 1]] <- aes(group = !!sym(group_by))
    }

    modify_list <- utils::getFromNamespace("modify_list", "ggplot2")
    point_args$mapping <- Reduce(modify_list, mapping)
    point_args$data <- data
    point_args$show.legend <- TRUE
    p <- p + do.call(geom_point, point_args)

    # Discrete color/fill scales by x
    if (has_fill) {
        if (isTRUE(keep_empty_col)) {
            p <- p + scale_fill_manual(
                name = color_col, values = colors, na.value = colors['NA'] %||% "grey80",
                breaks = col_levels, limits = col_levels, drop = FALSE
            )
        } else {
            p <- p + scale_fill_manual(name = color_col, values = colors, na.value = colors['NA'] %||% "grey80")
        }
        if (isTRUE(border)) {
            p <- p + scale_color_manual(values = colors, guide = "none", na.value = colors['NA'] %||% "grey80")
        }
    } else {
        if (isTRUE(keep_empty_col)) {
            p <- p + scale_color_manual(
                name = color_col, values = colors, na.value = colors['NA'] %||% "grey80",
                breaks = col_levels, limits = col_levels, drop = FALSE
            )
        } else {
            p <- p + scale_color_manual(name = color_col, values = colors, na.value = colors['NA'] %||% "grey80")
        }
    }

    # Size scale when mapped
    if (!is.numeric(size_by)) {
        # compute breaks on raw, but use transformed positions for sizes
        f <- if (is.null(size_trans)) {
            identity
        } else if (is.function(size_trans)) {
            size_trans
        } else {
            get(as.character(size_trans), inherits = TRUE)
        }
        raw_vals <- data$.size_raw
        raw_breaks <- unique(scales::pretty_breaks(n = 4)(raw_vals))
        mapped_breaks <- tryCatch(f(raw_breaks), error = function(e) raw_breaks)
        p <- p + scale_size_area(max_size = 6, breaks = mapped_breaks, labels = raw_breaks) +
            guides(size = guide_legend(
                title = size_name %||% size_by,
                override.aes = list(fill = "grey30", shape = shape), order = 1
            ))
    }

    # Highlight overlay on top (does not affect legends)
    if (any(data$.highlight)) {
        hi_df <- data[data$.highlight, , drop = FALSE]
        if (has_fill) {
            p <- p + geom_point(
                data = hi_df,
                mapping = if (!is.null(group_by)) {
                    aes(x = !!sym(x), y = !!sym(y), group = !!sym(group_by))
                } else {
                    aes(x = !!sym(x), y = !!sym(y))
                },
                shape = shape, fill = highlight_color, color = "transparent",
                position = pos, size = if (is.numeric(size_by)) highlight_size else highlight_size, alpha = highlight_alpha
            )
        } else {
            p <- p + geom_point(
                data = hi_df,
                mapping = if (!is.null(group_by)) {
                    aes(x = !!sym(x), y = !!sym(y), group = !!sym(group_by))
                } else {
                    aes(x = !!sym(x), y = !!sym(y))
                },
                shape = shape, color = highlight_color,
                position = pos, size = if (is.numeric(size_by)) highlight_size else highlight_size, alpha = highlight_alpha
            )
        }
    }

    # Labels layer
    if (any(data$.show_label)) {
        p <- p + geom_text_repel(
            data = data[data$.show_label, , drop = FALSE],
            mapping = aes(x = !!sym(".x_jittered"), y = !!sym(".y_jittered"), label = !!sym(".label")),
            color = label_fg, bg.color = label_bg, bg.r = label_bg_r,
            size = label_size, min.segment.length = 0, segment.color = "grey40",
            max.overlaps = 100
        )
    }
    # Optional horizontal reference lines
    if (!is.null(add_hline)) {
        p <- p + ggplot2::geom_hline(
            yintercept = add_hline,
            linetype = hline_type, linewidth = hline_width, color = hline_color, alpha = hline_alpha
        )
    }

    just <- calc_just(x_text_angle)
    p <- p +
        scale_x_discrete(drop = !isTRUE(keep_empty_x)) +
        scale_y_continuous(trans = y_trans, n.breaks = y_nbreaks) +
        labs(title = title, subtitle = subtitle, x = xlab %||% x, y = ylab %||% y)

    height <- width <- 0
    if (!identical(legend.position, "none")) {
        if (legend.position %in% c("right", "left")) {
            width <- width + 1
        } else if (legend.direction == "horizontal") {
            height <- height + 1
        } else {
            height <- height + 2
        }
    }
    x_maxchars <- max(nchar(levels(data[[x]])))
    nx <- nlevels(data[[x]])
    nd <- ifelse(is.null(group_by), 1, nlevels(data[[group_by]]))
    facet_free <- !is.null(facet_by) && (
        identical(facet_scales, "free") ||
        (!flip && identical(facet_scales, "free_y")) ||
        (flip && identical(facet_scales, "free_x"))
    )

    if (isTRUE(flip)) {
        strip_position <- "top"
        p <- p + ggplot2::theme(
            strip.text.y = element_text(angle = 0),
            panel.grid.major.y = element_line(color = "grey", linetype = 2)
        )
        if (facet_free) p <- p + coord_flip() else p <- p + coord_flip(ylim = c(y_min_use, y_max_use))
        width <- max(3, width + 2.2 + x_maxchars * 0.05)
        height <- height + nx * nd * 0.3
    } else {
        strip_position <- "top"
        p <- p + ggplot2::theme(
            strip.text.x = element_text(angle = 0),
            panel.grid.major.x = element_line(color = "grey", linetype = 2)
        )
        if (!facet_free) p <- p + ggplot2::coord_cartesian(ylim = c(y_min_use, y_max_use))
        height <- max(3, height + 2 + x_maxchars * 0.05)
        width <- width + nx * nd * 0.3
    }

    p <- p +
        do.call(theme, theme_args) +
        ggplot2::theme(
            aspect.ratio = aspect.ratio,
            axis.text.x = element_text(angle = x_text_angle, hjust = just$h, vjust = just$v),
            legend.position = legend.position,
            legend.direction = legend.direction
        )

    attr(p, "height") <- height
    attr(p, "width") <- max(width, height)

    facet_plot(p, facet_by, facet_scales, facet_nrow, facet_ncol, facet_byrow,
        strip.position = strip_position, legend.position = legend.position,
        legend.direction = legend.direction, drop = !isTRUE(keep_empty_facet)
    )
}

#' Jitter Plot
#'
#' @description
#'  Jittered point plot with optional background, highlight, labels and faceting.
#' @rdname jitterplot
#' @return The Jitter plot(s).
#'  When `split_by` is not provided, it returns a ggplot object.
#'  When `split_by` is provided, it returns a object of plots wrapped by `patchwork::wrap_plots` if `combine = TRUE`;
#'  otherwise, it returns a list of ggplot objects.
#' @export
#' @inheritParams JitterPlotAtomic
#' @inheritParams common_args
#' @examples
#' \donttest{
#' set.seed(8525)
#' n <- 180
#' x <- factor(
#'     sample(c("A", NA, LETTERS[3:5]), n, replace = TRUE),
#'     levels = c("A", "B", "C", "D", "E")
#' )
#' group <- factor(
#'     sample(c("G1", NA, "G3"), n, replace = TRUE),
#'     levels = c("G1", "G2", "G3")
#' )
#' size <- rexp(n, rate = 1)
#' id <- paste0("pt", seq_len(n))
#' y <- rnorm(n, mean = ifelse(is.na(group), 0, ifelse(group == "G1", 0.5, -0.5))) +
#'      as.numeric(ifelse(is.na(x), 0, x))/10
#' df <- data.frame(
#'   x = x,
#'   y = y,
#'   group = group,
#'   size = size,
#'   id = id
#' )
#'
#' # Basic
#' JitterPlot(df, x = "x", y = "y")
#'
#' # Keep empty x levels and NA
#' JitterPlot(df, x = "x", y = "y", keep_na = TRUE, keep_empty = TRUE)
#'
#' # Map size with transform; legend shows original values
#' JitterPlot(df, x = "x", y = "y", size_by = "size", size_name = "Abundance",
#'     size_trans = sqrt, order_by = "-y^2")
#'
#' # Dodge by group and add a horizontal line
#' JitterPlot(df, x = "x", y = "y", group_by = "group",
#'   add_hline = 0, hline_type = "dashed", hline_color = "red2")
#'
#' # Keep the empty levels only for color coding
#' # Note the G3 is not blue (which is taken by unused level G2)
#' JitterPlot(df, x = "x", y = "y", group_by = "group",
#'     keep_na = TRUE, keep_empty = 'level')
#'
#' # Label top points by distance (y^2 + size^2)
#' JitterPlot(df, x = "x", y = "y", size_by = "size", label_by = "id", nlabel = 3)
#'
#' # Flip axes
#' JitterPlot(df, x = "x", y = "y", flip = TRUE)
#' }
JitterPlot <- function(
    data, x, x_sep = "_", y = NULL, in_form = c("long", "wide"),
    split_by = NULL, split_by_sep = "_", keep_na = FALSE, keep_empty = FALSE,
    sort_x = c("none", "mean_asc", "mean_desc", "mean", "median_asc", "median_desc", "median"),
    flip = FALSE, group_by = NULL, group_by_sep = "_", group_name = NULL,
    x_text_angle = 0, order_by = "-({y}^2 + {size_by}^2)",
    theme = "theme_this", theme_args = list(), palette = "Paired", palcolor = NULL, alpha = 1,
    aspect.ratio = NULL, legend.position = "right", legend.direction = "vertical",
    shape = 21, border = "black",
    size_by = 2, size_name = NULL, size_trans = NULL, y_nbreaks = 4,
    jitter_width = 0.5, jitter_height = 0, y_max = NULL, y_min = NULL, y_trans = "identity",
    add_bg = FALSE, bg_palette = "stripe", bg_palcolor = NULL, bg_alpha = 0.2,
    add_hline = NULL, hline_type = "solid", hline_width = 0.5, hline_color = "black", hline_alpha = 1,
    labels = NULL, label_by = NULL, nlabel = 5, label_size = 3, label_fg = "black", label_bg = "white", label_bg_r = 0.1,
    highlight = NULL, highlight_color = "red2", highlight_size = 1, highlight_alpha = 1,
    facet_by = NULL, facet_scales = "fixed", facet_ncol = NULL, facet_nrow = NULL, facet_byrow = TRUE,
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL, seed = 8525,
    combine = TRUE, nrow = NULL, ncol = NULL, byrow = TRUE,
    axes = NULL, axis_titles = axes, guides = NULL, design = NULL, ...
) {
    validate_common_args(seed)
    keep_na <- check_keep_na(keep_na, c(x, split_by, group_by, facet_by))
    keep_empty <- check_keep_empty(keep_empty, c(x, split_by, group_by, facet_by))
    theme <- process_theme(theme)
    split_by <- check_columns(data, split_by, force_factor = TRUE, allow_multi = TRUE, concat_multi = TRUE, concat_sep = split_by_sep)

    if (!is.null(split_by)) {
        data <- process_keep_na_empty(data, keep_na, keep_empty, col = split_by)
        keep_na[[split_by]] <- NULL
        keep_empty[[split_by]] <- NULL
        datas <- split(data, data[[split_by]])
        datas <- datas[levels(data[[split_by]])]
    } else {
        datas <- list(data); names(datas) <- "..."
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
            JitterPlotAtomic(
                datas[[nm]],
                x = x, x_sep = x_sep, y = y, in_form = in_form, keep_na = keep_na, keep_empty = keep_empty,
                sort_x = sort_x, flip = flip, group_by = group_by, group_by_sep = group_by_sep, group_name = group_name,
                x_text_angle = x_text_angle, theme = theme, theme_args = theme_args, palette = palette[[nm]], palcolor = palcolor[[nm]], alpha = alpha,
                aspect.ratio = aspect.ratio, legend.position = legend.position[[nm]], legend.direction = legend.direction[[nm]],
                shape = shape, border = border, order_by = order_by,
                size_by = size_by, size_name = size_name, size_trans = size_trans, y_nbreaks = y_nbreaks,
                jitter_width = jitter_width, jitter_height = jitter_height, y_max = y_max, y_min = y_min, y_trans = y_trans,
                add_bg = add_bg, bg_palette = bg_palette, bg_palcolor = bg_palcolor, bg_alpha = bg_alpha,
                add_hline = add_hline, hline_type = hline_type, hline_width = hline_width, hline_color = hline_color, hline_alpha = hline_alpha,
                labels = labels, label_by = label_by, nlabel = nlabel, label_size = label_size, label_fg = label_fg, label_bg = label_bg, label_bg_r = label_bg_r,
                highlight = highlight, highlight_color = highlight_color, highlight_size = highlight_size, highlight_alpha = highlight_alpha,
                facet_by = facet_by, facet_scales = facet_scales, facet_ncol = facet_ncol, facet_nrow = facet_nrow, facet_byrow = facet_byrow,
                title = title, subtitle = subtitle, xlab = xlab, ylab = ylab, seed = seed, ...
            )
        }
    )

    combine_plots(plots, combine = combine, nrow = nrow, ncol = ncol, byrow = byrow,
        axes = axes, axis_titles = axis_titles, guides = guides, design = design)
}
