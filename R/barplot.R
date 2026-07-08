#' Single bar plot (no groups)
#'
#' @description
#' Core implementation for drawing a bar plot without grouping — each x-axis
#' category is a single bar.  This is the simpler code path dispatched by
#' \code{\link{BarPlotAtomic}} when \code{group_by = NULL}.  Bars can be
#' filled by a categorical variable, a continuous variable (numeric colour
#' gradient), or a solid colour.
#'
#' @section Architecture:
#' \enumerate{
#'   \item \strong{Column resolution} — \code{x}, \code{y}, and
#'         \code{facet_by} are validated and transformed via
#'         \code{\link{check_columns}}.  Multi-column \code{x} is concatenated
#'         with \code{x_sep}.
#'   \item \strong{Count aggregation} — when \code{y = NULL}, the count of
#'         observations per (\code{x}, \code{facet_by}) combination is
#'         computed as a new \code{.y} column.  Factor levels are preserved.
#'   \item \strong{Fill resolution} — \code{fill_by} can be \code{TRUE}
#'         (use x values), \code{FALSE}/\code{NULL} (solid fill), a
#'         categorical column name (discrete colour scale), or a numeric
#'         column name (continuous gradient).  Discrete fills use
#'         \code{\link{palette_this}()} for colour assignment; numeric fills
#'         use \code{\link{prepare_continuous_color_scale}()} with quantile
#'         / cutoff clamping and \code{scale_fill_gradientn()}.
#'   \item \strong{Background stripes} — when \code{add_bg = TRUE},
#'         \code{\link{bg_layer}()} adds alternating horizontal or vertical
#'         stripe fills behind the bars.
#'   \item \strong{Labels} — when \code{label} is set, values are displayed
#'         on or near the bar tops via \code{\link[ggrepel]{geom_text_repel}()}
#'         (non-flipped) or \code{geom_text()} (flipped).  The y-position is
#'         nudged by \code{label_nudge} × the data range.
#'   \item \strong{Trend line} — when \code{add_trend = TRUE}, a line and
#'         points are overlaid across the bar tops.
#'   \item \strong{Horizontal reference line} — \code{add_line} draws a
#'         horizontal line at the specified y-value, with a colour legend
#'         entry named by \code{line_name}.
#'   \item \strong{Dimension calculation} — \code{\link{calculate_plot_dimensions}()}
#'         computes plot height and width from the x-axis category count,
#'         label character widths, legend metrics, and flip state.  When
#'         flipped, height scales with the number of x categories.
#'   \item \strong{Coordinate transform} — when \code{flip = TRUE},
#'         \code{coord_flip()} swaps axes; otherwise \code{coord_cartesian()}
#'         applies \code{y_min} / \code{y_max} limits.  Free-scale faceting
#'         skips limit constraints.
#' }
#'
#' @inheritParams common_args
#' @param x A character vector of column name(s) for the x-axis.
#'  Character/factor columns are expected.  Multiple columns are
#'  concatenated with \code{x_sep}.
#' @param x_sep A character string to join multiple \code{x} columns.
#'  Default \code{"_"}.
#' @param y A character string specifying the numeric column for the y-axis.
#'  Default \code{NULL} — the count of observations per x category is used.
#' @param flip Logical; if \code{TRUE}, swap the x and y axes.
#' @param label A column name (or \code{TRUE}) for text labels on bars.
#'  When \code{TRUE}, the y-axis values are labelled.  When a column name,
#'  the values in that column are used.
#' @param label_nudge A numeric value controlling the distance between labels
#'  and the bar top, expressed as a fraction of the data range.
#' @param label_fg A character string specifying the label text colour.
#' @param label_size A numeric value specifying the label text size.
#' @param label_bg A character string specifying the label background colour.
#' @param label_bg_r A numeric value specifying the label background corner
#'  radius.
#' @param add_bg Logical; add alternating background stripes behind the bars.
#' @param bg_palette Palette for the background stripes.
#' @param bg_palcolor Custom colours for the background stripes.
#' @param bg_alpha Alpha transparency for the background stripes.
#' @param fill_by A variable used to fill the bars.  Can be \code{TRUE}
#'  (default; fill by x-axis values), \code{FALSE} (solid fill), or a column
#'  name (categorical or numeric).  When \code{group_by} is used in
#'  \code{\link{BarPlotAtomic}}, this parameter is ignored.
#' @param fill_name A character string for the fill legend title.
#' @param width A numeric value specifying the bar width (0–1).
#' @param add_line A numeric y-intercept for a horizontal reference line.
#' @param line_color Colour of the reference line.
#' @param line_width Width of the reference line.
#' @param line_type Linetype of the reference line (e.g., 1 = solid, 2 = dashed).
#' @param line_name Legend name for the reference line.
#' @param add_trend Logical; add a trend line and points connecting the bar tops.
#' @param trend_color Colour of the trend line.
#' @param trend_linewidth Width of the trend line.
#' @param trend_ptsize Size of the trend line points.
#' @param y_min,y_max Numeric limits for the y-axis (or x-axis when flipped).
#' @return A \code{ggplot} object with \code{height} and \code{width}
#'  attributes (in inches) attached.
#' @keywords internal
#' @importFrom rlang sym %||%
#' @importFrom dplyr %>% group_by summarise n
#' @importFrom tidyr complete
#' @importFrom ggplot2 aes geom_bar geom_text scale_fill_manual labs scale_x_discrete scale_y_continuous guide_legend guide_colorbar
#' @importFrom ggplot2 element_line waiver coord_flip scale_color_manual guide_legend coord_cartesian
#' @importFrom ggrepel geom_text_repel
#' @importFrom scales rescale
BarPlotSingle <- function(
    data,
    x,
    x_sep = "_",
    y = NULL,
    flip = FALSE,
    facet_by = NULL,
    facet_scales = "fixed",
    label = NULL,
    label_nudge = 0.02,
    label_fg = "black",
    label_size = 4,
    label_bg = "white",
    label_bg_r = 0.1,
    add_bg = FALSE,
    bg_palette = "stripe",
    bg_palcolor = NULL,
    bg_alpha = 0.2,
    theme = "theme_this",
    theme_args = list(),
    palette = NULL,
    palcolor = NULL,
    palreverse = FALSE,
    alpha = 1,
    lower_quantile = 0,
    upper_quantile = 0.99,
    lower_cutoff = NULL,
    upper_cutoff = NULL,
    x_text_angle = 0,
    aspect.ratio = 1,
    y_min = NULL,
    y_max = NULL,
    legend.position = "right",
    legend.direction = "vertical",
    add_line = NULL,
    line_color = "red2",
    line_width = .6,
    line_type = 2,
    line_name = NULL,
    add_trend = FALSE,
    trend_color = "black",
    trend_linewidth = 1,
    trend_ptsize = 2.5,
    title = NULL,
    subtitle = NULL,
    xlab = NULL,
    ylab = NULL,
    keep_na = FALSE,
    keep_empty = FALSE,
    expand = waiver(),
    fill_by = TRUE,
    fill_name = NULL,
    width = 0.9,
    ...
) {
    ggplot <- if (getOption("plotthis.gglogger.enabled", FALSE)) {
        gglogger::ggplot
    } else {
        ggplot2::ggplot
    }
    if (inherits(width, "waiver")) {
        width <- 0.9
    }

    x <- check_columns(
        data,
        x,
        force_factor = TRUE,
        allow_multi = TRUE,
        concat_multi = TRUE,
        concat_sep = x_sep
    )
    y <- check_columns(data, y)
    facet_by <- check_columns(
        data,
        facet_by,
        force_factor = TRUE,
        allow_multi = TRUE
    )
    if (is.null(y)) {
        orig_data <- data
        data <- data %>%
            group_by(!!!syms(unique(c(x, facet_by)))) %>%
            summarise(.y = n(), .groups = "drop")
        # keep the factor levels
        for (col in unique(c(x, facet_by))) {
            data[[col]] <- factor(
                data[[col]],
                levels = levels(orig_data[[col]])
            )
        }
        rm(orig_data)
        y <- ".y"
    }
    if (isTRUE(label)) {
        label <- y
    }
    if (isTRUE(fill_by)) {
        fill_by <- x
    }
    if (is.null(fill_by) || isFALSE(fill_by)) {
        fill_by <- ".fill"
        data[[fill_by]] <- factor("")
        fill_guide <- "none"
        fill_is_numeric <- FALSE
        palette <- palette %||% "Paired"
    } else {
        fill_by <- check_columns(data, fill_by, allow_multi = TRUE)
        if (is.numeric(data[[fill_by]])) {
            fill_guide <- guide_colorbar(
                frame.colour = "black",
                ticks.colour = "black",
                title.hjust = 0
            )
            fill_is_numeric <- TRUE
            palette <- palette %||% "Spectral"
        } else {
            fill_by <- check_columns(data, fill_by, force_factor = TRUE)
            fill_guide <- guide_legend(order = 1)
            fill_is_numeric <- FALSE
            palette <- palette %||% "Paired"
        }
    }

    label <- check_columns(data, label)
    if (inherits(expand, "waiver")) {
        expand <- c(0, 0, 0, 0)
        if (!is.null(label)) {
            if (any(data[[y]] > 0)) {
                # what is the best ratio for the label nudge?
                # based on maximum y value?
                expand[1] <- 0.05 + label_nudge * 0.5
            }
            if (any(data[[y]] < 0)) {
                expand[3] <- 0.05 + label_nudge * 0.5
            }
        }
    }
    expand <- norm_expansion(expand, x_type = "discrete", y_type = "continuous")
    data <- process_keep_na_empty(data, keep_na, keep_empty)

    keep_empty_x <- keep_empty[[x]]
    keep_empty_fill <- keep_empty[[fill_by]]

    if (!fill_is_numeric) {
        fill_vals <- levels(data[[fill_by]])
        if (anyNA(data[[fill_by]])) {
            fill_vals <- c(fill_vals, NA)
        }
        colors <- palette_this(
            fill_vals,
            palette = palette,
            palcolor = palcolor,
            NA_keep = TRUE,
            reverse = palreverse
        )
    } else {
        colors <- palette_this(
            palette = palette,
            palcolor = palcolor,
            reverse = palreverse
        )
    }

    feat_colors_value <- NULL
    if (fill_is_numeric) {
        result <- prepare_continuous_color_scale(
            data,
            fill_by,
            lower_quantile = lower_quantile,
            upper_quantile = upper_quantile,
            lower_cutoff = lower_cutoff,
            upper_cutoff = upper_cutoff
        )
        data <- result$data
        feat_colors_value <- result$feat_colors_value
    }

    p <- ggplot(data, aes(x = !!sym(x), y = !!sym(y), fill = !!sym(fill_by)))

    if (isTRUE(add_bg)) {
        p <- p +
            bg_layer(
                data,
                x,
                isTRUE(keep_empty_x),
                bg_palette,
                bg_palcolor,
                bg_alpha,
                facet_by
            )
    }

    just <- calc_just(x_text_angle)

    p <- p + geom_col(alpha = alpha, width = width, show.legend = TRUE)

    if (!is.null(label)) {
        # nudge doesn't work as aes, let's adjust the y
        label_data <- data
        label_data <- label_data[order(label_data[[x]]), , drop = FALSE]
        label_data$.label <- label_data[[label]]
        yr <- diff(range(data[[y]], na.rm = TRUE))
        label_data$.sign <- label_data[[y]] > 0
        label_data[[y]] <- label_data[[y]] +
            yr * label_nudge * ifelse(label_data$.sign, 1, -1)

        if (!isTRUE(flip)) {
            p <- p +
                geom_text_repel(
                    data = label_data,
                    mapping = aes(label = !!sym(".label")),
                    color = label_fg,
                    size = label_size,
                    hjust = if (flip) ifelse(label_data$.sign, 0, 1) else 0.5,
                    bg.color = label_bg,
                    bg.r = label_bg_r,
                    direction = "y",
                    force = 0,
                    min.segment.length = 0,
                    max.overlaps = 100,
                    segment.color = 'transparent'
                )
        } else {
            p <- p +
                geom_text(
                    data = label_data,
                    mapping = aes(label = !!sym(".label")),
                    color = label_fg,
                    size = label_size,
                    hjust = if (flip) ifelse(label_data$.sign, 0, 1) else 0.5,
                    show.legend = FALSE
                )
        }
    }
    p <- p +
        labs(
            title = title,
            subtitle = subtitle,
            x = xlab %||% x,
            y = ylab %||% y
        ) +
        scale_x_discrete(expand = expand$x, drop = !isTRUE(keep_empty_x)) +
        scale_y_continuous(expand = expand$y) +
        do_call(theme, theme_args) +
        ggplot2::theme(
            aspect.ratio = aspect.ratio,
            legend.position = legend.position,
            legend.direction = legend.direction,
            panel.grid.major = element_line(colour = "grey80", linetype = 2),
            axis.text.x = element_text(
                angle = x_text_angle,
                hjust = just$h,
                vjust = just$v
            )
        )

    if (!fill_is_numeric) {
        if (isTRUE(keep_empty_fill)) {
            p <- p +
                scale_fill_manual(
                    name = fill_name %||% fill_by,
                    na.value = colors['NA'] %||% 'grey80',
                    values = colors,
                    guide = fill_guide,
                    breaks = fill_vals,
                    limits = fill_vals,
                    drop = FALSE
                )
        } else {
            p <- p +
                scale_fill_manual(
                    name = fill_name %||% fill_by,
                    na.value = colors['NA'] %||% 'grey80',
                    values = colors,
                    guide = fill_guide
                )
        }
    } else {
        p <- p +
            scale_fill_gradientn(
                name = fill_name %||% fill_by,
                colors = colors,
                n.breaks = 3,
                values = if (!is.null(feat_colors_value)) {
                    scales::rescale(feat_colors_value)
                } else {
                    waiver()
                },
                limits = if (!is.null(feat_colors_value)) {
                    range(feat_colors_value)
                } else {
                    NULL
                },
                guide = fill_guide
            )
    }

    if (isTRUE(add_trend)) {
        p <- p +
            geom_line(
                aes(group = 1),
                position = position_dodge(width = 0.9),
                color = trend_color,
                linewidth = trend_linewidth
            ) +
            geom_point(
                position = position_dodge(width = 0.9),
                color = "black",
                fill = "white",
                size = trend_ptsize,
                shape = 21
            )
    }

    if (!is.null(add_line)) {
        p <- p +
            geom_hline(
                aes(
                    color = line_name %||% paste0(y, " = ", add_line),
                    yintercept = add_line
                ),
                linetype = line_type,
                linewidth = line_width
            ) +
            scale_color_manual(
                name = NULL,
                values = line_color,
                guide = guide_legend(order = 2)
            )
    }

    facet_free <- !is.null(facet_by) &&
        (identical(facet_scales, "free") ||
            (!flip && identical(facet_scales, "free_y")) ||
            (flip && identical(facet_scales, "free_x")))
    if (isTRUE(flip) && !facet_free) {
        p <- p + coord_flip(ylim = c(y_min, y_max))
    } else if (isTRUE(flip)) {
        p <- p + coord_flip()
    } else if (!facet_free) {
        p <- p + coord_cartesian(ylim = c(y_min, y_max))
    }

    x_maxchars <- max(nchar(unlist(strsplit(levels(data[[x]]), "\n"))))
    dims <- calculate_plot_dimensions(
        base_height = if (isTRUE(flip)) {
            max(.5 + nlevels(data[[x]]) * .8, 4)
        } else {
            3.5 + max(x_maxchars * 0.1, 1)
        },
        aspect.ratio = aspect.ratio,
        n_x = if (isTRUE(flip)) NULL else nlevels(data[[x]]),
        x_scale_factor = 0.8,
        legend.position = legend.position,
        legend.direction = legend.direction,
        legend_n = if (!fill_is_numeric) length(fill_vals) else 1,
        legend_nchar = if (!fill_is_numeric) {
            max(nchar(as.character(fill_vals)), na.rm = TRUE)
        } else {
            5
        },
        flip = isTRUE(flip)
    )

    attr(p, "height") <- dims$height
    attr(p, "width") <- dims$width + if (isTRUE(flip)) x_maxchars * 0.1 else 0

    p
}

#' Bar plot with groups
#'
#' @description
#' Core implementation for drawing a grouped bar plot.  Each x-axis category
#' is split into side-by-side (dodge) or stacked bars according to the
#' \code{group_by} variable.  This is the grouped code path dispatched by
#' \code{\link{BarPlotAtomic}} when \code{group_by} is provided.
#'
#' @section Architecture:
#' \enumerate{
#'   \item \strong{Column resolution} — \code{group_by}, \code{facet_by},
#'         \code{x}, and \code{y} are validated and transformed via
#'         \code{\link{check_columns}}.
#'   \item \strong{Count aggregation} — when \code{y = NULL}, the count of
#'         observations per (\code{x}, \code{group_by}, \code{facet_by})
#'         combination is computed.  Factor levels are preserved.
#'   \item \strong{Proportion scaling} — when \code{scale_y = TRUE},
#'         y-values are divided by the sum within each (\code{x},
#'         \code{facet_by}) group so that each x position stacks to 100\%.
#'   \item \strong{Position resolution} — \code{position = "auto"} chooses
#'         \code{"dodge"} for ≤5 groups or \code{"stack"} for >5 groups.
#'         Explicit \code{"dodge"} and \code{"stack"} are also accepted.
#'   \item \strong{Expand calculation} — for stacked bars, expansion is
#'         computed from y-range and label presence.  For dodged bars,
#'         expansion is minimal.  The \code{\link{norm_expansion}()} utility
#'         normalises the final expansion vector.
#'   \item \strong{Colour mapping} — \code{\link{palette_this}()} assigns
#'         per-group colours.  The fill scale \code{drop} argument is
#'         controlled by \code{keep_empty_group}.
#'   \item \strong{Labels} — when \code{label} is set:
#'         \itemize{
#'           \item For \strong{stacked} bars, cumulative label positions are
#'                 computed so each label is centred within its segment.
#'           \item For \strong{dodged} bars, labels are nudged above/below
#'                 the bar top by \code{label_nudge} × the data range.
#'           \item \code{\link[ggrepel]{geom_text_repel}()} is used for
#'                 automatic overlap avoidance.
#'         }
#'   \item \strong{Trend line} — when \code{add_trend = TRUE}, lines connect
#'         bar tops.  When \code{trend_color} is \code{NULL}, each group gets
#'         its own coloured line; otherwise a single colour is used.
#'   \item \strong{Horizontal reference line} — \code{add_line} draws a
#'         \code{geom_hline()} at the specified y-value.
#'   \item \strong{Dimension calculation} — width accounts for the number of
#'         x categories × number of groups (dodge) or just x categories
#'         (stack).  \code{calculate_plot_dimensions()} adjusts for
#'         \code{flip} and legend metrics.
#'   \item \strong{Coordinate transform} — \code{coord_flip()} or
#'         \code{coord_cartesian()} with \code{y_min} / \code{y_max}.
#' }
#'
#' @inheritParams common_args
#' @inheritParams BarPlotSingle
#' @param group_by A character vector of column name(s) to group the bars by.
#'  Each unique combination becomes a separate bar segment.  Multiple columns
#'  are concatenated with \code{group_by_sep}.  Required.
#' @param group_by_sep A character string to separate concatenated
#'  \code{group_by} columns.  Default \code{"_"}.
#' @param group_name A character string for the group fill legend title.
#'  When \code{NULL}, the \code{group_by} column name is used.
#' @param scale_y A logical value.  When \code{TRUE}, y-values are scaled to
#'  proportions within each x position so that each position's total is 100\%.
#'  Only applicable when \code{position = "stack"}.
#' @param position A character string specifying the bar layout:
#'  \code{"auto"} (default: dodge when ≤5 groups, stack otherwise),
#'  \code{"dodge"} (side-by-side), or \code{"stack"} (stacked on top of
#'  each other).
#' @param position_dodge_preserve A character string passed to
#'  \code{\link[ggplot2]{position_dodge2}()}: \code{"total"} preserves the
#'  overall bar group width; \code{"single"} preserves individual bar widths.
#' @return A \code{ggplot} object with \code{height} and \code{width}
#'  attributes (in inches) attached.
#' @keywords internal
#' @importFrom rlang sym syms %||%
#' @importFrom dplyr %>% summarise n mutate ungroup case_when last first
#' @importFrom ggplot2 aes geom_bar scale_fill_manual labs position_dodge2 coord_flip guide_legend scale_color_manual
BarPlotGrouped <- function(
    data,
    x,
    x_sep = "_",
    y = NULL,
    scale_y = FALSE,
    flip = FALSE,
    group_by,
    group_by_sep = "_",
    group_name = NULL,
    theme = "theme_this",
    theme_args = list(),
    palette = "Paired",
    palcolor = NULL,
    palreverse = FALSE,
    label = NULL,
    label_nudge = 0.02,
    label_fg = "black",
    label_size = 4,
    label_bg = "white",
    label_bg_r = 0.1,
    add_bg = FALSE,
    bg_palette = "stripe",
    bg_palcolor = NULL,
    bg_alpha = 0.2,
    alpha = 1,
    x_text_angle = 0,
    aspect.ratio = 1,
    add_line = NULL,
    line_color = "red2",
    line_width = .6,
    line_type = 2,
    line_name = NULL,
    add_trend = FALSE,
    trend_color = "black",
    trend_linewidth = 1,
    trend_ptsize = 2.5,
    position = "auto",
    position_dodge_preserve = "total",
    y_min = NULL,
    y_max = NULL,
    legend.position = "right",
    legend.direction = "vertical",
    title = NULL,
    subtitle = NULL,
    xlab = NULL,
    ylab = NULL,
    keep_na = FALSE,
    keep_empty = FALSE,
    expand = waiver(),
    width = 0.8,
    facet_by = NULL,
    facet_scales = "fixed",
    ...
) {
    ggplot <- if (getOption("plotthis.gglogger.enabled", FALSE)) {
        gglogger::ggplot
    } else {
        ggplot2::ggplot
    }
    group_by <- check_columns(
        data,
        group_by,
        force_factor = TRUE,
        allow_multi = TRUE,
        concat_multi = TRUE,
        concat_sep = group_by_sep
    )
    facet_by <- check_columns(
        data,
        facet_by,
        force_factor = TRUE,
        allow_multi = TRUE
    )
    x <- check_columns(
        data,
        x,
        force_factor = TRUE,
        allow_multi = TRUE,
        concat_multi = TRUE,
        concat_sep = x_sep
    )
    y <- check_columns(data, y)
    orig_data <- data
    if (is.null(y)) {
        data <- data %>%
            dplyr::group_by(!!!syms(unique(c(x, group_by, facet_by)))) %>%
            summarise(.y = n(), .groups = "drop")
        # keep the factor levels
        for (col in unique(c(x, group_by, facet_by))) {
            data[[col]] <- factor(
                data[[col]],
                levels = levels(orig_data[[col]])
            )
        }
        y <- ".y"
    }
    if (isTRUE(label)) {
        label <- y
    }
    label <- check_columns(data, label)
    if (isTRUE(scale_y)) {
        y_scaled <- paste0(y, "_scaled")
        data <- data %>%
            dplyr::group_by(!!!syms(unique(c(x, facet_by)))) %>%
            mutate(!!sym(y_scaled) := !!sym(y) / sum(!!sym(y))) %>%
            ungroup()

        for (col in unique(c(x, facet_by))) {
            data[[col]] <- factor(
                data[[col]],
                levels = levels(orig_data[[col]])
            )
        }
    }
    rm(orig_data)

    if (inherits(width, "waiver")) {
        width <- 0.8
    }

    data <- process_keep_na_empty(data, keep_na, keep_empty)

    keep_empty_x <- keep_empty[[x]]
    keep_empty_group <- keep_empty[[group_by]]
    group_vals <- levels(data[[group_by]])
    if (anyNA(data[[group_by]])) {
        group_vals <- c(group_vals, NA)
    }

    p <- ggplot(
        data,
        aes(
            x = !!sym(x),
            y = !!sym(ifelse(scale_y, y_scaled, y)),
            fill = !!sym(group_by)
        )
    )
    if (isTRUE(add_bg)) {
        p <- p +
            bg_layer(
                data,
                x,
                isTRUE(keep_empty_x),
                bg_palette,
                bg_palcolor,
                bg_alpha,
                facet_by
            )
    }

    colors <- palette_this(
        group_vals,
        palette = palette,
        palcolor = palcolor,
        NA_keep = TRUE,
        reverse = palreverse
    )
    just <- calc_just(x_text_angle)
    if (position == "auto") {
        position <- if (length(colors) <= 5) {
            position_dodge2(preserve = position_dodge_preserve, width = width)
        } else {
            "stack"
        }
    } else if (position == "dodge") {
        position <- position_dodge2(
            preserve = position_dodge_preserve,
            width = width
        )
    }
    if (inherits(expand, "waiver")) {
        if (identical(position, "stack")) {
            expand <- c(top = 0, bottom = 0)
            if (!is.null(label)) {
                if (any(data[[y]] > 0)) {
                    expand['top'] <- 0.05 + label_nudge * 0.5
                }
                if (any(data[[y]] < 0)) {
                    expand['bottom'] <- 0.05 + label_nudge * 0.5
                }
            }
        } else if (min(data[[y]], na.rm = TRUE) >= 0) {
            expand <- c(bottom = 0)
        } else if (max(data[[y]], na.rm = TRUE) <= 0) {
            expand <- c(top = 0)
        } else {
            expand <- NULL
        }
    }
    expand <- norm_expansion(expand, x_type = "discrete", y_type = "continuous")

    p <- p +
        geom_col(
            alpha = alpha,
            position = position,
            width = width,
            show.legend = TRUE
        ) +
        labs(
            title = title,
            subtitle = subtitle,
            x = xlab %||% x,
            y = ylab %||% y
        ) +
        scale_x_discrete(expand = expand$x, drop = !isTRUE(keep_empty_x)) +
        scale_y_continuous(expand = expand$y) +
        do_call(theme, theme_args) +
        ggplot2::theme(
            aspect.ratio = aspect.ratio,
            legend.position = legend.position,
            legend.direction = legend.direction,
            panel.grid.major = element_line(colour = "grey80", linetype = 2),
            axis.text.x = element_text(
                angle = x_text_angle,
                hjust = just$h,
                vjust = just$v
            )
        )

    if (isTRUE(keep_empty_group)) {
        p <- p +
            scale_fill_manual(
                name = group_name %||% group_by,
                values = colors,
                na.value = colors["NA"] %||% "grey80",
                guide = guide_legend(order = 1),
                breaks = group_vals,
                limits = group_vals,
                drop = FALSE
            )
    } else {
        p <- p +
            scale_fill_manual(
                name = group_name %||% group_by,
                values = colors,
                na.value = colors["NA"] %||% "grey80",
                guide = guide_legend(order = 1)
            )
    }

    if (isTRUE(add_trend)) {
        if (is.null(trend_color)) {
            p <- p +
                geom_line(
                    aes(group = !!sym(group_by), color = !!sym(group_by)),
                    position = position_dodge(width = 0.9),
                    linewidth = trend_linewidth,
                    show.legend = FALSE
                ) +
                scale_color_manual(values = colors)
        } else {
            p <- p +
                geom_line(
                    aes(group = !!sym(group_by)),
                    position = position_dodge(width = 0.9),
                    color = trend_color,
                    linewidth = trend_linewidth
                )
        }
        p <- p +
            geom_point(
                position = position_dodge(width = 0.9),
                color = "black",
                fill = "white",
                size = trend_ptsize,
                shape = 21
            )
    }

    if (!is.null(add_line)) {
        p <- p +
            geom_hline(
                aes(
                    color = line_name %||% as.character(add_line),
                    yintercept = add_line
                ),
                linetype = line_type,
                linewidth = line_width
            ) +
            scale_color_manual(
                name = NULL,
                values = line_color,
                guide = guide_legend(order = 2)
            )
    }

    if (!is.null(label)) {
        label_data <- data
        label_data <- label_data[
            order(label_data[[x]], label_data[[group_by]]),
            ,
            drop = FALSE
        ]
        label_data$.label <- label_data[[label]]
        .y <- ifelse(scale_y, y_scaled, y)
        label_data$.sign <- label_data[[.y]] > 0
        yr <- diff(range(data[[.y]], na.rm = TRUE))
        if (identical(position, "stack")) {
            # caculate the y-axis of the labels
            label_data <- mutate(
                label_data,
                !!sym(.y) := ifelse(
                    !!sym(".sign"),
                    cumsum(rev(!!sym(.y))) + yr * label_nudge,
                    cumsum(rev(!!sym(.y))) - yr * label_nudge
                ),
                .label = rev(!!sym(".label")),
                .by = unique(c(x, ".sign", facet_by))
            )

            p <- p +
                geom_text_repel(
                    data = label_data,
                    mapping = aes(label = !!sym(".label")),
                    color = label_fg,
                    size = label_size,
                    hjust = if (flip) ifelse(label_data$.sign, 0, 1) else 0.5,
                    bg.color = label_bg,
                    bg.r = label_bg_r,
                    direction = "y",
                    force = 0,
                    min.segment.length = 0,
                    max.overlaps = 100,
                    segment.color = 'transparent'
                )
        } else {
            label_data[[y]] <- label_data[[y]] +
                yr * label_nudge * ifelse(label_data$.sign, 1, -1)

            p <- p +
                geom_text_repel(
                    data = label_data,
                    mapping = aes(label = !!sym(".label")),
                    color = label_fg,
                    size = label_size,
                    hjust = if (flip) ifelse(label_data$.sign, 0, 1) else 0.5,
                    bg.color = label_bg,
                    bg.r = label_bg_r,
                    position = position,
                    direction = "y",
                    force = 0,
                    min.segment.length = 0,
                    max.overlaps = 100,
                    segment.color = 'transparent'
                )
        }
    }

    facet_free <- !is.null(facet_by) &&
        (identical(facet_scales, "free") ||
            (!flip && identical(facet_scales, "free_y")) ||
            (flip && identical(facet_scales, "free_x")))
    if (isTRUE(flip) && facet_free) {
        p <- p + coord_flip()
    } else if (isTRUE(flip)) {
        p <- p + coord_flip(ylim = c(y_min, y_max))
    } else if (!facet_free) {
        p <- p + coord_cartesian(ylim = c(y_min, y_max))
    }

    n_groups <- length(unique(data[[group_by]]))
    dims <- calculate_plot_dimensions(
        base_height = 4.5,
        aspect.ratio = aspect.ratio,
        n_x = if (is.character(position) && position == "stack") {
            nlevels(data[[x]])
        } else {
            nlevels(data[[x]]) * n_groups
        },
        x_scale_factor = if (is.character(position) && position == "stack") {
            0.8
        } else {
            0.5
        },
        legend.position = legend.position,
        legend.direction = legend.direction,
        legend_n = n_groups,
        legend_nchar = max(nchar(levels(data[[group_by]]))),
        flip = isTRUE(flip)
    )

    attr(p, "height") <- dims$height
    attr(p, "width") <- dims$width

    p
}

#' Atomic bar plot (internal)
#'
#' @description
#' Dispatcher that routes to \code{\link{BarPlotSingle}} (when
#' \code{group_by = NULL}) or \code{\link{BarPlotGrouped}} (when
#' \code{group_by} is provided).  This is the core implementation layer —
#' it takes a **single** data frame (no \code{split_by} support) and returns
#' a \code{ggplot} object with faceting applied.
#'
#' @section Dispatch logic:
#' \itemize{
#'   \item \strong{Without \code{group_by}} — delegates to
#'         \code{BarPlotSingle}.  \code{fill_by} controls bar colouring:
#'         \code{TRUE} (default) fills by x-axis values, \code{FALSE} uses a
#'         single colour, a character column produces discrete colours, and
#'         a numeric column produces a continuous gradient.
#'   \item \strong{With \code{group_by}} — delegates to
#'         \code{BarPlotGrouped}.  \code{fill_by} must match \code{group_by}
#'         or be left as default; an explicit mismatch raises a stop error.
#'         \code{position} controls dodge vs. stack layout with automatic
#'         selection based on group count (≤5 → dodge, >5 → stack).
#' }
#'
#' After the delegate returns, \code{\link{facet_plot}()} wraps the result
#' with \code{facet_wrap} / \code{facet_grid} if \code{facet_by} is provided.
#'
#' @inheritParams common_args
#' @inheritParams BarPlotSingle
#' @inheritParams BarPlotGrouped
#' @param fill_by A variable used to fill the bars.  Both categorical and
#'  numeric columns are accepted:
#'  \itemize{
#'    \item \code{TRUE} (default) — fill by the x-axis values.
#'    \item \code{FALSE} — solid fill (first palette colour).
#'    \item A column name (character/factor) — discrete colour scale.
#'    \item A column name (numeric) — continuous gradient with quantile
#'          / cutoff controls.
#'  }
#'  Ignored when \code{group_by} is provided (fill is determined by
#'  \code{group_by}).
#' @param fill_name A character string for the fill legend title.  Only
#'  applies when \code{group_by = NULL} and the fill is from \code{fill_by}.
#' @param facet_args A list of additional arguments passed to the faceting
#'  function (e.g., \code{scales}, \code{labeller}).
#' @return A \code{ggplot} object, possibly faceted, with \code{height}
#'  and \code{width} attributes (in inches) attached.
#' @importFrom ggplot2 waiver
#' @keywords internal
BarPlotAtomic <- function(
    data,
    x,
    x_sep = "_",
    y = NULL,
    scale_y = FALSE,
    flip = FALSE,
    group_by = NULL,
    group_by_sep = "_",
    group_name = NULL,
    fill_by = TRUE,
    fill_name = NULL,
    label_nudge = 0.02,
    label = NULL,
    label_fg = "black",
    label_size = 4,
    label_bg = "white",
    label_bg_r = 0.1,
    add_bg = FALSE,
    bg_palette = "stripe",
    bg_palcolor = NULL,
    bg_alpha = 0.2,
    theme = "theme_this",
    theme_args = list(),
    palette = NULL,
    palcolor = NULL,
    palreverse = FALSE,
    alpha = 1,
    lower_quantile = 0,
    upper_quantile = 0.99,
    lower_cutoff = NULL,
    upper_cutoff = NULL,
    x_text_angle = 0,
    aspect.ratio = 1,
    add_line = NULL,
    line_color = "red2",
    line_width = .6,
    line_type = 2,
    line_name = NULL,
    add_trend = FALSE,
    trend_color = "black",
    trend_linewidth = 1,
    trend_ptsize = 2,
    position = "auto",
    position_dodge_preserve = "total",
    y_min = NULL,
    y_max = NULL,
    legend.position = "right",
    legend.direction = "vertical",
    title = NULL,
    subtitle = NULL,
    xlab = NULL,
    ylab = NULL,
    keep_na = FALSE,
    keep_empty = FALSE,
    expand = waiver(),
    width = waiver(),
    facet_by = NULL,
    facet_scales = "fixed",
    facet_nrow = NULL,
    facet_ncol = NULL,
    facet_byrow = TRUE,
    facet_args = list(),
    ...
) {
    if (is.null(group_by)) {
        p <- BarPlotSingle(
            data,
            x,
            x_sep,
            y,
            label = label,
            label_nudge = label_nudge,
            label_fg = label_fg,
            label_size = label_size,
            label_bg = label_bg,
            label_bg_r = label_bg_r,
            facet_by = facet_by,
            facet_scales = facet_scales,
            flip = flip,
            line_name = line_name,
            theme = theme,
            theme_args = theme_args,
            palette = palette,
            palcolor = palcolor,
            palreverse = palreverse,
            alpha = alpha,
            lower_quantile = lower_quantile,
            upper_quantile = upper_quantile,
            lower_cutoff = lower_cutoff,
            upper_cutoff = upper_cutoff,
            x_text_angle = x_text_angle,
            aspect.ratio = aspect.ratio,
            add_bg = add_bg,
            add_line = add_line,
            line_color = line_color,
            line_width = line_width,
            line_type = line_type,
            add_trend = add_trend,
            trend_color = trend_color,
            trend_linewidth = trend_linewidth,
            trend_ptsize = trend_ptsize,
            legend.position = legend.position,
            legend.direction = legend.direction,
            y_min = y_min,
            y_max = y_max,
            title = title,
            subtitle = subtitle,
            xlab = xlab,
            ylab = ylab,
            keep_na = keep_na,
            keep_empty = keep_empty,
            expand = expand,
            fill_by = fill_by,
            fill_name = fill_name,
            width = width,
            ...
        )
    } else {
        stopifnot(
            "[BarPlot] `fill_by` cannot be applied when `group_by` is specified." = !missing(
                fill_by
            ) ||
                identical(fill_by, group_by)
        )
        p <- BarPlotGrouped(
            data,
            x,
            x_sep,
            y,
            label = label,
            label_nudge = label_nudge,
            label_fg = label_fg,
            label_size = label_size,
            label_bg = label_bg,
            label_bg_r = label_bg_r,
            scale_y = scale_y,
            group_by = group_by,
            group_by_sep = group_by_sep,
            group_name = group_name,
            facet_by = facet_by,
            facet_scales = facet_scales,
            flip = flip,
            line_name = line_name,
            add_bg = add_bg,
            bg_palette = bg_palette,
            bg_palcolor = bg_palcolor,
            bg_alpha = bg_alpha,
            theme = theme,
            theme_args = theme_args,
            palette = palette %||% "Paired",
            palcolor = palcolor,
            palreverse = palreverse,
            alpha = alpha,
            x_text_angle = x_text_angle,
            aspect.ratio = aspect.ratio,
            position = position,
            position_dodge_preserve = position_dodge_preserve,
            y_min = y_min,
            y_max = y_max,
            add_line = add_line,
            line_color = line_color,
            line_width = line_width,
            line_type = line_type,
            add_trend = add_trend,
            trend_color = trend_color,
            trend_linewidth = trend_linewidth,
            trend_ptsize = trend_ptsize,
            legend.position = legend.position,
            legend.direction = legend.direction,
            title = title,
            subtitle = subtitle,
            xlab = xlab,
            ylab = ylab,
            keep_na = keep_na,
            keep_empty = keep_empty,
            expand = expand,
            width = width,
            ...
        )
    }
    keep_empty_facet <- if (!is.null(facet_by)) {
        keep_empty[[facet_by[1]]]
    } else {
        NULL
    }
    if (length(facet_by) > 1) {
        stopifnot(
            "[BarPlot] `keep_empty` for `facet_by` variables must be identical." = identical(
                keep_empty_facet,
                keep_empty[[facet_by[2]]]
            )
        )
    }

    facet_args$plot <- p
    facet_args["facet_by"] <- list(facet_by)
    facet_args["facet_scales"] <- list(facet_scales)
    facet_args["drop"] <- list(!isTRUE(keep_empty_facet))
    facet_args["nrow"] <- list(facet_nrow)
    facet_args["ncol"] <- list(facet_ncol)
    facet_args["byrow"] <- list(facet_byrow)
    facet_args["legend.position"] <- list(legend.position)
    facet_args["legend.direction"] <- list(legend.direction)
    do_call(facet_plot, facet_args)
}

#' Bar / SplitBar / Waterfall Plot
#'
#' @description
#' `BarPlot` draws bar plots with flexible fill, grouping, labelling, and annotation
#' options.  Supports both simple single-colour bars and grouped bars (dodged
#' or stacked).  Bars can be filled by a categorical variable (discrete
#' colour scale), a continuous variable (colour gradient), or a fixed colour.
#'
#' The function supports \strong{count aggregation} (omit \code{y} to plot
#' observation counts), \strong{proportion scaling} (\code{scale_y = TRUE}
#' for grouped bars), background stripes (\code{add_bg}), bar labels, trend
#' lines, horizontal reference lines, and splitting into separate sub-plots
#' via \code{split_by}.
#'
#' @section split_by workflow:
#' When \code{split_by} is provided:
#' \enumerate{
#'   \item \code{\link{check_keep_na}()} and \code{\link{check_keep_empty}()}
#'         normalise the \code{keep_na} / \code{keep_empty} arguments for all
#'         columns (\code{x}, \code{split_by}, \code{facet_by}, \code{group_by}).
#'   \item The \code{split_by} column is validated and its NA / empty levels
#'         are processed.  It is then removed from the per-column
#'         \code{keep_na} / \code{keep_empty} lists.
#'   \item The data is split by \code{split_by} (preserving level order).  If
#'         \code{split_by} is \code{NULL}, the data is wrapped in a
#'         single-element list with name \code{"..."}.
#'   \item Per-split \code{palette}, \code{palcolor},
#'         \code{legend.position}, and \code{legend.direction} are resolved
#'         via \code{\link{check_palette}()}, \code{\link{check_palcolor}()},
#'         and \code{\link{check_legend}()}.
#'   \item \code{\link{BarPlotAtomic}()} is called for each split.
#'   \item Results are combined via \code{\link{combine_plots}()} (when
#'         \code{combine = TRUE}) or returned as a named list.
#' }
#'
#' @inheritParams common_args
#' @inheritParams BarPlotAtomic
#' @param split_by The column(s) to split the data by and produce separate
#'  sub-plots.  Multiple columns are concatenated with \code{split_by_sep}.
#' @param split_by_sep A character string to separate concatenated
#'  \code{split_by} columns.  Default \code{"_"}.
#' @param seed A numeric seed for reproducibility.
#' @param combine Logical; when \code{TRUE} (default), returns a combined
#'  \code{patchwork} object.  When \code{FALSE}, returns a named list of
#'  individual \code{ggplot} objects.
#' @param ncol,nrow Integer number of columns / rows for the combined layout.
#' @param byrow Logical; fill the combined layout by row (default \code{TRUE}).
#' @param axes,axis_titles Character strings for how axes and axis titles are
#'  treated across the combined layout.
#' @param guides A character string specifying how legends are collected
#'  across panels.
#' @param design A custom layout design for the combined plot.
#' @rdname barplot
#' @return A \code{ggplot} object, a \code{patchwork} object, or a named list
#'  of \code{ggplot} objects (when \code{combine = FALSE}), each with
#'  \code{height} and \code{width} attributes in inches.
#' @export
#' @importFrom ggplot2 waiver
#' @examples
#' \donttest{
#' data <- data.frame(
#'     x = c("A", "B", "C", "D", "E", "F", "G", "H"),
#'     y = c(10, 8, 16, 4, 6, 12, 14, 2),
#'     group = c("G1", "G1", "G2", "G2", "G3", "G3", "G4", "G4"),
#'     facet = c("F1", "F2", "F3", "F4", "F1", "F2", "F3", "F4")
#' )
#'
#' # Single-colour bars
#' BarPlot(data, x = "x", y = "y")
#'
#' # Solid fill (no colour mapping)
#' BarPlot(data, x = "x", y = "y", fill_by = FALSE)
#'
#' # Label bar tops
#' BarPlot(data, x = "x", y = "y", label = TRUE)
#' BarPlot(data, x = "x", y = "y", label = "facet", label_nudge = 0)
#'
#' # Grouped bars
#' BarPlot(data, x = "group", y = "y", group_by = "x")
#'
#' # Dodged bars with background stripes
#' BarPlot(data,
#'     x = "group", y = "y", group_by = "x",
#'     position = "dodge", add_bg = TRUE)
#'
#' # split_by with faceting
#' BarPlot(data,
#'     x = "x", y = "y", split_by = "group",
#'     facet_by = "facet", position = "dodge", facet_ncol = 1)
#'
#' # split_by with collected guides
#' BarPlot(data,
#'     x = "x", y = "y", split_by = "group", facet_by = "facet",
#'     position = "dodge", facet_ncol = 1, guides = 'collect')
#'
#' # Per-split palettes
#' BarPlot(data,
#'     x = "x", y = "y", split_by = "group",
#'     palette = list(G1 = "Reds", G2 = "Blues", G3 = "Greens", G4 = "Purp"),
#'     facet_by = "facet", position = "dodge", facet_ncol = 1)
#'
#' # Background stripe palette
#' BarPlot(data,
#'     x = "group", y = "y", group_by = "x",
#'     position = "dodge", add_bg = TRUE, bg_palette = "Spectral")
#'
#' # Count bars (y = NULL)
#' BarPlot(data, x = "group", ylab = "count")
#'
#' # Flipped axes
#' BarPlot(data, x = "group", flip = TRUE, ylab = "count")
#'
#' # Numeric fill_by with colour gradient
#' BarPlot(data, x = "x", y = "y", fill_by = "y", flip = TRUE)
#'
#' # Control fill colour scale limits (quantile)
#' BarPlot(data, x = "x", y = "y", fill_by = "y", flip = TRUE,
#'         lower_quantile = 0.1, upper_quantile = 0.9)
#'
#' # Control fill colour scale limits (explicit cutoff)
#' BarPlot(data, x = "x", y = "y", fill_by = "y", flip = TRUE,
#'         lower_cutoff = 5, upper_cutoff = 12)
#'
#' # keep_na and keep_empty examples
#' data <- data.frame(
#'     x = factor(c("A", "B", "C", "D", "E", "F", NA, "H"),
#'                levels = LETTERS[1:10]),
#'     y = c(10, 8, 16, 4, 6, NA, 14, 2),
#'     group = factor(c("G1", "G1", "G2", NA, "G3", "G3", "G5", "G5"),
#'                    levels = c("G1", "G2", "G3", "G4", "G5")),
#'     facet = factor(c("F1", NA, "F3", "F4", "F1", "F2", "F3", "F4"),
#'                    levels = c("F1", "F2", "F3", "F4", "F5"))
#' )
#'
#' # Default: NA and empty levels dropped
#' BarPlot(data, x = "x", y = "y")
#'
#' # Keep both NA and empty levels
#' BarPlot(data, x = "x", y = "y",
#'         keep_na = TRUE, keep_empty = TRUE)
#'
#' # With faceting
#' BarPlot(data, x = "x", y = "y",
#'         keep_na = TRUE, keep_empty = TRUE, facet_by = "facet")
#'
#' # Keep NA, hide empty levels but reserve their colours
#' BarPlot(data, x = "x", y = "y",
#'         keep_na = TRUE, keep_empty = 'level')
#'
#' # Per-column keep_na / keep_empty
#' BarPlot(data, x = "x", y = "y",
#'         keep_na = list(x = TRUE), keep_empty = list(x = FALSE))
#'
#' # Grouped bars with keep_na / keep_empty
#' BarPlot(data, x = "group", y = "y", group_by = "x")
#' BarPlot(data, x = "group", y = "y", group_by = "x",
#'         keep_na = TRUE, keep_empty = TRUE)
#' BarPlot(data, x = "group", y = "y", group_by = "x",
#'         keep_na = TRUE, keep_empty = TRUE, facet_by = "facet")
#'
#' # Per-column on grouped bars
#' BarPlot(data, x = "group", y = "y", group_by = "x",
#'         keep_na = list(x = TRUE, group = FALSE),
#'         keep_empty = list(x = FALSE, group = TRUE))
#' }
BarPlot <- function(
    data,
    x,
    x_sep = "_",
    y = NULL,
    flip = FALSE,
    fill_by = TRUE,
    fill_name = NULL,
    line_name = NULL,
    label_nudge = 0.02,
    label = NULL,
    label_fg = "black",
    label_size = 4,
    label_bg = "white",
    label_bg_r = 0.1,
    group_by = NULL,
    group_by_sep = "_",
    group_name = NULL,
    split_by = NULL,
    split_by_sep = "_",
    facet_by = NULL,
    facet_scales = "fixed",
    facet_ncol = NULL,
    facet_nrow = NULL,
    facet_byrow = TRUE,
    facet_args = list(),
    add_bg = FALSE,
    bg_palette = "stripe",
    bg_palcolor = NULL,
    bg_alpha = 0.2,
    add_line = NULL,
    line_color = "red2",
    line_width = .6,
    line_type = 2,
    add_trend = FALSE,
    trend_color = "black",
    trend_linewidth = 1,
    trend_ptsize = 2,
    theme = "theme_this",
    theme_args = list(),
    palette = NULL,
    palcolor = NULL,
    palreverse = FALSE,
    alpha = 1,
    lower_quantile = 0,
    upper_quantile = 0.99,
    lower_cutoff = NULL,
    upper_cutoff = NULL,
    x_text_angle = 0,
    aspect.ratio = 1,
    y_min = NULL,
    y_max = NULL,
    position = "auto",
    position_dodge_preserve = "total",
    legend.position = "right",
    legend.direction = "vertical",
    title = NULL,
    subtitle = NULL,
    xlab = NULL,
    ylab = NULL,
    keep_empty = FALSE,
    keep_na = FALSE,
    expand = waiver(),
    width = waiver(),
    combine = TRUE,
    nrow = NULL,
    ncol = NULL,
    byrow = TRUE,
    seed = 8525,
    axes = NULL,
    axis_titles = axes,
    guides = NULL,
    design = NULL,
    ...
) {
    validate_common_args(seed, facet_by = facet_by)
    keep_na <- check_keep_na(keep_na, c(x, split_by, facet_by, group_by))
    keep_empty <- check_keep_empty(
        keep_empty,
        c(x, split_by, facet_by, group_by)
    )
    theme <- process_theme(theme)
    split_by <- check_columns(
        data,
        split_by,
        force_factor = TRUE,
        allow_multi = TRUE,
        concat_multi = TRUE,
        concat_sep = split_by_sep
    )

    if (!is.null(split_by)) {
        data <- process_keep_na_empty(data, keep_na, keep_empty, col = split_by)
        keep_na[[split_by]] <- NULL
        keep_empty[[split_by]] <- NULL

        data[[split_by]] <- droplevels(data[[split_by]])
        datas <- split(data, data[[split_by]])
        # keep the order of levels
        datas <- datas[levels(data[[split_by]])]
    } else {
        datas <- list(data)
        split_by <- names(datas) <- "..."
    }

    palette <- if (is.null(palette)) {
        list()
    } else {
        check_palette(palette, names(datas))
    }
    palcolor <- check_palcolor(palcolor, names(datas))
    legend.direction <- check_legend(
        legend.direction,
        names(datas),
        "legend.direction"
    )
    legend.position <- check_legend(
        legend.position,
        names(datas),
        "legend.position"
    )

    plots <- lapply(
        names(datas),
        function(nm) {
            title <- title %||%
                (if (length(datas) == 1 && identical(nm, "...")) NULL else nm)
            BarPlotAtomic(
                datas[[nm]],
                label = label,
                label_nudge = label_nudge,
                label_fg = label_fg,
                label_size = label_size,
                label_bg = label_bg,
                label_bg_r = label_bg_r,
                x = x,
                x_sep = x_sep,
                y = y,
                flip = flip,
                group_by = group_by,
                group_by_sep = group_by_sep,
                group_name = group_name,
                fill_by = fill_by,
                fill_name = fill_name,
                theme = theme,
                theme_args = theme_args,
                palette = palette[[nm]],
                palcolor = palcolor[[nm]],
                palreverse = palreverse,
                alpha = alpha,
                lower_quantile = lower_quantile,
                upper_quantile = upper_quantile,
                lower_cutoff = lower_cutoff,
                upper_cutoff = upper_cutoff,
                add_bg = add_bg,
                bg_palette = bg_palette,
                bg_palcolor = bg_palcolor,
                bg_alpha = bg_alpha,
                x_text_angle = x_text_angle,
                aspect.ratio = aspect.ratio,
                line_name = line_name,
                add_line = add_line,
                line_color = line_color,
                line_width = line_width,
                line_type = line_type,
                add_trend = add_trend,
                trend_color = trend_color,
                trend_linewidth = trend_linewidth,
                trend_ptsize = trend_ptsize,
                position = position,
                position_dodge_preserve = position_dodge_preserve,
                y_min = y_min,
                y_max = y_max,
                legend.position = legend.position[[nm]],
                legend.direction = legend.direction[[nm]],
                title = title,
                subtitle = subtitle,
                xlab = xlab,
                ylab = ylab,
                keep_empty = keep_empty,
                keep_na = keep_na,
                expand = expand,
                width = width,
                facet_by = facet_by,
                facet_scales = facet_scales,
                facet_ncol = facet_ncol,
                facet_nrow = facet_nrow,
                facet_byrow = facet_byrow,
                facet_args = facet_args,
                ...
            )
        }
    )

    names(plots) <- names(datas)

    combine_plots(
        plots,
        combine = combine,
        split_by = split_by,
        nrow = nrow,
        ncol = ncol,
        byrow = byrow,
        axes = axes,
        axis_titles = axis_titles,
        guides = guides,
        design = design
    )
}

#' Atomic split bar plot (internal)
#'
#' @description
#' Core implementation for drawing a split (divergent / waterfall-style) bar
#' plot.  Bars extend left (negative values) and right (positive values) from
#' a central zero line, with the y-axis listing categories.  Bars can be
#' coloured by a categorical or continuous fill variable, and their opacity
#' can encode an additional numeric variable via \code{alpha_by}.
#'
#' This is the workhorse behind the exported \code{\link{SplitBarPlot}}
#' (also aliased as \code{\link{WaterfallPlot}}).
#'
#' @section Architecture:
#' \enumerate{
#'   \item \strong{Column resolution} — \code{x}, \code{y}, \code{fill_by},
#'         \code{alpha_by}, and \code{facet_by} are validated via
#'         \code{\link{check_columns}}.  Multi-column \code{y} and
#'         \code{fill_by} are concatenated with their respective separators.
#'   \item \strong{Direction assignment} — a \code{direction_name} column
#'         is created from the sign of \code{x}, assigning each row to the
#'         positive or negative group (customisable via
#'         \code{direction_pos_name} / \code{direction_neg_name}).
#'   \item \strong{Fill resolution} — if \code{fill_by} is \code{NULL},
#'         the direction column is used as the fill (two-colour palette).
#'         Categorical fills use a discrete colour scale; numeric fills use
#'         \code{\link{prepare_continuous_color_scale}()} with quantile /
#'         cutoff clamping and \code{scale_fill_gradientn()}.
#'   \item \strong{Alpha by} — when \code{alpha_by} is provided, bar
#'         opacity encodes an additional numeric variable via
#'         \code{scale_alpha_continuous()}.
#'   \item \strong{Ordering} — \code{order_y} is a named list controlling
#'         the vertical ordering of bars.  The keys \code{"+"} and \code{"-"}
#'         specify separate orderings for positive and negative bars; key
#'         \code{"*"} applies a single ordering to all bars.  Values are
#'         character vectors of ordering criteria: \code{"x_asc"},
#'         \code{"x_desc"}, \code{"alpha_asc"}, \code{"alpha_desc"}.
#'   \item \strong{Empty level padding} — when \code{keep_empty_y = TRUE},
#'         missing y-levels are padded per facet (or globally) with
#'         zero-height bars so they still appear on the axis.
#'   \item \strong{Text labels} — category names are drawn beside the bars
#'         via \code{geom_text()} at the zero line.  When flipped, labels are
#'         rotated 90°.  Long labels are wrapped via
#'         \code{\link[stringr]{str_wrap}()} using \code{max_charwidth}.
#'   \item \strong{Plot assembly} — the ggplot is built with
#'         \code{geom_vline(xintercept = 0)} for the centre line,
#'         \code{geom_col()} for bars, and the appropriate fill / alpha
#'         scales.
#'   \item \strong{Dimension calculation} — \code{calculate_plot_dimensions()}
#'         computes dimensions from the number of y categories, scaled by
#'         \code{bar_height / 4}.  Flipping adjusts the aspect ratio.
#'   \item \strong{Faceting} — \code{\link{facet_plot}()} wraps the result,
#'         defaulting to \code{facet_scales = "free_y"}.
#' }
#'
#' @inheritParams common_args
#' @param x A character string specifying the numeric column for the
#'  x-axis.  Values determine bar direction: positive → right, negative →
#'  left.
#' @param y A character string (or vector) specifying the column(s) for the
#'  category axis.  Each unique value becomes a bar.  Multiple columns are
#'  concatenated with \code{y_sep}.
#' @param y_sep A character string to join multiple \code{y} columns.
#'  Default \code{"_"}.
#' @param flip Logical; if \code{TRUE}, swap the x and y axes (bars become
#'  vertical rather than horizontal).
#' @param alpha_by A character string naming a numeric column to encode as
#'  bar opacity.  Default \code{NULL} (all bars fully opaque).
#' @param alpha_reverse Logical; if \code{TRUE}, reverse the alpha scale
#'  direction (solid for low values, transparent for high).
#' @param alpha_name A character string for the alpha legend title.
#' @param order_y A named list controlling the vertical ordering of bars.
#'  Keys are \code{"+"} (positive bars), \code{"-"} (negative bars), or
#'  \code{"*"} (all bars).  Values are character vectors of ordering
#'  criteria: \code{"x_asc"}, \code{"x_desc"}, \code{"alpha_asc"},
#'  \code{"alpha_desc"}.  Default orders positive bars by descending x and
#'  descending alpha; negative bars by descending x and ascending alpha.
#' @param bar_height A numeric value (0–1) specifying the bar height as a
#'  fraction of the available category slot.
#' @param lineheight A numeric value controlling the line height of wrapped
#'  category labels.
#' @param max_charwidth An integer specifying the maximum character width
#'  for wrapping category labels.
#' @param fill_by A character string (or vector) naming the column(s) for
#'  bar fill colour.  If \code{NULL}, the direction (positive/negative) is
#'  used.  Can be categorical or numeric.
#' @param fill_by_sep A character string to join multiple \code{fill_by}
#'  columns.  Default \code{"_"}.
#' @param fill_name A character string for the fill legend title.
#' @param direction_name A character string naming the internal direction
#'  column (used in legends).  Default \code{"direction"}.
#' @param direction_pos_name A character string labelling the positive
#'  direction in the legend.  Default \code{"positive"}.
#' @param direction_neg_name A character string labelling the negative
#'  direction in the legend.  Default \code{"negative"}.
#' @param x_min,x_max Numeric limits for the x-axis.  When \code{NULL},
#'  symmetric limits are computed from the maximum absolute x-value.
#' @return A \code{ggplot} object, possibly faceted, with \code{height}
#'  and \code{width} attributes (in inches) attached.
#' @keywords internal
#' @importFrom stringr str_wrap
#' @importFrom forcats fct_relabel
#' @importFrom dplyr .data
#' @importFrom ggplot2 aes geom_vline geom_col geom_text scale_fill_manual labs scale_y_discrete scale_alpha_continuous
#' @importFrom ggplot2 scale_alpha_continuous guide_none guide_legend
#' @importFrom scales rescale
SplitBarPlotAtomic <- function(
    data,
    x,
    y,
    y_sep = "_",
    flip = FALSE,
    alpha_by = NULL,
    alpha_reverse = FALSE,
    alpha_name = NULL,
    order_y = list(
        "+" = c("x_desc", "alpha_desc"),
        "-" = c("x_desc", "alpha_asc")
    ),
    bar_height = 0.9,
    lineheight = 0.5,
    max_charwidth = 80,
    fill_by = NULL,
    fill_by_sep = "_",
    fill_name = NULL,
    direction_name = "direction",
    direction_pos_name = "positive",
    direction_neg_name = "negative",
    theme = "theme_this",
    theme_args = list(),
    palette = "Spectral",
    palcolor = NULL,
    palreverse = FALSE,
    lower_quantile = 0,
    upper_quantile = 0.99,
    lower_cutoff = NULL,
    upper_cutoff = NULL,
    facet_by = NULL,
    facet_scales = "free_y",
    facet_nrow = NULL,
    facet_ncol = NULL,
    facet_byrow = TRUE,
    aspect.ratio = 1,
    x_min = NULL,
    x_max = NULL,
    legend.position = "right",
    legend.direction = "vertical",
    title = NULL,
    subtitle = NULL,
    xlab = NULL,
    ylab = NULL,
    keep_empty = FALSE,
    keep_na = FALSE,
    ...
) {
    ggplot <- if (getOption("plotthis.gglogger.enabled", FALSE)) {
        gglogger::ggplot
    } else {
        ggplot2::ggplot
    }
    x <- check_columns(data, x)
    y <- check_columns(
        data,
        y,
        force_factor = TRUE,
        allow_multi = TRUE,
        concat_multi = TRUE,
        concat_sep = y_sep
    )
    fill_by <- check_columns(data, fill_by, allow_multi = TRUE)
    fill_by <- fill_by %||% direction_name
    data[[direction_name]] <- ifelse(
        data[[x]] > 0,
        direction_pos_name,
        direction_neg_name
    )
    data[[direction_name]] <- factor(
        data[[direction_name]],
        levels = c(direction_pos_name, direction_neg_name)
    )
    if (is.numeric(data[[fill_by]])) {
        fill_by_numeric <- TRUE
    } else {
        fill_by_numeric <- FALSE
        fill_by <- check_columns(
            data,
            fill_by,
            force_factor = TRUE,
            allow_multi = TRUE,
            concat_multi = TRUE,
            concat_sep = fill_by_sep
        )
    }

    feat_colors_value <- NULL
    if (fill_by_numeric) {
        result <- prepare_continuous_color_scale(
            data,
            fill_by,
            lower_quantile = lower_quantile,
            upper_quantile = upper_quantile,
            lower_cutoff = lower_cutoff,
            upper_cutoff = upper_cutoff
        )
        data <- result$data
        feat_colors_value <- result$feat_colors_value
    }

    alpha_by <- check_columns(data, alpha_by)
    facet_by <- check_columns(
        data,
        facet_by,
        force_factor = TRUE,
        allow_multi = TRUE
    )

    if (is.null(alpha_by)) {
        data$.alpha <- 1
        alpha_by <- ".alpha"
        alpha_guide <- guide_none()
    } else {
        alpha_guide <- guide_legend(order = 2)
    }

    if (!is.null(order_y)) {
        if (!is.list(order_y)) {
            order_y <- list("*" = order_y)
        }
        if (length(order_y) != 1 && length(order_y) != 2) {
            stop("'order_y' must be a list of length 1 or 2.")
        }
        if (length(order_y) == 1 && names(order_y) != "*") {
            stop(
                "The name of the 'order_y' list must be '*' when it has only one element."
            )
        }
        if (length(order_y) == 2 && !all(c("+", "-") %in% names(order_y))) {
            stop(
                "The names of the 'order_y' list must be '+' and '-' when it has two elements."
            )
        }
        for (o in order_y) {
            sapply(
                o,
                match.arg,
                c("x_asc", "x_desc", "alpha_asc", "alpha_desc")
            )
        }

        order_df <- function(df, dirs) {
            order_list <- list()
            for (dir in dirs) {
                if (dir == "x_asc") {
                    order_list[[dir]] <- df[[x]]
                } else if (dir == "x_desc") {
                    order_list[[dir]] <- -df[[x]]
                } else if (dir == "alpha_asc") {
                    order_list[[dir]] <- df[[alpha_by]]
                } else if (dir == "alpha_desc") {
                    order_list[[dir]] <- -df[[alpha_by]]
                }
            }
            df[do_call(order, order_list), , drop = FALSE]
        }
        if (length(order_y) == 1) {
            # *
            data <- order_df(data, order_y[[1]])
        } else {
            # +, -
            data[[direction_name]][is.na(data[[x]])] <- "positive"
            data[[x]][is.na(data[[x]])] <- 0
            data_pos <- order_df(
                data[data[[direction_name]] == direction_pos_name, ],
                order_y[["+"]]
            )
            data_neg <- order_df(
                data[data[[direction_name]] == direction_neg_name, ],
                order_y[["-"]]
            )
            data <- rbind(data_pos, data_neg)
            rm(data_pos, data_neg)
        }
        ordered_levels <- rev(unique(as.character(data[[y]])))
        ordered_levels <- c(
            ordered_levels,
            setdiff(levels(data[[y]]), unique(data[[y]]))
        )
        data[[y]] <- factor(data[[y]], levels = ordered_levels)
    }
    data <- process_keep_na_empty(data, keep_na, keep_empty)
    data[[y]] <- fct_relabel(data[[y]], str_wrap, width = max_charwidth)

    # true: unused levels are kept on Y axis
    # false: unused levels are dropped
    # level: unused levels are dropped, but kept for determine fill_by colors
    keep_empty_y <- keep_empty[[y]]
    keep_empty_fill <- if (!fill_by_numeric) keep_empty[[fill_by]] else NULL
    keep_empty_facet <- if (!is.null(facet_by)) {
        keep_empty[[facet_by[1]]]
    } else {
        NULL
    }
    if (length(facet_by) > 1) {
        stopifnot(
            "[SplitBarPlot] `keep_empty` for `facet_by` variables must be identical." = identical(
                keep_empty_facet,
                keep_empty[[facet_by[2]]]
            )
        )
    }
    if (isTRUE(keep_empty_y)) {
        # Determine all possible y values globally (levels + NA if present anywhere)
        all_y_vals_global <- unique(c(
            levels(data[[y]]),
            if (anyNA(data[[y]])) NA else NULL
        ))

        # Create facet grouping (or single group if no facets)
        if (!is.null(facet_by)) {
            facet_cols <- if (length(facet_by) > 1) {
                interaction(data[facet_by], drop = TRUE, sep = "___")
            } else {
                data[[facet_by[1]]]
            }
        } else {
            facet_cols <- factor(rep("__single__", nrow(data)))
        }

        # Add missing levels per facet (or globally if no facets)
        data_list <- split(data, facet_cols)
        data_list <- lapply(names(data_list), function(facet_val) {
            facet_data <- data_list[[facet_val]]
            y_vals <- unique(facet_data[[y]])
            missing_levels <- setdiff(all_y_vals_global, y_vals)

            if (length(missing_levels) > 0) {
                add_data <- data.frame(matrix(
                    NA,
                    nrow = length(missing_levels),
                    ncol = ncol(facet_data)
                ))
                colnames(add_data) <- colnames(facet_data)
                add_data[[y]] <- missing_levels
                add_data[[x]] <- 0
                add_data[[direction_name]] <- "positive"

                # Set facet variables if they exist
                if (!is.null(facet_by)) {
                    if (length(facet_by) > 1) {
                        facet_vals <- strsplit(facet_val, "___", fixed = TRUE)[[
                            1
                        ]]
                        for (i in seq_along(facet_by)) {
                            add_data[[facet_by[i]]] <- facet_vals[i]
                        }
                    } else {
                        add_data[[facet_by[1]]] <- facet_val
                    }
                }

                facet_data <- rbind(facet_data, add_data)
            }
            facet_data
        })
        data <- do_call(rbind, data_list)
        rownames(data) <- NULL
    }

    x_min <- x_min %||% -max(abs(data[[x]]))
    x_max <- x_max %||% max(abs(data[[x]]))

    p <- ggplot(data, aes(x = !!sym(x), y = !!sym(y))) +
        geom_vline(xintercept = 0) +
        geom_col(
            aes(fill = !!sym(fill_by), alpha = !!sym(alpha_by)),
            color = "black",
            width = bar_height,
            show.legend = TRUE
        ) +
        scale_alpha_continuous(
            name = alpha_name %||% alpha_by,
            range = if (alpha_reverse) c(1, 0.1) else c(0.1, 1),
            guide = alpha_guide
        )

    if (!fill_by_numeric) {
        fill_vals <- if (!isFALSE(keep_empty_fill) || !anyNA(data[[fill_by]])) {
            levels(data[[fill_by]])
        } else {
            c(levels(data[[fill_by]]), NA)
        }
        fill_colors <- palette_this(
            fill_vals,
            palette = palette,
            palcolor = palcolor,
            NA_keep = TRUE,
            reverse = palreverse
        )

        if (isTRUE(keep_empty_fill)) {
            p <- p +
                scale_fill_manual(
                    name = fill_name %||% fill_by,
                    values = fill_colors,
                    na.value = fill_colors['NA'] %||% "grey80",
                    guide = guide_legend(order = 1),
                    breaks = fill_vals,
                    limits = fill_vals,
                    drop = FALSE
                )
        } else {
            p <- p +
                scale_fill_manual(
                    name = fill_name %||% fill_by,
                    values = fill_colors,
                    na.value = fill_colors['NA'] %||% "grey80",
                    guide = guide_legend(order = 1)
                )
        }
    } else {
        p <- p +
            scale_fill_gradientn(
                name = fill_name %||% fill_by,
                n.breaks = 3,
                colors = palette_this(
                    palette = palette,
                    palcolor = palcolor,
                    reverse = palreverse
                ),
                values = if (!is.null(feat_colors_value)) {
                    scales::rescale(feat_colors_value)
                } else {
                    waiver()
                },
                limits = if (!is.null(feat_colors_value)) {
                    range(feat_colors_value)
                } else {
                    NULL
                },
                na.value = "grey80",
                guide = guide_colorbar(
                    frame.colour = "black",
                    ticks.colour = "black",
                    title.hjust = 0
                )
            )
    }

    if (isTRUE(flip)) {
        p <- p +
            geom_text(
                aes(
                    x = 0,
                    y = !!sym(y),
                    # nudge_x is not an aesthetic
                    label = ifelse(
                        is.na(!!sym(y)),
                        " NA ",
                        ifelse(
                            .data[[x]] >= 0,
                            gsub("(\\n|$)", " \\1", !!sym(y)),
                            gsub("(^|\\n)", "\\1 ", !!sym(y))
                        )
                    ),
                    hjust = ifelse(.data[[x]] >= 0, 1, 0),
                ),
                color = "black",
                lineheight = lineheight,
                angle = 90
            ) +
            ggplot2::theme(
                axis.text.x = element_blank(),
                axis.ticks.x = element_blank()
            )
    } else {
        p <- p +
            geom_text(
                aes(
                    x = 0,
                    y = !!sym(y),
                    # nudge_y is not an aesthetic
                    label = ifelse(
                        is.na(!!sym(y)),
                        " NA ",
                        ifelse(
                            .data[[x]] >= 0,
                            gsub("(\\n|$)", " \\1", !!sym(y)),
                            gsub("(^|\\n)", "\\1 ", !!sym(y))
                        )
                    ),
                    hjust = ifelse(.data[[x]] >= 0, 1, 0),
                ),
                color = "black",
                lineheight = lineheight
            ) +
            ggplot2::theme(
                axis.text.y = element_blank(),
                axis.ticks.y = element_blank()
            )
    }

    p <- p +
        labs(
            title = title,
            subtitle = subtitle,
            x = xlab %||% x,
            y = ylab %||% y
        ) +
        # scale_x_continuous(expand = expand$x) +
        scale_y_discrete() +
        do_call(theme, theme_args) +
        ggplot2::theme(
            aspect.ratio = aspect.ratio,
            legend.position = legend.position,
            legend.direction = legend.direction
        )

    dims <- calculate_plot_dimensions(
        base_height = 5.5,
        aspect.ratio = aspect.ratio,
        n_y = nlevels(data[[y]]),
        y_scale_factor = bar_height / 4,
        legend.position = legend.position,
        legend.direction = legend.direction,
        legend_n = if (!fill_by_numeric) length(fill_vals) else 1,
        legend_nchar = if (!fill_by_numeric) {
            max(nchar(as.character(fill_vals)), na.rm = TRUE)
        } else {
            5
        },
        flip = isTRUE(flip)
    )

    if (isTRUE(flip)) {
        p <- p + coord_flip(xlim = c(x_min, x_max))
    } else {
        p <- p + coord_cartesian(xlim = c(x_min, x_max))
    }
    attr(p, "height") <- dims$height
    attr(p, "width") <- dims$width

    facet_plot(
        p,
        facet_by,
        facet_scales,
        facet_nrow,
        facet_ncol,
        facet_byrow,
        legend.position = legend.position,
        legend.direction = legend.direction,
        drop = !isTRUE(keep_empty_facet)
    )
}

#' @rdname barplot
#'
#' @description
#' `SplitBarPlot` (also known as `WaterfallPlot`) draws a divergent bar plot
#' where bars extend left (negative values) and right (positive values) from a
#' central zero line.  The bar fill colour and opacity can encode additional
#' variables, and the vertical ordering of categories is fully customisable.
#'
#' The function supports \strong{split_by} to produce separate panels,
#' \strong{facet_by} for grouped views within panels, and
#' \strong{alpha_by} for encoding a secondary numeric variable via opacity.
#'
#' @section split_by workflow:
#' When \code{split_by} is provided:
#' \enumerate{
#'   \item \code{\link{check_keep_na}()} and \code{\link{check_keep_empty}()}
#'         normalise the \code{keep_na} / \code{keep_empty} arguments.
#'   \item The \code{split_by} column is validated and its NA / empty levels
#'         are processed.  It is then removed from the per-column lists.
#'   \item The data is split by \code{split_by} (preserving level order).
#'   \item Per-split \code{palette}, \code{palcolor},
#'         \code{legend.position}, and \code{legend.direction} are resolved.
#'   \item \code{\link{SplitBarPlotAtomic}()} is called for each split.
#'         When \code{title} is a function, it receives the split level name
#'         for dynamic title generation.
#'   \item Results are combined via \code{\link{combine_plots}()}.
#' }
#'
#' @inheritParams common_args
#' @inheritParams SplitBarPlotAtomic
#' @param split_by The column(s) to split the data by for separate sub-plots.
#' @param split_by_sep Separator for concatenated \code{split_by} columns.
#' @param seed A numeric seed for reproducibility.
#' @param combine Logical; when \code{TRUE} (default), returns a combined
#'  \code{patchwork} object.  When \code{FALSE}, returns a named list of
#'  \code{ggplot} objects.
#' @param ncol,nrow Integer number of columns / rows for the combined layout.
#' @param byrow Logical; fill the combined layout by row (default \code{TRUE}).
#' @param axes,axis_titles Character strings for axis handling in the
#'  combined layout.
#' @param guides Character string for legend collection across panels.
#' @param design A custom layout design for the combined plot.
#' @return A \code{ggplot} object, a \code{patchwork} object, or a named list
#'  of \code{ggplot} objects (when \code{combine = FALSE}), each with
#'  \code{height} and \code{width} attributes in inches.
#' @export
#' @examples
#' \donttest{
#' set.seed(8525)
#' data <- data.frame(
#'     word = c("apple", "banana", "cherry", "date", "elderberry",
#'              "It is a very long term with a lot of words"),
#'     count = c(-10, 20, -30, 40, 50, 34),
#'     score = c(1, 2, 3, 4, 5, 3.2),
#'     group = c("A", "A", "B", "B", "C", "C")
#' )
#'
#' # Basic split bar plot with alpha encoding
#' SplitBarPlot(data, x = "count", y = "word", alpha_by = "score")
#'
#' # Control label wrapping
#' SplitBarPlot(data, x = "count", y = "word", alpha_by = "score",
#'              max_charwidth = 30, lineheight = 1.1)
#'
#' # Fill by categorical variable
#' SplitBarPlot(data, x = "count", y = "word", fill_by = "group")
#'
#' # Faceting
#' SplitBarPlot(data, x = "count", y = "word", facet_by = "group",
#'              fill_name = "Direction")
#'
#' # Per-split palettes
#' SplitBarPlot(data, x = "count", y = "word", alpha_by = "score",
#'              split_by = "group",
#'              palette = c(A = "Reds", B = "Blues", C = "Greens"))
#'
#' # keep_na and keep_empty examples
#' data <- data.frame(
#'     word = factor(c("apple", "banana", "cherry", NA, "elderberry",
#'          "It is a very long term with a lot of words"),
#'          levels = c("apple", "banana", "cherry", "date", "elderberry",
#'              "unused", "It is a very long term with a lot of words")),
#'     count = c(-10, 20, NA, 40, 10, 34),
#'     score = c(1, 2, 3, 4, 5, 3.2),
#'     group = factor(sample(c("A", "A", "B", "B", "C", "C")),
#'          levels = c("A", "B", "C", "D"))
#' )
#'
#' # Default: NA and empty levels dropped
#' SplitBarPlot(data, x = "count", y = "word", alpha_by = "score")
#'
#' # Keep NA and empty levels
#' SplitBarPlot(data, x = "count", y = "word", alpha_by = "score",
#'              keep_na = TRUE, keep_empty = TRUE)
#'
#' # Keep with faceting
#' SplitBarPlot(data, x = "count", y = "word", alpha_by = "score",
#'              keep_na = TRUE, keep_empty = TRUE, facet_by = "group")
#'
#' # Keep NA, hide empty levels (reserve colours)
#' SplitBarPlot(data, x = "count", y = "word", alpha_by = "score",
#'              keep_na = TRUE, keep_empty = "level")
#'
#' # Per-column control
#' SplitBarPlot(data, x = "count", y = "word", alpha_by = "score",
#'              keep_na = list(word = FALSE), keep_empty = list(word = TRUE))
#'
#' # Control fill colour scale limits
#' SplitBarPlot(data, x = "count", y = "word", fill_by = "score",
#'              lower_cutoff = 1, upper_cutoff = 4)
#' }
SplitBarPlot <- function(
    data,
    x,
    y,
    y_sep = "_",
    flip = FALSE,
    split_by = NULL,
    split_by_sep = "_",
    alpha_by = NULL,
    alpha_reverse = FALSE,
    alpha_name = NULL,
    order_y = list(
        "+" = c("x_desc", "alpha_desc"),
        "-" = c("x_desc", "alpha_asc")
    ),
    bar_height = 0.9,
    lineheight = 0.5,
    max_charwidth = 80,
    fill_by = NULL,
    fill_by_sep = "_",
    fill_name = NULL,
    direction_name = "direction",
    direction_pos_name = "positive",
    direction_neg_name = "negative",
    theme = "theme_this",
    theme_args = list(),
    palette = "Spectral",
    palcolor = NULL,
    palreverse = FALSE,
    lower_quantile = 0,
    upper_quantile = 0.99,
    lower_cutoff = NULL,
    upper_cutoff = NULL,
    facet_by = NULL,
    facet_scales = "free_y",
    facet_nrow = NULL,
    facet_ncol = NULL,
    facet_byrow = TRUE,
    aspect.ratio = 1,
    x_min = NULL,
    x_max = NULL,
    legend.position = "right",
    legend.direction = "vertical",
    title = NULL,
    subtitle = NULL,
    xlab = NULL,
    ylab = NULL,
    keep_empty = FALSE,
    keep_na = FALSE,
    combine = TRUE,
    nrow = NULL,
    ncol = NULL,
    byrow = TRUE,
    seed = 8525,
    axes = NULL,
    axis_titles = axes,
    guides = NULL,
    design = NULL,
    ...
) {
    validate_common_args(seed, facet_by = facet_by)
    keep_na <- check_keep_na(keep_na, c(y, split_by, facet_by, fill_by))
    keep_empty <- check_keep_empty(
        keep_empty,
        c(y, split_by, facet_by, fill_by)
    )
    theme <- process_theme(theme)
    split_by <- check_columns(
        data,
        split_by,
        force_factor = TRUE,
        allow_multi = TRUE,
        concat_multi = TRUE,
        concat_sep = split_by_sep
    )

    if (!is.null(split_by)) {
        data <- process_keep_na_empty(data, keep_na, keep_empty, col = split_by)
        keep_na[[split_by]] <- NULL
        keep_empty[[split_by]] <- NULL

        datas <- split(data, data[[split_by]])
        # keep the order of levels
        datas <- datas[levels(data[[split_by]])]
    } else {
        datas <- list(data)
        split_by <- names(datas) <- "..."
    }

    palette <- check_palette(palette, names(datas))
    palcolor <- check_palcolor(palcolor, names(datas))
    legend.direction <- check_legend(
        legend.direction,
        names(datas),
        "legend.direction"
    )
    legend.position <- check_legend(
        legend.position,
        names(datas),
        "legend.position"
    )

    plots <- lapply(
        names(datas),
        function(nm) {
            default_title <- if (length(datas) == 1 && identical(nm, "...")) {
                NULL
            } else {
                nm
            }
            if (is.function(title)) {
                title <- title(default_title)
            } else {
                title <- title %||% default_title
            }
            SplitBarPlotAtomic(
                datas[[nm]],
                x = x,
                y = y,
                y_sep = y_sep,
                flip = flip,
                alpha_by = alpha_by,
                alpha_reverse = alpha_reverse,
                alpha_name = alpha_name,
                order_y = order_y,
                bar_height = bar_height,
                lineheight = lineheight,
                max_charwidth = max_charwidth,
                fill_by = fill_by,
                fill_by_sep = fill_by_sep,
                fill_name = fill_name,
                direction_name = direction_name,
                direction_pos_name = direction_pos_name,
                direction_neg_name = direction_neg_name,
                theme = theme,
                theme_args = theme_args,
                palette = palette[[nm]],
                palcolor = palcolor[[nm]],
                palreverse = palreverse,
                lower_quantile = lower_quantile,
                upper_quantile = upper_quantile,
                lower_cutoff = lower_cutoff,
                upper_cutoff = upper_cutoff,
                facet_by = facet_by,
                facet_scales = facet_scales,
                facet_nrow = facet_nrow,
                facet_ncol = facet_ncol,
                facet_byrow = facet_byrow,
                aspect.ratio = aspect.ratio,
                x_min = x_min,
                x_max = x_max,
                legend.position = legend.position[[nm]],
                legend.direction = legend.direction[[nm]],
                title = title,
                subtitle = subtitle,
                xlab = xlab,
                ylab = ylab,
                keep_empty = keep_empty,
                keep_na = keep_na,
                ...
            )
        }
    )

    names(plots) <- names(datas)

    combine_plots(
        plots,
        combine = combine,
        split_by = split_by,
        nrow = nrow,
        ncol = ncol,
        byrow = byrow,
        axes = axes,
        axis_titles = axis_titles,
        guides = guides,
        design = design
    )
}

#' @rdname barplot
#' @inheritParams SplitBarPlot
#' @export
WaterfallPlot <- SplitBarPlot
