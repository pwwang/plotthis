#' Single-series line plot (internal)
#'
#' @description
#' Core implementation for drawing a single-series line plot.  This is the
#' workhorse behind \code{\link{LinePlotAtomic}} for ungrouped data -- it takes a
#' **single** data frame (no \code{split_by} support) and returns a
#' \code{ggplot} object.  Each x-axis category receives its own point and
#' connecting line, with optional per-x coloring, error bars, highlighted
#' points, and a horizontal reference line.
#'
#' @section Architecture:
#' \enumerate{
#'   \item \strong{ggplot dispatch} -- selects \code{gglogger::ggplot} or
#'         \code{ggplot2::ggplot} based on
#'         \code{getOption("plotthis.gglogger.enabled")}.
#'   \item \strong{Column resolution} -- \code{x} (forced to factor),
#'         \code{y}, and \code{facet_by} are validated via
#'         \code{\link{check_columns}}.
#'   \item \strong{Count aggregation} -- when \code{y = NULL}, the count of
#'         observations per x level (and facet) is computed and used as
#'         \code{y}.
#'   \item \strong{Error bar validation} -- if \code{add_errorbars = TRUE},
#'         checks that either \code{errorbar_min}/\code{errorbar_max} or
#'         \code{errorbar_sd} is provided.
#'   \item \strong{NA / empty-level handling} --
#'         \code{\link{process_keep_na_empty}()} applies \code{keep_na} and
#'         \code{keep_empty} policies.
#'   \item \strong{Highlight parsing} -- \code{highlight} (indices, row
#'         names, expression string, or \code{TRUE}) is resolved to a subset
#'         of data rows.
#'   \item \strong{Background layer} -- when \code{add_bg = TRUE},
#'         alternating background stripes are drawn via
#'         \code{\link{bg_layer}()}.
#'   \item \strong{Horizontal reference line} -- when \code{add_hline} is
#'         numeric, a \code{geom_hline()} is added at that intercept.
#'   \item \strong{Colour assignment} -- \code{\link{palette_this}()}
#'         assigns colours to all x-axis levels (including \code{NA},
#'         defaulting to \code{"grey80"}).
#'   \item \strong{Line rendering} -- when \code{color_line_by_x = TRUE},
#'         lines are coloured per x level via
#'         \code{\link[ggplot2]{geom_line}()} with \code{aes(color = x)} and
#'         \code{scale_color_manual()}.  Otherwise a single colour (the
#'         first palette entry) is used.
#'   \item \strong{Error bars} -- when \code{add_errorbars = TRUE}, three
#'         colour modes are supported: follow the line colour
#'         (\code{"line"}), track x levels, or use a constant colour.
#'         Error bars are added via
#'         \code{\link[ggplot2]{geom_errorbar}()}.
#'   \item \strong{Point rendering} -- when \code{fill_point_by_x = TRUE},
#'         points are filled per x level via
#'         \code{\link[ggplot2]{geom_point}()} with \code{aes(fill = x)} and
#'         \code{scale_fill_manual()}.  Otherwise a single fill colour
#'         (first palette entry) is used.
#'   \item \strong{Highlight overlay} -- an additional \code{geom_point()}
#'         layer draws highlighted rows in the specified colour and size.
#'   \item \strong{Plot assembly} -- \code{scale_x_discrete()},
#'         \code{labs()}, theme application, axis styling (angle, grid
#'         lines), and legend positioning.
#'   \item \strong{Dimension calculation} --
#'         \code{\link{calculate_plot_dimensions}()} computes \code{height}
#'         and \code{width} attributes (in inches) from x-axis category
#'         count, aspect ratio, and legend metrics.
#' }
#'
#' @inheritParams common_args
#' @param x A character string specifying the column name for the x-axis.
#'  Must be character or factor.
#' @param y A character string specifying the numeric column for the y-axis.
#'  When \code{NULL}, the count of observations per x level is used.
#' @param fill_point_by_x A logical value. When TRUE (default), points are
#'  filled by the x-axis categories using the palette. When FALSE, all
#'  points use the first palette colour.
#' @param color_line_by_x A logical value. When TRUE (default), lines are
#'  coloured by the x-axis categories using the palette. When FALSE, all
#'  lines use the first palette colour.
#' @param add_bg A logical value. When TRUE, alternating background stripes
#'  are drawn via \code{\link{bg_layer}()}. Default FALSE.
#' @param bg_palette A character string specifying the palette for the
#'  background stripe colours. Default \code{"stripe"}.
#' @param bg_palcolor A character vector of colours for the background
#'  stripes. When NULL (default), colours are derived from
#'  \code{bg_palette}.
#' @param bg_alpha A numeric value in \code{[0, 1]} for the transparency
#'  of background stripes. Default 0.2.
#' @param add_errorbars A logical value. When TRUE, error bars are added via
#'  \code{\link[ggplot2]{geom_errorbar}()}. Requires \code{errorbar_sd} or
#'  \code{errorbar_min}/\code{errorbar_max}. Default FALSE.
#' @param errorbar_width A numeric value for the width of the error bar
#'  caps. Default 0.1.
#' @param errorbar_alpha A numeric value in \code{[0, 1]} for the transparency of
#'  error bars. Default 1.
#' @param errorbar_color A character string for the colour of the error
#'  bars. When \code{"line"}, error bars are coloured the same as the lines
#'  (by x when \code{color_line_by_x = TRUE}, or single colour otherwise).
#'  Default \code{"grey30"}.
#' @param errorbar_linewidth A numeric value for the line width of error
#'  bars. Default 0.75.
#' @param errorbar_min A character string naming the column with the lower
#'  error bar bound. Ignored when \code{errorbar_sd} is provided.
#' @param errorbar_max A character string naming the column with the upper
#'  error bar bound. Ignored when \code{errorbar_sd} is provided.
#' @param errorbar_sd A character string naming the column with the standard
#'  deviation. When \code{errorbar_min} and \code{errorbar_max} are not
#'  provided, error bars are computed as y +/- \code{errorbar_sd}.
#' @param highlight A vector of row indices, row names, a single string
#'  expression (e.g. \code{"y > 10"}) filtering rows to highlight, or TRUE
#'  to highlight all points. When NULL (default), no highlighting is
#'  applied.
#' @param highlight_size A numeric value for the size of highlighted points.
#'  Defaults to \code{pt_size - 0.75}.
#' @param highlight_color A character string for the colour of highlighted
#'  points. Default \code{"red2"}.
#' @param highlight_alpha A numeric value in \code{[0, 1]} for the transparency of
#'  highlighted points. Default 0.8.
#' @param pt_alpha A numeric value in \code{[0, 1]} for the transparency of points.
#'  Default 1.
#' @param pt_size A numeric value for the point size. Default 5.
#' @param add_hline A numeric value specifying the y-intercept of a
#'  horizontal reference line. When FALSE (default), no line is drawn.
#' @param hline_type A character string specifying the line type of the
#'  horizontal reference line. Default \code{"solid"}.
#' @param hline_width A numeric value for the width of the horizontal
#'  reference line. Default 0.5.
#' @param hline_color A character string for the colour of the horizontal
#'  reference line. Default \code{"black"}.
#' @param hline_alpha A numeric value in \code{[0, 1]} for the transparency of the
#'  horizontal reference line. Default 1.
#' @param line_type A character string specifying the line type. Default
#'  \code{"solid"}.
#' @param line_width A numeric value for the line width (in mm). Default 1.
#' @param line_alpha A numeric value in \code{[0, 1]} for the transparency of the
#'  line. Default 0.8.
#' @return A \code{ggplot} object with \code{height} and \code{width}
#'  attributes (in inches).
#' @keywords internal
#' @importFrom rlang sym parse_expr
#' @importFrom ggplot2 geom_line scale_color_manual labs geom_rect geom_errorbar geom_point
LinePlotSingle <- function(
    data,
    x,
    y = NULL,
    fill_point_by_x = TRUE,
    color_line_by_x = TRUE,
    facet_by = NULL,
    add_bg = FALSE,
    bg_palette = "stripe",
    bg_palcolor = NULL,
    bg_alpha = 0.2,
    add_errorbars = FALSE,
    errorbar_width = 0.1,
    errorbar_alpha = 1,
    errorbar_color = "grey30",
    errorbar_linewidth = .75,
    errorbar_min = NULL,
    errorbar_max = NULL,
    errorbar_sd = NULL,
    highlight = NULL,
    highlight_size = pt_size - 0.75,
    highlight_color = "red2",
    highlight_alpha = 0.8,
    pt_alpha = 1,
    pt_size = 5,
    add_hline = FALSE,
    hline_type = "solid",
    hline_width = 0.5,
    hline_color = "black",
    hline_alpha = 1,
    line_type = "solid",
    line_width = 1,
    line_alpha = .8,
    theme = "theme_this",
    theme_args = list(),
    palette = "Paired",
    palcolor = NULL,
    palreverse = FALSE,
    x_text_angle = 0,
    aspect.ratio = 1,
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
    x <- check_columns(data, x, force_factor = TRUE)
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
            group_by(!!!syms(unique(x, facet_by))) %>%
            summarise(.y = n(), .groups = "drop")
        for (col in unique(c(x, facet_by))) {
            data[[col]] <- factor(
                data[[col]],
                levels = levels(orig_data[[col]])
            )
        }
        rm(orig_data)
        y <- ".y"
    }

    if (isTRUE(add_errorbars)) {
        if (
            is.null(errorbar_sd) &&
                (is.null(errorbar_min) || is.null(errorbar_max))
        ) {
            stop(
                "If 'errorbar_min' and 'errorbar_max' are not provided, 'errorbar_sd' must be provided."
            )
        }
        if (is.null(errorbar_min) || is.null(errorbar_max)) {
            data$errorbar_min <- data[[y]] - data[[errorbar_sd]]
            data$errorbar_max <- data[[y]] + data[[errorbar_sd]]
            errorbar_min <- "errorbar_min"
            errorbar_max <- "errorbar_max"
        }
    }

    data <- process_keep_na_empty(data, keep_na, keep_empty)
    keep_empty_x <- keep_empty[[x]]

    hidata <- NULL
    if (!is.null(highlight)) {
        if (isTRUE(highlight)) {
            hidata <- data
        } else if (is.numeric(highlight)) {
            hidata <- data[highlight, , drop = FALSE]
        } else if (is.character(highlight) && length(highlight) == 1) {
            hidata <- filter(data, !!parse_expr(highlight))
        } else if (is.null(rownames(data))) {
            stop(
                "No row names in the data, please provide a vector of indexes to highlight."
            )
        } else {
            hidata <- data[highlight, , drop = FALSE]
        }
    }

    p <- ggplot(data, aes(x = !!sym(x), y = !!sym(y)))
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

    if (!is.null(add_hline) && !isFALSE(add_hline)) {
        p <- p +
            geom_hline(
                yintercept = add_hline,
                linetype = hline_type,
                linewidth = hline_width,
                color = hline_color,
                alpha = hline_alpha
            )
    }

    x_vals <- levels(data[[x]])
    if (anyNA(data[[x]])) {
        x_vals <- c(x_vals, NA)
    }
    colors <- palette_this(
        x_vals,
        palette = palette,
        palcolor = palcolor,
        NA_keep = TRUE,
        reverse = palreverse
    )

    if (isTRUE(color_line_by_x)) {
        p <- p +
            geom_line(
                aes(color = !!sym(x), group = 1),
                alpha = line_alpha,
                linetype = line_type,
                linewidth = line_width,
                show.legend = TRUE
            )

        if (isTRUE(keep_empty_x)) {
            p <- p +
                scale_color_manual(
                    name = x,
                    values = colors,
                    guide = "legend",
                    breaks = x_vals,
                    limits = x_vals,
                    drop = FALSE,
                    na.value = colors["NA"] %||% "grey80"
                )
        } else {
            p <- p +
                scale_color_manual(
                    name = x,
                    values = colors,
                    guide = "legend",
                    na.value = colors["NA"] %||% "grey80"
                )
        }
    } else {
        p <- p +
            geom_line(
                aes(group = 1),
                color = colors[[1]],
                alpha = line_alpha,
                linetype = line_type,
                linewidth = line_width
            )
    }

    if (isTRUE(add_errorbars)) {
        if (errorbar_color == "line" && isTRUE(color_line_by_x)) {
            p <- p +
                geom_errorbar(
                    aes(
                        ymin = !!sym(errorbar_min),
                        ymax = !!sym(errorbar_max),
                        color = !!sym(x)
                    ),
                    alpha = errorbar_alpha,
                    width = errorbar_width,
                    linewidth = errorbar_linewidth
                )
        } else if (errorbar_color == "line") {
            p <- p +
                geom_errorbar(
                    aes(ymin = !!sym(errorbar_min), ymax = !!sym(errorbar_max)),
                    color = colors[[1]],
                    alpha = errorbar_alpha,
                    width = errorbar_width,
                    linewidth = errorbar_linewidth
                )
        } else {
            p <- p +
                geom_errorbar(
                    aes(ymin = !!sym(errorbar_min), ymax = !!sym(errorbar_max)),
                    color = errorbar_color,
                    alpha = errorbar_alpha,
                    width = errorbar_width,
                    linewidth = errorbar_linewidth
                )
        }
    }

    if (isTRUE(fill_point_by_x)) {
        p <- p +
            geom_point(
                aes(fill = !!sym(x)),
                color = "grey20",
                alpha = pt_alpha,
                size = pt_size,
                shape = 21,
                show.legend = TRUE
            )
        if (isTRUE(keep_empty_x)) {
            p <- p +
                scale_fill_manual(
                    name = x,
                    values = colors,
                    guide = "legend",
                    breaks = x_vals,
                    limits = x_vals,
                    drop = FALSE,
                    na.value = colors["NA"] %||% "grey80"
                )
        } else {
            p <- p +
                scale_fill_manual(
                    name = x,
                    values = colors,
                    guide = "legend",
                    ,
                    na.value = colors["NA"] %||% "grey80"
                )
        }
    } else {
        p <- p +
            geom_point(
                fill = colors[[1]],
                color = "grey20",
                alpha = pt_alpha,
                size = pt_size,
                shape = 21
            )
    }

    if (!is.null(hidata)) {
        p <- p +
            geom_point(
                data = hidata,
                fill = highlight_color,
                color = "transparent",
                size = highlight_size,
                shape = 21
            )
    }

    just <- calc_just(x_text_angle)
    p <- p +
        scale_x_discrete(drop = !isTRUE(keep_empty_x)) +
        labs(
            title = title,
            subtitle = subtitle,
            x = xlab %||% x,
            y = ylab %||% y
        ) +
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

    dims <- calculate_plot_dimensions(
        base_height = 4.5,
        aspect.ratio = aspect.ratio,
        n_x = nlevels(data[[x]]),
        x_scale_factor = 0.8,
        legend.position = legend.position,
        legend.direction = legend.direction,
        legend_n = length(x_vals),
        legend_nchar = max(nchar(as.character(x_vals)), na.rm = TRUE)
    )

    attr(p, "height") <- dims$height
    attr(p, "width") <- dims$width

    p
}

#' Multi-series line plot (internal)
#'
#' @description
#' Core implementation for drawing a multi-series line plot.  This is the
#' workhorse behind \code{\link{LinePlotAtomic}} for grouped data -- it takes a
#' **single** data frame with a \code{group_by} column and returns a
#' \code{ggplot} object.  Each group receives its own line rendered in a
#' distinct colour from the palette, with optional error bars, highlighted
#' points, per-group horizontal reference lines, and background stripes.
#'
#' @section Architecture:
#' \enumerate{
#'   \item \strong{ggplot dispatch} -- selects \code{gglogger::ggplot} or
#'         \code{ggplot2::ggplot} based on
#'         \code{getOption("plotthis.gglogger.enabled")}.
#'   \item \strong{Column resolution} -- \code{x} (forced to factor),
#'         \code{y}, \code{group_by} (multi-column concatenation supported),
#'         and \code{facet_by} are validated via
#'         \code{\link{check_columns}}.
#'   \item \strong{Count aggregation} -- when \code{y = NULL}, the count of
#'         observations per (\code{x}, \code{group_by}, \code{facet_by})
#'         combination is computed and used as \code{y}.
#'   \item \strong{Error bar validation} -- if \code{add_errorbars = TRUE},
#'         checks that either \code{errorbar_min}/\code{errorbar_max} or
#'         \code{errorbar_sd} is provided.
#'   \item \strong{NA / empty-level handling} --
#'         \code{\link{process_keep_na_empty}()} applies \code{keep_na} and
#'         \code{keep_empty} policies.
#'   \item \strong{Highlight parsing} -- \code{highlight} (indices, row
#'         names, expression string, or \code{TRUE}) is resolved to a subset
#'         of data rows.
#'   \item \strong{Background layer} -- when \code{add_bg = TRUE},
#'         alternating background stripes are drawn via
#'         \code{\link{bg_layer}()}.
#'   \item \strong{Colour assignment} -- \code{\link{palette_this}()}
#'         assigns colours to all \code{group_by} levels (including
#'         \code{NA}, defaulting to \code{"grey80"}).
#'   \item \strong{Horizontal reference line} -- when \code{add_hline} is
#'         set, one or more \code{geom_hline()} are added.  If
#'         \code{hline_color = TRUE}, each horizontal line is coloured to
#'         match the corresponding group colour, and \code{add_hline} can
#'         be a named vector/ list mapping groups to intercept values.
#'   \item \strong{Line rendering} -- lines are coloured and grouped by
#'         \code{group_by} via \code{\link[ggplot2]{geom_line}()} with
#'         \code{aes(color = group_by, group = group_by)} and
#'         \code{scale_color_manual()}.
#'   \item \strong{Error bars} -- when \code{add_errorbars = TRUE}, three
#'         colour modes are supported: follow the line colour
#'         (\code{"line"}), track \code{group_by} colours, or use a
#'         constant colour.  Error bars are added via
#'         \code{\link[ggplot2]{geom_errorbar}()}.
#'   \item \strong{Point rendering} -- points are filled per group via
#'         \code{\link[ggplot2]{geom_point}()} with
#'         \code{aes(fill = group_by)} and \code{scale_fill_manual()}.
#'   \item \strong{Highlight overlay} -- an additional \code{geom_point()}
#'         layer draws highlighted rows in the specified colour and size.
#'   \item \strong{Plot assembly} -- \code{scale_x_discrete()},
#'         \code{labs()}, theme application, axis styling (angle, grid
#'         lines), and legend positioning.
#'   \item \strong{Dimension calculation} --
#'         \code{\link{calculate_plot_dimensions}()} computes \code{height}
#'         and \code{width} attributes (in inches) from x-axis category
#'         count, aspect ratio, and legend metrics (legend entries reflect
#'         group levels rather than x levels).
#' }
#'
#' @inheritParams common_args
#' @inheritParams LinePlotSingle
#' @param group_by A character vector of column names to group the data by.
#'  Each unique combination becomes a separate line. Multiple columns are
#'  concatenated with \code{group_by_sep}. When \code{NULL}, the
#'  \code{LinePlotSingle} path is used instead.
#' @param group_by_sep A character string used to join multiple
#'  \code{group_by} column values into a single group identifier. Default
#'  \code{"_"}.
#' @return A \code{ggplot} object with \code{height} and \code{width}
#'  attributes (in inches).
#' @keywords internal
#' @importFrom rlang syms
#' @importFrom dplyr summarise %>% mutate n
#' @importFrom ggplot2 geom_line scale_color_manual labs geom_errorbar geom_point
LinePlotGrouped <- function(
    data,
    x,
    y = NULL,
    group_by,
    group_by_sep = "_",
    facet_by = NULL,
    add_bg = FALSE,
    bg_palette = "stripe",
    bg_palcolor = NULL,
    bg_alpha = 0.2,
    add_errorbars = FALSE,
    errorbar_width = 0.1,
    errorbar_alpha = 1,
    errorbar_color = "grey30",
    errorbar_linewidth = .75,
    errorbar_min = NULL,
    errorbar_max = NULL,
    errorbar_sd = NULL,
    highlight = NULL,
    highlight_size = pt_size - 0.75,
    highlight_color = "red2",
    highlight_alpha = 0.8,
    pt_alpha = 1,
    pt_size = 5,
    add_hline = FALSE,
    hline_type = "solid",
    hline_width = 0.5,
    hline_color = "black",
    hline_alpha = 1,
    line_type = "solid",
    line_width = 1,
    line_alpha = .8,
    theme = "theme_this",
    theme_args = list(),
    palette = "Paired",
    palcolor = NULL,
    palreverse = FALSE,
    x_text_angle = 0,
    aspect.ratio = 1,
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
    x <- check_columns(data, x, force_factor = TRUE)
    y <- check_columns(data, y)
    group_by <- check_columns(
        data,
        group_by,
        force_factor = TRUE,
        allow_multi = TRUE,
        concat_multi = TRUE,
        concat_sep = group_by_sep
    )
    if (is.null(y)) {
        orig_data <- data
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
        rm(orig_data)
        y <- ".y"
    }

    if (isTRUE(add_errorbars)) {
        if (
            is.null(errorbar_sd) &&
                (is.null(errorbar_min) || is.null(errorbar_max))
        ) {
            stop(
                "If 'errorbar_min' and 'errorbar_max' are not provided, 'errorbar_sd' must be provided."
            )
        }
        if (is.null(errorbar_min) || is.null(errorbar_max)) {
            data$errorbar_min <- data[[y]] - data[[errorbar_sd]]
            data$errorbar_max <- data[[y]] + data[[errorbar_sd]]
            errorbar_min <- "errorbar_min"
            errorbar_max <- "errorbar_max"
        }
    }

    data <- process_keep_na_empty(data, keep_na, keep_empty)

    keep_empty_x <- keep_empty[[x]]
    keep_empty_group <- if (!is.null(group_by)) keep_empty[[group_by]] else NULL

    hidata <- NULL
    if (!is.null(highlight)) {
        if (isTRUE(highlight)) {
            hidata <- data
        } else if (is.numeric(highlight)) {
            hidata <- data[highlight, , drop = FALSE]
        } else if (is.character(highlight) && length(highlight) == 1) {
            hidata <- filter(data, !!parse_expr(highlight))
        } else if (is.null(rownames(data))) {
            stop(
                "No row names in the data, please provide a vector of indexes to highlight."
            )
        } else {
            hidata <- data[highlight, , drop = FALSE]
        }
    }

    p <- ggplot(data, aes(x = !!sym(x), y = !!sym(y)))
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
    group_vals <- levels(data[[group_by]])
    if (anyNA(data[[group_by]])) {
        group_vals <- c(group_vals, NA)
    }

    colors <- palette_this(
        group_vals,
        palette = palette,
        palcolor = palcolor,
        NA_keep = TRUE,
        reverse = palreverse
    )

    if (!is.null(add_hline) && !isFALSE(add_hline)) {
        if (isTRUE(hline_color)) {
            if (!is.list(add_hline)) {
                add_hline <- stats::setNames(
                    as.list(add_hline),
                    levels(data[[group_by]])[1:length(add_hline)]
                )
            }
            add_hline <- add_hline[intersect(
                levels(data[[group_by]]),
                names(add_hline)
            )]
            hline_color <- colors[names(add_hline)]

            add_hline <- unlist(add_hline, use.names = FALSE)
        }
        p <- p +
            geom_hline(
                yintercept = add_hline,
                linetype = hline_type,
                linewidth = hline_width,
                color = hline_color,
                alpha = hline_alpha
            )
    }

    p <- p +
        geom_line(
            aes(color = !!sym(group_by), group = !!sym(group_by)),
            alpha = line_alpha,
            linetype = line_type,
            linewidth = line_width,
            show.legend = TRUE
        )

    if (isTRUE(keep_empty_group)) {
        p <- p +
            scale_color_manual(
                name = group_by,
                values = colors,
                guide = "legend",
                na.value = colors["NA"] %||% "grey80",
                breaks = group_vals,
                limits = group_vals,
                drop = FALSE
            )
    } else {
        p <- p +
            scale_color_manual(
                name = group_by,
                values = colors,
                guide = "legend",
                na.value = colors["NA"] %||% "grey80"
            )
    }

    if (isTRUE(add_errorbars)) {
        if (errorbar_color == "line") {
            p <- p +
                geom_errorbar(
                    aes(
                        ymin = !!sym(errorbar_min),
                        ymax = !!sym(errorbar_max),
                        color = !!sym(group_by)
                    ),
                    alpha = errorbar_alpha,
                    width = errorbar_width,
                    linewidth = errorbar_linewidth
                )
        } else {
            p <- p +
                geom_errorbar(
                    aes(ymin = !!sym(errorbar_min), ymax = !!sym(errorbar_max)),
                    color = errorbar_color,
                    alpha = errorbar_alpha,
                    width = errorbar_width,
                    linewidth = errorbar_linewidth
                )
        }
    }

    p <- p +
        geom_point(
            aes(fill = !!sym(group_by)),
            color = "grey20",
            alpha = pt_alpha,
            size = pt_size,
            shape = 21,
            show.legend = TRUE
        )
    if (isTRUE(keep_empty_group)) {
        p <- p +
            scale_fill_manual(
                name = group_by,
                values = colors,
                guide = "legend",
                breaks = group_vals,
                limits = group_vals,
                drop = FALSE,
                na.value = colors["NA"] %||% "grey80"
            )
    } else {
        p <- p +
            scale_fill_manual(
                name = group_by,
                values = colors,
                guide = "legend",
                na.value = colors["NA"] %||% "grey80"
            )
    }

    if (!is.null(hidata)) {
        p <- p +
            geom_point(
                data = hidata,
                fill = highlight_color,
                color = "transparent",
                size = highlight_size,
                shape = 21
            )
    }

    just <- calc_just(x_text_angle)
    p <- p +
        scale_x_discrete(drop = !isTRUE(keep_empty_x)) +
        labs(
            title = title,
            subtitle = subtitle,
            x = xlab %||% x,
            y = ylab %||% y
        ) +
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

    dims <- calculate_plot_dimensions(
        base_height = 4.5,
        aspect.ratio = aspect.ratio,
        n_x = nlevels(data[[x]]),
        x_scale_factor = 0.8,
        legend.position = legend.position,
        legend.direction = legend.direction,
        legend_n = length(group_vals),
        legend_nchar = max(nchar(as.character(group_vals)), na.rm = TRUE)
    )

    attr(p, "height") <- dims$height
    attr(p, "width") <- dims$width

    p
}

#' Atomic line-plot dispatcher (internal)
#'
#' @description
#' Dispatcher that routes to \code{\link{LinePlotSingle}} or
#' \code{\link{LinePlotGrouped}} depending on whether \code{group_by} is
#' provided.  Handles faceting via \code{\link{facet_plot}()} after the base
#' plot is built.
#'
#' @section Dispatch Logic:
#' \enumerate{
#'   \item \strong{Column resolution} -- \code{facet_by} is validated via
#'         \code{\link{check_columns}}.
#'   \item \strong{Routing} -- when \code{group_by = NULL}, delegates to
#'         \code{\link{LinePlotSingle}} (ungrouped, single-series line plot).
#'         When \code{group_by} is provided, delegates to
#'         \code{\link{LinePlotGrouped}} (multi-series line plot).
#'   \item \strong{Facet keep_empty consistency} -- when \code{facet_by}
#'         contains multiple columns, their \code{keep_empty} values must be
#'         identical.
#'   \item \strong{Faceting} -- \code{\link{facet_plot}()} applies
#'         \code{facet_wrap} / \code{facet_grid} with the resolved
#'         \code{drop} argument (derived from \code{keep_empty}).
#' }
#'
#' @inheritParams common_args
#' @inheritParams LinePlotGrouped
#' @param x A character string specifying the column name for the x-axis.
#'  Must be character or factor.
#' @param y A character string specifying the numeric column for the y-axis.
#' @param group_by A character vector of column names to group the data by.
#'  When NULL, a single-series line plot is drawn via
#'  \code{\link{LinePlotSingle}}. When provided, a multi-series line plot
#'  is drawn via \code{\link{LinePlotGrouped}}.
#' @param fill_point_by_x_if_no_group A logical value. When TRUE (default),
#'  points are filled by the x-axis categories via the palette when
#'  \code{group_by = NULL}. Passed to \code{LinePlotSingle} as
#'  \code{fill_point_by_x}. Has no effect when \code{group_by} is set.
#' @param color_line_by_x_if_no_group A logical value. When TRUE (default),
#'  lines are coloured by the x-axis categories via the palette when
#'  \code{group_by = NULL}. Passed to \code{LinePlotSingle} as
#'  \code{color_line_by_x}. Has no effect when \code{group_by} is set.
#' @param facet_args A list of additional arguments passed to
#'  \code{\link{facet_plot}()} for fine-grained control over faceting
#'  (e.g. \code{scales}, \code{space}, \code{labeller}).
#' @return A \code{ggplot} object with \code{height} and \code{width}
#'  attributes (in inches).
#' @keywords internal
LinePlotAtomic <- function(
    data,
    x,
    y = NULL,
    group_by = NULL,
    fill_point_by_x_if_no_group = TRUE,
    color_line_by_x_if_no_group = TRUE,
    add_bg = FALSE,
    bg_palette = "stripe",
    bg_palcolor = NULL,
    bg_alpha = 0.2,
    add_errorbars = FALSE,
    errorbar_width = 0.1,
    errorbar_alpha = 1,
    errorbar_color = "grey30",
    errorbar_linewidth = .75,
    errorbar_min = NULL,
    errorbar_max = NULL,
    errorbar_sd = NULL,
    highlight = NULL,
    highlight_size = pt_size - 0.75,
    highlight_color = "red2",
    highlight_alpha = 0.8,
    pt_alpha = 1,
    pt_size = 5,
    add_hline = FALSE,
    hline_type = "solid",
    hline_width = 0.5,
    hline_color = "black",
    hline_alpha = 1,
    line_type = "solid",
    line_width = 1,
    line_alpha = .8,
    theme = "theme_this",
    theme_args = list(),
    palette = "Paired",
    palcolor = NULL,
    palreverse = FALSE,
    x_text_angle = 0,
    aspect.ratio = 1,
    legend.position = "right",
    legend.direction = "vertical",
    title = NULL,
    subtitle = NULL,
    xlab = NULL,
    ylab = NULL,
    keep_empty = FALSE,
    keep_na = FALSE,
    facet_by = NULL,
    facet_scales = "fixed",
    facet_args = list(),
    facet_nrow = NULL,
    facet_ncol = NULL,
    facet_byrow = TRUE,
    ...
) {
    facet_by <- check_columns(
        data,
        facet_by,
        force_factor = TRUE,
        allow_multi = TRUE
    )
    if (is.null(group_by)) {
        p <- LinePlotSingle(
            data = data,
            x = x,
            y = y,
            fill_point_by_x = fill_point_by_x_if_no_group,
            color_line_by_x = color_line_by_x_if_no_group,
            facet_by = facet_by,
            add_bg = add_bg,
            bg_palette = bg_palette,
            bg_palcolor = bg_palcolor,
            bg_alpha = bg_alpha,
            add_errorbars = add_errorbars,
            errorbar_width = errorbar_width,
            errorbar_alpha = errorbar_alpha,
            errorbar_color = errorbar_color,
            errorbar_linewidth = errorbar_linewidth,
            errorbar_min = errorbar_min,
            errorbar_max = errorbar_max,
            errorbar_sd = errorbar_sd,
            highlight = highlight,
            highlight_size = highlight_size,
            highlight_color = highlight_color,
            highlight_alpha = highlight_alpha,
            pt_alpha = pt_alpha,
            pt_size = pt_size,
            add_hline = add_hline,
            hline_type = hline_type,
            hline_width = hline_width,
            hline_color = hline_color,
            hline_alpha = hline_alpha,
            line_type = line_type,
            line_width = line_width,
            line_alpha = line_alpha,
            theme = theme,
            theme_args = theme_args,
            palette = palette,
            palcolor = palcolor,
            palreverse = palreverse,
            x_text_angle = x_text_angle,
            aspect.ratio = aspect.ratio,
            legend.position = legend.position,
            legend.direction = legend.direction,
            title = title,
            subtitle = subtitle,
            xlab = xlab,
            ylab = ylab,
            keep_empty = keep_empty,
            keep_na = keep_na,
            ...
        )
    } else {
        p <- LinePlotGrouped(
            data = data,
            x = x,
            y = y,
            group_by = group_by,
            facet_by = facet_by,
            add_bg = add_bg,
            bg_palette = bg_palette,
            bg_palcolor = bg_palcolor,
            bg_alpha = bg_alpha,
            add_errorbars = add_errorbars,
            errorbar_width = errorbar_width,
            errorbar_alpha = errorbar_alpha,
            errorbar_color = errorbar_color,
            errorbar_linewidth = errorbar_linewidth,
            errorbar_min = errorbar_min,
            errorbar_max = errorbar_max,
            errorbar_sd = errorbar_sd,
            highlight = highlight,
            highlight_size = highlight_size,
            highlight_color = highlight_color,
            highlight_alpha = highlight_alpha,
            pt_alpha = pt_alpha,
            pt_size = pt_size,
            add_hline = add_hline,
            hline_type = hline_type,
            hline_width = hline_width,
            hline_color = hline_color,
            hline_alpha = hline_alpha,
            line_type = line_type,
            line_width = line_width,
            line_alpha = line_alpha,
            theme = theme,
            theme_args = theme_args,
            palette = palette,
            palcolor = palcolor,
            palreverse = palreverse,
            x_text_angle = x_text_angle,
            aspect.ratio = aspect.ratio,
            legend.position = legend.position,
            legend.direction = legend.direction,
            title = title,
            subtitle = subtitle,
            xlab = xlab,
            ylab = ylab,
            keep_empty = keep_empty,
            keep_na = keep_na,
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
            "[LinePlot] `keep_empty` for `facet_by` variables must be identical." = identical(
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

#' Line plot
#'
#' @description
#' Draws a line plot showing the change of a numeric value across the
#' progression of a categorical x-axis variable.  Each x-axis category is
#' rendered as a point connected by a line, with support for multiple
#' grouped series, error bars, highlighted points, background stripes,
#' and a horizontal reference line.
#'
#' Key features:
#' \itemize{
#'   \item \strong{Colour modes:} lines and points can be coloured by x
#'   category (single-series) or by a \code{group_by} variable
#'   (multi-series), or use a single uniform colour.
#'   \item \strong{Error bars:} additive error bars via
#'   \code{errorbar_sd}, \code{errorbar_min}, or \code{errorbar_max}.
#'   \item \strong{Highlighting:} specific points can be emphasised with a
#'   different colour and size via indices, row names, or a filter
#'   expression.
#'   \item \strong{Background stripes:} \code{add_bg = TRUE} draws
#'   alternating bands for visual grouping.
#'   \item \strong{Count aggregation:} omit \code{y} to plot observation
#'   counts per x category.
#' }
#'
#' @section split_by Workflow:
#' When \code{split_by} is provided:
#' \enumerate{
#'   \item \strong{Column validation} -- \code{\link{check_columns}()}
#'         resolves \code{split_by} with multi-column concatenation.
#'   \item \strong{NA / empty pre-processing} --
#'         \code{\link{process_keep_na_empty}()} handles \code{keep_na} /
#'         \code{keep_empty} for the split column before splitting, then
#'         removes the split column from the per-split lists.
#'   \item \strong{Data splitting} -- splits \code{data} by
#'         \code{split_by} levels, preserving factor level order.  When
#'         \code{split_by = NULL}, the data is wrapped in a single-element
#'         list with name \code{"..."}.
#'   \item \strong{Per-split palette / colour} --
#'         \code{\link{check_palette}()} and \code{\link{check_palcolor}()}
#'         resolve per-split palette and colour overrides.
#'   \item \strong{Per-split legend} -- \code{\link{check_legend}()}
#'         resolves \code{legend.position} and \code{legend.direction} per
#'         split level.
#'   \item \strong{Per-split title} -- when \code{title} is a function, it
#'         receives the default title (the split level name) and can return
#'         a custom string; otherwise \code{title \%||\% split_level} is
#'         used.
#'   \item \strong{Dispatch} -- each split subset is passed to
#'         \code{\link{LinePlotAtomic}}.
#'   \item \strong{Combination} -- \code{\link{combine_plots}()} assembles
#'         the list of plots via \code{patchwork::wrap_plots}, honouring
#'         \code{nrow}/\code{ncol}/\code{byrow}/\code{design}.
#' }
#'
#' @inheritParams common_args
#' @inheritParams LinePlotAtomic
#' @param split_by A character vector of column names to split the data by.
#'  Each split level produces a separate sub-plot. Multiple columns are
#'  concatenated with \code{split_by_sep}.
#' @param split_by_sep A character string used to join multiple
#'  \code{split_by} column values. Default \code{"_"}.
#' @param seed A numeric seed for reproducibility. Passed to
#'  \code{\link{validate_common_args}()}. Default 8525.
#' @param combine Logical; when TRUE (default), per-split plots are combined
#'  into a single \code{patchwork} object. When FALSE, a named list of
#'  \code{ggplot} objects is returned.
#' @param nrow,ncol Integer number of rows / columns for the combined
#'  layout (passed to \code{\link[patchwork]{wrap_plots}}).
#' @param byrow Logical; fill the combined layout by row. Default TRUE
#'  (passed to \code{\link[patchwork]{wrap_plots}}).
#' @param axes A character string specifying how axes should be treated
#'  across the combined layout (passed to
#'  \code{\link[patchwork]{wrap_plots}}).
#' @param axis_titles A character string specifying how axis titles should
#'  be treated across the combined layout. Defaults to \code{axes}.
#' @param guides A character string specifying how guides should be
#'  collected across panels. Passed to \code{\link{combine_plots}()}.
#' @param design A custom layout specification for combined plots (passed
#'  to \code{\link{combine_plots}()}). Overrides \code{nrow}/\code{ncol}
#'  when specified.
#' @return A \code{ggplot} object, a \code{patchwork} object (when
#'  \code{combine = TRUE} with \code{split_by}), or a named list of
#'  \code{ggplot} objects (when \code{combine = FALSE}), each with
#'  \code{height} and \code{width} attributes in inches.
#' @export
#' @examples
#' \donttest{
#' data <- data.frame(
#'    x = factor(c("A", "B", "C", "D", "A", "B", "C", "D"), levels = LETTERS[1:6]),
#'    y = c(10, 8, 16, 4, 6, 12, 14, 2),
#'    group = c("G1", "G1", "G1", "G1", "G2", "G2", "G2", "G2"),
#'    facet = c("F1", "F1", "F2", "F2", "F3", "F3", "F4", "F4")
#' )
#'
#' # --- Basic usage ---
#' LinePlot(data, x = "x", y = "y")
#' LinePlot(data, x = "x", y = "y", highlight = "group == 'G1'",
#'    fill_point_by_x_if_no_group = FALSE, color_line_by_x_if_no_group = FALSE)
#'
#' # --- Grouped lines ---
#' LinePlot(data, x = "x", y = "y", group_by = "group")
#' LinePlot(data, x = "x", y = "y", group_by = "group",
#'    add_hline = 10, hline_color = "red")
#' LinePlot(data, x = "x", y = "y", group_by = "group", add_bg = TRUE,
#'    highlight = "y > 10")
#' LinePlot(data, x = "x", y = "y", group_by = "group", facet_by = "facet")
#' LinePlot(data, x = "x", y = "y", group_by = "group", split_by = "facet")
#'
#' # --- Per-split styling ---
#' LinePlot(data, x = "x", y = "y", split_by = "group",
#'          palcolor = list(G1 = c("red", "blue"), G2 = c("green", "black")))
#'
#' # --- keep_na and keep_empty ---
#' data <- data.frame(
#'    x = factor(c("A", "B", NA, "D", "A", "B", NA, "D"), levels = LETTERS[1:4]),
#'    y = c(10, 8, 16, 4, 6, 12, 14, 2),
#'    group = factor(c("G1", "G1", "G1", NA, NA, "G3", "G3", "G3"),
#'      levels = c("G1", "G2", "G3")),
#'    facet = c("F1", "F1", "F2", "F2", "F3", "F3", "F4", "F4")
#' )
#'
#' LinePlot(data, x = "x", y = "y", keep_na = TRUE)
#' LinePlot(data, x = "x", y = "y", keep_empty = TRUE)
#' LinePlot(data, x = "x", y = "y", keep_empty = 'level')
#' LinePlot(data, x = "x", y = "y", group_by = "group", keep_na = TRUE)
#' LinePlot(data, x = "x", y = "y", group_by = "group", keep_empty = TRUE)
#' LinePlot(data, x = "x", y = "y", group_by = "group",
#'    keep_empty = list(x = TRUE, group = 'level'))
#' }
LinePlot <- function(
    data,
    x,
    y = NULL,
    group_by = NULL,
    group_by_sep = "_",
    split_by = NULL,
    split_by_sep = "_",
    fill_point_by_x_if_no_group = TRUE,
    color_line_by_x_if_no_group = TRUE,
    add_bg = FALSE,
    bg_palette = "stripe",
    bg_palcolor = NULL,
    bg_alpha = 0.2,
    add_errorbars = FALSE,
    errorbar_width = 0.1,
    errorbar_alpha = 1,
    errorbar_color = "grey30",
    errorbar_linewidth = .75,
    errorbar_min = NULL,
    errorbar_max = NULL,
    errorbar_sd = NULL,
    highlight = NULL,
    highlight_size = pt_size - 0.75,
    highlight_color = "red2",
    highlight_alpha = 0.8,
    pt_alpha = 1,
    pt_size = 5,
    keep_na = FALSE,
    keep_empty = FALSE,
    line_type = "solid",
    line_width = 1,
    line_alpha = .8,
    add_hline = FALSE,
    hline_type = "solid",
    hline_width = 0.5,
    hline_color = "black",
    hline_alpha = 1,
    theme = "theme_this",
    theme_args = list(),
    palette = "Paired",
    palcolor = NULL,
    palreverse = FALSE,
    x_text_angle = 0,
    aspect.ratio = 1,
    legend.position = "right",
    legend.direction = "vertical",
    facet_by = NULL,
    facet_scales = "fixed",
    combine = TRUE,
    nrow = NULL,
    ncol = NULL,
    byrow = TRUE,
    facet_nrow = NULL,
    facet_ncol = NULL,
    facet_byrow = TRUE,
    facet_args = list(),
    title = NULL,
    subtitle = NULL,
    xlab = NULL,
    ylab = NULL,
    seed = 8525,
    axes = NULL,
    axis_titles = axes,
    guides = NULL,
    design = NULL,
    ...
) {
    validate_common_args(seed, facet_by = facet_by)
    keep_na <- check_keep_na(keep_na, c(x, group_by, split_by, facet_by))
    keep_empty <- check_keep_empty(
        keep_empty,
        c(x, group_by, split_by, facet_by)
    )
    theme <- process_theme(theme)

    x <- check_columns(data, x, force_factor = TRUE)
    y <- check_columns(data, y)
    group_by <- check_columns(
        data,
        group_by,
        force_factor = TRUE,
        allow_multi = TRUE,
        concat_multi = TRUE,
        concat_sep = group_by_sep
    )
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
            LinePlotAtomic(
                datas[[nm]],
                x = x,
                y = y,
                group_by = group_by,
                fill_point_by_x_if_no_group = fill_point_by_x_if_no_group,
                color_line_by_x_if_no_group = color_line_by_x_if_no_group,
                add_bg = add_bg,
                bg_palette = bg_palette,
                bg_palcolor = bg_palcolor,
                bg_alpha = bg_alpha,
                add_errorbars = add_errorbars,
                errorbar_width = errorbar_width,
                errorbar_alpha = errorbar_alpha,
                errorbar_color = errorbar_color,
                errorbar_linewidth = errorbar_linewidth,
                errorbar_min = errorbar_min,
                errorbar_max = errorbar_max,
                errorbar_sd = errorbar_sd,
                highlight = highlight,
                highlight_size = highlight_size,
                highlight_color = highlight_color,
                highlight_alpha = highlight_alpha,
                pt_alpha = pt_alpha,
                pt_size = pt_size,
                line_type = line_type,
                line_width = line_width,
                line_alpha = line_alpha,
                add_hline = add_hline,
                hline_type = hline_type,
                hline_width = hline_width,
                hline_color = hline_color,
                hline_alpha = hline_alpha,
                theme = theme,
                theme_args = theme_args,
                palette = palette[[nm]],
                palcolor = palcolor[[nm]],
                palreverse = palreverse,
                x_text_angle = x_text_angle,
                aspect.ratio = aspect.ratio,
                legend.position = legend.position[[nm]],
                legend.direction = legend.direction[[nm]],
                facet_args = facet_args,
                facet_by = facet_by,
                facet_scales = facet_scales,
                facet_nrow = facet_nrow,
                facet_ncol = facet_ncol,
                facet_byrow = facet_byrow,
                title = title,
                subtitle = subtitle,
                xlab = xlab,
                ylab = ylab,
                keep_na = keep_na,
                keep_empty = keep_empty,
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
