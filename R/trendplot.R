#' Atomic trend plot (internal)
#'
#' @description
#' Core implementation for drawing a single trend plot. This is the workhorse
#' behind the exported \code{\link{TrendPlot}} function -- it takes a **single**
#' data frame (no \code{split_by} support) and returns a \code{ggplot} object.
#' A trend plot combines stacked bars (\code{\link[ggplot2]{geom_col}()}) with
#' a semi-transparent area background (\code{\link[ggplot2]{geom_area}()}) to
#' show how one or more groups accumulate across a discrete x-axis. This hybrid
#' style sits between a bar plot and an area plot, preserving the discrete
#' category separation of bars while softening the visual with an area fill.
#'
#' The function supports count aggregation (omit \code{y} to plot observation
#' counts per x-category), proportion scaling (\code{scale_y = TRUE} normalises
#' each x position to 100\%), per-group colour control, and faceting.
#'
#' @section Architecture:
#' \enumerate{
#'   \item \strong{Column resolution} -- \code{x}, \code{y}, \code{group_by},
#'         and \code{facet_by} are validated and transformed via
#'         \code{\link{check_columns}}. Multi-column inputs for \code{x} and
#'         \code{group_by} are concatenated into single columns using their
#'         respective separators (\code{x_sep}, \code{group_by_sep}).
#'   \item \strong{Count aggregation} -- when \code{y = NULL}, the count of
#'         observations in each unique (\code{x}, \code{group_by},
#'         \code{facet_by}) combination is computed as a new \code{.count}
#'         column. Factor levels are preserved after aggregation.
#'   \item \strong{Proportion scaling} -- when \code{scale_y = TRUE}, the
#'         y-values are divided by the sum within each (\code{x},
#'         \code{facet_by}) group, producing a proportion (0--1). Percent
#'         labels are used automatically on the y-axis.
#'   \item \strong{NA / empty-level handling} --
#'         \code{\link{process_keep_na_empty}()} applies \code{keep_na} and
#'         \code{keep_empty} policies. Per-column \code{keep_empty} settings
#'         are extracted for \code{x}, \code{group_by}, and \code{facet_by}
#'         independently. The facet columns must have identical
#'         \code{keep_empty} values. Note that \code{keep_empty = TRUE} is
#'         not supported globally because empty categories would break the
#'         continuity of the x-axis.
#'   \item \strong{Group fill setup} -- when \code{group_by = NULL}, a dummy
#'         \code{.fill} factor is created so the single group still draws with
#'         the first palette colour. The legend is suppressed
#'         (\code{legend.position = "none"}).
#'   \item \strong{Data completion} -- \code{\link[tidyr]{complete}()} pads
#'         all \code{x} by \code{group_by} (by \code{facet_by})
#'         combinations with \code{y = 0}. This prevents
#'         \code{\link[ggplot2]{geom_area}()} from interpolating across missing
#'         groups, which would otherwise cause stacked areas to exceed the
#'         correct total.
#'   \item \strong{Area layer construction} -- the completed data is expanded
#'         by duplicating each row and offsetting the x-axis positions by
#'         \eqn{\pm 0.2} on a numeric scale, creating discrete-width area
#'         strips that align perfectly with the bars.
#'   \item \strong{Colour mapping} -- \code{\link{palette_this}()} assigns
#'         colours to all \code{group_by} levels, including \code{NA}
#'         (defaulting to \code{"grey80"}).
#'   \item \strong{Plot assembly} -- the \code{ggplot} object is built with
#'         \code{geom_area()} (at half \code{alpha}, grey outline) as a
#'         background fill, overlaid with \code{geom_col()} (full \code{alpha},
#'         black outline, width 0.4) as the foreground bars. Both layers use
#'         \code{position_stack(vjust = 0.5)}. The x-axis uses
#'         \code{scale_x_discrete()} and the y-axis uses
#'         \code{scale_y_continuous()} (percent labels when scaled). The
#'         fill scale \code{drop} argument is controlled by
#'         \code{keep_empty_group}.
#'   \item \strong{Dimension calculation} --
#'         \code{\link{calculate_plot_dimensions}()} computes plot height and
#'         width from the x-axis category count, \code{aspect.ratio}, and
#'         legend metrics. The base height increases for plots with 10 or more
#'         x categories (from 4.5 to 6.5 inches). The resulting \code{height}
#'         and \code{width} attributes are stored on the \code{ggplot} object.
#'   \item \strong{Faceting} -- \code{\link{facet_plot}()} wraps the plot with
#'         \code{facet_wrap} / \code{facet_grid} if \code{facet_by} is
#'         provided, respecting the \code{keep_empty} setting for facet
#'         variables.
#' }
#'
#' @inheritParams common_args
#' @param x A character string specifying the column name to plot on the
#'  x-axis. Must be character or factor. Multiple columns can be provided;
#'  they are concatenated with \code{x_sep} as the separator.
#' @param x_sep A character string used to join multiple \code{x} columns.
#'  Default \code{"_"}. Ignored when \code{x} is a single column.
#' @param y A character string specifying the numeric column for the y-axis.
#'  When \code{NULL}, the count of observations in each (\code{x},
#'  \code{group_by}, \code{facet_by}) combination is used.
#' @param scale_y A logical value. When \code{TRUE}, y-values are scaled to
#'  proportions within each (\code{x}, \code{facet_by}) group so that each
#'  x position stacks to 1.0. The y-axis labels switch from numeric to
#'  percent format automatically.
#' @param group_by A character vector of column names to fill the bars and
#'  areas by. Each unique combination becomes a separate stacked segment.
#'  Multiple columns are concatenated with \code{group_by_sep}. When
#'  \code{NULL}, a single filled bar/area is drawn and the legend is hidden.
#' @param group_by_sep A character string to separate concatenated
#'  \code{group_by} columns. Default \code{"_"}.
#' @param group_name A character string used as the fill legend title.
#'  When \code{NULL}, the \code{group_by} column name is used.
#' @keywords internal
#' @return A \code{ggplot} object with \code{height} and \code{width}
#'  attributes (in inches) attached.
#' @importFrom rlang syms :=
#' @importFrom dplyr summarise n %>%
#' @importFrom tidyr complete
#' @importFrom ggplot2 geom_area geom_col scale_x_discrete scale_y_continuous scale_fill_manual
#' @importFrom ggplot2 labs theme element_line element_text position_stack waiver
TrendPlotAtomic <- function(
    data,
    x,
    y = NULL,
    x_sep = "_",
    group_by = NULL,
    group_by_sep = "_",
    group_name = NULL,
    scale_y = FALSE,
    theme = "theme_this",
    theme_args = list(),
    palette = "Paired",
    palcolor = NULL,
    palreverse = FALSE,
    alpha = 1,
    facet_by = NULL,
    facet_scales = "fixed",
    facet_ncol = NULL,
    facet_nrow = NULL,
    facet_byrow = TRUE,
    x_text_angle = 0,
    aspect.ratio = 1,
    legend.position = waiver(),
    legend.direction = "vertical",
    title = NULL,
    subtitle = NULL,
    xlab = NULL,
    ylab = NULL,
    keep_empty = FALSE,
    keep_na = FALSE,
    ...
) {
    if (isTRUE(keep_empty)) {
        stop(
            "[TrendPlot] 'keep_empty' = TRUE is not supported for TrendPlot as it would break the continuity of the plot."
        )
    }
    ggplot <- if (getOption("plotthis.gglogger.enabled", FALSE)) {
        gglogger::ggplot
    } else {
        ggplot2::ggplot
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

    orig_data <- data
    if (is.null(y)) {
        y <- ".count"
        data <- data %>%
            dplyr::group_by(!!!syms(unique(c(x, group_by, facet_by)))) %>%
            summarise(.count = n(), .groups = "drop")

        for (col in unique(c(x, group_by, facet_by))) {
            data[[col]] <- factor(
                data[[col]],
                levels = levels(orig_data[[col]])
            )
        }
    }

    if (isTRUE(scale_y)) {
        data <- data %>%
            dplyr::group_by(!!!syms(unique(c(x, facet_by)))) %>%
            mutate(!!sym(y) := !!sym(y) / sum(!!sym(y))) %>%
            ungroup()

        for (col in unique(c(x, facet_by))) {
            data[[col]] <- factor(
                data[[col]],
                levels = levels(orig_data[[col]])
            )
        }
    }
    data <- process_keep_na_empty(data, keep_na, keep_empty)
    keep_empty_x <- keep_empty[[x]]
    keep_empty_group <- if (!is.null(group_by)) keep_empty[[group_by]] else NULL
    keep_empty_facet <- if (!is.null(facet_by)) {
        keep_empty[[facet_by[1]]]
    } else {
        NULL
    }
    if (length(facet_by) > 1) {
        stopifnot(
            "[TrendPlot] `keep_empty` for `facet_by` variables must be identical." = identical(
                keep_empty_facet,
                keep_empty[[facet_by[2]]]
            )
        )
    }

    if (is.null(group_by)) {
        data$.fill <- factor("")
        group_by <- ".fill"
        legend.position <- ifelse(
            inherits(legend.position, "waiver"),
            "none",
            "right"
        )
    } else {
        # data[[group_by]] <- droplevels(data[[group_by]])
        # # fill up some missing group_by values for each x, and fill it with 0 for y
        # fill_levels <- levels(data[[group_by]])
        # complete_fill <- list(0)
        # names(complete_fill) <- y
        # data <- data %>%
        #     dplyr::group_by(!!!syms(unique(c(group_by, facet_by)))) %>%
        #     complete(!!sym(x), fill = complete_fill) %>%
        #     ungroup()
        legend.position <- ifelse(
            inherits(legend.position, "waiver"),
            "right",
            legend.position
        )
    }
    # Complete all x * group_by (and facet_by) combinations for the area layer
    # so that geom_area doesn't interpolate across missing groups, which would
    # cause stacked areas to exceed the correct total.
    complete_vars <- unique(c(x, group_by, facet_by))
    complete_fill <- setNames(list(0), y)
    data_for_area <- data %>%
        tidyr::complete(!!!syms(complete_vars), fill = complete_fill)
    # Restore factor levels that complete() may have altered
    for (col in complete_vars) {
        if (is.factor(data[[col]])) {
            data_for_area[[col]] <- factor(
                data_for_area[[col]],
                levels = levels(data[[col]])
            )
        }
    }

    nr <- nrow(data_for_area)
    dat_area <- data_for_area[rep(seq_len(nr), each = 2), , drop = FALSE]
    # Convert factor to numeric, handling NA values
    x_numeric <- as.numeric(dat_area[[x]])
    # For NA values in factors, assign them the position after all levels
    na_pos <- nlevels(dat_area[[x]]) + 1
    x_numeric[is.na(x_numeric)] <- na_pos
    dat_area[[x]] <- x_numeric
    dat_area[seq(1, nr * 2, 2), x] <- dat_area[seq(1, nr * 2, 2), x] - 0.2
    dat_area[seq(2, nr * 2, 2), x] <- dat_area[seq(2, nr * 2, 2), x] + 0.2

    group_vals <- levels(data[[group_by]])
    if (anyNA(data[[group_by]])) {
        group_vals <- c(group_vals, NA)
    }
    group_cols <- palette_this(
        group_vals,
        palette = palette,
        palcolor = palcolor,
        NA_keep = TRUE,
        reverse = palreverse
    )

    position <- position_stack(vjust = 0.5)

    just <- calc_just(x_text_angle)
    p <- ggplot(data, aes(x = !!sym(x), y = !!sym(y), fill = !!sym(group_by))) +
        geom_area(
            data = dat_area,
            mapping = aes(x = !!sym(x), fill = !!sym(group_by)),
            alpha = alpha / 2,
            color = "grey50",
            position = position,
            show.legend = TRUE
        ) +
        geom_col(
            aes(fill = !!sym(group_by)),
            width = 0.4,
            color = "black",
            alpha = alpha,
            position = position,
            show.legend = TRUE
        ) +
        scale_x_discrete(expand = c(0, 0), drop = !isTRUE(keep_empty_x)) +
        scale_y_continuous(
            expand = c(0, 0),
            labels = if (isFALSE(scale_y)) scales::number else scales::percent
        ) +
        # scale_fill_manual(
        #     name = group_name %||% group_by,
        #     values = palette_this(levels(data[[group_by]]), palette = palette, palcolor = palcolor)) +
        labs(
            title = title,
            subtitle = subtitle,
            x = xlab %||% "",
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

    if (isTRUE(keep_empty_group)) {
        p <- p +
            scale_fill_manual(
                name = group_name %||% group_by,
                values = group_cols,
                breaks = group_vals,
                limits = group_vals,
                drop = FALSE
            )
    } else {
        p <- p +
            scale_fill_manual(
                name = group_name %||% group_by,
                values = group_cols
            )
    }

    xs <- levels(data[[x]])
    if (anyNA(data[[x]])) {
        xs <- c(xs, NA)
    }
    dims <- calculate_plot_dimensions(
        base_height = ifelse(length(xs) < 10, 4.5, 6.5),
        aspect.ratio = aspect.ratio,
        n_x = length(xs),
        x_scale_factor = ifelse(length(xs) < 10, 0.8, 0.25),
        legend.position = legend.position,
        legend.direction = legend.direction,
        legend_n = length(group_vals),
        legend_nchar = max(nchar(as.character(group_vals)), na.rm = TRUE)
    )

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


#' Trend plot
#'
#' @description
#' Draws a trend plot combining stacked bars with a semi-transparent area
#' background to show how one or more groups accumulate across a discrete
#' x-axis variable. This hybrid style sits between a bar plot and an area
#' plot: it preserves the discrete category separation of bars while
#' softening the visual with an area fill, making trends across categories
#' easier to perceive.
#'
#' The function supports \strong{count aggregation} (omit \code{y} to plot
#' observation counts per x-category), \strong{proportion scaling}
#' (\code{scale_y = TRUE} normalises each x position to 100\%), per-group
#' colour control, faceting, and splitting into separate sub-plots via
#' \code{split_by}.
#'
#' @seealso \code{\link{AreaPlot}} for a pure stacked area plot.
#'
#' @section split_by workflow:
#' When \code{split_by} is provided:
#' \enumerate{
#'   \item \code{\link{check_keep_na}()} and \code{\link{check_keep_empty}()}
#'         normalise the \code{keep_na} / \code{keep_empty} arguments for all
#'         columns (\code{x}, \code{split_by}, \code{group_by}, \code{facet_by}).
#'   \item The \code{split_by} column is validated and its NA / empty levels
#'         are processed via \code{\link{process_keep_na_empty}()}. It is
#'         then removed from the per-column \code{keep_na} / \code{keep_empty}
#'         lists.
#'   \item The data frame is split by \code{split_by} (preserving level
#'         order). If \code{split_by} is \code{NULL}, the data is wrapped in
#'         a single-element list with name \code{"..."}.
#'   \item Per-split \code{palette}, \code{palcolor},
#'         \code{legend.position}, and \code{legend.direction} are resolved
#'         via \code{\link{check_palette}()}, \code{\link{check_palcolor}()},
#'         and \code{\link{check_legend}()}.
#'   \item \code{\link{TrendPlotAtomic}()} is called for each split. If
#'         \code{title} is a function, it receives the split level name and
#'         can generate dynamic titles.
#'   \item Results are combined via \code{\link{combine_plots}()} (when
#'         \code{combine = TRUE}) or returned as a named list.
#' }
#'
#' @inheritParams common_args
#' @inheritParams TrendPlotAtomic
#' @param split_by The column(s) to split the data by and produce separate
#'  sub-plots. Multiple columns are concatenated with \code{split_by_sep}.
#' @param split_by_sep A character string to separate concatenated
#'  \code{split_by} columns. Default \code{"_"}.
#' @param seed A numeric seed for reproducibility. Passed to
#'  \code{\link{validate_common_args}()}.
#' @param combine Logical; when \code{TRUE} (default), returns a combined
#'  \code{patchwork} object. When \code{FALSE}, returns a named list of
#'  individual \code{ggplot} objects.
#' @param ncol,nrow Integer number of columns / rows for the combined layout
#'  (passed to \code{\link[patchwork]{wrap_plots}}).
#' @param byrow Logical; fill the combined layout by row. Default \code{TRUE}
#'  (passed to \code{\link[patchwork]{wrap_plots}}).
#' @param axes A character string specifying how axes should be treated across
#'  the combined layout (passed to \code{\link[patchwork]{wrap_plots}}).
#' @param axis_titles A character string specifying how axis titles should be
#'  treated across the combined layout. Defaults to \code{axes}.
#' @param guides A character string specifying how guides (legends) should be
#'  collected across panels. Default \code{"collect"} (passed to
#'  \code{\link{combine_plots}()}).
#' @param design A custom layout design for the combined plot (passed to
#'  \code{\link{combine_plots}()}).
#' @return A \code{ggplot} object, a \code{patchwork} object, or a named list
#'  of \code{ggplot} objects (when \code{combine = FALSE}), each with
#'  \code{height} and \code{width} attributes in inches.
#' @export
#' @importFrom ggplot2 waiver
#' @examples
#' \donttest{
#' data <- data.frame(
#'     x = factor(rep(c("A", "B", NA, "D"), 3), levels = LETTERS[1:4]),
#'     y = c(1, 3, 6, 5, 4, 2, 5, 7, 8, 9, 4, 8),
#'     group = factor(rep(c("F1", NA, "F3"), each = 4), levels = c("F1", "F2", "F3"))
#' )
#'
#' # Basic trend plot with grouping
#' TrendPlot(data, x = "x", y = "y", group_by = "group")
#'
#' # Scaled to proportions
#' TrendPlot(data, x = "x", y = "y", group_by = "group",
#'          scale_y = TRUE)
#'
#' # Split into sub-plots (no group_by -- single-colour fill)
#' TrendPlot(data, x = "x", y = "y", split_by = "group")
#'
#' # Per-split palettes
#' TrendPlot(data, x = "x", y = "y", split_by = "group",
#'           palette = c(F1 = "Set1", F3 = "Dark2"))
#'
#' # How keep_na and keep_empty work
#' TrendPlot(data, x = "x", y = "y", group_by = "group",
#'          keep_na = TRUE, keep_empty = TRUE)
#' TrendPlot(data, x = "x", y = "y", group_by = "group",
#'          keep_na = TRUE, keep_empty = list(x = FALSE, group = 'level'))
#'
#' # Faceting
#' TrendPlot(data, x = "x", y = "y", facet_by = "group",
#'          keep_na = TRUE, keep_empty = list(x = FALSE, group = 'level'))
#' }
TrendPlot <- function(
    data,
    x,
    y = NULL,
    x_sep = "_",
    split_by = NULL,
    split_by_sep = "_",
    group_by = NULL,
    group_by_sep = "_",
    group_name = NULL,
    scale_y = FALSE,
    theme = "theme_this",
    theme_args = list(),
    palette = "Paired",
    palcolor = NULL,
    palreverse = FALSE,
    alpha = 1,
    facet_by = NULL,
    facet_scales = "fixed",
    facet_ncol = NULL,
    facet_nrow = NULL,
    facet_byrow = TRUE,
    x_text_angle = 0,
    aspect.ratio = 1,
    legend.position = waiver(),
    legend.direction = "vertical",
    title = NULL,
    subtitle = NULL,
    xlab = NULL,
    ylab = NULL,
    keep_na = FALSE,
    keep_empty = FALSE,
    seed = 8525,
    combine = TRUE,
    nrow = NULL,
    ncol = NULL,
    byrow = TRUE,
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
            TrendPlotAtomic(
                datas[[nm]],
                x = x,
                y = y,
                x_sep = x_sep,
                group_by = group_by,
                group_by_sep = group_by_sep,
                group_name = group_name,
                scale_y = scale_y,
                theme = theme,
                theme_args = theme_args,
                palette = palette[[nm]],
                palcolor = palcolor[[nm]],
                palreverse = palreverse,
                alpha = alpha,
                facet_by = facet_by,
                facet_scales = facet_scales,
                facet_ncol = facet_ncol,
                facet_nrow = facet_nrow,
                facet_byrow = facet_byrow,
                x_text_angle = x_text_angle,
                aspect.ratio = aspect.ratio,
                legend.position = legend.position[[nm]],
                legend.direction = legend.direction[[nm]],
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
