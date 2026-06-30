#' Atomic ring plot (internal)
#'
#' @description
#' Core implementation for drawing a single ring (multi-layer donut) plot.
#' This is the workhorse behind the exported \code{\link{RingPlot}} function —
#' it takes a **single** data frame (no \code{split_by} support) and returns a
#' \code{ggplot} object.  Each level of \code{x} becomes a concentric ring,
#' and within each ring the \code{group_by} variable divides the ring into
#' filled segments drawn via \code{geom_col()} with \code{coord_polar("y")}.
#'
#' When \code{y = NULL}, the count of observations per combination is used.
#' y-values are always normalised to proportions (0–1) within each
#' (\code{x}, \code{facet_by}) group so that each complete ring sums to 1.
#'
#' @section Architecture:
#' \enumerate{
#'   \item \strong{Column resolution} — \code{x}, \code{group_by}, \code{y},
#'         and \code{facet_by} are validated and transformed via
#'         \code{\link{check_columns}}.  If \code{x} is \code{NULL}, a dummy
#'         \code{.x} column (value \code{1}) is created, producing a single-ring
#'         (pie-chart-like) plot.  If \code{group_by} is \code{NULL}, a dummy
#'         \code{.group_by} factor is created and the legend is suppressed.
#'         Multi-column \code{group_by} inputs are concatenated with
#'         \code{group_by_sep}.
#'   \item \strong{NA / empty-level handling} — \code{\link{process_keep_na_empty}()}
#'         applies \code{keep_na} and \code{keep_empty} policies.  Per-column
#'         \code{keep_empty} settings are extracted for \code{x},
#'         \code{group_by}, and \code{facet_by} independently.  The facet
#'         columns must have identical \code{keep_empty} values.
#'   \item \strong{Count aggregation} — when \code{y = NULL}, the count of
#'         observations in each unique (\code{x}, \code{group_by},
#'         \code{facet_by}) combination is computed as a new \code{.y} column.
#'         Factor levels are preserved after aggregation.
#'   \item \strong{Proportion scaling} — y-values are divided by the sum within
#'         each (\code{x}, \code{facet_by}) group, normalising each ring to
#'         sum to 1.0.  This is essential for polar coordinates where each ring
#'         represents a full 360 degrees.
#'   \item \strong{Ring resolution} — The levels of \code{x} are reversed so
#'         the outermost ring appears first in the data (rings are drawn
#'         outward from the origin).  \code{NA} levels are appended.
#'   \item \strong{Label logic} — When \code{label = NULL}, labels are
#'         automatically suppressed for single-ring plots and shown for
#'         multi-ring plots.  Labels are placed at the inner edge of each ring
#'         via \code{geom_label()}.
#'   \item \strong{Clockwise ordering} — When \code{clockwise = TRUE} (default),
#'         the \code{group_by} levels are reversed so segments appear clockwise
#'         from the top.  The legend guide also reverses its order.
#'   \item \strong{Colour mapping} — \code{\link{palette_this}()} assigns
#'         colours to all \code{group_by} levels, including \code{NA}
#'         (defaulting to \code{"grey80"}).
#'   \item \strong{Plot assembly} — The \code{ggplot} object is built with
#'         \code{geom_col()} (ring segments), \code{coord_polar("y")} (polar
#'         transform), \code{scale_x_discrete()} (with a leading space level
#'         as the donut hole), and \code{scale_fill_manual()} with per-group
#'         colours.  All axes, ticks, grid lines, and panel borders are removed
#'         for a clean appearance.  The fill scale \code{drop} argument is
#'         controlled by \code{keep_empty_group}.
#'   \item \strong{Dimension calculation} — \code{\link{calculate_plot_dimensions}()}
#'         computes plot height and width from \code{aspect.ratio} and legend
#'         metrics.  The resulting \code{height} / \code{width} attributes are
#'         stored on the \code{ggplot} object.
#'   \item \strong{Faceting} — \code{\link{facet_plot}()} wraps the plot with
#'         \code{facet_wrap} / \code{facet_grid} if \code{facet_by} is
#'         provided, respecting the \code{keep_empty} setting for facet
#'         variables.
#' }
#'
#' @inheritParams common_args
#' @param x A character string specifying the column to define the rings
#'  (concentric layers) of the plot.  Each unique value becomes one ring.
#'  When \code{NULL}, a single-ring (pie-chart-like) plot is produced.
#'  Character or factor columns are expected.  Multiple columns are not
#'  supported for \code{x} in ring plots.
#' @param y A character string specifying the numeric column for segment
#'  sizing.  When \code{NULL} (default), the count of observations in each
#'  (\code{x}, \code{group_by}, \code{facet_by}) combination is used.
#'  Values are always normalised to proportions within each ring.
#' @param group_by A character vector of column names to divide each ring
#'  into filled segments.  Each unique combination becomes one segment per
#'  ring.  Multiple columns are concatenated with \code{group_by_sep}.
#'  When \code{NULL}, a single unsegmented ring is drawn and the legend
#'  is hidden.
#' @param group_by_sep A character string to join multiple \code{group_by}
#'  columns.  Default \code{"_"}.  Ignored when \code{group_by} is a single
#'  column.
#' @param group_name A character string used as the fill legend title.
#'  When \code{NULL}, the \code{group_by} column name is used.
#' @param label A logical value controlling whether ring labels are shown.
#'  Labels display the \code{x} values (ring names) at the inner edge of
#'  each ring.  Default \code{NULL} auto-selects: \code{FALSE} for
#'  single-ring plots, \code{TRUE} for multi-ring plots.
#' @param clockwise A logical value.  When \code{TRUE} (default), segments
#'  within each ring are drawn clockwise (top-to-right).  When
#'  \code{FALSE}, they are drawn counter-clockwise.  The legend also
#'  reverses to match the drawing order.
#' @keywords internal
#' @return A \code{ggplot} object with \code{height} and \code{width}
#'  attributes (in inches) attached.
#' @importFrom rlang sym syms
#' @importFrom dplyr %>% summarise n ungroup mutate
#' @importFrom ggplot2 geom_col scale_fill_manual scale_x_discrete geom_label
RingPlotAtomic <- function(
    data,
    x = NULL,
    y = NULL,
    group_by = NULL,
    group_by_sep = "_",
    group_name = NULL,
    label = NULL,
    clockwise = TRUE,
    keep_na = FALSE,
    keep_empty = FALSE,
    facet_by = NULL,
    facet_scales = "free_y",
    facet_ncol = NULL,
    facet_nrow = NULL,
    facet_byrow = TRUE,
    theme = "theme_this",
    theme_args = list(),
    palette = "Paired",
    palcolor = NULL,
    palreverse = FALSE,
    alpha = 1,
    aspect.ratio = 1,
    legend.position = "right",
    legend.direction = "vertical",
    title = NULL,
    subtitle = NULL,
    xlab = NULL,
    ylab = NULL,
    seed = 8525,
    ...
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
    if (is.null(group_by)) {
        group_by <- ".group_by"
        data[[group_by]] <- factor("")
        group_guide <- "none"
    } else {
        group_guide <- guide_legend(reverse = clockwise)
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
            "[RingPlot] `keep_empty` for `facet_by` variables must be identical." = identical(
                keep_empty_facet,
                keep_empty[[facet_by[2]]]
            )
        )
    }

    orig_data <- data
    if (is.null(y)) {
        data <- data %>%
            dplyr::group_by(!!!syms(unique(c(x, group_by, facet_by)))) %>%
            summarise(.y = n(), .groups = "drop")
        for (col in unique(c(x, facet_by))) {
            data[[col]] <- factor(
                data[[col]],
                levels = levels(orig_data[[col]])
            )
        }
        y <- ".y"
    } else {
        y <- check_columns(data, y)
    }
    # make sure each ring sums to 1
    data <- data %>%
        dplyr::group_by(!!!syms(unique(c(x, facet_by)))) %>%
        mutate(!!sym(y) := !!sym(y) / sum(!!sym(y))) %>%
        ungroup()
    for (col in unique(x, facet_by)) {
        data[[col]] <- factor(data[[col]], levels = levels(orig_data[[col]]))
    }
    rm(orig_data)

    rings <- rev(levels(data[[x]]))
    if (anyNA(data[[x]])) {
        rings <- c(rings, NA)
    }
    if (length(rings) == 1 && is.null(label)) {
        label <- FALSE
    } else if (length(rings) > 1 && is.null(label)) {
        label <- TRUE
    }
    if (isTRUE(clockwise)) {
        data[[group_by]] <- factor(
            data[[group_by]],
            levels = rev(levels(data[[group_by]]))
        )
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
    colors <- palette_this(
        group_vals,
        palette = palette,
        palcolor = palcolor,
        NA_keep = TRUE,
        reverse = palreverse
    )

    p <- ggplot(data, aes(x = !!sym(x), y = !!sym(y), fill = !!sym(group_by))) +
        geom_col(
            width = 0.9,
            color = "white",
            alpha = alpha,
            show.legend = TRUE
        ) +
        coord_polar("y", start = 0) +
        scale_x_discrete(limits = c(" ", rings), drop = !isTRUE(keep_empty_x)) +
        do_call(theme, theme_args) +
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
        labs(
            title = title,
            subtitle = subtitle,
            x = xlab %||% "",
            y = ylab %||% ""
        )

    if (isTRUE(keep_empty_group)) {
        p <- p +
            scale_fill_manual(
                name = group_name %||% group_by,
                values = colors,
                na.value = colors["NA"] %||% "grey80",
                breaks = rev(group_vals),
                limits = rev(group_vals),
                drop = FALSE,
                guide = group_guide
            )
    } else {
        p <- p +
            scale_fill_manual(
                name = group_name %||% group_by,
                values = colors,
                na.value = colors["NA"] %||% "grey80",
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
            label_df <- rbind(
                label_df,
                data.frame(x = NA, y = 0, stringsAsFactors = FALSE)
            )
        }
        names(label_df)[1] <- x
        label_df[[x]] <- factor(label_df[[x]], levels = rings)
        label_df$label_text <- ifelse(
            is.na(label_df[[x]]),
            "NA",
            as.character(label_df[[x]])
        )

        p <- p +
            geom_label(
                aes(label = !!sym("label_text"), x = !!sym(x), y = 0),
                data = label_df,
                inherit.aes = FALSE,
                color = "grey20",
                size = text_size_scale * 3
            )
    }

    dims <- calculate_plot_dimensions(
        base_height = 4.5,
        aspect.ratio = aspect.ratio,
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

#' Ring plot (multi-layer donut chart)
#'
#' @description
#' Draws a ring plot (multi-layer donut chart) where each level of \code{x}
#' becomes a concentric ring divided into filled segments by \code{group_by}.
#' The plot is built with \code{geom_col()} under \code{coord_polar("y")},
#' producing a publication-quality ring chart with automatic count aggregation,
#' per-group colour assignment, faceting, and splitting into sub-plots.
#'
#' When \code{x = NULL}, a single-ring plot is produced (functionally
#' equivalent to a pie chart via \code{\link{PieChart}}).
#'
#' @section split_by workflow:
#' When \code{split_by} is provided:
#' \enumerate{
#'   \item \code{\link{check_keep_na}()} and \code{\link{check_keep_empty}()}
#'         normalise the \code{keep_na} / \code{keep_empty} arguments for all
#'         columns (\code{x}, \code{group_by}, \code{split_by}, \code{facet_by}).
#'   \item The \code{split_by} column is validated and its NA / empty levels
#'         are processed via \code{\link{process_keep_na_empty}()}.  It is
#'         then removed from the per-column \code{keep_na} / \code{keep_empty}
#'         lists.
#'   \item The data frame is split by \code{split_by} (preserving level
#'         order).  If \code{split_by} is \code{NULL}, the data is wrapped in
#'         a single-element list with name \code{"..."}.
#'   \item Per-split \code{palette}, \code{palcolor},
#'         \code{legend.position}, and \code{legend.direction} are resolved
#'         via \code{\link{check_palette}()}, \code{\link{check_palcolor}()},
#'         and \code{\link{check_legend}()}.
#'   \item \code{\link{RingPlotAtomic}()} is called for each split.  If
#'         \code{title} is a function, it receives the split level name and
#'         can generate dynamic titles.
#'   \item Results are combined via \code{\link{combine_plots}()} (when
#'         \code{combine = TRUE}) or returned as a named list.
#' }
#'
#' @seealso \code{\link{PieChart}}
#' @inheritParams common_args
#' @inheritParams RingPlotAtomic
#' @param split_by The column(s) to split the data by and produce separate
#'  sub-plots.  Multiple columns are concatenated with \code{split_by_sep}.
#' @param split_by_sep A character string to separate concatenated
#'  \code{split_by} columns.  Default \code{"_"}.
#' @param seed A numeric seed for reproducibility.  Passed to
#'  \code{\link{validate_common_args}()}.
#' @param combine Logical; when \code{TRUE} (default), returns a combined
#'  \code{patchwork} object.  When \code{FALSE}, returns a named list of
#'  individual \code{ggplot} objects.
#' @param ncol,nrow Integer number of columns / rows for the combined layout
#'  (passed to \code{\link[patchwork]{wrap_plots}}).
#' @param byrow Logical; fill the combined layout by row.  Default \code{TRUE}
#'  (passed to \code{\link[patchwork]{wrap_plots}}).
#' @param axes A character string specifying how axes should be treated across
#'  the combined layout (passed to \code{\link[patchwork]{wrap_plots}}).
#' @param axis_titles A character string specifying how axis titles should be
#'  treated across the combined layout.  Defaults to \code{axes}.
#' @param guides A character string specifying how guides (legends) should be
#'  collected across panels.  Default \code{"collect"} (passed to
#'  \code{\link{combine_plots}()}).
#' @param design A custom layout design for the combined plot (passed to
#'  \code{\link{combine_plots}()}).
#' @return A \code{ggplot} object, a \code{patchwork} object, or a named list
#'  of \code{ggplot} objects (when \code{combine = FALSE}), each with
#'  \code{height} and \code{width} attributes in inches.
#' @export
#' @examples
#' \donttest{
#' # Basic single-ring plot (pie-chart-like)
#' RingPlot(datasets::iris, group_by = "Species")
#'
#' # Multi-ring plot with faceting
#' RingPlot(datasets::mtcars, x = "cyl", group_by = "carb", facet_by = "vs")
#'
#' # Split into sub-plots with per-split palettes
#' RingPlot(datasets::mtcars, x = "cyl", group_by = "carb", split_by = "vs",
#'         palette = c("0" = "Set1", "1" = "Paired"))
#'
#' # Custom data with NA and empty levels
#' data <- data.frame(
#'   x = factor(c("A", "B", NA, "D", "A", "B", NA, "D"), levels = c("A", "B", "C", "D")),
#'   y = c(1, 2, 5, 3, 4, 5, 2, 6),
#'   group = factor(c("a", "a", "a", NA, NA, "c", "c", "c"), levels = c("a", "b", "c"))
#' )
#'
#' # Default: NA and empty levels dropped
#' RingPlot(data, x = "x", y = "y", group_by = "group")
#'
#' # Keep NA and empty levels
#' RingPlot(data, x = "x", y = "y", group_by = "group",
#'         keep_na = TRUE, keep_empty = TRUE)
#'
#' # Per-column keep_na / keep_empty via named lists
#' RingPlot(data, x = "x", y = "y", group_by = "group",
#'         keep_na = TRUE, keep_empty = list(x = FALSE, group = 'level'))
#' }
RingPlot <- function(
    data,
    x = NULL,
    y = NULL,
    group_by = NULL,
    group_by_sep = "_",
    group_name = NULL,
    label = NULL,
    split_by = NULL,
    split_by_sep = "_",
    facet_by = NULL,
    facet_scales = "free_y",
    facet_ncol = NULL,
    facet_nrow = NULL,
    facet_byrow = TRUE,
    theme = "theme_this",
    theme_args = list(),
    palette = "Paired",
    palcolor = NULL,
    palreverse = FALSE,
    alpha = 1,
    aspect.ratio = 1,
    keep_na = FALSE,
    keep_empty = FALSE,
    legend.position = "right",
    legend.direction = "vertical",
    title = NULL,
    subtitle = NULL,
    xlab = NULL,
    ylab = NULL,
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
            RingPlotAtomic(
                datas[[nm]],
                x = x,
                y = y,
                label = label,
                group_by = group_by,
                group_by_sep = group_by_sep,
                group_name = group_name,
                facet_by = facet_by,
                facet_scales = facet_scales,
                facet_ncol = facet_ncol,
                facet_nrow = facet_nrow,
                facet_byrow = facet_byrow,
                theme = theme,
                theme_args = theme_args,
                palette = palette[[nm]],
                palcolor = palcolor[[nm]],
                palreverse = palreverse,
                alpha = alpha,
                aspect.ratio = aspect.ratio,
                keep_na = keep_na,
                keep_empty = keep_empty,
                legend.position = legend.position[[nm]],
                legend.direction = legend.direction[[nm]],
                title = title,
                subtitle = subtitle,
                xlab = xlab,
                ylab = ylab,
                seed = seed,
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
