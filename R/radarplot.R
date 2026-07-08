#' Atomic radar/spider plot (internal)
#'
#' @description
#' Core implementation for drawing a single radar (or spider) chart.  This
#' is the workhorse behind the exported \code{\link{RadarPlot}} and
#' \code{\link{SpiderPlot}} functions — it takes a **single** data frame
#' (no \code{split_by} support) and returns a \code{ggplot} object.
#'
#' Radar charts display multivariate data on a two-dimensional polar
#' coordinate system where each variable (x-axis category) is placed
#' along a radial axis evenly spaced around the circle.  The data values
#' are connected by lines forming a polygon (or multiple polygons when
#' \code{group_by} is provided).  When \code{polygon = FALSE} (the
#' default) the grid is rendered as concentric circles (classic radar
#' chart); when \code{polygon = TRUE} straight polygonal grid lines are
#' used (spider chart variant).
#'
#' The polar coordinate system is implemented via a local
#' \code{coord_radar()} helper that extends
#' \code{ggplot2::CoordPolar} with \code{is_linear = TRUE}, allowing
#' discrete x-axis positions to be mapped to evenly spaced angular
#' coordinates.
#'
#' @section Architecture:
#' \enumerate{
#'   \item \strong{Column resolution} — \code{x}, \code{y},
#'         \code{group_by}, and \code{facet_by} are validated and
#'         transformed via \code{\link{check_columns}}.  Multi-column
#'         inputs for \code{x} and \code{group_by} are concatenated
#'         using \code{x_sep} and \code{group_by_sep}.
#'   \item \strong{Group setup} — When \code{group_by} is \code{NULL},
#'         a dummy \code{.group} factor is created so polygons still
#'         draw.  The legend is suppressed.  When \code{groups} is
#'         provided, data is filtered to only those group levels.
#'   \item \strong{Count aggregation} — When \code{y = NULL}, the count
#'         of observations in each unique (\code{x}, \code{group_by},
#'         \code{facet_by}) combination is computed.  Factor levels
#'         are preserved after aggregation.
#'   \item \strong{Proportion scaling} — The \code{scale_y} argument
#'         controls y-value normalisation: \code{"group"} scales within
#'         each group, \code{"global"} within each facet (or the whole
#'         dataset), \code{"x"} within each x category, and
#'         \code{"none"} leaves values raw.  Percent labels are used
#'         for scaled modes.
#'   \item \strong{NA / empty-level handling} —
#'         \code{\link{process_keep_na_empty}()} applies \code{keep_na}
#'         and \code{keep_empty} policies.  Per-column \code{keep_empty}
#'         settings for \code{x}, \code{group_by}, and \code{facet_by}
#'         are extracted; facet columns must share the same value.
#'   \item \strong{Coordinate setup} — The local \code{coord_radar()}
#'         creates a \code{CoordPolar} subclass with
#'         \code{is_linear = TRUE}, placing discrete x positions at
#'         evenly spaced angles.
#'   \item \strong{Y-axis range} — \code{y_min}, \code{y_max}, and
#'         \code{y_nbreaks} determine the radial axis limits and
#'         the number of concentric grid lines.
#'   \item \strong{Colour mapping} — \code{\link{palette_this}()}
#'         assigns colours to all \code{group_by} levels, including
#'         \code{NA} (defaulting to \code{"grey80"}).
#'   \item \strong{Background layer} — When \code{polygon = TRUE}, a
#'         polygonal background fills the area between \code{y_min} and
#'         \code{y_max}.  When \code{polygon = FALSE}, a smooth circular
#'         background is interpolated via 360 sample points.
#'   \item \strong{Radial grid lines} — \code{geom_path()} draws
#'         radial lines from \code{y_min} to \code{y_max} at each
#'         x-axis position, respecting facets.
#'   \item \strong{Polygon grid} — When \code{polygon = TRUE},
#'         concentric polygonal grid lines are drawn at each break
#'         level via \code{geom_polygon()}.
#'   \item \strong{Data rendering} — \code{geom_polygon()} draws the
#'         filled (or unfilled) polygons for each group.
#'         \code{geom_point()} adds points at each observation.
#'         Radial axis labels are rendered as text at a fixed
#'         angular offset.
#'   \item \strong{Scale configuration} — \code{scale_y_continuous()}
#'         sets the radial axis limits and breaks.
#'         \code{scale_x_discrete()} positions the category labels.
#'         Colour scales use \code{scale_fill_manual()} and
#'         \code{scale_color_manual()} with \code{drop} controlled
#'         by \code{keep_empty_group}.
#'   \item \strong{Circular grid lines} — When \code{polygon = FALSE},
#'         dashed circular grid lines are styled via
#'         \code{panel.grid.major.y}.
#'   \item \strong{Dimension calculation} —
#'         \code{\link{calculate_plot_dimensions}()} computes plot
#'         height and width from \code{aspect.ratio}, legend metrics,
#'         and a base height.
#'   \item \strong{Faceting} — \code{\link{facet_plot}()} wraps the
#'         plot with \code{facet_wrap} / \code{facet_grid} if
#'         \code{facet_by} is supplied, respecting the
#'         \code{keep_empty} setting for facet variables.
#' }
#'
#' @inheritParams common_args
#' @param x A character string specifying the column name of the data
#'  frame to plot on the x-axis (the radial categories).  Must be
#'  character or factor.  Multiple columns can be provided; they are
#'  concatenated with \code{x_sep} as the separator.
#' @param x_sep A character string used to join multiple \code{x}
#'  columns.  Default \code{"_"}.  Ignored when \code{x} is a single
#'  column.
#' @param y A character string specifying the numeric column for the
#'  radial axis.  When \code{NULL}, the count of observations in each
#'  (\code{x}, \code{group_by}, \code{facet_by}) combination is used.
#' @param group_by A character vector of column names to group the
#'  data into separate filled polygons.  Each unique combination
#'  becomes a distinct polygon layer.  Multiple columns are
#'  concatenated with \code{group_by_sep}.  When \code{NULL}, a
#'  single polygon is drawn with no legend.
#' @param group_by_sep A character string used to join multiple
#'  \code{group_by} columns.  Default \code{"_"}.
#' @param group_name A character string used as the colour/fill legend
#'  title.  When \code{NULL}, the \code{group_by} column name is used.
#' @param groups A character vector of group values (in the
#'  \code{group_by} column) to include in the plot.  When \code{NULL},
#'  all groups are included.  This can control which groups appear and
#'  their legend order.  Implies \code{keep_empty = FALSE} for the
#'  \code{group_by} column: groups not present in the data are not
#'  shown in the legend.
#' @param scale_y How should the radial axis be scaled?  Default is
#'  \code{"group"}.  Options are \code{"group"}, \code{"global"},
#'  \code{"x"}, and \code{"none"}.
#'  \itemize{
#'    \item{\code{"group"} — scaled to the fraction within each group.}
#'    \item{\code{"global"} — scaled to the fraction of the total.}
#'    \item{\code{"x"} — scaled to the fraction within each x-axis category.}
#'    \item{\code{"none"} — raw counts or values, no scaling.}
#'  }
#' @param y_min A numeric value setting the minimum of the radial
#'  axis.  Default \code{0}.
#' @param y_max A numeric value setting the maximum of the radial
#'  axis.  When \code{NULL}, the maximum data value is used.
#' @param y_nbreaks A numeric value for the number of breaks
#'  (concentric grid lines) on the radial axis.  Default \code{4}.
#' @param polygon A logical value.  When \code{TRUE}, the background
#'  grid is drawn as a polygon (spider chart style); when
#'  \code{FALSE} (default), concentric circles are used (radar chart
#'  style).
#' @param bg_color A character string specifying the background fill
#'  colour.  Default \code{"grey80"}.
#' @param bg_alpha A numeric value for the transparency of the
#'  background fill.  Default \code{0.1}.
#' @param fill A logical value.  When \code{TRUE} (default), the data
#'  polygons are filled with the group colour.  When \code{FALSE},
#'  only outlines are drawn.
#' @param linewidth A numeric value for the width of the polygon
#'  outline lines.  Default \code{1}.
#' @param pt_size A numeric value for the size of the data point
#'  markers.  Default \code{4}.
#' @param max_charwidth A numeric value for the maximum character
#'  width of x-axis labels before wrapping.  Default \code{16}.
#' @keywords internal
#' @return A \code{ggplot} object with \code{height} and \code{width}
#'  attributes (in inches) attached.
#' @importFrom rlang syms
#' @importFrom dplyr summarise n %>% distinct ungroup
#' @importFrom tidyr complete
#' @importFrom ggplot2 geom_polygon geom_point geom_text scale_y_continuous scale_x_discrete scale_fill_manual
#' @importFrom ggplot2 scale_color_manual coord_polar labs theme element_blank element_line element_text element_rect
#' @importFrom ggplot2 ggproto CoordPolar waiver
RadarPlotAtomic <- function(
    data,
    x,
    x_sep = "_",
    group_by = NULL,
    group_by_sep = "_",
    y = NULL,
    group_name = NULL,
    groups = NULL,
    scale_y = c("group", "global", "x", "none"),
    y_min = 0,
    y_max = NULL,
    y_nbreaks = 4,
    polygon = FALSE,
    fill = TRUE,
    linewidth = 1,
    pt_size = 4,
    max_charwidth = 16,
    bg_color = "grey80",
    bg_alpha = 0.1,
    theme = "theme_this",
    theme_args = list(),
    palette = "Paired",
    palcolor = NULL,
    palreverse = FALSE,
    facet_by = NULL,
    facet_scales = "fixed",
    facet_ncol = NULL,
    facet_nrow = NULL,
    facet_byrow = TRUE,
    alpha = 0.2,
    aspect.ratio = 1,
    legend.position = waiver(),
    legend.direction = "vertical",
    keep_na = FALSE,
    keep_empty = FALSE,
    title = NULL,
    subtitle = NULL,
    ...
) {
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

    if (is.null(group_by)) {
        data$.group <- factor("")
        group_by <- ".group"
        legend.position <- ifelse(
            inherits(legend.position, "waiver"),
            "none",
            "right"
        )
    } else {
        if (!is.null(groups)) {
            data <- data[data[[group_by]] %in% groups, , drop = FALSE]
            data[[group_by]] <- droplevels(data[[group_by]])
        }
        legend.position <- ifelse(
            inherits(legend.position, "waiver"),
            "right",
            legend.position
        )
    }

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

    scale_y <- match.arg(scale_y)
    if (scale_y == "group") {
        data <- data %>%
            dplyr::group_by(!!!syms(unique(c(group_by, facet_by)))) %>%
            mutate(!!sym(y) := !!sym(y) / sum(!!sym(y))) %>%
            ungroup()
        for (col in unique(c(group_by, facet_by))) {
            data[[col]] <- factor(
                data[[col]],
                levels = levels(orig_data[[col]])
            )
        }
    } else if (scale_y == "global") {
        if (!is.null(facet_by)) {
            data <- data %>%
                dplyr::group_by(!!!syms(unique(facet_by))) %>%
                mutate(!!sym(y) := !!sym(y) / sum(!!sym(y))) %>%
                ungroup()
            for (col in unique(facet_by)) {
                data[[col]] <- factor(
                    data[[col]],
                    levels = levels(orig_data[[col]])
                )
            }
        } else {
            data <- data %>%
                mutate(!!sym(y) := !!sym(y) / sum(!!sym(y)))
        }
    } else if (scale_y == "x") {
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
    rm(orig_data)

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
            "[RadarPLot/SpiderPlot] `keep_empty` for `facet_by` variables must be identical." = identical(
                keep_empty_facet,
                keep_empty[[facet_by[2]]]
            )
        )
    }

    # We need all levels of x in each group and facet
    # fill the y value with 0 if the level is missing
    # complete_fill = list(0)
    # names(complete_fill) = y
    # data <- data %>%
    #     dplyr::group_by(!!!syms(unique(c(group_by, facet_by)))) %>%
    #     complete(!!sym(x), fill = complete_fill) %>%
    #     ungroup()

    coord_radar <- function(theta = "x", start = 0, direction = 1) {
        theta <- match.arg(theta, c("x", "y"))
        r <- if (theta == "x") "y" else "x"
        ggproto(
            "CoordRadar",
            CoordPolar,
            theta = theta,
            r = r,
            start = start,
            clip = "off",
            direction = sign(direction),
            is_linear = function(coord) TRUE
        )
    }

    y_min <- y_min %||% min(data[[y]])
    y_max <- y_max %||% max(data[[y]])
    breaks <- seq(y_min, y_max, length.out = y_nbreaks)
    group_vals <- levels(data[[group_by]])
    if (anyNA(data[[group_by]])) {
        group_vals <- c(group_vals, NA)
    }
    group_colors <- palette_this(
        group_vals,
        palette = palette,
        palcolor = palcolor,
        NA_keep = TRUE,
        reverse = palreverse
    )

    # Add background (circle or polygon based on polygon parameter)
    x_levels <- levels(data[[x]])
    if (anyNA(data[[x]])) {
        # Otherwise there will be an angle shift if NA is present
        x_levels <- c(x_levels, "NA")
        levels(data[[x]]) <- x_levels
        data[[x]][is.na(data[[x]])] <- "NA"
    }
    n_x <- length(x_levels)
    p <- ggplot(
        data,
        aes(
            x = !!sym(x),
            y = !!sym(y),
            group = !!sym(group_by),
            color = !!sym(group_by)
        )
    )

    if (polygon) {
        # Polygonal background - connects discrete x-axis points
        bg_df <- data.frame(
            x = factor(
                c(x_levels, x_levels[1], rev(x_levels), rev(x_levels)[1]),
                levels = x_levels
            ),
            y = c(rep(y_max, n_x), y_max, rep(y_min, n_x), y_min),
            bg_group = 1
        )

        p <- p +
            geom_polygon(
                data = bg_df,
                aes(x = !!sym("x"), y = !!sym("y"), group = !!sym("bg_group")),
                fill = bg_color,
                alpha = bg_alpha,
                color = NA,
                inherit.aes = FALSE
            )
    } else {
        # Circular background - smooth interpolated circle
        # In polar coordinates with discrete x, we need to go from 0.5 to n_x+0.5
        n_smooth <- 360
        x_positions <- seq(0.5, n_x + 0.5, length.out = n_smooth + 1)

        bg_df <- data.frame(
            x_pos = c(x_positions, rev(x_positions)),
            y = c(rep(y_max, n_smooth + 1), rep(y_min, n_smooth + 1)),
            bg_group = 1
        )

        p <- p +
            geom_polygon(
                data = bg_df,
                aes(
                    x = !!sym("x_pos"),
                    y = !!sym("y"),
                    group = !!sym("bg_group")
                ),
                fill = bg_color,
                alpha = bg_alpha,
                color = NA,
                inherit.aes = FALSE
            )
    }

    # Add radial grid lines that only extend from y_min to y_max
    if (!is.null(facet_by)) {
        grid_df <- data %>% select(!!!syms(facet_by)) %>% distinct()
        grid_df <- expand_grid(grid_df, x = x_levels)
    } else {
        grid_df <- data.frame(x = x_levels)
    }
    # Create two points for each radial line
    n_lines <- nrow(grid_df)
    grid_df <- grid_df[rep(seq_len(n_lines), each = 2), , drop = FALSE]
    grid_df$y <- rep(c(y_min, y_max), times = n_lines)
    # Create a unique identifier for grouping each radial line
    grid_df$line_id <- rep(seq_len(n_lines), each = 2)

    p <- p +
        geom_path(
            data = grid_df,
            aes(x = !!sym("x"), y = !!sym("y"), group = !!sym("line_id")),
            colour = "grey80",
            linetype = 1,
            inherit.aes = FALSE
        )
    if (isTRUE(polygon)) {
        # Create proper polygon grid at each break level
        for (i in seq_along(breaks)) {
            if (!is.null(facet_by)) {
                poly_grid_df <- data %>%
                    select(!!!syms(facet_by)) %>%
                    distinct()
                poly_grid_df <- expand_grid(poly_grid_df, x = x_levels)
            } else {
                poly_grid_df <- data.frame(x = x_levels)
            }
            poly_grid_df$y <- breaks[i]
            poly_grid_df$poly_id <- i

            p <- p +
                geom_polygon(
                    data = poly_grid_df,
                    aes(
                        x = !!sym("x"),
                        y = !!sym("y"),
                        group = !!sym("poly_id")
                    ),
                    fill = "transparent",
                    color = "grey80",
                    linetype = 2,
                    inherit.aes = FALSE
                )
        }
    }

    if (isTRUE(fill)) {
        p <- p +
            geom_polygon(
                aes(fill = !!sym(group_by)),
                alpha = alpha,
                linewidth = linewidth,
                show.legend = TRUE
            )
    } else {
        p <- p +
            geom_polygon(
                alpha = alpha,
                linewidth = linewidth,
                fill = "transparent"
            )
    }

    if (!is.null(facet_by)) {
        labels_df <- data %>% select(!!!syms(facet_by)) %>% distinct()
        labels_df <- expand_grid(labels_df, breaks)
    } else {
        labels_df <- data.frame(breaks = breaks)
    }

    mc <- min(max(nchar(levels(data[[x]]))), max_charwidth)
    panel.spacing <- theme_args$panel.spacing %||% unit(mc * 2, "points")
    theme_args$panel.spacing <- NULL
    p <- p +
        geom_point(
            aes(colour = !!sym(group_by), fill = !!sym(group_by)),
            size = pt_size,
            shape = 21,
            show.legend = TRUE
        ) +
        geom_text(
            data = labels_df,
            mapping = aes(
                y = !!sym("breaks"),
                label = if (scale_y == "none") {
                    scales::number(!!sym("breaks"))
                } else {
                    scales::percent(!!sym("breaks"))
                }
            ),
            x = pi / (length(breaks) - 0.88),
            color = "grey20",
            inherit.aes = FALSE
        ) +
        scale_y_continuous(
            limits = c(y_min, y_max),
            breaks = breaks,
            expand = c(.1, 0, 0, 0)
        ) +
        scale_x_discrete(
            labels = scales::label_wrap(max_charwidth),
            drop = !isTRUE(keep_empty_x)
        ) +
        # scale_fill_manual(
        #     name = group_name %||% group_by,
        #     values = palette_this(levels(data[[group_by]]), palette = palette, palcolor = palcolor)
        # ) +
        # scale_color_manual(
        #     values = palette_this(levels(data[[group_by]]), palette = palette, palcolor = palcolor)
        # ) +
        coord_radar(start = -pi / nlevels(data[[x]])) +
        labs(title = title, subtitle = subtitle) +
        do_call(theme, theme_args) +
        ggplot2::theme(
            axis.text.y = element_blank(),
            axis.ticks = element_blank(),
            axis.title = element_blank(),
            panel.grid = element_blank(),
            panel.border = element_blank(),
            panel.spacing = panel.spacing
        )

    if (isTRUE(keep_empty_group)) {
        p <- p +
            scale_fill_manual(
                name = group_name %||% group_by,
                values = group_colors,
                na.value = group_colors["NA"] %||% "grey80",
                breaks = group_vals,
                limits = group_vals,
                drop = FALSE
            ) +
            scale_color_manual(
                values = group_colors,
                na.value = group_colors["NA"] %||% "grey80",
                breaks = group_vals,
                limits = group_vals,
                drop = FALSE
            )
    } else {
        p <- p +
            scale_fill_manual(
                name = group_name %||% group_by,
                values = group_colors
            ) +
            scale_color_manual(
                values = group_colors
            )
    }

    if (isFALSE(polygon)) {
        p <- p +
            ggplot2::theme(
                panel.grid.major.y = element_line(
                    colour = c(rep("grey80", length(breaks)), NA),
                    linetype = 2
                )
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

#' Radar plot / Spider plot
#'
#' @description
#' Draws a radar chart (concentric circular grid) or spider chart
#' (polygonal grid) displaying multivariate data in a two-dimensional
#' polar coordinate system.  Each x-axis category is placed at an
#' evenly spaced angular position around the chart, and numeric values
#' are plotted along the radial axis.
#'
#' The function supports \strong{count aggregation} (omit \code{y} to
#' plot observation counts), \strong{proportion scaling} (via
#' \code{scale_y}), per-group colour control, faceting, and splitting
#' into separate sub-plots via \code{split_by}.
#'
#' \code{\link{SpiderPlot}} is an alias that renders the same data with
#' polygonal grid lines (spider chart style) by using
#' \code{polygon = TRUE}.
#'
#' @section split_by Workflow:
#' When \code{split_by} is provided:
#' \enumerate{
#'   \item \code{\link{check_keep_na}()} and
#'         \code{\link{check_keep_empty}()} normalise the
#'         \code{keep_na} / \code{keep_empty} arguments for all
#'         relevant columns (\code{x}, \code{split_by},
#'         \code{group_by}, \code{facet_by}).
#'   \item The \code{split_by} column is validated and its NA / empty
#'         levels are processed via
#'         \code{\link{process_keep_na_empty}()}.  It is then removed
#'         from the per-column \code{keep_na} / \code{keep_empty}
#'         lists.
#'   \item The data frame is split by \code{split_by} (preserving
#'         level order).  If \code{split_by} is \code{NULL}, the data
#'         is wrapped in a single-element list with name \code{"..."}.
#'   \item Per-split \code{palette}, \code{palcolor},
#'         \code{legend.position}, and \code{legend.direction} are
#'         resolved via \code{\link{check_palette}()},
#'         \code{\link{check_palcolor}()}, and
#'         \code{\link{check_legend}()}.
#'   \item \code{\link{RadarPlotAtomic}()} is called for each split
#'         with \code{polygon = FALSE}.  If \code{title} is a
#'         function, it receives the split level name and can generate
#'         dynamic titles.
#'   \item Results are combined via \code{\link{combine_plots}()}
#'         (when \code{combine = TRUE}) or returned as a named list.
#' }
#'
#' @rdname radarplot
#' @inheritParams common_args
#' @inheritParams RadarPlotAtomic
#' @param split_by The column(s) to split the data by and produce
#'  separate sub-plots.  Multiple columns are concatenated with
#'  \code{split_by_sep}.
#' @param split_by_sep A character string to separate concatenated
#'  \code{split_by} columns.  Default \code{"_"}.
#' @param seed A numeric seed for reproducibility.  Passed to
#'  \code{\link{validate_common_args}()}.
#' @param combine Logical; when \code{TRUE} (default), returns a
#'  combined \code{patchwork} object.  When \code{FALSE}, returns a
#'  named list of individual \code{ggplot} objects.
#' @param ncol,nrow Integer number of columns / rows for the combined
#'  layout (passed to \code{\link[patchwork]{wrap_plots}}).
#' @param byrow Logical; fill the combined layout by row.  Default
#'  \code{TRUE} (passed to \code{\link[patchwork]{wrap_plots}}).
#' @param axes A character string specifying how axes should be
#'  treated across the combined layout (passed to
#'  \code{\link[patchwork]{wrap_plots}}).
#' @param axis_titles A character string specifying how axis titles
#'  should be treated across the combined layout.  Defaults to
#'  \code{axes}.
#' @param guides A character string specifying how guides (legends)
#'  should be collected across panels (passed to
#'  \code{\link{combine_plots}()}).
#' @param design A custom layout design for the combined plot (passed
#'  to \code{\link{combine_plots}()}).
#' @importFrom ggplot2 waiver
#' @return A \code{ggplot} object (when \code{combine = TRUE} and
#'  \code{split_by} is \code{NULL}), a \code{patchwork} object (when
#'  \code{combine = TRUE} and \code{split_by} is provided), or a named
#'  list of \code{ggplot} objects (when \code{combine = FALSE}), each
#'  with \code{height} and \code{width} attributes in inches.
#' @export
RadarPlot <- function(
    data,
    x,
    x_sep = "_",
    group_by = NULL,
    group_by_sep = "_",
    y = NULL,
    group_name = NULL,
    groups = NULL,
    scale_y = c("group", "global", "x", "none"),
    y_min = 0,
    y_max = NULL,
    y_nbreaks = 4,
    bg_color = "grey80",
    bg_alpha = 0.1,
    fill = TRUE,
    linewidth = 1,
    pt_size = 4,
    max_charwidth = 16,
    split_by = NULL,
    split_by_sep = "_",
    theme = "theme_this",
    theme_args = list(),
    palette = "Paired",
    palcolor = NULL,
    palreverse = FALSE,
    facet_by = NULL,
    facet_scales = "fixed",
    facet_ncol = NULL,
    facet_nrow = NULL,
    facet_byrow = TRUE,
    alpha = 0.2,
    aspect.ratio = 1,
    legend.position = waiver(),
    legend.direction = "vertical",
    keep_na = FALSE,
    keep_empty = FALSE,
    title = NULL,
    subtitle = NULL,
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
    keep_na <- check_keep_na(keep_na, c(x, group_by, facet_by, split_by))
    keep_empty <- check_keep_empty(
        keep_empty,
        c(x, group_by, facet_by, split_by)
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
            RadarPlotAtomic(
                datas[[nm]],
                x = x,
                x_sep = x_sep,
                group_by = group_by,
                group_by_sep = group_by_sep,
                y = y,
                group_name = group_name,
                groups = groups,
                scale_y = scale_y,
                y_min = y_min,
                y_max = y_max,
                y_nbreaks = y_nbreaks,
                polygon = FALSE,
                fill = fill,
                linewidth = linewidth,
                pt_size = pt_size,
                max_charwidth = max_charwidth,
                bg_color = bg_color,
                bg_alpha = bg_alpha,
                theme = theme,
                theme_args = theme_args,
                palette = palette[[nm]],
                palcolor = palcolor[[nm]],
                palreverse = palreverse,
                facet_by = facet_by,
                facet_scales = facet_scales,
                facet_ncol = facet_ncol,
                facet_nrow = facet_nrow,
                facet_byrow = facet_byrow,
                alpha = alpha,
                aspect.ratio = aspect.ratio,
                legend.position = legend.position[[nm]],
                legend.direction = legend.direction[[nm]],
                keep_na = keep_na,
                keep_empty = keep_empty,
                title = title,
                subtitle = subtitle,
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

#' @rdname radarplot
#' @description
#' `SpiderPlot` is a variant of \code{\link{RadarPlot}} that renders the chart with
#' straight polygonal grid lines (spider chart) instead of concentric
#' circles.  Internally, it calls \code{\link{RadarPlotAtomic}} with
#' \code{polygon = TRUE} but is otherwise identical to
#' \code{\link{RadarPlot}} in behaviour and parameters.
#'
#' @inheritParams common_args
#' @inheritParams RadarPlotAtomic
#' @param split_by The column(s) to split the data by and produce
#'  separate sub-plots.  Multiple columns are concatenated with
#'  \code{split_by_sep}.
#' @param split_by_sep A character string to separate concatenated
#'  \code{split_by} columns.  Default \code{"_"}.
#' @param seed A numeric seed for reproducibility.  Passed to
#'  \code{\link{validate_common_args}()}.
#' @param combine Logical; when \code{TRUE} (default), returns a
#'  combined \code{patchwork} object.  When \code{FALSE}, returns a
#'  named list of individual \code{ggplot} objects.
#' @param ncol,nrow Integer number of columns / rows for the combined
#'  layout (passed to \code{\link[patchwork]{wrap_plots}}).
#' @param byrow Logical; fill the combined layout by row.  Default
#'  \code{TRUE} (passed to \code{\link[patchwork]{wrap_plots}}).
#' @param axes A character string specifying how axes should be
#'  treated across the combined layout (passed to
#'  \code{\link[patchwork]{wrap_plots}}).
#' @param axis_titles A character string specifying how axis titles
#'  should be treated across the combined layout.  Defaults to
#'  \code{axes}.
#' @param guides A character string specifying how guides (legends)
#'  should be collected across panels (passed to
#'  \code{\link{combine_plots}()}).
#' @param design A custom layout design for the combined plot (passed
#'  to \code{\link{combine_plots}()}).
#' @export
#' @importFrom ggplot2 waiver
#' @return A \code{ggplot} object (when \code{combine = TRUE} and
#'  \code{split_by} is \code{NULL}), a \code{patchwork} object (when
#'  \code{combine = TRUE} and \code{split_by} is provided), or a named
#'  list of \code{ggplot} objects (when \code{combine = FALSE}), each
#'  with \code{height} and \code{width} attributes in inches.
#' @examples
#' \donttest{
#' set.seed(8525)
#'
#' # --- Radar chart with observation counts ---
#' data <- data.frame(
#'     x = factor(
#'         c(rep("A", 20), rep("B", 30), rep(NA, 30), rep("D", 40), rep("E", 50)),
#'         levels = LETTERS[1:5]
#'     ),
#'     group = factor(
#'         sample(c("G1", NA, "G3", "G4"), 170, replace = TRUE),
#'         levels = c("G1", "G2", "G3", "G4")
#'     )
#' )
#'
#' # Basic radar chart
#' RadarPlot(data, x = "x")
#'
#' # Keep NA and empty factor levels
#' RadarPlot(data, x = "x", keep_na = TRUE, keep_empty = TRUE)
#'
#' # Custom background colour
#' RadarPlot(data, x = "x", bg_color = "lightpink")
#'
#' # Raw counts (no proportion scaling)
#' RadarPlot(data, x = "x", scale_y = "none")
#'
#' # Grouped by a variable
#' RadarPlot(data, x = "x", group_by = "group", keep_na = TRUE)
#'
#' # Faceted by a variable
#' RadarPlot(data, x = "x", facet_by = "group")
#'
#' # Spider chart variant (polygonal grid)
#' SpiderPlot(data, x = "x")
#' SpiderPlot(data, x = "x", group_by = "group")
#'
#' # --- Radar chart with explicit y values ---
#' data <- data.frame(
#'     x = rep(LETTERS[1:5], 2),
#'     y = c(1, 3, 6, 4, 2, 5, 7, 8, 9, 10),
#'     group = rep(c("G1", "G2"), each = 5)
#' )
#'
#' # Grouped radar with raw values
#' RadarPlot(data, x = "x", y = "y", scale_y = "none", group_by = "group")
#'
#' # Faceted radar
#' RadarPlot(data, x = "x", y = "y", facet_by = "group")
#'
#' # Split into separate sub-plots
#' RadarPlot(data, x = "x", y = "y", split_by = "group")
#'
#' # Per-split palettes
#' RadarPlot(data, x = "x", y = "y", split_by = "group",
#'           palette = c(G1 = "Set1", G2 = "Paired"))
#' }
SpiderPlot <- function(
    data,
    x,
    x_sep = "_",
    group_by = NULL,
    group_by_sep = "_",
    y = NULL,
    group_name = NULL,
    groups = NULL,
    scale_y = c("group", "global", "x", "none"),
    y_min = 0,
    y_max = NULL,
    y_nbreaks = 4,
    bg_color = "grey80",
    bg_alpha = 0.1,
    fill = TRUE,
    linewidth = 1,
    pt_size = 4,
    max_charwidth = 16,
    split_by = NULL,
    split_by_sep = "_",
    theme = "theme_this",
    theme_args = list(),
    palette = "Paired",
    palcolor = NULL,
    palreverse = FALSE,
    facet_by = NULL,
    facet_scales = "fixed",
    facet_ncol = NULL,
    facet_nrow = NULL,
    facet_byrow = TRUE,
    alpha = 0.2,
    aspect.ratio = 1,
    legend.position = waiver(),
    legend.direction = "vertical",
    keep_na = FALSE,
    keep_empty = FALSE,
    title = NULL,
    subtitle = NULL,
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
    keep_na <- check_keep_na(keep_na, c(x, group_by, facet_by, split_by))
    keep_empty <- check_keep_empty(
        keep_empty,
        c(x, group_by, facet_by, split_by)
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
            RadarPlotAtomic(
                datas[[nm]],
                x = x,
                x_sep = x_sep,
                group_by = group_by,
                group_by_sep = group_by_sep,
                y = y,
                group_name = group_name,
                groups = groups,
                scale_y = scale_y,
                y_min = y_min,
                y_max = y_max,
                y_nbreaks = y_nbreaks,
                polygon = TRUE,
                bg_color = bg_color,
                bg_alpha = bg_alpha,
                fill = fill,
                linewidth = linewidth,
                pt_size = pt_size,
                max_charwidth = max_charwidth,
                theme = theme,
                theme_args = theme_args,
                palette = palette[[nm]],
                palcolor = palcolor[[nm]],
                palreverse = palreverse,
                facet_by = facet_by,
                facet_scales = facet_scales,
                facet_ncol = facet_ncol,
                facet_nrow = facet_nrow,
                facet_byrow = facet_byrow,
                alpha = alpha,
                aspect.ratio = aspect.ratio,
                legend.position = legend.position[[nm]],
                legend.direction = legend.direction[[nm]],
                keep_na = keep_na,
                keep_empty = keep_empty,
                title = title,
                subtitle = subtitle,
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
