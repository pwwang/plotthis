#' Atomic scatter plot (internal)
#'
#' @description
#' Core implementation for drawing a single scatter plot. This is the workhorse
#' behind the exported \code{\link{ScatterPlot}} function -- it takes a
#' **single** data frame (no \code{split_by} support) and returns a
#' \code{ggplot} object. The plot positions points by \code{x} and \code{y}
#' coordinates (both numeric), with optional size encoding via a third numeric
#' variable (\code{size_by}), and colour encoding (\code{color_by}) that can be
#' numeric (continuous gradient) or categorical (discrete palette).
#'
#' Key features:
#' \itemize{
#'   \item \strong{Variable point size} -- \code{size_by} accepts either a
#'   numeric constant (uniform dot size) or a column name (size scales with
#'   value via \code{scale_size_area()}).
#'   \item \strong{Colour modes} -- \code{color_by} can be numeric (rendered as
#'   a continuous gradient via \code{scale_fill_gradientn()} or
#'   \code{scale_color_gradientn()}) or factor/character (discrete palette
#'   via \code{scale_fill_manual()}/\code{scale_color_manual()}).
#'   \item \strong{Shape support} -- shapes 21--25 support separate fill and
#'   border (\code{color}) aesthetics; all other shapes use only the colour
#'   aesthetic. The \code{border_color} parameter can be a constant colour,
#'   \code{TRUE} (track the fill gradient), or omitted.
#'   \item \strong{Colour scale trimming} -- \code{lower_quantile} /
#'   \code{upper_quantile} (or explicit \code{lower_cutoff} /
#'   \code{upper_cutoff}) trim/clamp the continuous colour scale extremes.
#'   \item \strong{Axis transformation} -- \code{xtrans} and \code{ytrans}
#'   apply arbitrary scale transformations (e.g. \code{"log10"}, \code{"sqrt"})
#'   to the x and y axes via \code{scale_x_continuous()} /
#'   \code{scale_y_continuous()}.
#'   \item \strong{Point highlighting} -- \code{highlight} accepts indices,
#'   rownames, logical \code{TRUE}, or a string expression to select points
#'   that are overlaid with a distinct shape, size, colour, and alpha.
#' }
#'
#' @section Architecture:
#' \enumerate{
#'   \item \strong{ggplot dispatch} -- selects \code{gglogger::ggplot} or
#'   \code{ggplot2::ggplot} based on
#'   \code{getOption("plotthis.gglogger.enabled")}.
#'   \item \strong{Column resolution} -- \code{facet_by} is validated and
#'   forced to factor via \code{\link{check_columns}()}. \code{size_by} is
#'   validated when it is a column name; numeric constants are kept as-is.
#'   \code{color_by} is validated.
#'   \item \strong{Dummy colour guard} -- when \code{color_by = NULL}, a
#'   synthetic \code{.color_by} column (constant \code{""}) is created and the
#'   colour legend is suppressed.
#'   \item \strong{Categorical colour coercion} -- when \code{color_by} is a
#'   non-numeric column, it is forced to factor via
#'   \code{\link{check_columns}()} with \code{force_factor = TRUE}.
#'   \item \strong{Continuous colour scale preparation} -- when
#'   \code{color_by} is numeric,
#'   \code{\link{prepare_continuous_color_scale}()} computes quantile-trimmed or
#'   cutoff-clamped colour range endpoints (\code{feat_colors_value}) and
#'   winsorizes out-of-range data.
#'   \item \strong{Highlight resolution} -- \code{highlight} is processed into
#'   a subset data frame (\code{hidata}) via one of four paths:
#'   \code{TRUE} (all points), numeric index vector, a single string expression
#'   (parsed with \code{rlang::parse_expr()} and applied via
#'   \code{\link[dplyr]{filter}()}), or a character vector of rownames.
#'   An error is thrown if rownames are used on data without row names.
#'   \item \strong{Shape fill detection} -- determines whether the point shape
#'   (\code{shape}) supports a separate fill aesthetic (shapes 21--25).
#'   \item \strong{Base ggplot} -- initialises \code{ggplot(data, aes(x, y))}.
#'   \item \strong{Point layer configuration} -- the aesthetic mapping and
#'   constant aesthetics for \code{geom_point()} are assembled:
#'   \itemize{
#'     \item \emph{Fill vs colour}: shapes 21--25 map \code{fill} (and
#'     optionally \code{color} when \code{border_color = TRUE}); other shapes
#'     map \code{color}.
#'     \item \emph{Size}: a numeric \code{size_by} is passed as a constant
#'     aesthetic; a column name is mapped to the \code{size} aesthetic.
#'   }
#'   \item \strong{Size scale} -- when \code{size_by} is a column name,
#'   \code{scale_size_area(max_size = 6)} is added with a size legend
#'   (order = 1, title = \code{size_name \%||\% size_by}).
#'   \item \strong{Fill / colour scale (4 branches)}:
#'   \itemize{
#'     \item \emph{Shape with fill + numeric colour}: \code{scale_fill_gradientn()}
#'     with \code{feat_colors_value} rescaling and either a colour-bar legend
#'     or suppressed guide. When \code{border_color = TRUE}, a matching
#'     \code{scale_color_gradientn()} is added.
#'     \item \emph{Shape with fill + factor colour}: \code{scale_fill_manual()}
#'     with \code{\link{palette_this}()} colours and a discrete legend. When
#'     \code{border_color = TRUE}, a matching \code{scale_color_manual()} is
#'     added.
#'     \item \emph{Shape without fill + numeric colour}: \code{scale_color_gradientn()}
#'     with continuous colour-bar legend.
#'     \item \emph{Shape without fill + factor colour}: \code{scale_color_manual()}
#'     with discrete legend.
#'   }
#'   \item \strong{Highlight overlay} -- when \code{hidata} is non-\code{NULL},
#'   a second \code{geom_point()} layer is added using \code{highlight_shape},
#'   \code{highlight_color}, \code{highlight_size}, and \code{highlight_alpha}.
#'   The fill or colour aesthetic is chosen based on whether
#'   \code{highlight_shape} is 21--25.
#'   \item \strong{Axis scales} -- \code{scale_x_continuous(trans = xtrans)}
#'   and \code{scale_y_continuous(trans = ytrans)} apply the requested
#'   transformations.
#'   \item \strong{Labels and theme} -- \code{labs(title, subtitle, x, y)} sets
#'   plot annotations. \code{do_call(theme, theme_args)} applies the theme.
#'   \code{panel.grid.major} is set to grey80 dashed lines.
#'   \item \strong{Dimension calculation} -- \code{\link{calculate_plot_dimensions}()}
#'   computes plot \code{height} and \code{width} from \code{base_height = 5},
#'   \code{aspect.ratio}, and legend metrics (number of legend items and
#'   maximum label character width).
#'   \item \strong{Faceting} -- \code{\link{facet_plot}()} wraps the plot with
#'   \code{facet_wrap} / \code{facet_grid} if \code{facet_by} is provided.
#' }
#'
#' @inheritParams common_args
#' @param x A character string specifying the numeric column to use for the
#'  x-axis. A numeric column is expected.
#' @param y A character string specifying the numeric column to use for the
#'  y-axis. A numeric column is expected.
#' @param size_by Either a numeric constant (uniform dot size) or a character
#'  string naming a numeric column whose values control dot size via
#'  \code{scale_size_area(max_size = 6)}. Default: \code{2}.
#' @param size_name A character string for the size legend title. When
#'  \code{NULL} (default), the \code{size_by} column name is used. Ignored when
#'  \code{size_by} is a numeric constant.
#' @param color_by A character string naming a column whose values control
#'  dot colour. Can be numeric (continuous gradient via
#'  \code{scale_fill_gradientn()} / \code{scale_color_gradientn()}) or
#'  factor/character (discrete palette via
#'  \code{scale_fill_manual()} / \code{scale_color_manual()}). For shapes
#'  21--25, the colour is applied to the fill aesthetic. When \code{NULL}
#'  (default), all dots are rendered in a single colour derived from the
#'  palette.
#' @param color_name A character string for the colour legend title. When
#'  \code{NULL} (default), the \code{color_by} column name is used.
#' @param border_color Controls the point border colour. For shapes 21--25:
#'  \itemize{
#'    \item \code{"black"} (default) -- constant black border.
#'    \item A colour string (e.g. \code{"red"}, \code{"#FF0000"}) -- constant
#'    colour border.
#'    \item \code{TRUE} -- border colour tracks the \code{color_by} gradient
#'    / palette via \code{scale_color_gradientn()} /
#'    \code{scale_color_manual()}.
#'  }
#'  For shapes without a fill aesthetic (not 21--25), this parameter has no
#'  effect.
#' @param highlight Specifies which points to highlight with an overlaid
#'  \code{geom_point()} layer. Accepted values:
#'  \itemize{
#'    \item \code{NULL} (default) -- no highlighting.
#'    \item \code{TRUE} -- all points are highlighted.
#'    \item A numeric vector -- row indices of points to highlight.
#'    \item A single character string -- an R expression (e.g.
#'    \code{"x > 0"}) that is parsed with \code{rlang::parse_expr()} and
#'    evaluated via \code{\link[dplyr]{filter}()} to select rows.
#'    \item A character vector -- rownames of points to highlight. An error
#'    is thrown if the data has no rownames.
#'  }
#' @param highlight_shape A numeric value specifying the point shape for
#'  highlighted points. Default: \code{16} (filled circle). Shapes 21--25
#'  use the \code{fill} aesthetic; other shapes use \code{color}.
#' @param highlight_size A numeric value specifying the size of highlighted
#'  points. Default: \code{3}.
#' @param highlight_color A character string specifying the colour of
#'  highlighted points. Default: \code{"red"}.
#' @param highlight_alpha A numeric value in \code{[0, 1]} specifying the
#'  transparency of highlighted points. Default: \code{1}.
#' @param shape A numeric value specifying the point shape. Default: \code{21}
#'  (filled circle with border). Shapes 21--25 support separate fill and border
#'  colour aesthetics; all other shapes use a single colour aesthetic.
#' @param xtrans A character string specifying the transformation for the
#'  x-axis, passed to \code{scale_x_continuous(trans = ...)}. Common options:
#'  \code{"identity"} (default), \code{"log10"}, \code{"log2"},
#'  \code{"sqrt"}, \code{"reverse"}. See \code{ggplot2::continuous_scale()}
#'  for a full list.
#' @param ytrans A character string specifying the transformation for the
#'  y-axis, passed to \code{scale_y_continuous(trans = ...)}. Common options:
#'  \code{"identity"} (default), \code{"log10"}, \code{"log2"},
#'  \code{"sqrt"}, \code{"reverse"}.
#' @return A \code{ggplot} object with \code{height} and \code{width}
#'  attributes (in inches) attached.
#' @keywords internal
#' @importFrom utils getFromNamespace
#' @importFrom dplyr filter
#' @importFrom ggplot2 aes geom_point scale_size_area scale_fill_gradientn scale_color_gradientn labs
#' @importFrom ggplot2 guide_colorbar guide_legend guides guide_none scale_size waiver
#' @importFrom scales rescale
ScatterPlotAtomic <- function(
    data,
    x,
    y,
    size_by = 2,
    size_name = NULL,
    color_by = NULL,
    color_name = NULL,
    palreverse = FALSE,
    theme = "theme_this",
    theme_args = list(),
    alpha = ifelse(shape %in% 21:25, 0.65, 1),
    shape = 21,
    border_color = "black",
    lower_quantile = 0,
    upper_quantile = 0.99,
    lower_cutoff = NULL,
    upper_cutoff = NULL,
    xtrans = "identity",
    ytrans = "identity",
    highlight = NULL,
    highlight_shape = 16,
    highlight_size = 3,
    highlight_color = "red",
    highlight_alpha = 1,
    palette = ifelse(
        !is.null(color_by) && !is.numeric(data[[color_by]]),
        "Paired",
        "Spectral"
    ),
    palcolor = NULL,
    facet_by = NULL,
    facet_scales = "fixed",
    facet_ncol = NULL,
    facet_nrow = NULL,
    facet_byrow = TRUE,
    aspect.ratio = 1,
    legend.position = "right",
    legend.direction = "vertical",
    title = NULL,
    subtitle = NULL,
    xlab = NULL,
    ylab = NULL,
    ...
) {
    ggplot <- if (getOption("plotthis.gglogger.enabled", FALSE)) {
        gglogger::ggplot
    } else {
        ggplot2::ggplot
    }
    facet_by <- check_columns(
        data,
        facet_by,
        force_factor = TRUE,
        allow_multi = TRUE
    )
    if (!is.numeric(size_by)) {
        size_by <- check_columns(data, size_by)
    }
    color_by <- check_columns(data, color_by)
    if (is.null(color_by)) {
        data$.color_by <- ""
        color_by <- ".color_by"
        color_legend <- FALSE
    } else {
        color_legend <- TRUE
    }
    if (!is.numeric(data[[color_by]])) {
        color_by <- check_columns(data, color_by, force_factor = TRUE)
    }

    feat_colors_value <- NULL
    if (!is.null(color_by) && is.numeric(data[[color_by]])) {
        result <- prepare_continuous_color_scale(
            data,
            color_by,
            lower_quantile = lower_quantile,
            upper_quantile = upper_quantile,
            lower_cutoff = lower_cutoff,
            upper_cutoff = upper_cutoff
        )
        data <- result$data
        feat_colors_value <- result$feat_colors_value
    }

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

    shape_has_fill <- shape %in% 21:25
    hishape_has_fill <- highlight_shape %in% 21:25

    p <- ggplot(data, aes(x = !!sym(x), y = !!sym(y)))
    mapping <- list()
    point_layer_args <- list(shape = shape)
    if (shape_has_fill) {
        mapping[[length(mapping) + 1]] <- aes(fill = !!sym(color_by))
        if (isTRUE(border_color)) {
            mapping[[length(mapping) + 1]] <- aes(color = !!sym(color_by))
        } else {
            point_layer_args$color <- border_color
        }
    } else {
        mapping[[length(mapping) + 1]] <- aes(color = !!sym(color_by))
    }
    if (is.numeric(size_by)) {
        point_layer_args$size <- size_by
    } else {
        mapping[[length(mapping) + 1]] <- aes(size = !!sym(size_by))
    }
    modify_list <- getFromNamespace("modify_list", "ggplot2")
    point_layer_args$mapping <- Reduce(modify_list, mapping)
    point_layer <- do_call(geom_point, point_layer_args)

    p <- p + point_layer
    if (!is.numeric(size_by)) {
        p <- p +
            scale_size_area(max_size = 6, n.breaks = 4) +
            guides(
                size = guide_legend(
                    title = size_name %||% size_by,
                    override.aes = list(fill = "grey30", shape = 21),
                    order = 1
                )
            )
    }
    if (shape_has_fill) {
        if (is.numeric(data[[color_by]])) {
            p <- p +
                scale_fill_gradientn(
                    n.breaks = 5,
                    colors = palette_this(
                        data[[color_by]],
                        palette = palette,
                        palcolor = palcolor,
                        reverse = palreverse,
                        alpha = alpha
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
                    guide = if (isTRUE(border_color) || isFALSE(color_legend)) {
                        # legend for border color will be added later
                        # so we don't need to show the legend for fill color
                        guide_none()
                    } else {
                        guide_colorbar(
                            title = color_name %||% color_by,
                            frame.colour = "black",
                            ticks.colour = "black",
                            title.hjust = 0,
                            order = 2
                        )
                    }
                )
            if (isTRUE(border_color)) {
                p <- p +
                    scale_color_gradientn(
                        n.breaks = 5,
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
                        guide = if (
                            isTRUE(border_color) && isTRUE(color_legend)
                        ) {
                            guide_colorbar(
                                title = color_name %||% color_by,
                                frame.colour = "black",
                                ticks.colour = "black",
                                title.hjust = 0,
                                order = 2
                            )
                        } else {
                            guide_none()
                        }
                    )
            }
        } else {
            # factor/character
            p <- p +
                scale_fill_manual(
                    values = palette_this(
                        levels(data[[color_by]]),
                        palette = palette,
                        palcolor = palcolor,
                        reverse = palreverse,
                        alpha = alpha
                    ),
                    na.value = "grey80",
                    guide = if (isTRUE(border_color) || isFALSE(color_legend)) {
                        guide_none()
                    } else {
                        guide_legend(
                            title = color_name %||% color_by,
                            override.aes = list(size = 4, alpha = 1),
                            order = 3
                        )
                    }
                )
            if (isTRUE(border_color)) {
                p <- p +
                    scale_color_manual(
                        values = palette_this(
                            levels(data[[color_by]]),
                            palette = palette,
                            palcolor = palcolor,
                            reverse = palreverse
                        ),
                        na.value = "black",
                        guide = if (
                            isTRUE(border_color) && isTRUE(color_legend)
                        ) {
                            guide_legend(
                                title = color_name %||% color_by,
                                order = 3
                            )
                        } else {
                            guide_none()
                        }
                    )
            }
        }
    } else {
        # shape has no fill
        if (is.numeric(data[[color_by]])) {
            p <- p +
                scale_color_gradientn(
                    n.breaks = 5,
                    colors = palette_this(
                        data[[color_by]],
                        palette = palette,
                        palcolor = palcolor,
                        reverse = palreverse,
                        alpha = alpha
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
                    guide = if (isTRUE(color_legend)) {
                        guide_colorbar(
                            title = color_name %||% color_by,
                            frame.colour = "black",
                            ticks.colour = "black",
                            title.hjust = 0,
                            order = 2
                        )
                    } else {
                        guide_none()
                    }
                )
        } else {
            # factor/character
            p <- p +
                scale_color_manual(
                    values = palette_this(
                        levels(data[[color_by]]),
                        palette = palette,
                        palcolor = palcolor,
                        reverse = palreverse,
                        alpha = alpha
                    ),
                    na.value = "grey80",
                    guide = if (isTRUE(color_legend)) {
                        guide_legend(
                            title = color_name %||% color_by,
                            override.aes = list(size = 4, alpha = 1),
                            order = 3
                        )
                    } else {
                        guide_none()
                    }
                )
        }
    }

    if (!is.null(hidata)) {
        if (hishape_has_fill) {
            p <- p +
                geom_point(
                    data = hidata,
                    shape = highlight_shape,
                    fill = highlight_color,
                    color = "transparent",
                    size = highlight_size,
                    alpha = highlight_alpha
                )
        } else {
            p <- p +
                geom_point(
                    data = hidata,
                    shape = highlight_shape,
                    color = highlight_color,
                    size = highlight_size,
                    alpha = highlight_alpha
                )
        }
    }

    p <- p +
        scale_x_continuous(trans = xtrans) +
        scale_y_continuous(trans = ytrans) +
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
            panel.grid.major = element_line(colour = "grey80", linetype = 2)
        )

    dims <- calculate_plot_dimensions(
        base_height = 5,
        aspect.ratio = aspect.ratio,
        legend.position = legend.position,
        legend.direction = legend.direction,
        legend_n = if (!is.null(color_by) && is.factor(data[[color_by]])) {
            nlevels(data[[color_by]])
        } else {
            1
        },
        legend_nchar = if (!is.null(color_by) && is.factor(data[[color_by]])) {
            max(nchar(levels(data[[color_by]])))
        } else {
            5
        }
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
        legend.direction = legend.direction
    )
}


#' Scatter Plot
#'
#' @description
#' Draws a scatter plot with optional size encoding, colour encoding
#' (continuous gradient or discrete palette), point highlighting, and axis
#' transformations. This is the user-facing wrapper around
#' \code{\link{ScatterPlotAtomic}} that adds \code{split_by} support
#' (generating separate sub-plots per group) and combines them via
#' \code{patchwork}.
#'
#' Key features:
#' \itemize{
#'   \item \strong{Variable point size} -- \code{size_by} accepts either a
#'   numeric constant or a column name.
#'   \item \strong{Colour modes} -- numeric \code{color_by} produces a
#'   continuous gradient; factor/character \code{color_by} produces a discrete
#'   palette.
#'   \item \strong{Colour scale trimming} -- \code{lower_quantile} /
#'   \code{upper_quantile} (or explicit \code{lower_cutoff} /
#'   \code{upper_cutoff}) trim/clamp continuous colour scale extremes.
#'   \item \strong{Border modes} -- \code{border_color} can be a constant
#'   colour, \code{TRUE} (track the fill gradient), or omitted.
#'   \item \strong{Point highlighting} -- \code{highlight} accepts indices,
#'   rownames, logical \code{TRUE}, or a string expression.
#'   \item \strong{Axis transformation} -- \code{xtrans} / \code{ytrans}
#'   support log, sqrt, and other scale transformations.
#'   \item \strong{Split sub-plots} -- \code{split_by} produces one scatter
#'   plot per group level, combined into a single \code{patchwork} layout.
#' }
#'
#' @section split_by Workflow:
#' When \code{split_by} is provided:
#' \enumerate{
#'   \item \strong{Seed validation} -- \code{\link{validate_common_args}()}
#'   sets the random seed for reproducibility.
#'   \item \strong{Theme resolution} -- \code{\link{process_theme}()} resolves
#'   the \code{theme} string or function.
#'   \item \strong{Split column resolution} -- \code{\link{check_columns}()}
#'   validates \code{split_by} (force_factor, allow_multi, concat_multi).
#'   \item \strong{Data splitting} -- unused factor levels are dropped and the
#'   data is split into a named list (preserving factor level order). When
#'   \code{split_by = NULL}, a single-element list named \code{"..."} is used.
#'   \item \strong{Per-split palette / colour} -- \code{\link{check_palette}()}
#'   and \code{\link{check_palcolor}()} resolve per-split palette and colour
#'   overrides.
#'   \item \strong{Per-split legend} -- \code{\link{check_legend}()} resolves
#'   \code{legend.position} and \code{legend.direction} per split.
#'   \item \strong{Per-split title} -- when \code{title} is a function, it
#'   receives the default title (the split level name) and can return a custom
#'   string; otherwise \code{title \%||\% split_level} is used.
#'   \item \strong{Dispatch} -- each split subset is passed to
#'   \code{\link{ScatterPlotAtomic}()}.
#'   \item \strong{Combination} -- \code{\link{combine_plots}()} assembles the
#'   list of plots via \code{patchwork::wrap_plots}, honouring
#'   \code{nrow}/\code{ncol}/\code{byrow}/\code{design}.
#' }
#'
#' @inheritParams common_args
#' @inheritParams ScatterPlotAtomic
#' @param split_by The column(s) to split data by and generate separate scatter
#'   plots for each level. The split column is processed before splitting;
#'   multiple columns are concatenated with \code{split_by_sep}.
#' @param split_by_sep A character string used to concatenate multiple
#'   \code{split_by} column values. Default: \code{"_"}.
#' @param seed The random seed for reproducibility. Passed to
#'   \code{\link{validate_common_args}()}. Default: \code{8525}.
#' @param combine A logical value. If \code{TRUE} (the default), the list of
#'   per-split plots is combined into a single \code{patchwork} object via
#'   \code{\link{combine_plots}()}. If \code{FALSE}, returns the raw list of
#'   \code{ggplot} objects.
#' @param nrow,ncol,byrow Integers controlling the layout of combined plots via
#'   \code{\link{combine_plots}()}. \code{byrow = TRUE} (default) fills the
#'   layout row-wise.
#' @param axes,axis_titles Strings controlling how axes and axis titles are
#'   handled across combined plots. Passed to \code{\link{combine_plots}()}.
#'   See \code{?patchwork::wrap_plots} for options (\code{"keep"},
#'   \code{"collect"}, \code{"collect_x"}, \code{"collect_y"}).
#' @param guides A string controlling guide collection across combined plots.
#'   Passed to \code{\link{combine_plots}()}.
#' @param design A custom layout specification for combined plots. Passed to
#'   \code{\link{combine_plots}()}. When specified, \code{nrow}, \code{ncol},
#'   and \code{byrow} are ignored.
#' @return A \code{ggplot} object (single plot), a \code{patchwork} object
#'   (when \code{combine = TRUE} with \code{split_by}), or a named list of
#'   \code{ggplot} objects (when \code{combine = FALSE}), each with
#'   \code{height} and \code{width} attributes in inches.
#' @export
#' @examples
#' set.seed(8525)
#'
#' data <- data.frame(
#'    x = rnorm(20),
#'    y = rnorm(20),
#'    w = abs(rnorm(20)),
#'    t = sample(c("A", "B"), 20, replace = TRUE)
#' )
#'
#' # --- Basic scatter plot ---
#' ScatterPlot(data, x = "x", y = "y")
#'
#' # --- Highlight points ---
#' ScatterPlot(data, x = "x", y = "y", highlight = 'x > 0')
#'
#' # --- Size encoding (column name) ---
#' ScatterPlot(data, x = "x", y = "y", size_by = "w")
#'
#' # --- Colour encoding (numeric gradient) ---
#' ScatterPlot(data, x = "x", y = "y", color_by = "w")
#'
#' # --- Colour encoding (categorical) with border ---
#' ScatterPlot(data, x = "x", y = "y", size_by = "w", color_by = "t",
#'  border_color = "red")
#'
#' # --- Border colour tracks fill gradient ---
#' ScatterPlot(data, x = "x", y = "y", size_by = "w", color_by = "t",
#'  border_color = TRUE)
#'
#' # --- Shape without fill (single colour aesthetic) ---
#' ScatterPlot(data, x = "x", y = "y", size_by = "w", color_by = "t",
#'  shape = 1, palette = "Set1")
#'
#' # --- split_by with per-split palcolor ---
#' ScatterPlot(data, x = "x", y = "y", split_by = "t",
#'             palcolor = list(A = "blue", B = "red"))
#'
#' # --- Colour scale limits (quantile-based) ---
#' ScatterPlot(data, x = "x", y = "y", color_by = "w",
#'             lower_quantile = 0.1, upper_quantile = 0.9)
#'
#' # --- Colour scale limits (explicit cutoffs) ---
#' ScatterPlot(data, x = "x", y = "y", color_by = "w",
#'             lower_cutoff = 0, upper_cutoff = 1)
ScatterPlot <- function(
    data,
    x,
    y,
    size_by = 2,
    size_name = NULL,
    color_by = NULL,
    color_name = NULL,
    lower_quantile = 0,
    upper_quantile = 0.99,
    lower_cutoff = NULL,
    upper_cutoff = NULL,
    palreverse = FALSE,
    split_by = NULL,
    split_by_sep = "_",
    shape = 21,
    alpha = ifelse(shape %in% 21:25, 0.65, 1),
    border_color = "black",
    highlight = NULL,
    highlight_shape = 16,
    highlight_size = 3,
    highlight_color = "red",
    highlight_alpha = 1,
    theme = "theme_this",
    theme_args = list(),
    palette = ifelse(
        !is.null(color_by) && !is.numeric(data[[color_by]]),
        "Paired",
        "Spectral"
    ),
    palcolor = NULL,
    facet_by = NULL,
    facet_scales = "fixed",
    facet_ncol = NULL,
    facet_nrow = NULL,
    facet_byrow = TRUE,
    aspect.ratio = 1,
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
        data[[split_by]] <- droplevels(data[[split_by]])
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
            ScatterPlotAtomic(
                datas[[nm]],
                x = x,
                y = y,
                size_by = size_by,
                size_name = size_name,
                color_by = color_by,
                highlight = highlight,
                highlight_shape = highlight_shape,
                highlight_size = highlight_size,
                highlight_color = highlight_color,
                highlight_alpha = highlight_alpha,
                color_name = color_name,
                lower_quantile = lower_quantile,
                upper_quantile = upper_quantile,
                lower_cutoff = lower_cutoff,
                upper_cutoff = upper_cutoff,
                palreverse = palreverse,
                theme = theme,
                theme_args = theme_args,
                alpha = alpha,
                shape = shape,
                border_color = border_color,
                palette = palette[[nm]],
                palcolor = palcolor[[nm]],
                facet_by = facet_by,
                facet_scales = facet_scales,
                facet_ncol = facet_ncol,
                facet_nrow = facet_nrow,
                facet_byrow = facet_byrow,
                aspect.ratio = aspect.ratio,
                legend.position = legend.position[[nm]],
                legend.direction = legend.direction[[nm]],
                title = title,
                subtitle = subtitle,
                xlab = xlab,
                ylab = ylab,
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
