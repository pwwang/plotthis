#' Atomic volcano plot (internal)
#'
#' @description
#' Core implementation for drawing a single volcano plot. This is the
#' workhorse behind the exported \code{\link{VolcanoPlot}} function — it takes
#' a **single** data frame (no \code{split_by} support) and returns a
#' \code{ggplot} object. The plot displays statistical significance (typically
#' -log10 adjusted p-value) on the y-axis versus magnitude of change (log2
#' fold change) on the x-axis, with points coloured by significance category
#' or a user-supplied variable. Top features can be automatically labelled
#' via \code{\link[ggrepel]{geom_text_repel}()}, and specific points can be
#' highlighted.
#'
#' The function categorises points into three groups based on cutoff
#' thresholds: \code{"sig_pos_x"} (points exceeding both the positive
#' x-cutoff and y-cutoff), \code{"sig_neg_x"} (points exceeding both the
#' negative x-cutoff and y-cutoff), and \code{"insig"} (all remaining
#' points). When \code{color_by = NULL}, this categorisation drives point
#' colouring; otherwise the supplied column controls the colour scale.
#'
#' @section Architecture:
#' \enumerate{
#'   \item \strong{ggplot dispatch} — selects \code{gglogger::ggplot} or
#'         \code{ggplot2::ggplot} based on
#'         \code{getOption("plotthis.gglogger.enabled")}.
#'   \item \strong{Input validation} — \code{trim} (must be length-2 in
#'         \code{[0, 1]}) and \code{xlim} (must be length-2 or \code{NULL})
#'         are validated via \code{stopifnot()}. \code{trim} is sorted.
#'   \item \strong{Column resolution} — \code{x}, \code{y}, \code{color_by},
#'         \code{facet_by}, and \code{label_by} are validated and transformed
#'         via \code{\link{check_columns}}. Multi-column \code{facet_by} is
#'         concatenated with \code{force_factor = TRUE}.
#'   \item \strong{y-axis transformation} — the y-column is transformed by
#'         \code{ytrans()} (default: \code{-log10(n)}). The
#'         \code{y_cutoff} value is also transformed.
#'   \item \strong{x_cutoff defaulting} — if \code{x_cutoff} is
#'         \code{NULL}, it is set to \code{0} (suppressing the x-cutoff
#'         legend line).
#'   \item \strong{Category assignment} — a \code{.category} factor with
#'         levels \code{c("sig_neg_x", "insig", "sig_pos_x")} is created:
#'         \itemize{
#'           \item When \code{y_cutoff} is non-\code{NULL}: points with
#'           \code{|x| > x_cutoff} AND \code{y > y_cutoff} are significant.
#'           \item When \code{y_cutoff} is \code{NULL}: only the x-cutoff
#'           determines significance.
#'         }
#'   \item \strong{Color resolution} — three cases:
#'         \itemize{
#'           \item \code{color_by = NULL}: uses \code{.category} as a
#'           discrete colour column; the legend is suppressed.
#'           \item Character/factor column: discrete colour scale via
#'           \code{\link{palette_this}()} and
#'           \code{scale_color_manual()}, guide suppressed.
#'           \item Numeric column: continuous gradient via
#'           \code{scale_color_gradientn()} with a framed colour-bar legend.
#'         }
#'   \item \strong{Flip negatives} — when \code{flip_negatives = TRUE},
#'         the y-values of points with negative x are multiplied by -1,
#'         creating a mirrored volcano where both up- and down-regulated
#'         features show their significance on the same side of the y-axis.
#'   \item \strong{Label column} — \code{.label} is populated from
#'         \code{label_by} or \code{rownames(data)}.
#'   \item \strong{Trim / winsorize} — x-values beyond the trim quantile
#'         bounds are clamped. When both bounds are nonzero and of opposite
#'         sign, they are symmetrised to the smaller absolute value.
#'         Outlying points are marked in \code{.outlier}.
#'   \item \strong{Label selection} — two modes:
#'         \itemize{
#'           \item Explicit \code{labels}: the specified rows (by name or
#'           index) are marked for labelling.
#'           \item Automatic: top \code{nlabel} points (by Euclidean distance
#'           to origin) are selected per \code{sign(x)} group, and per facet
#'           level if \code{facet_by} is set.
#'         }
#'         All labels are filtered to exclude \code{"insig"} points.
#'   \item \strong{Data split} — data is split into \code{pos_data}
#'         (\code{x >= 0}) and \code{neg_data} (\code{x < 0}) so that
#'         \code{ggrepel} labels can nudge in opposite directions (positive
#'         points nudge left, negative points nudge right).
#'   \item \strong{Outlier jitter} — outlier points are rendered separately
#'         with \code{position_jitter()} to reduce overplotting.
#'   \item \strong{Base ggplot} — \code{geom_point()} layers for positive,
#'         negative, and outlier data, with colour mapped to
#'         \code{color_by}.
#'   \item \strong{Colour scale} — discrete:
#'         \code{scale_color_manual()} with \code{"insig"} forced to
#'         \code{"grey"} (when \code{palcolor} is \code{NULL}); continuous:
#'         \code{scale_color_gradientn()} with palette re-scaled so that
#'         the colour-bar is centred at 0.
#'   \item \strong{Highlight} — when \code{highlight} is provided, two
#'         additional \code{geom_point()} layers (non-outliers and outliers
#'         with jitter) overlay the highlighted points in
#'         \code{highlight_color}.
#'   \item \strong{x-cutoff lines} — vertical dashed lines at
#'         \code{+/- x_cutoff} via \code{geom_vline()} with
#'         \code{\link[ggnewscale]{new_scale_color}()}, labelled by
#'         \code{x_cutoff_name}. Suppressed when \code{x_cutoff} is
#'         \code{NULL} or \code{0}.
#'   \item \strong{y-cutoff lines} — horizontal dashed line(s) at
#'         \code{y_cutoff} (or \code{+/- y_cutoff} when
#'         \code{flip_negatives = TRUE}) via \code{geom_hline()} with
#'         \code{\link[ggnewscale]{new_scale_color}()}, labelled by
#'         \code{y_cutoff_name}.
#'   \item \strong{Flip-negatives axis} — when \code{flip_negatives = TRUE},
#'         a solid \code{geom_hline(yintercept = 0)} is added and
#'         \code{scale_y_continuous(labels = abs)} formats the y-axis.
#'   \item \strong{x-axis limits} — optional \code{xlim} passed to
#'         \code{ggplot2::xlim()}.
#'   \item \strong{Reference line and labels} — a grey80 dashed vertical
#'         line at \code{x = 0}, followed by \code{geom_text_repel()} for
#'         positive and negative labelled points with separate x-nudges.
#'   \item \strong{Labels and theme} — \code{labs()},
#'         \code{coord_cartesian(clip = "off")},
#'         \code{do_call(theme, theme_args)}, and theme elements for
#'         \code{aspect.ratio}, \code{legend.position}, and
#'         \code{legend.direction}.
#'   \item \strong{Dimension calculation} —
#'         \code{\link{calculate_plot_dimensions}()} computes \code{height}
#'         and \code{width} attributes from \code{base_height = 5},
#'         \code{aspect.ratio}, and legend geometry.
#'   \item \strong{Faceting} — \code{\link{facet_plot}()} wraps the plot
#'         with \code{facet_wrap} / \code{facet_grid} if \code{facet_by} is
#'         provided.
#' }
#'
#' @inheritParams common_args
#' @param ytrans A function to transform the y-axis values before plotting.
#'  The default \code{function(n) -log10(n)} converts p-values to a -log10
#'  scale. The transformed values are used for both the y-axis and cutoff
#'  comparisons.
#' @param color_by A character string specifying the column name to colour
#'  the points by. When \code{NULL} (default), points are automatically
#'  categorised as \code{"sig_pos_x"}, \code{"sig_neg_x"}, or
#'  \code{"insig"} based on \code{x_cutoff} and \code{y_cutoff}, and the
#'  colour legend is suppressed. When a column name is provided, the colour
#'  mapping follows the column type — discrete (character/factor) uses
#'  \code{scale_color_manual()} with the specified \code{palette};
#'  numeric (continuous) uses \code{scale_color_gradientn()}.
#' @param color_name A character string for the colour legend title when
#'  \code{color_by} is a numeric column. When \code{NULL} (default), the
#'  \code{color_by} column name is used.
#' @param flip_negatives A logical value. When \code{TRUE}, y-values of
#'  points with negative x-values are multiplied by -1, creating a mirrored
#'  volcano plot where both up- and down-regulated features show their
#'  significance on the same side of the y-axis. A horizontal line at
#'  \code{y = 0} and absolute-value axis labels are added. Default:
#'  \code{FALSE}.
#' @param trim A numeric vector of length 2 specifying quantile bounds for
#'  winsorizing the x-axis values. Values below the first quantile are
#'  clamped to that quantile; values above the second quantile are clamped
#'  to that quantile. Both values must be in \code{[0, 1]}. When both bounds
#'  are nonzero and of opposite sign, they are symmetrised to the smaller
#'  absolute value. Default: \code{c(0, 1)} (no trimming).
#' @param xlim A numeric vector of length 2 to set the x-axis limits.
#'  Passed to \code{\link[ggplot2]{xlim}()}. When \code{NULL} (default),
#'  limits are determined automatically from the data.
#' @param x_cutoff A numeric value specifying the x-axis significance
#'  cutoff. Both the negative and positive of this value are used as
#'  vertical threshold lines. When \code{NULL} or \code{0}, no x-cutoff
#'  line is drawn. Default: \code{NULL}.
#' @param y_cutoff A numeric value specifying the y-axis significance
#'  cutoff in the **original** (untransformed) scale. The value is
#'  transformed by \code{ytrans} before plotting. When \code{NULL}, no
#'  y-cutoff line is drawn and the category assignment uses only the
#'  x-cutoff. Default: \code{0.05}.
#' @param x_cutoff_name A character string for the x-cutoff legend entry.
#'  When \code{"none"}, the legend for the x-cutoff line is suppressed
#'  entirely (the line is still drawn). When \code{NULL} (default), a
#'  label of the form \code{"<x> = +/-<value>"} is generated.
#' @param y_cutoff_name A character string for the y-cutoff legend entry.
#'  When \code{"none"}, the legend for the y-cutoff line is suppressed
#'  entirely (the line is still drawn). When \code{NULL} (default), a
#'  label of the form \code{"<ylab> = <value>"} is generated.
#' @param x_cutoff_color A character string specifying the colour of the
#'  x-axis cutoff line(s). Default: \code{"red2"}.
#' @param y_cutoff_color A character string specifying the colour of the
#'  y-axis cutoff line(s). Default: \code{"blue2"}.
#' @param x_cutoff_linetype A character string specifying the linetype of
#'  the x-axis cutoff line(s). Default: \code{"dashed"}.
#' @param y_cutoff_linetype A character string specifying the linetype of
#'  the y-axis cutoff line(s). Default: \code{"dashed"}.
#' @param x_cutoff_linewidth A numeric value specifying the linewidth of
#'  the x-axis cutoff line(s). Default: \code{0.5}.
#' @param y_cutoff_linewidth A numeric value specifying the linewidth of
#'  the y-axis cutoff line(s). Default: \code{0.5}.
#' @param pt_size A numeric value specifying the point size for all data
#'  points. Default: \code{2}.
#' @param pt_alpha A numeric value in \code{[0, 1]} specifying the
#'  transparency of all data points. Default: \code{0.5}.
#' @param nlabel An integer specifying the number of top features to label
#'  automatically. Points are ranked by Euclidean distance to the origin
#'  within each \code{sign(x)} group (and per facet level if
#'  \code{facet_by} is set). Only non-insignificant points receive labels.
#'  Default: \code{5}.
#' @param labels A character vector of row names or integer indices
#'  specifying which points to label. Overrides automatic \code{nlabel}
#'  selection. When \code{NULL} (default), top \code{nlabel} points are
#'  chosen automatically.
#' @param label_by A character string specifying the column whose values
#'  are used as label text. When \code{NULL} (default), row names of the
#'  data frame are used.
#' @param label_size A numeric value specifying the font size of the
#'  labels. Default: \code{3}.
#' @param label_fg A character string specifying the text colour of the
#'  labels. Default: \code{"black"}.
#' @param label_bg A character string specifying the background colour of
#'  the label boxes (passed to \code{geom_text_repel(bg.color = ...)}).
#'  Default: \code{"white"}.
#' @param label_bg_r A numeric value specifying the corner radius of the
#'  label background boxes (passed to \code{geom_text_repel(bg.r = ...)}).
#'  Default: \code{0.1}.
#' @param highlight A character vector of row names or integer indices
#'  specifying which points to highlight with an overlaid point layer in
#'  \code{highlight_color}. When \code{NULL} (default), no highlighting is
#'  applied.
#' @param highlight_color A character string specifying the colour of the
#'  highlight points. Default: \code{"red"}.
#' @param highlight_size A numeric value specifying the point size of the
#'  highlight layer. Default: \code{2}.
#' @param highlight_alpha A numeric value in \code{[0, 1]} specifying the
#'  transparency of the highlight points. Default: \code{1}.
#' @param highlight_stroke A numeric value specifying the stroke width of
#'  the highlight point borders. Default: \code{0.5}.
#'
#' @return A \code{ggplot} object with \code{height} and \code{width}
#'  attributes (in inches) attached.
#' @keywords internal
#' @importFrom dplyr %>% case_when mutate arrange row_number group_by ungroup desc filter
VolcanoPlotAtomic <- function(
    data,
    x,
    y,
    ytrans = function(n) -log10(n),
    color_by = NULL,
    color_name = NULL,
    flip_negatives = FALSE,
    x_cutoff = NULL,
    y_cutoff = 0.05,
    trim = c(0, 1),
    xlim = NULL,
    x_cutoff_name = NULL,
    y_cutoff_name = NULL,
    x_cutoff_color = "red2",
    y_cutoff_color = "blue2",
    x_cutoff_linetype = "dashed",
    y_cutoff_linetype = "dashed",
    x_cutoff_linewidth = 0.5,
    y_cutoff_linewidth = 0.5,
    pt_size = 2,
    pt_alpha = 0.5,
    nlabel = 5,
    labels = NULL,
    label_by = NULL,
    label_size = 3,
    label_fg = "black",
    label_bg = "white",
    label_bg_r = 0.1,
    highlight = NULL,
    highlight_color = "red",
    highlight_size = 2,
    highlight_alpha = 1,
    highlight_stroke = 0.5,
    facet_by = NULL,
    facet_scales = "fixed",
    facet_ncol = NULL,
    facet_nrow = NULL,
    facet_byrow = TRUE,
    theme = "theme_this",
    theme_args = list(),
    palette = "Spectral",
    palcolor = NULL,
    palreverse = FALSE,
    title = NULL,
    subtitle = NULL,
    xlab = NULL,
    ylab = NULL,
    aspect.ratio = 1,
    legend.position = "right",
    legend.direction = "vertical",
    seed = 8525,
    ...
) {
    ggplot <- if (getOption("plotthis.gglogger.enabled", FALSE)) {
        gglogger::ggplot
    } else {
        ggplot2::ggplot
    }
    stopifnot(
        "[VolcanoPlot] 'trim' must be a numeric vector of length 2 and both values must be in the range [0, 1]." = length(
            trim
        ) ==
            2 &&
            all(trim >= 0 & trim <= 1)
    )
    stopifnot(
        "[VolcanoPlot] 'xlim' must be a numeric vector of length 2." = is.null(
            xlim
        ) ||
            length(xlim) == 2
    )
    trim <- sort(trim)
    x <- check_columns(data, x)
    y <- check_columns(data, y)
    color_by <- check_columns(data, color_by)
    facet_by <- check_columns(
        data,
        facet_by,
        force_factor = TRUE,
        allow_multi = TRUE
    )
    label_by <- check_columns(data, label_by)

    data[[y]] <- ytrans(data[[y]])
    x_cutoff <- x_cutoff %||% 0
    y_cutoff <- ytrans(y_cutoff)

    if (!is.null(y_cutoff)) {
        data <- data %>%
            mutate(
                .category = factor(
                    case_when(
                        !!sym(x) > x_cutoff & !!sym(y) > y_cutoff ~ "sig_pos_x",
                        !!sym(x) < -x_cutoff &
                            !!sym(y) > y_cutoff ~ "sig_neg_x",
                        TRUE ~ "insig"
                    ),
                    levels = c("sig_neg_x", "insig", "sig_pos_x")
                )
            )
    } else {
        data <- data %>%
            mutate(
                .category = factor(
                    case_when(
                        !!sym(x) > x_cutoff ~ "sig_pos_x",
                        !!sym(x) < -x_cutoff ~ "sig_neg_x",
                        TRUE ~ "insig"
                    ),
                    levels = c("sig_neg_x", "insig", "sig_pos_x")
                )
            )
    }

    if (is.null(color_by)) {
        color_by <- ".category"
        color_type <- "discrete"
    } else if (is.character(data[[color_by]]) || is.factor(data[[color_by]])) {
        color_type <- "discrete"
    } else {
        color_type <- "continuous"
    }

    if (flip_negatives) {
        data[data[[x]] < 0, y] <- -data[data[[x]] < 0, y]
    }
    data$.label <- if (is.null(label_by)) rownames(data) else data[[label_by]]
    data$.show_label <- FALSE

    x_upper <- quantile(data[[x]][is.finite(data[[x]])], c(trim[2], 1))
    x_lower <- quantile(data[[x]][is.finite(data[[x]])], c(trim[1], 0))
    x_upper <- ifelse(x_upper[1] > 0, x_upper[1], x_upper[2])
    x_lower <- ifelse(x_lower[1] < 0, x_lower[1], x_lower[2])
    if (x_upper > 0 & x_lower < 0) {
        value_range <- min(abs(c(x_upper, x_lower)), na.rm = TRUE)
        x_upper <- value_range
        x_lower <- -value_range
    }
    data$.outlier <- data[[x]] > x_upper | data[[x]] < x_lower
    data[[x]][data[[x]] > x_upper] <- x_upper
    data[[x]][data[[x]] < x_lower] <- x_lower

    if (!is.null(labels)) {
        data[labels, ".show_label"] <- TRUE
    } else {
        # calculate the distance to the origin
        data$.distance <- sqrt(data[[x]]^2 + data[[y]]^2)
        if (!is.null(facet_by)) {
            data <- data %>%
                group_by(!!!syms(facet_by), sign(data[[x]])) %>%
                arrange(desc(!!sym(".distance"))) %>%
                mutate(.show_label = row_number() <= nlabel) %>%
                ungroup()
        } else {
            data <- data %>%
                group_by(sign(data[[x]])) %>%
                arrange(desc(!!sym(".distance"))) %>%
                mutate(.show_label = row_number() <= nlabel) %>%
                ungroup()
        }
    }
    data <- data %>%
        mutate(
            .show_label = !!sym(".show_label") & !!sym(".category") != "insig"
        ) %>%
        as.data.frame()
    # rownames(data) <- data$.label

    # x_nudge is not an aesthetic, we can't specify it separately for negative and positive values
    # so we plot the points in two layers
    # one for negative values and one for positive values
    # and set the x_nudge for each layer
    pos_data <- data[data[[x]] >= 0, , drop = FALSE]
    neg_data <- data[data[[x]] < 0, , drop = FALSE]

    outlier_data <- data[data$.outlier, , drop = FALSE]

    # x_nudges will be the same for all facets
    pos_x_nudge <- -diff(range(pos_data[[x]])) * 0.05
    neg_x_nudge <- diff(range(neg_data[[x]])) * 0.05
    jitter <- position_jitter(width = 0.2, height = 0.2, seed = seed)

    p <- ggplot(
        mapping = aes(x = !!sym(x), y = !!sym(y), color = !!sym(color_by))
    ) +
        geom_point(data = pos_data, size = pt_size, alpha = pt_alpha) +
        geom_point(data = neg_data, size = pt_size, alpha = pt_alpha) +
        geom_point(
            data = outlier_data,
            size = pt_size,
            alpha = pt_alpha,
            position = jitter
        )

    if (color_type == "discrete") {
        colors <- palette_this(
            levels(data[[color_by]]),
            palette = palette,
            palcolor = palcolor,
            reverse = palreverse
        )
        if (is.null(palcolor)) {
            colors['insig'] <- "grey"
        }
        p <- p + scale_color_manual(values = colors, guide = "none")
    } else {
        p <- p +
            scale_color_gradientn(
                colors = palette_this(
                    palette = palette,
                    palcolor = palcolor,
                    reverse = palreverse
                ),
                values = scales::rescale(unique(c(
                    min(c(unlist(data[[color_by]]), 0), na.rm = TRUE),
                    0,
                    max(unlist(data[[color_by]]), na.rm = TRUE)
                ))),
                guide = guide_colorbar(
                    frame.colour = "black",
                    ticks.colour = "black",
                    title.hjust = 0,
                    order = 1
                )
            )
    }

    if (!is.null(highlight)) {
        p <- p +
            geom_point(
                data = data[highlight, , drop = FALSE] %>%
                    filter(!!sym(".outlier") == FALSE),
                color = highlight_color,
                size = highlight_size,
                alpha = highlight_alpha,
                stroke = highlight_stroke
            ) +
            geom_point(
                data = data[highlight, , drop = FALSE] %>%
                    filter(!!sym(".outlier")),
                color = highlight_color,
                size = highlight_size,
                alpha = highlight_alpha,
                stroke = highlight_stroke,
                position = jitter
            )
    }

    if (!is.null(x_cutoff) && x_cutoff != 0) {
        if (identical(x_cutoff_name, "none")) {
            guide <- guide_none()
        } else {
            guide <- guide_legend(
                override.aes = list(alpha = 0.8, size = 5),
                order = 2,
                theme = ggplot2::theme(legend.margin = margin(0, 0, -10, 4.5))
            )
        }
        vline_df <- data.frame(xintercept = c(-x_cutoff, x_cutoff))
        if (!is.null(facet_by)) {
            vline_df <- expand_grid(
                vline_df,
                data[, facet_by, drop = FALSE] %>% distinct()
            )
        }
        p <- p +
            new_scale_color() +
            geom_vline(
                data = vline_df,
                mapping = aes(
                    xintercept = !!sym("xintercept"),
                    color = x_cutoff_name %||%
                        paste0(
                            x,
                            " = +/-",
                            scales::number(x_cutoff, accuracy = 0.01)
                        )
                ),
                alpha = 0.4,
                linetype = x_cutoff_linetype,
                linewidth = x_cutoff_linewidth,
            ) +
            scale_color_manual(
                name = NULL,
                values = x_cutoff_color,
                guide = guide
            )
    }

    if (!is.null(y_cutoff)) {
        if (isTRUE(flip_negatives)) {
            yintercept <- c(-y_cutoff, y_cutoff)
        } else {
            yintercept <- y_cutoff
        }
        if (identical(y_cutoff_name, "none")) {
            guide <- guide_none()
        } else {
            guide <- guide_legend(
                override.aes = list(alpha = 0.8, size = 5),
                order = 3
            )
        }
        hline_df <- data.frame(yintercept = yintercept)
        if (!is.null(facet_by)) {
            hline_df <- expand_grid(
                hline_df,
                data[, facet_by, drop = FALSE] %>% distinct()
            )
        }
        p <- p +
            new_scale_color() +
            geom_hline(
                data = hline_df,
                mapping = aes(
                    yintercept = !!sym("yintercept"),
                    color = y_cutoff_name %||%
                        paste0(
                            ylab %||% y,
                            " = ",
                            scales::number(y_cutoff, accuracy = 0.01)
                        )
                ),
                alpha = 0.4,
                linetype = y_cutoff_linetype,
                linewidth = y_cutoff_linewidth
            ) +
            scale_color_manual(
                name = NULL,
                values = y_cutoff_color,
                guide = guide
            )
    }

    if (isTRUE(flip_negatives)) {
        p <- p +
            geom_hline(yintercept = 0, color = "black", linetype = 1) +
            scale_y_continuous(labels = abs)
    }
    if (!is.null(xlim)) {
        p <- p + ggplot2::xlim(xlim)
    }

    p <- p +
        geom_vline(xintercept = 0, color = "grey80", linetype = 2) +
        geom_text_repel(
            data = pos_data[pos_data$.show_label, , drop = FALSE],
            aes(label = !!sym(".label")),
            nudge_x = pos_x_nudge,
            color = label_fg,
            bg.color = label_bg,
            bg.r = label_bg_r,
            size = label_size,
            min.segment.length = 0,
            segment.color = "grey40",
            max.overlaps = 100
        ) +
        geom_text_repel(
            data = neg_data[neg_data$.show_label, , drop = FALSE],
            aes(label = !!sym(".label")),
            nudge_x = neg_x_nudge,
            color = label_fg,
            bg.color = label_bg,
            bg.r = label_bg_r,
            size = label_size,
            min.segment.length = 0,
            segment.color = "grey40",
            max.overlaps = 100
        ) +
        labs(
            title = title,
            subtitle = subtitle,
            x = xlab %||% x,
            y = ylab %||% y
        ) +
        coord_cartesian(clip = "off")

    p <- p +
        do_call(theme, theme_args) +
        ggplot2::theme(
            aspect.ratio = aspect.ratio,
            legend.position = legend.position,
            legend.direction = legend.direction
        )

    dims <- calculate_plot_dimensions(
        base_height = 5,
        aspect.ratio = aspect.ratio,
        legend.position = legend.position,
        legend.direction = legend.direction
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

#' Volcano plot
#'
#' @description
#' Produces a volcano plot — a scatter plot that displays statistical
#' significance (typically -log10 adjusted p-value) on the y-axis versus
#' magnitude of change (log2 fold change) on the x-axis. Points are coloured
#' automatically by significance category (\code{"sig_pos_x"},
#' \code{"sig_neg_x"}, \code{"insig"}) or by a user-supplied column. The
#' most significant features can be labelled automatically via
#' \code{\link[ggrepel]{geom_text_repel}()}, and specific points can be
#' highlighted.
#'
#' The function supports \strong{automatic labelling} of top features (by
#' distance to origin), \strong{mirrored layout} via
#' \code{flip_negatives}, \strong{x-axis trimming} to reduce the influence
#' of extreme values, \strong{faceting}, and \strong{splitting} into
#' separate sub-plots via \code{split_by} with per-split colour palette and
#' legend control.
#'
#' @section split_by Workflow:
#' When \code{split_by} is provided:
#' \enumerate{
#'   \item The \code{split_by} column(s) are validated via
#'         \code{\link{check_columns}()} with \code{force_factor = TRUE} and
#'         \code{concat_multi = TRUE} (multiple columns are concatenated
#'         with \code{split_by_sep}).
#'   \item The data frame is split by \code{split_by} (preserving factor
#'         level order). If \code{split_by} is \code{NULL}, the data is
#'         wrapped in a single-element list with name \code{"..."}.
#'   \item Per-split \code{palette}, \code{palcolor},
#'         \code{legend.position}, and \code{legend.direction} are resolved
#'         via \code{\link{check_palette}()}, \code{\link{check_palcolor}()},
#'         and \code{\link{check_legend}()}.
#'   \item \code{\link{VolcanoPlotAtomic}()} is called for each split. If
#'         \code{title} is a function, it receives the split level name and
#'         can generate dynamic titles.
#'   \item Results are combined via \code{\link{combine_plots}()} (when
#'         \code{combine = TRUE}) or returned as a named list.
#' }
#'
#' @inheritParams common_args
#' @inheritParams VolcanoPlotAtomic
#' @param split_by The column(s) to split the data by and produce separate
#'  sub-plots. Multiple columns are concatenated with \code{split_by_sep}.
#' @param split_by_sep A character string to separate concatenated
#'  \code{split_by} columns. Default \code{"_"}.
#' @param combine Logical; when \code{TRUE} (default), returns a combined
#'  \code{patchwork} object. When \code{FALSE}, returns a named list of
#'  individual \code{ggplot} objects.
#' @param ncol,nrow Integer number of columns / rows for the combined layout
#'  (passed to \code{\link[patchwork]{wrap_plots}}).
#' @param byrow Logical; fill the combined layout by row. Default
#'  \code{TRUE} (passed to \code{\link[patchwork]{wrap_plots}}).
#' @param axes A character string specifying how axes should be treated
#'  across the combined layout (passed to
#'  \code{\link[patchwork]{wrap_plots}}).
#' @param axis_titles A character string specifying how axis titles should
#'  be treated across the combined layout. Defaults to \code{axes}.
#' @param guides A character string specifying how guides (legends) should
#'  be collected across panels. Default \code{"collect"} (passed to
#'  \code{\link{combine_plots}()}).
#' @param design A custom layout design for the combined plot (passed to
#'  \code{\link{combine_plots}()}).
#' @return A \code{ggplot} object, a \code{patchwork} object, or a named
#'  list of \code{ggplot} objects (when \code{combine = FALSE}), each with
#'  \code{height} and \code{width} attributes in inches.
#' @export
#' @examples
#' \donttest{
#' set.seed(8525)
#' # Obtained by Seurat::FindMakers for the first cluster of pbmc_small
#' data <- data.frame(
#'    avg_log2FC = c(
#'      -3.69, -4.10, -2.68, -3.51, -3.09, -2.52, -3.53, -3.35, -2.82, -2.71, -3.16, -2.24,
#'      -5.62, -3.10, -3.42, -2.72, -3.23, -3.25, -4.68, 3.67, -2.66, 4.79, -2.99, 10.14,
#'      -1.78, -2.67, -2.26, -2.59, -3.39, 5.36, 4.56, 4.62, -2.94, -9.47, -9.12, -1.63,
#'      -2.77, 3.31, -1.53, -3.89, -4.21, 4.72, -2.98, -2.29, -1.41, -9.48, -4.30, 3.01,
#'      -1.19, -4.83, -1.35, -1.68, -1.63, -2.70, 3.86, 3.81, 7.23, -1.45, -0.92, -2.45,
#'      3.91, -4.45, -9.33, 3.56, 2.27, -1.60, -1.15, 11.40, -9.77, -8.32, 2.61, -1.25,
#'      -1.72, 10.61, 11.34, 10.02, 2.78, -3.48, -1.98, 5.86, 5.57, 4.57, 9.75, 9.97,
#'      10.90, 9.19, 2.93, 5.10, -1.52, -3.93, -1.95, -2.46, -0.64, 4.60, -1.82, -0.80,
#'      9.34, 7.51, 6.45, 5.23, 4.41, 3.60, -1.94, -1.15),
#'    p_val_adj = c(
#'      3.82e-09, 1.52e-07, 1.79e-07, 4.68e-07, 4.83e-07, 6.26e-07, 2.61e-06, 1.33e-05,
#'      1.79e-05, 3.71e-05, 5.21e-05, 5.36e-05, 5.83e-05, 6.66e-05, 8.22e-05, 2.89e-04,
#'      3.00e-04, 4.94e-04, 7.62e-04, 8.93e-04, 9.55e-04, 9.61e-04, 1.12e-03, 1.47e-03,
#'      1.66e-03, 1.95e-03, 2.06e-03, 3.01e-03, 3.26e-03, 4.35e-03, 4.85e-03, 5.12e-03,
#'      5.40e-03, 7.18e-03, 7.18e-03, 1.04e-02, 1.24e-02, 1.90e-02, 1.94e-02, 1.97e-02,
#'      2.09e-02, 2.13e-02, 2.25e-02, 2.61e-02, 3.18e-02, 3.27e-02, 3.69e-02, 3.80e-02,
#'      4.95e-02, 5.73e-02, 5.77e-02, 6.10e-02, 6.22e-02, 6.31e-02, 6.72e-02, 9.23e-02,
#'      9.85e-02, 1.06e-01, 1.07e-01, 1.11e-01, 1.31e-01, 1.38e-01, 1.40e-01, 1.43e-01,
#'      2.00e-01, 2.39e-01, 2.49e-01, 2.57e-01, 2.86e-01, 2.86e-01, 2.98e-01, 3.32e-01,
#'      4.15e-01, 4.91e-01, 4.91e-01, 4.91e-01, 5.97e-01, 7.11e-01, 7.59e-01, 8.38e-01,
#'      9.20e-01, 9.20e-01, 9.29e-01, 9.29e-01, 9.29e-01, 9.29e-01, 9.34e-01, 9.68e-01,
#'      1.00e+00, 1.00e+00, 1.00e+00, 1.00e+00, 1.00e+00, 1.00e+00, 1.00e+00, 1.00e+00,
#'      1.00e+00, 1.00e+00, 1.00e+00, 1.00e+00, 1.00e+00, 1.00e+00, 1.00e+00, 1.00e+00),
#'    gene = c(
#'      "HLA-DPB1", "LYZ", "HLA-DRA", "TYMP", "HLA-DPA1", "HLA-DRB1", "CST3", "HLA-DQB1",
#'      "HLA-DRB5", "LST1", "HLA-DQA1", "AIF1", "S100A8", "IFITM3", "HLA-DMB", "FCGRT",
#'      "SERPINA1", "IFI30", "S100A9", "CCL5", "GRN", "LCK", "HLA-DMA", "MS4A6A", "CTSS",
#'      "CFP", "FCN1", "BID", "CFD", "CD3D", "CD7", "CD3E", "LGALS2", "CD14", "SMCO4",
#'      "LINC00936", "HCK", "CTSW", "LGALS1", "HLA-DQA2", "LRRC25", "GZMM", "RNF130",
#'      "LGALS3", "S100A11", "C5AR1", "IL1B", "GZMA", "FCER1G", "MPEG1", "TYROBP", "TSPO",
#'      "GSTP1", "CTSB", "IL32", "CD247", "GNLY", "COTL1", "NFKBIA", "NUP214", "LAMP1",
#'      "FPR1", "CLEC10A", "CST7", "PRF1", "BLVRA", "PSAP", "GZMH", "EAF2", "ASGR1",
#'      "RARRES3", "SAT1", "LY86", "GP9", "TUBB1", "NGFRAP1", "XBP1", "SCO2", "RGS2", "GZMB",
#'      "HIST1H2AC", "KLRD1", "PGRMC1", "AKR1C3", "PTGDR", "IL2RB", "GYPC", "CCL4", "CD68",
#'      "FCER1A", "CD79B", "MS4A7", "CARD16", "ACAP1", "CD79A", "ANXA2", "TMEM40", "PF4",
#'      "GNG11", "CLU", "CD9", "FGFBP2", "TNFRSF1B", "IFI6"),
#'   pct_diff = c(
#'      -0.752, -0.457, -0.460, -0.671, -0.626, -0.701, -0.502, -0.619, -0.623, -0.598,
#'      -0.566, -0.626, -0.543, -0.566, -0.541, -0.542, -0.515, -0.489, -0.444, 0.428,
#'      -0.517, 0.461, -0.491, -0.410, -0.480, -0.491, -0.521, -0.491, -0.438, 0.411,
#'      0.411, 0.409, -0.438, -0.359, -0.359, -0.440, -0.386, 0.385, -0.332, -0.361, -0.361,
#'      0.364, -0.387, -0.415, -0.454, -0.308, -0.335, 0.364, -0.454, -0.309, -0.379, -0.427,
#'      -0.377, -0.389, 0.335, 0.315, 0.313, -0.284, -0.502, -0.309, 0.313, -0.284, -0.256,
#'      0.309, 0.313, -0.364, -0.406, 0.244, -0.231, -0.231, 0.281, -0.311, -0.312, 0.220,
#'      0.220, 0.220, 0.261, -0.232, -0.367, 0.240, 0.218, 0.218, 0.195, 0.195, 0.195, 0.195,
#'      0.262, 0.218, -0.288, -0.207, -0.290, -0.233, -0.367, 0.217, -0.233, -0.403, 0.171,
#'      0.194, 0.194, 0.194, 0.194, 0.213, -0.235, -0.292),
#'   group = sample(LETTERS[1:2], 104, replace = TRUE)
#' )
#' # If set, it will be used as labels if label_by is not set.
#' # rownames(data) <- data$gene
#'
#' # --- Basic usage ---
#' VolcanoPlot(data, x = "avg_log2FC", y = "p_val_adj", color_by = "pct_diff",
#'    y_cutoff_name = "-log10(0.05)")
#' # --- With gene labels ---
#' VolcanoPlot(data, x = "avg_log2FC", y = "p_val_adj", color_by = "pct_diff",
#'    y_cutoff_name = "-log10(0.05)", label_by = "gene")
#' # --- Mirrored layout ---
#' VolcanoPlot(data, x = "avg_log2FC", y = "p_val_adj", y_cutoff_name = "none",
#'    flip_negatives = TRUE, label_by = "gene")
#' # --- With faceting ---
#' VolcanoPlot(data, x = "avg_log2FC", y = "p_val_adj", y_cutoff_name = "none",
#'    flip_negatives = TRUE, facet_by = "group", label_by = "gene")
#' # --- With splitting ---
#' VolcanoPlot(data, x = "avg_log2FC", y = "p_val_adj", y_cutoff_name = "none",
#'    flip_negatives = TRUE, split_by = "group", label_by = "gene")
#' # --- With highlighting ---
#' VolcanoPlot(data, x = "avg_log2FC", y = "p_val_adj", y_cutoff_name = "none",
#'    highlight = c("ANXA2", "TMEM40", "PF4", "GNG11", "CLU", "CD9", "FGFBP2",
#'    "TNFRSF1B", "IFI6"), label_by = "gene")
#' # --- Per-split palettes ---
#' VolcanoPlot(data, x = "avg_log2FC", y = "p_val_adj", color_by = "pct_diff",
#'    y_cutoff_name = "-log10(0.05)", split_by = "group", label_by = "gene",
#'    palette = c(A = "Set1", B = "Dark2"))
#' }
VolcanoPlot <- function(
    data,
    x,
    y,
    ytrans = function(n) -log10(n),
    color_by = NULL,
    color_name = NULL,
    xlim = NULL,
    flip_negatives = FALSE,
    x_cutoff = NULL,
    y_cutoff = 0.05,
    split_by = NULL,
    split_by_sep = "_",
    label_by = NULL,
    x_cutoff_name = NULL,
    y_cutoff_name = NULL,
    x_cutoff_color = "red2",
    y_cutoff_color = "blue2",
    x_cutoff_linetype = "dashed",
    y_cutoff_linetype = "dashed",
    x_cutoff_linewidth = 0.5,
    y_cutoff_linewidth = 0.5,
    pt_size = 2,
    pt_alpha = 0.5,
    nlabel = 5,
    labels = NULL,
    label_size = 3,
    label_fg = "black",
    label_bg = "white",
    label_bg_r = 0.1,
    highlight = NULL,
    highlight_color = "red",
    highlight_size = 2,
    highlight_alpha = 1,
    highlight_stroke = 0.5,
    trim = c(0, 1),
    facet_by = NULL,
    facet_scales = "fixed",
    facet_ncol = NULL,
    facet_nrow = NULL,
    facet_byrow = TRUE,
    theme = "theme_this",
    theme_args = list(),
    palette = "Spectral",
    palcolor = NULL,
    palreverse = FALSE,
    title = NULL,
    subtitle = NULL,
    xlab = NULL,
    ylab = NULL,
    aspect.ratio = 1,
    legend.position = "right",
    legend.direction = "vertical",
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
            VolcanoPlotAtomic(
                datas[[nm]],
                x = x,
                y = y,
                ytrans = ytrans,
                color_by = color_by,
                color_name = color_name,
                flip_negatives = flip_negatives,
                x_cutoff = x_cutoff,
                y_cutoff = y_cutoff,
                trim = trim,
                xlim = xlim,
                label_by = label_by,
                x_cutoff_name = x_cutoff_name,
                y_cutoff_name = y_cutoff_name,
                x_cutoff_color = x_cutoff_color,
                y_cutoff_color = y_cutoff_color,
                x_cutoff_linetype = x_cutoff_linetype,
                y_cutoff_linetype = y_cutoff_linetype,
                x_cutoff_linewidth = x_cutoff_linewidth,
                y_cutoff_linewidth = y_cutoff_linewidth,
                pt_size = pt_size,
                pt_alpha = pt_alpha,
                nlabel = nlabel,
                labels = labels,
                label_size = label_size,
                label_fg = label_fg,
                label_bg = label_bg,
                label_bg_r = label_bg_r,
                highlight = highlight,
                highlight_color = highlight_color,
                highlight_size = highlight_size,
                highlight_alpha = highlight_alpha,
                highlight_stroke = highlight_stroke,
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
                title = title,
                subtitle = subtitle,
                xlab = xlab,
                ylab = ylab,
                aspect.ratio = aspect.ratio,
                legend.position = legend.position[[nm]],
                legend.direction = legend.direction[[nm]],
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
