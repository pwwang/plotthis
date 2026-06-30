#' Atomic jitter plot (internal)
#'
#' @description
#' Core implementation for drawing a single jittered point plot.  This is the
#' workhorse behind the exported \code{\link{JitterPlot}} function — it takes a
#' **single** data frame (no \code{split_by} support) and returns a
#' \code{ggplot} object.  The plot displays individual data points with random
#' jitter along the x-axis (and optionally the y-axis) to reveal the
#' distribution of values within each x category, making it especially useful
#' for visualising overlapping discrete data.
#'
#' The function provides extensive annotation features including point labelling
#' (via \code{\link[ggrepel]{geom_text_repel}} with automatic top-n selection
#' using a configurable distance metric), point highlighting, optional dodging
#' via \code{group_by}, and horizontal reference lines.  It supports both long
#' and wide input formats, quantile-based axis limits, and x-axis reordering by
#' y-value summaries (mean or median).
#'
#' @section Architecture:
#' \enumerate{
#'   \item \strong{ggplot dispatch} — selects \code{gglogger::ggplot} or
#'         \code{ggplot2::ggplot} based on
#'         \code{getOption("plotthis.gglogger.enabled")}.
#'   \item \strong{in_form handling} — when \code{in_form = "wide"}, the x
#'         columns are pivoted to long format via
#'         \code{\link[tidyr]{pivot_longer}()}, creating a key column
#'         \code{.x} and a value column \code{.y}.
#'   \item \strong{Column resolution} — \code{x}, \code{y}, \code{group_by},
#'         and \code{facet_by} are validated and transformed via
#'         \code{\link{check_columns}}.  Multi-column inputs for \code{x} and
#'         \code{group_by} are concatenated into single columns using their
#'         respective separators (\code{x_sep}, \code{group_by_sep}).
#'   \item \strong{NA / empty-level handling} — \code{\link{process_keep_na_empty}()}
#'         applies \code{keep_na} and \code{keep_empty} policies.  Per-column
#'         \code{keep_empty} settings are extracted for \code{x},
#'         \code{group_by}, and \code{facet_by} independently.  When
#'         \code{facet_by} has more than one column, the \code{keep_empty}
#'         values for all facet columns must be identical.
#'   \item \strong{sort_x resolution} — \code{match.arg()} normalises the
#'         \code{sort_x} argument, and per-group (\code{x}, \code{group_by},
#'         \code{facet_by}) means and medians of \code{y} are computed for
#'         subsequent reordering.
#'   \item \strong{y-axis limit computation} — \code{y_max} and \code{y_min}
#'         support raw numeric values or quantile strings (e.g. \code{"q95"}),
#'         which are resolved via \code{\link[stats]{quantile}()}.  These
#'         limits are used for fixed \code{coord_cartesian} / \code{coord_flip}.
#'   \item \strong{Highlight flag} — when \code{highlight} is provided, a
#'         logical column \code{.highlight} is created.  Values can be
#'         \code{TRUE} (all points), a numeric vector of row indices, a single
#'         character string parsed as an \R expression, or a character vector
#'         matched against row names.
#'   \item \strong{Label preparation} — labels are resolved from
#'         \code{label_by} or row names.  When \code{labels} is \code{NULL}
#'         and \code{nlabel > 0}, the top \code{nlabel} points per x-group are
#'         selected by descending order of \code{order_by} (default:
#'         \code{-({y}^2 + {size_by}^2)}, the radial distance from zero in
#'         y-size space, analogous to VolcanoPlot).  When \code{facet_by} is
#'         active, the ranking is nested within each facet panel.
#'   \item \strong{x-axis reordering} — when \code{sort_x} is not
#'         \code{"none"}, the x-axis factor levels are reordered by the
#'         per-group mean or median via \code{\link[stats]{reorder}()}.
#'   \item \strong{Flip handling} — when \code{flip = TRUE}, the x-axis
#'         factor levels are reversed and the aspect ratio is inverted.
#'   \item \strong{Colour assignment} — \code{\link{palette_this}()} assigns
#'         colours to the levels of \code{group_by} (or \code{x} when
#'         \code{group_by} is \code{NULL}), including \code{NA} levels which
#'         are relabelled as \code{"NA"}.
#'   \item \strong{Background layer} — when \code{add_bg = TRUE}, alternating
#'         background stripes are drawn via \code{\link{bg_layer}()} using the
#'         x-axis level order.
#'   \item \strong{Position jitter} — when \code{group_by} is \code{NULL},
#'         \code{\link[ggplot2]{position_jitter}()} is used.  When
#'         \code{group_by} is provided, \code{\link[ggplot2]{position_jitterdodge}()}
#'         is used so points are dodged by group before jittering.
#'   \item \strong{Pre-compute jittered positions} — a temporary \code{ggplot}
#'         is built and rendered via \code{ggplot_build()} to extract the
#'         actual jittered coordinates.  These are stored as \code{.x_jittered}
#'         and \code{.y_jittered} columns and used by the label layer because
#'         \code{geom_text_repel} does not natively respect
#'         \code{position_jitter} / \code{position_jitterdodge}.
#'   \item \strong{Point layer} — \code{geom_point()} is built with the
#'         appropriate aesthetic mapping: fill (for shapes 21–25) or colour
#'         (for other shapes) mapped to the grouping variable.  Border
#'         behaviour for shapes 21–25 is controlled by \code{border}
#'         (constant string, mapped to group colour, or \code{NA}).  When
#'         \code{size_by} is a column, size is mapped via \code{aes(size)}.
#'   \item \strong{Discrete colour/fill scales} —
#'         \code{scale_fill_manual()} or \code{scale_color_manual()} with the
#'         assigned palette colours.  When \code{keep_empty} is \code{TRUE}
#'         for the colour column, \code{drop = FALSE} and explicit
#'         \code{breaks} / \code{limits} preserve empty levels.  An additional
#'         \code{scale_color_manual()} is added for border colour when
#'         \code{border = TRUE}.
#'   \item \strong{Size scale} — when \code{size_by} is a column,
#'         \code{scale_size_area(max_size = 6)} is applied.  If
#'         \code{size_trans} is provided, the legend labels show the original
#'         (untransformed) values while the mapping uses transformed values.
#'   \item \strong{Highlight overlay} — if any points are highlighted, a
#'         second \code{geom_point()} layer is drawn on top using the
#'         \code{highlight_color}, \code{highlight_size}, and
#'         \code{highlight_alpha} settings.  The highlight layer does not
#'         affect the legend.
#'   \item \strong{Label layer} — if any points are selected for labelling,
#'         \code{\link[ggrepel]{geom_text_repel}()} is rendered at the
#'         pre-computed jittered coordinates.
#'   \item \strong{Horizontal reference lines} — when \code{add_hline} is
#'         provided, \code{geom_hline()} is drawn using the specified line
#'         aesthetics.
#'   \item \strong{Axes and labels} — \code{scale_x_discrete()} (with
#'         \code{drop = !isTRUE(keep_empty_x)}),
#'         \code{scale_y_continuous(trans = y_trans, n.breaks = y_nbreaks)},
#'         and \code{labs()} set the axis scale properties and titles.
#'   \item \strong{Coordinate system} — when \code{flip = TRUE},
#'         \code{coord_flip()} is used with (or without) fixed y-limits
#'         depending on free-scale faceting.  When \code{flip = FALSE},
#'         \code{coord_cartesian()} sets the y-axis limits unless free scales
#'         are in use.
#'   \item \strong{Theme} — \code{do_call(theme, theme_args)} applies the
#'         selected theme, with additional \code{aspect.ratio}, x-axis text
#'         rotation, legend position, and legend direction adjustments.
#'         Panel grid lines are styled: major y-axis lines (dashed grey) for
#'         non-flipped plots; major x-axis lines for flipped plots.
#'   \item \strong{Dimension calculation} — \code{\link{calculate_plot_dimensions}()}
#'         computes plot height and width from the x-axis category count,
#'         \code{aspect.ratio}, dodge group count, legend metrics, and label
#'         character width.  The minimum dimensions account for x-axis label
#'         length.  The resulting \code{height} / \code{width} attributes are
#'         stored on the \code{ggplot} object.  For flipped plots, the width
#'         is set to at least \code{max(width, height)}.
#'   \item \strong{Faceting} — \code{\link{facet_plot}()} wraps the plot
#'         with \code{facet_wrap} / \code{facet_grid} if \code{facet_by} is
#'         provided, respecting the \code{keep_empty} setting for facet
#'         variables.
#' }
#'
#' @inheritParams common_args
#' @param x A character string specifying the column name for the x-axis.
#'  Must be character or factor.  Multiple columns can be provided; they are
#'  concatenated with \code{x_sep} as the separator.  When \code{in_form} is
#'  \code{"wide"}, the \code{x} columns are used as key columns and pivoted
#'  to long format (they are not concatenated).
#' @param x_sep A character string used to join multiple \code{x} columns.
#'  Default \code{"_"}.  Ignored when \code{x} is a single column or when
#'  \code{in_form} is \code{"wide"}.
#' @param y A character string specifying the numeric column for the y-axis.
#'  Required when \code{in_form} is \code{"long"} (default).  When
#'  \code{in_form} is \code{"wide"}, \code{y} is not required — the values
#'  under the \code{x} columns are used as y-values.
#' @param in_form A character string specifying the input data format.  Either
#'  \code{"long"} (default) or \code{"wide"}.  In \code{"long"} format,
#'  \code{x} and \code{y} are separate columns.  In \code{"wide"} format, the
#'  \code{x} columns contain the y-values and are pivoted to a key-value pair.
#' @param sort_x A character string controlling x-axis level reordering by
#'  y-value summaries.  One of \code{"none"}, \code{"mean_asc"},
#'  \code{"mean_desc"}, \code{"mean"}, \code{"median_asc"},
#'  \code{"median_desc"}, \code{"median"}.  \code{"none"} leaves the levels
#'  as-is.  \code{"mean_asc"} / \code{"mean"} sorts by ascending mean of y.
#'  \code{"mean_desc"} sorts by descending mean.  \code{"median_asc"} /
#'  \code{"median"} sorts by ascending median.  \code{"median_desc"} sorts by
#'  descending median.  Default: \code{"none"}.
#' @param flip A logical value.  When \code{TRUE}, the x and y axes are
#'  swapped via \code{coord_flip} and the x-axis factor levels are reversed.
#'  Dimension calculation accounts for the flip.  Default: \code{FALSE}.
#' @param group_by A character vector of column names for dodging the points.
#'  Each unique combination becomes a separate dodge group and the points are
#'  offset horizontally via \code{position_jitterdodge} to reduce overlap.
#'  Multiple columns are concatenated with \code{group_by_sep}.  When
#'  \code{NULL} (default), no dodging is applied — only jitter via
#'  \code{position_jitter}.
#' @param group_by_sep A character string used to join multiple
#'  \code{group_by} columns.  Default \code{"_"}.
#' @param group_name A character string for the dodge-group legend title.
#'  When \code{NULL} (default), the \code{group_by} column name is used.
#' @param add_bg A logical value.  When \code{TRUE}, alternating background
#'  stripes are drawn behind the points via \code{\link{bg_layer}()}, using
#'  the x-axis level order.  Default: \code{FALSE}.
#' @param bg_palette A character string specifying the palette for the
#'  background stripe colours.  Passed to \code{\link{bg_layer}()}.
#'  Default: \code{"stripe"}.
#' @param bg_palcolor A character vector of colours for the background
#'  stripes.  Passed to \code{\link{bg_layer}()}.  When \code{NULL} (default),
#'  colours are derived from \code{bg_palette}.
#' @param bg_alpha A numeric value in \code{[0, 1]} for the transparency of
#'  the background stripes.  Default: \code{0.2}.
#' @param shape A numeric value specifying the point shape (ggplot2 point
#'  shape codes).  Shapes 21–25 are filled shapes with borders; for these
#'  shapes the border behaviour is controlled by \code{border}.  Default:
#'  \code{21} (filled circle).
#' @param border Controls the border of points when the shape has a border
#'  (21–25).  If \code{TRUE}, the border colour follows the point fill colour
#'  (same as the group colour).  If a single colour string (e.g.
#'  \code{"black"}), uses that constant border colour for all points.  If
#'  \code{FALSE}, no border is drawn (\code{NA}).  Default: \code{"black"}.
#' @param size_by A numeric column name or a single numeric value controlling
#'  point size.  When a column name is provided, sizes are scaled using
#'  \code{scale_size_area(max_size = 6)} and a size legend is shown.  When a
#'  single numeric value, all points use that constant size.  Default:
#'  \code{2}.
#' @param size_name A character string for the size legend title.  When
#'  \code{NULL} (default) and \code{size_by} is a column, the column name is
#'  used.  Ignored when \code{size_by} is a single numeric value.
#' @param size_trans A function or a function name (as a string) to transform
#'  the \code{size_by} values for size mapping.  The transformed values
#'  determine the point size on the plot, but the legend labels show the
#'  original (untransformed) values.  When \code{NULL} (default), no
#'  transformation is applied.
#' @param alpha A numeric value in \code{[0, 1]} controlling point
#'  transparency.  Default: \code{1}.
#' @param jitter_width A numeric value controlling the amount of horizontal
#'  jitter (in x-axis units).  Passed to \code{position_jitter} /
#'  \code{position_jitterdodge}.  Default: \code{0.5}.
#' @param jitter_height A numeric value controlling the amount of vertical
#'  jitter (in y-axis units).  Passed to \code{position_jitter} /
#'  \code{position_jitterdodge}.  Default: \code{0}.
#' @param y_max,y_min Numeric values or quantile strings (e.g. \code{"q95"},
#'  \code{"q5"}) for y-axis limits used in \code{coord_cartesian} /
#'  \code{coord_flip}.  When \code{NULL} (default), the data range is used.
#'  When a quantile string, the corresponding quantile of the y-values is
#'  computed via \code{\link[stats]{quantile}()}.
#' @param y_trans A character string specifying a transformation for the
#'  y-axis (e.g. \code{"log10"}, \code{"sqrt"}).  Passed to
#'  \code{\link[ggplot2]{scale_y_continuous}}.  Default: \code{"identity"}.
#' @param y_nbreaks A numeric value hinting at the number of break intervals
#'  for the y-axis.  Passed to \code{scale_y_continuous}.  Default: \code{4}.
#' @param labels A vector of row names or row indices specifying which points
#'  to label.  When \code{NULL} (default) and \code{nlabel > 0}, the top
#'  \code{nlabel} points per x-group are selected automatically.
#' @param label_by A character string naming a column whose values are used
#'  as label text.  When \code{NULL} (default), row names are used as labels.
#' @param nlabel An integer specifying the number of points to label per
#'  x-group when \code{labels} is \code{NULL}.  Points are selected by
#'  descending order of \code{order_by}.  Default: \code{5}.  Set to
#'  \code{0} to suppress automatic labelling.
#' @param order_by A string expression passed to \code{\link[dplyr]{arrange}()}
#'  to determine which points are labelled.  Evaluated within each x-group
#'  (and facet panel when \code{facet_by} is set).  Default:
#'  \code{"-({y}^2 + {size_by}^2)"}, which selects points farthest from the
#'  origin in y-size radial distance, analogous to VolcanoPlot.
#' @param label_size,label_fg,label_bg,label_bg_r Label aesthetics for
#'  \code{\link[ggrepel]{geom_text_repel}}.  \code{label_size}: text size
#'  (default \code{3}).  \code{label_fg}: text colour (default
#'  \code{"black"}).  \code{label_bg}: background (halo) colour for the label
#'  text (default \code{"white"}).  \code{label_bg_r}: background border
#'  radius (default \code{0.1}).
#' @param add_hline One or more numeric values specifying y-values at which
#'  to draw horizontal reference lines.  When \code{NULL} (default), no
#'  reference lines are drawn.
#' @param hline_type A character string specifying the line type for the
#'  horizontal reference line(s).  Default: \code{"solid"}.
#' @param hline_width A numeric value specifying the line width for the
#'  horizontal reference line(s).  Default: \code{0.5}.
#' @param hline_color A character string specifying the colour for the
#'  horizontal reference line(s).  Default: \code{"black"}.
#' @param hline_alpha A numeric value in \code{[0, 1]} specifying the alpha
#'  (transparency) for the horizontal reference line(s).  Default: \code{1}.
#' @param highlight A specification of which points to highlight.  Can be:
#'  \code{TRUE} (highlight all points), a numeric vector of row indices, a
#'  single character string parsed as an \R expression, or a character vector
#'  of row names.  When a point is highlighted, an overlay \code{geom_point}
#'  is drawn on top with \code{highlight_color}, \code{highlight_size}, and
#'  \code{highlight_alpha}.  Default: \code{NULL} (no highlighting).
#' @param highlight_color A character string specifying the colour of
#'  highlighted points.  Default: \code{"red2"}.
#' @param highlight_size A numeric value specifying the size of highlighted
#'  points.  Default: \code{1}.
#' @param highlight_alpha A numeric value in \code{[0, 1]} specifying the
#'  transparency of highlighted points.  Default: \code{1}.
#' @return A \code{ggplot} object with \code{height} and \code{width}
#'  attributes (in inches) attached.
#' @keywords internal
#' @importFrom stats median quantile
#' @importFrom rlang sym syms parse_expr %||%
#' @importFrom dplyr mutate ungroup first
#' @importFrom ggplot2 geom_point labs theme element_line element_text coord_flip
#' @importFrom ggplot2 position_jitterdodge scale_fill_manual scale_color_manual scale_y_continuous scale_size_area guide_legend guide_colorbar
#' @importFrom ggrepel geom_text_repel
JitterPlotAtomic <- function(
    data,
    x,
    x_sep = "_",
    y = NULL,
    in_form = c("long", "wide"),
    keep_na = FALSE,
    keep_empty = FALSE,
    sort_x = c(
        "none",
        "mean_asc",
        "mean_desc",
        "mean",
        "median_asc",
        "median_desc",
        "median"
    ),
    flip = FALSE,
    group_by = NULL,
    group_by_sep = "_",
    group_name = NULL,
    x_text_angle = 0,
    order_by = "-({y}^2 + {size_by}^2)",
    theme = "theme_this",
    theme_args = list(),
    palette = "Paired",
    palcolor = NULL,
    palreverse = FALSE,
    alpha = 1,
    aspect.ratio = NULL,
    legend.position = "right",
    legend.direction = "vertical",
    shape = 21,
    border = "black",
    size_by = 2,
    size_name = NULL,
    size_trans = NULL,
    y_nbreaks = 4,
    jitter_width = 0.5,
    jitter_height = 0,
    y_max = NULL,
    y_min = NULL,
    y_trans = "identity",
    add_bg = FALSE,
    bg_palette = "stripe",
    bg_palcolor = NULL,
    bg_alpha = 0.2,
    add_hline = NULL,
    hline_type = "solid",
    hline_width = 0.5,
    hline_color = "black",
    hline_alpha = 1,
    labels = NULL,
    label_by = NULL,
    nlabel = 5,
    label_size = 3,
    label_fg = "black",
    label_bg = "white",
    label_bg_r = 0.1,
    highlight = NULL,
    highlight_color = "red2",
    highlight_size = 1,
    highlight_alpha = 1,
    facet_by = NULL,
    facet_scales = "fixed",
    facet_ncol = NULL,
    facet_nrow = NULL,
    facet_byrow = TRUE,
    title = NULL,
    subtitle = NULL,
    xlab = NULL,
    ylab = NULL,
    seed = 8525,
    ...
) {
    set.seed(seed)
    ggplot <- if (getOption("plotthis.gglogger.enabled", FALSE)) {
        gglogger::ggplot
    } else {
        ggplot2::ggplot
    }
    in_form <- match.arg(in_form)
    if (in_form == "wide") {
        data <- data %>%
            tidyr::pivot_longer(cols = x, names_to = ".x", values_to = ".y")
        x <- ".x"
        y <- ".y"
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
            "[JitterPlot] `keep_empty` for `facet_by` variables must be identical." = identical(
                keep_empty_facet,
                keep_empty[[facet_by[2]]]
            )
        )
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
            stop(
                "No row names in the data, please provide a vector of indexes to highlight."
            )
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
            data <- data %>%
                dplyr::group_by(!!!syms(facet_by), !!sym(x)) %>%
                dplyr::arrange(!!rlang::parse_expr(glue::glue(order_by))) %>%
                dplyr::mutate(.show_label = dplyr::row_number() <= nlabel) %>%
                dplyr::ungroup()
        } else {
            data <- data %>%
                dplyr::group_by(!!sym(x)) %>%
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
        if (length(aspect.ratio) == 0 || is.na(aspect.ratio)) {
            aspect.ratio <- NULL
        }
    }

    color_col <- ifelse(is.null(group_by), x, group_by)
    keep_empty_col <- if (color_col == x) keep_empty_x else keep_empty_group
    col_levels <- levels(data[[color_col]])
    if (anyNA(data[[color_col]])) {
        col_levels <- c(col_levels, NA)
    }
    colors <- palette_this(
        col_levels,
        palette = palette,
        palcolor = palcolor,
        NA_keep = TRUE,
        reverse = palreverse
    )
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

    # Positioner (jitter + optional dodge)
    if (is.null(group_by)) {
        pos <- position_jitter(
            width = jitter_width,
            height = jitter_height,
            seed = seed
        )
    } else {
        pos <- position_jitterdodge(
            jitter.width = jitter_width,
            jitter.height = jitter_height,
            dodge.width = 0.9,
            seed = seed
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
        shape = shape,
        position = pos,
        alpha = alpha
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
    p <- p + do_call(geom_point, point_args)

    # Discrete color/fill scales by x
    if (has_fill) {
        if (isTRUE(keep_empty_col)) {
            p <- p +
                scale_fill_manual(
                    name = color_col,
                    values = colors,
                    na.value = colors['NA'] %||% "grey80",
                    breaks = col_levels,
                    limits = col_levels,
                    drop = FALSE
                )
        } else {
            p <- p +
                scale_fill_manual(
                    name = color_col,
                    values = colors,
                    na.value = colors['NA'] %||% "grey80"
                )
        }
        if (isTRUE(border)) {
            p <- p +
                scale_color_manual(
                    values = colors,
                    guide = "none",
                    na.value = colors['NA'] %||% "grey80"
                )
        }
    } else {
        if (isTRUE(keep_empty_col)) {
            p <- p +
                scale_color_manual(
                    name = color_col,
                    values = colors,
                    na.value = colors['NA'] %||% "grey80",
                    breaks = col_levels,
                    limits = col_levels,
                    drop = FALSE
                )
        } else {
            p <- p +
                scale_color_manual(
                    name = color_col,
                    values = colors,
                    na.value = colors['NA'] %||% "grey80"
                )
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
        p <- p +
            scale_size_area(
                max_size = 6,
                breaks = mapped_breaks,
                labels = raw_breaks
            ) +
            guides(
                size = guide_legend(
                    title = size_name %||% size_by,
                    override.aes = list(fill = "grey30", shape = shape),
                    order = 1
                )
            )
    }

    # Highlight overlay on top (does not affect legends)
    if (any(data$.highlight)) {
        hi_df <- data[data$.highlight, , drop = FALSE]
        if (has_fill) {
            p <- p +
                geom_point(
                    data = hi_df,
                    mapping = if (!is.null(group_by)) {
                        aes(x = !!sym(x), y = !!sym(y), group = !!sym(group_by))
                    } else {
                        aes(x = !!sym(x), y = !!sym(y))
                    },
                    shape = shape,
                    fill = highlight_color,
                    color = "transparent",
                    position = pos,
                    size = if (is.numeric(size_by)) {
                        highlight_size
                    } else {
                        highlight_size
                    },
                    alpha = highlight_alpha
                )
        } else {
            p <- p +
                geom_point(
                    data = hi_df,
                    mapping = if (!is.null(group_by)) {
                        aes(x = !!sym(x), y = !!sym(y), group = !!sym(group_by))
                    } else {
                        aes(x = !!sym(x), y = !!sym(y))
                    },
                    shape = shape,
                    color = highlight_color,
                    position = pos,
                    size = if (is.numeric(size_by)) {
                        highlight_size
                    } else {
                        highlight_size
                    },
                    alpha = highlight_alpha
                )
        }
    }

    # Labels layer
    if (any(data$.show_label)) {
        p <- p +
            geom_text_repel(
                data = data[data$.show_label, , drop = FALSE],
                mapping = aes(
                    x = !!sym(".x_jittered"),
                    y = !!sym(".y_jittered"),
                    label = !!sym(".label")
                ),
                color = label_fg,
                bg.color = label_bg,
                bg.r = label_bg_r,
                size = label_size,
                min.segment.length = 0,
                segment.color = "grey40",
                max.overlaps = 100
            )
    }
    # Optional horizontal reference lines
    if (!is.null(add_hline)) {
        p <- p +
            ggplot2::geom_hline(
                yintercept = add_hline,
                linetype = hline_type,
                linewidth = hline_width,
                color = hline_color,
                alpha = hline_alpha
            )
    }

    just <- calc_just(x_text_angle)
    p <- p +
        scale_x_discrete(drop = !isTRUE(keep_empty_x)) +
        scale_y_continuous(trans = y_trans, n.breaks = y_nbreaks) +
        labs(
            title = title,
            subtitle = subtitle,
            x = xlab %||% x,
            y = ylab %||% y
        )

    x_maxchars <- max(nchar(levels(data[[x]])))
    nx <- nlevels(data[[x]])
    nd <- ifelse(is.null(group_by), 1, nlevels(data[[group_by]]))
    facet_free <- !is.null(facet_by) &&
        (identical(facet_scales, "free") ||
            (!flip && identical(facet_scales, "free_y")) ||
            (flip && identical(facet_scales, "free_x")))

    if (isTRUE(flip)) {
        strip_position <- "top"
        p <- p +
            ggplot2::theme(
                strip.text.y = element_text(angle = 0),
                panel.grid.major.y = element_line(color = "grey", linetype = 2)
            )
        if (facet_free) {
            p <- p + coord_flip()
        } else {
            p <- p + coord_flip(ylim = c(y_min_use, y_max_use))
        }
    } else {
        strip_position <- "top"
        p <- p +
            ggplot2::theme(
                strip.text.x = element_text(angle = 0),
                panel.grid.major.x = element_line(color = "grey", linetype = 2)
            )
        if (!facet_free) {
            p <- p + ggplot2::coord_cartesian(ylim = c(y_min_use, y_max_use))
        }
    }

    p <- p +
        do_call(theme, theme_args) +
        ggplot2::theme(
            aspect.ratio = aspect.ratio,
            axis.text.x = element_text(
                angle = x_text_angle,
                hjust = just$h,
                vjust = just$v
            ),
            legend.position = legend.position,
            legend.direction = legend.direction
        )

    if (isTRUE(flip)) {
        label_min_width <- max(3, 2.2 + x_maxchars * 0.05)
        dims <- calculate_plot_dimensions(
            base_height = label_min_width,
            aspect.ratio = aspect.ratio,
            n_y = nx * nd,
            y_scale_factor = 0.5,
            legend.position = legend.position,
            legend.direction = legend.direction,
            legend_n = nd,
            legend_nchar = max(nchar(col_levels)),
            flip = TRUE
        )
        height <- dims$height
        width <- max(dims$width, label_min_width)
    } else {
        label_min_height <- 2 + x_maxchars * 0.05
        dims <- calculate_plot_dimensions(
            base_height = label_min_height,
            aspect.ratio = aspect.ratio,
            n_x = nx * nd,
            x_scale_factor = 0.5,
            legend.position = legend.position,
            legend.direction = legend.direction,
            legend_n = nd,
            legend_nchar = max(nchar(col_levels)),
            flip = FALSE
        )
        height <- max(dims$height, label_min_height)
        width <- dims$width
    }

    attr(p, "height") <- height
    attr(p, "width") <- max(width, height)

    facet_plot(
        p,
        facet_by,
        facet_scales,
        facet_nrow,
        facet_ncol,
        facet_byrow,
        strip.position = strip_position,
        legend.position = legend.position,
        legend.direction = legend.direction,
        drop = !isTRUE(keep_empty_facet)
    )
}

#' Jitter plot
#'
#' @description
#' Draws a jittered point plot showing the distribution of numeric y-values
#' across a discrete x-axis.  Each data point is rendered with random jitter
#' along the x-axis (and optionally the y-axis) to reduce overplotting, making
#' it easy to visualise data density, spread, and outliers within each category.
#'
#' The function supports \strong{x-axis reordering} by y-value summaries
#' (mean or median), \strong{group dodging} via \code{group_by} to compare
#' subgroups side-by-side, \strong{point labelling} with automatic top-n
#' selection using a configurable distance metric (default: radial distance
#' \code{y^2 + size^2}), \strong{point highlighting} for emphasis, optional
#' \strong{horizontal reference lines}, and \strong{wide-format input} via
#' \code{in_form}.  Colour control, faceting, and splitting into separate
#' sub-plots via \code{split_by} are supported.
#'
#' @section split_by workflow:
#' When \code{split_by} is provided:
#' \enumerate{
#'   \item \code{\link{validate_common_args}()} validates the \code{seed}.
#'   \item \code{\link{check_keep_na}()} and \code{\link{check_keep_empty}()}
#'         normalise the \code{keep_na} / \code{keep_empty} arguments for all
#'         relevant columns (\code{x}, \code{split_by}, \code{group_by},
#'         \code{facet_by}).
#'   \item The \code{split_by} column is validated via
#'         \code{\link{check_columns}()} with \code{force_factor = TRUE}.
#'         Multiple \code{split_by} columns are concatenated with
#'         \code{split_by_sep}.
#'   \item If \code{split_by} is not \code{NULL}, the data frame is split
#'         (preserving factor level order).  If \code{split_by} is
#'         \code{NULL}, the data is wrapped in a single-element list with
#'         name \code{"..."}.
#'   \item Per-split \code{palette}, \code{palcolor},
#'         \code{legend.position}, and \code{legend.direction} are resolved
#'         via \code{\link{check_palette}()}, \code{\link{check_palcolor}()},
#'         and \code{\link{check_legend}()}.
#'   \item \code{\link{JitterPlotAtomic}()} is called for each split.  If
#'         \code{title} is a function, it receives the split level name and
#'         can generate dynamic titles.
#'   \item Results are combined via \code{\link{combine_plots}()} (when
#'         \code{combine = TRUE}) or returned as a named list.
#' }
#'
#' @rdname jitterplot
#' @export
#' @inheritParams JitterPlotAtomic
#' @inheritParams common_args
#' @param split_by The column(s) to split the data by and produce separate
#'  sub-plots.  Multiple columns are concatenated with \code{split_by_sep}.
#' @param split_by_sep A character string to separate concatenated
#'  \code{split_by} columns.  Default \code{"_"}.
#' @param seed A numeric seed for reproducibility.  Passed to
#'  \code{\link{validate_common_args}()}.  Default: \code{8525}.
#' @param combine Logical; when \code{TRUE} (default), returns a combined
#'  \code{patchwork} object.  When \code{FALSE}, returns a named list of
#'  individual \code{ggplot} objects.
#' @param ncol,nrow Integer number of columns / rows for the combined layout
#'  (passed to \code{\link[patchwork]{wrap_plots}}).
#' @param byrow Logical; fill the combined layout by row.  Default
#'  \code{TRUE} (passed to \code{\link[patchwork]{wrap_plots}}).
#' @param axes A character string specifying how axes should be treated
#'  across the combined layout (passed to \code{\link[patchwork]{wrap_plots}}).
#' @param axis_titles A character string specifying how axis titles should be
#'  treated across the combined layout.  Defaults to \code{axes}.
#' @param guides A character string specifying how guides (legends) should be
#'  collected across panels (passed to \code{\link{combine_plots}()}).
#' @param design A custom layout design for the combined plot (passed to
#'  \code{\link{combine_plots}()}).
#' @return A \code{ggplot} object (when \code{split_by} is \code{NULL}), a
#'  \code{patchwork} object (when \code{split_by} is provided and
#'  \code{combine = TRUE}), or a named list of \code{ggplot} objects (when
#'  \code{combine = FALSE}).  All \code{ggplot} objects have \code{height}
#'  and \code{width} attributes in inches.
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
    data,
    x,
    x_sep = "_",
    y = NULL,
    in_form = c("long", "wide"),
    split_by = NULL,
    split_by_sep = "_",
    keep_na = FALSE,
    keep_empty = FALSE,
    sort_x = c(
        "none",
        "mean_asc",
        "mean_desc",
        "mean",
        "median_asc",
        "median_desc",
        "median"
    ),
    flip = FALSE,
    group_by = NULL,
    group_by_sep = "_",
    group_name = NULL,
    x_text_angle = 0,
    order_by = "-({y}^2 + {size_by}^2)",
    theme = "theme_this",
    theme_args = list(),
    palette = "Paired",
    palcolor = NULL,
    palreverse = FALSE,
    alpha = 1,
    aspect.ratio = NULL,
    legend.position = "right",
    legend.direction = "vertical",
    shape = 21,
    border = "black",
    size_by = 2,
    size_name = NULL,
    size_trans = NULL,
    y_nbreaks = 4,
    jitter_width = 0.5,
    jitter_height = 0,
    y_max = NULL,
    y_min = NULL,
    y_trans = "identity",
    add_bg = FALSE,
    bg_palette = "stripe",
    bg_palcolor = NULL,
    bg_alpha = 0.2,
    add_hline = NULL,
    hline_type = "solid",
    hline_width = 0.5,
    hline_color = "black",
    hline_alpha = 1,
    labels = NULL,
    label_by = NULL,
    nlabel = 5,
    label_size = 3,
    label_fg = "black",
    label_bg = "white",
    label_bg_r = 0.1,
    highlight = NULL,
    highlight_color = "red2",
    highlight_size = 1,
    highlight_alpha = 1,
    facet_by = NULL,
    facet_scales = "fixed",
    facet_ncol = NULL,
    facet_nrow = NULL,
    facet_byrow = TRUE,
    title = NULL,
    subtitle = NULL,
    xlab = NULL,
    ylab = NULL,
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
    validate_common_args(seed)
    keep_na <- check_keep_na(keep_na, c(x, split_by, group_by, facet_by))
    keep_empty <- check_keep_empty(
        keep_empty,
        c(x, split_by, group_by, facet_by)
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
            JitterPlotAtomic(
                datas[[nm]],
                x = x,
                x_sep = x_sep,
                y = y,
                in_form = in_form,
                keep_na = keep_na,
                keep_empty = keep_empty,
                sort_x = sort_x,
                flip = flip,
                group_by = group_by,
                group_by_sep = group_by_sep,
                group_name = group_name,
                x_text_angle = x_text_angle,
                theme = theme,
                theme_args = theme_args,
                palette = palette[[nm]],
                palcolor = palcolor[[nm]],
                palreverse = palreverse,
                alpha = alpha,
                aspect.ratio = aspect.ratio,
                legend.position = legend.position[[nm]],
                legend.direction = legend.direction[[nm]],
                shape = shape,
                border = border,
                order_by = order_by,
                size_by = size_by,
                size_name = size_name,
                size_trans = size_trans,
                y_nbreaks = y_nbreaks,
                jitter_width = jitter_width,
                jitter_height = jitter_height,
                y_max = y_max,
                y_min = y_min,
                y_trans = y_trans,
                add_bg = add_bg,
                bg_palette = bg_palette,
                bg_palcolor = bg_palcolor,
                bg_alpha = bg_alpha,
                add_hline = add_hline,
                hline_type = hline_type,
                hline_width = hline_width,
                hline_color = hline_color,
                hline_alpha = hline_alpha,
                labels = labels,
                label_by = label_by,
                nlabel = nlabel,
                label_size = label_size,
                label_fg = label_fg,
                label_bg = label_bg,
                label_bg_r = label_bg_r,
                highlight = highlight,
                highlight_color = highlight_color,
                highlight_size = highlight_size,
                highlight_alpha = highlight_alpha,
                facet_by = facet_by,
                facet_scales = facet_scales,
                facet_ncol = facet_ncol,
                facet_nrow = facet_nrow,
                facet_byrow = facet_byrow,
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
