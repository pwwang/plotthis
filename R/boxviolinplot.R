#' Atomic Box / Violin / Bar / Beeswarm plot (internal)
#'
#' @description
#' Core implementation for drawing box plots, violin plots, bar plots (mean
#' ± error bars), or beeswarm plots.  This is the workhorse behind
#' \code{\link{BoxPlot}}, \code{\link{ViolinPlot}}, and
#' \code{\link{BeeswarmPlot}} — it takes a **single** data frame (no
#' \code{split_by} support) and returns a \code{ggplot} object.
#'
#' The \code{base} parameter selects the primary geometry:
#' \itemize{
#'   \item \code{"box"} — \code{geom_boxplot()}
#'   \item \code{"violin"} — \code{geom_violin()}
#'   \item \code{"bar"} — \code{stat_summary(fun = mean, geom = "col")}
#'         with optional error bars (SEM, SD, or CI).
#'   \item \code{"none"} — no primary geometry (used by
#'         \code{\link{BeeswarmPlot}} to draw beeswarm points alone).
#' }
#'
#' @section Architecture:
#' \enumerate{
#'   \item \strong{Wide-to-long conversion} — when \code{in_form = "wide"},
#'         data is pivoted via \code{tidyr::pivot_longer()}.  The
#'         \code{keep_na} / \code{keep_empty} lists are filtered to the new
#'         column names.
#'   \item \strong{Column resolution} — \code{x}, \code{y}, \code{group_by},
#'         \code{facet_by}, and \code{paired_by} are validated via
#'         \code{\link{check_columns}}.
#'   \item \strong{NA / empty-level handling} — per-column
#'         \code{keep_empty} settings are extracted for \code{x},
#'         \code{group_by}, and \code{facet_by} independently.
#'   \item \strong{Beeswarm validation} — if \code{add_beeswarm = TRUE},
#'         the \code{ggbeeswarm} package is required.  Beeswarm is disabled
#'         (with a warning) when \code{paired_by} is provided.
#'   \item \strong{Paired data validation} — structural checks ensure each
#'         (\code{x}, \code{paired_by}) combination has exactly 2
#'         observations (one per group when \code{group_by} is present).
#'         Paired observations force \code{add_point = TRUE}.
#'   \item \strong{Summary statistics} — \code{.y_mean} and
#'         \code{.y_median} are pre-computed per (\code{x}, \code{group_by},
#'         \code{facet_by}) for use in trend lines and fill modes.
#'   \item \strong{y-axis limits} — \code{y_max} / \code{y_min} accept
#'         numeric values or quantile notation (\code{"q95"},
#'         \code{"q5"}).  For bar plots, the limit is extended upward by
#'         the error bar extent.
#'   \item \strong{Highlight} — \code{highlight} can be \code{TRUE} (all
#'         points), a numeric index vector, a logical expression string, or
#'         a character vector of row names.
#'   \item \strong{sort_x} — an R expression string (e.g.,
#'         \code{"mean(y)"}) evaluated per x-level to reorder categories.
#'   \item \strong{Flip transformation} — when \code{flip = TRUE}, factor
#'         levels are reversed and \code{aspect.ratio} is inverted.
#'   \item \strong{Base geometry} — the primary geom is added:
#'         \code{geom_boxplot()}, \code{geom_violin()}, or
#'         \code{stat_summary(fun = mean, geom = "col")}.  Error bars
#'         (SEM / SD / CI) are layered on bar plots via a custom
#'         \code{stat_summary(fun.data = ...)}.
#'   \item \strong{Fill mode} — \code{fill_mode} controls colour mapping:
#'         \itemize{
#'           \item \code{"dodge"} — fill by \code{group_by} (discrete).
#'           \item \code{"x"} — fill by x-axis categories (discrete).
#'           \item \code{"mean"} / \code{"median"} — fill by pre-computed
#'                 mean/median (continuous gradient).
#'         }
#'   \item \strong{Box overlay} — when \code{add_box = TRUE} on a non-box
#'         base, a box plot is overlaid via \code{ggnewscale::new_scale_fill()}
#'         with a white fill/black outline.
#'   \item \strong{Statistical comparisons} — two pathways:
#'         \itemize{
#'           \item \strong{Pairwise} (\code{comparisons}) — uses
#'                 \code{ggpubr::geom_pwc()} with automatic or explicit
#'                 comparison pairs.  Data is preprocessed to avoid test
#'                 failures from zero-variance or all-NA groups.
#'           \item \strong{Multiple-group} (\code{multiplegroup_comparisons})
#'                 — uses \code{ggpubr::stat_compare_means()} for omnibus
#'                 tests (e.g., Kruskal-Wallis).
#'         }
#'         After comparison layers are added, \code{y_max_use} is expanded to
#'         accommodate significance brackets.
#'   \item \strong{Points} — jittered points (\code{geom_point()} with
#'         \code{position_jitterdodge}) or beeswarm points
#'         (\code{ggbeeswarm::geom_beeswarm()}).  Paired observations
#'         add connecting lines (\code{geom_line()}) between matched
#'         subjects.
#'   \item \strong{Trend lines} — \code{stat_summary(fun = first)} draws
#'         lines connecting group medians.  When \code{trend_color} is
#'         \code{NULL} and \code{group_by} is present, lines are coloured
#'         per group.
#'   \item \strong{Reference lines} — \code{geom_hline()} at the specified
#'         y-intercept.
#'   \item \strong{Stat summary points} — a custom \code{stat_summary()}
#'         point layer displaying a user-specified summary statistic
#'         (e.g., mean) with a shape legend entry.
#'   \item \strong{Stack layout} — when \code{stack = TRUE}, facets are
#'         arranged with shared strip labels and negative panel spacing for
#'         a compact stacked appearance.
#'   \item \strong{Dimension calculation} — \code{calculate_plot_dimensions()}
#'         accounts for the number of x-levels × dodge groups, flip state,
#'         and stack layout adjustments.  Minimum dimensions are enforced
#'         from label character widths.
#'   \item \strong{Faceting} — \code{\link{facet_plot}()} wraps the result
#'         with the appropriate strip position based on flip/stack state.
#' }
#'
#' @inheritParams common_args
#' @param base A character string specifying the primary plot type:
#'  \code{"box"} (box plot), \code{"violin"} (violin plot), \code{"bar"}
#'  (mean bars with optional error bars), or \code{"none"} (no primary
#'  geometry, used by beeswarm plots).
#' @param x A character string of column name(s) for the x-axis.
#'  Character/factor columns are expected.  Multiple columns are
#'  concatenated with \code{x_sep}.
#' @param x_sep A character string to join multiple \code{x} columns.
#'  Default \code{"_"}.
#' @param y A character string of the numeric column for the y-axis.
#'  Not required when \code{in_form = "wide"} (data values are taken from
#'  the \code{x} columns).
#' @param in_form A character string: \code{"long"} (default) or
#'  \code{"wide"}.  In wide form, \code{x} columns are pivoted to long
#'  format.
#' @param sort_x An R expression string (e.g., \code{"mean(y)"}) to order
#'  x-axis categories.  Default \code{NULL} keeps the original order.
#'  When \code{keep_empty_x} is \code{TRUE}, empty levels are placed last.
#' @param flip Logical; if \code{TRUE}, swap the x and y axes.
#' @param group_by A character vector of column name(s) to dodge the
#'  boxes/violins by.  Multiple columns are concatenated with
#'  \code{group_by_sep}.
#' @param group_by_sep A character string to separate concatenated
#'  \code{group_by} columns.  Default \code{"_"}.
#' @param group_name A character string for the dodge legend title.
#' @param paired_by A character string naming a column that identifies
#'  paired observations.  Forces \code{add_point = TRUE} and connects
#'  paired observations with lines.
#' @param fill_mode A character string controlling fill colour mapping:
#'  \code{"dodge"} (fill by \code{group_by}, discrete),
#'  \code{"x"} (fill by x-axis categories, discrete),
#'  \code{"mean"} or \code{"median"} (fill by pre-computed statistic,
#'  continuous gradient).
#' @param position_dodge_preserve Passed to
#'  \code{\link[ggplot2]{position_dodge}()}: \code{"total"} preserves the
#'  overall group width; \code{"single"} preserves individual element width.
#' @param add_point Logical; add jittered or beeswarm points to the plot.
#' @param pt_color Colour of the points.  When \code{add_beeswarm = TRUE}
#'  and \code{pt_color} is \code{NULL}, points are coloured by the fill
#'  variable.
#' @param pt_size Numeric size of the points.  Default computed from
#'  data size: \code{min(3000 / nrow(data), 0.6)}.
#' @param pt_alpha Numeric transparency of the points.
#' @param jitter_width Numeric width of the jitter.  Defaults to
#'  \code{0.5}, but set to \code{0} when \code{paired_by} is provided.
#' @param jitter_height Numeric height of the jitter.  Default \code{0}.
#' @param add_beeswarm Logical; use \code{ggbeeswarm::geom_beeswarm()} for
#'  non-overlapping point layout instead of jitter.  Requires the
#'  \code{ggbeeswarm} package.
#' @param beeswarm_method Beeswarm layout method: \code{"swarm"},
#'  \code{"compactswarm"}, \code{"hex"}, \code{"square"}, or
#'  \code{"center"}.
#' @param beeswarm_cex Numeric scaling for point spacing.  Larger values
#'  spread points more.
#' @param beeswarm_priority Point layout priority: \code{"ascending"},
#'  \code{"descending"}, \code{"density"}, or \code{"random"}.
#' @param beeswarm_dodge Numeric dodge width for beeswarm points when
#'  \code{group_by} is provided.  Default \code{0.9}.
#' @param add_box Logical; overlay a box plot on the primary geometry.
#'  Mutually exclusive with \code{base = "box"} and \code{base = "bar"}.
#' @param box_color Colour of the overlaid box plot outline and fill.
#' @param box_width Width of the overlaid box plot.
#' @param box_ptsize Size of the median point in the overlaid box plot.
#' @param add_errorbar Type of error bars for bar plots (\code{base = "bar"}):
#'  \code{"SEM"} (standard error of the mean, default), \code{"SD"}
#'  (standard deviation), \code{"CI"} or \code{"CI95"} (95\% confidence
#'  interval), or \code{"none"}.  Silently ignored for non-bar bases.
#' @param errorbar_color Colour of the error bar lines and caps.
#' @param errorbar_width Width of the error bar caps.
#' @param errorbar_linewidth Line width of the error bars.
#' @param add_trend Logical; add trend lines connecting group medians.
#' @param trend_color Colour of the trend line.  When \code{NULL} and
#'  \code{group_by} is present, lines are coloured per group.
#' @param trend_linewidth Width of the trend line.
#' @param trend_ptsize Size of the trend line points.
#' @param add_stat A summary function (e.g., \code{mean}, \code{median}) to
#'  display as a point with a shape legend entry.
#' @param stat_name Legend title for the stat summary shape.
#' @param stat_color Colour of the stat summary point.
#' @param stat_size Size of the stat summary point.
#' @param stat_stroke Stroke width of the stat summary point.
#' @param stat_shape Shape (an integer) for the stat summary point.  Uses
#'  \code{scale_shape_identity()} so the shape is rendered directly.
#' @param add_bg Logical; add alternating background stripes.
#' @param bg_palette Palette for the background stripes.
#' @param bg_palcolor Custom colours for the background stripes.
#' @param bg_alpha Alpha transparency for the background stripes.
#' @param add_line A numeric y-intercept for a horizontal reference line.
#' @param line_color Colour of the reference line.
#' @param line_width Width of the reference line.
#' @param line_type Linetype of the reference line.
#' @param highlight A specification of points to highlight: \code{TRUE}
#'  (all), a numeric index vector, a logical expression string, or a
#'  character vector of row names.
#' @param highlight_color Colour of highlighted points.
#' @param highlight_size Size of highlighted points.
#' @param highlight_alpha Alpha of highlighted points.
#' @param comparisons A logical value (\code{TRUE} for all pairs) or a list
#'  of two-element vectors specifying pairwise comparisons.  Only available
#'  when \code{fill_mode = "dodge"} (i.e., \code{group_by} is present).
#' @param ref_group A character string specifying the reference group for
#'  comparisons.
#' @param pairwise_method Method for pairwise tests.  Default
#'  \code{"wilcox.test"}.
#' @param multiplegroup_comparisons Logical; perform an omnibus test
#'  (e.g., Kruskal-Wallis) across all groups.
#' @param multiple_method Method for the omnibus test.  Default
#'  \code{"kruskal.test"}.
#' @param sig_label Label format for significance annotations.  For
#'  pairwise comparisons: \code{"p.format"}, \code{"p.signif"}, or a
#'  \pkg{glue} template (e.g., \code{"p = {p}"}).  For multiple-group
#'  tests: \code{"p.format"} or \code{"p.signif"}.
#' @param sig_labelsize Size of the significance label text.
#' @param hide_ns Logical; hide non-significant comparison labels.
#' @param symnum_args A list of arguments passed to
#'  \code{\link[stats]{symnum}} for symbolic p-value coding.
#' @param step_increase Fractional step increase for stacking significance
#'  brackets when multiple comparisons exist.
#' @param stack Logical; stack facetted panels in a compact layout with
#'  shared strip labels.
#' @param y_max,y_min Numeric y-axis limits, or quantile notation strings
#'  (e.g., \code{"q95"} for the 95th percentile, \code{"q5"} for the
#'  5th percentile).
#' @param y_trans A character string for y-axis transformation
#'  (e.g., \code{"log10"}).
#' @param y_nbreaks Integer number of y-axis breaks.
#' @return A \code{ggplot} object, possibly faceted, with \code{height}
#'  and \code{width} attributes (in inches) attached.
#' @param y_brackets Numeric y-axis position for significance brackets
#'  (or p-value labels for multiple comparisons).  If NULL, the brackets are placed above the maximum y-value.
#' @keywords internal
#' @importFrom utils combn
#' @importFrom stats median quantile sd qt
#' @importFrom rlang sym syms parse_expr
#' @importFrom dplyr mutate ungroup first
#' @importFrom ggplot2 geom_boxplot geom_violin geom_point geom_line geom_hline geom_vline layer_data geom_col geom_errorbar
#' @importFrom ggplot2 scale_fill_manual scale_color_manual scale_shape_manual scale_linetype_manual stat_summary
#' @importFrom ggplot2 labs theme element_line element_text position_dodge position_jitter coord_flip layer_scales
#' @importFrom ggplot2 position_jitterdodge scale_shape_identity scale_size_manual scale_alpha_manual scale_y_continuous
BoxViolinPlotAtomic <- function(
    data,
    x,
    x_sep = "_",
    y = NULL,
    base = c("box", "violin", "bar", "none"),
    in_form = c("long", "wide"),
    sort_x = NULL,
    flip = FALSE,
    keep_empty = FALSE,
    keep_na = FALSE,
    group_by = NULL,
    group_by_sep = "_",
    group_name = NULL,
    paired_by = NULL,
    x_text_angle = ifelse(isTRUE(flip), 0, 45),
    step_increase = 0.1,
    position_dodge_preserve = "total",
    fill_mode = ifelse(!is.null(group_by), "dodge", "x"),
    palreverse = FALSE,
    symnum_args = NULL,
    theme = "theme_this",
    theme_args = list(),
    palette = "Paired",
    palcolor = NULL,
    alpha = 1,
    aspect.ratio = NULL,
    legend.position = "right",
    legend.direction = "vertical",
    add_point = FALSE,
    pt_color = if (isTRUE(add_beeswarm)) NULL else "grey30",
    pt_size = NULL,
    pt_alpha = 1,
    y_nbreaks = 4,
    jitter_width = NULL,
    jitter_height = 0,
    stack = FALSE,
    y_max = NULL,
    y_min = NULL,
    y_trans = "identity",
    y_brackets = NULL,
    add_beeswarm = FALSE,
    beeswarm_method = "swarm",
    beeswarm_cex = 1,
    beeswarm_priority = "ascending",
    beeswarm_dodge = 0.9,
    add_box = FALSE,
    box_color = "black",
    box_width = 0.1,
    box_ptsize = 2.5,
    add_errorbar = "SEM",
    errorbar_color = "grey20",
    errorbar_width = 0.4,
    errorbar_linewidth = 0.6,
    add_trend = FALSE,
    trend_color = NULL,
    trend_linewidth = 1,
    trend_ptsize = 2,
    add_stat = NULL,
    stat_name = NULL,
    stat_color = "black",
    stat_size = 1,
    stat_stroke = 1,
    stat_shape = 25,
    add_bg = FALSE,
    bg_palette = "stripe",
    bg_palcolor = NULL,
    bg_alpha = 0.2,
    add_line = NULL,
    line_color = "red2",
    line_width = .6,
    line_type = 2,
    highlight = NULL,
    highlight_color = "red2",
    highlight_size = 1,
    highlight_alpha = 1,
    comparisons = NULL,
    ref_group = NULL,
    pairwise_method = "wilcox.test",
    multiplegroup_comparisons = FALSE,
    multiple_method = "kruskal.test",
    sig_label = "p.format",
    sig_labelsize = 3.5,
    hide_ns = FALSE,
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
            pivot_longer(cols = x, names_to = ".x", values_to = ".y")
        x <- ".x"
        y <- ".y"
        # if all values in keep_na are FALSE that means it is default
        if (is.list(keep_na) && all(sapply(keep_na, isFALSE))) {
            keep_na <- NULL
        } else if (
            is.list(keep_na) && length(setdiff(names(keep_na), c(x, y))) > 0
        ) {
            warning(
                "[Box/Violin/BeeswarmPlot] Ignoring `keep_na` for columns other than `.x` and `.y` when `in_form` is 'wide'."
            )
            keep_na <- keep_na[names(keep_na) %in% c(x, y)]
        }
        if (is.list(keep_empty) && all(sapply(keep_empty, isFALSE))) {
            keep_empty <- NULL
        } else if (
            is.list(keep_empty) && length(setdiff(names(keep_empty), x)) > 0
        ) {
            warning(
                "[Box/Violin/BeeswarmPlot] Ignoring `keep_empty` for columns other than `.x` when `in_form` is 'wide'."
            )
            keep_empty <- keep_empty[names(keep_empty) %in% x]
        }
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
    paired_by <- check_columns(data, paired_by, force_factor = TRUE)
    base_size <- theme_args$base_size %||% 12
    sig_labelsize <- sig_labelsize * base_size / 12

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
            "[Box/Violin/BeeswarmPlot] `keep_empty` for `facet_by` variables must be identical." = identical(
                keep_empty_facet,
                keep_empty[[facet_by[2]]]
            )
        )
    }

    # Validate beeswarm parameters
    if (isTRUE(add_beeswarm)) {
        if (!requireNamespace("ggbeeswarm", quietly = TRUE)) {
            stop(
                "Package 'ggbeeswarm' is required for beeswarm plots. Please install it with: install.packages('ggbeeswarm')"
            )
        }
        add_point <- TRUE
        if (!is.null(paired_by)) {
            warning(
                "'add_beeswarm' is not fully compatible with 'paired_by'. Using jittered points instead for paired data."
            )
            add_beeswarm <- FALSE
        }
    }

    if (!is.null(paired_by)) {
        if (!isTRUE(add_point)) {
            warning("Forcing 'add_point' = TRUE when 'paired_by' is provided.")
            add_point <- TRUE
        }

        if (any(is.na(data[[paired_by]]))) {
            warning(
                "'paired_by' contains missing values, removing corresponding rows."
            )
            data <- data[!is.na(data[[paired_by]]), , drop = FALSE]
        }
        n_total_col <- paste0(".n_total_", paired_by)
        sym_ntc <- sym(n_total_col)
        if (!is.null(group_by)) {
            # We should have exactly two groups for each x value
            # and for a pair, the two observations must belong to different groups
            # and the same paired_by value
            problem_groups <- data %>%
                dplyr::group_by(!!!syms(c(x, paired_by, group_by))) %>%
                dplyr::summarise(.n = dplyr::n(), .groups = "drop") %>%
                dplyr::add_count(
                    !!!syms(c(x, paired_by)),
                    name = n_total_col
                ) %>%
                dplyr::filter(!!sym(".n") != 1 | !!sym_ntc != 2) %>%
                dplyr::mutate(
                    .n = ifelse(
                        !!sym(".n") == 1,
                        !!sym(".n"),
                        paste0(!!sym(".n"), " (expecting 1)")
                    ),
                    !!sym_ntc := ifelse(
                        !!sym_ntc == 2,
                        !!sym_ntc,
                        paste0(!!sym_ntc, " (expecting 2)")
                    )
                )
            # If not, indicate which group (x, paired_by) has the problem
            if (nrow(problem_groups) > 0) {
                stop(
                    "When 'paired_by' and 'group_by' are both provided, each combination of 'x' and 'paired_by' must have exactly two observations, one for each group in 'group_by'. The following combinations do not satisfy this requirement:\n",
                    paste0(
                        apply(
                            problem_groups[, c(
                                x,
                                paired_by,
                                group_by,
                                ".n",
                                n_total_col
                            )],
                            1,
                            function(row) {
                                paste(
                                    paste(names(row), row, sep = "="),
                                    collapse = ", "
                                )
                            }
                        ),
                        collapse = "\n"
                    )
                )
            }
        } else if (dplyr::n_distinct(data[[x]], na.rm = TRUE) != 2) {
            stop(
                "Exactly two unique values of 'x' are required when 'paired_by' is provided without 'group_by'."
            )
        } else {
            problem_groups <- data %>%
                dplyr::group_by(!!!syms(c(x, paired_by))) %>%
                dplyr::summarise(.n = dplyr::n(), .groups = "drop") %>%
                dplyr::add_count(!!!syms(paired_by), name = n_total_col) %>%
                dplyr::filter(!!sym(".n") != 1 | !!sym_ntc != 2) %>%
                dplyr::mutate(
                    .n = ifelse(
                        !!sym(".n") == 1,
                        !!sym(".n"),
                        paste0(!!sym(".n"), " (expecting 1)")
                    ),
                    !!sym_ntc := ifelse(
                        !!sym_ntc == 2,
                        !!sym_ntc,
                        paste0(!!sym_ntc, " (expecting 2)")
                    )
                )
            if (nrow(problem_groups) > 0) {
                stop(
                    "When 'paired_by' is provided without 'group_by', each combination of 'x' and 'paired_by' must have exactly two observations, one for each value of 'x'. The following combinations do not satisfy this requirement:\n",
                    paste0(
                        apply(
                            problem_groups[, c(
                                x,
                                paired_by,
                                ".n",
                                n_total_col
                            )],
                            1,
                            function(row) {
                                paste(
                                    paste(names(row), row, sep = "="),
                                    collapse = ", "
                                )
                            }
                        ),
                        collapse = "\n"
                    )
                )
            }
        }

        # For paired tests, ensure data is sorted by paired_by so that
        # corresponding observations across groups are in the same order
        data <- data %>%
            dplyr::arrange(!!!syms(unique(c(paired_by, x, group_by))))
    }
    if (isTRUE(comparisons) && is.null(group_by)) {
        # stop("'group_by' must be provided to when 'comparisons' is TRUE.")
        comparisons <- combn(levels(data[[x]]), 2, simplify = FALSE)
    }
    if (length(comparisons) > 0) {
        if (!is.list(comparisons) && !isTRUE(comparisons)) {
            comparisons <- list(comparisons)
        }
        ncomp <- sapply(comparisons, length)
        if (any(ncomp) > 2) {
            stop(
                "'comparisons' must be a list in which all elements must be vectors of length 2"
            )
        }
    }
    if (!isFALSE(multiplegroup_comparisons)) {
        stopifnot(
            "'sig_label' must be 'p.format' or 'p.signif' when 'multiplegroup_comparisons' is TRUE." = sig_label %in%
                c("p.format", "p.signif")
        )
    }

    orig_data <- data
    data <- data %>%
        dplyr::group_by(!!!syms(unique(c(x, group_by, facet_by)))) %>%
        mutate(.y_mean = mean(!!sym(y)), .y_median = median(!!sym(y))) %>%
        ungroup()

    # keep the factor levels
    for (col in unique(c(x, group_by, facet_by))) {
        data[[col]] <- factor(data[[col]], levels = levels(orig_data[[col]]))
    }
    rm(orig_data)

    values <- data[[y]][is.finite(data[[y]])]
    if (is.character(y_max)) {
        q_max <- as.numeric(sub("(^q)(\\d+)", "\\2", y_max)) / 100
        y_max_use <- quantile(values, q_max, na.rm = TRUE)
    } else {
        y_max_use <- max(values, na.rm = TRUE)
    }
    if (is.null(y_min)) {
        y_min_use <- min(values, na.rm = TRUE)
    } else if (is.character(y_min)) {
        q_min <- as.numeric(sub("(^q)(\\d+)", "\\2", y_min)) / 100
        y_min_use <- quantile(values, q_min, na.rm = TRUE)
    } else {
        y_min_use <- y_min
    }
    rm(values)

    if (!is.null(highlight)) {
        if (isTRUE(highlight)) {
            data$.highlight <- TRUE
        } else if (is.numeric(highlight)) {
            data$.highlight <- 1:nrow(data) %in% highlight
        } else if (is.character(highlight) && length(highlight) == 1) {
            data <- mutate(data, .highlight = !!parse_expr(highlight))
        } else if (is.null(rownames(data))) {
            stop(
                "No row names in the data, please provide a vector of indexes to highlight."
            )
        } else {
            data$.highlight <- rownames(data) %in% highlight
        }
        if (isFALSE(add_point)) {
            warning("Forcing add_point = TRUE when highlight is provided.")
            add_point <- TRUE
        }
    } else {
        data$.highlight <- FALSE
    }
    data$.highlight <- factor(
        as.character(data$.highlight),
        levels = c("TRUE", "FALSE")
    )

    if (!is.null(sort_x)) {
        x_levels <- data %>%
            dplyr::group_by(!!sym(x)) %>%
            dplyr::summarise(
                .sort_x = !!rlang::parse_expr(sort_x),
                .groups = "drop"
            ) %>%
            dplyr::arrange(!!sym(".sort_x")) %>%
            dplyr::pull(!!sym(x)) %>%
            as.character()

        if (!isFALSE(keep_empty_x)) {
            x_levels <- c(x_levels, setdiff(levels(data[[x]]), x_levels))
        }
        data[[x]] <- factor(data[[x]], levels = x_levels)
    }

    if (isTRUE(flip)) {
        data[[x]] <- factor(data[[x]], levels = rev(levels(data[[x]])))
        aspect.ratio <- 1 / aspect.ratio
        if (length(aspect.ratio) == 0 || is.na(aspect.ratio)) {
            aspect.ratio <- NULL
        }
    }

    base <- match.arg(base)
    if (isTRUE(add_box) && base == "box") {
        stop("Cannot add box plot to box plot.")
    }
    if (isTRUE(add_box) && base == "bar") {
        stop("Cannot add box plot to bar plot.")
    }

    # Validate and parse add_errorbar
    add_errorbar <- toupper(as.character(add_errorbar))
    if (add_errorbar != "NONE" && base != "bar") {
        # Silently ignore add_errorbar for non-bar bases
        add_errorbar <- "NONE"
    }
    if (base == "bar" && add_errorbar != "NONE") {
        if (add_errorbar == "CI") {
            add_errorbar <- "CI95"
        }
        if (
            !add_errorbar %in% c("SEM", "SD") &&
                !grepl("^CI\\d+$", add_errorbar)
        ) {
            stop(
                "'add_errorbar' must be one of 'SEM', 'SD', 'CI', 'CIXX' (e.g., 'CI95'), or 'none'. Got: '",
                add_errorbar,
                "'."
            )
        }
    }

    # For bar plots, adjust y_max_use/y_min_use based on mean + errorbar extent
    if (base == "bar" && !isTRUE(add_point)) {
        grp_cols <- unique(c(x, group_by, facet_by))
        grp_stats <- data %>%
            dplyr::group_by(!!!syms(grp_cols)) %>%
            dplyr::summarise(
                .mean = mean(!!sym(y), na.rm = TRUE),
                .sd = sd(!!sym(y), na.rm = TRUE),
                .n = dplyr::n(),
                .groups = "drop"
            )
        if (add_errorbar == "NONE") {
            bar_max <- max(grp_stats$.mean, na.rm = TRUE)
        } else if (add_errorbar == "SEM") {
            bar_max <- max(
                grp_stats$.mean + grp_stats$.sd / sqrt(grp_stats$.n),
                na.rm = TRUE
            )
        } else if (add_errorbar == "SD") {
            bar_max <- max(grp_stats$.mean + grp_stats$.sd, na.rm = TRUE)
        } else {
            ci_level <- as.numeric(sub("^CI", "", add_errorbar)) / 100
            bar_max <- max(
                grp_stats$.mean +
                    qt((1 + ci_level) / 2, df = pmax(grp_stats$.n - 1, 1)) *
                        grp_stats$.sd /
                        sqrt(grp_stats$.n),
                na.rm = TRUE
            )
        }
        if (is.null(y_max) || is.character(y_max)) {
            y_max_use <- bar_max
        }
        if (is.null(y_min)) {
            y_min_use <- 0
        }
    }

    fill_mode <- match.arg(fill_mode, c("dodge", "x", "mean", "median"))
    if (fill_mode == "dodge") {
        fill_by <- group_by
    } else if (fill_mode == "x") {
        fill_by <- x
    } else if (fill_mode == "mean") {
        fill_by <- ".y_mean"
    } else {
        fill_by <- ".y_median"
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

    if (base == "box" || (base == "none" && isTRUE(add_box))) {
        p <- p +
            geom_boxplot(
                position = position_dodge(
                    width = 0.9,
                    preserve = position_dodge_preserve
                ),
                color = "black",
                width = 0.8,
                outlier.shape = NA,
                show.legend = TRUE
            )
    } else if (base == "violin") {
        p <- p +
            geom_violin(
                # There is a bug in ggplot2 with preserve = "single" for violin plots
                # See https://github.com/tidyverse/ggplot2/issues/2801
                # There is a fix but not yet released
                position = position_dodge(
                    width = 0.9,
                    preserve = position_dodge_preserve
                ),
                scale = "width",
                trim = TRUE,
                alpha = alpha,
                width = 0.8,
                show.legend = TRUE
            )
    } else if (base == "bar") {
        p <- p +
            stat_summary(
                fun = mean,
                geom = "col",
                position = position_dodge(
                    width = 0.9,
                    preserve = position_dodge_preserve
                ),
                width = 0.8,
                alpha = alpha,
                color = "black",
                show.legend = TRUE
            )
        if (add_errorbar != "NONE") {
            errorbar_fun <- function(y) {
                y <- y[!is.na(y)]
                m <- mean(y)
                n <- length(y)
                s <- sd(y)
                if (n < 2 || is.na(s)) {
                    return(data.frame(y = m, ymin = m, ymax = m))
                }
                if (add_errorbar == "SEM") {
                    se <- s / sqrt(n)
                    data.frame(y = m, ymin = m - se, ymax = m + se)
                } else if (add_errorbar == "SD") {
                    data.frame(y = m, ymin = m - s, ymax = m + s)
                } else {
                    # CI: extract level from e.g. "CI95"
                    ci_level <- as.numeric(sub("^CI", "", add_errorbar)) / 100
                    t_crit <- qt((1 + ci_level) / 2, df = n - 1)
                    me <- t_crit * s / sqrt(n)
                    data.frame(y = m, ymin = m - me, ymax = m + me)
                }
            }
            p <- p +
                stat_summary(
                    fun.data = errorbar_fun,
                    geom = "errorbar",
                    position = position_dodge(
                        width = 0.9,
                        preserve = position_dodge_preserve
                    ),
                    width = errorbar_width,
                    color = errorbar_color,
                    linewidth = errorbar_linewidth,
                    show.legend = FALSE
                )
        }
    }
    if (fill_mode == "dodge") {
        group_vals <- levels(data[[group_by]])
        if (anyNA(data[[group_by]])) {
            group_vals <- c(group_vals, NA)
        }
        group_colors <- palette_this(
            group_vals,
            palette = palette,
            palcolor = palcolor,
            reverse = palreverse,
            NA_keep = TRUE
        )

        if (isTRUE(keep_empty_group)) {
            p <- p +
                scale_fill_manual(
                    name = group_name %||% group_by,
                    values = group_colors,
                    na.value = group_colors['NA'] %||% "grey80",
                    breaks = group_vals,
                    limits = group_vals,
                    drop = FALSE
                )
        } else {
            p <- p +
                scale_fill_manual(
                    name = group_name %||% group_by,
                    values = group_colors,
                    na.value = group_colors['NA'] %||% "grey80"
                )
        }
    } else if (fill_mode == "x") {
        x_vals <- levels(data[[x]])
        if (anyNA(data[[x]])) {
            x_vals <- c(x_vals, NA)
        }
        x_colors <- palette_this(
            x_vals,
            palette = palette,
            palcolor = palcolor,
            reverse = palreverse,
            NA_keep = TRUE
        )

        if (isTRUE(keep_empty_x)) {
            p <- p +
                scale_fill_manual(
                    name = x,
                    values = x_colors,
                    na.value = x_colors['NA'] %||% "grey80",
                    breaks = x_vals,
                    limits = x_vals,
                    drop = FALSE
                )
        } else {
            p <- p +
                scale_fill_manual(
                    name = x,
                    values = x_colors,
                    na.value = x_colors['NA'] %||% "grey80"
                )
        }
    } else {
        p <- p +
            scale_fill_gradientn(
                name = paste0(y, " (", fill_mode, ")"),
                n.breaks = 3,
                colors = palette_this(
                    palette = palette,
                    palcolor = palcolor,
                    reverse = palreverse
                ),
                na.value = "grey80",
                guide = guide_colorbar(
                    frame.colour = "black",
                    ticks.colour = "black",
                    title.hjust = 0
                )
            )
    }

    # when base is none, boxes are added as base
    if (isTRUE(add_box) && !base %in% c("none", "bar")) {
        p <- p +
            new_scale_fill() +
            geom_boxplot(
                position = position_dodge(
                    width = 0.9,
                    preserve = position_dodge_preserve
                ),
                fill = box_color,
                color = box_color,
                width = box_width,
                show.legend = FALSE,
                outlier.shape = NA
            ) +
            stat_summary(
                fun = first,
                geom = "point",
                mapping = aes(y = !!sym(".y_median")),
                position = position_dodge(
                    width = 0.9,
                    preserve = position_dodge_preserve
                ),
                color = "black",
                fill = "white",
                size = box_ptsize,
                shape = 21
            )
    }

    if (length(comparisons) > 0) {
        if (isTRUE(comparisons)) {
            # group_use <- names(which(rowSums(table(data[[x]], data[[group_by]]) >= 2) >= 2))
            # print(group_use)
            if (any(rowSums(table(data[[x]], data[[group_by]]) >= 2) >= 3)) {
                message(
                    "Detected more than 2 groups. Use multiple_method for comparison"
                )
                # method <- multiple_method
                multiplegroup_comparisons <- TRUE
            } else {
                method <- pairwise_method

                if (!identical(fill_mode, "dodge")) {
                    stop(
                        "`comparisons` can only be used with `fill_mode = 'dodge'`."
                    )
                }

                # Preprocess data to avoid test failures
                # Check each x/facet combination for problematic data
                split_cols <- c(x, y, group_by)
                grouping_vars <- x
                if (!is.null(facet_by)) {
                    split_cols <- c(split_cols, facet_by)
                    grouping_vars <- c(grouping_vars, facet_by)
                }

                # Create grouping key for x and facet combinations
                if (length(grouping_vars) > 1) {
                    split_key <- interaction(
                        data[grouping_vars],
                        drop = TRUE,
                        sep = " // "
                    )
                } else {
                    split_key <- data[[grouping_vars]]
                }

                data_groups <- split(
                    data[, split_cols, drop = FALSE],
                    split_key
                )
                needs_fix <- FALSE

                # Check if any group will cause test failures
                for (group_data in data_groups) {
                    gs <- unique(as.character(group_data[[group_by]]))
                    if (length(gs) >= 2) {
                        yval1 <- group_data[[y]][
                            group_data[[group_by]] == gs[1]
                        ]
                        yval2 <- group_data[[y]][
                            group_data[[group_by]] == gs[2]
                        ]
                        # Check for zero variance or all NA

                        if (
                            all(is.na(yval1)) ||
                                all(is.na(yval2)) ||
                                (length(unique(yval1[!is.na(yval1)])) <= 3 ||
                                    length(unique(yval2[!is.na(yval2)])) <= 3)
                        ) {
                            needs_fix <- TRUE
                            break
                        }
                    }
                }

                pwc_data <- data
                if (needs_fix) {
                    warning(
                        "[Box/Violin/BeeswarmPlot] Some pairwise comparisons may fail due to insufficient data points or variability. Adjusting data to ensure valid comparisons."
                    )

                    # Split by facet if present
                    if (!is.null(facet_by)) {
                        facet_key <- interaction(
                            data[facet_by],
                            drop = TRUE,
                            sep = " // "
                        )
                        facet_splits <- split(
                            data[, split_cols, drop = FALSE],
                            facet_key
                        )
                    } else {
                        facet_splits <- list(data[, split_cols, drop = FALSE])
                    }

                    fixed_data_list <- lapply(
                        facet_splits,
                        function(facet_data) {
                            xdata <- split(facet_data, facet_data[[x]])
                            all_gs <- unique(as.character(facet_data[[
                                group_by
                            ]]))[1:2]

                            for (xval in names(xdata)) {
                                df <- xdata[[xval]]
                                gs <- unique(as.character(df[[group_by]]))

                                if (length(gs) < 2) {
                                    # Create minimal data for both groups
                                    df <- data.frame(
                                        x = xval,
                                        y = c(0, 1),
                                        group_by = all_gs
                                    )
                                    colnames(df) <- c(x, y, group_by)
                                    if (!is.null(facet_by)) {
                                        df[facet_by] <- unique(facet_data[
                                            facet_by
                                        ])
                                    }
                                } else {
                                    yval1 <- df[[y]][df[[group_by]] == gs[1]]
                                    yval2 <- df[[y]][df[[group_by]] == gs[2]]

                                    # Handle all NA cases
                                    if (all(is.na(yval1))) {
                                        yval1 <- c(
                                            0,
                                            rep(NA, length(yval1) - 1)
                                        )
                                    }
                                    if (all(is.na(yval2))) {
                                        yval2 <- c(
                                            1,
                                            rep(NA, length(yval2) - 1)
                                        )
                                    }

                                    # Handle zero variance cases
                                    unique_y1 <- unique(yval1[!is.na(yval1)])
                                    unique_y2 <- unique(yval2[!is.na(yval2)])

                                    if (
                                        length(unique_y1) == 1 &&
                                            length(unique_y2) == 1
                                    ) {
                                        # Both groups have same single value - add minimal relative variance
                                        # Calculate a small epsilon relative to the data scale
                                        all_y <- c(yval1, yval2)
                                        all_y_finite <- all_y[is.finite(all_y)]

                                        if (length(all_y_finite) > 0) {
                                            y_abs <- abs(all_y_finite)
                                            if (max(y_abs) > 0) {
                                                epsilon <- max(y_abs) * 1e-10
                                            } else {
                                                epsilon <- 1e-10
                                            }
                                        } else {
                                            epsilon <- 1e-10
                                        }

                                        # Add variance within each group while maintaining the same mean
                                        # This ensures the test will return p ≈ 1 (no significant difference)
                                        non_na_idx_1 <- which(!is.na(yval1))
                                        non_na_idx_2 <- which(!is.na(yval2))

                                        if (length(non_na_idx_1) >= 2) {
                                            yval1[non_na_idx_1[1]] <- unique_y1[
                                                1
                                            ] -
                                                epsilon
                                            yval1[non_na_idx_1[2]] <- unique_y1[
                                                1
                                            ] +
                                                epsilon
                                        } else if (length(non_na_idx_1) == 1) {
                                            yval1[non_na_idx_1[1]] <- unique_y1[
                                                1
                                            ]
                                        }

                                        if (length(non_na_idx_2) >= 2) {
                                            yval2[non_na_idx_2[1]] <- unique_y2[
                                                1
                                            ] -
                                                epsilon
                                            yval2[non_na_idx_2[2]] <- unique_y2[
                                                1
                                            ] +
                                                epsilon
                                        } else if (length(non_na_idx_2) == 1) {
                                            yval2[non_na_idx_2[1]] <- unique_y2[
                                                1
                                            ]
                                        }
                                    }

                                    df[[y]][df[[group_by]] == gs[1]] <- yval1
                                    df[[y]][df[[group_by]] == gs[2]] <- yval2
                                }
                                xdata[[xval]] <- df
                            }
                            do_call(rbind, xdata)
                        }
                    )
                    pwc_data <- do_call(rbind, fixed_data_list)
                }

                # Now call geom_pwc once with the preprocessed data
                # Add paired test support when paired_by is provided
                pwc_call <- list(
                    data = pwc_data,
                    label = sig_label,
                    label.size = sig_labelsize,
                    y.position = y_brackets %||% y_max_use,
                    step.increase = step_increase,
                    symnum.args = symnum_args,
                    tip.length = 0.03,
                    vjust = 0,
                    ref.group = ref_group,
                    method = method,
                    hide.ns = hide_ns
                )

                # Add paired test parameters if paired_by is provided
                if (!is.null(paired_by)) {
                    pwc_call$method.args <- c(
                        pwc_call$method.args,
                        list(paired = TRUE)
                    )
                }

                p <- p + do_call(ggpubr::geom_pwc, pwc_call)

                y_max_use <- layer_scales(p)$y$range$range[2]
            }
        } else if (!isTRUE(multiplegroup_comparisons)) {
            # if (!is.null(group_by)) {
            #     stop("`comparisons` can only be used when `group_by` is NULL is TRUE.")
            # }
            # Convert comparisons to indices
            comparisons <- lapply(
                comparisons,
                function(el) {
                    if (!is.numeric(el)) {
                        which(levels(data[[x]]) %in% el)
                    } else {
                        el
                    }
                }
            )

            # Preprocess data to avoid test failures (same as above for group_by case)
            split_cols <- if (!is.null(group_by)) c(x, y, group_by) else c(x, y)
            grouping_vars <- x
            if (!is.null(facet_by)) {
                split_cols <- c(split_cols, facet_by)
                grouping_vars <- c(grouping_vars, facet_by)
            }

            # Create grouping key for x and facet combinations
            if (length(grouping_vars) > 1) {
                split_key <- interaction(
                    data[grouping_vars],
                    drop = TRUE,
                    sep = " // "
                )
            } else {
                split_key <- data[[grouping_vars]]
            }

            data_groups <- split(data[, split_cols, drop = FALSE], split_key)
            needs_fix <- FALSE

            # For exact comparisons, we need to check x groups, not group_by groups
            # Check if any x group has zero variance
            for (group_data in data_groups) {
                yval <- group_data[[y]]
                # Check for zero variance or all NA
                if (
                    all(is.na(yval)) || length(unique(yval[!is.na(yval)])) <= 1
                ) {
                    needs_fix <- TRUE
                    break
                }
            }

            pwc_data <- data
            if (needs_fix) {
                warning(
                    "Some pairwise comparisons may fail due to insufficient variability. Adjusting data to ensure valid comparisons."
                )

                # Split by facet if present
                if (!is.null(facet_by)) {
                    facet_key <- interaction(
                        data[facet_by],
                        drop = TRUE,
                        sep = " // "
                    )
                    facet_splits <- split(
                        data[, split_cols, drop = FALSE],
                        facet_key
                    )
                } else {
                    facet_splits <- list(data[, split_cols, drop = FALSE])
                }

                fixed_data_list <- lapply(facet_splits, function(facet_data) {
                    xdata <- split(facet_data, facet_data[[x]])

                    for (xval in names(xdata)) {
                        df <- xdata[[xval]]
                        if (nrow(df) < 2) {
                            xdata[[xval]] <- df
                            next
                        }
                        yval <- df[[y]]

                        # Handle all NA cases
                        if (all(is.na(yval))) {
                            yval <- c(0, 1, rep(NA, length(yval) - 2))
                        }

                        # Handle zero variance cases
                        unique_y <- unique(yval[!is.na(yval)])

                        if (length(unique_y) == 1) {
                            # Single value - add minimal relative variance
                            # Calculate a small epsilon relative to the data scale
                            all_y_finite <- yval[is.finite(yval)]

                            if (length(all_y_finite) > 0) {
                                y_abs <- abs(all_y_finite)
                                if (max(y_abs) > 0) {
                                    epsilon <- max(y_abs) * 1e-10
                                } else {
                                    epsilon <- 1e-10
                                }
                            } else {
                                epsilon <- 1e-10
                            }

                            # Add symmetric variance around the mean
                            non_na_idx <- which(!is.na(yval))
                            if (length(non_na_idx) >= 2) {
                                yval[non_na_idx[1]] <- unique_y[1] - epsilon
                                yval[non_na_idx[2]] <- unique_y[1] + epsilon
                            }
                        }

                        df[[y]] <- yval
                        xdata[[xval]] <- df
                    }
                    do_call(rbind, xdata)
                })
                pwc_data <- do_call(rbind, fixed_data_list)
            }

            # Add paired test support when paired_by is provided
            method_args <- list(comparisons = comparisons)
            if (!is.null(paired_by)) {
                method_args$paired <- TRUE
            }

            p <- p +
                ggpubr::geom_pwc(
                    data = pwc_data,
                    label = sig_label,
                    label.size = sig_labelsize,
                    y.position = y_brackets %||% y_max_use,
                    step.increase = step_increase,
                    symnum.args = symnum_args,
                    tip.length = 0.03,
                    vjust = 0,
                    # comparisons = comparisons,
                    ref.group = ref_group,
                    method = pairwise_method,
                    method.args = method_args,
                    hide.ns = hide_ns
                )
            y_max_use <- layer_scales(p)$y$range$range[1] +
                (layer_scales(p)$y$range$range[2] -
                    layer_scales(p)$y$range$range[1]) *
                    1.15
        }
    }

    if (isTRUE(multiplegroup_comparisons)) {
        p <- p +
            ggpubr::stat_compare_means(
                mapping = if (!is.null(group_by)) {
                    aes(x = !!sym(x), y = !!sym(y), group = !!sym(group_by))
                } else {
                    aes(x = !!sym(x), y = !!sym(y))
                },
                inherit.aes = FALSE,
                method = multiple_method,
                symnum.args = symnum_args,
                label.y = y_brackets %||% y_max_use,
                size = sig_labelsize,
                label = sig_label,
                vjust = -0.5,
                hjust = ifelse(is.null(group_by), 0, 0.5)
            )
        y_max_use <- layer_scales(p)$y$range$range[1] +
            (layer_scales(p)$y$range$range[2] -
                layer_scales(p)$y$range$range[1]) *
                1.15
    }
    if (!is.null(y_max) && is.numeric(y_max)) {
        y_max_use <- max(y_max_use, y_max)
    }

    if (isTRUE(add_point)) {
        if (!is.null(paired_by)) {
            if (is.null(group_by)) {
                p <- p +
                    geom_line(
                        data = data,
                        mapping = aes(
                            x = !!sym(x),
                            y = !!sym(y),
                            group = !!sym(paired_by)
                        ),
                        color = pt_color,
                        alpha = pt_alpha,
                        linewidth = 0.3,
                        inherit.aes = FALSE
                    )
            } else {
                line_data <- data
                # re-calculate x
                # for the first group, x = integer(x) - n
                # for the second group, x = integer(x) + n
                line_data$.xint <- as.numeric(line_data[[x]])
                groups <- levels(line_data[[group_by]])
                line_data$.x <- ifelse(
                    line_data[[group_by]] == groups[1],
                    line_data$.xint - .225, # n = 0.225 = 0.9 / 2 / 2
                    line_data$.xint + .225
                )
                line_data$.line_group <- paste(
                    line_data[[paired_by]],
                    line_data[[x]],
                    sep = " // "
                )
                p <- p +
                    geom_line(
                        data = line_data,
                        mapping = aes(
                            x = !!sym(".x"),
                            y = !!sym(y),
                            group = !!sym(".line_group")
                        ),
                        color = pt_color,
                        alpha = pt_alpha,
                        linewidth = 0.3,
                        inherit.aes = FALSE
                    )
            }
        }

        # Use beeswarm or jittered points
        if (isTRUE(add_beeswarm)) {
            # Use ggbeeswarm for non-overlapping point layout
            if (!is.null(pt_color)) {
                p <- p +
                    ggbeeswarm::geom_beeswarm(
                        color = pt_color,
                        method = beeswarm_method,
                        cex = beeswarm_cex,
                        priority = beeswarm_priority,
                        dodge.width = beeswarm_dodge,
                        show.legend = FALSE
                    )
            } else {
                colors <- palette_this(
                    levels(data[[fill_by]]),
                    palette = palette,
                    palcolor = palcolor,
                    reverse = palreverse
                )
                p <- p +
                    ggbeeswarm::geom_beeswarm(
                        aes(color = !!sym(fill_by)),
                        method = beeswarm_method,
                        cex = beeswarm_cex,
                        priority = beeswarm_priority,
                        dodge.width = beeswarm_dodge
                    ) +
                    scale_color_manual(values = colors, guide = "legend")
            }
            if (any(data$.highlight == "TRUE")) {
                p <- p +
                    ggbeeswarm::geom_beeswarm(
                        data = data[data$.highlight == "TRUE", , drop = FALSE],
                        color = highlight_color,
                        method = beeswarm_method,
                        cex = beeswarm_cex,
                        priority = beeswarm_priority,
                        dodge.width = beeswarm_dodge,
                        show.legend = FALSE
                    ) +
                    scale_size_manual(
                        values = c(
                            "TRUE" = highlight_size,
                            "FALSE" = pt_size %||% min(3000 / nrow(data), 0.6)
                        ),
                        guide = "none"
                    ) +
                    scale_alpha_manual(
                        values = c(
                            "TRUE" = highlight_alpha,
                            "FALSE" = pt_alpha
                        ),
                        guide = "none"
                    )
            }
        } else {
            # Use regular jittered points
            p <- p +
                geom_point(
                    aes(
                        fill = !!sym(fill_by),
                        color = !!sym(".highlight"),
                        size = !!sym(".highlight"),
                        alpha = !!sym(".highlight")
                    ),
                    position = position_jitterdodge(
                        jitter.width = jitter_width %||%
                            ifelse(!is.null(paired_by), 0, 0.5),
                        jitter.height = jitter_height,
                        dodge.width = 0.9,
                        seed = seed
                    ),
                    show.legend = FALSE
                ) +
                scale_color_manual(
                    values = c("TRUE" = highlight_color, "FALSE" = pt_color),
                    guide = "none"
                ) +
                scale_size_manual(
                    values = c(
                        "TRUE" = highlight_size,
                        "FALSE" = pt_size %||% min(3000 / nrow(data), 0.6)
                    ),
                    guide = "none"
                ) +
                scale_alpha_manual(
                    values = c("TRUE" = highlight_alpha, "FALSE" = pt_alpha),
                    guide = "none"
                )
        }
    }

    if (isTRUE(add_trend)) {
        if (is.null(trend_color)) {
            p <- p +
                stat_summary(
                    fun = first,
                    geom = "line",
                    mapping = if (!is.null(group_by)) {
                        aes(
                            y = !!sym(".y_median"),
                            group = !!sym(group_by),
                            color = !!sym(group_by)
                        )
                    } else {
                        aes(y = !!sym(".y_median"), group = 1)
                    },
                    position = position_dodge(
                        width = 0.9,
                        preserve = position_dodge_preserve
                    ),
                    linewidth = trend_linewidth
                )
            if (!is.null(group_by)) {
                group_vals <- levels(data[[group_by]])
                if (anyNA(data[[group_by]])) {
                    group_vals <- c(group_vals, NA)
                }
                group_colors <- palette_this(
                    group_vals,
                    palette = palette,
                    palcolor = palcolor,
                    reverse = palreverse,
                    NA_keep = TRUE
                )

                if (isTRUE(keep_empty_group)) {
                    p <- p +
                        scale_color_manual(
                            values = group_colors,
                            na.value = group_colors['NA'] %||% "grey80",
                            breaks = group_vals,
                            limits = group_vals,
                            drop = FALSE
                        )
                } else {
                    p <- p +
                        scale_color_manual(
                            values = group_colors,
                            na.value = group_colors['NA'] %||% "grey80"
                        )
                }
            }
        } else {
            p <- p +
                stat_summary(
                    fun = first,
                    geom = "line",
                    mapping = if (!is.null(group_by)) {
                        aes(y = !!sym(".y_median"), group = !!sym(group_by))
                    } else {
                        aes(y = !!sym(".y_median"), group = 1)
                    },
                    position = position_dodge(
                        width = 0.9,
                        preserve = position_dodge_preserve
                    ),
                    color = trend_color,
                    linewidth = trend_linewidth
                )
        }

        p <- p +
            stat_summary(
                fun = first,
                geom = "point",
                mapping = if (!is.null(group_by)) {
                    aes(y = !!sym(".y_median"), group = !!sym(group_by))
                } else {
                    aes(y = !!sym(".y_median"), group = 1)
                },
                position = position_dodge(
                    width = 0.9,
                    preserve = position_dodge_preserve
                ),
                color = "black",
                fill = "white",
                size = trend_ptsize,
                shape = 21
            )
    }

    if (!is.null(add_line)) {
        p <- p +
            geom_hline(
                yintercept = add_line,
                color = line_color,
                linetype = line_type,
                linewidth = line_width
            )
    }

    if (!is.null(add_stat)) {
        p <- p +
            stat_summary(
                fun = add_stat,
                geom = "point",
                mapping = if (!is.null(group_by)) {
                    aes(shape = !!sym("stat_shape"), group = !!sym(group_by))
                } else {
                    aes(shape = !!sym("stat_shape"), group = 1)
                },
                position = position_dodge(
                    width = 0.9,
                    preserve = position_dodge_preserve
                ),
                color = stat_color,
                fill = stat_color,
                size = stat_size,
                stroke = stat_stroke,
            ) +
            scale_shape_identity(
                labels = stat_name %||%
                    paste0(y, " (", deparse(substitute(add_stat)), ")"),
                guide = guide_legend(title = "", order = 2)
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
        )

    if (base == "bar") {
        p <- p +
            scale_y_continuous(
                trans = y_trans,
                n.breaks = y_nbreaks,
                expand = expansion(mult = c(0, 0.05))
            )
    } else {
        p <- p + scale_y_continuous(trans = y_trans, n.breaks = y_nbreaks)
    }

    x_maxchars <- max(nchar(levels(data[[x]])))
    nx <- nlevels(data[[x]])
    nd <- ifelse(is.null(group_by), 1, nlevels(data[[group_by]]))
    facet_free <- !is.null(facet_by) &&
        (identical(facet_scales, "free") ||
            (!flip && identical(facet_scales, "free_y")) ||
            (flip && identical(facet_scales, "free_x")))
    if (isTRUE(flip) && isTRUE(stack)) {
        facet_nrow <- facet_nrow %||% 1
        strip_position <- "top"
        p <- p +
            ggplot2::theme(
                # strip.text.x = element_text(angle = 90),
                panel.grid.major.x = element_line(color = "grey", linetype = 2),
                panel.spacing.x = unit(-1, "pt")
            )
        if (facet_free) {
            p <- p + coord_flip()
        } else {
            p <- p + coord_flip(ylim = c(y_min_use, y_max_use))
        }
    } else if (isTRUE(flip) && isFALSE(stack)) {
        strip_position <- "top"
        p <- p +
            ggplot2::theme(
                strip.text.y = element_text(angle = 0),
                panel.grid.major.x = element_line(color = "grey", linetype = 2),
            )
        if (facet_free) {
            p <- p + coord_flip()
        } else {
            p <- p + coord_flip(ylim = c(y_min_use, y_max_use))
        }
    } else if (isTRUE(stack)) {
        facet_ncol <- facet_ncol %||% 1
        strip_position <- "right"
        p <- p +
            ggplot2::theme(
                panel.spacing.y = unit(-1, "pt"),
                strip.text.y = element_text(angle = 0, hjust = 0),
                panel.grid.major.y = element_line(color = "grey", linetype = 2),
            )
        if (!facet_free) {
            p <- p + coord_cartesian(ylim = c(y_min_use, y_max_use))
        }
    } else {
        strip_position <- "top"
        p <- p +
            ggplot2::theme(
                strip.text.x = element_text(angle = 0),
                panel.grid.major.x = element_line(color = "grey", linetype = 2),
            )
        if (!facet_free) {
            p <- p + coord_cartesian(ylim = c(y_min_use, y_max_use))
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
            legend.direction = legend.direction,
        )

    # Dimension calculation
    # When flipped: nx*nd categories land on the visual y-axis (height-driven);
    #               x-axis label chars add to width.
    # When not flipped: nx*nd categories land on the visual x-axis (width-driven);
    #                   x-axis label chars add to height.
    if (isTRUE(flip)) {
        label_min_width <- if (isTRUE(stack)) {
            max(3, 2 + x_maxchars * 0.05)
        } else {
            max(3, 2.2 + x_maxchars * 0.05)
        }
        dims <- calculate_plot_dimensions(
            base_height = label_min_width,
            aspect.ratio = aspect.ratio,
            n_y = nx * nd,
            y_scale_factor = 0.5,
            legend.position = legend.position,
            legend.direction = legend.direction,
            legend_n = nd,
            legend_nchar = if (is.null(group_by)) {
                5
            } else {
                max(nchar(levels(data[[group_by]])))
            },
            flip = TRUE
        )
        height <- dims$height
        width <- max(dims$width, label_min_width)
    } else {
        label_min_height <- if (isTRUE(stack)) {
            4 + x_maxchars * 0.05
        } else {
            2 + x_maxchars * 0.05
        }
        dims <- calculate_plot_dimensions(
            base_height = label_min_height,
            aspect.ratio = aspect.ratio,
            n_x = nx * nd,
            x_scale_factor = 0.5,
            legend.position = legend.position,
            legend.direction = legend.direction,
            legend_n = nd,
            legend_nchar = if (is.null(group_by)) {
                5
            } else {
                max(nchar(levels(data[[group_by]])))
            },
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

#' Box / Violin / Bar / Beeswarm plot (internal)
#'
#' @description
#' Internal wrapper that handles \code{split_by} processing and dispatches
#' to \code{\link{BoxViolinPlotAtomic}} for each split.  This is the
#' intermediate layer between the three public APIs
#' (\code{\link{BoxPlot}}, \code{\link{ViolinPlot}}, and
#' \code{\link{BeeswarmPlot}}) and the core implementation.
#'
#' @section split_by workflow:
#' When \code{split_by} is provided:
#' \enumerate{
#'   \item \code{\link{check_keep_na}()} and \code{\link{check_keep_empty}()}
#'         normalise the \code{keep_na} / \code{keep_empty} arguments.
#'   \item The \code{split_by} column is validated and its NA / empty levels
#'         are processed.  It is then removed from the per-column lists.
#'   \item The data is split by \code{split_by} (preserving level order).
#'         If \code{split_by} is \code{NULL}, the data is wrapped in a
#'         single-element list with name \code{"..."}.
#'   \item Per-split \code{palette}, \code{palcolor},
#'         \code{legend.position}, and \code{legend.direction} are resolved.
#'   \item \code{\link{BoxViolinPlotAtomic}()} is called for each split.
#'         When \code{title} is a function, it receives the split level name
#'         for dynamic titles.
#'   \item Results are combined via \code{\link{combine_plots}()}.
#' }
#'
#' @rdname BoxViolinPlot-internal
#' @inheritParams common_args
#' @inheritParams BoxViolinPlotAtomic
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
#' @keywords internal
#' @importFrom rlang %||%
BoxViolinPlot <- function(
    data,
    x,
    x_sep = "_",
    y = NULL,
    base = c("box", "violin", "bar", "none"),
    in_form = c("long", "wide"),
    split_by = NULL,
    split_by_sep = "_",
    symnum_args = NULL,
    sort_x = NULL,
    flip = FALSE,
    keep_empty = FALSE,
    keep_na = FALSE,
    group_by = NULL,
    group_by_sep = "_",
    group_name = NULL,
    paired_by = NULL,
    x_text_angle = ifelse(isTRUE(flip), 0, 45),
    step_increase = 0.1,
    fill_mode = ifelse(!is.null(group_by), "dodge", "x"),
    palreverse = FALSE,
    position_dodge_preserve = "total",
    theme = "theme_this",
    theme_args = list(),
    palette = "Paired",
    palcolor = NULL,
    alpha = 1,
    aspect.ratio = NULL,
    legend.position = "right",
    legend.direction = "vertical",
    add_point = FALSE,
    pt_color = if (isTRUE(add_beeswarm)) NULL else "grey30",
    pt_size = NULL,
    pt_alpha = 1,
    jitter_width = NULL,
    jitter_height = 0,
    stack = FALSE,
    y_max = NULL,
    y_min = NULL,
    y_brackets = NULL,
    add_beeswarm = FALSE,
    beeswarm_method = "swarm",
    beeswarm_cex = 1,
    beeswarm_priority = "ascending",
    beeswarm_dodge = 0.9,
    add_box = FALSE,
    box_color = "black",
    box_width = 0.1,
    box_ptsize = 2.5,
    add_errorbar = "SEM",
    errorbar_color = "grey20",
    errorbar_width = 0.4,
    errorbar_linewidth = 0.6,
    add_trend = FALSE,
    trend_color = NULL,
    trend_linewidth = 1,
    trend_ptsize = 2,
    add_stat = NULL,
    stat_name = NULL,
    stat_color = "black",
    stat_size = 1,
    stat_stroke = 1,
    stat_shape = 25,
    add_bg = FALSE,
    bg_palette = "stripe",
    bg_palcolor = NULL,
    bg_alpha = 0.2,
    add_line = NULL,
    line_color = "red2",
    line_width = .6,
    line_type = 2,
    highlight = NULL,
    highlight_color = "red2",
    highlight_size = 1,
    highlight_alpha = 1,
    comparisons = NULL,
    ref_group = NULL,
    pairwise_method = "wilcox.test",
    multiplegroup_comparisons = FALSE,
    multiple_method = "kruskal.test",
    sig_label = "p.format",
    sig_labelsize = 3.5,
    hide_ns = FALSE,
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
    keep_na <- check_keep_na(
        keep_na,
        c(x, split_by, group_by, paired_by, facet_by)
    )
    keep_empty <- check_keep_empty(
        keep_empty,
        c(x, split_by, group_by, paired_by, facet_by)
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

    stat_name <- stat_name %||%
        paste0(y, " (", deparse(substitute(add_stat)), ")")
    base <- match.arg(base)

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
            BoxViolinPlotAtomic(
                datas[[nm]],
                x = x,
                x_sep = x_sep,
                y = y,
                base = base,
                in_form = in_form,
                sort_x = sort_x,
                flip = flip,
                keep_empty = keep_empty,
                keep_na = keep_na,
                group_by = group_by,
                group_by_sep = group_by_sep,
                group_name = group_name,
                paired_by = paired_by,
                x_text_angle = x_text_angle,
                fill_mode = fill_mode,
                palreverse = palreverse,
                step_increase = step_increase,
                theme = theme,
                theme_args = theme_args,
                palette = palette[[nm]],
                palcolor = palcolor[[nm]],
                alpha = alpha,
                position_dodge_preserve = position_dodge_preserve,
                aspect.ratio = aspect.ratio,
                legend.position = legend.position[[nm]],
                legend.direction = legend.direction[[nm]],
                add_point = add_point,
                pt_color = pt_color,
                pt_size = pt_size,
                pt_alpha = pt_alpha,
                symnum_args = symnum_args,
                jitter_width = jitter_width,
                jitter_height = jitter_height,
                stack = stack,
                y_max = y_max,
                y_min = y_min,
                y_brackets = y_brackets,
                add_beeswarm = add_beeswarm,
                beeswarm_method = beeswarm_method,
                beeswarm_cex = beeswarm_cex,
                beeswarm_priority = beeswarm_priority,
                beeswarm_dodge = beeswarm_dodge,
                add_box = add_box,
                box_color = box_color,
                box_width = box_width,
                box_ptsize = box_ptsize,
                add_errorbar = add_errorbar,
                errorbar_color = errorbar_color,
                errorbar_width = errorbar_width,
                errorbar_linewidth = errorbar_linewidth,
                add_trend = add_trend,
                trend_color = trend_color,
                trend_linewidth = trend_linewidth,
                trend_ptsize = trend_ptsize,
                add_stat = add_stat,
                stat_name = stat_name,
                stat_color = stat_color,
                stat_size = stat_size,
                stat_stroke = stat_stroke,
                stat_shape = stat_shape,
                add_bg = add_bg,
                bg_palette = bg_palette,
                bg_palcolor = bg_palcolor,
                bg_alpha = bg_alpha,
                add_line = add_line,
                line_color = line_color,
                line_width = line_width,
                line_type = line_type,
                highlight = highlight,
                highlight_color = highlight_color,
                highlight_size = highlight_size,
                highlight_alpha = highlight_alpha,
                comparisons = comparisons,
                ref_group = ref_group,
                pairwise_method = pairwise_method,
                multiplegroup_comparisons = multiplegroup_comparisons,
                multiple_method = multiple_method,
                sig_label = sig_label,
                sig_labelsize = sig_labelsize,
                hide_ns = hide_ns,
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

#' Box / Violin / Bar / Beeswarm plot
#'
#' @description
#' `BoxPlot` draws box plots or bar plots (mean ± error bars) with extensive
#' customisation options.  Supports jittered or beeswarm points, paired
#' observations with connecting lines, trend lines, statistical test
#' annotations (pairwise or omnibus), background stripes, reference lines,
#' point highlighting, and custom summary statistic overlays.
#'
#' This is the public API — it delegates to \code{\link{BoxViolinPlot}}
#' with \code{base = "box"} or \code{base = "bar"}, which in turn dispatches
#' to \code{\link{BoxViolinPlotAtomic}} for each \code{split_by} level.
#'
#' @section Bar plots (\code{base = "bar"}):
#' When \code{base = "bar"}, bars display group means with optional error
#' bars.  \code{add_errorbar} controls the error bar type:
#' \itemize{
#'   \item \code{"SEM"} (default) — standard error of the mean.
#'   \item \code{"SD"} — standard deviation.
#'   \item \code{"CI"} or \code{"CI95"} — 95\% confidence interval.
#'   \item \code{"none"} — no error bars.
#' }
#' Error bars are computed via a custom \code{stat_summary(fun.data = ...)}
#' that handles per-group mean, SD, and sample size.
#'
#' @rdname boxviolinplot
#' @inheritParams BoxViolinPlot
#' @param base A character string: \code{"box"} (default) or \code{"bar"}.
#'  Bar plots show group means with optional error bars.
#' @param add_errorbar Type of error bars for bar plots.  See Details.
#' @param errorbar_color,errorbar_width,errorbar_linewidth Error bar
#'  appearance controls.
#' @return A \code{ggplot} object, a \code{patchwork} object, or a named list
#'  of \code{ggplot} objects (when \code{combine = FALSE}), each with
#'  \code{height} and \code{width} attributes in inches.
#' @export
#' @examples
#' \donttest{
#' set.seed(8525)
#' data <- data.frame(
#'     x = rep(LETTERS[1:8], each = 40),
#'     y = c(rnorm(160), rnorm(160, mean = 1)),
#'     group1 = sample(c("g1", "g2"), 320, replace = TRUE),
#'     group2 = sample(c("h1", "h2", "h3", "h4"), 320, replace = TRUE)
#' )
#'
#' # Basic box plot
#' BoxPlot(data, x = "x", y = "y")
#'
#' # With beeswarm points
#' BoxPlot(data, x = "x", y = "y", add_beeswarm = TRUE, pt_color = "grey30")
#'
#' # Stacked + flipped + faceted
#' BoxPlot(data,
#'     x = "x", y = "y",
#'     stack = TRUE, flip = TRUE, facet_by = "group1",
#'     add_bg = TRUE, bg_palette = "Paired")
#'
#' # Stacked + flipped + split_by with per-split colours
#' BoxPlot(data,
#'     x = "x", y = "y",
#'     stack = TRUE, flip = TRUE, split_by = "group1",
#'     add_bg = TRUE, bg_palette = "Paired",
#'     palcolor = list(g1 = c("red", "blue"), g2 = c("blue", "red")))
#'
#' # sort_x — order by mean(y)
#' data <- data.frame(
#'   x = factor(rep(LETTERS[1:5], each = 40),
#'      levels = c(LETTERS[1:2], "unused", LETTERS[3:5])),
#'   y = c(rnorm(40, mean = 5), rnorm(40, mean = 4), rnorm(40, mean = 3),
#'      rnorm(40, mean = 2), rnorm(40, mean = 1))
#' )
#' BoxPlot(data, x = "x", y = "y", sort_x = "mean(y)", keep_empty = TRUE)
#' BoxPlot(data, x = "x", y = "y", sort_x = "mean(-y)", keep_empty = TRUE)
#'
#' # Wide-form data
#' data_wide <- data.frame(A = rnorm(100), B = rnorm(100), C = rnorm(100))
#' BoxPlot(data_wide, x = c("A", "B", "C"), in_form = "wide")
#'
#' # Paired observations with connecting lines and paired test
#' paired_data <- data.frame(
#'     subject = rep(paste0("s", 1:10), each = 2),
#'     visit = rep(c("pre", "post"), times = 10),
#'     value = rnorm(20))
#' BoxPlot(paired_data,
#'     x = "visit", y = "value", comparisons = TRUE,
#'     paired_by = "subject", add_point = TRUE)
#'
#' # Paired + grouped
#' paired_group_data <- data.frame(
#'     subject = rep(paste0("s", 1:6), each = 2),
#'     x = rep(c("A", "B"), each = 6),
#'     group = rep(c("before", "after"), times = 6),
#'     value = rnorm(12))
#' BoxPlot(paired_group_data,
#'     x = "x", y = "value",
#'     paired_by = "subject", group_by = "group",
#'     comparisons = TRUE, pt_size = 3, pt_color = "red")
#'
#' # keep_na and keep_empty examples
#' data <- data.frame(
#'     x = factor(rep(c(LETTERS[1:3], NA, LETTERS[5:8]), each = 40),
#'        levels = c(LETTERS[1:8])),
#'     y = c(rnorm(160), rnorm(160, mean = 1)),
#'     group1 = sample(c("g1", "g2"), 320, replace = TRUE),
#'     group2 = factor(sample(c("h1", NA, "h3", "h4"), 320, replace = TRUE),
#'        levels = c("h1", "h2", "h3", "h4")))
#'
#' BoxPlot(data, x = "x", y = "y")
#' BoxPlot(data, x = "x", y = "y", keep_na = TRUE, keep_empty = TRUE)
#' BoxPlot(data, x = "x", y = "y", keep_na = TRUE, keep_empty = TRUE,
#'         facet_by = "group2")
#' BoxPlot(data, x = "x", y = "y", keep_na = TRUE, keep_empty = 'level')
#' BoxPlot(data, x = "x", y = "y", group_by = "group2")
#' BoxPlot(data, x = "x", y = "y", group_by = "group2",
#'         keep_na = TRUE, keep_empty = TRUE)
#' BoxPlot(data, x = "x", y = "y", group_by = "group2",
#'         keep_na = TRUE, keep_empty = 'level')
#'
#' # Per-column keep_na / keep_empty
#' BoxPlot(data, x = "x", y = "y", group_by = "group2",
#'         keep_na = list(x = TRUE, group2 = FALSE),
#'         keep_empty = list(x = FALSE, group2 = TRUE))
#'
#' # Bar plot (base = "bar")
#' data$y <- abs(data$y)
#' BoxPlot(data, x = "x", y = "y", base = "bar")
#' BoxPlot(data, x = "x", y = "y", base = "bar", add_errorbar = "SD")
#' BoxPlot(data, x = "x", y = "y", base = "bar", add_errorbar = "CI95")
#' BoxPlot(data, x = "x", y = "y", base = "bar", add_errorbar = "none")
#' BoxPlot(data, x = "x", y = "y", base = "bar", group_by = "group1")
#' BoxPlot(data, x = "x", y = "y", base = "bar", add_point = TRUE)
#' BoxPlot(data, x = "x", y = "y", base = "bar",
#'         fill_mode = "mean", palette = "Blues")
#' }
BoxPlot <- function(
    data,
    x,
    x_sep = "_",
    y = NULL,
    base = c("box", "bar"),
    in_form = c("long", "wide"),
    split_by = NULL,
    split_by_sep = "_",
    symnum_args = NULL,
    sort_x = NULL,
    flip = FALSE,
    keep_empty = FALSE,
    keep_na = FALSE,
    group_by = NULL,
    group_by_sep = "_",
    group_name = NULL,
    paired_by = NULL,
    x_text_angle = ifelse(isTRUE(flip), 0, 45),
    step_increase = 0.1,
    fill_mode = ifelse(!is.null(group_by), "dodge", "x"),
    palreverse = FALSE,
    position_dodge_preserve = "total",
    theme = "theme_this",
    theme_args = list(),
    palette = "Paired",
    palcolor = NULL,
    alpha = 1,
    aspect.ratio = NULL,
    legend.position = "right",
    legend.direction = "vertical",
    add_point = FALSE,
    pt_color = if (isTRUE(add_beeswarm)) NULL else "grey30",
    pt_size = NULL,
    pt_alpha = 1,
    jitter_width = NULL,
    jitter_height = 0,
    stack = FALSE,
    y_max = NULL,
    y_min = NULL,
    y_brackets = NULL,
    add_beeswarm = FALSE,
    beeswarm_method = "swarm",
    beeswarm_cex = 1,
    beeswarm_priority = "ascending",
    beeswarm_dodge = 0.9,
    add_trend = FALSE,
    trend_color = NULL,
    trend_linewidth = 1,
    trend_ptsize = 2,
    add_stat = NULL,
    stat_name = NULL,
    stat_color = "black",
    stat_size = 1,
    stat_stroke = 1,
    stat_shape = 25,
    add_errorbar = "SEM",
    errorbar_color = "grey20",
    errorbar_width = 0.4,
    errorbar_linewidth = 0.6,
    add_bg = FALSE,
    bg_palette = "stripe",
    bg_palcolor = NULL,
    bg_alpha = 0.2,
    add_line = NULL,
    line_color = "red2",
    line_width = .6,
    line_type = 2,
    highlight = NULL,
    highlight_color = "red2",
    highlight_size = 1,
    highlight_alpha = 1,
    comparisons = NULL,
    ref_group = NULL,
    pairwise_method = "wilcox.test",
    multiplegroup_comparisons = FALSE,
    multiple_method = "kruskal.test",
    sig_label = "p.format",
    sig_labelsize = 3.5,
    hide_ns = FALSE,
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
    ...
) {
    base <- match.arg(base)
    stat_name <- stat_name %||%
        paste0(y, " (", deparse(substitute(add_stat)), ")")
    BoxViolinPlot(
        data = data,
        x = x,
        x_sep = x_sep,
        y = y,
        base = base,
        in_form = in_form,
        split_by = split_by,
        split_by_sep = split_by_sep,
        sort_x = sort_x,
        flip = flip,
        keep_empty = keep_empty,
        keep_na = keep_na,
        group_by = group_by,
        group_by_sep = group_by_sep,
        group_name = group_name,
        paired_by = paired_by,
        x_text_angle = x_text_angle,
        fill_mode = fill_mode,
        palreverse = palreverse,
        step_increase = step_increase,
        theme = theme,
        theme_args = theme_args,
        palette = palette,
        palcolor = palcolor,
        alpha = alpha,
        position_dodge_preserve = position_dodge_preserve,
        aspect.ratio = aspect.ratio,
        legend.position = legend.position,
        legend.direction = legend.direction,
        add_point = add_point,
        pt_color = pt_color,
        pt_size = pt_size,
        pt_alpha = pt_alpha,
        symnum_args = symnum_args,
        jitter_width = jitter_width,
        jitter_height = jitter_height,
        stack = stack,
        y_max = y_max,
        y_min = y_min,
        y_brackets = y_brackets,
        add_beeswarm = add_beeswarm,
        beeswarm_method = beeswarm_method,
        beeswarm_cex = beeswarm_cex,
        beeswarm_priority = beeswarm_priority,
        beeswarm_dodge = beeswarm_dodge,
        add_trend = add_trend,
        trend_color = trend_color,
        trend_linewidth = trend_linewidth,
        trend_ptsize = trend_ptsize,
        add_stat = add_stat,
        stat_name = stat_name,
        stat_color = stat_color,
        stat_size = stat_size,
        stat_stroke = stat_stroke,
        stat_shape = stat_shape,
        add_errorbar = add_errorbar,
        errorbar_color = errorbar_color,
        errorbar_width = errorbar_width,
        errorbar_linewidth = errorbar_linewidth,
        add_bg = add_bg,
        bg_palette = bg_palette,
        bg_palcolor = bg_palcolor,
        bg_alpha = bg_alpha,
        add_line = add_line,
        line_color = line_color,
        line_width = line_width,
        line_type = line_type,
        highlight = highlight,
        highlight_color = highlight_color,
        highlight_size = highlight_size,
        highlight_alpha = highlight_alpha,
        comparisons = comparisons,
        ref_group = ref_group,
        pairwise_method = pairwise_method,
        multiplegroup_comparisons = multiplegroup_comparisons,
        multiple_method = multiple_method,
        sig_label = sig_label,
        sig_labelsize = sig_labelsize,
        hide_ns = hide_ns,
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
        combine = combine,
        nrow = nrow,
        ncol = ncol,
        byrow = byrow,
        axes = axes,
        axis_titles = axis_titles,
        guides = guides,
        ...
    )
}

#' @description
#' `ViolinPlot` draws violin plots with extensive customisation options.  Supports jittered
#' or beeswarm points, box plot overlays, trend lines, statistical test
#' annotations, background stripes, reference lines, point highlighting,
#' and custom summary statistic overlays.
#'
#' This is the public API — it delegates to \code{\link{BoxViolinPlot}}
#' with \code{base = "violin"}, which dispatches to
#' \code{\link{BoxViolinPlotAtomic}} for each \code{split_by} level.
#'
#' @rdname boxviolinplot
#' @inheritParams BoxViolinPlot
#' @export
#' @examples
#' \donttest{
#' ViolinPlot(data, x = "x", y = "y")
#' ViolinPlot(data, x = "x", y = "y", add_beeswarm = TRUE, pt_color = "grey30")
#' ViolinPlot(data, x = "x", y = "y", add_box = TRUE)
#' ViolinPlot(data, x = "x", y = "y", add_point = TRUE)
#' ViolinPlot(data, x = "x", y = "y", add_trend = TRUE)
#' ViolinPlot(data, x = "x", y = "y", add_stat = mean)
#' ViolinPlot(data, x = "x", y = "y", add_bg = TRUE)
#' ViolinPlot(data, x = "x", y = "y", add_line = 0)
#'
#' # Grouped
#' ViolinPlot(data, x = "x", y = "y", group_by = "group1")
#'
#' # Grouped + faceted + box overlay
#' ViolinPlot(data,
#'     x = "x", y = "y", group_by = "group1",
#'     facet_by = "group2", add_box = TRUE)
#'
#' # Highlight
#' ViolinPlot(data,
#'     x = "x", y = "y", add_point = TRUE,
#'     highlight = 'group1 == "g1"', alpha = 0.8,
#'     highlight_size = 1.5, pt_size = 1, add_box = TRUE)
#'
#' # Pairwise comparisons with formatted labels
#' if (requireNamespace("ggpubr", quietly = TRUE)) {
#'   # https://github.com/kassambara/ggpubr/issues/751
#'   library(ggpubr)
#'   ViolinPlot(data,
#'     x = "x", y = "y", group_by = "group1",
#'     comparisons = TRUE, sig_label = "p = {p}")
#' }
#'
#' # Explicit comparison list + hide non-significant
#' ViolinPlot(data,
#'     x = "x", y = "y", sig_label = "p.format", hide_ns = TRUE,
#'     facet_by = "group2", comparisons = list(c("D", "E")))
#'
#' # Continuous fill (mean) + omnibus test
#' ViolinPlot(data,
#'     x = "x", y = "y", fill_mode = "mean",
#'     facet_by = "group2", palette = "Blues",
#'     multiplegroup_comparisons = TRUE)
#'
#' # Per-split palettes
#' ViolinPlot(data,
#'     x = "x", y = "y", fill_mode = "mean",
#'     split_by = "group1", palette = c(g1 = "Blues", g2 = "Reds"))
#'
#' # Stacked faceting
#' ViolinPlot(data,
#'     x = "x", y = "y", stack = TRUE,
#'     facet_by = "group2", add_box = TRUE, add_bg = TRUE,
#'     bg_palette = "Paired")
#' }
ViolinPlot <- function(
    data,
    x,
    x_sep = "_",
    y = NULL,
    in_form = c("long", "wide"),
    split_by = NULL,
    split_by_sep = "_",
    symnum_args = NULL,
    sort_x = NULL,
    flip = FALSE,
    keep_empty = FALSE,
    keep_na = FALSE,
    group_by = NULL,
    group_by_sep = "_",
    group_name = NULL,
    paired_by = NULL,
    x_text_angle = ifelse(isTRUE(flip), 0, 45),
    step_increase = 0.1,
    fill_mode = ifelse(!is.null(group_by), "dodge", "x"),
    palreverse = FALSE,
    position_dodge_preserve = "total",
    theme = "theme_this",
    theme_args = list(),
    palette = "Paired",
    palcolor = NULL,
    alpha = 1,
    aspect.ratio = NULL,
    legend.position = "right",
    legend.direction = "vertical",
    add_point = FALSE,
    pt_color = if (isTRUE(add_beeswarm)) NULL else "grey30",
    pt_size = NULL,
    pt_alpha = 1,
    jitter_width = NULL,
    jitter_height = 0,
    stack = FALSE,
    y_max = NULL,
    y_min = NULL,
    y_brackets = NULL,
    add_beeswarm = FALSE,
    beeswarm_method = "swarm",
    beeswarm_cex = 1,
    beeswarm_priority = "ascending",
    beeswarm_dodge = 0.9,
    add_box = FALSE,
    box_color = "black",
    box_width = 0.1,
    box_ptsize = 2.5,
    add_trend = FALSE,
    trend_color = NULL,
    trend_linewidth = 1,
    trend_ptsize = 2,
    add_stat = NULL,
    stat_name = NULL,
    stat_color = "black",
    stat_size = 1,
    stat_stroke = 1,
    stat_shape = 25,
    add_bg = FALSE,
    bg_palette = "stripe",
    bg_palcolor = NULL,
    bg_alpha = 0.2,
    add_line = NULL,
    line_color = "red2",
    line_width = .6,
    line_type = 2,
    highlight = NULL,
    highlight_color = "red2",
    highlight_size = 1,
    highlight_alpha = 1,
    comparisons = NULL,
    ref_group = NULL,
    pairwise_method = "wilcox.test",
    multiplegroup_comparisons = FALSE,
    multiple_method = "kruskal.test",
    sig_label = "p.format",
    sig_labelsize = 3.5,
    hide_ns = FALSE,
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
    ...
) {
    stat_name <- stat_name %||%
        paste0(y, " (", deparse(substitute(add_stat)), ")")
    BoxViolinPlot(
        data = data,
        x = x,
        x_sep = x_sep,
        y = y,
        base = "violin",
        in_form = in_form,
        split_by = split_by,
        split_by_sep = split_by_sep,
        sort_x = sort_x,
        flip = flip,
        keep_empty = keep_empty,
        keep_na = keep_na,
        group_by = group_by,
        group_by_sep = group_by_sep,
        group_name = group_name,
        paired_by = paired_by,
        x_text_angle = x_text_angle,
        fill_mode = fill_mode,
        palreverse = palreverse,
        step_increase = step_increase,
        theme = theme,
        theme_args = theme_args,
        palette = palette,
        palcolor = palcolor,
        alpha = alpha,
        position_dodge_preserve = position_dodge_preserve,
        aspect.ratio = aspect.ratio,
        legend.position = legend.position,
        legend.direction = legend.direction,
        add_point = add_point,
        pt_color = pt_color,
        pt_size = pt_size,
        pt_alpha = pt_alpha,
        symnum_args = symnum_args,
        jitter_width = jitter_width,
        jitter_height = jitter_height,
        stack = stack,
        y_max = y_max,
        y_min = y_min,
        y_brackets = y_brackets,
        add_beeswarm = add_beeswarm,
        beeswarm_method = beeswarm_method,
        beeswarm_cex = beeswarm_cex,
        beeswarm_priority = beeswarm_priority,
        beeswarm_dodge = beeswarm_dodge,
        add_box = add_box,
        box_color = box_color,
        box_width = box_width,
        box_ptsize = box_ptsize,
        add_trend = add_trend,
        trend_color = trend_color,
        trend_linewidth = trend_linewidth,
        trend_ptsize = trend_ptsize,
        add_stat = add_stat,
        stat_name = stat_name,
        stat_color = stat_color,
        stat_size = stat_size,
        stat_stroke = stat_stroke,
        stat_shape = stat_shape,
        add_bg = add_bg,
        bg_palette = bg_palette,
        bg_palcolor = bg_palcolor,
        bg_alpha = bg_alpha,
        add_line = add_line,
        line_color = line_color,
        line_width = line_width,
        line_type = line_type,
        highlight = highlight,
        highlight_color = highlight_color,
        highlight_size = highlight_size,
        highlight_alpha = highlight_alpha,
        comparisons = comparisons,
        ref_group = ref_group,
        pairwise_method = pairwise_method,
        multiplegroup_comparisons = multiplegroup_comparisons,
        multiple_method = multiple_method,
        sig_label = sig_label,
        sig_labelsize = sig_labelsize,
        hide_ns = hide_ns,
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
        combine = combine,
        nrow = nrow,
        ncol = ncol,
        byrow = byrow,
        axes = axes,
        axis_titles = axis_titles,
        guides = guides,
        ...
    )
}

#' @description
#' `BeeswarmPlot` draws beeswarm plots — points arranged by the beeswarm algorithm to
#' avoid overlap while displaying the distribution density.  This is a
#' convenience wrapper that delegates to \code{\link{BoxViolinPlot}} with
#' \code{base = "none"} and \code{add_beeswarm = TRUE}.
#'
#' Requires the \pkg{ggbeeswarm} package.  To get a beeswarm plot WITH a
#' box plot, use \code{BeeswarmPlot(..., add_box = TRUE)}.  To get a violin
#' plot with beeswarm points, use \code{ViolinPlot(..., add_beeswarm = TRUE)}.
#'
#' @rdname boxviolinplot
#' @inheritParams BoxViolinPlot
#' @param add_violin Logical; whether to add a violin plot behind the
#'  beeswarm points.  **Not supported** — the function will stop with an
#'  error directing you to use \code{ViolinPlot(..., add_beeswarm = TRUE)}
#'  instead.
#' @export
#' @examples
#' \donttest{
#' # Basic beeswarm
#' BeeswarmPlot(data, x = "x", y = "y")
#'
#' # Control point size
#' BeeswarmPlot(data, x = "x", y = "y", pt_size = 1)
#'
#' # Beeswarm with box overlay
#' BeeswarmPlot(data, x = "x", y = "y", add_box = TRUE, pt_color = "grey30")
#'
#' # Grouped
#' BeeswarmPlot(data, x = "x", y = "y", group_by = "group1")
#'
#' # Grouped without dodging
#' BeeswarmPlot(data, x = "x", y = "y", group_by = "group1",
#'              beeswarm_dodge = NULL)
#'
#' # Hex layout with wider spacing
#' BeeswarmPlot(data,
#'     x = "x", y = "y", beeswarm_method = "hex",
#'     beeswarm_cex = 2)
#' }
BeeswarmPlot <- function(
    data,
    x,
    x_sep = "_",
    y = NULL,
    in_form = c("long", "wide"),
    split_by = NULL,
    split_by_sep = "_",
    symnum_args = NULL,
    sort_x = NULL,
    flip = FALSE,
    keep_empty = FALSE,
    keep_na = FALSE,
    group_by = NULL,
    group_by_sep = "_",
    group_name = NULL,
    paired_by = NULL,
    x_text_angle = ifelse(isTRUE(flip), 0, 45),
    step_increase = 0.1,
    fill_mode = ifelse(!is.null(group_by), "dodge", "x"),
    palreverse = FALSE,
    theme = "theme_this",
    theme_args = list(),
    palette = "Paired",
    palcolor = NULL,
    alpha = 1,
    aspect.ratio = NULL,
    legend.position = "right",
    legend.direction = "vertical",
    pt_color = NULL,
    pt_size = NULL,
    pt_alpha = 1,
    position_dodge_preserve = "total",
    jitter_width = NULL,
    jitter_height = 0,
    stack = FALSE,
    y_max = NULL,
    y_min = NULL,
    y_brackets = NULL,
    add_violin = FALSE,
    beeswarm_method = "swarm",
    beeswarm_cex = 1,
    beeswarm_priority = "ascending",
    beeswarm_dodge = 0.9,
    add_box = FALSE,
    box_color = "black",
    box_width = 0.1,
    box_ptsize = 2.5,
    add_trend = FALSE,
    trend_color = NULL,
    trend_linewidth = 1,
    trend_ptsize = 2,
    add_stat = NULL,
    stat_name = NULL,
    stat_color = "black",
    stat_size = 1,
    stat_stroke = 1,
    stat_shape = 25,
    add_bg = FALSE,
    bg_palette = "stripe",
    bg_palcolor = NULL,
    bg_alpha = 0.2,
    add_line = NULL,
    line_color = "red2",
    line_width = .6,
    line_type = 2,
    highlight = NULL,
    highlight_color = "red2",
    highlight_size = 1,
    highlight_alpha = 1,
    comparisons = NULL,
    ref_group = NULL,
    pairwise_method = "wilcox.test",
    multiplegroup_comparisons = FALSE,
    multiple_method = "kruskal.test",
    sig_label = "p.format",
    sig_labelsize = 3.5,
    hide_ns = FALSE,
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
    ...
) {
    if (isTRUE(add_violin)) {
        stop(
            "Adding violin to a beeswarm plot is not supported. Please use ViolinPlot(..., add_beeswarm = TRUE) instead."
        )
    }
    stat_name <- stat_name %||%
        paste0(y, " (", deparse(substitute(add_stat)), ")")

    BoxViolinPlot(
        data = data,
        x = x,
        x_sep = x_sep,
        y = y,
        base = "none",
        in_form = in_form,
        split_by = split_by,
        split_by_sep = split_by_sep,
        sort_x = sort_x,
        flip = flip,
        keep_empty = keep_empty,
        keep_na = keep_na,
        group_by = group_by,
        group_by_sep = group_by_sep,
        group_name = group_name,
        paired_by = paired_by,
        x_text_angle = x_text_angle,
        fill_mode = fill_mode,
        palreverse = palreverse,
        step_increase = step_increase,
        theme = theme,
        theme_args = theme_args,
        palette = palette,
        palcolor = palcolor,
        alpha = alpha,
        position_dodge_preserve = position_dodge_preserve,
        aspect.ratio = aspect.ratio,
        legend.position = legend.position,
        legend.direction = legend.direction,
        add_point = TRUE,
        pt_color = pt_color,
        pt_size = pt_size,
        pt_alpha = pt_alpha,
        symnum_args = symnum_args,
        jitter_width = jitter_width,
        jitter_height = jitter_height,
        stack = stack,
        y_max = y_max,
        y_min = y_min,
        y_brackets = y_brackets,
        add_beeswarm = TRUE,
        beeswarm_method = beeswarm_method,
        beeswarm_cex = beeswarm_cex,
        beeswarm_priority = beeswarm_priority,
        beeswarm_dodge = beeswarm_dodge,
        add_box = add_box,
        box_color = box_color,
        box_width = box_width,
        box_ptsize = box_ptsize,
        add_trend = add_trend,
        trend_color = trend_color,
        trend_linewidth = trend_linewidth,
        trend_ptsize = trend_ptsize,
        add_stat = add_stat,
        stat_name = stat_name,
        stat_color = stat_color,
        stat_size = stat_size,
        stat_stroke = stat_stroke,
        stat_shape = stat_shape,
        add_bg = add_bg,
        bg_palette = bg_palette,
        bg_palcolor = bg_palcolor,
        bg_alpha = bg_alpha,
        add_line = add_line,
        line_color = line_color,
        line_width = line_width,
        line_type = line_type,
        highlight = highlight,
        highlight_color = highlight_color,
        highlight_size = highlight_size,
        highlight_alpha = highlight_alpha,
        comparisons = comparisons,
        ref_group = ref_group,
        pairwise_method = pairwise_method,
        multiplegroup_comparisons = multiplegroup_comparisons,
        multiple_method = multiple_method,
        sig_label = sig_label,
        sig_labelsize = sig_labelsize,
        hide_ns = hide_ns,
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
        combine = combine,
        nrow = nrow,
        ncol = ncol,
        byrow = byrow,
        axes = axes,
        axis_titles = axis_titles,
        guides = guides,
        ...
    )
}
