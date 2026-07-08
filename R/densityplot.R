#' Atomic density/histogram plot
#'
#' @description
#' Core implementation for density and histogram plots. This is the internal workhorse
#' dispatched by both the `DensityPlot()` and `Histogram()` public wrappers. It
#' renders a grouped density curve or histogram, with optional data-distribution
#' bars along the y=0 axis, trend-line interpolation (histogram only), and full
#' faceting support.
#'
#' The function supports two plotting modes selected by `type`:
#' - **density** — renders `ggplot2::geom_density()` for each group.
#' - **histogram** — renders `ggplot2::geom_histogram()` with optional trend
#'   overlays (`use_trend`, `add_trend`), including zero-skip interpolation
#'   (`trend_skip_zero`) that uses `zoo::na.approx()` to bridge gaps where a
#'   bin has zero observations under a transformed y-axis.
#'
#' When `add_bars = TRUE`, a data rug is drawn along the bottom of the plot using
#' `ggplot2::geom_linerange()`. Each group's bars are offset vertically so they
#' stack without overlapping.
#'
#' @section Architecture:
#' **DensityHistoPlotAtomic** executes the following steps:
#'
#' 1. **ggplot dispatch** — selects `gglogger::ggplot` or `ggplot2::ggplot`
#'    based on `getOption("plotthis.gglogger.enabled")`.
#' 2. **Type check** — `match.arg(type, c("density", "histogram"))`.
#' 3. **Expansion normalization** — `norm_expansion()` converts the `expand`
#'    vector into CSS-padding-style x/y components.
#' 4. **Column resolution** — `check_columns()` validates `x`, `group_by`
#'    (force_factor, allow_multi, concat_multi), and `facet_by`.
#' 5. **Default group** — when `group_by` is `NULL`, a synthetic `.group`
#'    factor with a single empty-string level is created so the colour-mapping
#'    pipeline runs uniformly.
#' 6. **Histogram bin default** — if `type = "histogram"` and neither `bins`
#'    nor `binwidth` is set, `bins = 30` with a message.
#' 7. **Add-bars pre-calculation** — when `add_bars = TRUE`:
#'    - For density: max y = `max(density(x)$y) * 1.5`.
#'    - For histogram: max y = max bin count from `cut(x, s)`.
#'    - Computes `.ymin` and `.ymax` per row, offset by `bar_height * max_y`
#'      for each group so rugs stack without colliding.
#' 8. **NA / empty handling** — `process_keep_na_empty()` filters data and
#'    `keep_empty` values are extracted for group and facet dimensions.
#' 9. **Palette resolution** — `palette_this()` maps group levels to colours.
#' 10. **Base ggplot + scales** — initialises `ggplot(data, aes(x, fill, color))`,
#'     then adds `scale_fill_manual()` / `scale_color_manual()`. When
#'     `keep_empty_group` is `TRUE`, `drop = FALSE`, `breaks`, and `limits`
#'     are set to preserve empty factor levels.
#' 11. **Geometry layer**:
#'     - *Histogram (no trend)*: `geom_histogram(alpha, bins, binwidth, position)`.
#'     - *Histogram (use_trend / add_trend)*: adds `stat_bin(geom = "point")`
#'       for trend points + `stat_bin(geom = "line")` for the trend curve.
#'       When `trend_skip_zero = TRUE`, an `after_stat()` expression sets zero
#'       counts to `NA`, transforms y, applies `zoo::na.approx()` per
#'       `..group..`, and inverts — producing a continuous trend that
#'       interpolates over empty bins.
#'     - *Density*: `geom_density(alpha, position)`.
#' 12. **Add-bars geometry** — if `add_bars = TRUE`, `geom_linerange()` draws
#'     vertical ticks at the pre-computed `.ymin` / `.ymax` positions.
#' 13. **Scales, theme, labels** — x/y continuous scales with transforms,
#'     theme applied via `do_call(theme, theme_args)`, and axis / title
#'     labels (default y-lab: "Count" for histogram, "Density" for density).
#' 14. **Flip** — optional `coord_flip()`.
#' 15. **Dimension calculation** — `calculate_plot_dimensions(base_height = 3.5,
#'     aspect.ratio, legend, flip)` sets `height` / `width` attributes.
#' 16. **Faceting** — `facet_plot()` applies `facet_grid` / `facet_wrap`.
#'
#' @inheritParams common_args
#' @param x A character string specifying the column name for the x-axis values.
#'   A numeric column is expected.
#' @param group_by A character string specifying the column(s) to group the data
#'   by. Multiple columns are concatenated with `group_by_sep`. Each group
#'   receives a distinct fill and outline colour.
#' @param group_by_sep A character string used to join multiple `group_by`
#'   column values into a single factor level. Default: `"_"`.
#' @param group_name A character string used as the legend title for the
#'   `group_by` aesthetic. When `NULL` (default), the (possibly concatenated)
#'   `group_by` column name is used.
#' @param xtrans A character string specifying the transformation applied to
#'   the x-axis. Passed to `ggplot2::scale_x_continuous(transform = ...)`.
#'   Supported values include `"identity"` (default), `"log10"`, `"log2"`,
#'   `"sqrt"`, `"reverse"`, etc.
#' @param ytrans A character string specifying the transformation applied to
#'   the y-axis. Passed to `ggplot2::scale_y_continuous(transform = ...)`.
#'   Used by `trend_skip_zero` to correctly interpolate across zero bins on
#'   a transformed scale. Default: `"identity"`.
#' @param type A character string specifying the plot type. `"density"` (default)
#'   renders `geom_density()`; `"histogram"` renders `geom_histogram()` with
#'   optional trend overlays.
#' @param bins A numeric value specifying the number of bins for the histogram.
#'   Ignored when `type = "density"`. Defaults to `30` when neither `bins` nor
#'   `binwidth` is provided.
#' @param binwidth A numeric value specifying the width of individual bins for
#'   the histogram. Ignored when `type = "density"`. Takes precedence over
#'   `bins` when both are set.
#' @param flip A logical value. If `TRUE`, the x and y axes are swapped via
#'   `coord_flip()`. Dimension calculation accounts for the flip.
#' @param add_bars A logical value. If `TRUE`, a data-distribution rug is
#'   drawn along the y = 0 axis using `geom_linerange()`. Each group's bars
#'   are vertically offset to avoid overlap.
#' @param bar_height A numeric value specifying the height (in data units,
#'   relative to the maximum y) of the rug bars added by `add_bars`.
#'   The actual pixel height scales with `max_y`. Default: `0.025`.
#' @param bar_alpha A numeric value in `[0, 1]` for the transparency of the
#'   rug bars. Default: `1`.
#' @param bar_width A numeric value passed as the `linewidth` aesthetic of
#'   `geom_linerange()`. Controls the thickness of each rug tick.
#'   Default: `0.1`.
#' @param position A character string specifying the position adjustment for
#'   the bars or density curves. Default: `"identity"`, which shows the
#'   actual count / density per group (unlike `ggplot2`'s default `"stack"`).
#'   Other options: `"stack"`, `"dodge"`, `"fill"`.
#' @param use_trend A logical value. If `TRUE`, the histogram bars are replaced
#'   entirely by a trend line (points + connecting line). Only applies when
#'   `type = "histogram"`.
#' @param add_trend A logical value. If `TRUE`, a trend line is overlaid on
#'   top of the histogram bars. Only applies when `type = "histogram"`.
#' @param trend_alpha A numeric value in `[0, 1]` controlling the transparency
#'   of the trend points and line. Default: `1`.
#' @param trend_linewidth A numeric value for the thickness of the trend line.
#'   Default: `0.8`.
#' @param trend_pt_size A numeric value for the size of the trend points.
#'   Default: `1.5`.
#' @param trend_skip_zero A logical value. If `TRUE`, bins with zero count are
#'   set to `NA` before the trend line is computed, and `zoo::na.approx()` is
#'   used to interpolate across the gaps — producing a continuous curve even
#'   when some bins are empty. Requires `ytrans` to be correctly specified.
#'   Only applies when `type = "histogram"` and `use_trend` or `add_trend` is
#'   active.
#' @importFrom utils getFromNamespace
#' @importFrom zoo na.approx
#' @importFrom ggplot2 geom_density scale_fill_manual labs theme geom_histogram coord_flip waiver
#' @importFrom ggplot2 scale_color_manual scale_x_continuous scale_y_continuous stat_bin
#' @keywords internal
DensityHistoPlotAtomic <- function(
    data,
    x,
    group_by = NULL,
    group_by_sep = "_",
    group_name = NULL,
    xtrans = "identity",
    ytrans = "identity",
    type = c("density", "histogram"),
    bins = NULL,
    binwidth = NULL,
    flip = FALSE,
    keep_na = FALSE,
    keep_empty = FALSE,
    add_bars = FALSE,
    bar_height = 0.025,
    bar_alpha = 1,
    bar_width = .1,
    position = "identity",
    use_trend = FALSE,
    add_trend = FALSE,
    trend_alpha = 1,
    trend_linewidth = 0.8,
    trend_pt_size = 1.5,
    trend_skip_zero = FALSE,
    palette = "Paired",
    palcolor = NULL,
    palreverse = FALSE,
    alpha = .5,
    theme = "theme_this",
    theme_args = list(),
    aspect.ratio = 1,
    title = NULL,
    subtitle = NULL,
    xlab = NULL,
    ylab = NULL,
    expand = c(bottom = 0, left = 0, right = 0),
    facet_by = NULL,
    facet_scales = "fixed",
    facet_ncol = NULL,
    facet_nrow = NULL,
    facet_byrow = TRUE,
    legend.position = ifelse(is.null(group_by), "none", "right"),
    legend.direction = "vertical",
    ...
) {
    ggplot <- if (getOption("plotthis.gglogger.enabled", FALSE)) {
        gglogger::ggplot
    } else {
        ggplot2::ggplot
    }
    type <- match.arg(type)
    expand <- norm_expansion(
        expand,
        x_type = "continuous",
        y_type = "continuous"
    )
    x <- check_columns(data, x)
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
        group_by <- ".group"
        data[[group_by]] <- factor("")
    }
    if (is.null(bins) && is.null(binwidth) && type == "histogram") {
        bins <- 30
        message("Using `bins = 30`. Pick better value with `binwidth`.")
    }
    if (isTRUE(add_bars)) {
        if (type == "density") {
            # calculate the max density for the y-axis
            max_y <- max(stats::density(data[[x]])$y) * 1.5
        } else {
            # calculate the max count for the y-axis by bins
            if (is.null(bins) && is.null(binwidth)) {
                s <- seq(min(data[[x]]), max(data[[x]]), length.out = 30)
            } else if (!is.null(bins)) {
                s <- seq(min(data[[x]]), max(data[[x]]), length.out = bins)
            } else {
                s <- seq(min(data[[x]]), max(data[[x]]), by = binwidth)
            }
            max_y <- max(table(cut(data[[x]], s)))
        }
        lnheight <- bar_height * max_y
        # calculate the ymin ymax for each group to plot the data lines
        data$.ymin <- lnheight * (1 - as.integer(data[[group_by]]))
        data$.ymax <- data$.ymin - lnheight
    }
    data <- process_keep_na_empty(data, keep_na, keep_empty)
    keep_empty_group <- if (!is.null(group_by)) keep_empty[[group_by]] else NULL
    keep_empty_facet <- if (!is.null(facet_by)) keep_empty[[facet_by]] else NULL
    if (length(facet_by) > 1) {
        stopifnot(
            "[Density/HistoPlot] `keep_empty` for `facet_by` variables must be identical." = identical(
                keep_empty_facet,
                keep_empty[[facet_by[2]]]
            )
        )
    }
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

    p <- ggplot(
        data,
        aes(x = !!sym(x), fill = !!sym(group_by), color = !!sym(group_by))
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
            ) +
            scale_color_manual(
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
            ) +
            scale_color_manual(
                name = group_name %||% group_by,
                values = group_colors,
                na.value = group_colors['NA'] %||% "grey80"
            )
    }

    if (type == "histogram") {
        if (!use_trend) {
            p <- p +
                geom_histogram(
                    alpha = alpha,
                    bins = bins,
                    binwidth = binwidth,
                    position = position,
                    show.legend = TRUE,
                    ...
                )
        }
        if (use_trend || add_trend) {
            p <- p +
                stat_bin(
                    geom = "point",
                    bins = bins,
                    binwidth = binwidth,
                    alpha = trend_alpha,
                    size = trend_pt_size,
                    position = position,
                    ...
                )
            if (trend_skip_zero) {
                if (inherits(ytrans, "transform")) {
                    ytrans_obj <- ytrans
                } else if (is.character(ytrans)) {
                    ytrans_obj <- getFromNamespace(
                        paste0("transform_", ytrans),
                        "scales"
                    )()
                } else if (is.function(ytrans)) {
                    ytrans_obj <- ytrans()
                } else {
                    stop(
                        "ytrans should be a character, a transform object, or a function returning a transform object."
                    )
                }
                p <- p +
                    stat_bin(
                        aes(
                            y = after_stat({
                                y <- ifelse(
                                    !!sym("count") > 0,
                                    !!sym("count"),
                                    NA
                                )
                                y <- ytrans_obj$transform(y)
                                y <- split(y, !!sym("..group.."))
                                y <- unlist(lapply(y, na.approx, na.rm = FALSE))
                                ytrans_obj$inverse(y)
                            })
                        ),
                        bins = bins,
                        binwidth = binwidth,
                        geom = "line",
                        position = position,
                        linewidth = trend_linewidth,
                        ...
                    )
            } else {
                p <- p +
                    stat_bin(
                        aes(y = after_stat(!!sym("count"))),
                        bins = bins,
                        binwidth = binwidth,
                        geom = "line",
                        position = position,
                        linewidth = trend_linewidth,
                        ...
                    )
            }
        }
    } else {
        p <- p +
            geom_density(
                alpha = alpha,
                position = position,
                show.legend = TRUE,
                ...
            )
    }
    if (isTRUE(add_bars)) {
        p <- p +
            geom_linerange(
                aes(ymin = !!sym(".ymin"), ymax = !!sym(".ymax")),
                alpha = bar_alpha,
                linewidth = bar_width
            )
    }
    p <- p +
        scale_x_continuous(expand = expand$x, transform = xtrans) +
        scale_y_continuous(expand = expand$y, transform = ytrans) +
        do_call(theme, theme_args) +
        labs(
            title = title,
            subtitle = subtitle,
            x = xlab %||% x,
            y = ylab %||% ifelse(type == "histogram", "Count", "Density")
        ) +
        ggplot2::theme(
            aspect.ratio = aspect.ratio,
            legend.position = legend.position,
            legend.direction = legend.direction
        )

    if (flip) {
        p <- p + coord_flip()
    }

    dims <- calculate_plot_dimensions(
        base_height = 3.5,
        aspect.ratio = aspect.ratio,
        legend.position = legend.position,
        legend.direction = legend.direction,
        legend_n = length(group_vals),
        legend_nchar = max(nchar(as.character(group_vals)), na.rm = TRUE),
        flip = flip
    )
    attr(p, "height") <- dims$height
    attr(p, "width") <- dims$width

    facet_plot(
        p,
        facet_by = facet_by,
        facet_scales = facet_scales,
        ncol = facet_ncol,
        nrow = facet_nrow,
        byrow = facet_byrow,
        legend.position = legend.position,
        legend.direction = legend.direction
    )
}

#' Atomic ridge plot
#'
#' @description
#' Core implementation for ridge (joy) plots. Renders overlapping density curves
#' for each group on the y-axis using `ggridges::geom_density_ridges()`, with
#' optional vertical reference lines and wide-to-long data conversion.
#'
#' The function accepts data in two forms:
#' - **long form** (default) — a numeric `x` column plus a `group_by` factor
#'   column whose levels become the y-axis ridges.
#' - **wide form** — multiple numeric columns named in `group_by` are gathered
#'   via `tidyr::pivot_longer()` into `.x` / `.group` columns, then processed
#'   identically to the long form.
#'
#' Vertical reference lines (`add_vline`) can be specified as a numeric vector
#' (same lines for all groups), a named list (per-group values), or `TRUE`
#' (group means). When `vline_color = TRUE`, each line is coloured with a
#' darkened blend of the corresponding ridge fill.
#'
#' @section Architecture:
#' **RidgePlotAtomic** executes the following steps:
#'
#' 1. **ggplot dispatch** — selects `gglogger::ggplot` or `ggplot2::ggplot`.
#' 2. **Wide-to-long conversion** — when `in_form = "wide"`, calls
#'    `tidyr::pivot_longer()` on the `group_by` columns, producing `.group`
#'    (factor) and `.x` (values). `x` and `group_by` are redirected to these
#'    synthetic columns.
#' 3. **Column resolution** — `check_columns()` validates `x`, `group_by`
#'    (force_factor, allow_multi, concat_multi), and `facet_by`.
#' 4. **Default group** — when `group_by` is `NULL`, a synthetic `.group`
#'    factor with a single space character level is created so the fill
#'    pipeline runs uniformly.
#' 5. **Reverse ordering** — if `reverse = TRUE`, factor levels of `group_by`
#'    are reversed, flipping the y-axis ridge order.
#' 6. **NA / empty handling** — `process_keep_na_empty()` filters data. When
#'    `reverse = TRUE` and any group value is `NA`, the NA level is renamed
#'    to the literal string `"NA"` and moved to the end of the factor so
#'    colour mapping and display remain consistent.
#' 7. **Palette resolution** — `palette_this()` maps group levels to fill
#'    colours.
#' 8. **Base ggplot** — initialises `ggplot(data, aes(x, y, fill))` with
#'    `group_by` on the y-axis.
#' 9. **Ridge geometry** — `ggridges::geom_density_ridges(alpha, scale)`.
#'    When `scale` is `NULL`, ggridges auto-computes the overlap factor.
#' 10. **Vertical reference lines** — if `add_vline` is not `NULL` / `FALSE`:
#'     - `add_vline = TRUE` → computes `tapply(x, group_by, mean)`.
#'     - `vline_color = TRUE` → resolves per-group line colours by
#'       darkening each fill colour via `blend_colors(mode = "multiply")`.
#'       Named list elements are matched to factor levels.
#'     - Adds `geom_vline(xintercept, linetype, linewidth, color, alpha)`.
#' 11. **Scales and labels** — `scale_y_discrete(drop = !keep_empty_group)`,
#'     `scale_x_continuous()`, and `labs()`.
#' 12. **Fill scale** — `scale_fill_manual()`. When `keep_empty_group = TRUE`,
#'     `drop = FALSE`, `breaks`, and `limits` are set to preserve empty
#'     factor levels.
#' 13. **Flip-aware theme** — when `flip = TRUE`:
#'     - `coord_flip()` is applied.
#'     - x-axis text angle is set from `x_text_angle` with computed
#'       `hjust` / `vjust` via `calc_just()`.
#'     - Major grid lines are drawn on the x-axis.
#'     - When `flip = FALSE`, y-axis text is right-aligned and grid lines
#'       appear on the y-axis.
#' 14. **Theme application** — `do_call(theme, theme_args)` applies the
#'     resolved theme function, then `aspect.ratio` and legend position are
#'     set.
#' 15. **Dimension calculation** — `calculate_plot_dimensions(base_height = 1,
#'     n_y = nlevels(group_by), y_scale_factor = 1, aspect.ratio, legend,
#'     flip)` sets `height` / `width` attributes. The base height of 1 unit
#'     per ridge keeps individual ridges compact.
#' 16. **Faceting** — `facet_plot()` applies `facet_grid` / `facet_wrap`, with
#'     `drop = !keep_empty_facet`.
#'
#' @inheritParams common_args
#' @param data A data frame. Accepted in two forms:
#'   - **long** (`in_form = "long"`): a numeric column (named by `x`) and a
#'     factor column (named by `group_by`) whose levels become y-axis ridges.
#'   - **wide** (`in_form = "wide"`): multiple numeric columns listed in
#'     `group_by` are gathered into `.x` / `.group` via `tidyr::pivot_longer()`.
#' @param x A character string specifying the column name for the numeric
#'   values plotted on the x-axis. When `in_form = "wide"`, `x` should be
#'   `NULL`; the gathered values are stored in a synthetic `.x` column.
#' @param in_form A character string specifying whether `data` is in
#'   `"long"` (default) or `"wide"` format.
#' @param group_by A character string specifying the column(s) whose levels
#'   define the individual ridges on the y-axis. Multiple columns are
#'   concatenated with `group_by_sep`. In wide mode, these are the column
#'   names to gather.
#' @param group_by_sep A character string used to join multiple `group_by`
#'   column values into a single factor level. In wide form the columns are
#'   not concatenated (each becomes its own ridge). Default: `"_"`.
#' @param group_name A character string used as the legend title for the
#'   `group_by` fill aesthetic. Defaults to the (concatenated) `group_by`
#'   column name.
#' @param flip A logical value. If `TRUE`, the axes are swapped via
#'   `coord_flip()`. X-axis text angle and grid-line placement are adjusted
#'   accordingly.
#' @param alpha A numeric value in `[0, 1]` for the transparency of the
#'   ridge fill. Default: `0.8`.
#' @param reverse A logical value. If `TRUE`, the y-axis group order is
#'   reversed. NA groups are renamed to the literal string `"NA"` and
#'   placed at the end.
#' @param scale A numeric value controlling the vertical overlap of ridges.
#'   Passed to `ggridges::geom_density_ridges(scale = ...)`. Smaller values
#'   increase overlap. When `NULL`, ggridges auto-computes the scale.
#' @param add_vline A specification for vertical reference lines:
#'   - `NULL` or `FALSE`: no lines.
#'   - `TRUE`: draw a line at the mean of each group.
#'   - A numeric vector: draw the same lines for all groups.
#'   - A named list of numeric vectors: per-group lines, where names should
#'     match `group_by` levels.
#' @param vline_type A character string specifying the line type for the
#'   vertical reference lines. Passed as `linetype` to `geom_vline()`.
#'   Default: `"solid"`.
#' @param vline_color The colour of the vertical reference lines:
#'   - A literal colour value or vector (recycled): applied directly.
#'   - `TRUE` (default): each line is coloured with a darkened blend of
#'     the corresponding ridge fill colour, computed via
#'     `blend_colors(mode = "multiply")`.
#' @param vline_width A numeric value for the thickness of the vertical
#'   reference lines. Passed as `linewidth` to `geom_vline()`.
#'   Default: `0.5`.
#' @param vline_alpha A numeric value in `[0, 1]` for the transparency of
#'   the vertical reference lines. Default: `1`.
#' @param x_text_angle A numeric value specifying the angle (in degrees) for
#'   x-axis text when `flip = TRUE`. Used with `calc_just()` to compute
#'   optimal `hjust` / `vjust`. Default: `90`.
#' @param x_min,x_max Numeric limits for the x-axis. When `NULL` (default),
#'   limits are determined from the data range. Passed to `coord_cartesian()`.
#' @param ... Additional arguments passed to `ggridges::geom_density_ridges()`
#'   (bandwidth, jittered_points, quantile_lines, etc.).
#' @importFrom tidyr pivot_longer
#' @keywords internal
RidgePlotAtomic <- function(
    data,
    x = NULL,
    in_form = c("long", "wide"),
    group_by = NULL,
    group_by_sep = "_",
    group_name = NULL,
    add_vline = NULL,
    vline_type = "solid",
    vline_color = TRUE,
    vline_width = 0.5,
    vline_alpha = 1,
    flip = FALSE,
    alpha = 0.8,
    scale = NULL,
    theme = "theme_this",
    theme_args = list(),
    palette = "Paired",
    palcolor = NULL,
    palreverse = FALSE,
    title = NULL,
    subtitle = NULL,
    xlab = NULL,
    ylab = NULL,
    x_text_angle = 90,
    x_min = NULL,
    x_max = NULL,
    keep_na = FALSE,
    keep_empty = FALSE,
    reverse = FALSE,
    facet_by = NULL,
    facet_scales = "fixed",
    facet_ncol = NULL,
    facet_nrow = NULL,
    facet_byrow = TRUE,
    aspect.ratio = 1,
    legend.position = "none",
    legend.direction = "vertical",
    ...
) {
    ggplot <- if (getOption("plotthis.gglogger.enabled", FALSE)) {
        gglogger::ggplot
    } else {
        ggplot2::ggplot
    }
    in_form <- match.arg(in_form)
    if (in_form == "wide") {
        data <- data %>%
            pivot_longer(cols = group_by, names_to = ".group", values_to = ".x")
        x <- ".x"
        group_by <- ".group"
    }
    x <- check_columns(data, x)
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
        group_by <- ".group"
        data[[group_by]] <- factor(" ")
    }
    if (isTRUE(reverse)) {
        data[[group_by]] <- factor(
            data[[group_by]],
            levels = rev(levels(data[[group_by]]))
        )
    }

    data <- process_keep_na_empty(data, keep_na, keep_empty)
    keep_empty_group <- if (!is.null(group_by)) keep_empty[[group_by]] else NULL
    keep_empty_facet <- if (!is.null(facet_by)) keep_empty[[facet_by]] else NULL
    if (length(facet_by) > 1) {
        stopifnot(
            "[RidgePlot] `keep_empty` for `facet_by` variables must be identical." = identical(
                keep_empty_facet,
                keep_empty[[facet_by[2]]]
            )
        )
    }
    group_vals <- levels(data[[group_by]])
    if (anyNA(data[[group_by]])) {
        group_vals <- c(group_vals, NA)
    }

    x_min <- x_min %||% min(data[[x]], na.rm = TRUE)
    x_max <- x_max %||% max(data[[x]], na.rm = TRUE)
    if (x_min == x_max) {
        stop("[RidgePlot] x_min and x_max are equal. Please provide a valid range for the x-axis.")
    }
    colors <- palette_this(
        group_vals,
        palette = palette,
        palcolor = palcolor,
        NA_keep = TRUE,
        reverse = palreverse
    )
    if (anyNA(group_vals) && reverse) {
        names(colors)[is.na(names(colors))] <- "NA"
        group_vals[is.na(group_vals)] <- "NA"
        levels(data[[group_by]]) <- c("NA", setdiff(group_vals, "NA"))
        data[[group_by]][is.na(data[[group_by]])] <- "NA"
    }

    p <- ggplot(
        data,
        aes(x = !!sym(x), y = !!sym(group_by), fill = !!sym(group_by))
    )

    if (!is.null(scale)) {
        p <- p +
            ggridges::geom_density_ridges(
                alpha = alpha,
                scale = scale,
                show.legend = TRUE
            )
    } else {
        # Let the geom_density_ridges function to calculate the scale
        p <- p +
            ggridges::geom_density_ridges(alpha = alpha, show.legend = TRUE)
    }
    if (!is.null(add_vline) && !isFALSE(add_vline)) {
        if (isTRUE(add_vline)) {
            # calculate the mean of each group
            add_vline <- tapply(data[[x]], data[[group_by]], mean, na.rm = TRUE)
        }
        if (isTRUE(vline_color)) {
            if (!is.list(add_vline)) {
                add_vline <- as.list(add_vline)
                if (reverse) {
                    names(add_vline) <- rev(levels(data[[group_by]]))[
                        1:length(add_vline)
                    ]
                } else {
                    names(add_vline) <- levels(data[[group_by]])[
                        1:length(add_vline)
                    ]
                }
            }
            add_vline <- add_vline[intersect(
                levels(data[[group_by]]),
                names(add_vline)
            )]
            vline_color <- sapply(
                colors[names(add_vline)],
                function(cl) blend_colors(c(cl, cl, cl), mode = "multiply")
            )
            add_vline <- unlist(add_vline, use.names = FALSE)
        }
        p <- p +
            geom_vline(
                xintercept = add_vline,
                linetype = vline_type,
                linewidth = vline_width,
                color = vline_color,
                alpha = vline_alpha
            )
    }
    p <- p +
        scale_y_discrete(drop = !isTRUE(keep_empty_group), expand = c(0, 0)) +
        scale_x_continuous(expand = c(0, 0), limits = c(x_min, x_max)) +
        labs(
            title = title,
            subtitle = subtitle,
            x = xlab %||% x,
            y = ylab %||% group_by
        )

    if (isTRUE(keep_empty_group)) {
        p <- p +
            scale_fill_manual(
                name = group_name %||% group_by,
                na.value = colors[length(colors)],
                values = colors,
                breaks = group_vals,
                limits = group_vals,
                drop = FALSE
            )
    } else {
        p <- p +
            scale_fill_manual(
                name = group_name %||% group_by,
                na.value = colors[length(colors)],
                values = colors
            )
    }

    if (flip) {
        just <- calc_just(x_text_angle)
        p <- p +
            ggplot2::theme(
                axis.text.x = element_text(
                    angle = x_text_angle,
                    hjust = just$h,
                    vjust = just$v
                ),
                axis.ticks.x = element_line(),
                panel.grid.major.x = element_line(color = "grey", linetype = 2)
            ) +
            coord_flip()
    } else {
        p <- p +
            ggplot2::theme(
                axis.text.x = element_text(),
                axis.text.y = element_text(hjust = 1),
                axis.ticks.y = element_line(),
                panel.grid.major.y = element_line(color = "grey", linetype = 2)
            )
    }

    p <- p +
        do_call(theme, theme_args) +
        ggplot2::theme(
            aspect.ratio = aspect.ratio,
            legend.position = legend.position,
            legend.direction = legend.direction
        )

    dims <- calculate_plot_dimensions(
        base_height = 1,
        aspect.ratio = aspect.ratio,
        n_y = nlevels(data[[group_by]]),
        y_scale_factor = 1,
        legend.position = legend.position,
        legend.direction = legend.direction,
        legend_n = length(group_vals),
        legend_nchar = max(nchar(as.character(group_vals)), na.rm = TRUE),
        flip = flip
    )
    attr(p, "height") <- dims$height
    attr(p, "width") <- dims$width

    facet_plot(
        p,
        facet_by = facet_by,
        facet_scales = facet_scales,
        ncol = facet_ncol,
        nrow = facet_nrow,
        byrow = facet_byrow,
        legend.position = legend.position,
        legend.direction = legend.direction,
        drop = !isTRUE(keep_empty_facet)
    )
}

#' Ridge Plot
#'
#' @description
#' Ridge (joy) plot for visualising the distribution of a numeric variable across
#' multiple groups. Each group is rendered as a partially overlapping density
#' curve along the y-axis, making it easy to compare distribution shapes, central
#' tendency, and spread across categories.
#'
#' The function supports both **long** and **wide** data formats:
#' - **Long form** (`in_form = "long"`, default) — a numeric column (`x`) plus a
#'   factor column (`group_by`) whose levels become the y-axis ridges.
#' - **Wide form** (`in_form = "wide"`) — multiple numeric columns listed in
#'   `group_by` are gathered internally into long form.
#'
#' Optional vertical reference lines (`add_vline`) can mark group means,
#' specific values, or per-group thresholds. Supports faceting, split-by
#' splitting, and full palette customisation.
#'
#' @section split_by Workflow:
#'
#' When `split_by` is specified, `RidgePlot()` executes the following pipeline:
#'
#' 1. **Argument validation** — `validate_common_args()` checks the seed and
#'    facet-by consistency.
#' 2. **NA / empty normalisation** — `check_keep_na()` / `check_keep_empty()`
#'    convert `keep_na` / `keep_empty` to per-column lists.
#' 3. **Theme resolution** — `process_theme()` resolves the theme string to a
#'    theme function.
#' 4. **Split column resolution** — `check_columns()` validates `split_by`
#'    (force_factor, concat_multi).
#' 5. **Pre-filtering** — `process_keep_na_empty()` removes NA / empty levels
#'    from the split column, then `data` is split by `split_by` levels (order
#'    preserved).
#' 6. **Per-split parameter resolution** — `check_palette()`,
#'    `check_palcolor()`, `check_legend()` resolve palette, palcolor,
#'    legend.position, and legend.direction for each split.
#' 7. **Per-split dispatch** — each split is passed to `RidgePlotAtomic()` with
#'    its resolved parameters. Title defaults to the split level name unless
#'    `title` is a function (in which case it is called with the default).
#' 8. **Combination** — `combine_plots()` assembles the list of plots via
#'    `patchwork::wrap_plots()`, applying `nrow`, `ncol`, `byrow`, `axes`,
#'    `axis_titles`, `guides`, and `design`.
#'
#' @inheritParams common_args
#' @inheritParams RidgePlotAtomic
#' @return A `ggplot` object (single plot), a `patchwork` / `wrap_plots` object
#'   (when `split_by` is provided and `combine = TRUE`), or a list of `ggplot`
#'   objects (when `split_by` is provided and `combine = FALSE`).
#' @export
#' @examples
#' \donttest{
#' set.seed(8525)
#' data <- data.frame(
#'    x = c(rnorm(250, -1), rnorm(250, 1)),
#'    group = factor(rep(c("A", NA, LETTERS[3:5]), each = 100), levels = LETTERS[1:6])
#' )
#'
#' # basic usage
#' RidgePlot(data, x = "x")  # single ridge (no group_by)
#' RidgePlot(data, x = "x", add_vline = 0, vline_color = "black")
#'
#' # grouped ridges
#' RidgePlot(data, x = "x", group_by = "group")
#' RidgePlot(data, x = "x", group_by = "group",
#'    keep_na = TRUE, keep_empty = TRUE)
#' RidgePlot(data, x = "x", group_by = "group", reverse = TRUE)
#' RidgePlot(data, x = "x", group_by = "group",
#'    add_vline = TRUE, vline_color = TRUE, alpha = 0.7)
#'
#' # faceting
#' RidgePlot(data, x = "x", facet_by = "group",
#'    keep_na = TRUE, keep_empty = TRUE)
#'
#' # wide form
#' data_wide <- data.frame(
#'    A = rnorm(100),
#'    B = rnorm(100),
#'    C = rnorm(100),
#'    D = rnorm(100),
#'    E = rnorm(100),
#'    group = sample(letters[1:4], 100, replace = TRUE)
#' )
#' RidgePlot(data_wide, group_by = LETTERS[1:5], in_form = "wide")
#' RidgePlot(data_wide, group_by = LETTERS[1:5], in_form = "wide", facet_by = "group")
#'
#' # split_by with per-split palettes
#' RidgePlot(data_wide, group_by = LETTERS[1:5], in_form = "wide", split_by = "group",
#'    palette = list(a = "Reds", b = "Blues", c = "Greens", d = "Purples"))
#' }
RidgePlot <- function(
    data,
    x = NULL,
    in_form = c("long", "wide"),
    split_by = NULL,
    split_by_sep = "_",
    group_by = NULL,
    group_by_sep = "_",
    group_name = NULL,
    scale = NULL,
    keep_na = FALSE,
    keep_empty = FALSE,
    add_vline = NULL,
    vline_type = "solid",
    vline_color = TRUE,
    vline_width = 0.5,
    vline_alpha = 1,
    flip = FALSE,
    alpha = 0.8,
    theme = "theme_this",
    theme_args = list(),
    palette = "Paired",
    palcolor = NULL,
    palreverse = FALSE,
    title = NULL,
    subtitle = NULL,
    xlab = NULL,
    ylab = NULL,
    x_text_angle = 90,
    x_min = NULL,
    x_max = NULL,
    reverse = FALSE,
    facet_by = NULL,
    facet_scales = "fixed",
    facet_ncol = NULL,
    facet_nrow = NULL,
    facet_byrow = TRUE,
    aspect.ratio = 1,
    legend.position = "none",
    legend.direction = "vertical",
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
    keep_na <- check_keep_na(keep_na, c(group_by, split_by, facet_by))
    keep_empty <- check_keep_empty(keep_empty, c(group_by, split_by, facet_by))
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
            RidgePlotAtomic(
                datas[[nm]],
                x = x,
                in_form = in_form,
                group_by = group_by,
                group_by_sep = group_by_sep,
                group_name = group_name,
                scale = scale,
                add_vline = add_vline,
                vline_type = vline_type,
                vline_color = vline_color,
                vline_width = vline_width,
                vline_alpha = vline_alpha,
                flip = flip,
                alpha = alpha,
                theme = theme,
                theme_args = theme_args,
                palette = palette[[nm]],
                palcolor = palcolor[[nm]],
                palreverse = palreverse,
                title = title,
                subtitle = subtitle,
                xlab = xlab,
                ylab = ylab,
                x_text_angle = x_text_angle,
                x_min = x_min,
                x_max = x_max,
                keep_na = keep_na,
                keep_empty = keep_empty,
                reverse = reverse,
                facet_by = facet_by,
                facet_scales = facet_scales,
                facet_ncol = facet_ncol,
                facet_nrow = facet_nrow,
                aspect.ratio = aspect.ratio,
                legend.position = legend.position[[nm]],
                legend.direction = legend.direction[[nm]],
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

#' Density Plot / Histogram
#'
#' @description
#' Density plot for visualising the distribution of a numeric variable. Uses
#' `ggplot2::geom_density()` to render smooth kernel density estimates, with
#' optional grouping, faceting, split-by splitting, and data-distribution rug
#' bars along the baseline.
#'
#' This is the public entry point for density plots; the companion
#' `Histogram()` function provides binned-histogram rendering.
#' Both dispatch to the same internal engine (`DensityHistoPlotAtomic`)
#' with `type = "density"` or `type = "histogram"` respectively.
#'
#' @section split_by Workflow:
#'
#' When `split_by` is specified, `DensityPlot()` executes the following pipeline:
#'
#' 1. **Argument validation** — `validate_common_args()` checks the seed and
#'    facet-by consistency.
#' 2. **NA / empty normalisation** — `check_keep_na()` / `check_keep_empty()`
#'    convert `keep_na` / `keep_empty` to per-column lists.
#' 3. **Theme resolution** — `process_theme()` resolves the theme string to a
#'    theme function.
#' 4. **Split column resolution** — `check_columns()` validates `split_by`
#'    (force_factor, concat_multi).
#' 5. **Pre-filtering** — `process_keep_na_empty()` removes NA / empty levels
#'    from the split column, then `data` is split by `split_by` levels (order
#'    preserved).
#' 6. **Per-split parameter resolution** — `check_palette()`,
#'    `check_palcolor()`, `check_legend()` resolve palette, palcolor,
#'    legend.position, and legend.direction for each split.
#' 7. **Per-split dispatch** — each split is passed to
#'    `DensityHistoPlotAtomic(type = "density", ...)` with its resolved
#'    parameters. Title defaults to the split level name unless `title` is
#'    a function.
#' 8. **Combination** — `combine_plots()` assembles the list of plots via
#'    `patchwork::wrap_plots()`, applying `nrow`, `ncol`, `byrow`, `axes`,
#'    `axis_titles`, `guides`, and `design`.
#'
#' @rdname densityhistoplot
#' @inheritParams common_args
#' @inheritParams DensityHistoPlotAtomic
#' @return A `ggplot` object (single plot), a `patchwork` / `wrap_plots` object
#'   (when `split_by` is provided and `combine = TRUE`), or a list of `ggplot`
#'   objects (when `split_by` is provided and `combine = FALSE`).
#' @export
#' @examples
#' \donttest{
#' set.seed(8525)
#' data <- data.frame(
#'     x = c(rnorm(500, -1), rnorm(500, 1)),
#'     group = factor(rep(c("A", NA, "C", "D"), each = 250), levels = LETTERS[1:4]),
#'     facet = sample(c("F1", "F2"), 1000, replace = TRUE)
#' )
#'
#' # basic density
#' DensityPlot(data, x = "x")
#' DensityPlot(data, x = "x", group_by = "group")
#'
#' # NA / empty level handling
#' DensityPlot(data, x = "x", group_by = "group",
#'     keep_na = TRUE, keep_empty = TRUE)
#' DensityPlot(data, x = "x", group_by = "group",
#'     keep_na = TRUE, keep_empty = 'level')
#'
#' # faceting and splitting
#' DensityPlot(data, x = "x", group_by = "group", facet_by = "facet")
#' DensityPlot(data, x = "x", split_by = "facet", add_bars = TRUE)
#' DensityPlot(data, x = "x", split_by = "facet", add_bars = TRUE,
#'     palette = c(F1 = "Set1", F2 = "Set2"))
#' }
DensityPlot <- function(
    data,
    x,
    group_by = NULL,
    group_by_sep = "_",
    group_name = NULL,
    xtrans = "identity",
    ytrans = "identity",
    split_by = NULL,
    split_by_sep = "_",
    flip = FALSE,
    position = "identity",
    palette = "Paired",
    palcolor = NULL,
    palreverse = FALSE,
    alpha = .5,
    theme = "theme_this",
    theme_args = list(),
    add_bars = FALSE,
    bar_height = 0.025,
    bar_alpha = 1,
    bar_width = .1,
    keep_na = FALSE,
    keep_empty = FALSE,
    title = NULL,
    subtitle = NULL,
    xlab = NULL,
    ylab = NULL,
    expand = c(bottom = 0, left = 0, right = 0),
    facet_by = NULL,
    facet_scales = "free_y",
    facet_ncol = NULL,
    facet_nrow = NULL,
    facet_byrow = TRUE,
    aspect.ratio = 1,
    legend.position = ifelse(is.null(group_by), "none", "right"),
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
    keep_na <- check_keep_na(keep_na, c(group_by, split_by, facet_by))
    keep_empty <- check_keep_empty(keep_empty, c(group_by, split_by, facet_by))
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
            DensityHistoPlotAtomic(
                datas[[nm]],
                x = x,
                group_by = group_by,
                group_by_sep = group_by_sep,
                group_name = group_name,
                type = "density",
                flip = flip,
                xtrans = xtrans,
                ytrans = ytrans,
                position = position,
                aspect.ratio = aspect.ratio,
                add_bars = add_bars,
                bar_height = bar_height,
                bar_alpha = bar_alpha,
                bar_width = bar_width,
                palette = palette[[nm]],
                palcolor = palcolor[[nm]],
                palreverse = palreverse,
                alpha = alpha,
                theme = theme,
                theme_args = theme_args,
                title = title,
                subtitle = subtitle,
                xlab = xlab,
                ylab = ylab,
                expand = expand,
                keep_na = keep_na,
                keep_empty = keep_empty,
                facet_by = facet_by,
                facet_scales = facet_scales,
                facet_ncol = facet_ncol,
                facet_nrow = facet_nrow,
                facet_byrow = facet_byrow,
                legend.position = legend.position[[nm]],
                legend.direction = legend.direction[[nm]],
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

#' @description
#' Histogram for visualising the distribution of a numeric variable via binned
#' counts. Uses `ggplot2::geom_histogram()`, with optional trend-line overlays,
#' zero-skip interpolation, grouping, faceting, and split-by splitting.
#'
#' This is the histogram companion to `DensityPlot()`. Both dispatch to the
#' same internal engine (`DensityHistoPlotAtomic`) with `type = "histogram"`
#' or `type = "density"` respectively.
#'
#' When `use_trend = TRUE`, the histogram bars are replaced entirely by a
#' point-and-line trend; when `add_trend = TRUE`, the trend is overlaid on top
#' of the bars. The `trend_skip_zero` option uses `zoo::na.approx()` to
#' interpolate across empty bins for a continuous trend curve — particularly
#' useful with transformed y-axes.
#'
#' @section split_by Workflow:
#'
#' When `split_by` is specified, `Histogram()` executes the following pipeline:
#'
#' 1. **Argument validation** — `validate_common_args()` checks the seed and
#'    facet-by consistency.
#' 2. **NA / empty normalisation** — `check_keep_na()` / `check_keep_empty()`
#'    convert `keep_na` / `keep_empty` to per-column lists.
#' 3. **Theme resolution** — `process_theme()` resolves the theme string to a
#'    theme function.
#' 4. **Split column resolution** — `check_columns()` validates `split_by`
#'    (force_factor, concat_multi).
#' 5. **Pre-filtering** — `process_keep_na_empty()` removes NA / empty levels
#'    from the split column, then `data` is split by `split_by` levels (order
#'    preserved).
#' 6. **Per-split parameter resolution** — `check_palette()`,
#'    `check_palcolor()`, `check_legend()` resolve palette, palcolor,
#'    legend.position, and legend.direction for each split.
#' 7. **Per-split dispatch** — each split is passed to
#'    `DensityHistoPlotAtomic(type = "histogram", ...)` with its resolved
#'    parameters (including `bins`, `binwidth`, `use_trend`, `add_trend`,
#'    `trend_skip_zero`, `trend_alpha`, `trend_linewidth`, `trend_pt_size`).
#'    Title defaults to the split level name unless `title` is a function.
#' 8. **Combination** — `combine_plots()` assembles the list of plots via
#'    `patchwork::wrap_plots()`, applying `nrow`, `ncol`, `byrow`, `axes`,
#'    `axis_titles`, `guides`, and `design`.
#'
#' @rdname densityhistoplot
#' @inheritParams common_args
#' @inheritParams DensityHistoPlotAtomic
#' @return A `ggplot` object (single plot), a `patchwork` / `wrap_plots` object
#'   (when `split_by` is provided and `combine = TRUE`), or a list of `ggplot`
#'   objects (when `split_by` is provided and `combine = FALSE`).
#' @export
#' @examples
#' set.seed(8525)
#' data <- data.frame(
#'     x = sample(setdiff(1:100, c(30:36, 50:55, 70:77)), 1000, replace = TRUE),
#'     group = factor(rep(c("A", "B", NA, "D"), each = 250), levels = LETTERS[1:4]),
#'     facet = sample(c("F1", "F2"), 1000, replace = TRUE)
#' )
#'
#' # basic histogram
#' Histogram(data, x = "x")
#' Histogram(data, x = "x", group_by = "group")
#'
#' # NA / empty level handling
#' Histogram(data, x = "x", group_by = "group", keep_na = TRUE, keep_empty = 'level')
#'
#' # add_bars and trend overlays
#' Histogram(data, x = "x", split_by = "facet", add_bars = TRUE)
#' Histogram(data, x = "x", group_by = "group", add_trend = TRUE)
#' Histogram(data, x = "x", group_by = "group", add_trend = TRUE, trend_skip_zero = TRUE)
#'
#' # use_trend replaces bars entirely
#' Histogram(data, x = "x", group_by = "group", split_by = "facet",
#'  use_trend = TRUE, trend_pt_size = 3)
#'
#' # per-split palettes
#' Histogram(data, x = "x", group_by = "group", split_by = "facet",
#'  palette = c(F1 = "Paired", F2 = "Spectral"))
Histogram <- function(
    data,
    x,
    group_by = NULL,
    group_by_sep = "_",
    group_name = NULL,
    xtrans = "identity",
    ytrans = "identity",
    split_by = NULL,
    split_by_sep = "_",
    flip = FALSE,
    bins = NULL,
    binwidth = NULL,
    trend_skip_zero = FALSE,
    add_bars = FALSE,
    bar_height = 0.025,
    bar_alpha = 1,
    bar_width = .1,
    position = "identity",
    keep_na = FALSE,
    keep_empty = FALSE,
    use_trend = FALSE,
    add_trend = FALSE,
    trend_alpha = 1,
    trend_linewidth = 0.8,
    trend_pt_size = 1.5,
    palette = "Paired",
    palcolor = NULL,
    palreverse = FALSE,
    alpha = .5,
    theme = "theme_this",
    theme_args = list(),
    title = NULL,
    subtitle = NULL,
    xlab = NULL,
    ylab = NULL,
    expand = c(bottom = 0, left = 0, right = 0),
    facet_by = NULL,
    facet_scales = "free_y",
    facet_ncol = NULL,
    facet_nrow = NULL,
    facet_byrow = TRUE,
    aspect.ratio = 1,
    legend.position = ifelse(is.null(group_by), "none", "right"),
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
    keep_na <- check_keep_na(keep_na, c(group_by, split_by, facet_by))
    keep_empty <- check_keep_empty(keep_empty, c(group_by, split_by, facet_by))
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
            DensityHistoPlotAtomic(
                datas[[nm]],
                x = x,
                group_by = group_by,
                group_by_sep = group_by_sep,
                group_name = group_name,
                type = "histogram",
                flip = flip,
                xtrans = xtrans,
                ytrans = ytrans,
                use_trend = use_trend,
                trend_skip_zero = trend_skip_zero,
                add_trend = add_trend,
                trend_alpha = trend_alpha,
                trend_linewidth = trend_linewidth,
                trend_pt_size = trend_pt_size,
                add_bars = add_bars,
                bar_height = bar_height,
                bar_alpha = bar_alpha,
                bar_width = bar_width,
                bins = bins,
                binwidth = binwidth,
                expand = expand,
                position = position,
                keep_na = keep_na,
                keep_empty = keep_empty,
                palette = palette[[nm]],
                palcolor = palcolor[[nm]],
                palreverse = palreverse,
                alpha = alpha,
                theme = theme,
                theme_args = theme_args,
                title = title,
                subtitle = subtitle,
                xlab = xlab,
                ylab = ylab,
                aspect.ratio = aspect.ratio,
                facet_by = facet_by,
                facet_scales = facet_scales,
                facet_ncol = facet_ncol,
                facet_nrow = facet_nrow,
                facet_byrow = facet_byrow,
                legend.position = legend.position[[nm]],
                legend.direction = legend.direction[[nm]],
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
