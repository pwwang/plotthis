#' Atomic QQ/PP plot
#'
#' @description
#' Core implementation for drawing a single quantile-quantile (QQ) or
#' probability-probability (PP) plot. This is the internal workhorse
#' dispatched by the exported \code{\link{QQPlot}} function -- it takes a
#' **single** data frame (no \code{split_by} support) and returns a
#' \code{ggplot} object. The function compares the empirical distribution
#' of a numeric variable against a theoretical distribution (default:
#' standard normal) via the \pkg{qqplotr} package.
#'
#' Two plot types are supported via the \code{type} parameter:
#' \itemize{
#'   \item \strong{QQ plot} (\code{type = "qq"}, the default) -- plots
#'   sample quantiles against theoretical quantiles. Deviations from the
#'   reference line indicate departures from the assumed distribution
#'   (skewness, heavy tails, outliers).
#'   \item \strong{PP plot} (\code{type = "pp"}) -- plots empirical
#'   cumulative probability against theoretical cumulative probability.
#'   PP plots are more sensitive to deviations in the centre of the
#'   distribution, while QQ plots are more sensitive at the tails.
#' }
#'
#' The function can overlay \strong{confidence bands} (\code{band}) around
#' the reference line using several methods (pointwise confidence intervals,
#' Kolmogorov-Smirnov, Tukey's simultaneous intervals, or bootstrap).
#' Multiple bands can be combined and each receives a separate fill colour
#' from the \code{palette}.
#'
#' @section Architecture:
#'
#' \strong{QQPlotAtomic} executes the following steps:
#' \enumerate{
#'   \item \strong{ggplot dispatch} -- selects \code{gglogger::ggplot} or
#'   \code{ggplot2::ggplot} based on
#'   \code{getOption("plotthis.gglogger.enabled")}.
#'   \item \strong{Type validation} -- \code{match.arg()} resolves
#'   \code{type} to \code{"qq"} or \code{"pp"}.
#'   \item \strong{Input validation} -- \code{stopifnot()} checks that
#'   \code{band} is \code{TRUE}, a list, or \code{NULL}; \code{line} and
#'   \code{point} are lists or \code{NULL}; and \code{xlim}/\code{ylim}
#'   are numeric vectors of length 2 or \code{NULL}.
#'   \item \strong{Column resolution} -- \code{\link{check_columns}()}
#'   validates the \code{val} column.
#'   \item \strong{Value transformation} -- when \code{val_trans} is
#'   provided, it is applied to the \code{val} column (e.g. log-transform).
#'   \item \strong{Base ggplot} -- initialises
#'   \code{ggplot(data, aes(sample = !!sym(val)))}. The \code{sample}
#'   aesthetic is the standard interface for \pkg{qqplotr}.
#'   \item \strong{Band rendering} -- when \code{band} is not \code{NULL}:
#'   \enumerate{
#'     \item Selects the band stat function:
#'     \code{qqplotr::stat_qq_band} for QQ plots or
#'     \code{qqplotr::stat_pp_band} for PP plots.
#'     \item Converts \code{band = TRUE} to an empty list (default
#'     arguments).
#'     \item Normalises a single band (non-list or named list) into a
#'     list-of-lists format.
#'     \item Iterates over bands, assigning each a default fill aesthetic
#'     (\code{"Band_1"}, \code{"Band_2"}, ..., up to a maximum of 10).
#'     The user can override the fill mapping via \code{mapping} inside
#'     each band's argument list.
#'     \item Sets \code{alpha} per band, falling back to the
#'     \code{band_alpha} parameter.
#'     \item Adds each band to the plot via \code{do_call(band_fn, bnd)}.
#'   }
#'   \item \strong{Legend position resolution} -- if no bands were rendered
#'   or all bands use default \code{"Band_"} names, the legend defaults to
#'   \code{"none"} (when \code{legend.position} is a \code{waiver});
#'   otherwise defaults to \code{"right"}.
#'   \item \strong{Reference line} -- when \code{line} is not \code{NULL},
#'   adds \code{qqplotr::stat_qq_line} (QQ) or
#'   \code{qqplotr::stat_pp_line} (PP) via \code{do_call()}.
#'   \item \strong{Points} -- when \code{point} is not \code{NULL}, adds
#'   \code{qqplotr::stat_qq_point} (QQ) or
#'   \code{qqplotr::stat_pp_point} (PP) via \code{do_call()}.
#'   \item \strong{Fill colour scale} -- when bands are present,
#'   \code{scale_fill_manual()} is added with colours resolved via
#'   \code{\link{palette_this}()} using the band names, \code{palette},
#'   \code{palcolor}, and \code{palreverse}. The legend title is set to
#'   \code{fill_name}.
#'   \item \strong{Axis limits} -- \code{ggplot2::xlim()} and
#'   \code{ggplot2::ylim()} are applied if \code{xlim} or \code{ylim} are
#'   set.
#'   \item \strong{Labels and theme} -- \code{labs(title, subtitle, x, y)}
#'   with fallback to \code{val} for axis labels; \code{do_call(theme,
#'   theme_args)}; \code{ggplot2::theme()} with \code{aspect.ratio},
#'   \code{panel.grid.major} (grey80, dashed), \code{legend.position},
#'   and \code{legend.direction}.
#'   \item \strong{Dimension calculation} --
#'   \code{\link{calculate_plot_dimensions}()} with \code{base_height =
#'   4.5}, \code{aspect.ratio}, and legend metrics (number of bands, band
#'   name character width).
#'   \item \strong{Faceting} -- \code{\link{facet_plot}()} applies
#'   \code{facet_wrap} / \code{facet_grid} if \code{facet_by} is provided.
#' }
#'
#' @inheritParams common_args
#' @param val A character string naming the numeric column whose distribution
#'   is compared against the theoretical distribution.
#' @param val_trans A transformation function applied to the \code{val}
#'   column before plotting. For example, \code{log} or \code{sqrt}.
#'   Default: \code{NULL} (no transformation).
#' @param type A character string specifying the plot type. Either
#'   \code{"qq"} (quantile-quantile, the default) or \code{"pp"}
#'   (probability-probability). Partial matching is supported.
#' @param band A list of arguments passed to \code{\link[qqplotr]{stat_qq_band}}
#'   or \code{\link[qqplotr]{stat_pp_band}}, depending on \code{type}.
#'   Set to \code{TRUE} or an empty list to use default arguments.
#'   Set to \code{NULL} (the default) to suppress bands entirely.
#'   To add multiple bands, provide a list of lists, each containing
#'   arguments for one band (e.g. different \code{bandType} or
#'   \code{distribution}). Each band can also include a custom
#'   \code{mapping} aesthetic to control its fill colour legend entry.
#' @param line A list of arguments passed to \code{\link[qqplotr]{stat_qq_line}}
#'   or \code{\link[qqplotr]{stat_pp_line}}, depending on \code{type}.
#'   Default: \code{list()} (adds a reference line with default arguments).
#'   Set to \code{NULL} to omit the line entirely.
#' @param point A list of arguments passed to \code{\link[qqplotr]{stat_qq_point}}
#'   or \code{\link[qqplotr]{stat_pp_point}}, depending on \code{type}.
#'   Default: \code{list()} (adds points with default arguments).
#'   Set to \code{NULL} to omit points (not recommended).
#' @param fill_name A character string for the fill legend title used
#'   when bands are present. Default: \code{"Bands"}.
#' @param band_alpha A numeric value in \code{[0, 1]} setting the
#'   transparency of all bands. Individual bands can override this via
#'   \code{alpha} inside the \code{band} argument list.
#'   Default: \code{0.5}.
#' @param seed A numeric value for the random seed used internally.
#'   Default: \code{8525}. Passed to \code{\link{set.seed}}.
#' @param xlim A numeric vector of length 2 specifying the x-axis limits.
#'   Default: \code{NULL} (use data range).
#' @param ylim A numeric vector of length 2 specifying the y-axis limits.
#'   Default: \code{NULL} (use data range).
#' @keywords internal
#' @return A \code{ggplot} object with \code{height} and \code{width}
#'   attributes (in inches).
#' @importFrom rlang eval_tidy as_name
#' @importFrom utils modifyList
#' @importFrom ggplot2 aes waiver scale_fill_manual labs
QQPlotAtomic <- function(
    data,
    val,
    val_trans = NULL,
    type = c("qq", "pp"),
    band = NULL,
    line = list(),
    point = list(),
    fill_name = "Bands",
    band_alpha = 0.5,
    theme = "theme_this",
    theme_args = list(),
    palette = "Spectral",
    palcolor = NULL,
    palreverse = FALSE,
    facet_by = NULL,
    facet_scales = "fixed",
    facet_ncol = NULL,
    facet_nrow = NULL,
    facet_byrow = TRUE,
    aspect.ratio = 1,
    legend.position = waiver(),
    legend.direction = "vertical",
    title = NULL,
    subtitle = NULL,
    seed = 8525,
    xlim = NULL,
    ylim = NULL,
    xlab = ifelse(type == "qq", "Theoretical Quantiles", "Probability Points"),
    ylab = ifelse(type == "qq", "Sample Quantiles", "Cumulative Probability"),
    ...
) {
    set.seed(seed)
    ggplot <- if (getOption("plotthis.gglogger.enabled", FALSE)) {
        gglogger::ggplot
    } else {
        ggplot2::ggplot
    }
    type <- match.arg(type)
    stopifnot(
        "[QQPlot] 'band' must be TRUE, a list or NULL" = isTRUE(band) ||
            is.list(band) ||
            is.null(band)
    )
    stopifnot(
        "[QQPlot] 'line' must be a list or NULL" = is.list(line) ||
            is.null(line)
    )
    stopifnot(
        "[QQPlot] 'point' must be a list or NULL" = is.list(point) ||
            is.null(point)
    )
    stopifnot(
        "[QQPlot] 'xlim' must be a numeric vector of length 2 or NULL" = is.null(
            xlim
        ) ||
            (is.numeric(xlim) && length(xlim) == 2)
    )
    stopifnot(
        "[QQPlot] 'ylim' must be a numeric vector of length 2 or NULL" = is.null(
            ylim
        ) ||
            (is.numeric(ylim) && length(ylim) == 2)
    )

    val <- check_columns(data, val)

    if (!is.null(val_trans)) {
        data[[val]] <- val_trans(data[[val]])
    }

    p <- ggplot(data, aes(sample = !!sym(val)))

    bands <- c()
    if (!is.null(band)) {
        band_fn <- if (type == "qq") {
            qqplotr::stat_qq_band
        } else {
            qqplotr::stat_pp_band
        }
        if (isTRUE(band)) {
            band <- list()
        }
        if (length(band) == 0 || !is.null(names(band))) {
            # single band
            band <- list(band)
        }
        for (i in seq_along(band)) {
            bnd <- band[[i]]
            if (i == 1) {
                # Using paste, aes does not evaluate eagerly, is there a better way?
                default_bnd_fill <- aes(fill = "Band_1")
            } else if (i == 2) {
                default_bnd_fill <- aes(fill = "Band_2")
            } else if (i == 3) {
                default_bnd_fill <- aes(fill = "Band_3")
            } else if (i == 4) {
                default_bnd_fill <- aes(fill = "Band_4")
            } else if (i == 5) {
                default_bnd_fill <- aes(fill = "Band_5")
            } else if (i == 6) {
                default_bnd_fill <- aes(fill = "Band_6")
            } else if (i == 7) {
                default_bnd_fill <- aes(fill = "Band_7")
            } else if (i == 8) {
                default_bnd_fill <- aes(fill = "Band_8")
            } else if (i == 9) {
                default_bnd_fill <- aes(fill = "Band_9")
            } else if (i == 10) {
                default_bnd_fill <- aes(fill = "Band_10")
            } else {
                stop(
                    "[QQPlot] Too many bands! Please specify the fill aesthetic manually."
                )
            }
            bnd$mapping <- bnd$mapping %||% default_bnd_fill
            if (is.null(bnd$mapping$fill)) {
                bnd$mapping <- modifyList(bnd$mapping, default_bnd_fill)
            }
            bnd$alpha <- bnd$alpha %||% band_alpha
            bands <- c(bands, as_name(eval_tidy(bnd$mapping)$fill))

            p <- p + do_call(band_fn, bnd)
        }
    }

    if (length(bands) == 0 || all(startsWith(bands, "Band_"))) {
        legend.position <- ifelse(
            inherits(legend.position, "waiver"),
            "none",
            legend.position
        )
    } else {
        legend.position <- ifelse(
            inherits(legend.position, "waiver"),
            "right",
            legend.position
        )
    }

    if (!is.null(line)) {
        line_fn <- if (type == "qq") {
            qqplotr::stat_qq_line
        } else {
            qqplotr::stat_pp_line
        }
        p <- p + do_call(line_fn, line)
    }

    if (!is.null(point)) {
        point_fn <- if (type == "qq") {
            qqplotr::stat_qq_point
        } else {
            qqplotr::stat_pp_point
        }
        p <- p + do_call(point_fn, point)
    }

    if (length(bands) > 0) {
        p <- p +
            scale_fill_manual(
                name = fill_name,
                values = palette_this(
                    bands,
                    palette = palette,
                    palcolor = palcolor,
                    reverse = palreverse
                )
            )
    }
    if (!is.null(xlim)) {
        p <- p + ggplot2::xlim(xlim[1], xlim[2])
    }
    if (!is.null(ylim)) {
        p <- p + ggplot2::ylim(ylim[1], ylim[2])
    }
    p <- p +
        labs(
            title = title,
            subtitle = subtitle,
            x = xlab %||% val,
            y = ylab %||% val
        ) +
        do_call(theme, theme_args) +
        ggplot2::theme(
            aspect.ratio = aspect.ratio,
            panel.grid.major = element_line(colour = "grey80", linetype = 2),
            legend.position = legend.position,
            legend.direction = legend.direction
        )

    dims <- calculate_plot_dimensions(
        base_height = 4.5,
        aspect.ratio = aspect.ratio,
        legend.position = legend.position,
        legend.direction = legend.direction,
        legend_n = length(bands),
        legend_nchar = if (length(bands) > 0) max(nchar(bands)) else 5
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


#' QQ/PP plot
#'
#' @description
#' Produces a quantile-quantile (QQ) plot or probability-probability (PP)
#' plot to compare the empirical distribution of a numeric variable against
#' a theoretical distribution (default: standard normal). The function
#' delegates to the \pkg{qqplotr} package for the underlying statistics
#' and rendering.
#'
#' Key features:
#' \itemize{
#'   \item \strong{QQ and PP modes} -- switch between quantile-quantile
#'   and probability-probability displays via \code{type}.
#'   \item \strong{Confidence bands} -- overlay one or more confidence bands
#'   (pointwise, KS, Tukey simultaneous, or bootstrap) with custom fill
#'   colours and alpha.
#'   \item \strong{Reference line} -- a diagonal reference line (QQ) or
#'   diagonal probability line (PP) for comparison.
#'   \item \strong{Distribution fitting} -- compare against any
#'   distribution supported by \pkg{qqplotr} (normal, exponential, uniform,
#'   etc.) by passing \code{distribution} and \code{dparams} inside the
#'   \code{band}, \code{line}, and \code{point} lists.
#'   \item \strong{Detrending} -- enable \code{detrend = TRUE} inside the
#'   argument lists to remove the reference line and visualise only
#'   deviations (flat PP plot centred at zero).
#'   \item \strong{Splitting} -- use \code{split_by} to produce separate
#'   QQ/PP plots for different groups, combined into a single layout.
#' }
#'
#' @section split_by Workflow:
#' When \code{split_by} is provided:
#' \enumerate{
#'   \item \strong{Common arg validation} --
#'   \code{\link{validate_common_args}()} checks the \code{seed} and
#'   \code{facet_by} constraints.
#'   \item \strong{Theme processing} -- \code{\link{process_theme}()}
#'   resolves the \code{theme} string or function.
#'   \item \strong{split_by column resolution} --
#'   \code{\link{check_columns}()} validates the \code{split_by} column(s)
#'   with \code{force_factor = TRUE}. Multiple columns are concatenated
#'   with \code{split_by_sep}.
#'   \item \strong{Data splitting} -- the data frame is split by
#'   \code{split_by} levels (droplevels applied, level order preserved).
#'   If \code{split_by} is \code{NULL}, the data is wrapped in a
#'   single-element list with name \code{"..."}.
#'   \item \strong{Per-split parameter resolution} --
#'   \code{\link{check_palette}()}, \code{\link{check_palcolor}()}, and
#'   \code{\link{check_legend}()} resolve per-split \code{palette},
#'   \code{palcolor}, \code{legend.position}, and
#'   \code{legend.direction}.
#'   \item \strong{Dispatch per split} -- \code{\link{QQPlotAtomic}()} is
#'   called for each split level. If \code{title} is a function, it
#'   receives the split level name and generates a dynamic title;
#'   otherwise the level name is used as the default title.
#'   \item \strong{Combination} -- results are combined via
#'   \code{\link{combine_plots}()} (when \code{combine = TRUE}) or
#'   returned as a named list (when \code{combine = FALSE}).
#' }
#'
#' @inheritParams common_args
#' @inheritParams QQPlotAtomic
#' @return A \code{ggplot} object (single plot), a \code{patchwork} object
#'   (combined split plots), or a named list of \code{ggplot} objects
#'   (when \code{combine = FALSE}), each with \code{height} and
#'   \code{width} attributes in inches.
#' @importFrom ggplot2 waiver
#' @export
#' @examples
#' set.seed(8525)
#' data <- data.frame(norm = rnorm(100))
#'
#' # Basic QQ plot with default confidence band
#' QQPlot(data, val = "norm", band = TRUE)
#'
#' # Multiple confidence bands with custom fill labels
#' QQPlot(data, val = "norm", band = list(
#'     list(bandType = "ks", mapping = ggplot2::aes(fill = "KS"), alpha = 0.3),
#'     list(bandType = "ts", mapping = ggplot2::aes(fill = "TS")),
#'     list(bandType = "pointwise", mapping = ggplot2::aes(fill = "Normal")),
#'     list(bandType = "boot", mapping = ggplot2::aes(fill = "Bootstrap"))
#' ), band_alpha = 0.6)
#'
#' # Compare against exponential distribution
#' data(airquality, package = "datasets")
#' di <- "exp"
#' dp <- list(rate = 2)
#' QQPlot(airquality, val = "Ozone",
#'     band = list(distribution = di, dparams = dp),
#'     line = list(distribution = di, dparams = dp),
#'     point = list(distribution = di, dparams = dp)
#' )
#'
#' # Detrended QQ plot: deviations from the reference line
#' de <- TRUE
#' QQPlot(airquality, val = "Ozone",
#'     band = list(distribution = di, dparams = dp, detrend = de),
#'     line = list(distribution = di, dparams = dp, detrend = de),
#'     point = list(distribution = di, dparams = dp, detrend = de)
#' )
#'
#' # PP plot (probability-probability)
#' QQPlot(data, val = "norm", type = "pp", band = TRUE)
#'
#' # PP plot with shifted/scaled normal distribution
#' dp <- list(mean = 2, sd = 2)
#' QQPlot(data, val = "norm", type = "pp",
#'     band = list(dparams = dp),
#'     point = list(dparams = dp))
#'
#' # PP plot with custom intercept/slope line
#' QQPlot(data, val = "norm", type = "pp", band = TRUE,
#'     line = list(ab = c(.2, .5)))
#'
#' # Detrended PP plot with axis limits
#' di <- "exp"
#' dp <- list(rate = .022)
#' de <- TRUE
#' QQPlot(airquality, val = "Ozone", type = "pp",
#'     band = list(distribution = di, detrend = de, dparams = dp),
#'     line = list(detrend = de),
#'     point = list(distribution = di, detrend = de, dparams = dp),
#'     ylim = c(-.5, .5)
#' )
QQPlot <- function(
    data,
    val,
    val_trans = NULL,
    type = c("qq", "pp"),
    split_by = NULL,
    split_by_sep = "_",
    band = NULL,
    line = list(),
    point = list(),
    fill_name = "Bands",
    band_alpha = 0.5,
    theme = "theme_this",
    theme_args = list(),
    palette = "Spectral",
    palcolor = NULL,
    palreverse = FALSE,
    facet_by = NULL,
    facet_scales = "fixed",
    facet_ncol = NULL,
    facet_nrow = NULL,
    facet_byrow = TRUE,
    aspect.ratio = 1,
    legend.position = waiver(),
    legend.direction = "vertical",
    title = NULL,
    subtitle = NULL,
    xlim = NULL,
    ylim = NULL,
    xlab = ifelse(type == "qq", "Theoretical Quantiles", "Probability Points"),
    ylab = ifelse(type == "qq", "Sample Quantiles", "Cumulative Probability"),
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
            QQPlotAtomic(
                datas[[nm]],
                val = val,
                val_trans = val_trans,
                type = type,
                band = band,
                line = line,
                point = point,
                fill_name = fill_name,
                band_alpha = band_alpha,
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
                aspect.ratio = aspect.ratio,
                legend.position = legend.position[[nm]],
                legend.direction = legend.direction[[nm]],
                title = title,
                subtitle = subtitle,
                xlab = xlab,
                ylab = ylab,
                seed = seed,
                xlim = xlim,
                ylim = ylim,
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
