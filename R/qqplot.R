#' Atomic QQ plot
#'
#' @inheritParams common_args
#' @param val A character string of the column name for the values to plot.
#' A numeric column is expected.
#' @param val_trans A function to transform the values before plotting.
#' Default is NULL, which means no transformation.
#' @param type A character string to specify the type of plot.
#' Default is "qq", which means QQ plot.
#' Other options are "pp", which means PP plot.
#' @param band A list of arguments to pass to [qqplotr::stat_qq_band()] or
#' [qqplotr::stat_pp_band()], depending on the value of `type`.
#' Default is NULL, which means no band.
#' If an empty list or TRUE is provided, the default arguments will be used.
#' Multiple bands can be added by providing a list of lists.
#' @param line A list of arguments to pass to [qqplotr::stat_qq_line()] or
#' [qqplotr::stat_pp_line()], depending on the value of `type`.
#' Default is `list()`, which means to add a line with default arguments.
#' If `NULL` is provided, no line will be added.
#' @param point A list of arguments to pass to [qqplotr::stat_qq_point()] or
#' [qqplotr::stat_pp_point()], depending on the value of `type`.
#' Default is `list()`, which means to add points with default arguments.
#' If `NULL` is provided, no points will be added (not recommended).
#' @param fill_name A character string to name the legend of fill.
#' Default is "Band Type".
#' @param band_alpha A numeric value to set the alpha of all bands.
#' Default is 0.5.
#' It is a shortcut for setting alpha of all bands. You can override it by
#' setting `alpha` in `band` argument.
#' For example, `band = list(list(alpha = 0.3), list(alpha = 0.7))`.
#' @param seed A numeric value to set the seed for random number generation.
#' Default is 8525.
#' @param xlim A numeric vector of length 2 to set the x-axis limits.
#' @param ylim A numeric vector of length 2 to set the y-axis limits.
#' @keywords internal
#' @return A ggplot object
#' @importFrom rlang eval_tidy as_name
#' @importFrom utils modifyList
#' @importFrom ggplot2 aes waiver scale_fill_manual labs
QQPlotAtomic <- function(
    data, val, val_trans = NULL, type = c("qq", "pp"),
    band = NULL, line = list(), point = list(), fill_name = "Bands", band_alpha = 0.5,
    theme = "theme_this", theme_args = list(), palette = "Spectral", palcolor = NULL,
    facet_by = NULL, facet_scales = "fixed", facet_ncol = NULL, facet_nrow = NULL, facet_byrow = TRUE,
    aspect.ratio = 1, legend.position = waiver(), legend.direction = "vertical",
    title = NULL, subtitle = NULL, seed = 8525, xlim = NULL, ylim = NULL,
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
    stopifnot("[QQPlot] 'band' must be TRUE, a list or NULL" = isTRUE(band) || is.list(band) || is.null(band))
    stopifnot("[QQPlot] 'line' must be a list or NULL" = is.list(line) || is.null(line))
    stopifnot("[QQPlot] 'point' must be a list or NULL" = is.list(point) || is.null(point))
    stopifnot("[QQPlot] 'xlim' must be a numeric vector of length 2 or NULL" = is.null(xlim) || (is.numeric(xlim) && length(xlim) == 2))
    stopifnot("[QQPlot] 'ylim' must be a numeric vector of length 2 or NULL" = is.null(ylim) || (is.numeric(ylim) && length(ylim) == 2))

    val <- check_columns(data, val)

    if (!is.null(val_trans)) {
        data[[val]] <- val_trans(data[[val]])
    }

    p <- ggplot(data, aes(sample = !!sym(val)))

    bands = c()
    if (!is.null(band)) {
        band_fn <- if (type == "qq") {
            qqplotr::stat_qq_band
        } else {
            qqplotr::stat_pp_band
        }
        if (isTRUE(band)) {
            band = list()
        }
        if (length(band) == 0 || !is.null(names(band))) {
            # single band
            band = list(band)
        }
        for (i in seq_along(band)) {
            bnd <- band[[i]]
            if (i == 1) {
                # Using paste, aes does not evaluate eagerly, is there a better way?
                default_bnd_fill = aes(fill = "Band_1")
            } else if (i == 2) {
                default_bnd_fill = aes(fill = "Band_2")
            } else if (i == 3) {
                default_bnd_fill = aes(fill = "Band_3")
            } else if (i == 4) {
                default_bnd_fill = aes(fill = "Band_4")
            } else if (i == 5) {
                default_bnd_fill = aes(fill = "Band_5")
            } else if (i == 6) {
                default_bnd_fill = aes(fill = "Band_6")
            } else if (i == 7) {
                default_bnd_fill = aes(fill = "Band_7")
            } else if (i == 8) {
                default_bnd_fill = aes(fill = "Band_8")
            } else if (i == 9) {
                default_bnd_fill = aes(fill = "Band_9")
            } else if (i == 10) {
                default_bnd_fill = aes(fill = "Band_10")
            } else {
                stop("[QQPlot] Too many bands! Please specify the fill aesthetic manually.")
            }
            bnd$mapping <- bnd$mapping %||% default_bnd_fill
            if (is.null(bnd$mapping$fill)) {
                bnd$mapping <- modifyList(bnd$mapping, default_bnd_fill)
            }
            bnd$alpha <- bnd$alpha %||% band_alpha
            bands <- c(bands, as_name(eval_tidy(bnd$mapping)$fill))

            p <- p + do.call(band_fn, bnd)
        }
    }

    if (length(bands) == 0 || all(startsWith(bands, "Band_"))) {
        legend.position <- ifelse(inherits(legend.position, "waiver"), "none", legend.position)
    } else {
        legend.position <- ifelse(inherits(legend.position, "waiver"), "right", legend.position)
    }

    if (!is.null(line)) {
        line_fn <- if (type == "qq") {
            qqplotr::stat_qq_line
        } else {
            qqplotr::stat_pp_line
        }
        p <- p + do.call(line_fn, line)
    }

    if (!is.null(point)) {
        point_fn <- if (type == "qq") {
            qqplotr::stat_qq_point
        } else {
            qqplotr::stat_pp_point
        }
        p <- p + do.call(point_fn, point)
    }

    if (length(bands) > 0) {
        p <- p +
            scale_fill_manual(
                name = fill_name,
                values = palette_this(bands, palette = palette, palcolor = palcolor)
            )
    }
    if (!is.null(xlim)) {
        p <- p + ggplot2::xlim(xlim[1], xlim[2])
    }
    if (!is.null(ylim)) {
        p <- p + ggplot2::ylim(ylim[1], ylim[2])
    }
    p <- p +
        labs(title = title, subtitle = subtitle, x = xlab %||% val, y = ylab %||% val) +
        do.call(theme, theme_args) +
        ggplot2::theme(
            aspect.ratio = aspect.ratio,
            panel.grid.major = element_line(colour = "grey80", linetype = 2),
            legend.position = legend.position,
            legend.direction = legend.direction
        )

    height <- width <- 4.5
    if (!identical(legend.position, "none")) {
        if (legend.position %in% c("right", "left")) {
            width <- width + 1
        } else if (legend.direction == "horizontal") {
            height <- height + 1
        } else {
            width <- width + 2
        }
    }

    attr(p, "height") <- height
    attr(p, "width") <- width

    facet_plot(p, facet_by, facet_scales, facet_nrow, facet_ncol, facet_byrow,
        legend.position = legend.position, legend.direction = legend.direction)
}


#' QQ plot
#'
#' @description QQ plot is a graphical tool to compare two distributions by plotting their quantiles against each other.
#' @inheritParams common_args
#' @inheritParams QQPlotAtomic
#' @return A ggplot object or wrap_plots object or a list of ggplot objects
#' @importFrom ggplot2 waiver
#' @export
#' @examples
#' set.seed(8525)
#' data <- data.frame(norm = rnorm(100))
#'
#' QQPlot(data, val = "norm", band = TRUE)
#' QQPlot(data, val = "norm", band = list(
#'     list(bandType = "ks", mapping = ggplot2::aes(fill = "KS"), alpha = 0.3),
#'     list(bandType = "ts", mapping = ggplot2::aes(fill = "TS")),
#'     list(bandType = "pointwise", mapping = ggplot2::aes(fill = "Normal")),
#'     list(bandType = "boot", mapping = ggplot2::aes(fill = "Bootstrap"))
#' ), band_alpha = 0.6)
#'
#' data(airquality, package = "datasets")
#' di <- "exp" # exponential distribution
#' dp <- list(rate = 2) # exponential rate parameter
#' QQPlot(airquality, val = "Ozone",
#'     band = list(distribution = di, dparams = dp),
#'     line = list(distribution = di, dparams = dp),
#'     point = list(distribution = di, dparams = dp)
#' )
#'
#' de <- TRUE # enabling the detrend option
#' QQPlot(airquality, val = "Ozone",
#'     band = list(distribution = di, dparams = dp, detrend = de),
#'     line = list(distribution = di, dparams = dp, detrend = de),
#'     point = list(distribution = di, dparams = dp, detrend = de)
#' )
#'
#' QQPlot(data, val = "norm", type = "pp", band = TRUE)
#'
#' dp <- list(mean = 2, sd = 2) # shifted and rescaled Normal parameters
#' QQPlot(data, val = "norm", type = "pp",
#'     band = list(dparams = dp),
#'     point = list(dparams = dp))
#'
#' QQPlot(data, val = "norm", type = "pp", band = TRUE,
#'     line = list(ab = c(.2, .5)))
#'
#' di <- "exp"
#' dp <- list(rate = .022) # value is based on some empirical tests
#' de <- TRUE
#' QQPlot(airquality, val = "Ozone", type = "pp",
#'    band = list(distribution = di, detrend = de, dparams = dp),
#'    line = list(detrend = de),
#'    point = list(distribution = di, detrend = de, dparams = dp),
#'    ylim = c(-.5, .5)
#' )
QQPlot <- function(
    data, val, val_trans = NULL, type = c("qq", "pp"), split_by = NULL, split_by_sep = "_",
    band = NULL, line = list(), point = list(), fill_name = "Bands", band_alpha = 0.5,
    theme = "theme_this", theme_args = list(), palette = "Spectral", palcolor = NULL,
    facet_by = NULL, facet_scales = "fixed", facet_ncol = NULL, facet_nrow = NULL, facet_byrow = TRUE,
    aspect.ratio = 1, legend.position = waiver(), legend.direction = "vertical",
    title = NULL, subtitle = NULL, xlim = NULL, ylim = NULL,
    xlab = ifelse(type == "qq", "Theoretical Quantiles", "Probability Points"),
    ylab = ifelse(type == "qq", "Sample Quantiles", "Cumulative Probability"),
    combine = TRUE, nrow = NULL, ncol = NULL, byrow = TRUE, seed = 8525,
    ...
) {
    validate_common_args(seed, facet_by = facet_by)
    theme <- process_theme(theme)
    split_by <- check_columns(data, split_by, force_factor = TRUE, allow_multi = TRUE, concat_multi = TRUE, concat_sep = split_by_sep)

    if (!is.null(split_by)) {
        datas <- split(data, data[[split_by]])
        # keep the order of levels
        datas <- datas[levels(data[[split_by]])]
    } else {
        datas <- list(data)
        names(datas) <- "..."
    }
    palette <- check_palette(palette, names(datas))
    palcolor <- check_palcolor(palcolor, names(datas))

    plots <- lapply(
        names(datas), function(nm) {
            default_title <- if (length(datas) == 1 && identical(nm, "...")) NULL else nm
            if (is.function(title)) {
                title <- title(default_title)
            } else {
                title <- title %||% default_title
            }
            QQPlotAtomic(
                datas[[nm]], val = val, val_trans = val_trans, type = type,
                band = band, line = line, point = point, fill_name = fill_name, band_alpha = band_alpha,
                theme = theme, theme_args = theme_args, palette = palette[[nm]], palcolor = palcolor[[nm]],
                facet_by = facet_by, facet_scales = facet_scales, facet_ncol = facet_ncol, facet_nrow = facet_nrow, facet_byrow = facet_byrow,
                aspect.ratio = aspect.ratio, legend.position = legend.position, legend.direction = legend.direction,
                title = title, subtitle = subtitle, xlab = xlab, ylab = ylab, seed = seed, xlim = xlim, ylim = ylim,
                ...
            )
        }
    )

    combine_plots(plots, combine = combine, nrow = nrow, ncol = ncol, byrow = byrow)
}
