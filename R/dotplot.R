#' Atomic dot/lollipop plot
#'
#' Core implementation for dot plots and lollipop plots. This is the internal
#' workhorse dispatched by both \code{DotPlot()} and \code{LollipopPlot()}. It
#' renders a matrix of points where dot size encodes one variable and dot fill
#' colour encodes another, with optional background stripes and a lollipop
#' (bar + dot) display mode.
#'
#' The function supports two display modes:
#' \itemize{
#'   \item \strong{Dot plot} (\code{lollipop = FALSE}, the default) — renders
#'   \code{ggplot2::geom_point()} with \code{shape = 21} (filled circle). Either
#'   or both axes can be numeric (producing a scatter plot) or factor (producing
#'   a dot matrix). Dot size scales by \code{size_by} (or the per-combination
#'   observation count when \code{size_by} is \code{NULL}) via
#'   \code{scale_size(range = c(size_min, size_max))}. Dot fill follows the
#'   \code{fill_by} column via \code{scale_fill_gradientn()}.
#'   \item \strong{Lollipop plot} (\code{lollipop = TRUE}) — expects a numeric
#'   \code{x} and a factor/character \code{y}. Renders a two-layer bar:
#'   an outer shadow segment followed by an inner coloured segment, each capped
#'   by a filled dot. The outer shadow is black (or a custom colour when
#'   \code{border_color} is a string), and the inner segment + dot fill follow
#'   \code{fill_by}.
#' }
#'
#' When \code{add_bg = TRUE}, alternating background stripes are drawn via
#' \code{\link{bg_layer}()} along the discrete axis (vertical stripes for
#' factor \code{x}, horizontal stripes for factor \code{y}). The fill colour
#' scale is prepared via \code{\link{prepare_continuous_color_scale}()} and
#' can be trimmed with \code{lower_quantile}/\code{upper_quantile} or
#' \code{lower_cutoff}/\code{upper_cutoff}.
#'
#' When \code{fill_cutoff} is set (e.g. \code{"< 18"}), values of
#' \code{fill_by} matching the condition are set to \code{NA} and rendered in
#' grey ("grey80"), with a separate legend entry documenting the cutoff.
#'
#' \strong{Border colour modes:}
#' \itemize{
#'   \item \code{border_color = TRUE} — dot borders and lollipop inner bar
#'   colour track the \code{fill_by} gradient via
#'   \code{scale_color_gradientn()}; lollipop outer shadow is black.
#'   \item \code{border_color = "black"} (default) — constant black borders on
#'   dots and a black outer shadow on lollipop bars.
#'   \item \code{border_color = FALSE} — no dot borders, no lollipop outer
#'   shadow (the inner coloured segment remains).
#' }
#'
#' @section Architecture:
#'
#' \strong{DotPlotAtomic} executes the following steps:
#' \enumerate{
#'   \item \strong{ggplot dispatch} — selects \code{gglogger::ggplot} or
#'   \code{ggplot2::ggplot} based on
#'   \code{getOption("plotthis.gglogger.enabled")}.
#'   \item \strong{bg_direction normalisation} — \code{match.arg()} resolves
#'   \code{"v"}/\code{"h"} abbreviations to \code{"vertical"}/\code{"horizontal"}.
#'   \item \strong{Axis type detection} — determines whether \code{x} (resp.
#'   \code{y}) is numeric by checking that the column is a single name and the
#'   data column is neither character nor factor.
#'   \item \strong{Column resolution} — non-numeric x and y are processed via
#'   \code{check_columns()} with \code{force_factor = TRUE, allow_multi = TRUE,
#'   concat_multi = TRUE} using \code{x_sep}/\code{y_sep}. \code{fill_by} and
#'   \code{facet_by} are also validated.
#'   \item \strong{NA / empty handling} — \code{process_keep_na_empty()} filters
#'   data and extracts \code{keep_empty} settings for x, y, and facet_by.
#'   \item \strong{Multi-facet keep_empty guard} — when \code{facet_by} has more
#'   than one column, the \code{keep_empty} values must be identical for all
#'   facet columns (consistent drop behaviour).
#'   \item \strong{fill_cutoff guard} — errors if \code{fill_cutoff} is set but
#'   \code{fill_by} is \code{NULL}.
#'   \item \strong{Size-by resolution} — when \code{size_by = NULL}:
#'     \itemize{
#'       \item Groups data by the unique combination of x, y, and facet_by
#'       columns and counts rows per combination into \code{.size}.
#'       \item If \code{fill_by} is present, \code{summarise()} also takes the
#'       first value of \code{fill_by} per group (with a warning that only the
#'       first value is used).
#'       \item Factor levels of x, y, and facet_by are preserved post-summary.
#'       \item Sets \code{size_by <- ".size"}.
#'     }
#'   \item \strong{fill_cutoff parsing} — if \code{fill_cutoff} is set:
#'     \itemize{
#'       \item Numeric shorthand (e.g. \code{18}) is converted to \code{"< 18"}.
#'       \item The string is parsed with regex \code{^(<=?|>=?)\\\\s*(-?[0-9.]+)$}
#'       to extract operator and numeric threshold.
#'       \item A \code{switch()} on the operator NAs-out matching values in
#'       \code{fill_by} (e.g. \code{"<"} sets values \emph{below} the threshold
#'       to \code{NA}).
#'       \item A label \code{"<fill_by> <fill_cutoff>"} is generated for the
#'       legend.
#'     }
#'   \item \strong{Default fill_by} — when \code{fill_by = NULL}, a synthetic
#'   \code{.fill_by} column (constant \code{1}) is created and the fill legend
#'   is suppressed.
#'   \item \strong{Continuous colour scale preparation} — when \code{fill_by}
#'   is numeric, \code{\link{prepare_continuous_color_scale}()} computes
#'   \code{feat_colors_value} (the range endpoints after optional quantile
#'   trimming or cutoff clamping).
#'   \item \strong{Base ggplot} — initialises \code{ggplot(data, aes(x, y))}.
#'   \item \strong{Background layer} — if \code{add_bg = TRUE}:
#'     \itemize{
#'       \item Vertical stripes (\code{bg_direction = "vertical"}) require a
#'       non-numeric x-axis; horizontal stripes require a non-numeric y-axis.
#'       \item Calls \code{\link{bg_layer}()} with the relevant discrete column
#'       and stripe styling.
#'     }
#'   \item \strong{Discrete axis scales} — \code{scale_x_discrete()} /
#'   \code{scale_y_discrete()} with \code{drop = !isTRUE(keep_empty_*)} for
#'   non-numeric axes.
#'   \item \strong{Lollipop branch} (\code{lollipop = TRUE}):
#'     \itemize{
#'       \item Sets x-axis expansion to \code{c(0, 0, 0.05, 0)} (bars start at
#'       x = 0 with a 5\% right pad).
#'       \item \strong{Outer shadow} — \code{geom_segment()} from x = 0 to the
#'       data value, coloured black (or \code{border_color} when a string),
#'       with linewidth \code{border_size * 4}. Skipped when
#'       \code{border_color = FALSE}.
#'       \item \strong{Inner coloured bar} — \code{geom_segment()} mapped to
#'       the \code{fill_by} gradient via \code{scale_color_gradientn()}, with
#'       linewidth \code{border_size * 2}.
#'       \item \code{ggnewscale::new_scale_color()} resets the colour scale for
#'       the subsequent point layer.
#'     }
#'   \item \strong{Point layer} — \code{geom_point(shape = 21)} (filled circle
#'   with border) with four dispatch paths:
#'     \itemize{
#'       \item \emph{Numeric size + gradient border}: \code{aes(fill, color)}
#'       both mapped to \code{fill_by}; \code{size = size_by} constant.
#'       \item \emph{Numeric size + constant/no border}: \code{aes(fill,
#'       color = "")} with a constant colour string.
#'       \item \emph{Column size + gradient border}: \code{aes(size, fill,
#'       color)} with \code{scale_size(range = c(size_min, size_max))} and a
#'       size legend (title = \code{size_name}, order = 1).
#'       \item \emph{Column size + constant/no border}: \code{aes(size, fill,
#'       color = "")} with the same size scale.
#'     }
#'   \item \strong{Fill scale} — \code{scale_fill_gradientn()} with
#'   \code{palette_this()}, \code{feat_colors_value} for rescaled colour
#'   mapping, \code{na.value = "grey80"}, and a colour-bar legend (title =
#'   \code{fill_name}, order = 2) or \code{guide_none()} when no
#'   \code{fill_by} was provided.
#'   \item \strong{Labels} — \code{labs(title, subtitle, x, y)} with fallback
#'   to column names via \code{\%||\%}.
#'   \item \strong{Theme} — \code{do_call(theme, theme_args)} plus
#'   \code{panel.grid.major} (grey80 dashed) and rotated x-axis text via
#'   \code{calc_just(x_text_angle)}.
#'   \item \strong{Colour (border) scale}:
#'     \itemize{
#'       \item \code{border_color = TRUE} — \code{scale_color_gradientn()}
#'       following the \code{fill_by} gradient (with border_alpha), guide
#'       suppressed (the fill colour-bar serves both).
#'       \item \code{border_color = FALSE} — \code{scale_color_manual()} with
#'       \code{"transparent"}, guide suppressed.
#'       \item Constant string — \code{scale_color_manual()} with the
#'       alpha-adjusted colour, guide suppressed.
#'     }
#'   \item \strong{fill_cutoff legend} — when \code{fill_cutoff} is active and
#'   there are NA values in \code{fill_by}, a \code{guide_legend()} (order = 3)
#'   is added showing the cutoff label with a grey fill and the border colour.
#'   \item \strong{Flip} — optional \code{coord_flip()} swaps x and y axes.
#'   \item \strong{Dimension calculation} — \code{calculate_plot_dimensions()}
#'   with \code{base_height = 4.5}, \code{aspect.ratio = NULL}, and per-axis
#'   scale factors (0.9 for the categorical axis driving width, 0.6 for the
#'   categorical axis driving height). Legend width, y-axis label length, and
#'   minimum dimensions (width ≥ 5, height ≥ 4) are factored in. A fallback
#'   manual calculation is used when \code{calculate_plot_dimensions()} returns
#'   \code{NULL}.
#'   \item \strong{Faceting} — \code{facet_plot()} applies
#'   \code{facet_grid}/\code{facet_wrap} with \code{drop = !isTRUE(keep_empty_facet)}.
#' }
#'
#' @inheritParams common_args
#' @param x A character vector specifying the column(s) to use for the x-axis.
#'   Can be numeric (for scatter/lollipop mode) or factor/character (for dot
#'   matrix mode). When multiple columns are provided for a non-numeric axis,
#'   they are concatenated with \code{x_sep} as the separator.
#' @param y A character vector specifying the column(s) to use for the y-axis.
#'   Can be numeric (for scatter mode) or factor/character (for dot matrix or
#'   lollipop mode). When multiple columns are provided for a non-numeric axis,
#'   they are concatenated with \code{y_sep} as the separator.
#' @param x_sep A character string used to join multiple \code{x} column values
#'   into a single factor level. Only used when x is non-numeric and multiple
#'   columns are provided. Default: \code{"_"}.
#' @param y_sep A character string used to join multiple \code{y} column values
#'   into a single factor level. Only used when y is non-numeric and multiple
#'   columns are provided. Default: \code{"_"}.
#' @param size_by A character string naming a numeric column whose values
#'   control dot size. When \code{NULL} (the default), the per-combination
#'   observation count is computed automatically (via \code{dplyr::summarise(n =
#'   n())}) and used as the size variable. If \code{fill_by} is also present,
#'   the first value of \code{fill_by} per combination is retained with a
#'   warning. A single numeric value is also accepted and sets a constant dot
#'   size (used by \code{ScatterPlot}).
#' @param size_min A numeric value for the smallest dot size in the
#'   \code{scale_size(range = c(size_min, size_max))} range.
#'   Default: \code{1}.
#' @param size_max A numeric value for the largest dot size in the
#'   \code{scale_size(range = c(size_min, size_max))} range.
#'   Default: \code{10}.
#' @param fill_by A character string naming a numeric column whose values
#'   control the fill colour of the dots (and lollipop inner bars). A
#'   continuous gradient from \code{palette} is applied via
#'   \code{scale_fill_gradientn()}. When \code{NULL} (the default), all dots
#'   are filled with a single constant colour from the middle of the palette.
#' @param fill_cutoff A string expression specifying which values of
#'   \code{fill_by} to grey out. Format: an operator followed by a number,
#'   e.g. \code{"< 18"}, \code{"<= 18"}, \code{"> 18"}, or \code{">= 18"}.
#'   Values matching the condition are set to \code{NA} and rendered in grey
#'   (\code{"grey80"}), while the rest are coloured by the fill gradient. The
#'   operator determines which side of the threshold is greyed out,
#'   independent of \code{palreverse}. A numeric value is also accepted as
#'   shorthand for \code{"<"} (e.g. \code{18} is equivalent to
#'   \code{"< 18"}). Requires \code{fill_by} to be set.
#' @param size_name A character string for the size legend title. When
#'   \code{NULL} (the default), the \code{size_by} column name is used.
#' @param fill_name A character string for the fill colour-bar legend title.
#'   When \code{NULL} (the default), the \code{fill_by} column name is used.
#' @param fill_cutoff_name A character string for the fill cutoff legend title
#'   (shown when \code{fill_cutoff} is active). Defaults to
#'   \code{"<fill_by> <fill_cutoff>"}, e.g. \code{"mpg < 18"}.
#' @param flip A logical value. If \code{TRUE}, the x and y axes are swapped via
#'   \code{coord_flip()}. Dimension calculation accounts for the flip.
#'   Default: \code{FALSE}.
#' @param lollipop A logical value. If \code{TRUE}, renders a lollipop plot
#'   with bars extending from x = 0 to each data point, capped by filled dots.
#'   Requires \code{x} to be numeric and \code{y} to be factor/character.
#'   Default: \code{FALSE}.
#' @param border_color Controls the dot border colour and lollipop outer-shadow
#'   appearance:
#'   \itemize{
#'     \item \code{TRUE} — dot borders and lollipop inner bars follow the
#'     \code{fill_by} gradient via \code{scale_color_gradientn()}; lollipop
#'     outer shadow is black.
#'     \item \code{"black"} (default) — constant black borders on dots and
#'     black outer shadow on lollipop bars.
#'     \item A colour string (e.g. \code{"red"}, \code{"#FF0000"}) — constant
#'     colour for both dot borders and lollipop outer shadows.
#'     \item \code{FALSE} — no dot borders and no lollipop outer shadow (the
#'     inner coloured bars remain visible in lollipop mode).
#'   }
#' @param border_size A numeric value for the stroke width of dot borders and
#'   the base linewidth of lollipop bars. In lollipop mode, the outer shadow
#'   uses \code{border_size * 4} and the inner bar uses \code{border_size * 2}.
#'   Default: \code{0.5}.
#' @param border_alpha A numeric value in \code{[0, 1]} controlling the
#'   transparency of dot borders and lollipop bar segments.
#'   Default: \code{1}.
#' @param add_bg A logical value. If \code{TRUE}, alternating background
#'   stripes are drawn behind the points via \code{\link{bg_layer}()}. The
#'   striped axis is determined by \code{bg_direction}. Requires the striped
#'   axis to be non-numeric. Default: \code{FALSE}.
#' @param bg_palette A character string specifying the palette for the
#'   background stripe colours. Passed to \code{\link{bg_layer}()}.
#'   Default: \code{"stripe"}.
#' @param bg_palcolor A character vector of colours for the background stripes.
#'   Passed to \code{\link{bg_layer}()}. When \code{NULL} (default), colours
#'   are derived from \code{bg_palette}.
#' @param bg_alpha A numeric value in \code{[0, 1]} for the transparency of
#'   the background stripes. Default: \code{0.2}.
#' @param bg_direction A character string specifying which axis receives the
#'   alternating background stripes. \code{"vertical"} (default) stripes by x
#'   levels; \code{"horizontal"} stripes by y levels. Abbreviations \code{"v"}
#'   and \code{"h"} are also accepted.
#' @return A \code{ggplot} object with \code{height} and \code{width}
#'   attributes.
#' @keywords internal
#' @importFrom dplyr %>% group_by summarise n first
#' @importFrom ggplot2 geom_point scale_y_discrete scale_size_area scale_fill_gradientn scale_color_gradientn labs
#' @importFrom ggplot2 coord_flip guide_colorbar guide_legend guides guide_none scale_size geom_segment
#' @importFrom ggnewscale new_scale_color
#' @importFrom scales alpha rescale
#' @importFrom ggplot2 waiver
DotPlotAtomic <- function(
    data,
    x,
    y,
    x_sep = "_",
    y_sep = "_",
    flip = FALSE,
    lollipop = FALSE,
    size_by = NULL,
    fill_by = NULL,
    fill_cutoff = NULL,
    palreverse = FALSE,
    size_name = NULL,
    fill_name = NULL,
    fill_cutoff_name = NULL,
    size_min = 1,
    size_max = 10,
    theme = "theme_this",
    theme_args = list(),
    palette = "Spectral",
    palcolor = NULL,
    alpha = 1,
    border_color = "black",
    border_size = 0.5,
    border_alpha = 1,
    lower_quantile = 0,
    upper_quantile = 0.99,
    lower_cutoff = NULL,
    upper_cutoff = NULL,
    facet_by = NULL,
    facet_scales = "fixed",
    facet_ncol = NULL,
    facet_nrow = NULL,
    facet_byrow = TRUE,
    x_text_angle = 0,
    aspect.ratio = 1,
    legend.position = "right",
    legend.direction = "vertical",
    add_bg = FALSE,
    bg_palette = "stripe",
    bg_palcolor = NULL,
    bg_alpha = 0.2,
    bg_direction = c("vertical", "horizontal", "v", "h"),
    title = NULL,
    subtitle = NULL,
    xlab = NULL,
    ylab = NULL,
    keep_na = FALSE,
    keep_empty = FALSE,
    ...
) {
    ggplot <- if (getOption("plotthis.gglogger.enabled", FALSE)) {
        gglogger::ggplot
    } else {
        ggplot2::ggplot
    }
    bg_direction <- match.arg(bg_direction)
    if (bg_direction %in% c("h", "horizontal")) {
        bg_direction <- "horizontal"
    } else {
        bg_direction <- "vertical"
    }
    x_is_numeric <- length(x) == 1 &&
        !is.character(data[[x]]) &&
        !is.factor(data[[x]])
    y_is_numeric <- length(y) == 1 &&
        !is.character(data[[y]]) &&
        !is.factor(data[[y]])
    if (!x_is_numeric) {
        x <- check_columns(
            data,
            x,
            force_factor = TRUE,
            allow_multi = TRUE,
            concat_multi = TRUE,
            concat_sep = x_sep
        )
    }
    if (!y_is_numeric) {
        y <- check_columns(
            data,
            y,
            force_factor = TRUE,
            allow_multi = TRUE,
            concat_multi = TRUE,
            concat_sep = y_sep
        )
    }
    fill_by <- check_columns(data, fill_by)
    facet_by <- check_columns(
        data,
        facet_by,
        force_factor = TRUE,
        allow_multi = TRUE
    )

    data <- process_keep_na_empty(data, keep_na, keep_empty)
    keep_empty_x <- keep_empty[[x]]
    keep_empty_y <- keep_empty[[y]]
    keep_empty_facet <- if (!is.null(facet_by)) {
        keep_empty[[facet_by[1]]]
    } else {
        NULL
    }
    if (length(facet_by) > 1) {
        stopifnot(
            "[DotPlot/LillipopPlot] `keep_empty` for `facet_by` variables must be identical." = identical(
                keep_empty_facet,
                keep_empty[[facet_by[2]]]
            )
        )
    }

    if (!is.null(fill_cutoff) && is.null(fill_by)) {
        stop(
            "[DotPlot/LollipopPlot]'fill_by' must be provided when 'fill_cutoff' is specified."
        )
    }

    if (!is.numeric(size_by)) {
        size_by <- check_columns(data, size_by)
    }
    if (is.null(size_by)) {
        if (is.null(fill_by)) {
            data <- data %>%
                group_by(!!!syms(unique(c(x, y, facet_by)))) %>%
                summarise(.size = n(), .groups = "drop")
        } else {
            warning(
                "[DotPlot] Using the first value of fill_by.",
                immediate. = TRUE
            )
            data <- data %>%
                group_by(!!!syms(unique(c(x, y, facet_by)))) %>%
                summarise(
                    !!sym(fill_by) := first(!!sym(fill_by)),
                    .size = n(),
                    .groups = "drop"
                )
        }
        # keep the levels of x, y, and facet_by
        for (col in unique(c(x, y, facet_by))) {
            if (is.factor(data[[col]])) {
                data[[col]] <- factor(data[[col]], levels = levels(data[[col]]))
            }
        }
        size_by <- ".size"
    }

    if (!is.null(fill_by) && !is.null(fill_cutoff)) {
        # Numeric shorthand: 18 → "< 18"
        if (is.numeric(fill_cutoff)) {
            fill_cutoff <- paste("<", fill_cutoff)
        }
        if (
            !is.character(fill_cutoff) ||
                length(fill_cutoff) != 1 ||
                is.na(fill_cutoff)
        ) {
            stop(
                "[DotPlot/LollipopPlot] 'fill_cutoff' must be a string like '< 18' or '> 18'."
            )
        }
        parsed <- regmatches(
            fill_cutoff,
            regexec("^(<=?|>=?)\\s*(-?[0-9.]+)$", fill_cutoff)
        )[[1]]
        if (length(parsed) != 3) {
            stop(
                "[DotPlot/LollipopPlot] 'fill_cutoff' must be a string like '< 18', '<= 18', '> 18', or '>= 18'."
            )
        }
        cutoff_op <- parsed[2]
        cutoff_val <- as.numeric(parsed[3])
        if (is.na(cutoff_val)) {
            stop(
                "[DotPlot/LollipopPlot] Invalid numeric value in 'fill_cutoff': ",
                fill_cutoff
            )
        }
        fill_cutoff_label <- paste0(fill_by, " ", fill_cutoff)
        switch(
            cutoff_op,
            "<" = {
                data[[fill_by]][data[[fill_by]] < cutoff_val] <- NA
            },
            "<=" = {
                data[[fill_by]][data[[fill_by]] <= cutoff_val] <- NA
            },
            ">" = {
                data[[fill_by]][data[[fill_by]] > cutoff_val] <- NA
            },
            ">=" = {
                data[[fill_by]][data[[fill_by]] >= cutoff_val] <- NA
            }
        )
    }
    if (is.null(fill_by)) {
        data$.fill_by <- 1
        fill_by <- ".fill_by"
        fill_legend <- FALSE
    } else {
        fill_legend <- TRUE
    }

    feat_colors_value <- NULL
    if (!is.null(fill_by) && is.numeric(data[[fill_by]])) {
        result <- prepare_continuous_color_scale(
            data,
            fill_by,
            lower_quantile = lower_quantile,
            upper_quantile = upper_quantile,
            lower_cutoff = lower_cutoff,
            upper_cutoff = upper_cutoff
        )
        data <- result$data
        feat_colors_value <- result$feat_colors_value
    }

    just <- calc_just(x_text_angle)
    p <- ggplot(data, aes(x = !!sym(x), y = !!sym(y)))
    if (add_bg) {
        if (bg_direction == "vertical") {
            if (x_is_numeric) {
                stop(
                    "Vertical 'bg_direction' is not supported when 'x' is numeric."
                )
            }
            p <- p +
                bg_layer(
                    data,
                    x,
                    isTRUE(keep_empty_x),
                    bg_palette,
                    bg_palcolor,
                    bg_alpha,
                    facet_by,
                    bg_direction
                )
        } else {
            if (y_is_numeric) {
                stop(
                    "Horizontal 'bg_direction' is not supported when 'y' is numeric."
                )
            }
            p <- p +
                bg_layer(
                    data,
                    y,
                    isTRUE(keep_empty_y),
                    bg_palette,
                    bg_palcolor,
                    bg_alpha,
                    facet_by,
                    bg_direction
                )
        }
    }
    if (!x_is_numeric) {
        p <- p + scale_x_discrete(drop = !isTRUE(keep_empty_x))
    }
    if (!y_is_numeric) {
        p <- p + scale_y_discrete(drop = !isTRUE(keep_empty_y))
    }

    if (isTRUE(lollipop)) {
        lw_outer <- border_size * 4
        lw_inner <- border_size * 2
        p <- p +
            scale_x_continuous(expand = c(0, 0, 0.05, 0))
        # outer shadow / border (hidden when border_color is FALSE)
        if (!isFALSE(border_color)) {
            outer_color <- if (isTRUE(border_color)) {
                scales::alpha("black", border_alpha)
            } else {
                scales::alpha(border_color, border_alpha)
            }
            p <- p +
                geom_segment(
                    aes(x = 0, xend = !!sym(x), yend = !!sym(y)),
                    color = outer_color,
                    linewidth = lw_outer
                )
        }
        # inner colored bar (always drawn)
        p <- p +
            geom_segment(
                aes(
                    x = 0,
                    xend = !!sym(x),
                    yend = !!sym(y),
                    color = !!sym(fill_by)
                ),
                linewidth = lw_inner
            ) +
            scale_color_gradientn(
                n.breaks = 5,
                colors = palette_this(
                    palette = palette,
                    palcolor = palcolor,
                    reverse = palreverse,
                    alpha = border_alpha
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
                guide = "none"
            ) +
            new_scale_color()
    }
    if (is.numeric(size_by)) {
        if (isTRUE(border_color)) {
            p <- p +
                geom_point(
                    aes(fill = !!sym(fill_by), color = !!sym(fill_by)),
                    size = size_by,
                    shape = 21,
                    stroke = border_size,
                    alpha = alpha
                )
        } else {
            p <- p +
                geom_point(
                    aes(fill = !!sym(fill_by), color = ""),
                    size = size_by,
                    shape = 21,
                    stroke = border_size,
                    alpha = alpha
                )
        }
    } else {
        if (isTRUE(border_color)) {
            p <- p +
                geom_point(
                    aes(
                        size = !!sym(size_by),
                        fill = !!sym(fill_by),
                        color = !!sym(fill_by)
                    ),
                    shape = 21,
                    stroke = border_size,
                    alpha = alpha
                ) +
                # scale_size_area(max_size = 6, n.breaks = 4) +
                scale_size(range = c(size_min, size_max)) +
                guides(
                    size = guide_legend(
                        title = size_name %||% size_by,
                        override.aes = list(fill = "transparent", shape = 21),
                        order = 1
                    )
                )
        } else {
            p <- p +
                geom_point(
                    aes(
                        size = !!sym(size_by),
                        fill = !!sym(fill_by),
                        color = ""
                    ),
                    shape = 21,
                    stroke = border_size,
                    alpha = alpha
                ) +
                # scale_size_area(max_size = 6, n.breaks = 4) +
                scale_size(range = c(size_min, size_max)) +
                guides(
                    size = guide_legend(
                        title = size_name %||% size_by,
                        override.aes = list(fill = "transparent", shape = 21),
                        order = 1
                    )
                )
        }
    }

    p <- p +
        scale_fill_gradientn(
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
            guide = if (isTRUE(fill_legend)) {
                guide_colorbar(
                    title = fill_name %||% fill_by,
                    frame.colour = "black",
                    ticks.colour = "black",
                    title.hjust = 0,
                    order = 2
                )
            } else {
                guide_none()
            }
        ) +
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
            panel.grid.major = element_line(colour = "grey80", linetype = 2),
            axis.text.x = element_text(
                angle = x_text_angle,
                hjust = just$h,
                vjust = just$v
            )
        )

    if (isTRUE(border_color)) {
        p <- p +
            scale_color_gradientn(
                n.breaks = 5,
                colors = palette_this(
                    palette = palette,
                    palcolor = palcolor,
                    reverse = palreverse,
                    alpha = border_alpha
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
                guide = "none"
            )
    } else {
        border_color_value <- if (isFALSE(border_color)) {
            "transparent"
        } else {
            scales::alpha(border_color, border_alpha)
        }
        p <- p +
            scale_color_manual(
                values = border_color_value,
                na.value = border_color_value,
                guide = "none"
            )
    }
    if (!is.null(fill_by) && !is.null(fill_cutoff) && anyNA(data[[fill_by]])) {
        fill_cutoff_colour <- if (isTRUE(border_color)) {
            scales::alpha("black", border_alpha)
        } else if (isFALSE(border_color)) {
            "transparent"
        } else {
            scales::alpha(border_color, border_alpha)
        }
        p <- p +
            guides(
                color = guide_legend(
                    title = fill_cutoff_name %||% fill_cutoff_label,
                    override.aes = list(
                        colour = fill_cutoff_colour,
                        fill = "grey80",
                        size = 3
                    ),
                    order = 3
                )
            )
    }

    if (isTRUE(flip)) {
        p <- p + coord_flip()
    }

    if (x_is_numeric) {
        nx <- 5
    } else if (isTRUE(keep_empty_x)) {
        nx <- nlevels(data[[x]])
    } else {
        nx <- nlevels(droplevels(data[[x]]))
    }
    if (y_is_numeric) {
        ny <- 5
    } else if (isTRUE(keep_empty_y)) {
        ny <- nlevels(data[[y]])
    } else {
        ny <- nlevels(droplevels(data[[y]]))
    }

    y_label_len <- if (!y_is_numeric) {
        max(sapply(strsplit(levels(data[[y]]), "\n"), function(x) {
            max(nchar(x))
        }))
    } else {
        0
    }
    legend_nchar <- max(
        nchar(c(
            fill_name %||% fill_by,
            if (!is.numeric(size_by)) size_name %||% size_by else character(0)
        )),
        na.rm = TRUE,
        5
    )

    # Use aspect.ratio = NULL so that each axis is sized independently from its content:
    #   - visual x-axis (groups when not flipped, terms when flipped) drives width via n_x
    #   - visual y-axis (terms when not flipped, groups when flipped) drives height via n_y
    # Scale factors: 0.9 per group (fewer categories, need more space),
    #                0.6 per term (often many, needs a compact but readable size).
    # min_width=5 ensures the plot is wide enough for a horizontal legend colorbar.
    # min_height=4 prevents the flipped case from being too short for the group rows.
    dims <- calculate_plot_dimensions(
        base_height = 4.5,
        aspect.ratio = NULL,
        n_x = if (isTRUE(flip)) ny else nx,
        x_scale_factor = if (isTRUE(flip)) 0.6 else 0.9,
        n_y = if (isTRUE(flip)) nx else ny,
        y_scale_factor = if (isTRUE(flip)) 0.9 else 0.6,
        legend.position = legend.position,
        legend.direction = legend.direction,
        legend_n = 2,
        legend_nchar = legend_nchar,
        min_width = 5,
        min_height = 4
    )

    if (is.null(dims)) {
        if (ny / nx > 10) {
            height <- ny * 0.2
            width <- nx * 2
        } else if (ny / nx < 0.1) {
            height <- ny * 2
            width <- nx * 0.4
        } else {
            height <- ny * 0.5
            width <- nx * 0.8
        }
        width <- width + y_label_len * 0.1
        width <- max(width, 3)
        height <- max(height, 3)
        ar <- if (isTRUE(flip)) 1 / aspect.ratio else aspect.ratio
        coupled_height <- width * ar
        if (abs(coupled_height - height) / max(height, 1) < 0.5) {
            height <- coupled_height
            height <- max(height, 3)
        }
        if (!identical(legend.position, "none")) {
            if (legend.position %in% c("right", "left")) {
                width <- width + 1
            } else if (legend.direction == "horizontal") {
                height <- height + 1
            } else {
                width <- width + 2
            }
        }
        height <- max(height, 3)
        if (isTRUE(flip)) {
            attr(p, "height") <- width
            attr(p, "width") <- height
        } else {
            attr(p, "height") <- height
            attr(p, "width") <- width
        }
    } else {
        h <- dims$height
        w <- dims$width + y_label_len * 0.1
        # For dot-plot matrices rendered with a square panel (default aspect.ratio = 1),
        # prevent extreme H/W ratios that leave large blank areas inside the figure.
        # Neither dimension should be less than 50 % of the other.
        h <- max(h, w * 0.5)
        w <- max(w, h * 0.5)
        attr(p, "height") <- min(h, 12)
        attr(p, "width") <- min(w, 12)
    }

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

#' Dot Plot, Scatter Plot, and Lollipop Plot
#'
#' @rdname dotplot
#' @description
#' \code{DotPlot()} renders a matrix of filled circles (dot plot) where dot
#' size encodes one numeric variable and fill colour encodes another. Either
#' axis can be numeric or factor, enabling four layout combinations:
#'
#' \itemize{
#'   \item \strong{Both axes factor} — a classic dot matrix (e.g. genes × cell
#'   types), where each cell is a dot whose size reflects expression magnitude
#'   and whose colour reflects a summary statistic.
#'   \item \strong{Both axes numeric} — a scatter plot, with dots positioned by
#'   x/y coordinates, sized by a third variable, and coloured by a fourth.
#'   \item \strong{One numeric, one factor} — a strip plot or (with
#'   \code{lollipop = TRUE}) a lollipop chart.
#' }
#'
#' \code{LollipopPlot()} is a convenience wrapper that sets
#' \code{lollipop = TRUE}, producing horizontal bars from the y-axis to each
#' data point, capped by filled dots. It expects a numeric \code{x} and a
#' factor/character \code{y}.
#'
#' Key features:
#' \itemize{
#'   \item \strong{Auto-count:} when \code{size_by = NULL}, the per-combination
#'   observation count is computed automatically.
#'   \item \strong{fill_cutoff:} values in \code{fill_by} matching a threshold
#'   expression (e.g. \code{"< 18"}) are greyed out with a dedicated legend
#'   entry.
#'   \item \strong{Background stripes:} \code{add_bg = TRUE} draws alternating
#'   background bands along the discrete axis for visual grouping.
#'   \item \strong{Border modes:} \code{border_color} can track the fill
#'   gradient (\code{TRUE}), use a constant colour (\code{"black"}), or be
#'   suppressed (\code{FALSE}).
#'   \item \strong{Colour scale trimming:} \code{lower_quantile} /
#'   \code{upper_quantile} (or explicit \code{lower_cutoff} /
#'   \code{upper_cutoff}) trim the continuous fill scale extremes.
#' }
#'
#' @section split_by Workflow (DotPlot):
#'
#' When \code{split_by} is provided, the following pipeline executes:
#' \enumerate{
#'   \item \strong{Column validation} — \code{check_columns()} resolves
#'   \code{split_by} (force_factor, allow_multi, concat_multi).
#'   \item \strong{NA / empty pre-processing} — \code{process_keep_na_empty()}
#'   handles \code{keep_na} / \code{keep_empty} for the split column before
#'   splitting, then removes the split column from the per-split
#'   \code{keep_na}/\code{keep_empty} lists.
#'   \item \strong{Data splitting} — splits \code{data} by \code{split_by}
#'   levels (preserving factor level order).
#'   \item \strong{Per-split palette / colour} — \code{check_palette()} and
#'   \code{check_palcolor()} resolve per-split palette and colour overrides.
#'   \item \strong{Per-split legend} — \code{check_legend()} resolves
#'   \code{legend.position} and \code{legend.direction} per split.
#'   \item \strong{Per-split title} — when \code{title} is a function, it
#'   receives the default title (the split level name) and can return a
#'   custom string; otherwise \code{title \%||\% split_level} is used.
#'   \item \strong{Dispatch} — each split subset is passed to
#'   \code{\link{DotPlotAtomic}} (with \code{lollipop = FALSE}).
#'   \item \strong{Combination} — \code{\link{combine_plots}()} assembles the
#'   list of plots via \code{patchwork::wrap_plots}, honouring
#'   \code{nrow}/\code{ncol}/\code{byrow}/\code{design}.
#' }
#'
#' @section split_by Workflow (LollipopPlot):
#'
#' Same pipeline as \code{DotPlot} above, but dispatches to
#' \code{\link{DotPlotAtomic}} with \code{lollipop = TRUE}.
#'
#' @inheritParams DotPlotAtomic
#' @inheritParams common_args
#' @param split_by The column(s) to split data by and generate separate plots
#'   for each level. The split column is processed for \code{keep_na} /
#'   \code{keep_empty} before splitting.
#' @param split_by_sep A character string used to concatenate multiple
#'   \code{split_by} column values. Default: \code{"_"}.
#' @param seed The random seed for reproducibility. Passed to
#'   \code{validate_common_args()}. Default: \code{8525}.
#' @param combine A logical value. If \code{TRUE} (the default), the list of
#'   per-split plots is combined into a single \code{patchwork} object. If
#'   \code{FALSE}, returns the raw list.
#' @param nrow,ncol,byrow Integers controlling the layout of combined plots via
#'   \code{patchwork::wrap_plots()}. \code{byrow = TRUE} (default) fills the
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
#'   (when \code{combine = TRUE} with \code{split_by}), or a list of
#'   \code{ggplot} objects (when \code{combine = FALSE}).
#' @export
#' @examples
#' \donttest{
#' mtcars <- datasets::mtcars
#' mtcars$carb <- factor(mtcars$carb)
#' mtcars$gear <- factor(mtcars$gear)
#'
#' # --- Basic dot plot (factor × factor, size + fill) ---
#' DotPlot(mtcars, x = "carb", y = "gear", size_by = "wt",
#'         fill_by = "mpg", fill_cutoff = "< 18")
#' DotPlot(mtcars, x = "carb", y = "gear", size_by = "wt",
#'         fill_by = "mpg", fill_cutoff = "> 18")
#'
#' # --- Background stripes ---
#' DotPlot(mtcars, x = "carb", y = "gear", size_by = "wt",
#'         fill_by = "mpg", fill_cutoff = "< 18", add_bg = TRUE)
#' DotPlot(mtcars, x = "carb", y = "gear", size_by = "wt",
#'         fill_by = "mpg", fill_cutoff = "< 18", add_bg = TRUE,
#'         bg_direction = "h")
#'
#' # --- Faceting ---
#' DotPlot(mtcars, x = "carb", y = "gear", size_by = "wt",
#'         fill_by = "mpg", fill_cutoff = "< 18", facet_by = "cyl")
#' DotPlot(mtcars, x = "carb", y = "gear", size_by = "wt",
#'         fill_by = "mpg", fill_cutoff = "< 18", facet_by = "cyl",
#'         facet_scales = "free_x")
#'
#' # --- split_by ---
#' DotPlot(mtcars, x = "carb", y = "gear", size_by = "wt",
#'         fill_by = "mpg", fill_cutoff = "< 18", split_by = "cyl")
#' DotPlot(mtcars, x = "carb", y = "gear", size_by = "wt",
#'         fill_by = "mpg", fill_cutoff = "< 18", split_by = "cyl",
#'         palette = list("4" = "Set1", "6" = "Paired", "8" = "Reds"))
#'
#' # --- Scatter plot (both axes numeric) ---
#' DotPlot(mtcars, x = "qsec", y = "drat", size_by = "wt",
#'         fill_by = "mpg", fill_cutoff = "< 18",
#'         fill_cutoff_name = "Small mpgs")
#'
#' # --- keep_na and keep_empty ---
#' mtcars$carb[mtcars$carb == "1"] <- NA
#' mtcars$gear[mtcars$gear == "3"] <- NA
#' DotPlot(mtcars, x = "carb", y = "gear", size_by = "wt",
#'         fill_by = "mpg", fill_cutoff = "< 18",
#'         keep_na = TRUE, keep_empty = TRUE)
#'
#' # --- Border customization ---
#' DotPlot(mtcars, x = "carb", y = "gear", size_by = "wt",
#'         fill_by = "mpg", border_color = "red", border_size = 2)
#' DotPlot(mtcars, x = "carb", y = "gear", size_by = "wt",
#'         fill_by = "mpg", border_color = TRUE, border_size = 1.5,
#'         border_alpha = 0.5)
#' DotPlot(mtcars, x = "carb", y = "gear",
#'         fill_by = "mpg", border_color = FALSE)
#'
#' # --- Colour scale trimming ---
#' DotPlot(mtcars, x = "carb", y = "gear", size_by = "wt",
#'         fill_by = "mpg", lower_quantile = 0.05, upper_quantile = 0.95)
#' DotPlot(mtcars, x = "carb", y = "gear", size_by = "wt",
#'         fill_by = "mpg", lower_cutoff = 15, upper_cutoff = 25)
#' }
DotPlot <- function(
    data,
    x,
    y,
    x_sep = "_",
    y_sep = "_",
    flip = FALSE,
    split_by = NULL,
    split_by_sep = "_",
    size_name = NULL,
    fill_name = NULL,
    fill_cutoff_name = NULL,
    add_bg = FALSE,
    bg_palette = "stripe",
    bg_palcolor = NULL,
    bg_alpha = 0.2,
    bg_direction = c("vertical", "horizontal", "v", "h"),
    size_by = NULL,
    fill_by = NULL,
    fill_cutoff = NULL,
    palreverse = FALSE,
    size_min = 1,
    size_max = 10,
    theme = "theme_this",
    theme_args = list(),
    palette = "Spectral",
    palcolor = NULL,
    alpha = 1,
    border_color = "black",
    border_size = 0.5,
    border_alpha = 1,
    lower_quantile = 0,
    upper_quantile = 0.99,
    lower_cutoff = NULL,
    upper_cutoff = NULL,
    facet_by = NULL,
    facet_scales = "fixed",
    facet_ncol = NULL,
    facet_nrow = NULL,
    facet_byrow = TRUE,
    x_text_angle = 0,
    seed = 8525,
    aspect.ratio = 1,
    legend.position = "right",
    legend.direction = "vertical",
    title = NULL,
    subtitle = NULL,
    xlab = NULL,
    ylab = NULL,
    keep_na = FALSE,
    keep_empty = FALSE,
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
    keep_na <- check_keep_na(keep_na, c(x, y, split_by, fill_by, facet_by))
    keep_empty <- check_keep_empty(
        keep_empty,
        c(x, y, split_by, fill_by, facet_by)
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
            DotPlotAtomic(
                datas[[nm]],
                x = x,
                y = y,
                x_sep = x_sep,
                y_sep = y_sep,
                flip = flip,
                bg_direction = bg_direction,
                size_min = size_min,
                size_max = size_max,
                lollipop = FALSE,
                size_by = size_by,
                fill_by = fill_by,
                fill_cutoff = fill_cutoff,
                palreverse = palreverse,
                theme = theme,
                theme_args = theme_args,
                palette = palette[[nm]],
                palcolor = palcolor[[nm]],
                alpha = alpha,
                border_color = border_color,
                border_size = border_size,
                border_alpha = border_alpha,
                lower_quantile = lower_quantile,
                upper_quantile = upper_quantile,
                lower_cutoff = lower_cutoff,
                upper_cutoff = upper_cutoff,
                facet_by = facet_by,
                facet_scales = facet_scales,
                facet_ncol = facet_ncol,
                facet_nrow = facet_nrow,
                facet_byrow = facet_byrow,
                x_text_angle = x_text_angle,
                size_name = size_name,
                fill_name = fill_name,
                fill_cutoff_name = fill_cutoff_name,
                add_bg = add_bg,
                bg_palette = bg_palette,
                bg_palcolor = bg_palcolor,
                bg_alpha = bg_alpha,
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

#' @rdname dotplot
#' @description
#' \code{LollipopPlot()} is a convenience wrapper around \code{DotPlot()}
#' that sets \code{lollipop = TRUE}. It renders a horizontal bar extending
#' from the y-axis (\code{x = 0}) to each data point, capped by a filled dot.
#' The bar has a two-layer construction: an outer shadow (black or custom
#' colour) and an inner coloured segment that follows the \code{fill_by}
#' gradient. Dot size scales by \code{size_by} (or the per-combination
#' observation count when \code{size_by = NULL}).
#'
#' Expects \code{x} to be a numeric column and \code{y} to be a factor or
#' character column.
#'
#' @inheritParams DotPlot
#' @inheritParams common_args
#' @param x A character string naming the column for the x-axis. Must be a
#'   numeric column (bars extend from 0 to the data value).
#' @param y A character string naming the column for the y-axis. Must be a
#'   factor or character column (each level gets a lollipop bar).
#' @return A \code{ggplot} object (single plot), a \code{patchwork} object
#'   (when \code{combine = TRUE} with \code{split_by}), or a list of
#'   \code{ggplot} objects (when \code{combine = FALSE}).
#' @export
#' @examples
#' \donttest{
#' mtcars <- datasets::mtcars
#'
#' # --- Basic lollipop ---
#' LollipopPlot(mtcars, x = "qsec", y = "drat", size_by = "wt",
#'              fill_by = "mpg")
#'
#' # --- Faceting ---
#' LollipopPlot(mtcars, x = "qsec", y = "drat", size_by = "wt",
#'              fill_by = "mpg", fill_cutoff = "< 18", facet_by = "cyl",
#'              facet_scales = "free_y")
#'
#' # --- split_by ---
#' LollipopPlot(mtcars, x = "qsec", y = "drat", size_by = "wt",
#'              split_by = "vs", palette = list("0" = "Reds", "1" = "Blues"))
#'
#' # --- Border customization ---
#' LollipopPlot(mtcars, x = "qsec", y = "drat", size_by = "wt",
#'              fill_by = "mpg", border_color = "red", border_size = 2)
#' LollipopPlot(mtcars, x = "qsec", y = "drat", size_by = "wt",
#'              fill_by = "mpg", border_color = TRUE, border_size = 1.5,
#'              border_alpha = 0.5)
#' LollipopPlot(mtcars, x = "qsec", y = "drat",
#'              fill_by = "mpg", border_color = FALSE)
#' }
LollipopPlot <- function(
    data,
    x,
    y,
    y_sep = NULL,
    flip = FALSE,
    split_by = NULL,
    split_by_sep = "_",
    size_name = NULL,
    fill_name = NULL,
    fill_cutoff_name = NULL,
    size_by = NULL,
    fill_by = NULL,
    fill_cutoff = NULL,
    palreverse = FALSE,
    size_min = 1,
    size_max = 10,
    theme = "theme_this",
    theme_args = list(),
    palette = "Spectral",
    palcolor = NULL,
    alpha = 1,
    border_color = "black",
    border_size = 0.5,
    border_alpha = 1,
    lower_quantile = 0,
    upper_quantile = 0.99,
    lower_cutoff = NULL,
    upper_cutoff = NULL,
    facet_by = NULL,
    facet_scales = "fixed",
    facet_ncol = NULL,
    facet_nrow = NULL,
    facet_byrow = TRUE,
    x_text_angle = 0,
    seed = 8525,
    aspect.ratio = 1,
    legend.position = "right",
    legend.direction = "vertical",
    title = NULL,
    subtitle = NULL,
    xlab = NULL,
    ylab = NULL,
    keep_na = FALSE,
    keep_empty = FALSE,
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
    keep_na <- check_keep_na(keep_na, c(y, split_by, fill_by, facet_by))
    keep_empty <- check_keep_empty(
        keep_empty,
        c(y, split_by, fill_by, facet_by)
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
            DotPlotAtomic(
                datas[[nm]],
                lollipop = TRUE,
                x = x,
                y = y,
                x_sep = NULL,
                y_sep = y_sep,
                flip = flip,
                size_min = size_min,
                size_max = size_max,
                size_by = size_by,
                fill_by = fill_by,
                fill_cutoff = fill_cutoff,
                palreverse = palreverse,
                theme = theme,
                theme_args = theme_args,
                palette = palette[[nm]],
                palcolor = palcolor[[nm]],
                alpha = alpha,
                border_color = border_color,
                border_size = border_size,
                border_alpha = border_alpha,
                lower_quantile = lower_quantile,
                upper_quantile = upper_quantile,
                lower_cutoff = lower_cutoff,
                upper_cutoff = upper_cutoff,
                facet_by = facet_by,
                facet_scales = facet_scales,
                facet_ncol = facet_ncol,
                facet_nrow = facet_nrow,
                facet_byrow = facet_byrow,
                x_text_angle = x_text_angle,
                size_name = size_name,
                fill_name = fill_name,
                fill_cutoff_name = fill_cutoff_name,
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
