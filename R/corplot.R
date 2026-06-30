#' Atomic Correlation Plot
#'
#' @description
#' Core implementation for drawing a scatter plot of two variables with a
#' linear regression line, optional correlation statistics, and point
#' highlighting.  This is the workhorse behind the exported
#' \code{\link{CorPlot}} — it takes a \strong{single} data frame (no
#' \code{split_by} support) and returns a \code{ggplot} object with
#' faceting applied.
#'
#' The function supports \strong{group-based colouring} (\code{group_by}),
#' point highlighting by expression or rowname, multiple annotation items
#' (regression equation, R-squared, p-value, Spearman/Pearson/Kendall
#' correlation, N), raster rendering for large datasets, configurable
#' regression line style, and faceting.
#'
#' @section Architecture:
#' \enumerate{
#'   \item \strong{ggplot dispatch} — selects \code{gglogger::ggplot} or
#'         \code{ggplot2::ggplot} based on
#'         \code{getOption("plotthis.gglogger.enabled")}.
#'   \item \strong{Parameter normalisation} — \code{match.arg()} resolves
#'         \code{anno_position} abbreviations (\code{"tl"}, \code{"tr"},
#'         \code{"bl"}, \code{"br"}) to their full forms.
#'         \code{raster_dpi} is expanded to length 2 when given as a scalar.
#'   \item \strong{Column resolution} — \code{x}, \code{y}, and
#'         \code{group_by} are validated via \code{\link{check_columns}}.
#'         Multi-column \code{group_by} is concatenated with
#'         \code{group_by_sep}.
#'   \item \strong{Grouping fallback} — when \code{group_by = NULL}, a dummy
#'         \code{.group} column is created and the legend is suppressed
#'         (\code{legend.position = "none"}).
#'   \item \strong{Annotation data calculation} — the data is grouped by
#'         \code{facet_by} and a linear model (\code{lm(y ~ x)}) is fitted
#'         per group.  Each requested \code{anno_items} is computed:
#'         \itemize{
#'           \item \code{"eq"} — regression equation \code{y = a + bx}.
#'           \item \code{"r2"} — R-squared of the model.
#'           \item \code{"p"} — p-value of the x coefficient.
#'           \item \code{"spearman"} — Spearman's rho.
#'           \item \code{"pearson"} — Pearson's r.
#'           \item \code{"kendall"} — Kendall's tau.
#'           \item \code{"n"} — number of observations.
#'         }
#'         The results are stored in an \code{annodata} data frame for
#'         \code{geom_text_repel}.
#'   \item \strong{Highlight parsing} — the \code{highlight} argument is
#'         resolved into a \code{.highlight} logical column:
#'         \itemize{
#'           \item \code{TRUE} — highlights all points.
#'           \item A character expression — evaluated via
#'                 \code{dplyr::filter} to select rows.
#'           \item A character vector — matched against rownames of the data.
#'           \item A numeric vector — treated as row indices.
#'         }
#'   \item \strong{Point rendering} — two branches:
#'         \itemize{
#'           \item \strong{Raster mode} (\code{raster = TRUE}) — uses
#'                 \code{scattermore::geom_scattermore()} for efficient
#'                 rendering of large datasets.  Highlighted points are drawn
#'                 in two layers (stroke + fill).
#'           \item \strong{Vector mode} (\code{raster = FALSE}, default) —
#'                 uses \code{ggplot2::geom_point()} with configurable size,
#'                 shape, and alpha.  Highlighted points get an outer stroke
#'                 via a second point layer.
#'         }
#'   \item \strong{Regression line} — \code{geom_smooth(method = "lm")}
#'         draws the linear regression line with optional standard error band
#'         (\code{smooth_se}).
#'   \item \strong{Annotation text} — \code{ggrepel::geom_text_repel()}
#'         places the computed annotations at the specified
#'         \code{anno_position} corner, with background styling.
#'   \item \strong{Colour scale} — \code{scale_color_manual()} maps group
#'         levels to colours via \code{\link{palette_this}()}.
#'   \item \strong{Labels and theme} — \code{labs()} sets titles and axis
#'         labels.  The theme is applied via \code{do_call()}, with
#'         \code{aspect.ratio}, \code{legend.position}, and
#'         \code{legend.direction} enforced.
#'   \item \strong{Dimension calculation} — \code{\link{calculate_plot_dimensions}()}
#'         computes \code{height} and \code{width} attributes from
#'         \code{base_height = 4.5}, \code{aspect.ratio}, legend metrics, and
#'         the number of group levels.
#'   \item \strong{Faceting} — \code{\link{facet_plot}()} applies
#'         \code{facet_wrap} / \code{facet_grid} if \code{facet_by} is
#'         provided.
#' }
#'
#' @inheritParams common_args
#' @param x A character string specifying the column name for the x-axis.
#'  Must be numeric.
#' @param y A character string specifying the column name for the y-axis.
#'  Must be numeric.
#' @param group_by A character vector of column names to colour the points
#'  by.  Each unique combination becomes a separate group in the legend.
#'  Multiple columns are concatenated with \code{group_by_sep}.  When
#'  \code{NULL}, all points are a single colour and the legend is hidden.
#' @param group_by_sep A character string to separate concatenated
#'  \code{group_by} columns.  Default \code{"_"}.
#' @param group_name A character string used as the colour legend title.
#'  When \code{NULL}, the \code{group_by} column name is used.
#' @param pt_size A numeric value specifying the size of the points.
#'  Default: \code{2}.
#' @param pt_shape A numeric value specifying the shape of the points
#'  (see \code{\link[ggplot2]{geom_point}}).  Default: \code{16} (filled
#'  circle).
#' @param raster A logical value.  When \code{TRUE}, uses
#'  \code{scattermore::geom_scattermore()} for efficient rendering of
#'  large datasets.  Default: \code{FALSE}.
#' @param raster_dpi An integer vector of length 1 or 2 specifying the
#'  raster resolution in (width, height) pixels.  When a single value is
#'  provided, it is recycled.  Default: \code{c(512, 512)}.
#' @param highlight Specifies which points to emphasise.  Can be:
#'  \itemize{
#'    \item \code{TRUE} — highlight all points.
#'    \item A character expression (e.g. \code{'Species == "setosa"'}) —
#'          evaluated via \code{dplyr::filter}.
#'    \item A character vector — matched against rownames of the data.
#'    \item A numeric vector — treated as row indices.
#'  }
#'  Default: \code{NULL} (no highlighting).
#' @param highlight_color A character string specifying the colour of the
#'  highlighted point borders.  Default: \code{"black"}.
#' @param highlight_size A numeric value specifying the size of the
#'  highlighted points (the inner fill).  Default: \code{1}.
#' @param highlight_alpha A numeric value specifying the alpha transparency
#'  of the highlighted points.  Default: \code{1}.
#' @param highlight_stroke A numeric value specifying the stroke width of
#'  the highlighted point borders.  The outer layer size is
#'  \code{highlight_size + highlight_stroke}.  Default: \code{0.8}.
#' @param anno_items A character vector specifying which statistics to
#'  display as text annotation.  Available items: \code{"eq"} (regression
#'  equation), \code{"r2"} (R-squared), \code{"p"} (p-value),
#'  \code{"spearman"}, \code{"pearson"}, \code{"kendall"},
#'  \code{"n"} (observation count).  Default: \code{c("eq", "r2", "p")}.
#' @param anno_size A numeric value specifying the font size of the
#'  annotation text (scaled by \code{base_size / 12}).  Default: \code{3}.
#' @param anno_fg A character string specifying the colour of the
#'  annotation text.  Default: \code{"black"}.
#' @param anno_bg A character string specifying the background colour of
#'  the annotation text boxes.  Default: \code{"white"}.
#' @param anno_bg_r A numeric value specifying the corner radius of the
#'  annotation text background boxes.  Default: \code{0.1}.
#' @param anno_position A character string specifying the corner position
#'  of the annotation text.  One of \code{"topleft"} (alias \code{"tl"}),
#'  \code{"topright"} (\code{"tr"}), \code{"bottomleft"} (\code{"bl"}),
#'  \code{"bottomright"} (\code{"br"}).
#' @param add_smooth A logical value.  When \code{TRUE} (default), a linear
#'  regression line (\code{geom_smooth(method = "lm")}) is added.
#' @param smooth_color A character string specifying the colour of the
#'  regression line.  Default: \code{"red2"}.
#' @param smooth_width A numeric value specifying the linewidth of the
#'  regression line.  Default: \code{1.5}.
#' @param smooth_se A logical value.  When \code{TRUE}, a standard error
#'  band is drawn around the regression line.  Default: \code{FALSE}.
#' @return A \code{ggplot} object with \code{height} and \code{width}
#'  attributes (in inches) attached.
#' @keywords internal
#' @importFrom stats cor lm coef
#' @importFrom rlang syms sym
#' @importFrom dplyr group_modify filter
#' @importFrom ggplot2 geom_point geom_smooth geom_text scale_color_manual labs
CorPlotAtomic <- function(
    data,
    x,
    y,
    group_by = NULL,
    group_by_sep = "_",
    group_name = NULL,
    pt_size = 2,
    pt_shape = 16,
    alpha = 1,
    raster = FALSE,
    raster_dpi = c(512, 512),
    highlight = NULL,
    highlight_color = "black",
    highlight_size = 1,
    highlight_alpha = 1,
    highlight_stroke = 0.8,
    anno_items = c("eq", "r2", "p"),
    anno_size = 3,
    anno_fg = "black",
    anno_bg = "white",
    anno_bg_r = 0.1,
    anno_position = c(
        "topleft",
        "topright",
        "bottomleft",
        "bottomright",
        "tl",
        "tr",
        "bl",
        "br"
    ),
    add_smooth = TRUE,
    smooth_color = "red2",
    smooth_width = 1.5,
    smooth_se = FALSE,
    theme = "theme_this",
    theme_args = list(),
    palette = ifelse(is.null(group_by), "Spectral", "Paired"),
    palcolor = NULL,
    palreverse = FALSE,
    title = NULL,
    subtitle = NULL,
    xlab = NULL,
    ylab = NULL,
    facet_by = NULL,
    facet_scales = "fixed",
    facet_ncol = NULL,
    facet_nrow = NULL,
    facet_byrow = TRUE,
    aspect.ratio = 1,
    legend.position = waiver(),
    legend.direction = "vertical",
    seed = 8525,
    ...
) {
    set.seed(seed)
    ggplot <- if (getOption("plotthis.gglogger.enabled", FALSE)) {
        gglogger::ggplot
    } else {
        ggplot2::ggplot
    }
    anno_position <- match.arg(anno_position)
    anno_position <- switch(
        anno_position,
        tl = "topleft",
        tr = "topright",
        bl = "bottomleft",
        br = "bottomright",
        anno_position
    )
    if (length(raster_dpi) == 1) {
        raster_dpi <- rep(raster_dpi, 2)
    }

    x <- check_columns(data, x)
    y <- check_columns(data, y)
    group_by <- check_columns(
        data,
        group_by,
        force_factor = TRUE,
        allow_multi = TRUE,
        concat_multi = TRUE,
        concat_sep = group_by_sep
    )

    if (is.null(group_by)) {
        group_by <- ".group"
        data[[group_by]] <- factor("")
        legend.position <- ifelse(
            inherits(legend.position, "waiver"),
            "none",
            "right"
        )
    } else {
        legend.position <- ifelse(
            inherits(legend.position, "waiver"),
            "right",
            legend.position
        )
    }

    base_size <- theme_args$base_size %||% 12
    text_size_scale <- base_size / 12

    # Calcuate the equation of the line, r-squared, and rho
    annodata <- data %>%
        dplyr::group_by(!!!syms(facet_by)) %>%
        group_modify(function(dat, g) {
            m <- lm(data[[y]] ~ data[[x]])
            anno <- c()
            for (item in anno_items) {
                if (item == "eq") {
                    a <- format(as.numeric(coef(m)[1]), digits = 2)
                    b <- format(as.numeric(abs(coef(m)[2])), digits = 2)
                    if (is.na(coef(m)[2])) {
                        anno_eq <- substitute(
                            italic(y) == "NaN"
                        )
                    } else if (coef(m)[2] >= 0) {
                        anno_eq <- substitute(
                            italic(y) == a + b %.% italic(x),
                            list(a = a, b = b)
                        )
                    } else {
                        anno_eq <- substitute(
                            italic(y) == a - b %.% italic(x),
                            list(a = a, b = b)
                        )
                    }
                    anno <- c(anno, as.character(as.expression(anno_eq)))
                } else if (item == "r2") {
                    anno_r2 <- substitute(
                        italic(r)^2 ~ "=" ~ r2,
                        list(r2 = format(summary(m)$r.squared, digits = 2))
                    )
                    anno <- c(anno, as.character(as.expression(anno_r2)))
                } else if (item == "p") {
                    coefs <- summary(m)$coefficients
                    if (nrow(coefs) < 2 || all(is.na(coefs[2, ]))) {
                        # Case: x is constant → no slope, no p-value
                        anno_p <- substitute(
                            italic("coeff.") ~ italic(p) ~ "=" ~ "NA"
                        )
                    } else {
                        anno_p <- substitute(
                            italic("coeff.") ~ italic(p) ~ "=" ~ pvalue,
                            list(
                                pvalue = format(
                                    coefs[2, 4],
                                    digits = 2
                                )
                            )
                        )
                    }
                    anno <- c(anno, as.character(as.expression(anno_p)))
                } else if (item == "spearman") {
                    rho <- cor(dat[[x]], dat[[y]], method = "spearman")
                    anno_rho <- substitute(
                        italic("spearman's") ~ italic(rho) ~ "=" ~ value,
                        list(value = format(rho, digits = 2))
                    )
                    anno <- c(anno, as.character(as.expression(anno_rho)))
                } else if (item == "pearson") {
                    r <- cor(dat[[x]], dat[[y]], method = "pearson")
                    anno_r <- substitute(
                        italic("pearson's") ~ italic(r) ~ "=" ~ value,
                        list(value = format(r, digits = 2))
                    )
                    anno <- c(anno, as.character(as.expression(anno_r)))
                } else if (item == "kendall") {
                    tau <- cor(dat[[x]], dat[[y]], method = "kendall")
                    anno_tau <- substitute(
                        italic("kendall's") ~ italic(tau) ~ "=" ~ value,
                        list(value = format(tau, digits = 2))
                    )
                    anno <- c(anno, as.character(as.expression(anno_tau)))
                } else if (item == "n") {
                    n <- nrow(dat)
                    anno_n <- substitute(
                        italic(N) ~ "=" ~ value,
                        list(value = n)
                    )
                    anno <- c(anno, as.character(as.expression(anno_n)))
                } else {
                    stop(
                        "Unknown annotation item: ",
                        item,
                        ". Expect: eq, r2, p, spearman, pearson, kendall, n"
                    )
                }
            }
            data.frame(anno = anno)
        })

    p <- ggplot(data, aes(x = !!sym(x), y = !!sym(y)))

    data$.highlight <- FALSE
    if (!is.null(highlight)) {
        if (isTRUE(highlight)) {
            data$.highlight <- TRUE
        } else if (length(highlight == 1 && is.character(highlight))) {
            data <- data %>% mutate(.highlight = eval(parse(text = highlight)))
        } else {
            all_inst <- rownames(data) %||% 1:nrow(data)
            if (!any(highlight %in% all_inst)) {
                stop("No highlight items found in the data (rownames).")
            }
            if (!all(highlight %in% all_inst)) {
                warning(
                    "Some highlight items not found in the data (rownames)."
                )
            }
            data$.highlight <- all_inst %in% highlight
            rm(all_inst)
        }
    }

    if (isTRUE(raster)) {
        # Normal points
        if (sum(data$.highlight == FALSE) > 0) {
            p <- p +
                scattermore::geom_scattermore(
                    data = data[data$.highlight == FALSE, , drop = FALSE],
                    aes(color = !!sym(group_by)),
                    pointsize = ceiling(pt_size),
                    alpha = alpha,
                    pixels = raster_dpi
                )
        }
        # Highlight points
        if (sum(data$.highlight) > 0) {
            hi_df <- data[data$.highlight, , drop = FALSE]
            p <- p +
                scattermore::geom_scattermore(
                    data = hi_df,
                    aes(x = !!sym(x), y = !!sym(y)),
                    color = highlight_color,
                    pointsize = floor(highlight_size) + highlight_stroke,
                    alpha = highlight_alpha,
                    pixels = raster_dpi,
                    inherit.aes = FALSE
                ) +
                scattermore::geom_scattermore(
                    data = hi_df,
                    aes(color = !!sym(group_by)),
                    pointsize = floor(highlight_size),
                    alpha = highlight_alpha,
                    pixels = raster_dpi
                )
            rm(hi_df)
        }
    } else {
        # Normal points
        p <- p +
            geom_point(
                aes(color = !!sym(group_by)),
                size = pt_size,
                shape = pt_shape,
                alpha = alpha
            )
        # Highlight points
        if (sum(data$.highlight) > 0) {
            hi_df <- data[data$.highlight, , drop = FALSE]
            p <- p +
                geom_point(
                    data = hi_df,
                    aes(x = !!sym(x), y = !!sym(y)),
                    color = highlight_color,
                    size = highlight_size + highlight_stroke,
                    shape = pt_shape,
                    alpha = highlight_alpha,
                    inherit.aes = FALSE
                ) +
                geom_point(
                    data = hi_df,
                    aes(color = !!sym(group_by)),
                    size = highlight_size,
                    shape = pt_shape,
                    alpha = highlight_alpha
                )
            rm(hi_df)
        }
    }

    anno_x <- ifelse(grepl("left", anno_position), -Inf, Inf)
    anno_y <- ifelse(grepl("top", anno_position), Inf, -Inf)
    p <- p +
        geom_smooth(
            method = "lm",
            formula = y ~ x,
            se = smooth_se,
            color = smooth_color,
            linewidth = smooth_width,
            alpha = 0.5
        ) +
        geom_text_repel(
            data = annodata,
            parse = TRUE,
            hjust = 0,
            direction = "y",
            aes(label = !!sym("anno")),
            x = anno_x,
            y = anno_y,
            seed = seed,
            size = text_size_scale * anno_size,
            bg.color = anno_bg,
            bg.r = anno_bg_r,
            color = anno_fg,
            force = 0.5,
            max.overlaps = 100,
            segment.color = "transparent"
        ) +
        scale_color_manual(
            name = group_name %||% group_by,
            values = palette_this(
                levels(data[[group_by]]),
                palette = palette,
                palcolor = palcolor,
                reverse = palreverse
            )
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
            legend.direction = legend.direction
        )

    dims <- calculate_plot_dimensions(
        base_height = 4.5,
        aspect.ratio = aspect.ratio,
        legend.position = legend.position,
        legend.direction = legend.direction,
        legend_n = nlevels(data[[group_by]]),
        legend_nchar = max(nchar(levels(data[[group_by]])))
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

#' Correlation scatter plot
#'
#' @description
#' Draws a scatter plot of two numeric variables with a linear regression
#' line, optional correlation statistics, and point highlighting.  This is
#' the public entry point that wraps \code{\link{CorPlotAtomic}} with
#' \code{split_by} support.
#'
#' Key features include \strong{group-based colouring} (\code{group_by}),
#' \strong{point highlighting} by expression, rowname, or index,
#' \strong{annotation items} (regression equation, R-squared, p-value,
#' Spearman/Pearson/Kendall rho, N), \strong{raster rendering} for large
#' datasets, \strong{faceting} (\code{facet_by}), and \strong{splitting}
#' into separate sub-plots via \code{split_by}.
#'
#' @section split_by workflow:
#' When \code{split_by} is provided:
#' \enumerate{
#'   \item The \code{split_by} column is validated via
#'         \code{\link{check_columns}()} with \code{force_factor = TRUE}.
#'         Empty levels are dropped (\code{droplevels()}).
#'   \item The data frame is split by \code{split_by} (preserving level
#'         order).  If \code{split_by} is \code{NULL}, the data is wrapped
#'         in a single-element list with name \code{"..."}.
#'   \item Per-split \code{palette}, \code{palcolor},
#'         \code{legend.position}, and \code{legend.direction} are resolved
#'         via \code{\link{check_palette}()}, \code{\link{check_palcolor}()},
#'         and \code{\link{check_legend}()}.
#'   \item \code{\link{CorPlotAtomic}()} is called for each split.  When
#'         \code{title} is a function, it receives the split level name and
#'         can generate dynamic titles.
#'   \item Results are combined via \code{\link{combine_plots}()} (when
#'         \code{combine = TRUE}) or returned as a named list.
#' }
#'
#' @inheritParams common_args
#' @inheritParams CorPlotAtomic
#' @param split_by The column(s) to split the data by and produce separate
#'  sub-plots.  Multiple columns are concatenated with \code{split_by_sep}.
#' @param split_by_sep A character string to separate concatenated
#'  \code{split_by} columns.  Default \code{"_"}.
#' @param seed A numeric seed for reproducibility.  Passed to
#'  \code{\link{validate_common_args}()}.
#' @param combine Logical; when \code{TRUE} (default), returns a combined
#'  \code{patchwork} object.  When \code{FALSE}, returns a named list of
#'  individual \code{ggplot} objects.
#' @param ncol,nrow Integer number of columns / rows for the combined
#'  layout (passed to \code{\link[patchwork]{wrap_plots}}).
#' @param byrow Logical; fill the combined layout by row.  Default
#'  \code{TRUE}.
#' @param axes A character string specifying how axes should be treated
#'  across the combined layout (passed to
#'  \code{\link[patchwork]{wrap_plots}}).
#' @param axis_titles A character string specifying how axis titles should
#'  be treated across the combined layout.  Defaults to \code{axes}.
#' @param guides A character string specifying how guides (legends) should
#'  be collected across panels (passed to \code{\link{combine_plots}()}).
#' @param design A custom layout design for the combined plot (passed to
#'  \code{\link{combine_plots}()}).
#' @return A \code{ggplot} object (when \code{split_by} is \code{NULL}),
#'  a \code{patchwork} object (when \code{combine = TRUE}), or a named
#'  list of \code{ggplot} objects (when \code{combine = FALSE}), each
#'  with \code{height} and \code{width} attributes in inches.
#' @export
#' @examples
#' \donttest{
#' data(iris)
#'
#' # Basic scatter with group colours
#' CorPlot(iris, "Sepal.Length", "Sepal.Width", group_by = "Species")
#'
#' # Highlight a specific group with custom stroke
#' CorPlot(iris, "Sepal.Length", "Sepal.Width", group_by = "Species",
#'     highlight = 'Species == "setosa"', highlight_stroke = 1.5,
#'     anno_items = c("eq", "pearson"), anno_position = "bottomright")
#'
#' # Faceted by species
#' CorPlot(iris, "Sepal.Length", "Sepal.Width", facet_by = "Species",
#'     facet_scales = "free")
#'
#' # Per-split palettes
#' CorPlot(iris, "Sepal.Length", "Sepal.Width", split_by = "Species",
#'     palette = c(setosa = "Set1", versicolor = "Dark2", virginica = "Paired"))
#' }
CorPlot <- function(
    data,
    x,
    y,
    group_by = NULL,
    group_by_sep = "_",
    group_name = NULL,
    split_by = NULL,
    split_by_sep = "_",
    pt_size = 2,
    pt_shape = 16,
    raster = FALSE,
    alpha = 1,
    raster_dpi = c(512, 512),
    highlight = NULL,
    highlight_color = "black",
    highlight_size = 1,
    highlight_alpha = 1,
    highlight_stroke = 0.8,
    anno_items = c("eq", "r2", "p"),
    anno_size = 3,
    anno_fg = "black",
    anno_bg = "white",
    anno_bg_r = 0.1,
    anno_position = c(
        "topleft",
        "topright",
        "bottomleft",
        "bottomright",
        "tl",
        "tr",
        "bl",
        "br"
    ),
    add_smooth = TRUE,
    smooth_color = "red2",
    smooth_width = 1.5,
    smooth_se = FALSE,
    theme = "theme_this",
    theme_args = list(),
    palette = ifelse(is.null(group_by), "Spectral", "Paired"),
    palcolor = NULL,
    palreverse = FALSE,
    title = NULL,
    subtitle = NULL,
    xlab = NULL,
    ylab = NULL,
    facet_by = NULL,
    facet_scales = "fixed",
    facet_ncol = NULL,
    facet_nrow = NULL,
    facet_byrow = TRUE,
    aspect.ratio = 1,
    legend.position = waiver(),
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
            CorPlotAtomic(
                data = datas[[nm]],
                x = x,
                y = y,
                group_by = group_by,
                group_by_sep = group_by_sep,
                group_name = group_name,
                pt_size = pt_size,
                pt_shape = pt_shape,
                raster = raster,
                raster_dpi = raster_dpi,
                highlight = highlight,
                highlight_color = highlight_color,
                highlight_size = highlight_size,
                highlight_alpha = highlight_alpha,
                highlight_stroke = highlight_stroke,
                anno_items = anno_items,
                anno_size = anno_size,
                anno_fg = anno_fg,
                anno_bg = anno_bg,
                anno_bg_r = anno_bg_r,
                anno_position = anno_position,
                add_smooth = add_smooth,
                smooth_color = smooth_color,
                smooth_width = smooth_width,
                smooth_se = smooth_se,
                theme = theme,
                theme_args = theme_args,
                palette = palette[[nm]],
                palcolor = palcolor[[nm]],
                palreverse = palreverse,
                alpha = alpha,
                title = title,
                subtitle = subtitle,
                xlab = xlab,
                ylab = ylab,
                facet_by = facet_by,
                facet_scales = facet_scales,
                facet_ncol = facet_ncol,
                facet_nrow = facet_nrow,
                facet_byrow = facet_byrow,
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

#' Atomic Correlation Pairs Plot
#'
#' @description
#' Core implementation for drawing a correlation pairs (scatterplot matrix)
#' grid.  This is the workhorse behind the exported
#' \code{\link{CorPairsPlot}} — it takes a \strong{single} data frame (no
#' \code{split_by} support) and returns a \code{patchwork} object.
#'
#' The grid arranges all pairwise scatter plots of selected columns.  The
#' upper or lower triangle displays correlation tiles (fill) while the
#' opposite triangle displays scatter plots with regression lines.  The
#' diagonal can show density plots, violin plots, histograms, box plots,
#' or a simple diagonal line.
#'
#' The function supports \strong{four layout orientations} (\code{layout}),
#' \strong{three correlation methods} (\code{cor_method}), configurable
#' diagonal plots using other plotthis functions, and custom correlation
#' tile formatting.
#'
#' @section Architecture:
#' \enumerate{
#'   \item \strong{ggplot dispatch} — selects \code{gglogger::ggplot} or
#'         \code{ggplot2::ggplot} based on
#'         \code{getOption("plotthis.gglogger.enabled")}.
#'   \item \strong{facet_by guard} — raises an error if \code{facet_by} is
#'         provided (not supported in pairs plots; use \code{split_by}
#'         instead).
#'   \item \strong{Column resolution} — when \code{columns} is \code{NULL},
#'         all columns (except \code{group_by}) are used.  Otherwise,
#'         \code{columns} are validated via \code{\link{check_columns}()}.
#'         At least two columns are required.
#'   \item \strong{Default diag_type} — when \code{diag_type} is \code{NULL},
#'         it defaults to \code{"density"} (no \code{group_by}) or
#'         \code{"violin"} (with \code{group_by}).
#'   \item \strong{Layout determination} — \code{get_plot_info()} is called
#'         for each cell \code{(i, j)} to determine:
#'         \itemize{
#'           \item \strong{Type}: \code{layout} (diagonal line),
#'                 \code{"cor"} (scatter plot with regression),
#'                 \code{"fill"} (correlation tile).
#'           \item \strong{Axis/label positions}: which sides of the cell
#'                 should show x-axis, y-axis, x-label, and y-label, based
#'                 on the cell's location in the matrix and the chosen
#'                 layout.
#'         }
#'   \item \strong{Cell rendering} — \code{get_plot()} draws each cell
#'         based on its \code{info$type}:
#'         \itemize{
#'           \item \strong{Diagonal, \code{diag_type = "none"}} — draws an
#'                 X or backslash diagonal line via \code{geom_line()}.
#'           \item \strong{Diagonal, \code{"density"}} — delegates to
#'                 \code{\link{DensityPlot}()}.
#'           \item \strong{Diagonal, \code{"violin"}} — delegates to
#'                 \code{\link{ViolinPlot}()}; requires \code{group_by}.
#'           \item \strong{Diagonal, \code{"histogram"}} — delegates to
#'                 \code{\link{Histogram}()}.
#'           \item \strong{Diagonal, \code{"box"}} — delegates to
#'                 \code{\link{BoxPlot}()}; requires \code{group_by}.
#'           \item \strong{Off-diagonal, \code{"cor"}} — delegates to
#'                 \code{\link{CorPlot}()} for the scatter plot with
#'                 regression line.
#'           \item \strong{Off-diagonal, \code{"fill"}} — draws a
#'                 \code{geom_tile()} filled by the correlation value
#'                 with \code{scale_fill_gradientn()} and displays the
#'                 formatted correlation via
#'                 \code{ggrepel::geom_text_repel()}.
#'         }
#'   \item \strong{Axis/label positioning} — axes, axis titles, and labels
#'         are positioned at the margins of the full grid based on
#'         \code{info$xaxis}, \code{info$yaxis}, \code{info$xlab},
#'         \code{info$ylab} using \code{scale_x_*}/\code{scale_y_*} with
#'         appropriate \code{position} arguments.
#'   \item \strong{Layout assembly} — all cell plots are arranged into a
#'         matrix via \code{patchwork::wrap_plots()} with
#'         \code{ncol = length(columns)}.  Guides are collected, and the
#'         title/subtitle are added via \code{plot_annotation()}.  The
#'         result is wrapped in \code{patchwork::wrap_elements()} so the
#'         title displays correctly.
#'   \item \strong{Dimension calculation} — \code{\link{calculate_plot_dimensions}()}
#'         computes \code{height} and \code{width} attributes from
#'         \code{base_height = sqrt(length(columns)) * 4}, a fixed aspect
#'         ratio of 1, and legend metrics.
#' }
#'
#' @inheritParams common_args
#' @param columns A character vector of column names to include in the
#'  pairs plot.  When \code{NULL} (default), all columns except
#'  \code{group_by} are used.  At least two columns are required.
#' @param group_by A character vector of column names to colour the
#'  scatter points by.  Each unique combination becomes a separate group.
#'  Required for \code{diag_type = "violin"} and \code{diag_type = "box"}.
#'  Multiple columns are concatenated with \code{group_by_sep}.
#' @param group_by_sep A character string to separate concatenated
#'  \code{group_by} columns.  Default \code{"_"}.
#' @param group_name A character string used as the colour legend title
#'  in the scatter plots.  When \code{NULL}, the \code{group_by} column
#'  name is used.
#' @param diag_type A character string specifying the plot type for
#'  diagonal cells.  One of \code{"density"}, \code{"violin"},
#'  \code{"histogram"}, \code{"box"}, or \code{"none"} (diagonal line).
#'  Default: \code{"density"} (no \code{group_by}) or \code{"violin"}
#'  (with \code{group_by}).
#' @param diag_args A named list of additional arguments passed to the
#'  diagonal plot function (\code{\link{DensityPlot}},
#'  \code{\link{ViolinPlot}}, \code{\link{Histogram}}, or
#'  \code{\link{BoxPlot}}).  Default: \code{list()}.
#' @param layout A character string specifying the layout orientation.
#'  One of the following codes (dot = scatter, backslash/slash =
#'  diagonal): \code{.\\}, \code{\\\.}, \code{/.}, \code{./}.
#'  Default: \code{.\\}.
#' @param cor_method A character string specifying the correlation method
#'  for the fill tiles.  One of \code{"pearson"}, \code{"spearman"},
#'  \code{"kendall"}.  Default: \code{"pearson"}.
#' @param cor_palette A character string specifying the colour palette for
#'  the correlation fill tiles.  Default: \code{"RdBu"}.
#' @param cor_palcolor A character vector of custom colours used to create
#'  the correlation tile palette.  When \code{NULL}, the palette's default
#'  colours are used.
#' @param cor_size A numeric value specifying the font size of the
#'  correlation text in the fill tiles.  Default: \code{3}.
#' @param cor_format A character string specifying a glue template for
#'  formatting the correlation text.  The template is evaluated by
#'  \code{glue::glue()} with access to \code{corr} (the correlation value),
#'  \code{x}, and \code{y} (the column names).  Default:
#'  \code{"corr: \\{round(corr, 2)\\}"}.
#' @param cor_fg A character string specifying the colour of the
#'  correlation text.  Default: \code{"black"}.
#' @param cor_bg A character string specifying the background colour of
#'  the correlation text boxes.  Default: \code{"white"}.
#' @param cor_bg_r A numeric value specifying the corner radius of the
#'  correlation text background boxes.  Default: \code{0.1}.
#' @param ... Additional arguments passed to the underlying
#'  \code{\link{CorPlot}} for the scatter plot cells.
#' @details \code{theme} and \code{theme_args} are supported and passed to
#'   each individual cell plot.
#' @return A \code{patchwork} object with \code{height} and \code{width}
#'  attributes (in inches) attached.
#' @importFrom ggplot2 waiver geom_tile element_blank scale_fill_gradientn guide_colorbar scale_x_continuous
#' @importFrom ggplot2 geom_line scale_y_continuous
#' @importFrom ggrepel geom_text_repel
#' @importFrom patchwork wrap_plots plot_layout plot_annotation
#' @keywords internal
CorPairsPlotAtomic <- function(
    data,
    columns = NULL,
    group_by = NULL,
    group_by_sep = "_",
    group_name = NULL,
    diag_type = NULL,
    diag_args = list(),
    layout = c(".\\", "\\.", "/.", "./"),
    cor_method = c("pearson", "spearman", "kendall"),
    cor_palette = "RdBu",
    cor_palcolor = NULL,
    cor_size = 3,
    cor_format = "corr: {round(corr, 2)}",
    cor_fg = "black",
    cor_bg = "white",
    cor_bg_r = 0.1,
    theme = "theme_this",
    theme_args = list(),
    palette = ifelse(is.null(group_by), "Spectral", "Paired"),
    palcolor = NULL,
    palreverse = FALSE,
    title = NULL,
    subtitle = NULL,
    facet_by = NULL,
    legend.position = "right",
    legend.direction = "vertical",
    seed = 8525,
    ...
) {
    set.seed(seed)
    ggplot <- if (getOption("plotthis.gglogger.enabled", FALSE)) {
        gglogger::ggplot
    } else {
        ggplot2::ggplot
    }
    if (!is.null(facet_by)) {
        stop(
            "'facet_by' is not supported in CorPairsPlot. Consider using 'split_by'."
        )
    }
    if (is.null(columns)) {
        if (!is.null(group_by)) {
            columns <- setdiff(colnames(data), group_by)
        } else {
            columns <- colnames(data)
        }
    } else {
        columns <- check_columns(data, columns, allow_multi = TRUE)
    }
    if (length(columns) < 2) {
        stop("At least two columns are required.")
    }
    if (is.null(diag_type)) {
        diag_type <- ifelse(is.null(group_by), "density", "violin")
    }
    diag_type <- match.arg(
        diag_type,
        c("density", "violin", "histogram", "box", "none")
    )
    layout <- match.arg(layout)
    cor_method <- match.arg(cor_method)
    base_size <- theme_args$base_size %||% 12
    text_size_scale <- base_size / 12

    # get the plot info (plotype, axis position, etc.) based on the layout
    get_plot_info <- function(i, j, j1) {
        diag_no_x_axis <- diag_type %in% c("box", "violin", "none")
        xaxis <- yaxis <- xlab <- ylab <- "none"
        if (i == j1) {
            # diagonal
            if (layout %in% c(".\\", "/.")) {
                if (i == 1) {
                    # top corner
                    yaxis <- ifelse(
                        diag_type == "none",
                        "none",
                        ifelse(layout == ".\\", "left", "right")
                    )
                    xlab <- "top"
                } else if (i == length(columns)) {
                    # bottom corner
                    xaxis <- ifelse(diag_no_x_axis, "none", "bottom")
                    ylab <- ifelse(layout == ".\\", "right", "left")
                }
            } else {
                # layout %in% c("\\.", "./")
                if (i == 1) {
                    # top corner
                    xaxis <- ifelse(diag_no_x_axis, "none", "top")
                    xlab <- "none"
                    ylab <- ifelse(layout == "\\.", "left", "right")
                } else if (i == length(columns)) {
                    # bottom corner
                    yaxis <- ifelse(
                        diag_type == "none",
                        "none",
                        ifelse(layout == "\\.", "right", "left")
                    )
                    xlab <- "bottom"
                }
            }
            return(list(
                type = layout,
                xaxis = xaxis,
                yaxis = yaxis,
                xlab = xlab,
                ylab = ylab
            ))
        } else if (i < j1) {
            # upper triangle
            if (layout %in% c(".\\", "/.")) {
                xlab <- ifelse(i > 1, "none", "top")
                ylab <- ifelse(
                    j1 < length(columns),
                    "none",
                    ifelse(layout == "/.", "left", "right")
                )
                return(list(
                    type = "fill",
                    xaxis = xaxis,
                    yaxis = yaxis,
                    xlab = xlab,
                    ylab = ylab
                ))
            } else {
                # layout %in% c("\\.", "./")
                xaxis <- ifelse(i > 1, "none", "top")
                yaxis <- ifelse(
                    j1 < length(columns),
                    "none",
                    ifelse(layout == "\\.", "right", "left")
                )
                return(list(
                    type = "cor",
                    xaxis = xaxis,
                    yaxis = yaxis,
                    xlab = xlab,
                    ylab = ylab
                ))
            }
        } else {
            # i > j1, lower triangle
            if (layout %in% c(".\\", "/.")) {
                xaxis <- ifelse(i < length(columns), "none", "bottom")
                yaxis <- ifelse(
                    j1 > 1,
                    "none",
                    ifelse(layout == "/.", "right", "left")
                )
                return(list(
                    type = "cor",
                    xaxis = xaxis,
                    yaxis = yaxis,
                    xlab = xlab,
                    ylab = ylab
                ))
            } else {
                # layout %in% c("\\.", "./")
                xlab <- ifelse(i < length(columns), "none", "bottom")
                ylab <- ifelse(
                    j1 > 1,
                    "none",
                    ifelse(layout == "\\.", "left", "right")
                )
                return(list(
                    type = "fill",
                    xaxis = xaxis,
                    yaxis = yaxis,
                    xlab = xlab,
                    ylab = ylab
                ))
            }
        }
    }

    get_plot <- function(x, y, info) {
        # Use facet to add label
        expand <- c(0, 0)
        if (diag_type == "none") {
            if (info$type %in% c("./", "/.")) {
                p <- ggplot(
                    data.frame(x = c(0, 1), y = c(0, 1)),
                    aes(x = x, y = y)
                ) +
                    geom_line(color = "darkred", linewidth = 1.5) +
                    scale_x_continuous(expand = expand) +
                    scale_y_continuous(expand = expand)
            } else if (info$type %in% c(".\\", "\\.")) {
                p <- ggplot(
                    data.frame(x = c(0, 1), y = c(1, 0)),
                    aes(x = x, y = y)
                ) +
                    geom_line(color = "darkred", linewidth = 1.5) +
                    scale_x_continuous(expand = expand) +
                    scale_y_continuous(expand = expand)
            }
        } else if (info$type %in% c(".\\", "\\.", "/.", "./")) {
            info$type <- diag_type
        }
        if (info$type == "density") {
            # Use guides instead of legend.position to remove the legend
            # This way & theme(legend.position = ...) can work as expected
            args <- diag_args
            args$data <- data
            args$x <- x
            args$group_by <- group_by
            args$add_bars <- args$add_bars %||% FALSE
            args$palette <- args$palette %||% palette
            args$palcolor <- args$palcolor %||% palcolor
            p <- do_call(DensityPlot, args)
            if (
                is.null(group_by) ||
                    (identical(args$palette, palette) &&
                        identical(args$palcolor, palcolor))
            ) {
                p <- p + guides(fill = "none", color = "none")
            }
        } else if (info$type == "violin") {
            if (is.null(group_by)) {
                stop(
                    "No 'group_by' is specified for 'violin' plot on the diagnal cells. Consider using 'density' instead."
                )
            }
            args <- diag_args
            args$data <- data
            args$x <- group_by
            args$y <- x
            args$palette <- args$palette %||% palette
            args$palcolor <- args$palcolor %||% palcolor
            p <- do_call(ViolinPlot, args)
            if (
                identical(args$palette, palette) &&
                    identical(args$palcolor, palcolor)
            ) {
                p <- p + guides(fill = "none")
            }
        } else if (info$type == "histogram") {
            args <- diag_args
            args$data <- data
            args$x <- x
            args$group_by <- group_by
            args$palette <- args$palette %||% palette
            args$palcolor <- args$palcolor %||% palcolor
            p <- do_call(Histogram, args)
            if (
                is.null(group_by) ||
                    (identical(args$palette, palette) &&
                        identical(args$palcolor, palcolor))
            ) {
                p <- p + guides(fill = "none", color = "none")
            }
        } else if (info$type == "box") {
            if (is.null(group_by)) {
                stop(
                    "No 'group_by' is specified for 'box' plot on the diagnal cells. Consider using 'histogram' instead."
                )
            }
            args <- diag_args
            args$data <- data
            args$x <- group_by
            args$y <- x
            args$palette <- args$palette %||% palette
            args$palcolor <- args$palcolor %||% palcolor
            p <- do_call(BoxPlot, args)
            if (
                identical(args$palette, palette) &&
                    identical(args$palcolor, palcolor)
            ) {
                p <- p + guides(fill = "none")
            }
        } else if (info$type == "cor") {
            expand <- waiver()
            p <- CorPlot(
                data,
                x = x,
                y = y,
                group_by = group_by,
                group_name = group_name,
                palette = palette,
                palcolor = palcolor,
                legend.position = legend.position,
                legend.direction = legend.direction,
                seed = seed,
                ...
            )
            if (is.null(group_by)) {
                p <- p + guides(fill = "none", color = "none")
            }
        } else if (info$type == "fill") {
            corr <- cor(data[[x]], data[[y]], method = cor_method)
            # x, y for cor_format
            p <- ggplot(
                data.frame(i = 0.5, j = 0.5, x = y, y = x, corr = corr),
                aes(x = i, y = j, fill = corr)
            ) +
                geom_tile(width = 1, height = 1) +
                geom_text_repel(
                    mapping = aes(label = glue(cor_format)),
                    segment.color = "transparent",
                    force = 0,
                    color = cor_fg,
                    bg.color = cor_bg,
                    bg.r = cor_bg_r,
                    size = text_size_scale * cor_size,
                    seed = seed
                ) +
                scale_fill_gradientn(
                    colors = palette_this(
                        c(-1, 1),
                        palette = cor_palette,
                        palcolor = cor_palcolor,
                        reverse = palreverse
                    ),
                    limits = c(-1, 1),
                    breaks = c(-1, 0, 1),
                    labels = scales::number_format(accuracy = 0.1),
                    guide = guide_colorbar(
                        frame.colour = "black",
                        ticks.colour = "black",
                        title.hjust = 0
                    )
                ) +
                scale_x_continuous(expand = expand) +
                scale_y_continuous(expand = expand)
        }
        p <- p + do_call(theme, theme_args)

        suppressMessages({
            # place the axis and labels
            if (info$type %in% c("violin", "box")) {
                scale_x <- scale_x_discrete
                expand <- waiver()
            } else {
                scale_x <- scale_x_continuous
            }
            if (info$xlab == "top") {
                p <- p +
                    ggplot2::xlab(x) +
                    scale_x(position = "top", expand = expand) +
                    ggplot2::theme(
                        axis.title.x = element_textbox(face = "bold")
                    )
            } else if (info$xlab == "bottom") {
                p <- p +
                    ggplot2::xlab(x) +
                    scale_x(expand = expand) +
                    ggplot2::theme(
                        axis.title.x = element_textbox(face = "bold")
                    )
            } else {
                p <- p + ggplot2::theme(axis.title.x = element_blank())
            }
            if (info$ylab == "left") {
                p <- p +
                    ggplot2::ylab(y) +
                    scale_y_continuous(expand = expand) +
                    ggplot2::theme(
                        axis.title.y = element_textbox(
                            orientation = "left-rotated"
                        )
                    )
            } else if (info$ylab == "right") {
                p <- p +
                    ggplot2::ylab(y) +
                    scale_y_continuous(position = "right", expand = expand) +
                    ggplot2::theme(
                        axis.title.y = element_textbox(
                            orientation = "right-rotated"
                        )
                    )
            } else {
                p <- p + ggplot2::theme(axis.title.y = ggplot2::element_blank())
            }
            if (info$xaxis == "bottom") {
                p <- p + scale_x(expand = expand)
            } else if (info$xaxis == "top") {
                p <- p + scale_x(position = "top", expand = expand)
            } else {
                p <- p +
                    ggplot2::theme(
                        axis.text.x = ggplot2::element_blank(),
                        axis.ticks.x = ggplot2::element_blank()
                    )
            }
            if (info$yaxis == "left") {
                p <- p + scale_y_continuous(expand = expand)
            } else if (info$yaxis == "right") {
                p <- p + scale_y_continuous(position = "right", expand = expand)
            } else {
                p <- p +
                    ggplot2::theme(
                        axis.text.y = ggplot2::element_blank(),
                        axis.ticks.y = ggplot2::element_blank()
                    )
            }
        })
        p + ggplot2::theme(plot.margin = margin(1, 1, 1, 1), aspect.ratio = 1)
    }

    plots <- list()
    for (i in seq_along(columns)) {
        for (j in seq_along(columns)) {
            j1 <- ifelse(layout %in% c("./", "/."), length(columns) - j + 1, j)
            info <- get_plot_info(i, j, j1)
            # For debugging
            # info$i <- i
            # info$j <- j
            # info$j1 <- j1
            # str(info)
            p <- get_plot(columns[j1], columns[i], info)
            plots <- c(plots, list(p))
        }
    }

    p <- wrap_plots(plots, ncol = length(columns), byrow = TRUE) +
        plot_layout(guides = "collect") +
        plot_annotation(title = title, subtitle = subtitle) &
        ggplot2::theme(
            legend.position = legend.position,
            legend.direction = legend.direction
        )
    # so that the plot title can be displayed
    p <- patchwork::wrap_elements(p)

    dims <- calculate_plot_dimensions(
        base_height = sqrt(length(columns)) * 4,
        aspect.ratio = 1, # pairs plot panels are always square
        legend.position = legend.position,
        legend.direction = legend.direction,
        legend_n = if (!is.null(group_by)) nlevels(data[[group_by]]) else 1,
        legend_nchar = if (!is.null(group_by)) {
            max(nchar(levels(data[[group_by]])))
        } else {
            5
        }
    )

    attr(p, "height") <- dims$height
    attr(p, "width") <- dims$width

    p$data <- data
    p
}

#' Correlation pairs (scatterplot matrix)
#'
#' @description
#' Draws a grid of pairwise scatter plots for selected numeric columns,
#' arranged in a scatterplot matrix layout.  The upper or lower triangle
#' displays correlation tiles while the opposite triangle shows scatter
#' plots with regression lines.  Diagonal cells can show density plots,
#' violin plots, histograms, box plots, or a simple diagonal line.
#'
#' \strong{NOTE:} The \code{facet_by} parameter is \strong{not supported}
#' in CorPairsPlot (an error is raised if provided).  Use
#' \code{split_by} instead to create separate correlation pair matrices
#' per group.
#'
#' The function supports \strong{four layout orientations} (\code{layout}),
#' \strong{three correlation methods}, configurable diagonal plots via
#' other plotthis functions, custom correlation tile formatting, and
#' splitting into separate sub-plots via \code{split_by}.
#'
#' @section split_by workflow:
#' When \code{split_by} is provided:
#' \enumerate{
#'   \item The \code{split_by} column is validated via
#'         \code{\link{check_columns}()} with \code{force_factor = TRUE}.
#'         Empty levels are dropped.
#'   \item The data frame is split by \code{split_by} (preserving level
#'         order).  If \code{split_by} is \code{NULL}, the data is wrapped
#'         in a single-element list with name \code{"..."}.  The
#'         \code{split_by} column is removed from each split's data before
#'         plotting.
#'   \item Per-split \code{palette}, \code{palcolor},
#'         \code{legend.position}, and \code{legend.direction} are resolved
#'         via \code{\link{check_palette}()}, \code{\link{check_palcolor}()},
#'         and \code{\link{check_legend}()}.
#'   \item \code{\link{CorPairsPlotAtomic}()} is called for each split.
#'         When \code{title} is a function, it receives the split level
#'         name and can generate dynamic titles.
#'   \item Results are combined via \code{\link{combine_plots}()} (when
#'         \code{combine = TRUE}) or returned as a named list.
#' }
#'
#' @inheritParams common_args
#' @inheritParams CorPairsPlotAtomic
#' @param split_by The column(s) to split the data by and produce separate
#'  sub-plots.  Multiple columns are concatenated with \code{split_by_sep}.
#' @param split_by_sep A character string to separate concatenated
#'  \code{split_by} columns.  Default \code{"_"}.
#' @param seed A numeric seed for reproducibility.
#' @param combine Logical; when \code{TRUE} (default), returns a combined
#'  \code{patchwork} object.  When \code{FALSE}, returns a named list of
#'  individual \code{patchwork} objects.
#' @param ncol,nrow Integer number of columns / rows for the combined
#'  layout.
#' @param byrow Logical; fill the combined layout by row.  Default
#'  \code{TRUE}.
#' @param axes A character string specifying how axes should be treated
#'  across the combined layout.
#' @param axis_titles A character string specifying how axis titles should
#'  be treated across the combined layout.  Defaults to \code{axes}.
#' @param guides A character string specifying how guides (legends) should
#'  be collected across panels.
#' @param design A custom layout design for the combined plot.
#' @return A \code{patchwork} object (when \code{combine = TRUE}) or a
#'  named list of \code{patchwork} objects (when \code{combine = FALSE}),
#'  each with \code{height} and \code{width} attributes in inches.
#' @export
#' @importFrom glue glue
#' @examples
#' \donttest{
#' set.seed(8525)
#' data <- data.frame(x = rnorm(100))
#' data$y <- rnorm(100, 10, sd = 0.5)
#' data$z <- -data$x + data$y + rnorm(100, 20, 1)
#' data$g <- sample(1:4, 100, replace = TRUE)
#'
#' # Histogram diagonal, slash layout
#' CorPairsPlot(data, diag_type = "histogram",
#'     diag_args = list(bins = 30, palette = "Paired"),
#'     layout = "/.")
#'
#' # No diagonal with axis title styling
#' CorPairsPlot(data, group_by = "g", diag_type = "none", layout = "./",
#'     theme_args = list(axis.title = element_textbox(
#'         color = "black", box.color = "grey20", size = 16, halign = 0.5,
#'         fill = "grey90", linetype = 1,
#'         width = grid::unit(1, "npc"),
#'         padding = ggplot2::margin(5, 5, 5, 5))))
#'
#' # Violin diagonal with custom format
#' CorPairsPlot(data, group_by = "g", diag_type = "violin", layout = "\\.",
#'     cor_format = "{x}\n{y}\ncorr: {round(corr, 2)}")
#'
#' # Per-split with bottom legend
#' CorPairsPlot(data, split_by = "g", diag_type = "none", layout = ".\\",
#'     legend.position = "bottom", legend.direction = "horizontal",
#'     group_name = "group")
#'
#' # Per-split with custom palette colours
#' CorPairsPlot(data, split_by = "g",
#'     palcolor = list("1" = "red", "2" = "blue", "3" = "green",
#'                     "4" = "yellow"))
#' }
CorPairsPlot <- function(
    data,
    columns = NULL,
    group_by = NULL,
    group_by_sep = "_",
    group_name = NULL,
    split_by = NULL,
    split_by_sep = "_",
    diag_type = NULL,
    diag_args = list(),
    layout = c(".\\", "\\.", "/.", "./"),
    cor_method = c("pearson", "spearman", "kendall"),
    cor_palette = "RdBu",
    cor_palcolor = NULL,
    cor_size = 3,
    cor_format = "corr: {round(corr, 2)}",
    cor_fg = "black",
    cor_bg = "white",
    cor_bg_r = 0.1,
    theme = "theme_this",
    theme_args = list(),
    palette = ifelse(is.null(group_by), "Spectral", "Paired"),
    palcolor = NULL,
    palreverse = FALSE,
    title = NULL,
    subtitle = NULL,
    facet_by = NULL,
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
    validate_common_args(seed)
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
            if (!is.null(split_by)) {
                datas[[nm]][[split_by]] <- NULL
            }

            CorPairsPlotAtomic(
                data = datas[[nm]],
                columns = columns,
                group_by = group_by,
                group_by_sep = group_by_sep,
                group_name = group_name,
                diag_type = diag_type,
                diag_args = diag_args,
                layout = layout,
                cor_method = cor_method,
                cor_palette = cor_palette,
                cor_palcolor = cor_palcolor,
                cor_size = cor_size,
                cor_format = cor_format,
                cor_fg = cor_fg,
                cor_bg = cor_bg,
                cor_bg_r = cor_bg_r,
                theme = theme,
                theme_args = theme_args,
                palette = palette[[nm]],
                palcolor = palcolor[[nm]],
                palreverse = palreverse,
                title = title,
                subtitle = subtitle,
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
