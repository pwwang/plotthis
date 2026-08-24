#' Atomic rarefaction / extrapolation plot (internal)
#'
#' @description
#' Core implementation for drawing a single rarefaction (or extrapolation)
#' curve from biodiversity data.  This is the workhorse behind the exported
#' \code{\link{RarefactionPlot}} — it takes a **single** fortifed data frame
#' (produced by \code{\link[ggplot2]{fortify}()} on an \code{iNEXT} object)
#' and returns a \code{ggplot} object.
#'
#' The function renders three types of curves:
#' \enumerate{
#'   \item \strong{Sample-size-based} (\code{type = 1}) — species diversity
#'         as a function of sample size (number of individuals or sampling
#'         units), with rarefaction (interpolation) and extrapolation segments.
#'   \item \strong{Sample completeness} (\code{type = 2}) — sample coverage
#'         as a function of sample size.
#'   \item \strong{Coverage-based} (\code{type = 3}) — species diversity as a
#'         function of sample coverage.
#' }
#'
#' Observed data points are marked with \code{geom_point()} (shape per group),
#' the rarefaction / extrapolation lines are drawn with \code{geom_line()}
#' using a solid/dashed linetype to distinguish the two phases, and
#' confidence intervals are rendered as semi-transparent ribbons when
#' \code{se = TRUE} and the fortifed data contains \code{y.lwr} / \code{y.upr}
#' columns.
#'
#' @section Architecture:
#' \enumerate{
#'   \item \strong{ggplot dispatch} — selects \code{gglogger::ggplot} or
#'         \code{ggplot2::ggplot} based on
#'         \code{getOption("plotthis.gglogger.enabled")}.
#'   \item \strong{Axis labels} — set according to the combination of
#'         \code{type} and the data's \code{datatype} attribute
#'         (\code{"abundance"} vs. \code{"incidence"}):
#'         \itemize{
#'           \item \code{type = 2}: x = "Number of individuals" or
#'                 "Number of sampling units", y = "Sample coverage".
#'           \item \code{type = 3 || 4}: x = "Sample coverage",
#'                 y = "Species diversity".
#'           \item \code{type = 1} (abundance): x = "Number of individuals",
#'                 y = "Species diversity".
#'           \item \code{type = 1} (incidence): x = "Number of sampling units",
#'                 y = "Species diversity".
#'         }
#'   \item \strong{Group name fallback} — if \code{group_name} is \code{NULL},
#'         it is set to the value of \code{group_by} (i.e. \code{"q"} or
#'         \code{"group"}).
#'   \item \strong{Base ggplot} — initialises \code{ggplot(data, aes(x, y,
#'         color = group_by))}.
#'   \item \strong{Observed data points} — \code{geom_point()} on rows where
#'         \code{Method == "Observed"}, with shape mapped to the group variable.
#'   \item \strong{Colour scale} — \code{scale_color_manual()} using
#'         \code{\link{palette_this}()}.  The legend is suppressed when
#'         \code{group_by} is the dummy \code{".group"}.
#'   \item \strong{Shape scale} — \code{scale_shape_discrete()} with the same
#'         legend-suppression logic.
#'   \item \strong{Rarefaction / extrapolation lines} — \code{geom_line()}
#'         with linetype mapped to the \code{lty} column (\code{"Rarefaction"}
#'         vs. \code{"Extrapolation"}).
#'   \item \strong{Linetype scale} — \code{scale_linetype_manual()} with
#'         \code{solid} and \code{dashed} values, legend key width set to
#'         1 cm.
#'   \item \strong{Theme and labels} — \code{do_call(theme, theme_args)},
#'         \code{panel.grid.major} (grey80 dashed), aspect ratio, legend
#'         position / direction, and title / subtitle.
#'   \item \strong{Confidence ribbon} — when \code{se = TRUE}, a
#'         \code{geom_ribbon(aes(ymin = y.lwr, ymax = y.upr, fill = group_by))}
#'         is added with the same per-group palette at transparency
#'         \code{alpha}.
#'   \item \strong{Dimension calculation} — \code{\link{calculate_plot_dimensions}()}
#'         computes \code{height} / \code{width} attributes from
#'         \code{base_height = 4.5}, \code{aspect.ratio}, and legend metrics.
#'   \item \strong{Faceting} — \code{\link{facet_plot}()} wraps the plot with
#'         \code{facet_wrap} / \code{facet_grid} if \code{facet_by} is
#'         provided.
#' }
#'
#' @inheritParams common_args
#' @param data A data frame produced by \code{\link[ggplot2]{fortify}()} on an
#'  \code{iNEXT} object.  Expected to contain columns \code{x} (sample size or
#'  coverage), \code{y} (diversity or coverage estimate), \code{Method}
#'  (\code{"Observed"}, \code{"Rarefaction"}, \code{"Extrapolation"}),
#'  \code{y.lwr} / \code{y.upr} (confidence bounds, optional), \code{datatype}
#'  (\code{"abundance"} or \code{"incidence"}), \code{group} (assemblage name,
#'  renamed from \code{Assemblage}), and \code{q} (diversity order, renamed
#'  from \code{Order.q}).
#' @param type An integer specifying the curve type: \code{1} for
#'  sample-size-based rarefaction/extrapolation, \code{2} for sample
#'  completeness, or \code{3} for coverage-based rarefaction/extrapolation.
#'  A vector of types can be passed and the data will be fortifed for all of
#'  them; faceting or splitting then separates the panels.  Default: \code{1}.
#' @param se A logical value indicating whether to display confidence intervals
#'  as semi-transparent ribbons around the estimated curve.  When \code{NULL}
#'  (the default), it resolves to \code{TRUE} if the fortifed data contains
#'  \code{y.lwr} and \code{y.upr} columns, and \code{FALSE} otherwise.
#' @param group_by A character vector specifying how to group the data for
#'  colouring the lines.  Must be one or both of \code{"q"} (diversity order)
#'  and \code{"group"} (assemblage/site).  Multiple values are concatenated
#'  with \code{group_by_sep}.  When \code{NULL}, a dummy \code{".group"}
#'  column is created and the legend is hidden.  Default: \code{"group"}.
#' @param group_name A character string used as the title for the colour
#'  (and shape) legend.  When \code{NULL} (the default), the value of
#'  \code{group_by} is used.
#' @param pt_size A numeric value specifying the size of the observed-data
#'  points.  Default: \code{3}.
#' @param line_width A numeric value specifying the width of the rarefaction /
#'  extrapolation lines.  Default: \code{1}.
#' @param palette A character string specifying the colour palette to use.
#'  A named list or vector can be used to specify palettes for different
#'  \code{split_by} levels in the exported \code{\link{RarefactionPlot}}.
#'  Default: \code{"Spectral"}.
#' @param alpha A numeric value between 0 and 1 specifying the transparency
#'  of the confidence-interval ribbon fill.  Default: \code{0.2}.
#' @param ... Additional arguments (currently unused in the atomic function;
#'  accepted for forward compatibility with the exported wrapper).
#' @keywords internal
#' @return A \code{ggplot} object with \code{height} and \code{width}
#'  attributes (in inches) attached.
#' @importFrom dplyr rename
#' @importFrom ggplot2 fortify geom_point geom_line geom_ribbon scale_color_manual scale_linetype_manual
#' @importFrom ggplot2 element_line element_text scale_shape_discrete unit
RarefactionPlotAtomic <- function(
    data,
    type = 1,
    se = TRUE,
    group_by = "group",
    group_name = NULL,
    pt_size = 3,
    line_width = 1,
    theme = "theme_this",
    theme_args = list(),
    palette = "Spectral",
    palcolor = NULL,
    palreverse = FALSE,
    alpha = 0.2,
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
    datatype <- unique(data$datatype)
    if (type == 2L) {
        if (identical(datatype, "abundance")) {
            xlab <- xlab %||% "Number of individuals"
            ylab <- ylab %||% "Sample coverage"
        } else {
            xlab <- xlab %||% "Number of sampling units"
            ylab <- ylab %||% "Sample coverage"
        }
    } else if (type == 3L || type == 4L) {
        xlab <- xlab %||% "Sample coverage"
        ylab <- ylab %||% "Species diversity"
    } else if (identical(datatype, "abundance")) {
        xlab <- xlab %||% "Number of individuals"
        ylab <- ylab %||% "Species diversity"
    } else {
        xlab <- xlab %||% "Number of sampling units"
        ylab <- ylab %||% "Species diversity"
    }

    group_name <- group_name %||% group_by
    p <- ggplot(
        data,
        aes(x = !!sym("x"), y = !!sym("y"), color = !!sym(group_by))
    ) +
        geom_point(
            aes(shape = !!sym(group_by)),
            size = pt_size,
            data = data[data$Method == "Observed", , drop = FALSE]
        ) +
        scale_color_manual(
            name = group_name,
            values = palette_this(
                levels(data[[group_by]]),
                palette = palette,
                palcolor = palcolor,
                reverse = palreverse
            ),
            guide = ifelse(identical(group_by, ".group"), "none", "legend")
        ) +
        scale_shape_discrete(
            name = group_name,
            guide = ifelse(identical(group_by, ".group"), "none", "legend")
        ) +
        geom_line(aes(linetype = !!sym("lty")), linewidth = line_width) +
        scale_linetype_manual(
            name = "",
            values = c("solid", "dashed"),
            # make items wider
            guide = guide_legend(
                theme = ggplot2::theme(legend.key.width = unit(1, "cm"))
            )
        ) +
        do_call(theme, theme_args) +
        ggplot2::theme(
            aspect.ratio = aspect.ratio,
            legend.position = legend.position,
            legend.direction = legend.direction,
            panel.grid.major = element_line(colour = "grey80", linetype = 2)
        ) +
        labs(title = title, subtitle = subtitle, x = xlab, y = ylab)

    if (isTRUE(se)) {
        p <- p +
            geom_ribbon(
                aes(
                    ymin = !!sym("y.lwr"),
                    ymax = !!sym("y.upr"),
                    fill = !!sym(group_by)
                ),
                color = "transparent",
                alpha = alpha
            ) +
            scale_fill_manual(
                name = group_name,
                values = palette_this(
                    levels(data[[group_by]]),
                    palette = palette,
                    palcolor = palcolor,
                    reverse = palreverse
                ),
                guide = ifelse(identical(group_by, ".group"), "none", "legend")
            )
    }

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


#' Rarefaction / extrapolation plot
#'
#' @description
#' Draws rarefaction and extrapolation curves for biodiversity data using the
#' \code{iNEXT} package.  Accepts raw species-abundance / incidence-frequency
#' lists (which are passed to \code{\link[iNEXT]{iNEXT}()} for estimation) or
#' pre-computed \code{iNEXT} objects.
#'
#' The function supports three curve types (sample-size-based, sample
#' completeness, and coverage-based), diversity orders (\code{q}), per-group
#' colouring, faceting, and splitting into separate sub-plots via
#' \code{split_by}.  Observed data are marked with points, rarefaction lines
#' are solid, and extrapolation segments are dashed.  Confidence intervals are
#' shown as semi-transparent ribbons.
#'
#' @section split_by workflow:
#' When \code{split_by} is provided:
#' \enumerate{
#'   \item \code{\link{validate_common_args}()} checks the \code{seed} and
#'         \code{facet_by} validity.
#'   \item The \code{type} argument is validated (must be one or more of
#'         1, 2, 3).
#'   \item \code{group_by}, \code{split_by}, and \code{facet_by} are validated
#'         for allowed values (\code{"q"} and/or \code{"group"}) and checked
#'         for mutual exclusivity — no parameter may overlap with another.
#'   \item If \code{data} is not an \code{iNEXT} object, it is passed to
#'         \code{\link[iNEXT]{iNEXT}()} with \code{...} (which may contain
#'         \code{q}, \code{datatype}, \code{nboot}, etc.).
#'   \item The \code{iNEXT} object is fortifed via
#'         \code{\link[ggplot2]{fortify}()} for the requested \code{type}s.
#'         Columns \code{Assemblage} and \code{Order.q} are renamed to
#'         \code{group} and \code{q}, respectively.
#'   \item The \code{se} parameter is resolved: if \code{NULL} it becomes
#'         \code{TRUE} when the fortifed data contains \code{y.lwr} /
#'         \code{y.upr} columns.
#'   \item A \code{lty} column is created (factor with levels
#'         \code{"Rarefaction"} and \code{"Extrapolation"}) to distinguish the
#'         two line phases via solid / dashed linetypes.
#'   \item \code{group_by}, \code{split_by}, and \code{facet_by} are processed
#'         via \code{\link{check_columns}()} with \code{force_factor = TRUE}
#'         and multi-column concatenation.
#'   \item If \code{group_by} is \code{NULL}, a dummy \code{".group"} column
#'         is created and the legend is hidden.
#'   \item The data is split by \code{split_by} (preserving level order).  If
#'         \code{split_by} is \code{NULL}, the data is wrapped in a
#'         single-element list with name \code{"..."}.
#'   \item Per-split \code{palette}, \code{palcolor},
#'         \code{legend.position}, and \code{legend.direction} are resolved
#'         via \code{\link{check_palette}()}, \code{\link{check_palcolor}()},
#'         and \code{\link{check_legend}()}.
#'   \item \code{\link{RarefactionPlotAtomic}()} is called for each split.
#'         If \code{title} is a function, it receives the split level name and
#'         can generate dynamic titles.
#'   \item Results are combined via \code{\link{combine_plots}()} (when
#'         \code{combine = TRUE}) or returned as a named list.
#' }
#'
#' @inheritParams common_args
#' @inheritParams RarefactionPlotAtomic
#' @param split_by A character vector specifying how to split the data into
#'  separate sub-plots.  Must be one or both of \code{"q"} (diversity order)
#'  and \code{"group"} (assemblage/site).  Multiple values are concatenated
#'  with \code{split_by_sep}.  Cannot overlap with \code{group_by} or
#'  \code{facet_by}.  Default: \code{NULL}.
#' @param split_by_sep A character string used to join multiple
#'  \code{split_by} column values when \code{split_by} has length > 1.
#'  Default: \code{"_"}.
#' @param group_by_sep A character string used to join multiple
#'  \code{group_by} column values when \code{group_by} has length > 1.
#'  Also used by the exported function for the group concatenation.
#'  Default: \code{"_"}.
#' @param seed A numeric seed for reproducibility.  Passed to
#'  \code{\link{validate_common_args}()}.  Default: \code{8525}.
#' @param combine Logical; when \code{TRUE} (default), returns a combined
#'  \code{patchwork} object.  When \code{FALSE}, returns a named list of
#'  individual \code{ggplot} objects.
#' @param ncol,nrow Integer number of columns / rows for the combined layout
#'  (passed to \code{\link[patchwork]{wrap_plots}}).
#' @param byrow Logical; fill the combined layout by row.  Default \code{TRUE}.
#' @param axes A character string specifying how axes should be treated across
#'  the combined layout (passed to \code{\link{combine_plots}()}).
#' @param axis_titles A character string specifying how axis titles should be
#'  treated across the combined layout.  Defaults to \code{axes}.
#' @param guides A character string specifying how guides (legends) should be
#'  collected across panels (passed to \code{\link{combine_plots}()}).
#' @param design A custom layout design for the combined plot (passed to
#'  \code{\link{combine_plots}()}).
#' @param ... Additional arguments passed to \code{\link[iNEXT]{iNEXT}} when
#'  \code{data} is not already an \code{iNEXT} object.  Common options include
#'  \code{q} (diversity order, default \code{c(0, 1, 2)}), \code{datatype}
#'  (\code{"abundance"} or \code{"incidence"}), and \code{nboot} (number of
#'  bootstrap replicates).
#' @return A \code{ggplot} object (single split), a \code{patchwork} object
#'  (multiple splits with \code{combine = TRUE}), or a named list of
#'  \code{ggplot} objects (when \code{combine = FALSE}), each with
#'  \code{height} and \code{width} attributes in inches.
#' @export
#' @examples
#' \donttest{
#' set.seed(8525)
#' spider <- list(
#'    Girdled = c(46, 22, 17, 15, 15, 9, 8, 6, 6, 4, rep(2, 4), rep(1, 12)),
#'    Logged = c(88, 22, 16, 15, 13, 10, 8, 8, 7, 7, 7, 5, 4, 4, 4, 3, 3, 3, 3,
#'      2, 2, 2, 2, rep(1, 14))
#' )
#'
#' # Basic sample-size-based rarefaction (type = 1)
#' RarefactionPlot(spider)
#'
#' # Multiple diversity orders with faceting
#' RarefactionPlot(spider, q = c(0, 1, 2), facet_by = "q")
#'
#' # Multiple diversity orders split into sub-plots
#' RarefactionPlot(spider, q = c(0, 1, 2), split_by = "q")
#'
#' # Per-split palettes
#' RarefactionPlot(spider, q = c(0, 1, 2), split_by = "q",
#'                 palette = c("0" = "Paired", "1" = "Set1", "2" = "Dark2"))
#'
#' # Coverage-based rarefaction (type = 3) with
#' # group_by = "q" and facet_by = "group"
#' RarefactionPlot(spider, q = c(0, 1, 2), group_by = "q",
#'  facet_by = "group", palette = "Set1", type = 3)
#' }
RarefactionPlot <- function(
    data,
    type = 1,
    se = NULL,
    group_by = "group",
    group_by_sep = "_",
    group_name = NULL,
    split_by = NULL,
    split_by_sep = "_",
    theme = "theme_this",
    theme_args = list(),
    palette = "Spectral",
    palcolor = NULL,
    palreverse = FALSE,
    alpha = 0.2,
    pt_size = 3,
    line_width = 1,
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
    stopifnot(
        "Invalid 'type' value. It must be 1, 2, or 3 or combination of them." = length(
            type
        ) >
            0 &&
            length(setdiff(type, 1:3)) == 0
    )
    # check group_by, split_by, facet_by.
    # If given, they should be "q" or "group". Only facet_by could be both.
    stopifnot(
        "Invalid 'group_by' value. It must be 'q' and/or 'group'." = is.null(
            group_by
        ) ||
            all(group_by %in% c("q", "group"))
    )
    stopifnot(
        "Invalid 'split_by' value. It must be 'q' and/or 'group'." = is.null(
            split_by
        ) ||
            all(split_by %in% c("q", "group"))
    )
    stopifnot(
        "Invalid 'facet_by' value. It must be 'q' and/or 'group'." = is.null(
            facet_by
        ) ||
            all(facet_by %in% c("q", "group"))
    )
    # They should not overlap with each other.
    # If they are not NULL, they should be different.
    stopifnot(
        "Invalid 'group_by'/'split_by'. They should not overlap." = is.null(
            group_by
        ) ||
            is.null(split_by) ||
            length(intersect(group_by, split_by)) == 0
    )
    stopifnot(
        "Invalid 'group_by'/'facet_by'. They should not overlap." = is.null(
            group_by
        ) ||
            is.null(facet_by) ||
            length(intersect(group_by, facet_by)) == 0
    )
    stopifnot(
        "Invalid 'split_by'/'facet_by'. They should not overlap." = is.null(
            split_by
        ) ||
            is.null(facet_by) ||
            length(intersect(split_by, facet_by)) == 0
    )

    if (!inherits(data, "iNEXT")) {
        data <- iNEXT::iNEXT(data, ...)
    }

    data <- suppressWarnings({
        fortify(data, type = type)
    })
    # rename Assemblage to group and Order.q to q
    data <- rename(data, group = "Assemblage", q = "Order.q")

    se <- se %||% ('y.lwr' %in% names(data))

    data$Method2 <- data$Method
    data$Method2[data$Method2 == "Observed"] <- "Rarefaction"
    data$lty <- factor(data$Method2, levels = c("Rarefaction", "Extrapolation"))

    group_by <- check_columns(
        data,
        group_by,
        force_factor = TRUE,
        allow_multi = TRUE,
        concat_multi = TRUE,
        concat_sep = group_by_sep
    )

    split_by <- check_columns(
        data,
        split_by,
        force_factor = TRUE,
        allow_multi = TRUE,
        concat_multi = TRUE,
        concat_sep = split_by_sep
    )

    facet_by <- check_columns(
        data,
        facet_by,
        force_factor = TRUE,
        allow_multi = TRUE
    )
    if (!is.null(facet_by)) {
        data[[facet_by]] <- droplevels(data[[facet_by]])
    }

    if (is.null(group_by)) {
        group_by <- ".group"
        data$.group <- factor("")
    }

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
                ifelse(
                    identical(split_by, "q"),
                    paste(split_by, nm, sep = " = "),
                    nm
                )
            }
            if (is.function(title)) {
                title <- title(default_title)
            } else {
                title <- title %||% default_title
            }
            RarefactionPlotAtomic(
                datas[[nm]],
                type = type,
                se = se,
                group_by = group_by,
                group_name = group_name,
                pt_size = pt_size,
                theme = theme,
                theme_args = theme_args,
                palette = palette[[nm]],
                palcolor = palcolor[[nm]],
                palreverse = palreverse,
                line_width = line_width,
                alpha = alpha,
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
