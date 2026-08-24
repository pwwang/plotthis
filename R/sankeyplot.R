#' Atomic Sankey / alluvial diagram (internal)
#'
#' @description
#' Core implementation for drawing a Sankey (alluvial) diagram using
#' \code{\link[ggalluvial]{geom_alluvium}} (or \code{\link[ggalluvial]{geom_flow}})
#' and \code{\link[ggalluvial]{geom_stratum}}.  This function takes a **single**
#' data frame (no \code{split_by} support) and returns a \code{ggplot} object
#' with faceting applied.
#'
#' The function supports five input formats (\code{in_form}) which control how
#' the data columns are interpreted.  Nodes (strata) are rendered as vertical
#' blocks whose fill colour and alpha can be customised independently of the
#' links (alluvia / flows) connecting them.  Link colours can match node
#' colours or use a separate palette; link borders can be set to a fixed colour
#' or to follow the fill colour (\code{links_color = ".fill"}).
#'
#' Automatic legend resolution determines whether nodes on different x-axis
#' positions receive a merged legend or separate legends, based on overlaps
#' between stratum values across positions.
#'
#' @section Architecture:
#' \enumerate{
#'   \item \strong{Format resolution} — \code{in_form} is matched and aliased
#'         (\code{"long"} → \code{"lodes"}, \code{"wide"} → \code{"alluvia"}).
#'   \item \strong{Data parsing by format} — one of five code paths executes:
#'   \itemize{
#'     \item \strong{Lodes / long} — \code{x}, \code{stratum}, \code{alluvium},
#'           and \code{links_fill_by} are validated via
#'           \code{\link{check_columns}()}.  Multi-column inputs are
#'           concatenated with their respective separators.  When \code{y} is
#'           \code{NULL}, counts are computed per (\code{x}, \code{stratum},
#'           \code{alluvium}, \code{links_fill_by}, \code{facet_by}).
#'     \item \strong{Counts} (x without \code{"."} prefix) — numeric \code{x}
#'           columns are pivoted to long form via
#'           \code{\link[tidyr]{pivot_longer}()}, creating the y-axis from the
#'           cell values.  \code{alluvium} and \code{stratum} default to
#'           \code{links_fill_by}.
#'     \item \strong{Counts with source node} (x with \code{"."} as first
#'           element, \code{is_flowcounts}) — same as counts but also injects
#'           \code{links_fill_by} values as an additional first column of
#'           nodes, visualising how flows originate from source groups.
#'     \item \strong{Alluvia / wide} — validated via
#'           \code{\link[ggalluvial]{is_alluvia_form}()} and converted to lodes
#'           form via \code{\link[ggalluvial]{to_lodes_form}()}.
#'           \code{stratum} and \code{alluvium} are ignored.
#'     \item \strong{Auto / fallback} — when no other branch matches, the lodes
#'           path is attempted and validated via
#'           \code{\link[ggalluvial]{is_lodes_form}()}.
#'   }
#'   \item \strong{Palette assignment} — \code{\link{palette_this}()} resolves
#'         colours for both nodes (\code{nodes_palette} / \code{nodes_palcolor})
#'         and links (\code{links_palette} / \code{links_palcolor}).
#'   \item \strong{Flow-counts guide logic} — when \code{is_flowcounts} is
#'         \code{TRUE}, the links guide is suppressed if the first-column node
#'         colours match the link colours, avoiding redundant legends.  When
#'         the palettes are identical but colours differ (too few colours in
#'         the palette), the first-column node colours are reused.
#'   \item \strong{Legend auto-detection} — when \code{nodes_legend = "auto"}:
#'         if \code{nodes_label = TRUE} or if \code{stratum} and
#'         \code{links_fill_by} share identical values and colours, the nodes
#'         legend is hidden.  Otherwise, overlapping stratum values across
#'         x-axis positions are checked: any overlap triggers a merged legend;
#'         no overlap produces separate legends per position.
#'   \item \strong{Base ggplot} — constructed with \code{aes(x = x,
#'         stratum = stratum, alluvium = alluvium, y = y)}.
#'   \item \strong{Separate node fill layers} — when \code{stratum} differs
#'         from \code{links_fill_by} and \code{nodes_legend = "separate"}, a
#'         \code{\link[ggplot2]{geom_col}()} +
#'         \code{\link[ggplot2]{scale_fill_manual}()} layer pair is added per
#'         x-axis position, each followed by
#'         \code{\link[ggnewscale]{new_scale_fill}()} to produce independent
#'         legends.
#'   \item \strong{Link rendering} — \code{\link[ggalluvial]{geom_alluvium}()}
#'         (default) or \code{\link[ggalluvial]{geom_flow}()} when
#'         \code{flow = TRUE}.  When \code{links_color = ".fill"}, the colour
#'         aesthetic is mapped to \code{links_fill_by} and the colour scale
#'         guide is suppressed.  For \code{geom_flow} with a distinct
#'         \code{stratum} and \code{links_fill_by}, \code{stat = "alluvium"}
#'         is forced to preserve the fill variable through the flow stat
#'         transformation.
#'   \item \strong{Link fill scale} — \code{\link[ggplot2]{scale_fill_manual}()}
#'         with \code{links_colors} and the resolved \code{links_guide}.
#'   \item \strong{Node rendering} — \code{\link[ggalluvial]{geom_stratum}()}
#'         with \code{nodes_color} (border) and \code{nodes_alpha}.  When
#'         \code{nodes_color = ".fill"}, the colour aesthetic maps to
#'         \code{stratum}.
#'   \item \strong{Node fill scale} — \code{\link[ggplot2]{scale_fill_manual}()}
#'         with \code{nodes_colors}.  The guide is \code{"none"} when
#'         \code{nodes_legend} is \code{"none"} or \code{"separate"}
#'         (already handled by per-position layers); otherwise \code{"legend"}.
#'   \item \strong{Labels} — when \code{nodes_label = TRUE},
#'         \code{\link[ggplot2]{geom_label}()} with
#'         \code{stat = \link[ggalluvial]{StatStratum}} and
#'         \code{min.y = nodes_label_miny}.
#'   \item \strong{Axes, theme, labels} — \code{\link[ggplot2]{scale_x_discrete}()},
#'         \code{\link[ggplot2]{scale_y_continuous}()}, custom theme, plot
#'         title / subtitle, axis labels, and aesthetic adjustments (aspect
#'         ratio, legend position / direction, grid removal, x-text angle).
#'   \item \strong{Coordinate flip} — when \code{flip = TRUE},
#'         \code{\link[ggplot2]{coord_flip}()} swaps the axes.
#'   \item \strong{Dimension calculation} — \code{\link{calculate_plot_dimensions}()}
#'         computes \code{height} and \code{width} attributes from the number
#'         of x-axis positions, legend metrics, and flip state, scaled by
#'         \code{aspect.ratio}.
#'   \item \strong{Faceting} — \code{\link{facet_plot}()} wraps the result
#'         when \code{facet_by} is provided.
#' }
#'
#' @inheritParams common_args
#' @param in_form A character string specifying the input data format.
#'  One of \code{"auto"} (default), \code{"long"}, \code{"lodes"},
#'  \code{"wide"}, \code{"alluvia"}, or \code{"counts"}.
#'  \code{"long"} is an alias for \code{"lodes"}; \code{"wide"} is an alias for
#'  \code{"alluvia"}.  See the \code{data} parameter of \code{\link{SankeyPlot}}
#'  for format descriptions.
#' @param x A character vector of column name(s) for the x-axis categories.
#'  Each unique value or concatenated pair represents a time point, state, or
#'  position along the horizontal axis.  Behaviour depends on \code{in_form}:
#'  for \code{"lodes"} at least one column is expected; for \code{"alluvia"}
#'  and \code{"counts"} at least two columns are required.  In the latter two
#'  cases \code{x_sep} is not used.
#' @param x_sep A character string to join multiple \code{x} columns when
#'  \code{in_form} is \code{"lodes"} or auto-determined as lodes.
#'  Default \code{"_"}.
#' @param y A character string specifying the numeric column for the y-axis
#'  (frequency / value).  When \code{NULL} (default), the count of observations
#'  per combination of \code{x}, \code{stratum}, \code{alluvium},
#'  \code{links_fill_by}, and \code{facet_by} is computed automatically.
#'  Ignored when \code{in_form} is \code{"counts"}.
#' @param stratum A character string specifying the column that defines the
#'  node categories at each x-axis position.  Each unique value becomes a
#'  stratum (node block) at each x position.  When \code{NULL}, defaults to
#'  \code{links_fill_by}.  Multiple columns are concatenated with
#'  \code{stratum_sep}.  Ignored in \code{"alluvia"} format.
#' @param stratum_sep A character string to join multiple \code{stratum}
#'  columns.  Default \code{"_"}.
#' @param alluvium A character string specifying the column that identifies
#'  individual flows (alluvia) across x-axis positions.  Each unique value
#'  represents a single observational unit tracked across positions.  When
#'  \code{NULL} in \code{"counts"} format, an auto-generated identifier is
#'  created.  Multiple columns are concatenated with \code{alluvium_sep}.
#'  Ignored in \code{"alluvia"} format.
#' @param alluvium_sep A character string to join multiple \code{alluvium}
#'  columns.  Default \code{"_"}.
#' @param flow A logical value.  When \code{FALSE} (default),
#'  \code{\link[ggalluvial]{geom_alluvium}()} is used for the links.  When
#'  \code{TRUE}, \code{\link[ggalluvial]{geom_flow}()} is used instead, which
#'  draws the flows with a directional gradient between x positions.
#' @param nodes_color A character string specifying the border colour of the
#'  node (stratum) rectangles.  Use the special value \code{".fill"} to match
#'  the border colour to the node fill colour.  Default \code{"grey30"}.
#' @param links_fill_by A character string specifying the column that
#'  determines the fill colour of the links (alluvia / flows).  When
#'  \code{NULL} in \code{"lodes"} format, defaults to \code{alluvium}.  In
#'  \code{"counts"} format with the \code{"."} prefix, this parameter is
#'  required.  Multiple columns are concatenated with
#'  \code{links_fill_by_sep}.
#' @param links_fill_by_sep A character string to join multiple
#'  \code{links_fill_by} columns.  Default \code{"_"}.
#' @param links_name A character string for the legend title of the link fill
#'  scale.  When \code{NULL} (default), the \code{links_fill_by} column name
#'  is used.
#' @param links_color A character string specifying the border colour of the
#'  links (alluvia / flows).  Use the special value \code{".fill"} to match
#'  the link border colour to the link fill colour.  Default \code{"gray80"}.
#' @param nodes_palette A character string specifying the colour palette for
#'  the node (stratum) fill.  Passed to \code{\link{palette_this}()}.
#'  Default \code{"Paired"}.
#' @param nodes_palcolor A character vector of custom colours for the node
#'  fill, used as \code{palcolor} in \code{\link{palette_this}()}.  When
#'  \code{NULL} (default), the palette colours are used directly.
#' @param nodes_alpha A numeric value in \eqn{[0, 1]} controlling the
#'  transparency of the node (stratum) fill.  Default \code{1}.
#' @param nodes_label A logical value.  When \code{TRUE}, stratum labels are
#'  drawn inside each node using \code{\link[ggplot2]{geom_label}()} with
#'  \code{\link[ggalluvial]{StatStratum}}.  Default \code{FALSE}.
#' @param nodes_label_miny A numeric value specifying the minimum y
#'  (frequency) threshold for displaying node labels.  Nodes with y-values
#'  below this threshold are not labelled.  Default \code{0}.
#' @param nodes_width A numeric value (typically 0–1) specifying the width of
#'  the node (stratum) rectangles as a fraction of the x-axis spacing.
#'  Default \code{0.25}.
#' @param nodes_legend Controls how the node legend is displayed.  One of:
#'  \describe{
#'    \item{\code{"auto"} (default)}{Automatically determined:
#'      if \code{nodes_label = TRUE}, or if \code{stratum} is identical to
#'      \code{links_fill_by} with matching colours, the legend is hidden.
#'      Otherwise, overlapping stratum values across x positions are checked:
#'      any overlap produces a merged legend; no overlap produces separate
#'      legends per x position.}
#'    \item{\code{"merge"}}{A single merged legend for all nodes.}
#'    \item{\code{"separate"}}{One legend per x-axis position, generated via
#'      separate \code{\link[ggplot2]{scale_fill_manual}()} layers.}
#'    \item{\code{"none"}}{No node legend is shown.}
#'  }
#' @param links_palette A character string specifying the colour palette for
#'  the link fill.  Passed to \code{\link{palette_this}()}.
#'  Default \code{"Paired"}.
#' @param links_palcolor A character vector of custom colours for the link
#'  fill, used as \code{palcolor} in \code{\link{palette_this}()}.  When
#'  \code{NULL} (default), the palette colours are used directly.
#' @param links_alpha A numeric value in \eqn{[0, 1]} controlling the
#'  transparency of the link fill.  Default \code{0.6}.
#' @param legend.box A character string specifying the arrangement of legend
#'  boxes, either \code{"vertical"} (default) or \code{"horizontal"}.
#' @param flip A logical value.  When \code{TRUE},
#'  \code{\link[ggplot2]{coord_flip}()} is applied to swap the x and y axes.
#'  Default \code{FALSE}.
#' @param ... Additional arguments passed to
#'  \code{\link[ggalluvial]{geom_alluvium}()} or
#'  \code{\link[ggalluvial]{geom_flow}()}, depending on the \code{flow}
#'  setting.  For \code{geom_flow} with a distinct \code{links_fill_by}
#'  column, passing \code{stat = "alluvium"} preserves the fill variable.
#' @return A \code{ggplot} object with \code{height} and \code{width}
#'  attributes (in inches) attached.
#' @keywords internal
#' @importFrom utils combn
#' @importFrom rlang sym syms %||% dots_n
#' @importFrom dplyr %>% group_by summarise n ungroup mutate add_count
#' @importFrom tidyr pivot_wider pivot_longer
#' @importFrom ggnewscale new_scale_fill
#' @importFrom ggplot2 geom_col scale_fill_manual geom_label after_stat scale_x_discrete scale_y_continuous labs coord_flip
SankeyPlotAtomic <- function(
    data,
    in_form = c("auto", "long", "lodes", "wide", "alluvia", "counts"),
    x,
    x_sep = "_",
    y = NULL,
    stratum = NULL,
    stratum_sep = "_",
    alluvium = NULL,
    alluvium_sep = "_",
    flow = FALSE,
    nodes_color = "grey30",
    links_fill_by = NULL,
    links_fill_by_sep = "_",
    links_name = NULL,
    links_color = "gray80",
    nodes_palette = "Paired",
    nodes_palcolor = NULL,
    palreverse = FALSE,
    nodes_alpha = 1,
    nodes_label = FALSE,
    nodes_width = 0.25,
    nodes_label_miny = 0,
    nodes_legend = c("auto", "separate", "merge", "none"),
    expand = c(0, 0, 0, 0),
    links_palette = "Paired",
    links_palcolor = NULL,
    links_alpha = 0.6,
    legend.box = "vertical",
    keep_empty = TRUE,
    x_text_angle = 0,
    aspect.ratio = 1,
    legend.position = "right",
    legend.direction = "vertical",
    flip = FALSE,
    theme = "theme_this",
    theme_args = list(),
    title = NULL,
    subtitle = NULL,
    xlab = NULL,
    ylab = NULL,
    facet_by = NULL,
    facet_scales = "fixed",
    facet_ncol = NULL,
    facet_nrow = NULL,
    facet_byrow = TRUE,
    ...
) {
    ggplot <- if (getOption("plotthis.gglogger.enabled", FALSE)) {
        gglogger::ggplot
    } else {
        ggplot2::ggplot
    }

    is_flowcounts <- FALSE
    nodes_legend <- match.arg(nodes_legend)
    in_form <- match.arg(in_form)
    if (in_form == "long") {
        in_form <- "lodes"
    }
    if (in_form == "wide") {
        in_form <- "alluvia"
    }
    if (in_form == "lodes") {
        x <- check_columns(
            data,
            x,
            force_factor = TRUE,
            allow_multi = TRUE,
            concat_multi = TRUE,
            concat_sep = x_sep
        )
        alluvium <- check_columns(
            data,
            alluvium,
            force_factor = TRUE,
            allow_multi = TRUE,
            concat_multi = TRUE,
            concat_sep = alluvium_sep
        )
        links_fill_by <- check_columns(
            data,
            links_fill_by,
            force_factor = TRUE,
            allow_multi = TRUE,
            concat_multi = TRUE,
            concat_sep = links_fill_by_sep
        ) %||%
            alluvium
        stratum <- check_columns(
            data,
            stratum,
            force_factor = TRUE,
            allow_multi = TRUE,
            concat_multi = TRUE,
            concat_sep = stratum_sep
        ) %||%
            links_fill_by
        if (is.null(y)) {
            data <- add_count(
                data,
                !!!syms(unique(c(
                    x,
                    stratum,
                    alluvium,
                    links_fill_by,
                    facet_by
                ))),
                name = ".y"
            )
            y <- ".y"
        }
    } else if (
        !identical(x[1], ".") &&
            (in_form == "counts" ||
                (in_form == "auto" &&
                    length(x) > 1 &&
                    all(sapply(data[, x, drop = FALSE], is.numeric))))
    ) {
        x <- check_columns(data, x, allow_multi = TRUE)
        # if (!is.null(stratum)) warning("[SankeyPlot] 'stratum' is ignored in 'counts' format.")
        if (!is.null(y)) {
            warning("[SankeyPlot] 'y' is ignored in 'counts' format.")
        }
        if (is.null(alluvium)) {
            alluvium <- "alluvium"
            data[[alluvium]] <- as.character(1:nrow(data))
        } else {
            alluvium <- check_columns(
                data,
                alluvium,
                force_factor = TRUE,
                allow_multi = TRUE,
                concat_multi = TRUE,
                concat_sep = alluvium_sep
            )
        }
        links_fill_by <- check_columns(
            data,
            links_fill_by,
            force_factor = TRUE,
            allow_multi = TRUE,
            concat_multi = TRUE,
            concat_sep = links_fill_by_sep
        ) %||%
            alluvium
        data <- pivot_longer(
            data,
            cols = x,
            names_to = "x",
            values_to = "Frequency"
        )
        data$x <- factor(data$x, levels = x)
        stratum <- check_columns(
            data,
            stratum,
            force_factor = TRUE,
            allow_multi = TRUE,
            concat_multi = TRUE,
            concat_sep = stratum_sep
        ) %||%
            links_fill_by
        x <- "x"
        y <- "Frequency"
    } else if (
        identical(x[1], ".") &&
            (in_form == "counts" ||
                (in_form == "auto" &&
                    length(x) > 1 &&
                    all(sapply(data[, x[-1], drop = FALSE], is.numeric))))
    ) {
        is_flowcounts <- TRUE
        x <- check_columns(data, x[-1], allow_multi = TRUE)
        stratum <- check_columns(
            data,
            stratum,
            force_factor = TRUE,
            allow_multi = TRUE,
            concat_multi = TRUE,
            concat_sep = stratum_sep
        )
        stratum_levels <- if (is.null(stratum)) {
            NULL
        } else {
            levels(data[[stratum]])
        }
        if (!is.null(y)) {
            warning("[SankeyPlot] 'y' is ignored in 'counts' format.")
        }
        if (is.null(alluvium)) {
            alluvium <- "alluvium"
            data[[alluvium]] <- as.character(1:nrow(data))
        } else {
            alluvium <- check_columns(
                data,
                alluvium,
                force_factor = TRUE,
                allow_multi = TRUE,
                concat_multi = TRUE,
                concat_sep = alluvium_sep
            )
        }
        links_fill_by <- check_columns(
            data,
            links_fill_by,
            force_factor = TRUE,
            allow_multi = TRUE,
            concat_multi = TRUE,
            concat_sep = links_fill_by_sep
        )
        stopifnot(
            "[SankeyPlot] The 'links_fill_by' must be provided in 'counts' format." = !is.null(
                links_fill_by
            )
        )
        data <- pivot_longer(
            data,
            cols = x,
            names_to = "x",
            values_to = "Frequency"
        )
        if (is.null(stratum)) {
            stratum <- links_fill_by
        } else {
            data[[stratum]] <- factor(data[[stratum]], levels = stratum_levels)
        }
        dotdata <- data[data$x == x[1], , drop = FALSE]
        dotdata$x <- links_fill_by
        dotdata[[stratum]] <- dotdata[[links_fill_by]]
        data <- rbind(data, dotdata)
        data$x <- factor(data$x, levels = unique(c(links_fill_by, x)))
        data[[stratum]] <- factor(
            data[[stratum]],
            levels = unique(c(levels(data[[links_fill_by]]), stratum_levels))
        )
        x <- "x"
        y <- "Frequency"
    } else if (
        in_form == "alluvia" ||
            (in_form == "auto" &&
                length(x) > 1 &&
                ggalluvial::is_alluvia_form(data, axes = x, weight = y))
    ) {
        x <- check_columns(data, x, force_factor = TRUE, allow_multi = TRUE)
        stopifnot(
            "[SankeyPlot] 'x' must be at least 2 columns in 'alluvia' format." = length(
                x
            ) >=
                2
        )
        if (!is.null(stratum)) {
            warning("[SankeyPlot] 'stratum' is ignored in 'alluvia' format.")
        }
        if (!is.null(alluvium)) {
            warning("[SankeyPlot] 'alluvium' is ignored in 'alluvia' format.")
        }
        links_fill_by <- check_columns(
            data,
            links_fill_by,
            force_factor = TRUE,
            allow_multi = TRUE,
            concat_multi = TRUE,
            concat_sep = links_fill_by_sep
        )
        if (is.null(y)) {
            data <- add_count(
                data,
                !!!syms(unique(c(x, links_fill_by, facet_by))),
                name = ".y"
            )
            y <- ".y"
        }
        # make a copy of links_fill_by in case it's one of x or alluvium that gets transformed later
        if (!is.null(links_fill_by) && links_fill_by %in% x) {
            is_flowcounts <- identical(links_fill_by, x[1])
            data <- ggalluvial::to_lodes_form(
                data,
                axes = x,
                diffuse = !!sym(links_fill_by)
            )
        } else {
            data <- ggalluvial::to_lodes_form(data, axes = x)
        }
        x <- "x"
        alluvium <- "alluvium"
        stratum <- "stratum"
    } else {
        # maybe lodes
        x <- check_columns(
            data,
            x,
            force_factor = TRUE,
            allow_multi = TRUE,
            concat_multi = TRUE,
            concat_sep = x_sep
        )
        alluvium <- check_columns(
            data,
            alluvium,
            force_factor = TRUE,
            allow_multi = TRUE,
            concat_multi = TRUE,
            concat_sep = alluvium_sep
        )
        links_fill_by <- check_columns(
            data,
            links_fill_by,
            force_factor = TRUE,
            allow_multi = TRUE,
            concat_multi = TRUE,
            concat_sep = links_fill_by_sep
        ) %||%
            alluvium
        stratum <- check_columns(
            data,
            stratum,
            force_factor = TRUE,
            allow_multi = TRUE,
            concat_multi = TRUE,
            concat_sep = stratum_sep
        ) %||%
            links_fill_by
        if (
            !ggalluvial::is_lodes_form(
                data,
                key = x,
                value = stratum,
                id = alluvium
            )
        ) {
            stop(
                "[SankeyPlot] 'data' must be in 'lodes/long' or 'alluvia/wide' format."
            )
        }
        if (is.null(y)) {
            data <- add_count(
                data,
                !!!syms(unique(c(
                    x,
                    stratum,
                    alluvium,
                    links_fill_by,
                    facet_by
                ))),
                name = ".y"
            )
            y <- ".y"
        }
    }

    nodes_colors <- palette_this(
        levels(data[[stratum]]),
        palette = nodes_palette,
        palcolor = nodes_palcolor,
        reverse = palreverse
    )
    links_colors <- palette_this(
        levels(data[[links_fill_by]]),
        palette = links_palette,
        palcolor = links_palcolor,
        reverse = palreverse
    )
    links_guide <- guide_legend(
        order = 1,
        override.aes = list(
            alpha = min(links_alpha + 0.2, 1),
            color = "transparent"
        )
    )
    if (is_flowcounts) {
        if (identical(nodes_colors[names(links_colors)], links_colors)) {
            links_guide <- "none"
        } else if (
            identical(links_palette, nodes_palette) &&
                identical(links_palcolor, nodes_palcolor)
        ) {
            links_guide <- "none"
            # Plotting the flow of the first column of nodes. Links guide is still showing because
            # the first column of nodes have different colors as the links. It is probably because nodes_palette
            # does not have enough colors. Please use a palette with more colors for both
            # nodes_palette and links_palette.
            nodes_colors1 <- palette_this(
                levels(data[[links_fill_by]]),
                palette = nodes_palette,
                palcolor = nodes_palcolor,
                reverse = palreverse
            )
            nodes_colors <- c(
                nodes_colors1,
                nodes_colors[setdiff(names(nodes_colors), names(nodes_colors1))]
            )
        }
    }

    just <- calc_just(x_text_angle)
    base_size <- theme_args$base_size %||% 12
    text_size_scale <- base_size / 12
    expand <- norm_expansion(expand, x_type = "discrete", y_type = "continuous")
    if (nodes_legend == "auto") {
        if (
            isTRUE(nodes_label) ||
                (identical(stratum, links_fill_by) &&
                    identical(nodes_colors, links_colors))
        ) {
            nodes_legend <- "none"
        } else {
            stratum_values <- lapply(levels(data[[x]]), function(xval) {
                as.character(unique(data[
                    data[[x]] == xval,
                    stratum,
                    drop = TRUE
                ]))
            })
            idxes <- combn(seq_along(stratum_values), 2)
            nodes_legend <- ifelse(
                !is_flowcounts &&
                    any(sapply(
                        as.data.frame(idxes),
                        function(idx) {
                            length(intersect(
                                stratum_values[[idx[1]]],
                                stratum_values[[idx[2]]]
                            )) >
                                0
                        }
                    )),
                "merge",
                "separate"
            )
        }
    }

    p <- ggplot(
        data = data,
        aes(
            x = !!sym(x),
            stratum = !!sym(stratum),
            alluvium = !!sym(alluvium),
            y = !!sym(y)
        )
    )

    # fill nodes on each x so they can have different legends/guides
    # but when stratum and alluvium are the same, they should be filled with the same palette
    if (!identical(stratum, links_fill_by) && nodes_legend == "separate") {
        xs <- levels(data[[x]])
        for (i in seq_along(xs)) {
            xdf <- filter(data, !!sym(x) == xs[i])
            xdf <- xdf[order(xdf[[stratum]]), , drop = FALSE]
            p <- p +
                geom_col(
                    data = xdf,
                    inherit.aes = FALSE,
                    aes(x = !!sym(x), fill = !!sym(stratum), y = 0),
                    width = 0
                ) +
                scale_fill_manual(
                    name = xs[i],
                    values = nodes_colors,
                    breaks = unique(xdf[[stratum]]),
                    guide = guide_legend(
                        order = i + 1,
                        override.aes = list(alpha = min(nodes_alpha + 0.2, 1))
                    )
                ) +
                new_scale_fill()
        }
    }

    if (!isTRUE(flow)) {
        if (identical(links_color, ".fill")) {
            p <- p +
                ggalluvial::geom_alluvium(
                    aes(
                        fill = !!sym(links_fill_by),
                        color = !!sym(links_fill_by)
                    ),
                    width = nodes_width,
                    alpha = links_alpha,
                    na.rm = !keep_empty,
                    ...
                ) +
                scale_color_manual(guide = "none", values = links_colors)
        } else {
            p <- p +
                ggalluvial::geom_alluvium(
                    aes(fill = !!sym(links_fill_by)),
                    width = nodes_width,
                    alpha = links_alpha,
                    color = links_color,
                    na.rm = !keep_empty,
                    ...
                )
        }
    } else {
        if (identical(links_color, ".fill")) {
            # stratum changed to "stratum" after flow stat
            if (identical(stratum, links_fill_by)) {
                p <- p +
                    ggalluvial::geom_flow(
                        aes(
                            fill = after_stat(!!sym("stratum")),
                            color = after_stat(!!sym("stratum"))
                        ),
                        width = nodes_width,
                        alpha = links_alpha,
                        na.rm = !keep_empty,
                        ...
                    ) +
                    scale_color_manual(guide = "none", values = links_colors)
            } else {
                p <- p +
                    ggalluvial::geom_flow(
                        aes(
                            fill = !!sym(links_fill_by),
                            color = !!sym(links_fill_by)
                        ),
                        width = nodes_width,
                        alpha = links_alpha,
                        na.rm = !keep_empty,
                        ...
                    ) +
                    scale_color_manual(guide = "none", values = links_colors)
            }
        } else {
            if (identical(stratum, links_fill_by)) {
                p <- p +
                    ggalluvial::geom_flow(
                        aes(fill = after_stat(!!sym("stratum"))),
                        width = nodes_width,
                        alpha = links_alpha,
                        color = links_color,
                        na.rm = !keep_empty,
                        ...
                    ) +
                    scale_color_manual(guide = "none", values = links_colors)
            } else if (dots_n(...) == 0 || !"stat" %in% names(list(...))) {
                p <- p +
                    ggalluvial::geom_flow(
                        aes(fill = !!sym(links_fill_by)),
                        width = nodes_width,
                        alpha = links_alpha,
                        color = links_color,
                        na.rm = !keep_empty,
                        stat = "alluvium",
                        ...
                    )
            } else {
                warning(
                    "[SankeyPlot] You probably see no color filling for the links. ",
                    paste0(
                        "This is because 'flow' stat of ggalluvial::geom_flow loses '",
                        links_fill_by,
                        "' "
                    ),
                    "while building the plot. Please use 'stat = 'alluvium' instead."
                )
                p <- p +
                    ggalluvial::geom_flow(
                        aes(fill = !!sym(links_fill_by)),
                        width = nodes_width,
                        alpha = links_alpha,
                        color = links_color,
                        na.rm = !keep_empty,
                        ...
                    )
            }
        }
    }

    p <- p +
        scale_fill_manual(
            name = links_name %||% links_fill_by,
            values = links_colors,
            breaks = levels(data[[links_fill_by]]),
            guide = links_guide
        ) +
        new_scale_fill()

    if (identical(nodes_color, ".fill")) {
        p <- p +
            ggalluvial::geom_stratum(
                aes(fill = !!sym(stratum), color = !!sym(stratum)),
                alpha = nodes_alpha,
                width = nodes_width,
                na.rm = !keep_empty
            ) +
            scale_color_manual(guide = "none", values = nodes_colors)
    } else {
        p <- p +
            ggalluvial::geom_stratum(
                aes(fill = !!sym(stratum)),
                alpha = nodes_alpha,
                width = nodes_width,
                color = nodes_color,
                na.rm = !keep_empty
            )
    }

    p <- p +
        scale_fill_manual(
            values = nodes_colors,
            breaks = levels(data[[stratum]]),
            guide = ifelse(
                nodes_legend %in% c("none", "separate"),
                "none",
                "legend"
            )
        )

    if (isTRUE(nodes_label)) {
        p <- p +
            geom_label(
                aes(label = !!sym(stratum)),
                stat = ggalluvial::StatStratum,
                min.y = nodes_label_miny,
                size = text_size_scale * 3
            )
    }

    p <- p +
        scale_x_discrete(expand = expand$x) +
        scale_y_continuous(expand = expand$y) +
        do_call(theme, theme_args) +
        labs(
            title = title,
            subtitle = subtitle,
            x = xlab %||% x,
            y = ylab %||% ifelse(identical(y, ".y"), links_fill_by, "Frequency")
        ) +
        ggplot2::theme(
            aspect.ratio = aspect.ratio,
            legend.position = legend.position,
            legend.direction = legend.direction,
            legend.box = legend.box,
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            axis.ticks.x = element_blank(),
            axis.text.x = element_text(
                angle = x_text_angle,
                hjust = just$h,
                vjust = just$v
            )
        )

    if (isTRUE(flip)) {
        p <- p + coord_flip()
        dims <- calculate_plot_dimensions(
            base_height = 6,
            aspect.ratio = aspect.ratio,
            n_y = nlevels(data[[x]]),
            y_scale_factor = 1.5,
            legend.position = legend.position,
            legend.direction = legend.direction,
            legend_n = length(unique(c(
                levels(data[[stratum]]),
                levels(data[[links_fill_by]])
            ))),
            legend_nchar = max(nchar(unique(c(
                levels(data[[stratum]]),
                levels(data[[links_fill_by]])
            )))),
            flip = TRUE
        )
        attr(p, "height") <- dims$height
        attr(p, "width") <- dims$width
    } else {
        dims <- calculate_plot_dimensions(
            base_height = 6,
            aspect.ratio = aspect.ratio,
            n_x = nlevels(data[[x]]),
            x_scale_factor = 1.5,
            legend.position = legend.position,
            legend.direction = legend.direction,
            legend_n = length(unique(c(
                levels(data[[stratum]]),
                levels(data[[links_fill_by]])
            ))),
            legend_nchar = max(nchar(unique(c(
                levels(data[[stratum]]),
                levels(data[[links_fill_by]])
            ))))
        )
        attr(p, "height") <- dims$height
        attr(p, "width") <- dims$width
    }

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


#' Sankey / Alluvial Plot
#'
#' @description
#' Draws Sankey (alluvial) diagrams to visualise flow, movement, or change
#' from one categorical state to another across discrete positions (time
#' points, stages, or groups).  The plot consists of \strong{nodes} (vertical
#' blocks, or strata) representing categories at each position, and
#' \strong{links} (alluvia / flows) representing the observation units that
#' move between categories across positions.
#'
#' The function accepts data in several formats, controlled by \code{in_form}:
#' \describe{
#'   \item{\code{"lodes"} / \code{"long"}}{Each row is an observation at one
#'     x-position, with columns for \code{x}, \code{stratum}, \code{alluvium},
#'     and optionally \code{y}.}
#'   \item{\code{"alluvia"} / \code{"wide"}}{Each row is an observation unit
#'     tracked across all positions; \code{x} columns represent the categories
#'     at each position.  Converted internally via
#'     \code{\link[ggalluvial]{to_lodes_form}()}.}
#'   \item{\code{"counts"}}{Numeric columns under each \code{x} represent
#'     frequencies.  When the first element of \code{x} is \code{"."}, the
#'     \code{links_fill_by} values are injected as an additional first column
#'     of nodes, visualising the source distribution of flows.}
#'   \item{\code{"auto"} (default)}{Automatically detects the format:
#'     numeric multi-column \code{x} → \code{"counts"};
#'     multi-column \code{x} passing \code{is_alluvia_form} → \code{"alluvia"};
#'     otherwise → \code{"lodes"}.}
#' }
#'
#' Supports \strong{split_by} to produce separate sub-plots for different
#' subsets of the data, \strong{facet_by} for within-plot faceting, and
#' independent styling of nodes and links (colours, alpha, borders, labels,
#' and legend behaviour).
#'
#' \code{AlluvialPlot} is an alias of \code{SankeyPlot}.
#'
#' @section split_by workflow:
#' When \code{split_by} is provided:
#' \enumerate{
#'   \item The \code{split_by} column(s) are validated and coerced to factors
#'         via \code{\link{check_columns}()}.  Multi-column \code{split_by}
#'         is concatenated with \code{split_by_sep}.
#'   \item Empty factor levels are dropped from \code{split_by}.
#'   \item The data is split by \code{split_by} level (preserving level order).
#'         If \code{split_by} is \code{NULL}, the data is wrapped in a
#'         single-element list with name \code{"..."}.
#'   \item \code{\link{SankeyPlotAtomic}()} is called for each split, with
#'         \code{title} resolved per level (supports function-valued titles).
#'   \item Results are combined via \code{\link{combine_plots}()} (when
#'         \code{combine = TRUE}) or returned as a named list.
#' }
#'
#' @inheritParams common_args
#' @inheritParams SankeyPlotAtomic
#' @return A \code{ggplot} object (single panel, no \code{split_by}), a
#'  \code{patchwork} object (when \code{combine = TRUE} and
#'  \code{split_by} is used), or a named list of \code{ggplot} objects
#'  (when \code{combine = FALSE}).  Each plot carries \code{height} and
#'  \code{width} attributes in inches.
#' @export
#' @rdname sankeyplot
#' @examples
#' \donttest{
#' # Examples from ggalluvial datasets
#' set.seed(8525)
#'
#' data(UCBAdmissions, package = "datasets")
#' UCBAdmissions <- as.data.frame(UCBAdmissions)
#' SankeyPlot(as.data.frame(UCBAdmissions), x = c("Gender", "Dept"),
#'     y = "Freq", nodes_width = 1/12, links_fill_by = "Admit", nodes_label = TRUE,
#'     nodes_palette = "simspec", links_palette = "Set1", links_alpha = 0.5,
#'     nodes_palcolor = "black", links_color = "transparent")
#'
#' data(HairEyeColor, package = "datasets")
#' SankeyPlot(as.data.frame(HairEyeColor), x = c("Hair", "Eye", "Sex"),
#'     y = "Freq", links_fill_by = "Eye", nodes_width = 1/8, nodes_alpha = 0.4,
#'     flip = TRUE, reverse = FALSE, knot.pos = 0, links_color = "transparent",
#'     ylab = "Freq", links_alpha = 0.5, links_name = "Eye (links)", links_palcolor = c(
#'         Brown = "#70493D", Hazel = "#E2AC76", Green = "#3F752B", Blue = "#81B0E4"))
#'
#' data(Refugees, package = "alluvial")
#' country_regions <- c(
#'     Afghanistan = "Middle East",
#'     Burundi = "Central Africa",
#'     `Congo DRC` = "Central Africa",
#'     Iraq = "Middle East",
#'     Myanmar = "Southeast Asia",
#'     Palestine = "Middle East",
#'     Somalia = "Horn of Africa",
#'     Sudan = "Central Africa",
#'     Syria = "Middle East",
#'     Vietnam = "Southeast Asia"
#' )
#' Refugees$region <- country_regions[Refugees$country]
#' SankeyPlot(Refugees, x = "year", y = "refugees", alluvium = "country",
#'     links_fill_by = "country", links_color = ".fill", links_alpha = 0.75,
#'     links_palette = "Set3", facet_by = "region", x_text_angle = -45, nodes_legend = "none",
#'     theme_args = list(strip.background = ggplot2::element_rect(fill="grey80")),
#'     decreasing = FALSE, nodes_width = 0, nodes_color = "transparent", ylab = "refugees",
#'     title = "Refugee volume by country and region of origin")
#'
#' data(majors, package = "ggalluvial")
#' majors$curriculum <- as.factor(majors$curriculum)
#' SankeyPlot(majors, x = "semester", stratum = "curriculum", alluvium = "student",
#'     links_fill_by = "curriculum", flow = TRUE, stat = "alluvium", nodes_palette = "Set2",
#'     links_palette = "Set2")
#'
#' data(vaccinations, package = "ggalluvial")
#' vaccinations <- transform(vaccinations,
#'     response = factor(response, rev(levels(response))))
#' SankeyPlot(vaccinations, x = "survey", stratum = "response", alluvium = "subject",
#'     y = "freq", links_fill_by = "response", nodes_label = TRUE, nodes_alpha = 0.5,
#'     nodes_palette = "seurat", links_palette = "seurat", links_alpha = 0.5,
#'     legend.position = "none", flow = TRUE, expand = c(0, 0, 0, .15), stat = "alluvium",
#'     title = "vaccination survey responses at three points in time")
#'
#' data(Titanic, package = "datasets")
#' SankeyPlot(as.data.frame(Titanic), x = c("Class", "Sex"), y = "Freq",
#'     links_fill_by = "Survived", flow = TRUE, facet_by = "Age", facet_scales = "free_y",
#'     nodes_label = TRUE, expand = c(0.05, 0), xlab = "", links_palette = "Set1",
#'     nodes_palcolor = "white", nodes_label_miny = 10)
#'
#' # Simulated examples
#' df <- data.frame(
#'     Clone = paste0("clone", 1:10),
#'     Timepoint1 = sample(c(rep(0, 30), 1:100), 10),
#'     Timepoint2 = sample(c(rep(0, 30), 1:100), 10)
#' )
#' SankeyPlot(df, x = c("Timepoint1", "Timepoint2"), alluvium = "Clone",
#'     links_color = ".fill")
#'
#' df <- data.frame(
#'     Clone = rep(paste0("clone", 1:6), each = 2),
#'     Timepoint1 = sample(c(rep(0, 30), 1:100), 6),
#'     Timepoint2 = sample(c(rep(0, 30), 1:100), 6),
#'     Group = rep(c("A", "B"), 6)
#' )
#' SankeyPlot(df, x = c(".", "Timepoint1", "Timepoint2"),
#'     stratum = "Group", links_fill_by = "Clone", links_color = ".fill")
#' }
SankeyPlot <- function(
    data,
    in_form = c("auto", "long", "lodes", "wide", "alluvia", "counts"),
    x,
    x_sep = "_",
    y = NULL,
    stratum = NULL,
    stratum_sep = "_",
    alluvium = NULL,
    alluvium_sep = "_",
    split_by = NULL,
    split_by_sep = "_",
    keep_empty = TRUE,
    flow = FALSE,
    expand = c(0, 0, 0, 0),
    nodes_legend = c("auto", "separate", "merge", "none"),
    nodes_color = "grey30",
    links_fill_by = NULL,
    links_fill_by_sep = "_",
    links_name = NULL,
    links_color = "gray80",
    nodes_palette = "Paired",
    nodes_palcolor = NULL,
    nodes_alpha = 1,
    nodes_label = FALSE,
    nodes_label_miny = 0,
    nodes_width = 0.25,
    links_palette = "Paired",
    links_palcolor = NULL,
    palreverse = FALSE,
    links_alpha = 0.6,
    legend.box = "vertical",
    x_text_angle = 0,
    aspect.ratio = 1,
    legend.position = "right",
    legend.direction = "vertical",
    flip = FALSE,
    theme = "theme_this",
    theme_args = list(),
    title = NULL,
    subtitle = NULL,
    xlab = NULL,
    ylab = NULL,
    facet_by = NULL,
    facet_scales = "fixed",
    facet_ncol = NULL,
    facet_nrow = NULL,
    facet_byrow = TRUE,
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
            SankeyPlotAtomic(
                datas[[nm]],
                in_form = in_form,
                x = x,
                x_sep = x_sep,
                y = y,
                stratum = stratum,
                stratum_sep = stratum_sep,
                alluvium = alluvium,
                alluvium_sep = alluvium_sep,
                keep_empty = keep_empty,
                nodes_legend = nodes_legend,
                nodes_color = nodes_color,
                links_fill_by = links_fill_by,
                links_fill_by_sep = links_fill_by_sep,
                links_name = links_name,
                links_color = links_color,
                expand = expand,
                nodes_label_miny = nodes_label_miny,
                nodes_palette = nodes_palette,
                nodes_palcolor = nodes_palcolor,
                nodes_alpha = nodes_alpha,
                nodes_label = nodes_label,
                nodes_width = nodes_width,
                flow = flow,
                links_palette = links_palette,
                links_palcolor = links_palcolor,
                palreverse = palreverse,
                links_alpha = links_alpha,
                legend.box = legend.box,
                x_text_angle = x_text_angle,
                aspect.ratio = aspect.ratio,
                legend.position = legend.position,
                legend.direction = legend.direction,
                flip = flip,
                theme = theme,
                theme_args = theme_args,
                title = title,
                subtitle = subtitle,
                xlab = xlab,
                ylab = ylab,
                facet_by = facet_by,
                facet_scales = facet_scales,
                facet_ncol = facet_ncol,
                facet_nrow = facet_nrow,
                facet_byrow = facet_byrow,
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


#' @rdname sankeyplot
#' @export
AlluvialPlot <- SankeyPlot
