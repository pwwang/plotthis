#' Atomic clustree plot
#'
#' Core implementation for clustree (clustering tree) plots. This internal
#' function renders a tree-based visualisation showing how cluster assignments
#' change across increasing clustering resolutions. It is called by the
#' exported \code{ClustreePlot()} function.
#'
#' The function expects a data frame containing columns named with a common
#' \code{prefix} followed by numeric resolution values (e.g. \code{"res_0.1"},
#' \code{"res_0.3"}, \code{"res_0.5"}). Each column represents cluster
#' assignments at a given resolution, and the tree visualises how cells (rows)
#' transition between clusters as resolution increases.
#'
#' The core layout is produced via \code{clustree::clustree()}, which computes
#' a \code{ggraph} layout (default: \code{"sugiyama"}) linking clusters across
#' adjacent resolutions. Nodes represent clusters and edges represent cell
#' transitions; edge width and colour encode the number of transitioning cells.
#' Cluster labels are overlaid on each node via \code{ggplot2::geom_text()}.
#'
#' Key features:
#' \itemize{
#'   \item \strong{Resolution-level node colouring} — each resolution level
#'   receives a distinct colour from \code{palette} via
#'   \code{ggplot2::scale_color_manual()}.
#'   \item \strong{Edge gradient} — edges are coloured by transition count
#'   using \code{ggraph::scale_edge_color_gradientn()} with the
#'   \code{edge_palette} gradient.
#'   \item \strong{Flip support} — \code{flip = TRUE} places resolutions on
#'   the x-axis with cluster IDs as y-axis row labels for left-to-right
#'   reading.
#'   \item \strong{Automatic dimension calculation} — plot height and width
#'   are sized based on the number of resolutions, number of clusters, and
#'   legend configuration, with different formulae for flipped vs. non-flipped
#'   layouts.
#' }
#'
#' @section Architecture:
#'
#' \strong{ClustreePlotAtomic} executes the following steps:
#' \enumerate{
#'   \item \strong{Column selection} — selects all columns whose names start
#'   with \code{prefix} via \code{dplyr::starts_with()}.
#'   \item \strong{NA filtering} — removes rows with any missing values via
#'   \code{stats::complete.cases()}.
#'   \item \strong{Expansion normalisation} — \code{\link{norm_expansion}()}
#'   parses the \code{expand} argument for continuous x and y axes.
#'   \item \strong{Empty data guard} — stops with an informative error if no
#'   columns match the prefix or no complete rows remain.
#'   \item \strong{Resolution parsing} — extracts the suffix after
#'   \code{prefix} from each column name and strips leading separators
#'   (\code{"_"} or \code{"."}) if present. Values are converted to numeric
#'   and sorted. Column names are rewritten to use the original prefix for
#'   consistent \code{clustree} processing.
#'   \item \strong{Cluster counting} — computes the number of unique clusters
#'   at the highest resolution (\code{max_clusters}), used for dimension
#'   calculation in the non-flipped layout.
#'   \item \strong{Argument merging} — builds a call to
#'   \code{clustree::clustree()} by merging user-supplied \code{...} with
#'   sensible defaults for \code{node_alpha}, \code{edge_width},
#'   \code{show_axis}, \code{layout} (\code{"sugiyama"}),
#'   \code{node_size_range} (\code{c(6, 12)}), \code{node_text_size}
#'   (\code{3}), and \code{node_text_colour} (\code{"black"}).
#'   \item \strong{clustree execution} — calls
#'   \code{gglogger::register(clustree::clustree)} with the merged arguments
#'   to produce the base \code{ggplot} object.
#'   \item \strong{Node labels} — overlays \code{ggplot2::geom_text()} with
#'   the cluster label from the \code{clustree} output, using the configured
#'   text size and colour.
#'   \item \strong{Resolution colour scale} — applies
#'   \code{ggplot2::scale_color_manual()} with one colour per resolution
#'   level using \code{palette_this()}. The colour legend is suppressed (the
#'   edge gradient legend serves as the primary guide).
#'   \item \strong{Edge colour scale} — applies
#'   \code{ggraph::scale_edge_color_gradientn()} with the
#'   \code{edge_palette} palette, encoding transition counts via a
#'   colour-bar guide with black frame and ticks.
#'   \item \strong{Theme and labels} — applies the selected theme and sets
#'   \code{title}, \code{subtitle}, x-axis label, and y-axis label (default
#'   y-label is the \code{prefix} with trailing separator stripped).
#'   \item \strong{Flip branch} (\code{flip = TRUE}):
#'     \itemize{
#'       \item Resolutions are placed on the x-axis (horizontal); clusters
#'       become y-axis rows.
#'       \item Width scales with \code{max(3, 0.5 + nres * 1.0)}; height
#'       scales with \code{max(3, 0.5 + max_clusters * 0.5)} (or via
#'       aspect.ratio).
#'       \item \code{scale_y_reverse()} + \code{coord_flip()} reorients the
#'       layout so resolutions read left-to-right.
#'       \item Horizontal grid lines are dashed grey80; y-axis text and
#'       ticks are suppressed.
#'     }
#'   \item \strong{Non-flip branch} (\code{flip = FALSE}, default):
#'     \itemize{
#'       \item Resolutions are on the y-axis (rows); clusters spread across
#'       the x-axis (columns).
#'       \item Width scales with \code{max(3, 0.5 + max_clusters * 0.5)};
#'       height scales with \code{max(3, 0.5 + nres * 1.0)}
#'       (or via aspect.ratio).
#'       \item \code{coord_cartesian(clip = "off")} and
#'       \code{scale_y_continuous()} show resolution labels on the y-axis.
#'       \item Vertical grid lines are dashed grey80; x-axis text and ticks
#'       are suppressed.
#'     }
#'   \item \strong{Dimension storage} — \code{\link{calculate_plot_dimensions}()}
#'   computes \code{height} and \code{width} attributes, stored on the
#'   \code{ggplot} object.
#' }
#'
#' @inheritParams common_args
#' @param prefix A character string specifying the common prefix of the
#'   resolution columns in \code{data}. All columns whose names start with
#'   this prefix are selected as resolution columns. The suffix after the
#'   prefix is parsed as a numeric resolution value. Supports \code{"_"}
#'   and \code{"."} as separators between the prefix and the resolution
#'   value (e.g. \code{"res_0.5"} or \code{"p.0.5"}).
#' @param flip A logical value. If \code{TRUE}, the tree is flipped so that
#'   resolutions are displayed on the x-axis (left to right) and cluster
#'   assignments are shown as row labels on the y-axis.
#'   Default: \code{FALSE}.
#' @param alpha A numeric value in \code{[0, 1]} specifying the transparency
#'   of the nodes in the clustree plot. Only used when \code{node_alpha} is
#'   not explicitly provided via \code{...}. Default: \code{0.85}.
#' @param edge_palette A character string specifying the palette name for
#'   the edge colour gradient. Edges are coloured by the number of
#'   transitioning cells between clusters at adjacent resolutions, using
#'   \code{ggraph::scale_edge_color_gradientn()}. Default: \code{"Spectral"}.
#' @param edge_palcolor A character vector of custom colours for the edge
#'   colour gradient. When \code{NULL} (the default), colours are derived
#'   from \code{edge_palette}.
#' @param ... Additional arguments passed to \code{clustree::clustree()}.
#'   Commonly used overrides include \code{node_size_range},
#'   \code{node_text_size}, \code{layout} (default: \code{"sugiyama"}),
#'   \code{show_axis}, and \code{node_text_colour}. Note that \code{x}
#'   (the data) and \code{prefix} are set internally and cannot be
#'   overridden here.
#' @return A \code{ggplot} object with \code{height} and \code{width}
#'   attributes.
#' @keywords internal
#' @importFrom stats complete.cases
#' @importFrom dplyr %>% select starts_with
#' @importFrom ggplot2 scale_color_manual coord_cartesian element_line element_blank labs coord_flip
#' @importFrom ggplot2 scale_y_reverse coord_flip
#' @importFrom ggrepel geom_text_repel
ClustreePlotAtomic <- function(
    data,
    prefix,
    flip = FALSE,
    alpha = 0.85,
    palette = "Paired",
    palcolor = NULL,
    palreverse = FALSE,
    edge_palette = "Spectral",
    edge_palcolor = NULL,
    aspect.ratio = 1,
    legend.position = "right",
    legend.direction = "vertical",
    title = NULL,
    subtitle = NULL,
    xlab = NULL,
    ylab = NULL,
    expand = c(0.1, 0.1),
    theme = "theme_this",
    theme_args = list(),
    ...
) {
    # if (!requireNamespace("ggraph", quietly = TRUE)) {
    #     stop("ggraph is not available for ClustreePlot. Please install it first.")
    # }
    # ! Unknown guide: edge_colourbar
    # suppressMessages(suppressWarnings(suppressPackageStartupMessages(library(ggraph))))
    # if (!requireNamespace("clustree", quietly = TRUE)) {
    #     stop("clustree is not available for ClustreePlot. Please install it first.")
    # }

    data <- data %>% select(starts_with(prefix))
    data <- data[complete.cases(data), , drop = FALSE]
    expand <- norm_expansion(
        expand,
        x_type = "continuous",
        y_type = "continuous"
    )

    if (ncol(data) == 0 || nrow(data) == 0) {
        stop("No data found with prefix '", prefix, "'")
    }
    resolutions <- substring(colnames(data), nchar(prefix) + 1)
    nres <- length(resolutions)
    if (all(startsWith(resolutions, "_"))) {
        prefix <- paste0(prefix, "_")
        resolutions <- substring(resolutions, 2)
    } else if (all(startsWith(resolutions, "."))) {
        prefix <- paste0(prefix, ".")
        resolutions <- substring(resolutions, 2)
    }
    resolutions <- as.numeric(resolutions)
    # make sure res.0.50 to be access by res.0.5
    colnames(data) <- paste0(prefix, resolutions)
    resolutions <- sort(resolutions)
    nres <- length(resolutions)
    max_clusters <- length(unique(data[[paste0(prefix, max(resolutions))]]))
    clustree_args <- list(...)
    clustree_args$x <- data
    clustree_args$prefix <- prefix
    clustree_args$node_alpha <- clustree_args$node_alpha %||% alpha %||% 0.85
    clustree_args$edge_width <- clustree_args$edge_width %||% 0.9
    clustree_args$show_axis <- clustree_args$show_axis %||% TRUE
    clustree_args$layout <- clustree_args$layout %||% "sugiyama"
    clustree_args$node_size_range <- clustree_args$node_size_range %||% c(6, 12)
    clustree_args$node_text_size <- clustree_args$node_text_size %||% 3
    clustree_args$node_text_colour <- clustree_args$node_text_colour %||%
        clustree_args$node_text_color %||%
        "black"
    clustree <- gglogger::register(clustree::clustree)

    p <- suppressMessages(do_call(clustree, clustree_args))

    # make glow effect
    # ggrepel::geom_text_repel with bg.colour is not working well with coord_flip
    # theta <- seq(pi / 8, 2 * pi, length.out = 16)
    # for (i in theta) {
    #     dx <- cos(i) * clustree_args$node_text_size / 10
    #     dy <- sin(i) * clustree_args$node_text_size / 10
    #     p <- p + geom_text(aes(label = cluster, x = x + dx, y = y + dy), fontface = "bold",
    #         size = clustree_args$node_text_size, color = "white")
    # }
    # p <- p + geom_text(aes(label = cluster, x = x + 0.1, y = y + 0.1),
    #     size = clustree_args$node_text_size, color = "white")
    # p <- p + geom_text(aes(label = cluster, x = x + 0.1, y = y - 0.1),
    #     size = clustree_args$node_text_size, color = "white")
    # p <- p + geom_text(aes(label = cluster, x = x - 0.1, y = y - 0.1),
    #     size = clustree_args$node_text_size, color = "white")
    # p <- p + geom_text(aes(label = cluster, x = x - 0.1, y = y - 0.1),
    #     size = clustree_args$node_text_size, color = "white")

    p <- suppressMessages({
        p +
            geom_text(
                aes(label = !!sym("cluster"), x = !!sym("x"), y = !!sym("y")),
                size = clustree_args$node_text_size,
                color = clustree_args$node_text_colour
            ) +
            scale_color_manual(
                values = palette_this(
                    n = nres,
                    palette = palette,
                    palcolor = palcolor,
                    keep_names = FALSE,
                    reverse = palreverse
                ),
                guide = "none"
            ) +
            ggraph::scale_edge_color_gradientn(
                name = "count",
                n.breaks = 5,
                colors = palette_this(
                    palette = edge_palette,
                    palcolor = edge_palcolor,
                    reverse = palreverse
                ),
                na.value = "grey80",
                guide = ggraph::guide_edge_colorbar(
                    frame.colour = "black",
                    ticks.colour = "black",
                    title.hjust = 0
                )
            ) +
            do_call(theme, theme_args) +
            ggplot2::theme(
                aspect.ratio = aspect.ratio,
                axis.title.x = element_text(),
                axis.title.y = element_text(),
                legend.position = legend.position,
                legend.direction = legend.direction,
                # panel.border = element_blank()
            ) +
            labs(
                title = title,
                subtitle = subtitle,
                x = xlab %||% "",
                y = ylab %||% sub("\\.|_$", "", prefix)
            ) +
            scale_x_continuous(expand = expand$x)
    })

    if (isTRUE(flip)) {
        # After flip: resolutions become x-axis columns, clusters become y-axis rows
        # Width driven by nres; height by max_clusters (or aspect.ratio applied to width)
        content_width_fl <- max(3, 0.5 + nres * 1.0)
        content_height_fl <- if (!is.null(aspect.ratio)) {
            max(3, min(content_width_fl * aspect.ratio, 12))
        } else {
            max(3, 0.5 + max_clusters * 0.5)
        }
        dims <- calculate_plot_dimensions(
            base_height = content_height_fl,
            aspect.ratio = NULL,
            n_x = nres,
            x_scale_factor = 1.0,
            legend.position = legend.position,
            legend.direction = legend.direction
        )
        attr(p, "height") <- dims$height
        attr(p, "width") <- dims$width
        p <- suppressMessages({
            p +
                scale_y_reverse(
                    breaks = nres:1,
                    labels = resolutions,
                    expand = expand$y
                ) +
                coord_flip(clip = "off") +
                ggplot2::theme(
                    panel.grid.major.x = element_line(
                        colour = "grey80",
                        linetype = 2
                    ),
                    panel.grid.major.y = element_blank(),
                    axis.line.y = element_blank(),
                    axis.text.y = element_blank(),
                    axis.ticks.y = element_blank()
                )
        })
    } else {
        # Non-flip: resolutions on y-axis (rows), clusters spread on x-axis (columns)
        # Width driven by max_clusters; height by nres (or aspect.ratio applied to width)
        content_width_nf <- max(3, 0.5 + max_clusters * 0.5)
        content_height_nf <- if (!is.null(aspect.ratio)) {
            max(3, min(content_width_nf * aspect.ratio, 12))
        } else {
            max(3, 0.5 + nres * 1.0)
        }
        dims <- calculate_plot_dimensions(
            base_height = content_height_nf,
            aspect.ratio = NULL,
            n_x = max_clusters,
            x_scale_factor = 0.5,
            legend.position = legend.position,
            legend.direction = legend.direction
        )
        attr(p, "height") <- dims$height
        attr(p, "width") <- dims$width
        p <- suppressMessages({
            p +
                coord_cartesian(clip = "off") +
                scale_y_continuous(
                    breaks = nres:1,
                    labels = resolutions,
                    expand = expand$y
                ) +
                ggplot2::theme(
                    panel.grid.major.x = element_blank(),
                    panel.grid.major.y = element_line(
                        colour = "grey80",
                        linetype = 2
                    ),
                    axis.line.x = element_blank(),
                    axis.text.x = element_blank(),
                    axis.ticks.x = element_blank()
                )
        })
    }
    p
}

#' Clustree Plot
#'
#' @description
#' Creates a clustree (clustering tree) plot visualising how cluster
#' assignments change across increasing clustering resolutions. The plot
#' helps identify stable clustering solutions and understand the hierarchical
#' relationships among clusters at different resolution thresholds.
#'
#' The function expects a data frame with columns named by a common
#' \code{prefix} followed by numeric resolution values (e.g.
#' \code{"res_0.1"}, \code{"res_0.3"}, \code{"res_0.5"}). Each column
#' contains cluster labels (factor or character) for every observation at
#' that resolution.
#'
#' Internally, the function uses \code{clustree::clustree()} to compute a
#' \code{ggraph}-based tree layout where nodes are clusters and edges
#' represent cells transitioning between clusters at adjacent resolutions.
#' Edge colour and width encode the number of transitioning cells.
#'
#' Key features:
#' \itemize{
#'   \item \strong{Resolution-level node colouring} — each resolution
#'   receives a distinct colour from the selected \code{palette}.
#'   \item \strong{Edge gradient} — edges are coloured by transition count
#'   using a separate \code{edge_palette} colour gradient.
#'   \item \strong{Flip support} — \code{flip = TRUE} places resolutions
#'   on the x-axis for left-to-right reading.
#'   \item \strong{Split by groups} — \code{split_by} generates per-group
#'   clustree plots that are combined via \code{patchwork}.
#'   \item \strong{Automatic dimensions} — plot height and width are
#'   automatically computed based on the number of resolutions, clusters,
#'   and the legend configuration.
#' }
#'
#' @section split_by Workflow (ClustreePlot):
#'
#' When \code{split_by} is provided, the following pipeline executes:
#' \enumerate{
#'   \item \strong{Argument validation} — \code{validate_common_args()}
#'   checks the \code{seed} value and sets the random seed.
#'   \item \strong{Theme resolution} — \code{process_theme()} resolves the
#'   \code{theme} string or function to a theme function.
#'   \item \strong{Split column validation} — \code{check_columns()}
#'   resolves \code{split_by} with \code{force_factor = TRUE,
#'   allow_multi = TRUE, concat_multi = TRUE}.
#'   \item \strong{Data splitting} — splits \code{data} by \code{split_by}
#'   levels (unused levels dropped), preserving factor level order.
#'   \item \strong{Per-split palette / colour / legend} —
#'   \code{check_palette()}, \code{check_palcolor()}, and
#'   \code{check_legend()} resolve per-split overrides for
#'   \code{palette}, \code{palcolor}, \code{legend.position}, and
#'   \code{legend.direction}.
#'   \item \strong{Per-split title} — when \code{title} is a function, it
#'   receives the default title (the split level name) and can return a
#'   custom string; otherwise \code{title \%||\% split_level} is used.
#'   \item \strong{Dispatch} — each split subset is passed to
#'   \code{\link{ClustreePlotAtomic}} with the per-split parameters.
#'   \item \strong{Combination} — \code{\link{combine_plots}()} assembles
#'   the list of plots via \code{patchwork::wrap_plots}, honouring
#'   \code{nrow}/\code{ncol}/\code{byrow}/\code{design}.
#' }
#'
#' @inheritParams ClustreePlotAtomic
#' @inheritParams common_args
#' @param split_by The column(s) to split data by and generate separate
#'   clustree plots for each level. Each split level produces an independent
#'   clustree plot via \code{\link{ClustreePlotAtomic}}.
#' @param split_by_sep A character string used to concatenate multiple
#'   \code{split_by} column values when \code{split_by} specifies more than
#'   one column. Default: \code{"_"}.
#' @param seed The random seed for reproducibility. Passed to
#'   \code{validate_common_args()}. Default: \code{8525}.
#' @param combine A logical value. If \code{TRUE} (the default), the list
#'   of per-split plots is combined into a single \code{patchwork} object.
#'   If \code{FALSE}, returns the raw list of \code{ggplot} objects.
#' @param nrow,ncol,byrow Integers controlling the layout of combined plots
#'   via \code{patchwork::wrap_plots()}. \code{byrow = TRUE} (default)
#'   fills the layout row-wise. Ignored when \code{design} is provided.
#' @param axes,axis_titles Strings controlling how axes and axis titles are
#'   handled across combined plots. Passed to \code{\link{combine_plots}()}.
#'   See \code{?patchwork::wrap_plots} for options (\code{"keep"},
#'   \code{"collect"}, \code{"collect_x"}, \code{"collect_y"}).
#' @param guides A string controlling guide collection across combined
#'   plots. Passed to \code{\link{combine_plots}()}.
#' @param design A custom layout specification for combined plots. Passed
#'   to \code{\link{combine_plots}()}. When specified, \code{nrow},
#'   \code{ncol}, and \code{byrow} are ignored.
#' @return A \code{ggplot} object (single plot), a \code{patchwork} object
#'   (when \code{combine = TRUE} with \code{split_by}), or a \code{list} of
#'   \code{ggplot} objects (when \code{combine = FALSE}).
#' @export
#' @examples
#' \donttest{
#' set.seed(8525)
#' N <- 100
#' data <- data.frame(
#'     p.0.4 = sample(LETTERS[1:5], N, replace = TRUE),
#'     p.0.5 = sample(LETTERS[1:6], N, replace = TRUE),
#'     p.0.6 = sample(LETTERS[1:7], N, replace = TRUE),
#'     p.0.7 = sample(LETTERS[1:8], N, replace = TRUE),
#'     p.0.8 = sample(LETTERS[1:9], N, replace = TRUE),
#'     p.0.9 = sample(LETTERS[1:10], N, replace = TRUE),
#'     p.1 = sample(LETTERS[1:30], N, replace = TRUE),
#'     split = sample(1:2, N, replace = TRUE)
#' )
#'
#' # --- Basic clustree plot ---
#' ClustreePlot(data, prefix = "p")
#'
#' # --- Flipped layout (resolutions on x-axis) ---
#' ClustreePlot(data, prefix = "p", flip = TRUE)
#'
#' # --- Split by group ---
#' ClustreePlot(data, prefix = "p", split_by = "split")
#'
#' # --- Split by group with per-split palettes ---
#' ClustreePlot(data, prefix = "p", split_by = "split",
#'              palette = c("1" = "Set1", "2" = "Paired"))
#' }
ClustreePlot <- function(
    data,
    prefix,
    flip = FALSE,
    split_by = NULL,
    split_by_sep = "_",
    palette = "Paired",
    palcolor = NULL,
    palreverse = FALSE,
    edge_palette = "Spectral",
    edge_palcolor = NULL,
    aspect.ratio = 1,
    legend.position = "right",
    legend.direction = "vertical",
    title = NULL,
    subtitle = NULL,
    xlab = NULL,
    ylab = NULL,
    expand = c(0.1, 0.1),
    theme = "theme_this",
    theme_args = list(),
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
    validate_common_args(seed)
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
            ClustreePlotAtomic(
                datas[[nm]],
                prefix = prefix,
                flip = flip,
                palette = palette[[nm]],
                palcolor = palcolor[[nm]],
                palreverse = palreverse,
                edge_palette = edge_palette,
                edge_palcolor = edge_palcolor,
                expand = expand,
                aspect.ratio = aspect.ratio,
                legend.position = legend.position[[nm]],
                legend.direction = legend.direction[[nm]],
                title = title,
                subtitle = subtitle,
                xlab = xlab,
                ylab = ylab,
                theme = theme,
                theme_args = theme_args,
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
