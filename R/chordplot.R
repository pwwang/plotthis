#' Atomic chord plot (internal)
#'
#' @description
#' Core implementation for drawing a chord diagram using
#' \code{circlize::chordDiagram()}.  This is the workhorse behind the
#' exported \code{\link{ChordPlot}} function — it takes a **single** data
#' frame (no \code{split_by} support) and returns a \code{patchwork}
#' wrapped element.
#'
#' Chord diagrams visualise relationships (flows) between two sets of
#' categories arranged around a circle.  The width of each link is
#' proportional to the value (\code{y}) connecting its source and target
#' nodes.
#'
#' @section Architecture:
#' \enumerate{
#'   \item \strong{Column resolution} — \code{from}, \code{to}, and
#'         \code{y} are validated and transformed via
#'         \code{\link{check_columns}}.  Multi-column \code{from} and
#'         \code{to} are concatenated with their respective separators
#'         (\code{from_sep}, \code{to_sep}).
#'   \item \strong{Count aggregation} — when \code{y = NULL}, the count
#'         of observations per (\code{from}, \code{to}) pair is computed
#'         as a new \code{.y} column.  Factor levels from the original
#'         data are preserved.
#'   \item \strong{NA / empty-level handling} —
#'         \code{\link{process_keep_na_empty}()} applies \code{keep_na}
#'         and \code{keep_empty} policies.
#'   \item \strong{Flip} — when \code{flip = TRUE}, the \code{from} and
#'         \code{to} columns are swapped, effectively reversing the link
#'         direction.
#'   \item \strong{Colour mapping} — \code{\link{palette_this}()} assigns
#'         colours to all unique \code{from} and \code{to} values (the grid
#'         sectors).  \code{NA} values are mapped to a literal \code{"NA"}
#'         level and assigned a colour from the palette.
#'   \item \strong{Link colour resolution} — \code{links_color} controls
#'         whether each connecting ribbon takes its colour from the
#'         \code{"from"} side (default) or the \code{"to"} side.
#'   \item \strong{circlize rendering} — two rendering paths exist,
#'         selected by \code{labels_rot}:
#'         \itemize{
#'           \item \strong{Horizontal labels} (\code{labels_rot = FALSE}):
#'                 track 1 has a fixed height of 1 mm and uses
#'                 \code{niceFacing = TRUE} for automatic label rotation.
#'                 Track 2 adds axis ticks on the outside for wide-enough
#'                 sectors.
#'           \item \strong{Rotated labels} (\code{labels_rot = TRUE}):
#'                 track 1 height is computed from the maximum string width
#'                 of all node names, and labels are rendered with
#'                 \code{facing = "clockwise"}.  Track 2 adds axis ticks.
#'         }
#'         Links use arrow heads (\code{link.arr.type = "big.arrow"}) and
#'         differentiated height (\code{direction.type = c("diffHeight",
#'         "arrows")}).  Both tracks set \code{bg.border = NA} to prevent
#'         border lines from appearing between sectors.
#'   \item \strong{Patchwork integration} — the circlize formula is wrapped
#'         via \code{\link[patchwork]{wrap_elements}()} so it can be
#'         composed with other ggplot objects.  Title and subtitle are added
#'         via \code{\link[patchwork]{plot_annotation}()}.
#'   \item \strong{Dimension attributes} — \code{height} and \code{width}
#'         attributes (in inches) are set to a base size (7) scaled up by
#'         2, 4, or 6 depending on the maximum label character width when
#'         \code{labels_rot = TRUE}.
#' }
#'
#' @inheritParams common_args
#' @param from A character string (or vector) specifying the column name(s)
#'  for the source nodes.  Character/factor columns are expected.  Multiple
#'  columns are concatenated with \code{from_sep}.
#' @param from_sep A character string to join multiple \code{from} columns.
#'  Default \code{"_"}.
#' @param to A character string (or vector) specifying the column name(s)
#'  for the target nodes.  Character/factor columns are expected.  Multiple
#'  columns are concatenated with \code{to_sep}.
#' @param to_sep A character string to join multiple \code{to} columns.
#'  Default \code{"_"}.
#' @param y A character string specifying the numeric column whose values
#'  determine link thickness.  When \code{NULL}, the count of observations
#'  per (\code{from}, \code{to}) pair is used.
#' @param flip Logical; if \code{TRUE}, swap the source and target nodes,
#'  reversing the link direction.
#' @param links_color A character string controlling which node's colour
#'  each link ribbon takes: \code{"from"} (default) or \code{"to"}.
#' @param labels_rot Logical; if \code{TRUE}, rotate sector labels by 90
#'  degrees (clockwise).  Default \code{FALSE} uses \code{niceFacing} for
#'  automatic orientation.
#' @param alpha Numeric transparency for the link ribbons (0–1).
#'  Default \code{0.5}.
#' @return A \code{patchwork} wrapped element with \code{height} and
#'  \code{width} attributes (in inches) attached.  The original data is
#'  stored in the \code{p$data} field.
#' @importFrom dplyr %>% group_by summarise n select
#' @importFrom patchwork wrap_elements plot_annotation
#' @keywords internal
ChordPlotAtomic <- function(
    data,
    y = NULL,
    from = NULL,
    from_sep = "_",
    to = NULL,
    to_sep = "_",
    flip = FALSE,
    links_color = c("from", "to"),
    theme = "theme_this",
    theme_args = list(),
    palette = "Paired",
    palcolor = NULL,
    palreverse = FALSE,
    alpha = 0.5,
    labels_rot = FALSE,
    title = NULL,
    subtitle = NULL,
    keep_na = FALSE,
    keep_empty = FALSE,
    ...
) {
    # if (!requireNamespace("circlize", quietly = TRUE)) {
    #     stop("circlize is required for chord plot.")
    # }
    links_color <- match.arg(links_color)
    from <- check_columns(
        data,
        from,
        force_factor = TRUE,
        allow_multi = TRUE,
        concat_multi = TRUE,
        concat_sep = from_sep
    )
    to <- check_columns(
        data,
        to,
        force_factor = TRUE,
        allow_multi = TRUE,
        concat_multi = TRUE,
        concat_sep = to_sep
    )
    y <- check_columns(data, y)
    if (is.null(y)) {
        from_levels <- levels(data[[from]])
        to_levels <- levels(data[[to]])
        data <- data %>%
            group_by(!!sym(from), !!sym(to)) %>%
            summarise(.y = n(), .groups = "drop")

        data[[from]] <- factor(data[[from]], levels = from_levels)
        data[[to]] <- factor(data[[to]], levels = to_levels)

        y <- ".y"
    }

    data <- process_keep_na_empty(data, keep_na, keep_empty)
    if (isTRUE(flip)) {
        data <- data %>%
            select(from = !!sym(to), to = !!sym(from), value = !!sym(y))
    } else {
        data <- data %>%
            select(from = !!sym(from), to = !!sym(to), value = !!sym(y))
    }

    data <- data[order(data$from, data$to), , drop = FALSE]
    # data$from <- as.character(data$from)
    # data$to <- as.character(data$to)

    # froms <- unique(data$from)
    # tos <- unique(data$to)
    from_vals <- levels(data$from)
    if (anyNA(data$from)) {
        from_vals <- c(from_vals, NA)
    }
    to_vals <- levels(data$to)
    if (anyNA(data$to)) {
        to_vals <- c(to_vals, NA)
    }

    grid_cols <- palette_this(
        unique(c(from_vals, to_vals)),
        palette = palette,
        palcolor = palcolor,
        NA_keep = TRUE,
        reverse = palreverse
    )
    names(grid_cols)[is.na(names(grid_cols))] <- "NA"
    if (anyNA(from_vals)) {
        from_vals[is.na(from_vals)] <- "NA"
        levels(data$from) <- from_vals
        data$from[is.na(data$from)] <- "NA"
    }
    if (anyNA(to_vals)) {
        to_levels[is.na(to_vals)] <- "NA"
        levels(data$to) <- to_vals
        data$to[is.na(data$to)] <- "NA"
    }
    link_cols <- grid_cols[as.character(data[[links_color]])]

    circlize::circos.clear()
    circlize::circos.par(track.margin = c(0.01, 0.02))

    if (!isTRUE(labels_rot)) {
        p <- ~ {
            circlize::chordDiagram(
                data,
                grid.col = grid_cols,
                col = link_cols,
                transparency = 1 - alpha,
                direction = 1,
                annotationTrack = "grid",
                direction.type = c("diffHeight", "arrows"),
                link.arr.type = "big.arrow",
                link.arr.length = .04,
                preAllocateTracks = list(
                    list(track.height = circlize::mm_h(1)),
                    list(track.height = circlize::mm_h(.1))
                )
            )
            circlize::circos.track(
                track.index = 1,
                panel.fun = function(x, y) {
                    circlize::circos.text(
                        circlize::CELL_META$xcenter,
                        circlize::CELL_META$ylim[1] + 5.5,
                        circlize::CELL_META$sector.index,
                        niceFacing = TRUE,
                        adj = c(0.5, 0.5)
                    )
                },
                bg.border = NA
            )
            circlize::circos.track(
                track.index = 2,
                panel.fun = function(x, y) {
                    for (si in circlize::get.all.sector.index()) {
                        start.degree <- circlize::get.cell.meta.data(
                            "cell.start.degree",
                            sector.index = si
                        )
                        end.degree <- circlize::get.cell.meta.data(
                            "cell.end.degree",
                            sector.index = si
                        )
                        if (abs(end.degree - start.degree) > 2) {
                            # otherwise: patchwork wrap_elements 'x' and 'units' must have length > 0
                            circlize::circos.axis(
                                h = "top",
                                labels.cex = 0.7,
                                labels.niceFacing = TRUE,
                                sector.index = si
                            )
                        }
                    }
                },
                bg.border = NA
            ) # here set bg.border to NA is important
        }
    } else {
        allnames <- unique(c(from_vals, to_vals))
        p <- ~ {
            circlize::chordDiagram(
                data,
                grid.col = grid_cols,
                col = link_cols,
                transparency = 1 - alpha,
                direction = 1,
                annotationTrack = "grid",
                direction.type = c("diffHeight", "arrows"),
                link.arr.type = "big.arrow",
                link.arr.length = .04,
                preAllocateTracks = list(
                    list(track.height = max(strwidth(allnames))),
                    list(track.height = circlize::mm_h(.1))
                )
            )
            circlize::circos.track(
                track.index = 1,
                panel.fun = function(x, y) {
                    circlize::circos.text(
                        circlize::CELL_META$xcenter,
                        circlize::CELL_META$ylim[1] + .15,
                        circlize::CELL_META$sector.index,
                        facing = "clockwise",
                        niceFacing = TRUE,
                        adj = c(0, 0.5)
                    )
                },
                bg.border = NA
            )
            circlize::circos.track(
                track.index = 2,
                panel.fun = function(x, y) {
                    for (si in circlize::get.all.sector.index()) {
                        start.degree <- circlize::get.cell.meta.data(
                            "cell.start.degree",
                            sector.index = si
                        )
                        end.degree <- circlize::get.cell.meta.data(
                            "cell.end.degree",
                            sector.index = si
                        )
                        if (abs(end.degree - start.degree) > 2) {
                            # otherwise: patchwork wrap_elements 'x' and 'units' must have length > 0
                            circlize::circos.axis(
                                h = "top",
                                labels.cex = 0.7,
                                labels.niceFacing = TRUE,
                                sector.index = si
                            )
                        }
                    }
                },
                bg.border = NA
            ) # here set bg.border to NA is important
        }
    }
    p <- wrap_elements(full = p)

    if (!is.null(title) || !is.null(subtitle)) {
        p <- p +
            plot_annotation(
                title = title,
                subtitle = subtitle,
                theme = do_call(theme, theme_args)
            )
    }
    # allow to access data from the plot object
    p$data <- data

    base_size <- 7
    if (isTRUE(labels_rot)) {
        maxchar <- max(c(nchar(from_vals), nchar(to_vals)))
        if (maxchar < 16) {
            base_size <- base_size + 2
        } else if (maxchar < 32) {
            base_size <- base_size + 4
        } else {
            base_size <- base_size + 6
        }
    }

    attr(p, "height") <- base_size
    attr(p, "width") <- base_size
    p
}

#' Chord / Circos plot
#'
#' @description
#' Draws a chord diagram (also known as a circos plot) to visualise
#' relationships between two categorical variables.  Categories are arranged
#' around a circle, and connecting ribbons (links) represent the flow or
#' association between source and target nodes.  The width of each link is
#' proportional to the associated numeric value or observation count.
#'
#' The function supports \strong{count aggregation} (omit \code{y} to plot
#' observation counts per pair), \strong{link colouring} by source or target
#' node, \strong{label rotation} options, and splitting into separate
#' sub-diagrams via \code{split_by}.
#'
#' \code{CircosPlot} is an alias of \code{ChordPlot}.
#'
#' @section split_by workflow:
#' When \code{split_by} is provided:
#' \enumerate{
#'   \item \code{\link{check_keep_na}()} and \code{\link{check_keep_empty}()}
#'         normalise the \code{keep_na} / \code{keep_empty} arguments for all
#'         columns (\code{split_by}, \code{from}, \code{to}).
#'   \item The \code{split_by} column is validated and its NA / empty levels
#'         are processed.  It is then removed from the per-column lists.
#'   \item The data is split by \code{split_by} (preserving level order).  If
#'         \code{split_by} is \code{NULL}, the data is wrapped in a
#'         single-element list with name \code{"..."}.
#'   \item Per-split \code{palette} and \code{palcolor} are resolved via
#'         \code{\link{check_palette}()} and \code{\link{check_palcolor}()}.
#'   \item \code{\link{ChordPlotAtomic}()} is called for each split.
#'         When \code{title} is a function, it receives the split level name
#'         for dynamic titles.
#'   \item Results are combined via \code{\link{combine_plots}()}.
#' }
#'
#' @inheritParams common_args
#' @inheritParams ChordPlotAtomic
#' @param split_by The column(s) to split the data by for separate sub-diagrams.
#'  Multiple columns are concatenated with \code{split_by_sep}.
#' @param split_by_sep A character string to separate concatenated
#'  \code{split_by} columns.  Default \code{"_"}.
#' @param seed A numeric seed for reproducibility.
#' @param combine Logical; when \code{TRUE} (default), returns a combined
#'  \code{patchwork} object.  When \code{FALSE}, returns a named list of
#'  individual wrapped elements.
#' @param ncol,nrow Integer number of columns / rows for the combined layout.
#' @param byrow Logical; fill the combined layout by row (default \code{TRUE}).
#' @param axes,axis_titles Character strings for axis handling in the
#'  combined layout.
#' @param guides Character string for legend collection across panels.
#' @param design A custom layout design for the combined plot.
#' @return A \code{patchwork} object or a named list of wrapped elements
#'  (when \code{combine = FALSE}), each with \code{height} and \code{width}
#'  attributes in inches.
#' @rdname chordplot
#' @export
#' @examples
#' \donttest{
#' set.seed(8525)
#' data <- data.frame(
#'     nodes1 = sample(c("Soure1", "Source2", "Source3"), 10, replace = TRUE),
#'     nodes2 = sample(letters[1:3], 10, replace = TRUE),
#'     y = sample(1:5, 10, replace = TRUE)
#' )
#'
#' # Basic chord diagram (counts)
#' ChordPlot(data, from = "nodes1", to = "nodes2")
#'
#' # Links coloured by target + rotated labels
#' ChordPlot(data, from = "nodes1", to = "nodes2",
#'           links_color = "to", labels_rot = TRUE)
#'
#' # With explicit y values (link thickness)
#' ChordPlot(data, from = "nodes1", to = "nodes2", y = "y")
#'
#' # Split by a column — one diagram per split level
#' ChordPlot(data, from = "nodes1", to = "nodes2", split_by = "y")
#'
#' # Per-split palettes
#' ChordPlot(data, from = "nodes1", to = "nodes2", split_by = "y",
#'           palette = c("1" = "Reds", "2" = "Blues",
#'                       "3" = "Greens", "4" = "Purp"))
#'
#' # Flip source/target direction
#' ChordPlot(data, from = "nodes1", to = "nodes2", flip = TRUE)
#' }
ChordPlot <- function(
    data,
    y = NULL,
    from = NULL,
    from_sep = "_",
    to = NULL,
    to_sep = "_",
    split_by = NULL,
    split_by_sep = "_",
    flip = FALSE,
    links_color = c("from", "to"),
    theme = "theme_this",
    theme_args = list(),
    palette = "Paired",
    palcolor = NULL,
    palreverse = FALSE,
    alpha = 0.5,
    labels_rot = FALSE,
    title = NULL,
    subtitle = NULL,
    seed = 8525,
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
    validate_common_args(seed)
    keep_na <- check_keep_na(keep_na, c(split_by, from, to))
    keep_empty <- check_keep_empty(keep_empty, c(split_by, from, to))
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
            ChordPlotAtomic(
                datas[[nm]],
                y = y,
                from = from,
                from_sep = from_sep,
                to = to,
                to_sep = to_sep,
                flip = flip,
                links_color = links_color,
                theme = theme,
                theme_args = theme_args,
                palette = palette[[nm]],
                palcolor = palcolor[[nm]],
                palreverse = palreverse,
                alpha = alpha,
                labels_rot = labels_rot,
                title = title,
                subtitle = subtitle,
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

#' @rdname chordplot
#' @description
#' \code{CircosPlot} is an alias for \code{\link{ChordPlot}}.
#' @export
CircosPlot <- ChordPlot
