#' Atomic chord plot
#'
#' @inheritParams common_args
#' @param from A character string of the column name to plot for the source.
#'   A character/factor column is expected.
#' @param from_sep A character string to concatenate the columns in `from`, if multiple columns are provided.
#' @param to A character string of the column name to plot for the target.
#'   A character/factor column is expected.
#' @param to_sep A character string to concatenate the columns in `to`, if multiple columns are provided.
#' @param flip A logical value to flip the source and target.
#' @param labels_rot A logical value to rotate the labels by 90 degrees.
#' @param links_color A character string to specify the color of the links.
#'   Either "from" or "to".
#' @return A wrapped element of chord plot
#' @importFrom dplyr %>% group_by summarise n select
#' @importFrom patchwork wrap_elements plot_annotation
#' @keywords internal
ChordPlotAtomic <- function(
    data, y = NULL, from = NULL, from_sep = "_", to = NULL, to_sep = "_", flip = FALSE, links_color = c("from", "to"),
    theme = "theme_this", theme_args = list(), palette = "Paired", palcolor = NULL, alpha = 0.5,
    labels_rot = FALSE, title = NULL, subtitle = NULL, ...
) {
    # if (!requireNamespace("circlize", quietly = TRUE)) {
    #     stop("circlize is required for chord plot.")
    # }
    links_color <- match.arg(links_color)
    from <- check_columns(data, from, force_factor = TRUE, allow_multi = TRUE, concat_multi = TRUE, concat_sep = from_sep)
    to <- check_columns(data, to, force_factor = TRUE, allow_multi = TRUE, concat_multi = TRUE, concat_sep = to_sep)
    y <- check_columns(data, y)
    if (is.null(y)) {
        data <- data %>%
            group_by(!!sym(from), !!sym(to)) %>%
            summarise(.y = n(), .groups = "drop")

        y <- ".y"
    }

    if (isTRUE(flip)) {
        data <- data %>% select(from = !!sym(to), to = !!sym(from), value = !!sym(y))
    } else {
        data <- data %>% select(from = !!sym(from), to = !!sym(to), value = !!sym(y))
    }

    data <- data[order(data$from, data$to), , drop = FALSE]
    data$from <- as.character(data$from)
    data$to <- as.character(data$to)

    froms <- unique(data$from)
    tos <- unique(data$to)
    grid_cols <- palette_this(c(froms, tos), palette = palette, palcolor = palcolor)
    link_cols <- grid_cols[data[[links_color]]]

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
            circlize::circos.track(track.index = 1, panel.fun = function(x, y) {
                circlize::circos.text(
                    circlize::CELL_META$xcenter, circlize::CELL_META$ylim[1] + 5.5,
                    circlize::CELL_META$sector.index,
                    niceFacing = TRUE,
                    adj = c(0.5, 0.5))
            }, bg.border = NA)
            circlize::circos.track(track.index = 2, panel.fun = function(x, y) {
                for (si in circlize::get.all.sector.index()) {
                    start.degree <- circlize::get.cell.meta.data("cell.start.degree", sector.index = si)
                    end.degree <- circlize::get.cell.meta.data("cell.end.degree", sector.index = si)
                    if (abs(end.degree - start.degree) > 2) {
                        # otherwise: patchwork wrap_elements 'x' and 'units' must have length > 0
                        circlize::circos.axis(h = "top", labels.cex = 0.7, labels.niceFacing = TRUE, sector.index = si)
                    }
                }
            }, bg.border = NA) # here set bg.border to NA is important
        }
    } else {
        allnames <- unique(c(froms, tos))
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
            circlize::circos.track(track.index = 1, panel.fun = function(x, y) {
                circlize::circos.text(
                    circlize::CELL_META$xcenter, circlize::CELL_META$ylim[1] + .15,
                    circlize::CELL_META$sector.index,
                    facing = "clockwise",
                    niceFacing = TRUE,
                    adj = c(0, 0.5))
            }, bg.border = NA)
            circlize::circos.track(track.index = 2, panel.fun = function(x, y) {
                for (si in circlize::get.all.sector.index()) {
                    start.degree <- circlize::get.cell.meta.data("cell.start.degree", sector.index = si)
                    end.degree <- circlize::get.cell.meta.data("cell.end.degree", sector.index = si)
                    if (abs(end.degree - start.degree) > 2) {
                        # otherwise: patchwork wrap_elements 'x' and 'units' must have length > 0
                        circlize::circos.axis(h = "top", labels.cex = 0.7, labels.niceFacing = TRUE, sector.index = si)
                    }
                }
            }, bg.border = NA) # here set bg.border to NA is important
        }
    }
    p <- wrap_elements(full = p)

    if (!is.null(title) || !is.null(subtitle)) {
        p <- p + plot_annotation(title = title, subtitle = subtitle, theme = do.call(theme, theme_args))
    }

    base_size <- 7
    if (isTRUE(labels_rot)) {
        maxchar <- max(c(nchar(froms), nchar(tos)))
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
#' @description `ChordPlot` is used to create a chord plot to visualize the relationships between two categorical variables.
#'  `CircosPlot` is an alias of `ChordPlot`.
#' @inheritParams common_args
#' @inheritParams ChordPlotAtomic
#' @return A combined plot or a list of plots
#' @rdname chordplot
#' @export
#' @examples
#' \dontrun{
#' set.seed(8525)
#' data <- data.frame(
#'     nodes1 = sample(c("Soure1", "Source2", "Source3"), 10, replace = TRUE),
#'     nodes2 = sample(letters[1:3], 10, replace = TRUE),
#'     y = sample(1:5, 10, replace = TRUE)
#' )
#'
#' ChordPlot(data, from = "nodes1", to = "nodes2")
#' ChordPlot(data, from = "nodes1", to = "nodes2",
#'           links_color = "to", labels_rot = TRUE)
#' ChordPlot(data, from = "nodes1", to = "nodes2", y = "y")
#' ChordPlot(data, from = "nodes1", to = "nodes2", split_by = "y")
#' ChordPlot(data, from = "nodes1", to = "nodes2", flip = TRUE)
#' }
ChordPlot <- function(
    data, y = NULL, from = NULL, from_sep = "_", to = NULL, to_sep = "_",
    split_by = NULL, split_by_sep = "_", flip = FALSE, links_color = c("from", "to"),
    theme = "theme_this", theme_args = list(), palette = "Paired", palcolor = NULL, alpha = 0.5,
    labels_rot = FALSE, title = NULL, subtitle = NULL, seed = 8525,
    combine = TRUE, nrow = NULL, ncol = NULL, byrow = TRUE, ...
) {
    validate_common_args(seed)
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

    plots <- lapply(
        names(datas), function(nm) {
            default_title <- if (length(datas) == 1 && identical(nm, "...")) NULL else nm
            if (is.function(title)) {
                title <- title(default_title)
            } else {
                title <- title %||% default_title
            }
            ChordPlotAtomic(datas[[nm]],
                y = y, from = from, from_sep = from_sep, to = to, to_sep = to_sep, flip = flip, links_color = links_color,
                theme = theme, theme_args = theme_args, palette = palette, palcolor = palcolor, alpha = alpha,
                labels_rot = labels_rot, title = title, subtitle = subtitle, ...
            )
        }
    )

    combine_plots(plots, combine = combine, nrow = nrow, ncol = ncol, byrow = byrow)
}

#' @export
#' @rdname chordplot
CircosPlot <- ChordPlot
