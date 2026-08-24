#' Compute velocity on a regular grid from sparse cell embeddings
#'
#' Computes velocity vectors on a regular grid by averaging cell-level velocities
#' within a Gaussian-weighted neighborhood of each grid point. This function is
#' adapted from the scvelo Python implementation at
#' \url{https://github.com/theislab/scvelo/blob/master/scvelo/plotting/velocity_embedding_grid.py}.
#'
#' @param embedding A numeric matrix of dimension n_obs x n_dim containing the low-dimensional embedding coordinates of each cell.
#' @param v_embedding A numeric matrix of dimension n_obs x n_dim containing the velocity vectors for each cell.
#' @param density A numeric value specifying the density of the grid points along each dimension. Higher values produce a finer grid. Default is 1.
#' @param smooth A numeric value specifying the standard deviation multiplier for the Gaussian kernel used to weight neighboring cells when averaging velocities onto grid points. Default is 0.5.
#' @param n_neighbors An integer value specifying the number of nearest neighbors to consider for each grid point when computing the weighted average velocity. Default is \code{ceiling(n_obs / 50)}.
#' @param min_mass A numeric value specifying the minimum mass threshold. Grid points with total weight below this threshold are filtered out. When \code{adjust_for_stream = TRUE}, this is interpreted on a logarithmic scale (\code{10^(min_mass - 6)}). Default is 1.
#' @param scale A numeric value specifying the scaling factor to apply to the resulting grid velocity vectors. Only used when \code{adjust_for_stream = FALSE}. Default is 1.
#' @param adjust_for_stream A logical value indicating whether to adjust the output for streamline rendering. When \code{TRUE}, returns a 2D array format with different filtering logic suitable for \code{metR::geom_streamline}. Default is \code{FALSE}.
#' @param cutoff_perc A numeric value specifying the percentile cutoff for removing low-density grid points. Only used when \code{adjust_for_stream = TRUE}. Default is 5.
#' @return A list with components \code{x_grid} (grid point coordinates) and \code{v_grid} (velocity vectors at each grid point). When \code{adjust_for_stream = TRUE}, \code{x_grid} is a 2-row matrix of unique coordinates and \code{v_grid} is a 3D array.
#' @keywords internal
.compute_velocity_on_grid <- function(
    embedding,
    v_embedding,
    density = NULL,
    smooth = NULL,
    n_neighbors = NULL,
    min_mass = NULL,
    scale = 1,
    adjust_for_stream = FALSE,
    cutoff_perc = NULL
) {
    n_obs <- nrow(embedding)
    n_dim <- ncol(embedding)

    density <- density %||% 1
    smooth <- smooth %||% 0.5
    n_neighbors <- n_neighbors %||% ceiling(n_obs / 50)
    min_mass <- min_mass %||% 1
    cutoff_perc <- cutoff_perc %||% 5

    grs <- list()
    for (dim_i in 1:n_dim) {
        m <- min(embedding[, dim_i], na.rm = TRUE)
        M <- max(embedding[, dim_i], na.rm = TRUE)
        # m <- m - 0.01 * abs(M - m)
        # M <- M + 0.01 * abs(M - m)
        gr <- seq(m, M, length.out = ceiling(50 * density))
        grs <- c(grs, list(gr))
    }
    x_grid <- as.matrix(expand.grid(grs))

    d <- proxyC::dist(
        x = Matrix::Matrix(as.matrix(embedding), sparse = TRUE),
        y = Matrix::Matrix(x_grid, sparse = TRUE),
        method = "euclidean",
        use_nan = TRUE
    )

    neighbors <- t(as.matrix(apply(d, 2, function(x) {
        order(x, decreasing = FALSE)[1:n_neighbors]
    })))
    dists <- t(as.matrix(apply(d, 2, function(x) {
        x[order(x, decreasing = FALSE)[1:n_neighbors]]
    })))

    weight <- stats::dnorm(
        dists,
        sd = mean(sapply(grs, function(g) g[2] - g[1])) * smooth
    )
    p_mass <- p_mass_v <- rowSums(weight)
    p_mass_v[p_mass_v < 1] <- 1

    neighbors_emb <- array(
        as.matrix(v_embedding)[neighbors, seq_len(ncol(v_embedding))],
        dim = c(dim(neighbors), dim(v_embedding)[2])
    )
    v_grid <- apply((neighbors_emb * c(weight)), c(1, 3), sum)
    v_grid <- v_grid / p_mass_v

    if (isTRUE(adjust_for_stream)) {
        x_grid <- matrix(
            c(unique(x_grid[, 1]), unique(x_grid[, 2])),
            nrow = 2,
            byrow = TRUE
        )
        ns <- floor(sqrt(length(v_grid[, 1])))
        v_grid <- array(t(v_grid), dim = c(2, ns, ns))

        mass <- sqrt(apply(v_grid**2, c(2, 3), sum))
        min_mass <- 10**(min_mass - 6) # default min_mass = 1e-5
        min_mass[min_mass > max(mass, na.rm = TRUE) * 0.9] <- max(
            mass,
            na.rm = TRUE
        ) *
            0.9
        cutoff <- array(t(mass), dim = c(ns, ns)) < min_mass

        lens <- t(apply(apply(abs(neighbors_emb), c(1, 3), mean), 1, sum))
        lens <- array(t(lens), dim = c(ns, ns))

        cutoff <- cutoff | lens < quantile(lens, cutoff_perc / 100)
        v_grid[1, , ][cutoff] <- NA
    } else {
        min_mass <- min_mass * quantile(p_mass, 0.99) / 100
        x_grid <- x_grid[p_mass > min_mass, , drop = FALSE]
        v_grid <- v_grid[p_mass > min_mass, , drop = FALSE]
        if (!is.null(scale)) {
            v_grid <- v_grid * scale
        }
    }

    return(list(x_grid = x_grid, v_grid = v_grid))
}

#' Cell velocity plot
#'
#' Plots RNA velocity vectors on a low-dimensional embedding (e.g., UMAP, t-SNE)
#' to visualize the direction and magnitude of cellular state transitions. Supports
#' three visualization modes: raw arrows at each cell position, arrows on a regular
#' grid, and streamline paths. Optionally colors arrows by cell metadata groups.
#'
#' @section Rendering Pipeline:
#' The \code{VelocityPlot} function proceeds through the following steps:
#' \enumerate{
#'   \item{\strong{Input validation} --- Verifies that \code{embedding} and \code{v_embedding} are matrices or data frames of equal dimensions, that \code{group_by} matches the number of rows (if provided), and that \code{split_by} is \code{NULL} (unsupported and raises an error).}
#'   \item{\strong{Axis label resolution} --- Uses the column names of \code{embedding} as axis labels, falling back to \code{"Reduction 1"} and \code{"Reduction 2"} when column names are \code{NULL}.}
#'   \item{\strong{Grouping setup} --- Converts \code{group_by} to a factor and applies \code{keep_na} / \code{keep_empty} logic to filter or recode missing values and empty factor levels.}
#'   \item{\strong{Plot-type dispatch} --- Branches on \code{plot_type}:}
#'   \itemize{
#'     \item{\strong{raw} --- Optionally subsamples cells when \code{density < 1}, scales velocity vectors by \code{scale}, computes arrow lengths proportional to the embedding range, and renders \code{\link[ggplot2]{geom_segment}} with arrowheads. When \code{group_by} is provided, arrows are colored by group using \code{group_palette}.}
#'     \item{\strong{grid} --- Delegates to \code{\link{.compute_velocity_on_grid}} to interpolate the sparse cell velocities onto a regular grid, then renders \code{\link[ggplot2]{geom_segment}} with arrowheads at each grid point. \code{group_by} is ignored with a warning.}
#'     \item{\strong{stream} --- Delegates to \code{\link{.compute_velocity_on_grid}} with \code{adjust_for_stream = TRUE}, then renders smooth streamline paths via \code{\link[metR]{geom_streamline}}. When \code{streamline_color} is provided, streamlines use a fixed color with a background stroke; when \code{NULL}, streamlines are colored by velocity magnitude using \code{streamline_palette}. \code{group_by} is ignored with a warning.}
#'   }
#'   \item{\strong{Layer return or plot assembly} --- If \code{return_layer = TRUE}, returns the list of ggplot layers. Otherwise, constructs a full \code{ggplot} object with labels, theme, aspect ratio, legend configuration, and \code{height} / \code{width} attributes via \code{\link{calculate_plot_dimensions}()}.}
#' }
#'
#' @inheritParams common_args
#' @param embedding A matrix or data frame of dimension n_obs x n_dim specifying the low-dimensional embedding coordinates (e.g., UMAP, t-SNE) of the cells. The first two columns are used for the x and y axes.
#' @param v_embedding A matrix or data frame of dimension n_obs x n_dim specifying the velocity vectors for each cell. Must have the same dimensions as \code{embedding}.
#' @param plot_type A character string specifying the visualization method. \code{"raw"} plots arrows directly from each cell's embedding position. \code{"grid"} averages velocities onto a regular grid and plots arrows at grid points. \code{"stream"} computes smooth streamline paths from the gridded velocity field. Default is \code{"raw"}.
#' @param split_by Not supported for VelocityPlot. Setting this parameter will raise an error.
#' @param group_by An optional vector of the same length as the number of rows in \code{embedding} specifying a grouping variable for cells. When provided, arrows are colored by group using \code{group_palette}. Only applies to \code{plot_type = "raw"}; ignored with a warning for \code{"grid"} and \code{"stream"}. Default is \code{NULL}.
#' @param group_name A character string specifying the legend title for the grouping variable. Default is \code{"Group"}.
#' @param group_palette A character string specifying the color palette to use for the grouping variable. Passed to \code{\link{palette_this}}. Default is \code{"Paired"}.
#' @param group_palcolor An optional character vector of specific colors for the grouping variable. If \code{NULL}, colors are generated from \code{group_palette}. Default is \code{NULL}.
#' @param n_neighbors An integer value specifying the number of nearest neighbors for computing grid velocities. Only used when \code{plot_type} is \code{"grid"} or \code{"stream"}. Default is \code{ceiling(nrow(embedding) / 50)}.
#' @param density A numeric value specifying the grid density along each dimension. Only used when \code{plot_type} is \code{"grid"} or \code{"stream"}. For \code{plot_type = "raw"}, when \code{density} is between 0 and 1, it specifies the fraction of cells to randomly subsample. Default is 1.
#' @param smooth A numeric value specifying the standard deviation multiplier for the Gaussian kernel when averaging cell velocities onto grid points. Only used when \code{plot_type} is \code{"grid"} or \code{"stream"}. Default is 0.5.
#' @param scale A numeric value specifying the scaling factor for the velocity vectors. Applied to raw and grid arrows. For \code{plot_type = "stream"}, this is fixed to 1 internally. Default is 1.
#' @param min_mass A numeric value specifying the minimum mass threshold for retaining grid points. Only used when \code{plot_type} is \code{"grid"} or \code{"stream"}. Default is 1.
#' @param cutoff_perc A numeric value specifying the percentile cutoff for removing low-density grid points. Only used when \code{plot_type} is \code{"stream"}. Default is 5.
#' @param arrow_angle A numeric value specifying the angle of the arrowheads in degrees. Applied to \code{\link[grid]{arrow}} when \code{plot_type} is \code{"raw"} or \code{"grid"}. Default is 20.
#' @param arrow_color A character string specifying the color of the velocity arrows. For \code{plot_type = "stream"}, this sets only the arrowhead color. Default is \code{"black"}.
#' @param arrow_alpha A numeric value between 0 and 1 specifying the transparency of the velocity arrows. Only used when \code{plot_type = "raw"} or \code{"grid"}; for \code{plot_type = "stream"}, use \code{streamline_alpha} instead. Default is 1.
#' @param keep_na A logical or character value specifying how to handle NA values in \code{group_by}. Unlike other plot functions, VelocityPlot does not support named lists for per-column control. See \code{keep_na} in \code{common_args} for details of supported values. Default is \code{FALSE}.
#' @param keep_empty One of \code{FALSE}, \code{TRUE}, or \code{"level"} specifying how to handle empty factor levels in \code{group_by}. Unlike other plot functions, VelocityPlot does not support named lists for per-column control. See \code{keep_empty} in \code{common_args} for details. Default is \code{FALSE}.
#' @param streamline_l A numeric value specifying the integration length of the streamlines. Passed to \code{\link[metR]{geom_streamline}} as the \code{L} parameter. Default is 5.
#' @param streamline_minl A numeric value specifying the minimum streamline length. Shorter streamlines are not drawn. Passed to \code{\link[metR]{geom_streamline}} as the \code{min.L} parameter. Default is 1.
#' @param streamline_res A numeric value specifying the resolution of the streamline integration. Passed to \code{\link[metR]{geom_streamline}} as the \code{res} parameter. Default is 1.
#' @param streamline_n A numeric value specifying the number of streamlines to draw. Passed to \code{\link[metR]{geom_streamline}} as the \code{n} parameter. Default is 15.
#' @param streamline_width A numeric vector of length 2 specifying the range of line widths for streamlines. Passed to \code{scale_size(range = ...)}. Only used when \code{streamline_color} is \code{NULL}. Default is \code{c(0, 0.8)}.
#' @param streamline_alpha A numeric value between 0 and 1 specifying the transparency of the velocity streamlines. Default is 1.
#' @param streamline_color An optional character string specifying a fixed color for streamlines. When \code{NULL} (the default), streamlines are colored by velocity magnitude using \code{streamline_palette}.
#' @param streamline_palette A character string specifying the color palette for streamline velocity magnitude. Passed to \code{\link{palette_this}}. Only used when \code{streamline_color} is \code{NULL}. Default is \code{"RdYlBu"}.
#' @param streamline_palcolor An optional character vector of specific colors for the streamline velocity gradient. If \code{NULL}, colors are generated from \code{streamline_palette}. Default is \code{NULL}.
#' @param streamline_bg_color A character string specifying the background (outline) color applied to streamlines to create a stroke effect. Default is \code{"white"}.
#' @param streamline_bg_stroke A numeric value specifying the additional line width of the background stroke relative to the foreground streamline. Default is 0.5.
#' @param return_layer A logical value indicating whether to return only the ggplot layers instead of the full assembled plot. When \code{TRUE}, returns a list of ggplot layers suitable for combining with other ggplot objects. Default is \code{FALSE}.
#' @return A ggplot object representing the cell velocity plot, with \code{height} and \code{width} attributes set for consistent rendering. If \code{return_layer = TRUE}, returns a list of ggplot layers instead.
#' @importFrom rlang %||% sym
#' @importFrom ggplot2 geom_segment scale_color_gradientn scale_size after_stat
#' @export
#' @seealso \code{\link{DimPlot}} \code{\link{FeatureDimPlot}}
#' @examples
#' \donttest{
#' data(dim_example)
#' dim_example$clusters[dim_example$clusters == "Ductal"] <- NA
#'
#' # Basic velocity plot with group coloring
#' VelocityPlot(dim_example[, 1:2], dim_example[, 3:4], group_by = dim_example$clusters)
#'
#' # Handle NA groups with keep_na / keep_empty
#' VelocityPlot(dim_example[, 1:2], dim_example[, 3:4], group_by = dim_example$clusters,
#'     keep_na = TRUE, keep_empty = TRUE)
#' VelocityPlot(dim_example[, 1:2], dim_example[, 3:4], group_by = dim_example$clusters,
#'     keep_na = TRUE, keep_empty = 'level')
#' VelocityPlot(dim_example[, 1:2], dim_example[, 3:4], group_by = dim_example$clusters,
#'     keep_na = TRUE, keep_empty = FALSE)
#' }
VelocityPlot <- function(
    embedding,
    v_embedding,
    plot_type = c("raw", "grid", "stream"),
    split_by = NULL,
    group_by = NULL,
    group_name = "Group",
    group_palette = "Paired",
    group_palcolor = NULL,
    n_neighbors = NULL,
    density = 1,
    smooth = 0.5,
    scale = 1,
    min_mass = 1,
    cutoff_perc = 5,
    arrow_angle = 20,
    arrow_color = "black",
    arrow_alpha = 1,
    keep_na = FALSE,
    keep_empty = FALSE,
    streamline_l = 5,
    streamline_minl = 1,
    streamline_res = 1,
    streamline_n = 15,
    streamline_width = c(0, 0.8),
    streamline_alpha = 1,
    streamline_color = NULL,
    streamline_palette = "RdYlBu",
    streamline_palcolor = NULL,
    palreverse = FALSE,
    streamline_bg_color = "white",
    streamline_bg_stroke = 0.5,
    aspect.ratio = 1,
    title = "Cell velocity",
    subtitle = NULL,
    xlab = NULL,
    ylab = NULL,
    legend.position = "right",
    legend.direction = "vertical",
    theme = "theme_this",
    theme_args = list(),
    return_layer = FALSE,
    seed = 8525
) {
    ggplot <- if (getOption("plotthis.gglogger.enabled", FALSE)) {
        gglogger::ggplot
    } else {
        ggplot2::ggplot
    }
    stopifnot(
        "[VelocityPlot] 'split_by' is not supported yet" = is.null(split_by)
    )
    stopifnot(
        "[VelocityPlot] 'keep_na' supports only atomic values (logical or character)" = is.atomic(
            keep_na
        ) &&
            (is.logical(keep_na) || is.character(keep_na))
    )
    stopifnot(
        "[VelocityPlot] 'keep_empty' supports only atomic values (logical or 'level'/'levels')" = is.atomic(
            keep_empty
        ) &&
            (is.logical(keep_empty) ||
                (is.character(keep_empty) &&
                    keep_empty %in% c("level", "levels")))
    )
    stopifnot(
        "[VelocityPlot] 'embedding' must be a matrix or data.frame" = is.matrix(
            embedding
        ) ||
            is.data.frame(embedding)
    )
    stopifnot(
        "[VelocityPlot] 'v_embedding' must be a matrix or data.frame" = is.matrix(
            v_embedding
        ) ||
            is.data.frame(v_embedding)
    )
    stopifnot(
        "[VelocityPlot] 'embedding' and 'v_embedding' must have the same dimensions" = all(
            dim(embedding) == dim(v_embedding)
        )
    )
    stopifnot(
        "[VelocityPlot] 'group_by' must be NULL or a vector of the same length as the number of rows in 'embedding'" = is.null(
            group_by
        ) ||
            (length(group_by) == nrow(embedding))
    )

    set.seed(seed)

    plot_type <- match.arg(plot_type)
    reduc_names <- colnames(embedding)
    if (is.null(reduc_names)) {
        xlab <- xlab %||% "Reduction 1"
        ylab <- ylab %||% "Reduction 2"
    } else {
        xlab <- xlab %||% reduc_names[1]
        ylab <- ylab %||% reduc_names[2]
    }

    if (identical(theme, "theme_blank")) {
        theme_args[["xlab"]] <- xlab
        theme_args[["ylab"]] <- ylab
    }
    n_neighbors <- n_neighbors %||% ceiling(nrow(embedding) / 50)

    if (!is.null(group_by)) {
        group_by <- as.factor(group_by)
    }

    if (
        !is.null(group_by) &&
            anyNA(group_by) &&
            !isTRUE(keep_na) &&
            !is.na(keep_na)
    ) {
        if (isFALSE(keep_na)) {
            row_idxes <- is.na(group_by)
            group_by <- group_by[!row_idxes]
            embedding <- embedding[!row_idxes, , drop = FALSE]
            v_embedding <- v_embedding[!row_idxes, , drop = FALSE]
        } else {
            keep_na <- as.character(keep_na)
            levels(group_by) <- c(levels(group_by), keep_na)
            group_by[is.na(group_by)] <- keep_na
        }
    }
    if (!is.null(group_by) && isFALSE(keep_empty)) {
        group_by <- droplevels(group_by)
    }

    if (plot_type == "raw") {
        if (!is.null(density) && (density > 0 && density < 1)) {
            s <- ceiling(density * nrow(embedding))
            ix_choice <- sample(
                seq_len(nrow(embedding)),
                size = s,
                replace = FALSE
            )
            embedding <- embedding[ix_choice, ]
            v_embedding <- v_embedding[ix_choice, ]
        }
        if (!is.null(scale)) {
            v_embedding <- v_embedding * scale
        }
        df <- cbind.data.frame(embedding, v_embedding)
        colnames(df) <- c("x", "y", "u", "v")
        df$length <- sqrt(df[["u"]]^2 + df[["v"]]^2)
        global_size <- sqrt(
            max(df$x, na.rm = TRUE)^2 + max(df$y, na.rm = TRUE)^2
        )
        df$length_perc <- df$length / global_size

        if (!is.null(group_by)) {
            df[[group_name]] <- group_by
            group_vals <- levels(group_by)
            if (anyNA(group_by)) {
                group_vals <- c(group_vals, NA)
            }
            group_cols <- palette_this(
                group_vals,
                palette = group_palette,
                palcolor = group_palcolor,
                NA_keep = TRUE,
                reverse = palreverse
            )
            velocity_layer <- list(
                geom_segment(
                    data = df,
                    mapping = aes(
                        x = !!sym("x"),
                        y = !!sym("y"),
                        xend = !!sym("x") + !!sym("u"),
                        yend = !!sym("y") + !!sym("v"),
                        color = !!sym(group_name)
                    ),
                    alpha = arrow_alpha,
                    arrow = if (
                        utils::compareVersion(
                            as.character(utils::packageVersion("ggplot2")),
                            "4"
                        ) !=
                            0
                    ) {
                        arrow(
                            length = unit(df$length_perc, "npc"),
                            type = "closed",
                            angle = arrow_angle
                        )
                    } else {
                        warning(
                            "[VelocityPlot] 'arrow()' in ggplot2 == 4.0.0 does not support varying lengths. Using fixed length instead. See https://github.com/tidyverse/ggplot2/issues/6594 for details."
                        )
                        NULL
                    },
                    lineend = "round",
                    linejoin = "mitre",
                    inherit.aes = FALSE,
                    show.legend = TRUE
                )
            )
            if (isTRUE(keep_empty)) {
                velocity_layer[[
                    length(velocity_layer) + 1
                ]] <- scale_color_manual(
                    name = group_name,
                    values = group_cols,
                    na.value = group_cols["NA"] %||% "grey80",
                    breaks = group_vals,
                    limits = group_vals,
                    drop = FALSE,
                    guide = guide_legend(
                        title.hjust = 0,
                        order = 1,
                        override.aes = list(linewidth = 2, alpha = 1)
                    )
                )
            } else {
                velocity_layer[[
                    length(velocity_layer) + 1
                ]] <- scale_color_manual(
                    name = group_name,
                    values = group_cols,
                    na.value = group_cols["NA"] %||% "grey80",
                    guide = guide_legend(
                        title.hjust = 0,
                        order = 1,
                        override.aes = list(linewidth = 2, alpha = 1)
                    )
                )
            }
            attr(velocity_layer, "scales") <- unique(c(
                attr(velocity_layer, "scales"),
                "color"
            ))
        } else {
            velocity_layer <- list(
                geom_segment(
                    data = df,
                    aes(
                        x = !!sym("x"),
                        y = !!sym("y"),
                        xend = !!sym("x") + !!sym("u"),
                        yend = !!sym("y") + !!sym("v")
                    ),
                    color = arrow_color,
                    alpha = arrow_alpha,
                    arrow = arrow(
                        length = unit(df$length_perc, "npc"),
                        type = "closed",
                        angle = arrow_angle
                    ),
                    lineend = "round",
                    linejoin = "mitre",
                    inherit.aes = FALSE
                )
            )
        }
    } else if (plot_type == "grid") {
        if (!is.null(group_by)) {
            warning(
                "[VelocityPlot] Ignoring 'group_by', which is not supported for 'grid' plot type."
            )
        }
        res <- .compute_velocity_on_grid(
            embedding,
            v_embedding,
            density = density,
            smooth = smooth,
            n_neighbors = n_neighbors,
            min_mass = min_mass,
            scale = scale
        )
        x_grid <- res$x_grid
        v_grid <- res$v_grid

        df <- cbind.data.frame(x_grid, v_grid)
        colnames(df) <- c("x", "y", "u", "v")
        df$length <- sqrt(df[["u"]]^2 + df[["v"]]^2)
        global_size <- sqrt(
            max(df$x, na.rm = TRUE)^2 + max(df$y, na.rm = TRUE)^2
        )
        df$length_perc <- df$length / global_size
        velocity_layer <- list(
            geom_segment(
                data = df,
                aes(
                    x = !!sym("x"),
                    y = !!sym("y"),
                    xend = !!sym("x") + !!sym("u"),
                    yend = !!sym("y") + !!sym("v")
                ),
                color = arrow_color,
                alpha = arrow_alpha,
                arrow = arrow(
                    length = unit(df$length_perc, "npc"),
                    type = "closed",
                    angle = arrow_angle
                ),
                lineend = "round",
                linejoin = "mitre",
                inherit.aes = FALSE
            )
        )
    } else if (plot_type == "stream") {
        if (!is.null(group_by)) {
            warning(
                "[VelocityPlot] Ignoring 'group_by', which is not supported for 'stream' plot type."
            )
        }
        res <- .compute_velocity_on_grid(
            embedding,
            v_embedding,
            density = density,
            smooth = smooth,
            n_neighbors = n_neighbors,
            min_mass = min_mass,
            scale = 1,
            cutoff_perc = cutoff_perc,
            adjust_for_stream = TRUE
        )
        x_grid <- res$x_grid
        v_grid <- res$v_grid

        df <- expand.grid(x_grid[1, ], x_grid[2, ])
        colnames(df) <- c("x", "y")
        u <- reshape2::melt(t(v_grid[1, , ]))
        v <- reshape2::melt(t(v_grid[2, , ]))
        df[, "u"] <- u$value
        df[, "v"] <- v$value
        df[is.na(df)] <- 0

        if (!is.null(streamline_color)) {
            velocity_layer <- list(
                # Streamline background layer for stroke effect
                metR::geom_streamline(
                    data = df,
                    aes(
                        x = !!sym("x"),
                        y = !!sym("y"),
                        dx = !!sym("u"),
                        dy = !!sym("v")
                    ),
                    L = streamline_l,
                    min.L = streamline_minl,
                    res = streamline_res,
                    n = streamline_n,
                    linewidth = max(streamline_width, na.rm = TRUE) +
                        streamline_bg_stroke,
                    color = streamline_bg_color,
                    alpha = streamline_alpha,
                    arrow.type = "closed",
                    arrow.angle = arrow_angle,
                    lineend = "round",
                    linejoin = "mitre",
                    inherit.aes = FALSE
                ),
                # Streamline foreground layer with fixed color
                metR::geom_streamline(
                    data = df,
                    aes(
                        x = !!sym("x"),
                        y = !!sym("y"),
                        dx = !!sym("u"),
                        dy = !!sym("v")
                    ),
                    L = streamline_l,
                    min.L = streamline_minl,
                    res = streamline_res,
                    n = streamline_n,
                    linewidth = max(streamline_width, na.rm = TRUE),
                    color = streamline_color,
                    alpha = streamline_alpha,
                    arrow.type = "closed",
                    arrow.angle = arrow_angle,
                    lineend = "round",
                    linejoin = "mitre",
                    inherit.aes = FALSE
                ),
                # Streamline arrowhead layer
                metR::geom_streamline(
                    data = df,
                    aes(
                        x = !!sym("x"),
                        y = !!sym("y"),
                        dx = !!sym("u"),
                        dy = !!sym("v")
                    ),
                    L = streamline_l,
                    min.L = streamline_minl,
                    res = streamline_res,
                    n = streamline_n,
                    linetype = 0,
                    color = arrow_color,
                    arrow.type = "closed",
                    arrow.angle = arrow_angle,
                    lineend = "round",
                    linejoin = "mitre",
                    inherit.aes = FALSE
                )
            )
        } else {
            velocity_layer <- list(
                # Streamline background layer for stroke effect
                metR::geom_streamline(
                    data = df,
                    aes(
                        x = !!sym("x"),
                        y = !!sym("y"),
                        dx = !!sym("u"),
                        dy = !!sym("v")
                    ),
                    L = streamline_l,
                    min.L = streamline_minl,
                    res = streamline_res,
                    n = streamline_n,
                    linewidth = max(streamline_width, na.rm = TRUE) +
                        streamline_bg_stroke,
                    color = streamline_bg_color,
                    alpha = streamline_alpha,
                    arrow.type = "closed",
                    arrow.angle = arrow_angle,
                    lineend = "round",
                    linejoin = "mitre",
                    inherit.aes = FALSE
                ),
                # Streamline foreground layer colored by velocity magnitude
                metR::geom_streamline(
                    data = df,
                    aes(
                        x = !!sym("x"),
                        y = !!sym("y"),
                        dx = !!sym("u"),
                        dy = !!sym("v"),
                        linewidth = after_stat(!!sym("step")),
                        color = sqrt(
                            after_stat(!!sym("dx"))^2 +
                                after_stat(!!sym("dy"))^2
                        )
                    ),
                    L = streamline_l,
                    min.L = streamline_minl,
                    res = streamline_res,
                    n = streamline_n,
                    alpha = streamline_alpha,
                    arrow = NULL,
                    lineend = "round",
                    linejoin = "mitre",
                    inherit.aes = FALSE
                ),
                # Streamline arrowhead layer
                metR::geom_streamline(
                    data = df,
                    aes(
                        x = !!sym("x"),
                        y = !!sym("y"),
                        dx = !!sym("u"),
                        dy = !!sym("v")
                    ),
                    L = streamline_l,
                    min.L = streamline_minl,
                    res = streamline_res,
                    n = streamline_n,
                    linetype = 0,
                    color = arrow_color,
                    arrow.type = "closed",
                    arrow.angle = arrow_angle,
                    lineend = "round",
                    linejoin = "mitre",
                    inherit.aes = FALSE
                ),
                # Color scale for velocity magnitude
                scale_color_gradientn(
                    name = "Velocity",
                    n.breaks = 4,
                    colors = palette_this(
                        palette = streamline_palette,
                        palcolor = streamline_palcolor,
                        reverse = palreverse
                    ),
                    guide = guide_colorbar(
                        frame.colour = "black",
                        ticks.colour = "black",
                        title.hjust = 0,
                        order = 1
                    )
                ),
                # Size scale for streamline width
                scale_size(range = range(streamline_width), guide = "none")
            )
            attr(velocity_layer, "scales") <- unique(c(
                attr(velocity_layer, "scales"),
                "color"
            ))
        }
    }

    if (isTRUE(return_layer)) {
        return(velocity_layer)
    }

    p <- ggplot() +
        velocity_layer +
        labs(title = title, subtitle = subtitle, x = xlab, y = ylab) +
        do_call(theme, theme_args) +
        ggplot2::theme(
            aspect.ratio = aspect.ratio,
            legend.position = legend.position,
            legend.direction = legend.direction
        )

    dims <- calculate_plot_dimensions(
        base_height = 6,
        aspect.ratio = aspect.ratio,
        legend.position = legend.position,
        legend.direction = legend.direction
    )

    attr(p, "height") <- dims$height
    attr(p, "width") <- dims$width

    return(p)
}
