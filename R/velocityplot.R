#' Compute velocity on grid
#'
#' The original python code is on https://github.com/theislab/scvelo/blob/master/scvelo/plotting/velocity_embedding_grid.py
#'
#' @param embedding A matrix of dimension n_obs x n_dim specifying the embedding coordinates of the cells.
#' @param v_embedding A matrix of dimension n_obs x n_dim specifying the velocity vectors of the cells.
#' @param density An optional numeric value specifying the density of the grid points along each dimension. Default is 1.
#' @param smooth An optional numeric value specifying the smoothing factor for the velocity vectors. Default is 0.5.
#' @param n_neighbors An optional numeric value specifying the number of nearest neighbors for each grid point. Default is ceiling(n_obs / 50).
#' @param min_mass An optional numeric value specifying the minimum mass required for a grid point to be considered. Default is 1.
#' @param scale An optional numeric value specifying the scaling factor for the velocity vectors. Default is 1.
#' @param adjust_for_stream A logical value indicating whether to adjust the velocity vectors for streamlines. Default is FALSE.
#' @param cutoff_perc An optional numeric value specifying the percentile cutoff for removing low-density grid points. Default is 5.
#' @keywords internal
.compute_velocity_on_grid <- function(
    embedding, v_embedding,
    density = NULL, smooth = NULL, n_neighbors = NULL, min_mass = NULL,
    scale = 1, adjust_for_stream = FALSE, cutoff_perc = NULL
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

    neighbors <- t(as.matrix(apply(d, 2, function(x) order(x, decreasing = FALSE)[1:n_neighbors])))
    dists <- t(as.matrix(apply(d, 2, function(x) x[order(x, decreasing = FALSE)[1:n_neighbors]])))

    weight <- stats::dnorm(dists, sd = mean(sapply(grs, function(g) g[2] - g[1])) * smooth)
    p_mass <- p_mass_v <- rowSums(weight)
    p_mass_v[p_mass_v < 1] <- 1

    neighbors_emb <- array(as.matrix(v_embedding)[neighbors, seq_len(ncol(v_embedding))],
        dim = c(dim(neighbors), dim(v_embedding)[2])
    )
    v_grid <- apply((neighbors_emb * c(weight)), c(1, 3), sum)
    v_grid <- v_grid / p_mass_v

    if (isTRUE(adjust_for_stream)) {
        x_grid <- matrix(c(unique(x_grid[, 1]), unique(x_grid[, 2])), nrow = 2, byrow = TRUE)
        ns <- floor(sqrt(length(v_grid[, 1])))
        v_grid <- array(t(v_grid), dim = c(2, ns, ns))

        mass <- sqrt(apply(v_grid**2, c(2, 3), sum))
        min_mass <- 10**(min_mass - 6) # default min_mass = 1e-5
        min_mass[min_mass > max(mass, na.rm = TRUE) * 0.9] <- max(mass, na.rm = TRUE) * 0.9
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
#' The plot shows the velocity vectors of the cells in a specified reduction space.
#'
#' @inheritParams common_args
#' @param embedding A matrix or data.frame of dimension n_obs x n_dim specifying the embedding coordinates of the cells.
#' @param v_embedding A matrix or data.frame of dimension n_obs x n_dim specifying the velocity vectors of the cells.
#' @param plot_type A character string specifying the type of plot to create. Options are "raw", "grid", or "stream". Default is "raw".
#' @param split_by An optional character string specifying a variable to split the plot by. Not supported yet.
#' @param group_by An optional character string specifying a variable to group the cells by.
#' @param group_name An optional character string specifying the name of the grouping variable in legend. Default is "Group".
#' @param group_palette A character string specifying the color palette to use for grouping. Default is "Paired".
#' @param group_palcolor An optional character vector specifying the colors to use for grouping. If NULL, the colors will be generated from the group_palette.
#' @param n_neighbors An optional numeric value specifying the number of nearest neighbors for each grid point. Default is ceiling(ncol(embedding) / 50).
#' @param density An optional numeric value specifying the density of the grid points along each dimension. Default is 1.
#' @param smooth An optional numeric value specifying the smoothing factor for the velocity vectors. Default is 0.5.
#' @param scale An optional numeric value specifying the scaling factor for the velocity vectors. Default is 1.
#' @param min_mass An optional numeric value specifying the minimum mass required for a grid point to be considered. Default is 1.
#' @param cutoff_perc An optional numeric value specifying the percentile cutoff for removing low-density grid points. Default is 5.
#' @param arrow_angle An optional numeric value specifying the angle of the arrowheads in degrees for velocity arrows. Default is 20.
#' @param arrow_color A character string specifying the color of the velocity arrowheads. Default is "black".
#' @param arrow_alpha A numeric value specifying the transparency of the velocity arrows. Default is 1 (fully opaque).
#' Only works for `plot_type = "raw"` and `plot_type = "grid"`. For `plot_type = "stream"`, use `streamline_alpha` instead.
#' @param streamline_l An optional numeric value specifying the length of the velocity streamlines. Default is 5.
#' @param streamline_minl An optional numeric value specifying the minimum length of the velocity streamlines. Default is 1.
#' @param streamline_res An optional numeric value specifying the resolution of the velocity streamlines. Default is 1.
#' @param streamline_n An optional numeric value specifying the number of velocity streamlines to draw. Default is 15.
#' @param streamline_width A numeric vector of length 2 specifying the width of the velocity streamlines. Default is c(0, 0.8).
#' @param streamline_alpha A numeric value specifying the transparency of the velocity streamlines. Default is 1 (fully opaque).
#' @param streamline_color A character string specifying the color of the velocity streamlines.
#' @param streamline_palette A character string specifying the color palette to use for the velocity streamlines. Default is "RdYlBu".
#' @param streamline_palcolor An optional character vector specifying the colors to use for the velocity streamlines. If NULL, the colors will be generated from the streamline_palette.
#' @param streamline_bg_color A character string specifying the background color of the velocity streamlines. Default is "white".
#' @param streamline_bg_stroke A numeric value specifying the background stroke width of the velocity streamlines. Default is 0.5.
#' @param return_layer A logical value indicating whether to return the ggplot layer instead of the full plot. Default is FALSE.
#' @return A ggplot object representing the cell velocity plot or a ggplot layer if `return_layer` is TRUE.
#' @importFrom rlang %||% sym
#' @importFrom ggplot2 geom_segment scale_color_gradientn scale_size after_stat
#' @examples
#' \dontrun{
#' data(dim_example)
#' VelocityPlot(dim_example[, 1:2], dim_example[, 3:4])
#' VelocityPlot(dim_example[, 1:2], dim_example[, 3:4], group_by = dim_example$clusters)
#' VelocityPlot(dim_example[, 1:2], dim_example[, 3:4], plot_type = "grid")
#' VelocityPlot(dim_example[, 1:2], dim_example[, 3:4], plot_type = "stream")
#' }
VelocityPlot <- function(
    embedding, v_embedding, plot_type = c("raw", "grid", "stream"), split_by = NULL,
    group_by = NULL, group_name = "Group", group_palette = "Paired", group_palcolor = NULL,
    n_neighbors = NULL, density = 1, smooth = 0.5, scale = 1, min_mass = 1, cutoff_perc = 5,
    arrow_angle = 20, arrow_color = "black", arrow_alpha = 1,
    streamline_l = 5, streamline_minl = 1, streamline_res = 1, streamline_n = 15,
    streamline_width = c(0, 0.8), streamline_alpha = 1, streamline_color = NULL, streamline_palette = "RdYlBu", streamline_palcolor = NULL,
    streamline_bg_color = "white", streamline_bg_stroke = 0.5,
    aspect.ratio = 1, title = "Cell velocity", subtitle = NULL, xlab = NULL, ylab = NULL,
    legend.position = "right", legend.direction = "vertical",
    theme = "theme_this", theme_args = list(),
    return_layer = FALSE, seed = 8525) {
    ggplot <- if (getOption("plotthis.gglogger.enabled", FALSE)) {
        gglogger::ggplot
    } else {
        ggplot2::ggplot
    }
    stopifnot("[VelocityPlot] 'split_by' is not supported yet" = is.null(split_by))

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

    if (plot_type == "raw") {
        if (!is.null(density) && (density > 0 && density < 1)) {
            s <- ceiling(density * nrow(embedding))
            ix_choice <- sample(seq_len(nrow(embedding)), size = s, replace = FALSE)
            embedding <- embedding[ix_choice, ]
            v_embedding <- v_embedding[ix_choice, ]
        }
        if (!is.null(scale)) {
            v_embedding <- v_embedding * scale
        }
        df <- cbind.data.frame(embedding, v_embedding)
        colnames(df) <- c("x", "y", "u", "v")
        df$length <- sqrt(df[["u"]]^2 + df[["v"]]^2)
        global_size <- sqrt(max(df$x, na.rm = TRUE)^2 + max(df$y, na.rm = TRUE)^2)
        df$length_perc <- df$length / global_size

        if (!is.null(group_by)) {
            df[[group_name]] <- as.factor(group_by)
            velocity_layer <- list(
                geom_segment(
                    data = df, mapping = aes(
                        x = !!sym("x"), y = !!sym("y"),
                        xend = !!sym("x") + !!sym("u"),
                        yend = !!sym("y") + !!sym("v"),
                        color = !!sym(group_name)
                    ),
                    alpha = arrow_alpha,
                    arrow = arrow(length = unit(df$length_perc, "npc"), type = "closed", angle = arrow_angle),
                    lineend = "round", linejoin = "mitre", inherit.aes = FALSE
                ),
                scale_color_manual(
                    name = group_name,
                    values = palette_this(df[[group_name]], palette = group_palette, palcolor = group_palcolor),
                    guide = guide_legend(title.hjust = 0, order = 1, override.aes = list(linewidth = 2, alpha = 1))
                )
            )
            attr(velocity_layer, "scales") <- unique(c(attr(velocity_layer, "scales"), "color"))
        } else {
            velocity_layer <- list(
                geom_segment(
                    data = df, aes(
                        x = !!sym("x"), y = !!sym("y"),
                        xend = !!sym("x") + !!sym("u"),
                        yend = !!sym("y") + !!sym("v")),
                    color = arrow_color, alpha = arrow_alpha,
                    arrow = arrow(length = unit(df$length_perc, "npc"), type = "closed", angle = arrow_angle),
                    lineend = "round", linejoin = "mitre", inherit.aes = FALSE
                )
            )
        }
    } else if (plot_type == "grid") {
        if (!is.null(group_by)) {
            warning("[VelocityPlot] Ignoring 'group_by', which is not supported for 'grid' plot type.")
        }
        res <- .compute_velocity_on_grid(embedding, v_embedding,
            density = density, smooth = smooth, n_neighbors = n_neighbors,
            min_mass = min_mass, scale = scale
        )
        x_grid <- res$x_grid
        v_grid <- res$v_grid

        df <- cbind.data.frame(x_grid, v_grid)
        colnames(df) <- c("x", "y", "u", "v")
        df$length <- sqrt(df[["u"]]^2 + df[["v"]]^2)
        global_size <- sqrt(max(df$x, na.rm = TRUE)^2 + max(df$y, na.rm = TRUE)^2)
        df$length_perc <- df$length / global_size
        velocity_layer <- list(
            geom_segment(
                data = df, aes(
                    x = !!sym("x"), y = !!sym("y"),
                    xend = !!sym("x") + !!sym("u"),
                    yend = !!sym("y") + !!sym("v")),
                color = arrow_color, alpha = arrow_alpha,
                arrow = arrow(length = unit(df$length_perc, "npc"), type = "closed", angle = arrow_angle),
                lineend = "round", linejoin = "mitre", inherit.aes = FALSE
            )
        )
    } else if (plot_type == "stream") {
        if (!is.null(group_by)) {
            warning("[VelocityPlot] Ignoring 'group_by', which is not supported for 'stream' plot type.")
        }
        res <- .compute_velocity_on_grid(embedding, v_embedding,
            density = density, smooth = smooth, n_neighbors = n_neighbors,
            min_mass = min_mass, scale = 1, cutoff_perc = cutoff_perc,
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
                metR::geom_streamline(
                    data = df, aes(x = !!sym("x"), y = !!sym("y"), dx = !!sym("u"), dy = !!sym("v")),
                    L = streamline_l, min.L = streamline_minl, res = streamline_res,
                    n = streamline_n, size = max(streamline_width, na.rm = TRUE) + streamline_bg_stroke, color = streamline_bg_color, alpha = streamline_alpha,
                    arrow.type = "closed", arrow.angle = arrow_angle,
                    lineend = "round", linejoin = "mitre", inherit.aes = FALSE
                ),
                metR::geom_streamline(
                    data = df, aes(x = !!sym("x"), y = !!sym("y"), dx = !!sym("u"), dy = !!sym("v")),
                    L = streamline_l, min.L = streamline_minl, res = streamline_res,
                    n = streamline_n, size = max(streamline_width, na.rm = TRUE), color = streamline_color, alpha = streamline_alpha,
                    arrow.type = "closed", arrow.angle = arrow_angle,
                    lineend = "round", linejoin = "mitre", inherit.aes = FALSE
                ),
                metR::geom_streamline(
                    data = df, aes(x = !!sym("x"), y = !!sym("y"), dx = !!sym("u"), dy = !!sym("v")),
                    L = streamline_l, min.L = streamline_minl, res = streamline_res,
                    n = streamline_n, linetype = 0, color = arrow_color,
                    arrow.type = "closed", arrow.angle = arrow_angle,
                    lineend = "round", linejoin = "mitre", inherit.aes = FALSE
                )
            )
        } else {
            velocity_layer <- list(
                metR::geom_streamline(
                    data = df, aes(x = !!sym("x"), y = !!sym("y"), dx = !!sym("u"), dy = !!sym("v")),
                    L = streamline_l, min.L = streamline_minl, res = streamline_res,
                    n = streamline_n, size = max(streamline_width, na.rm = TRUE) + streamline_bg_stroke, color = streamline_bg_color, alpha = streamline_alpha,
                    arrow.type = "closed", arrow.angle = arrow_angle,
                    lineend = "round", linejoin = "mitre", inherit.aes = FALSE
                ),
                metR::geom_streamline(
                    data = df, aes(x = !!sym("x"), y = !!sym("y"), dx = !!sym("u"), dy = !!sym("v"),
                        size = after_stat(!!sym("step")), color = sqrt(after_stat(!!sym("dx"))^2 + after_stat(!!sym("dy"))^2)),
                    L = streamline_l, min.L = streamline_minl, res = streamline_res,
                    n = streamline_n, alpha = streamline_alpha,
                    arrow = NULL, lineend = "round", linejoin = "mitre", inherit.aes = FALSE
                ),
                metR::geom_streamline(
                    data = df, aes(x = !!sym("x"), y = !!sym("y"), dx = !!sym("u"), dy = !!sym("v")),
                    L = streamline_l, min.L = streamline_minl, res = streamline_res,
                    n = streamline_n, linetype = 0, color = arrow_color,
                    arrow.type = "closed", arrow.angle = arrow_angle,
                    lineend = "round", linejoin = "mitre", inherit.aes = FALSE
                ),
                scale_color_gradientn(
                    name = "Velocity", n.breaks = 4,
                    colors = palette_this(palette = streamline_palette, palcolor = streamline_palcolor),
                    guide = guide_colorbar(frame.colour = "black", ticks.colour = "black", title.hjust = 0, order = 1)
                ),
                scale_size(range = range(streamline_width), guide = "none")
            )
            attr(velocity_layer, "scales") <- unique(c(attr(velocity_layer, "scales"), "color"))
        }
    }

    if (isTRUE(return_layer)) {
        return(velocity_layer)
    }

    p <- ggplot() +
        velocity_layer +
        labs(title = title, subtitle = subtitle, x = xlab, y = ylab) +
        do.call(theme, theme_args) +
        ggplot2::theme(
            aspect.ratio = aspect.ratio,
            legend.position = legend.position,
            legend.direction = legend.direction
        )

    attr(p, "width") <- 6
    attr(p, "height") <- 6

    return(p)
}
