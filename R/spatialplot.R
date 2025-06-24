#' @title Flip values on the y-axis direction, and negate the Y-Coordinates of SpatRaster, SpatVector Object and data.frame
#'
#' @description
#' These internal functions flip the y-coordinates of `SpatRaster` and `SpatVector` objects from the `terra` package.
#' For rasters, the function vertically flips the raster and adjusts its extent accordingly.
#' For vectors, the function negates the y-coordinates of all geometries.
#' For data frames, it negates the values in the specified y column.
#'
#' @param data A `SpatRaster` or `SpatVector` object from the `terra` package, or a data.frame with x and y columns.
#'
#' @return
#' For `SpatRaster` input, a `SpatRaster` object with flipped y-coordinates and adjusted extent.
#' For `SpatVector` input, a `SpatVector` object with y-coordinates negated.
#' For `data.frame` input, a data frame with the specified y column negated.
#'
#' @details
#' These functions are intended for internal use to facilitate coordinate transformations.
#' When visualizing spatial data, it is often necessary to flip the y-axis to put the origin at the top left corner.
#' However, a lot of elements have to be visualized with [ggplot2::geom_sf()], which won't work with
#' [ggplot2::scale_y_reverse()]. See also [this GitHub issue comment](https://github.com/tidyverse/ggplot2/issues/4021#issuecomment-650787582).
#' So we need these functions to flip the values along the y-axis and negate the y-coordinates.
#' This way, we can remove the negative sign from the y-axis labels to mimick the behavior of `scale_y_reverse()`.#'
#'
#' @keywords internal
#' @rdname dot_flip_y
.flip_y <- function(data, ...) {
    UseMethod(".flip_y", data)
}

#' @keywords internal
#' @rdname dot_flip_y
.flip_y.SpatRaster <- function(data, ...) {
    # Flip the raster vertically
    out <- terra::flip(data, direction = "vertical")

    # Adjust the y-coordinates to go from -nrow to 0
    nrows <- terra::nrow(data)
    ext_orig <- terra::ext(data)

    # Create new extent with flipped y coordinates
    new_ext <- terra::ext(
        ext_orig[1], ext_orig[2],  # keep x coordinates
        -1 * ext_orig[4], -1 * ext_orig[3]  # flip y coordinates
    )

    # Set the new extent
    terra::ext(out) <- new_ext

    return(out)
}

#' @keywords internal
#' @rdname dot_flip_y
.flip_y.SpatVector <- function(data, ...) {
    # Manual coordinate transformation approach (more reliable)
    # Get all geometries and transform coordinates
    coords_list <- terra::geom(data)
    coords_list[, "y"] <- -coords_list[, "y"]

    # Recreate the SpatVector with flipped coordinates
    data_flipped <- terra::vect(coords_list, type = terra::geomtype(data), atts = terra::values(data))

    return(data_flipped)
}

#' @keywords internal
#' @rdname dot_flip_y
.flip_y.data.frame <- function(data, y = "y", ...) {
    data[[y]] <- -data[[y]]
    return(data)
}

#' Prepare the extent for spatial plots
#'
#' @param ext A numeric vector of length 4 specifying the extent as `c(xmin, xmax, ymin, ymax)`,
#' or a `SpatExtent` object from the `terra` package.
#' @return A `SpatExtent` object if `ext` is a numeric vector, or the original `SpatExtent` if it is already one.
#' NULL is returned if `ext` is NULL.
#' @keywords internal
.prepare_extent <- function(ext) {
    if (is.null(ext)) {
        return(NULL)
    }
    if (is.numeric(ext) && length(ext) == 4) {
        # Convert numeric vector to SpatExtent
        ext <- terra::ext(ext[1], ext[2], ext[3], ext[4])
    }
    if (!inherits(ext, "SpatExtent")) {
        stop("'ext' must be a numeric vector of length 4 or a SpatExtent object.")
    }
    return(ext)
}

#' Wrap spatial plot if plotted independently
#'
#' This function is used to wrap spatial plots if they are plotted independently
#' with return_layer = FALSE.
#'
#' @param layers A list of ggplot layers to be wrapped.
#' @param ext A numeric vector of length 4 specifying the extent as `c(xmin, xmax, ymin, ymax)`. Default is NULL.
#' @param flip_y Whether to flip the y-axis direction. Default is TRUE.
#' @param legend.position The position of the legend. Default is "right".
#' @param legend.direction The direction of the legend. Default is "vertical".
#' @param title The title of the plot. Default is NULL.
#' @param subtitle The subtitle of the plot. Default is NULL.
#' @param xlab The x-axis label. Default is NULL.
#' @param ylab The y-axis label. Default is NULL.
#' @param theme The theme to be used for the plot. Default is "theme_box".
#' @param theme_args A list of arguments to be passed to the theme function. Default is an empty list.
#' @return A ggplot object with the specified layers.
#' @importFrom ggplot2 coord_sf labs scale_y_continuous
#' @keywords internal
.wrap_spatial_layers <- function(
    layers, ext = NULL, flip_y = TRUE,
    legend.position = "right", legend.direction = "vertical",
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL,
    theme = "theme_box", theme_args = list()
) {
    ggplot <- if (getOption("plotthis.gglogger.enabled", FALSE)) {
        gglogger::ggplot
    } else {
        ggplot2::ggplot
    }

    xlim <- ylim <- NULL
    # Set default width and height based on extent proportions
    if (!is.null(ext)) {
        ext_width <- ext[2] - ext[1]
        ext_height <- ext[4] - ext[3]
        aspect_ratio <- ext_width / ext_height

        # Base height of 6, adjust width proportionally
        base_height <- 6
        base_width <- base_height * aspect_ratio

        # Ensure reasonable bounds
        if (base_width > 12) {
            base_width <- 12
            base_height <- base_width / aspect_ratio
        } else if (base_width < 3) {
            base_width <- 3
            base_height <- base_width / aspect_ratio
        }

        xlim <- c(ext[1], ext[2])
        if (flip_y) {
            ylim <- c(-ext[4], -ext[3])  # Flip y-axis
        } else {
            ylim <- c(ext[3], ext[4])
        }

    } else {
        # Default dimensions when no extent specified
        base_height <- 6
        base_width <- 6
    }

    # Adjust for legend position (similar to DimPlot)
    if (!identical(legend.position, "none")) {
        if (legend.position %in% c("right", "left")) {
            base_width <- base_width + 1
        } else if (legend.direction == "horizontal") {
            base_height <- base_height + 1
        } else {
            base_width <- base_width + 2
        }
    }
    p <- ggplot() +
        layers +
        coord_sf(expand = 0, xlim = xlim, ylim = ylim) +
        labs(
            title = title,
            subtitle = subtitle,
            x = xlab,
            y = ylab
        ) +
        do.call(process_theme(theme), theme_args) +
        ggplot2::theme(
            legend.position = legend.position,
            legend.direction = legend.direction
        )

    if (flip_y) {
        p <- p + scale_y_continuous(labels = function(x) sub("-", "\u2212", as.character(-x)))
    }

    attr(p, "height") <- base_height
    attr(p, "width") <- base_width

    return(p)
}

#' Plots for spatial elements
#'
#' * `SpatImagePlot`: Plot a SpatRaster object as an image.
#' * `SpatMasksPlot`: Plot a SpatRaster object as masks.
#' * `SpatShapesPlot`: Plot a SpatVector object as shapes.
#' * `SpatPointsPlot`: Plot a data.frame of points with spatial coordinates.
#'
#' @rdname spatialplots
#' @concept spatial
#' @inheritParams common_args
#' @param data A `SpatRaster` or `SpatVector` object from the `terra` package,
#' or a data.frame for `SpatShapesPlot` or `SpatPointsPlot`.
#' @param x A character string specifying the x-axis column name for `SpatPointsPlot` or `SpatShapesPlot` when `data` is a data.frame.
#' If `data` is a `SpatRaster` or `SpatVector`, this argument is ignored.
#' @param y A character string specifying the y-axis column name for `SpatPointsPlot` or `SpatShapesPlot` when `data` is a data.frame.
#' If `data` is a `SpatRaster` or `SpatVector`, this argument is ignored.
#' @param group A character string specifying the grouping column for `SpatShapesPlot` when `data` is a data.frame.
#' @param ext A `terra`'s `SpatExtent` object or a numeric vector of length 4 specifying the extent as `c(xmin, xmax, ymin, ymax)`. Default is NULL.
#' @param flip_y Whether to flip the y-axis direction. Default is TRUE.
#' This is useful for visualizing spatial data with the origin at the top left corner.
#' @param palette A character string specifying the color palette to use.
#' For `SpatImagePlot`, if the data has 3 channels (RGB), it will be used as a color identity and
#' this argument will be ignored.
#' @param palette_reverse Whether to reverse the color palette. Default is FALSE.
#' @param fill_by A character string or vector specifying the column(s) to fill the shapes in `SpatShapesPlot`.
#' @param fill_name A character string for the fill legend title.
#' @param color_by A character string specifying the column to color the points in `SpatPointsPlot`.
#' @param color_name A character string for the color legend title in `SpatPointsPlot`.
#' @param size_by A character string specifying the column to size the points in `SpatPointsPlot`.
#' @param size Alias of `size_by` when size is a numeric value.
#' @param size_name A character string for the size legend title in `SpatPointsPlot`.
#' @param lower_quantile,upper_quantile,lower_cutoff,upper_cutoff Vector of minimum and maximum cutoff values or quantile values for each numeric value.
#' @param shape A numeric value or character string specifying the shape of the points in `SpatPointsPlot`.
#' @param add_border Whether to add a border around the masks in `SpatMasksPlot`. Default is TRUE.
#' @param border_color A character string of the border color. Default is "black".
#' @param border_size A numeric value of the border width. Default is 0.5.
#' @param border_alpha A numeric value of the border transparency. Default is 1.
#' @param raster Whether to raster the plot. Default is NULL.
#' @param raster_dpi A numeric vector of the raster dpi. Default is c(512, 512).
#' @param hex Whether to use hex plot. Default is FALSE.
#' @param hex_linewidth A numeric value of the hex line width. Default is 0.5.
#' @param hex_count Whether to count the hex.
#' @param hex_bins A numeric value of the hex bins. Default is 50.
#' @param hex_binwidth A numeric value of the hex bin width. Default is NULL.
#' @param label Whether to show the labels of groups. Default is FALSE.
#' @param label_size A numeric value of the label size. Default is 4.
#' @param label_fg A character string of the label foreground color. Default is "white".
#' @param label_bg A character string of the label background color. Default is "black".
#' @param label_bg_r A numeric value of the background ratio of the labels. Default is 0.1.
#' @param label_repel Whether to repel the labels. Default is FALSE.
#' @param label_repulsion A numeric value of the label repulsion. Default is 20.
#' @param label_pt_size A numeric value of the label point size. Default is 1.
#' @param label_pt_color A character string of the label point color. Default is "black".
#' @param label_segment_color A character string of the label segment color. Default is "black".
#' @param label_insitu Whether to place the raw labels (group names) in the center of the points with the corresponding group.
#' Default is FALSE, which uses numbers instead of raw labels.
#' @param label_pos A character string or a function specifying the position of the labels.
#' * "mean": Place labels at the mean position of the points in each group.
#'   Same as `function(x) mean(x, na.rm = TRUE)`.
#' * "center": Place labels at the center of the points in each group.
#'   Same as `function(x) mean(range(x, na.rm = TRUE))`.
#' * "median": Place labels at the median position of the points in each group.
#'   Same as `function(x) median(x, na.rm = TRUE)`.
#' * "first": Place labels at the first point in each group.
#'   Same as `function(x) x[1]`.
#' * "last": Place labels at the last point in each group.
#'   Same as `function(x) x[length(x)]`.
#' * "random": Place labels at a random point in each group.
#'   Same as `function(x) sample(x, 1)`.
#' * "min": Place labels at the minimum position (both x and y) of the points in each group.
#'   Same as `function(x) min(x, na.rm = TRUE)`.
#' * "max": Place labels at the maximum position (both x and y) of the points in each group.
#'   Same as `function(x) max(x, na.rm = TRUE)`.
#' @param highlight A character vector of the row names to highlight. Default is NULL.
#' @param highlight_alpha A numeric value of the highlight transparency. Default is 1.
#' @param highlight_size A numeric value of the highlight size. Default is 1.
#' @param highlight_color A character string of the highlight color. Default is "black".
#' @param highlight_stroke A numeric value of the highlight stroke. Default is 0.8.
#' @param graph A character string of column names or the indexes in the data for the graph data. Default is NULL.
#' If "@graph" is provided, the graph data will be extracted from the data attribute 'graph'.
#' The graph data should be an adjacency matrix (numeric matrix) with row and column names matching the point IDs.
#' Or a data.frame with x, xend, y, yend and value columns. If so,
#' `graph_x`, `graph_y`, `graph_xend`, `graph_yend`, and `graph_value` arguments can be used to specify the column names.
#' @param graph_x A character string of the x column name for the graph data.
#' @param graph_y A character string of the y column name for the graph data.
#' @param graph_xend A character string of the xend column name for the graph data.
#' @param graph_yend A character string of the yend column name for the graph data.
#' @param graph_value A character string of the value column name for the graph data.
#' @param edge_size A numeric vector of the edge size range. Default is c(0.05, 0.5).
#' @param edge_alpha A numeric value of the edge transparency. Default is 0.1.
#' @param edge_color A character string of the edge color. Default is "grey40".
#' @param return_layer Whether to return the layers or the plot. Default is FALSE.
#' @importFrom rlang %||% sym parse_exprs
#' @importFrom ggplot2 scale_fill_gradientn aes guide_colorbar element_blank
#' @importFrom ggplot2 geom_sf ylim labs coord_sf geom_hex stat_summary_hex
#' @importFrom ggplot2 geom_segment scale_linewidth_continuous
#' @export
#' @examples
#' \donttest{
#' set.seed(8525)
#' # --- SpatImagePlot ---
#' # Generate a sample SpatRaster
#' r <- terra::rast(
#'     nrows = 50, ncols = 40, vals = runif(2000),
#'     xmin = 0, xmax = 40, ymin = 0, ymax = 50,
#'     crs = ""
#' )
#' SpatImagePlot(r)
#' SpatImagePlot(r, raster = TRUE, raster_dpi = 20)
#' SpatImagePlot(r, alpha = 0.5, theme = "theme_blank",
#'     theme_args = list(add_coord = FALSE), fill_name = "value")
#' SpatImagePlot(r, ext = c(0, 10, 0, 10), flip_y = FALSE, palette = "viridis")
#'
#' # --- SpatMasksPlot ---
#' m <- terra::rast(
#'    nrows = 50, ncols = 40,
#'    vals = sample(c(1:5, NA), 2000, replace = TRUE, prob = c(rep(0.04, 5), 0.8)),
#'    xmin = 0, xmax = 40, ymin = 0, ymax = 50,
#'    crs = ""
#' )
#' SpatMasksPlot(m, border_color = "red")
#' SpatMasksPlot(m, ext = c(0, 15, 0, 20), add_border = FALSE,
#'     palette_reverse = TRUE, fill_name = "value")
#'
#' # --- SpatShapesPlot ---
#' polygons <- data.frame(
#'    id = paste0("poly_", 1:10),
#'    cat = sample(LETTERS[1:3], 10, replace = TRUE),
#'    feat1 = rnorm(10),
#'    feat2 = rnorm(10),
#'    geometry = c(
#'        'POLYGON((64.6 75.3,66.0 70.5,66.4 70.2,67.0 69.8,72.8 70.4,64.6 75.3))',
#'        'POLYGON((56.7 63.0,52.3 65.6,48.0 63.2,51.2 55.7,57.1 59.2,56.7 63.0))',
#'        'POLYGON((9.9 16.5,9.3 15.9,8.0 13.1,11.5 7.8,17.8 11.3,9.9 16.5))',
#'        'POLYGON((64.9 37.2,60.3 37.4,57.6 31.7,58.9 29.3,64.0 28.1,64.9 37.2))',
#'        'POLYGON((30.5 49.1,22.4 46.5,22.4 43.9,30.9 41.9,31.6 42.9,30.5 49.1))',
#'        'POLYGON((78.3 57.8,70.5 61.6,71.6 52.7,72.2 52.5,77.4 54.5,78.3 57.8))',
#'        'POLYGON((41.8 23.8,41.3 25.9,41.0 26.4,36.5 28.7,35.8 28.6,41.8 23.8))',
#'        'POLYGON((15.7 75.9,14.2 74.4,15.7 67.5,23.0 69.8,23.4 71.7,15.7 75.9))',
#'        'POLYGON((80.7 37.4,75.3 31.3,77.1 28.5,82.5 28.0,83.1 28.5,80.7 37.4))',
#'        'POLYGON((15.5 37.8,14.4 38.6,7.3 32.6,8.3 30.9,15.1 30.2,15.5 37.8))'
#'    )
#' )
#'
#' polygons <- terra::vect(polygons, crs = "EPSG:4326", geom = "geometry")
#'
#' SpatShapesPlot(polygons)
#' SpatShapesPlot(polygons, ext = c(0, 20, 0, 20))
#' SpatShapesPlot(polygons, highlight = 'cat == "A"', highlight_color = "red2")
#' SpatShapesPlot(polygons, border_color = "red", border_size = 2)
#' SpatShapesPlot(polygons, fill_by = "cat", fill_name = "category")
#' # Let border color be determined by fill
#' SpatShapesPlot(polygons, fill_by = "cat", alpha = 0.6, border_color = TRUE)
#' SpatShapesPlot(polygons, fill_by = "feat1")
#' SpatShapesPlot(polygons, fill_by = c("feat1", "feat2"), palette = "RdYlBu")
#'
#' # --- SpatPointsPlot ---
#' # create some random points in the above polygons
#' points <- data.frame(
#'   id = paste0("point_", 1:30),
#'   gene = sample(LETTERS[1:3], 30, replace = TRUE),
#'   feat1 = runif(30, 0, 100),
#'   feat2 = runif(30, 0, 100),
#'   size = runif(30, 1, 5),
#'   x = c(
#'     61.6, 14.3, 12.7, 49.6, 74.9, 58.9, 13.9, 24.7, 16.9, 15.6,
#'     72.4, 60.1, 75.4, 14.9, 80.3, 78.8, 16.7, 27.6, 48.9, 52.5,
#'     12.9, 11.8, 50.4, 25.6, 10.4, 51.9, 73.4, 26.8, 50.4, 60.0
#'   ),
#'   y = c(
#'     32.1, 12.8, 33.2, 59.9, 57.8, 31.9, 10.1, 46.8, 75.3, 69.0,
#'     60.0, 29.4, 54.2, 34.2, 35.3, 33.1, 74.7, 48.0, 63.2, 59.2,
#'     9.2, 15.1, 64.5, 47.1, 11.4, 60.1, 54.1, 44.5, 61.9, 30.3
#'   )
#' )
#'
#' SpatPointsPlot(points)
#' SpatPointsPlot(points, color_by = "gene", size_by = "size", shape = 22,
#'   border_size = 1)
#' SpatPointsPlot(points, raster = TRUE, raster_dpi = 30, color_by = "feat1")
#' SpatPointsPlot(points, color_by = c("feat1", "feat2"), size_by = "size")
#' SpatPointsPlot(points, color_by = "feat1", upper_cutoff = 50)
#' SpatPointsPlot(points, color_by = "feat1", hex = TRUE)
#' SpatPointsPlot(points, color_by = "gene", label = TRUE)
#' SpatPointsPlot(points, color_by = "gene", highlight = 1:20,
#'   highlight_color = "red2", highlight_stroke = 0.8)
#'
#' # --- Graph/Network functionality ---
#' # Create a simple adjacency matrix for demonstration
#' set.seed(8525)
#' graph_mat <- matrix(0, nrow = 30, ncol = 30)
#' # Add some random connections with weights
#' for(i in 1:30) {
#'   neighbors <- sample(setdiff(1:30, i), size = sample(2:5, 1))
#'   graph_mat[i, neighbors] <- runif(length(neighbors), 0.1, 1)
#' }
#' rownames(graph_mat) <- colnames(graph_mat) <- rownames(points)
#' attr(points, "graph") <- graph_mat
#'
#' SpatPointsPlot(points, color_by = "gene", graph = "@graph",
#'   edge_color = "grey60", edge_alpha = 0.3)
#' SpatPointsPlot(points, color_by = "feat1", graph = graph_mat,
#'   edge_size = c(0.1, 1), edge_alpha = 0.5)
#'
#' # --- Use the `return_layer` argument to get the ggplot layers
#' ext = c(0, 40, 0, 50)
#' ggplot2::ggplot() +
#'   SpatImagePlot(r, return_layer = TRUE, alpha = 0.2, ext = ext) +
#'   SpatShapesPlot(polygons, return_layer = TRUE, ext = ext, fill_by = "white") +
#'   SpatPointsPlot(points, return_layer = TRUE, ext = ext, color_by = "feat1") +
#'   theme_box() +
#'   ggplot2::coord_sf(expand = 0) +
#'   ggplot2::scale_y_continuous(labels = function(x) -x)
#'
#' }
SpatImagePlot <- function(
    data,
    ext = NULL, raster = NULL, raster_dpi = NULL, flip_y = TRUE,
    palette = "turbo", palcolor = NULL, palette_reverse = FALSE,
    alpha = 1, fill_name = NULL, return_layer = FALSE,
    theme = "theme_box", theme_args = list(),
    legend.position = ifelse(return_layer, "none", "right"), legend.direction = "vertical",
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL, seed = 8525
) {
    set.seed(seed)
    stopifnot("'data' must be a SpatRaster object" = inherits(data, "SpatRaster"))

    ext <- .prepare_extent(ext)
    if (!is.null(ext)) {
        data <- terra::crop(data, ext)
        if (terra::ncell(data) == 0) {
            stop("[SpatImagePlot] No data in the specified extent. Please check your extent.")
        }
    }

    raster <- raster %||% (terra::ncell(data) > 1e6)
    if (raster) {
        # Calculate appropriate aggregation factor based on desired output resolution
        if (is.null(raster_dpi)) {
            raster_dpi <- c(512, 512)
        }
        if (length(raster_dpi) == 1) {
            raster_dpi <- rep(raster_dpi, 2)
        }

        # Calculate aggregation factors to achieve target resolution
        current_dims <- c(terra::ncol(data), terra::nrow(data))
        agg_factors <- pmax(1, round(current_dims / raster_dpi))

        # Aggregate raster to reduce resolution
        data <- terra::aggregate(data, fact = agg_factors, fun = "mean")
    }

    if (flip_y) { data <- .flip_y(data) }

    if (dim(data)[3] == 3) {
        names(data) <- c("red", "green", "blue")
        data <- terra::as.data.frame(data, xy = TRUE)
        data$red <- scales::rescale(data$red, to = c(0, 255))
        data$green <- scales::rescale(data$green, to = c(0, 255))
        data$blue <- scales::rescale(data$blue, to = c(0, 255))
        data$value <- rgb(data$red, data$green, data$blue, maxColorValue = 255)
        colnames(data)[1:2] <- c("x", "y")
        fill_identity <- TRUE
    } else {
        data <- terra::as.data.frame(data, xy = TRUE)
        colnames(data) <- c("x", "y", "value")
        fill_identity <- FALSE
    }

    layers <- list(
        ggplot2::geom_raster(
            data = data,
            aes(x = !!sym("x"), y = !!sym("y"), fill = !!sym("value")),
            alpha = alpha
        )
    )
    if (fill_identity) {
        layers <- c(layers, list(ggplot2::scale_fill_identity(guide = "none")))
    } else {
        layers <- c(layers, list(
            scale_fill_gradientn(
                name = fill_name %||% names(data)[1],
                n.breaks = 4,
                colors = palette_this(palette = palette, n = 256, reverse = palette_reverse, palcolor = palcolor),
                guide = if (identical(legend.position, "none")) "none" else guide_colorbar(
                    frame.colour = "black", ticks.colour = "black", title.hjust = 0,
                    alpha = alpha
                ),
                na.value = "transparent"
            )
        ))
    }
    # If we add another layers, we need the ggnewscale::new_scale_fill() to avoid conflicts
    attr(layers, "scales") <- "fill"

    if (return_layer) {
        return(layers)
    }

    .wrap_spatial_layers(layers,
        ext = ext, flip_y = flip_y,
        legend.position = legend.position, legend.direction = legend.direction,
        title = title, subtitle = subtitle, xlab = xlab, ylab = ylab,
        theme = theme, theme_args = theme_args
    )
}

#' @rdname spatialplots
#' @concept spatial
#' @export
SpatMasksPlot <- function(
    data,
    ext = NULL, flip_y = TRUE, add_border = TRUE, border_color = "black",
    border_size = 0.5, border_alpha = 1,
    palette = "turbo", palcolor = NULL, palette_reverse = FALSE,
    alpha = 1, fill_name = NULL, return_layer = FALSE,
    theme = "theme_box", theme_args = list(),
    legend.position = "right", legend.direction = "vertical",
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL, seed = 8525
) {
    set.seed(seed)
    ggplot <- if (getOption("plotthis.gglogger.enabled", FALSE)) {
        gglogger::ggplot
    } else {
        ggplot2::ggplot
    }

    stopifnot("'data' must be a SpatRaster object" = inherits(data, "SpatRaster"))

    ext <- .prepare_extent(ext)
    if (!is.null(ext)) {
        data <- terra::crop(data, ext)
        if (terra::ncell(data) == 0) {
            stop("[SpatMasksPlot] No data in the specified extent. Please check your extent.")
        }
    }

    if (flip_y) { data <- .flip_y(data) }

    # Set background (0 values) to NA for transparency
    data[data == 0] <- NA

    layers <- list(
        # Convert to data frame for proper transparency handling
        ggplot2::geom_raster(
            data = stats::setNames(terra::as.data.frame(data, xy = TRUE), c("x", "y", "value")),
            aes(x = !!sym("x"), y = !!sym("y"), fill = !!sym("value")),
            alpha = alpha
        )
    )

    if (add_border) {
        # Convert mask values to polygons for cleaner visualization
        polys <- terra::as.polygons(data, dissolve = TRUE)
        layers <- c(
            layers,
            list(ggplot2::geom_sf(
                data = sf::st_as_sf(polys),
                fill = NA,
                color = border_color,
                linewidth = border_size,
                alpha = border_alpha
            ))
        )
    }

    layers <- c(
        layers,
        list(scale_fill_gradientn(
            name = fill_name %||% names(data)[1],
            n.breaks = 4,
            colors = palette_this(palette = palette, n = 256, reverse = palette_reverse, palcolor = palcolor),
            guide = if (identical(legend.position, "none")) "none" else guide_colorbar(
                frame.colour = "black", ticks.colour = "black", title.hjust = 0
            ),
            na.value = "transparent"
        ))
    )
    # If we add another layers, we need the ggnewscale::new_scale_fill() to avoid conflicts
    attr(layers, "scales") <- "fill"

    if (return_layer) {
        return(layers)
    }

    .wrap_spatial_layers(layers,
        ext = ext, flip_y = flip_y,
        legend.position = legend.position, legend.direction = legend.direction,
        title = title, subtitle = subtitle, xlab = xlab, ylab = ylab,
        theme = theme, theme_args = theme_args
    )
}

#' @rdname spatialplots
#' @concept spatial
#' @export
SpatShapesPlot <- function(data, x = NULL, y = NULL, group = NULL,
    ext = NULL, flip_y = TRUE,
    fill_by = NULL, border_color = "black", border_size = 0.5, border_alpha = 1,
    palette = NULL, palcolor = NULL, palette_reverse = FALSE,
    alpha = 1, fill_name = NULL,
    highlight = NULL, highlight_alpha = 1, highlight_size = 1, highlight_color = "black", highlight_stroke = 0.8,
    facet_scales = "fixed", facet_nrow = NULL, facet_ncol = NULL, facet_byrow = TRUE,
    return_layer = FALSE,
    theme = "theme_box", theme_args = list(),
    legend.position = ifelse(return_layer, "none", "right"), legend.direction = "vertical",
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL, seed = 8525) {
    UseMethod("SpatShapesPlot", data)
}

#' @rdname spatialplots
#' @concept spatial
#' @export
SpatShapesPlot.SpatVector <- function(
    data, x = NULL, y = NULL, group = NULL,
    ext = NULL, flip_y = TRUE,
    fill_by = NULL, border_color = "black", border_size = 0.5, border_alpha = 1,
    palette = NULL, palcolor = NULL, palette_reverse = FALSE,
    alpha = 1, fill_name = NULL,
    highlight = NULL, highlight_alpha = 1, highlight_size = 1, highlight_color = "black", highlight_stroke = 0.8,
    facet_scales = "fixed", facet_nrow = NULL, facet_ncol = NULL, facet_byrow = TRUE,
    return_layer = FALSE,
    theme = "theme_box", theme_args = list(),
    legend.position = ifelse(return_layer, "none", "right"), legend.direction = "vertical",
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL, seed = 8525
) {
    set.seed(seed)
    ggplot <- if (getOption("plotthis.gglogger.enabled", FALSE)) {
        gglogger::ggplot
    } else {
        ggplot2::ggplot
    }

    ext <- .prepare_extent(ext)
    if (!is.null(ext)) {
        data <- terra::crop(data, ext)
        if (terra::nrow(data) == 0 || terra::ncol(data) == 0) {
            stop("[SpatShapesPlot] No data in the specified extent. Please check your extent.")
        }
    }

    if (flip_y) { data <- .flip_y(data) }

    # Convert to sf object for ggplot
    data_sf <- sf::st_as_sf(data)

    # Handle multiple fill_by columns for faceting
    if (!is.null(fill_by) && length(fill_by) > 1) {
        # Check that all fill_by columns exist and are numeric
        missing_cols <- fill_by[!fill_by %in% names(data_sf)]
        if (length(missing_cols) > 0) {
            stop("Columns not found in data: ", paste(missing_cols, collapse = ", "))
        }

        non_numeric_cols <- fill_by[!sapply(fill_by, function(col) is.numeric(data_sf[[col]]))]
        if (length(non_numeric_cols) > 0) {
            stop("Multiple fill_by columns must be numeric. Non-numeric columns: ", paste(non_numeric_cols, collapse = ", "))
        }

        # Reshape data for faceting: convert to long format
        data_long <- data_sf
        # Create a list to store the reshaped data
        facet_data_list <- list()

        for (col in fill_by) {
            temp_data <- data_sf
            temp_data$.fill <- temp_data[[col]]
            temp_data$.facet_var <- col
            facet_data_list[[col]] <- temp_data
        }

        # Combine all facet data
        data_sf <- do.call(rbind, facet_data_list)

        # Convert .facet_var to factor with levels in original fill_by order
        data_sf$.facet_var <- factor(data_sf$.facet_var, levels = fill_by)

        # Set fill_by to the new column name
        fill_by <- ".fill"
        facet_by <- ".facet_var"

    } else {
        # Single column case
        if (!is.null(fill_by) && is.character(fill_by) && length(fill_by) == 1) {
            if (fill_by %in% names(data_sf)) {
                # It's a column name
                fill_by <- check_columns(
                    data_sf,
                    fill_by,
                    force_factor = !is.numeric(data_sf[[fill_by]])
                )
            } else {
                # It's a fixed color - don't treat as column
                fill_by <- fill_by
            }
        }
        facet_by <- NULL
    }

    # Set default palette based on data type
    if (is.null(palette) && !is.null(fill_by) && fill_by %in% names(data_sf)) {
        if (is.numeric(data_sf[[fill_by]])) {
            palette <- "turbo"
        } else {
            palette <- "Paired"
        }
    }
    palette <- palette %||% "turbo"  # fallback default

    # Determine geometry type for appropriate geom
    # Assuming polygons
    # geom_type <- unique(sf::st_geometry_type(data_sf))

    # Build aesthetic mappings
    aes_mapping <- aes()
    fill_by_is_column <- !is.null(fill_by) && is.character(fill_by) && length(fill_by) == 1 && fill_by %in% names(data_sf)

    if (fill_by_is_column) {
        aes_mapping$fill <- sym(fill_by)
    }

    # Handle border_color mapping
    border_aes <- "none"
    if (isTRUE(border_color) && fill_by_is_column) {
        # Use same variable for both fill and color
        aes_mapping$colour <- sym(fill_by)
        border_aes <- "same_as_fill"
    } else if (is.character(border_color) && length(border_color) == 1) {
        # Single color string - will be applied as fixed color
        border_aes <- "fixed_color"
    } else if (isFALSE(border_color)) {
        # No border
        border_aes <- "none"
    }

    # POLYGON, MULTIPOLYGON, or other
    # Build geom parameters conditionally
    geom_params <- list(
        data = data_sf,
        mapping = aes_mapping,
        alpha = alpha,
        linewidth = border_size
    )

    # Only add color if not mapped and needed
    if (!("colour" %in% names(aes_mapping))) {
        if (border_aes == "fixed_color") {
            geom_params$color <- adjcolors(border_color, border_alpha)
        } else if (border_aes == "none") {
            geom_params$color <- NA
        }
    }

    # Only add fill if not mapped and needed
    if (!("fill" %in% names(aes_mapping))) {
        if (!fill_by_is_column && !is.null(fill_by)) {
            geom_params$fill <- fill_by
        }
    }

    geom_layer <- do.call(ggplot2::geom_sf, geom_params)
    layers <- list(geom_layer)

    # Add appropriate scales
    if (fill_by_is_column) {
        if (is.numeric(data_sf[[fill_by]])) {
            # Numeric data - use gradient scale
            layers <- c(layers, list(
                scale_fill_gradientn(
                    name = fill_name %||% fill_by,
                    n.breaks = 4,
                    colors = palette_this(palette = palette, n = 256, reverse = palette_reverse, palcolor = palcolor),
                    guide = if (identical(legend.position, "none")) "none" else guide_colorbar(
                        frame.colour = "black", ticks.colour = "black", title.hjust = 0
                    ),
                    na.value = "transparent"
                )
            ))
        } else {
            # Categorical data - use manual/discrete scale
            layers <- c(layers, list(
                ggplot2::scale_fill_manual(
                    name = fill_name %||% fill_by,
                    values = palette_this(levels(data_sf[[fill_by]]), palette = palette, reverse = palette_reverse, palcolor = palcolor),
                    guide = if (identical(legend.position, "none")) "none" else "legend",
                    na.value = "transparent"
                )
            ))
        }
    }

    # Add color scale only when border_color = TRUE (same as fill)
    if (border_aes == "same_as_fill") {
        if (is.numeric(data_sf[[fill_by]])) {
            # Numeric data - use gradient scale
            layers <- c(layers, list(
                scale_color_gradientn(
                    n.breaks = 4,
                    colors = palette_this(palette = palette, n = 256, reverse = palette_reverse, palcolor = palcolor, alpha = border_alpha),
                    guide = if (identical(legend.position, "none")) "none" else guide_colorbar(
                        frame.colour = "black", ticks.colour = "black", title.hjust = 0
                    ),
                    na.value = "transparent"
                )
            ))
        } else {
            # Categorical data - use manual/discrete scale
            layers <- c(layers, list(
                ggplot2::scale_color_manual(
                    values = palette_this(levels(data_sf[[fill_by]]), palette = palette, reverse = palette_reverse, palcolor = palcolor),
                    guide = "none",  # Always hide guide for border color
                    na.value = "transparent"
                )
            ))
        }
    }

    # Set scale attribute for layers conflicts
    scales_used <- c(
        if (!is.null(fill_by)) "fill",
        if (border_aes == "same_as_fill") "color"
    )

    # Adding the highlight
    if (!isFALSE(highlight) && !is.null(highlight)) {
        if (isTRUE(highlight)) {
            hi_sf <- data_sf
        } else if (length(highlight) == 1 && is.character(highlight)) {
            hi_sf <- dplyr::filter(data_sf, !!!parse_exprs(highlight))
        } else {
            all_inst <- rownames(data_sf) %||% 1:nrow(data_sf)
            if (!any(highlight %in% all_inst)) {
                stop("No highlight items found in the data (rownames).")
            }
            if (!all(highlight %in% all_inst)) {
                warning("Not all highlight items found in the data (rownames).", immediate. = TRUE)
            }
            hi_sf <- data_sf[intersect(highlight, all_inst), , drop = FALSE]
            rm(all_inst)
        }
        if (nrow(hi_sf) > 0) {
            # Add highlight border
            layers <- c(layers, list(
                ggplot2::geom_sf(
                    data = hi_sf,
                    fill = NA,
                    color = highlight_color,
                    linewidth = border_size + highlight_stroke,
                    alpha = highlight_alpha
                )
            ))
            scales_used <- c(scales_used, "color")
        }
    }

    # Set scale attribute for layers conflicts
    attr(layers, "scales") <- unique(scales_used)

    if (return_layer) {
        return(layers)
    }

    p <- .wrap_spatial_layers(layers,
        ext = ext, flip_y = flip_y,
        legend.position = legend.position, legend.direction = legend.direction,
        title = title, subtitle = subtitle, xlab = xlab, ylab = ylab,
        theme = theme, theme_args = theme_args
    )

    if (!is.null(facet_by)) {
        p <- facet_plot(p, facet_by, facet_scales, facet_nrow, facet_ncol, facet_byrow,
                       legend.position = legend.position,
                       legend.direction = legend.direction)
    }

    p
}

#' @rdname spatialplots
#' @concept spatial
#' @export
SpatShapesPlot.data.frame <- function(
    data, x, y, group,
    ext = NULL, flip_y = TRUE,
    fill_by = "grey90", border_color = "black", border_size = 0.5, border_alpha = 1,
    palette = NULL, palcolor = NULL, palette_reverse = FALSE,
    alpha = 1, fill_name = NULL,
    highlight = NULL, highlight_alpha = 1, highlight_size = 1, highlight_color = "black", highlight_stroke = 0.8,
    facet_scales = "fixed", facet_nrow = NULL, facet_ncol = NULL, facet_byrow = TRUE,
    return_layer = FALSE,
    theme = "theme_box", theme_args = list(),
    legend.position = ifelse(return_layer, "none", "right"), legend.direction = "vertical",
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL, seed = 8525
) {
    set.seed(seed)
    ggplot <- if (getOption("plotthis.gglogger.enabled", FALSE)) {
        gglogger::ggplot
    } else {
        ggplot2::ggplot
    }

    x <- check_columns(data, x)
    y <- check_columns(data, y)
    group <- check_columns(data, group, force_factor = TRUE)

    ext <- .prepare_extent(ext)
    if (!is.null(ext)) {
        data <- data[data[[x]] >= ext[1] & data[[x]] <= ext[2] & data[[y]] >= ext[3] & data[[y]] <= ext[4], , drop = FALSE]
        if (nrow(data) == 0) {
            stop("[SpatShapesPlot] No data in the specified extent. Please check your extent.")
        }
    }

    if (flip_y) { data <- .flip_y(data) }

    # Handle multiple fill_by columns for faceting
    if (!is.null(fill_by) && length(fill_by) > 1) {
        # Check that all fill_by columns exist and are numeric
        missing_cols <- fill_by[!fill_by %in% names(data)]
        if (length(missing_cols) > 0) {
            stop("[SpatShapesPlot] Columns not found in data: ", paste(missing_cols, collapse = ", "))
        }

        non_numeric_cols <- fill_by[!sapply(fill_by, function(col) is.numeric(data[[col]]))]
        if (length(non_numeric_cols) > 0) {
            stop("[SpatShapesPlot] Multiple fill_by columns must be numeric. Non-numeric columns: ", paste(non_numeric_cols, collapse = ", "))
        }

        # Reshape data for faceting: convert to long format
        data_long <- data
        # Create a list to store the reshaped data
        facet_data_list <- list()

        for (col in fill_by) {
            temp_data <- data
            temp_data$.fill <- temp_data[[col]]
            temp_data$.facet_var <- col
            facet_data_list[[col]] <- temp_data
        }

        # Combine all facet data
        data <- do.call(rbind, facet_data_list)

        # Convert .facet_var to factor with levels in original fill_by order
        data$.facet_var <- factor(data$.facet_var, levels = fill_by)

        # Set fill_by to the new column name
        fill_by <- ".fill"
        facet_by <- ".facet_var"

    } else {
        # Single column case
        if (!is.null(fill_by) && is.character(fill_by) && length(fill_by) == 1) {
            if (fill_by %in% names(data)) {
                # It's a column name
                fill_by <- check_columns(
                    data, fill_by,
                    force_factor = !is.numeric(data[[fill_by]])
                )
            } else {
                # It's a fixed color - don't treat as column
                fill_by <- fill_by
            }
        }
        facet_by <- NULL
    }

    # Set default palette based on data type
    if (is.null(palette) && !is.null(fill_by) && fill_by %in% names(data)) {
        if (is.numeric(data[[fill_by]])) {
            palette <- "turbo"
        } else {
            palette <- "Paired"
        }
    }
    palette <- palette %||% "turbo"  # fallback default

    # Determine geometry type for appropriate geom
    # Assuming polygons
    # geom_type <- unique(sf::st_geometry_type(data))

    # Build aesthetic mappings
    aes_mapping <- aes(x = !!sym(x), y = !!sym(y), group = !!sym(group))
    fill_by_is_column <- !is.null(fill_by) && is.character(fill_by) && length(fill_by) == 1 && fill_by %in% names(data)

    if (fill_by_is_column) {
        aes_mapping$fill <- sym(fill_by)
    }

    # Handle border_color mapping
    border_aes <- "none"
    if (isTRUE(border_color) && fill_by_is_column) {
        # Use same variable for both fill and color
        aes_mapping$colour <- sym(fill_by)
        border_aes <- "same_as_fill"
    } else if (is.character(border_color) && length(border_color) == 1) {
        # Single color string - will be applied as fixed color
        border_aes <- "fixed_color"
    } else if (isFALSE(border_color)) {
        # No border
        border_aes <- "none"
    }

    # POLYGON, MULTIPOLYGON, or other
    # Build geom parameters conditionally
    geom_params <- list(
        data = data,
        mapping = aes_mapping,
        alpha = alpha,
        linewidth = border_size
    )

    # Only add color if not mapped and needed
    if (!("colour" %in% names(aes_mapping))) {
        if (border_aes == "fixed_color") {
            geom_params$color <- adjcolors(border_color, border_alpha)
        } else if (border_aes == "none") {
            geom_params$color <- NA
        }
    }

    # Only add fill if not mapped and needed
    if (!("fill" %in% names(aes_mapping))) {
        if (!fill_by_is_column && !is.null(fill_by)) {
            geom_params$fill <- fill_by
        }
    }

    geom_layer <- do.call(ggplot2::geom_polygon, geom_params)
    layers <- list(geom_layer)

    # Add appropriate scales
    if (fill_by_is_column) {
        if (is.numeric(data[[fill_by]])) {
            # Numeric data - use gradient scale
            layers <- c(layers, list(
                scale_fill_gradientn(
                    name = fill_name %||% fill_by,
                    n.breaks = 4,
                    colors = palette_this(palette = palette, n = 256, reverse = palette_reverse, palcolor = palcolor),
                    guide = if (identical(legend.position, "none")) "none" else guide_colorbar(
                        frame.colour = "black", ticks.colour = "black", title.hjust = 0
                    ),
                    na.value = "transparent"
                )
            ))
        } else {
            # Categorical data - use manual/discrete scale
            layers <- c(layers, list(
                ggplot2::scale_fill_manual(
                    name = fill_name %||% fill_by,
                    values = palette_this(levels(data[[fill_by]]), palette = palette, reverse = palette_reverse, palcolor = palcolor),
                    guide = if (identical(legend.position, "none")) "none" else "legend",
                    na.value = "transparent"
                )
            ))
        }
    }

    # Add color scale only when border_color = TRUE (same as fill)
    if (border_aes == "same_as_fill") {
        if (is.numeric(data[[fill_by]])) {
            # Numeric data - use gradient scale
            layers <- c(layers, list(
                scale_color_gradientn(
                    n.breaks = 4,
                    colors = palette_this(palette = palette, n = 256, reverse = palette_reverse, palcolor = palcolor, alpha = border_alpha),
                    guide = if (identical(legend.position, "none")) "none" else guide_colorbar(
                        frame.colour = "black", ticks.colour = "black", title.hjust = 0
                    ),
                    na.value = "transparent"
                )
            ))
        } else {
            # Categorical data - use manual/discrete scale
            layers <- c(layers, list(
                ggplot2::scale_color_manual(
                    values = palette_this(levels(data[[fill_by]]), palette = palette, reverse = palette_reverse, palcolor = palcolor),
                    guide = "none",  # Always hide guide for border color
                    na.value = "transparent"
                )
            ))
        }
    }

    # Set scale attribute for layers conflicts
    scales_used <- c(
        if (!is.null(fill_by)) "fill",
        if (border_aes == "same_as_fill") "color"
    )

    # Adding the highlight
    if (!isFALSE(highlight) && !is.null(highlight)) {
        if (isTRUE(highlight)) {
            hi_data <- data
        } else if (length(highlight) == 1 && is.character(highlight)) {
            hi_data <- dplyr::filter(data, !!!parse_exprs(highlight))
        } else {
            all_inst <- rownames(data) %||% 1:nrow(data)
            if (!any(highlight %in% all_inst)) {
                stop("No highlight items found in the data (rownames).")
            }
            if (!all(highlight %in% all_inst)) {
                warning("Not all highlight items found in the data (rownames).", immediate. = TRUE)
            }
            hi_data <- data[intersect(highlight, all_inst), , drop = FALSE]
            rm(all_inst)
        }
        if (nrow(hi_data) > 0) {
            # Add highlight border
            layers <- c(layers, list(
                ggplot2::geom_polygon(
                    data = hi_data,
                    mapping = aes(x = !!sym(x), y = !!sym(y), group = !!sym(group)),
                    fill = NA,
                    color = highlight_color,
                    linewidth = border_size + highlight_stroke,
                    alpha = highlight_alpha
                )
            ))
            scales_used <- c(scales_used, "color")
        }
    }

    # Set scale attribute for layers conflicts
    attr(layers, "scales") <- unique(scales_used)

    if (return_layer) {
        return(layers)
    }

    p <- .wrap_spatial_layers(layers,
        ext = ext, flip_y = flip_y,
        legend.position = legend.position, legend.direction = legend.direction,
        title = title, subtitle = subtitle, xlab = xlab, ylab = ylab,
        theme = theme, theme_args = theme_args
    )

    if (!is.null(facet_by)) {
        p <- facet_plot(p, facet_by, facet_scales, facet_nrow, facet_ncol, facet_byrow,
                       legend.position = legend.position,
                       legend.direction = legend.direction)
    }

    p
}

#' @rdname spatialplots
#' @concept spatial
#' @export
SpatPointsPlot <- function(
    data, x = NULL, y = NULL,
    ext = NULL, flip_y = TRUE, color_by = NULL, size_by = NULL, size = NULL, fill_by = NULL,
    lower_quantile = 0, upper_quantile = 0.99, lower_cutoff = NULL, upper_cutoff = NULL,
    palette = NULL, palcolor = NULL, palette_reverse = FALSE,
    alpha = 1, color_name = NULL, size_name = NULL, shape = 16,
    border_color = "black", border_size = 0.5, border_alpha = 1,
    raster = NULL, raster_dpi = c(512, 512),
    hex = FALSE, hex_linewidth = 0.5, hex_count = FALSE, hex_bins = 50, hex_binwidth = NULL,
    label = FALSE, label_size = 4, label_fg = "white", label_bg = "black", label_bg_r = 0.1,
    label_repel = FALSE, label_repulsion = 20, label_pt_size = 1, label_pt_color = "black",
    label_segment_color = "black", label_insitu = FALSE,
    label_pos = c("median", "mean", "max", "min", "first", "last", "center", "random"),
    highlight = NULL, highlight_alpha = 1, highlight_size = 1, highlight_color = "black", highlight_stroke = 0.8,
    graph = NULL, graph_x = NULL, graph_y = NULL, graph_xend = NULL, graph_yend = NULL, graph_value = NULL,
    edge_size = c(0.05, 0.5), edge_alpha = 0.1, edge_color = "grey40",
    facet_scales = "fixed", facet_nrow = NULL, facet_ncol = NULL, facet_byrow = TRUE,
    return_layer = FALSE, theme = "theme_box", theme_args = list(),
    legend.position = ifelse(return_layer, "none", "right"), legend.direction = "vertical",
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL, seed = 8525
) {
    set.seed(seed)
    ggplot <- if (getOption("plotthis.gglogger.enabled", FALSE)) {
        gglogger::ggplot
    } else {
        ggplot2::ggplot
    }
    stopifnot("'size_by' and 'size' should not be both specified" = is.null(size_by) || is.null(size))
    stopifnot("'size' must be a single numeric value" = is.null(size) || (is.numeric(size) && length(size) == 1))
    size_by <- size_by %||% size

    stopifnot("'data' must be a data.frame" = is.data.frame(data))
    if (!is.null(fill_by) && !is.null(color_by) && fill_by != color_by) {
        stop("Can't use both 'fill_by' and 'color_by'. Always use 'color_by' for points.")
    }
    color_by <- color_by %||% fill_by

    # Determine x and y columns
    if (is.null(x)) {
        x_candidates <- c("x", "X", "sdimx", "coord_x", "spatial_x", "longitude", "lon", "lng")
        x <- x_candidates[x_candidates %in% names(data)][1]
        if (is.na(x)) stop("Could not find x coordinate column. Please specify 'x' parameter.")
    }

    if (is.null(y)) {
        y_candidates <- c("y", "Y", "sdimy", "coord_y", "spatial_y", "latitude", "lat")
        y <- y_candidates[y_candidates %in% names(data)][1]
        if (is.na(y)) stop("Could not find y coordinate column. Please specify 'y' parameter.")
    }
    x <- check_columns(data, x)
    y <- check_columns(data, y)

    # Set up raster parameters similar to DimPlot
    if (length(raster_dpi) == 1) {
        raster_dpi <- rep(raster_dpi, 2)
    }
    raster_is_null <- is.null(raster)
    raster <- raster %||% (nrow(data) > 1e6)
    if (isTRUE(raster) && raster_is_null) {
        warning("[SpatPointsPlot] Rasterization is enabled by default for large datasets (nrow > 1e6). Set 'raster = FALSE' to disable.")
    }

    # Apply extent cropping if specified
    ext <- .prepare_extent(ext)
    if (!is.null(ext)) {
        data <- data[
            data[[x]] >= ext[1] & data[[x]] <= ext[2] &
            data[[y]] >= ext[3] & data[[y]] <= ext[4], , drop = FALSE]
        if (nrow(data) == 0) {
            stop("[SpatPointsPlot] No data in the specified extent. Please check your extent.")
        }
    }

    # Apply y-axis flipping
    if (flip_y) { data <- .flip_y(data, y = y) }

    # Handle multiple color_by columns for faceting
    if (!is.null(color_by) && length(color_by) > 1) {
        # Check that all color_by columns exist and are numeric
        missing_cols <- color_by[!color_by %in% names(data)]
        if (length(missing_cols) > 0) {
            stop("Columns not found in data: ", paste(missing_cols, collapse = ", "))
        }

        non_numeric_cols <- color_by[!sapply(color_by, function(col) is.numeric(data[[col]]))]
        if (length(non_numeric_cols) > 0) {
            stop("Multiple color_by columns must be numeric. Non-numeric columns: ", paste(non_numeric_cols, collapse = ", "))
        }

        # Reshape data for faceting: convert to long format
        facet_data_list <- list()

        for (col in color_by) {
            temp_data <- data
            temp_data$.color <- temp_data[[col]]
            temp_data$.facet_var <- col
            facet_data_list[[col]] <- temp_data
        }

        # Combine all facet data
        data <- do.call(rbind, facet_data_list)

        # Convert .facet_var to factor with levels in original color_by order
        data$.facet_var <- factor(data$.facet_var, levels = color_by)

        # Set color_by to the new column name
        color_by <- ".color"
        facet_by <- ".facet_var"

    } else {
        # Single column case
        if (!is.null(color_by) && is.character(color_by) && length(color_by) == 1) {
            if (color_by %in% names(data)) {
                # It's a column name
                color_by <- check_columns(data, color_by, force_factor = !is.numeric(data[[color_by]]))
            } else {
                # It's a fixed color - don't treat as column
                color_by <- color_by
            }
        }
        facet_by <- NULL
    }

    if (!is.null(color_by) && color_by %in% names(data) && is.numeric(data[[color_by]])) {
        lower_cutoff <- lower_cutoff %||% quantile(data[[color_by]][is.finite(data[[color_by]])], lower_quantile, na.rm = TRUE)
        upper_cutoff <- upper_cutoff %||% quantile(data[[color_by]][is.finite(data[[color_by]])], upper_quantile, na.rm = TRUE)
        if (upper_cutoff == lower_cutoff) {
            if (upper_cutoff == 0) {
                upper_cutoff <- 1e-3
            } else {
                upper_cutoff <- upper_cutoff + upper_cutoff * 1e-3
            }
        }
        data[[color_by]][data[[color_by]] < lower_cutoff] <- lower_cutoff
        data[[color_by]][data[[color_by]] > upper_cutoff] <- upper_cutoff
    }

    # Set default palette based on data type
    if (is.null(palette) && !is.null(color_by) && color_by %in% names(data)) {
        if (is.numeric(data[[color_by]])) {
            palette <- "turbo"
        } else {
            palette <- "Paired"
        }
    }
    palette <- palette %||% "turbo"  # fallback default

    # Check and prepare aesthetic columns
    color_by_is_column <- !is.null(color_by) && is.character(color_by) && length(color_by) == 1 && color_by %in% names(data)
    size_by_is_column <- !is.null(size_by) && is.character(size_by) && length(size_by) == 1 && size_by %in% names(data)

    scales_used <- c()
    # Build aesthetic mappings
    aes_mapping <- aes(x = !!sym(x), y = !!sym(y))

    # Only add aesthetic mappings when they are actual column mappings
    if (size_by_is_column && !is.null(size_by) && size_by %in% names(data)) {
        aes_mapping$size <- sym(size_by)
        scales_used <- c(scales_used, "size")
    }

    # Check if shape supports borders (shapes 21-25)
    has_border <- shape %in% 21:25

    # Handle aesthetic mapping based on shape type
    if (has_border) {
        # For border shapes (21-25), color_by maps to fill when it's a column
        if (color_by_is_column && !is.null(color_by) && color_by %in% names(data) && !isTRUE(raster)) {
            aes_mapping$fill <- sym(color_by)
            scales_used <- c(scales_used, "fill")

            # Handle border_color logic for border shapes
            if (isTRUE(border_color)) {
                # Use same variable for both fill and color (border)
                aes_mapping$colour <- sym(color_by)
                border_aes <- "same_as_fill"
                scales_used <- c(scales_used, "color")
            } else {
                border_aes <- if (is.character(border_color)) "fixed_color" else "none"
            }
        } else {
            # No column mapping for fill
            border_aes <- if (is.character(border_color)) "fixed_color" else "none"
        }
    } else {
        # For non-border shapes, color_by maps to color when it's a column
        if (color_by_is_column && !is.null(color_by) && color_by %in% names(data)) {
            aes_mapping$colour <- sym(color_by)
            scales_used <- c(scales_used, "color")
        }
        border_aes <- "none"
    }

    # Create geom layers with raster and hex support
    layers <- list()

    ## Adding the graph/network
    if (!is.null(graph)) {
        is_coord_graph <- !is.null(graph_x) && !is.null(graph_y) &&
            !is.null(graph_xend) && !is.null(graph_yend) && !is.null(graph_value)

        if (is.character(graph) && length(graph) == 1 && startsWith(graph, "@")) {
            graph <- substring(graph, 2)
            net_mat <- attr(data, graph)
        } else if (is.matrix(graph) || is.data.frame(graph)) {
            net_mat <- graph
        } else if (is.numeric(graph)) {
            graph <- colnames(data)[graph]
            net_mat <- data[graph]
        } else if (is.character(graph)) {
            net_mat <- data[graph]
        } else {
            stop("The 'graph' should be a matrix, data.frame, indexes, or column names.")
        }
        if (is_coord_graph) {
            graph_x <- check_columns(net_mat, graph_x)
            graph_y <- check_columns(net_mat, graph_y)
            graph_xend <- check_columns(net_mat, graph_xend)
            graph_yend <- check_columns(net_mat, graph_yend)
            graph_value <- check_columns(net_mat, graph_value)
            net_mat <- data.frame(
                x = net_mat[[graph_x]], y = net_mat[[graph_y]],
                xend = net_mat[[graph_xend]], yend = net_mat[[graph_yend]],
                value = net_mat[[graph_value]]
            )
        } else {
            if (!is.matrix(net_mat)) {
                net_mat <- as.matrix(net_mat)
            }

            if (!is.null(rownames(net_mat)) && !is.null(colnames(net_mat))) {
                net_mat <- net_mat[rownames(data), rownames(data)]
            } else if (nrow(net_mat) != ncol(net_mat)) {
                stop("[SpatPointsPlot] The graph matrix should be square (same number of rows and columns).")
            } else if (nrow(net_mat) != nrow(data)) {
                stop("[SpatPointsPlot] The graph matrix should have the same number of rows as the input data.")
            }
            net_mat[net_mat == 0] <- NA
            net_mat[upper.tri(net_mat)] <- NA

            handle_single_facet_value <- function(mat) {
                ndf <- reshape2::melt(mat, na.rm = TRUE, stringsAsFactors = FALSE)
                ndf$value <- as.numeric(ndf$value)
                ndf$Var1 <- as.character(ndf$Var1)
                ndf$Var2 <- as.character(ndf$Var2)
                ndf$x <- data[ndf$Var1, x]
                ndf$y <- data[ndf$Var1, y]
                ndf$xend <- data[ndf$Var2, x]
                ndf$yend <- data[ndf$Var2, y]
                return(ndf)
            }

            if (!is.null(facet_by)) {
                net_mat <- do.call(rbind, lapply(split(data, data[, facet_by]), function(d) {
                    d <- handle_single_facet_value(net_mat[rownames(d), rownames(d)])
                    d[, facet_by] <- d[1, facet_by]
                    d
                }))
            } else {
                net_mat <- handle_single_facet_value(net_mat)
            }
        }

        layers <- c(layers, list(
            ggplot2::geom_segment(
                data = net_mat,
                mapping = aes(x = !!sym("x"), y = !!sym("y"), xend = !!sym("xend"),
                    yend = !!sym("yend"), linewidth = !!sym("value")),
                color = edge_color, alpha = edge_alpha, show.legend = FALSE
            ),
            ggplot2::scale_linewidth_continuous(range = edge_size)
        ))
    }

    # Create the main point layers
    if (isTRUE(hex)) {
        # Hex functionality - only for numeric color_by
        if (is.null(color_by)) {
            stop("Hex plotting requires a 'color_by' column to aggregate values.")
        }
        if (!is.null(color_by) && color_by_is_column && !is.numeric(data[[color_by]])) {
            stop("Hex plotting only works with numeric 'color_by' values. Use regular points for categorical data.")
        }

        if (isTRUE(hex_count)) {
            if (is.null(color_by) || !color_by_is_column) {
                stop("Don't know how to count for the hex when 'color_by' is not provided.")
            }
            geom_layer <- ggplot2::geom_hex(
                data = data,
                mapping = aes(x = !!sym(x), y = !!sym(y), color = !!sym(color_by), fill = !!sym(color_by),
                    alpha = after_stat(!!sym("count"))),
                linewidth = hex_linewidth, bins = hex_bins, binwidth = hex_binwidth
            )
            scales_used <- c(scales_used, "fill", "color")
        } else {
            # Hex without color mapping - use stat_summary_hex for aggregation
            data_na <- data[is.na(data[[color_by]]), , drop = FALSE]
            if (nrow(data_na) > 0) {
                geom_layer <- list(
                    ggplot2::geom_hex(
                        data = data_na, mapping = aes(x = !!sym(x), y = !!sym(y)),
                        fill = "grey80", linewidth = hex_linewidth, bins = hex_bins,
                        binwidth = hex_binwidth, alpha = alpha / 2
                    ),
                    ggplot2::stat_summary_hex(
                        data = data[!is.na(data[[color_by]]), , drop = FALSE],
                        mapping = aes(x = !!sym(x), y = !!sym(y), z = !!sym(color_by)),
                        linewidth = hex_linewidth, bins = hex_bins, binwidth = hex_binwidth, alpha = alpha
                    )
                )
            } else {
                geom_layer <- ggplot2::stat_summary_hex(
                    data = data,
                    mapping = aes(x = !!sym(x), y = !!sym(y), z = !!sym(color_by)),
                    linewidth = hex_linewidth, bins = hex_bins, binwidth = hex_binwidth, alpha = alpha
                )
                scales_used <- c(scales_used, "fill")
            }
        }
    } else if (isTRUE(raster)) {
        if (raster_is_null && !identical(raster_dpi, c(512, 512))) {
            message("[SpatPointsPlot] 'raster' is enabled. Point size (size_by) is ignored, try 'raster_dpi' to control resolution.")
        }
        # Use scattermore for rasterized plotting
        if (!color_by_is_column) {
            # No color mapping - use fixed color
            fixed_color <- if (!is.null(color_by) && is.character(color_by)) color_by else "black"
            geom_layer <- scattermore::geom_scattermore(
                data = data,
                mapping = aes(x = !!sym(x), y = !!sym(y)),
                color = fixed_color,
                alpha = alpha,
                pixels = raster_dpi
            )
        } else if (has_border) {
            # Border shapes with color mapping - need two layers
            geom_layer <- list(
                scattermore::geom_scattermore(
                    data = data,
                    mapping = aes(x = !!sym(x), y = !!sym(y)),
                    color = if (border_aes == "fixed_color") adjcolors(border_color, border_alpha) else "black",
                    alpha = alpha,
                    pixels = raster_dpi
                ),
                scattermore::geom_scattermore(
                    data = data,
                    mapping = aes(x = !!sym(x), y = !!sym(y), color = !!sym(color_by)),
                    alpha = alpha,
                    pixels = raster_dpi
                )
            )
            scales_used <- c(scales_used, "color")
        } else {
            # Non-border shapes with color mapping
            geom_layer <- list(scattermore::geom_scattermore(
                data = data,
                mapping = aes(x = !!sym(x), y = !!sym(y), color = !!sym(color_by)),
                alpha = alpha,
                pixels = raster_dpi
            ))
        }
    } else {
        # Standard geom_point - build parameters conditionally
        geom_params <- list(
            data = data,
            mapping = aes_mapping,
            alpha = alpha,
            shape = shape
        )
        if (has_border) geom_params$stroke <- border_size

        # Only add size parameter when not mapped
        if (!size_by_is_column && is.numeric(size_by)) {
            geom_params$size <- size_by
        }

        # Only add color parameter when not mapped
        if (!("colour" %in% names(aes_mapping))) {
            if (has_border && border_aes == "fixed_color") {
                geom_params$color <- adjcolors(border_color, border_alpha)
            } else if (has_border && border_aes == "none") {
                geom_params$color <- NA
            } else if (!has_border && !color_by_is_column && !is.null(color_by) && is.character(color_by)) {
                geom_params$color <- color_by
            } else if (!has_border && !color_by_is_column) {
                geom_params$color <- "black"
            }
        }

        # Only add fill parameter when not mapped
        if (!("fill" %in% names(aes_mapping))) {
            if (has_border && !color_by_is_column && !is.null(color_by) && is.character(color_by)) {
                geom_params$fill <- color_by
            }
        }

        geom_layer <- do.call(ggplot2::geom_point, geom_params)
    }

    if (inherits(geom_layer, c("gg", "Layer"))) {
        layers <- c(layers, list(geom_layer))
    } else {
        layers <- c(layers, geom_layer)
    }

    # Add appropriate scales
    if (color_by_is_column && !is.null(color_by)) {
        if ("fill" %in% scales_used) {
            if (is.numeric(data[[color_by]])) {
                layers <- c(layers, list(
                    scale_fill_gradientn(
                        name = color_name %||% color_by,
                        n.breaks = 4,
                        colors = palette_this(palette = palette, n = 256, reverse = palette_reverse, palcolor = palcolor),
                        guide = if (identical(legend.position, "none")) "none" else guide_colorbar(
                            frame.colour = "black", ticks.colour = "black", title.hjust = 0
                        ),
                        na.value = "transparent"
                    )
                ))
            } else {
                layers <- c(layers, list(
                    ggplot2::scale_fill_manual(
                        name = color_name %||% color_by,
                        values = palette_this(levels(data[[color_by]]), palette = palette, reverse = palette_reverse, palcolor = palcolor),
                        guide = if (identical(legend.position, "none")) "none" else "legend",
                        na.value = "transparent"
                    )
                ))
            }
        }
        if ("color" %in% scales_used) {
            if (is.numeric(data[[color_by]])) {
                if (has_border && border_aes == "same_as_fill") {
                    colors <- palette_this(palette = palette, n = 256, reverse = palette_reverse, palcolor = palcolor,
                        alpha = border_alpha, transparent = FALSE)
                } else {
                    colors <- palette_this(palette = palette, n = 256, reverse = palette_reverse, palcolor = palcolor)
                }
                layers <- c(layers, list(
                    scale_color_gradientn(
                        name = color_name %||% color_by,
                        n.breaks = 4,
                        colors = colors,
                        guide = if (identical(legend.position, "none") || "fill" %in% scales_used) "none" else guide_colorbar(
                            frame.colour = "black", ticks.colour = "black", title.hjust = 0
                        ),
                        na.value = "transparent"
                    )
                ))
            } else {
                if (has_border && border_aes == "same_as_fill") {
                    # Use same colors as fill
                    colors <- palette_this(levels(data[[color_by]]), palette = palette, reverse = palette_reverse, palcolor = palcolor,
                        alpha = border_alpha, transparent = FALSE)
                } else {
                    colors <- palette_this(levels(data[[color_by]]), palette = palette, reverse = palette_reverse, palcolor = palcolor)
                }
                layers <- c(layers, list(
                    ggplot2::scale_color_manual(
                        name = color_name %||% color_by,
                        values = colors,
                        guide = if (identical(legend.position, "none") || "fill" %in% scales_used) "none" else "legend",
                        na.value = "transparent"
                    )
                ))
            }
        }
    }

    if ("size" %in% scales_used) {
        layers <- c(layers, list(
            ggplot2::scale_size_continuous(
                name = size_name %||% size_by,
                guide = if (identical(legend.position, "none")) "none" else "legend"
            )
        ))
    }

    # Adding the highlight (similar to DimPlot)
    if (!isFALSE(highlight) && !is.null(highlight)) {
        if (isTRUE(hex)) {
            stop("Highlight is not supported for hex plot.")
        }
        if (isTRUE(highlight)) {
            hi_df <- data
        } else if (length(highlight) == 1 && is.character(highlight)) {
            hi_df <- dplyr::filter(data, !!!parse_exprs(highlight))
        } else {
            all_inst <- rownames(data) %||% 1:nrow(data)
            if (!any(highlight %in% all_inst)) {
                stop("No highlight items found in the data (rownames).")
            }
            if (!all(highlight %in% all_inst)) {
                warning("Not all highlight items found in the data (rownames).", immediate. = TRUE)
            }
            hi_df <- data[intersect(highlight, all_inst), , drop = FALSE]
            rm(all_inst)
        }
        if (nrow(hi_df) > 0) {
            if (isTRUE(raster)) {
                layers <- c(layers, list(
                    scattermore::geom_scattermore(
                        data = hi_df, aes(x = !!sym(x), y = !!sym(y)), color = highlight_color,
                        pointsize = floor(highlight_size) + highlight_stroke, alpha = highlight_alpha, pixels = raster_dpi
                    ),
                    scattermore::geom_scattermore(
                        data = hi_df, aes(x = !!sym(x), y = !!sym(y), color = !!sym(color_by)),
                        pointsize = floor(highlight_size), alpha = highlight_alpha, pixels = raster_dpi
                    )
                ))
                scales_used <- c(scales_used, "color")
            } else {
                layers <- c(layers, list(
                    geom_point(
                        data = hi_df, aes(x = !!sym(x), y = !!sym(y)), color = highlight_color,
                        size = highlight_size + highlight_stroke, alpha = highlight_alpha
                    ),
                    geom_point(
                        data = hi_df, aes(x = !!sym(x), y = !!sym(y), color = !!sym(color_by)),
                        size = highlight_size, alpha = highlight_alpha
                    )
                ))
                scales_used <- c(scales_used, "color")
            }
        }
    }

    # Force label to be TRUE when label_repel or label_insitu is TRUE (similar to DimPlot)
    if ((isTRUE(label_repel) || isTRUE(label_insitu)) && !isTRUE(label)) {
        message("Forcing label to be TRUE when label_repel or label_insitu is TRUE.")
        label <- TRUE
    }

    # Adding the labels (similar to DimPlot)
    if (isTRUE(label)) {
        if (is.null(color_by) || !color_by_is_column) {
            stop("Adding labels requires 'color_by' to be specified as a categorical column.")
        }
        if (is.numeric(data[[color_by]])) {
            stop("Adding labels is not supported for numeric 'color_by' values. Use categorical data.")
        }
        if (isTRUE(hex)) {
            stop("Adding labels is not supported for hex plots.")
        }

        if (is.character(label_pos)) {
            label_pos <- match.arg(label_pos)
            if (label_pos == "median") {
                label_pos <- function(x) median(x, na.rm = TRUE)
            } else if (label_pos == "mean") {
                label_pos <- function(x) mean(x, na.rm = TRUE)
            } else if (label_pos == "first") {
                label_pos <- function(x) x[1]
            } else if (label_pos == "last") {
                label_pos <- function(x) x[length(x)]
            } else if (label_pos == "random") {
                label_pos <- function(x) sample(x, 1)
            } else if (label_pos == "center") {
                label_pos <- function(x) mean(range(x, na.rm = TRUE))
            } else if (label_pos == "min") {
                label_pos <- function(x) min(x, na.rm = TRUE)
            } else if (label_pos == "max") {
                label_pos <- function(x) max(x, na.rm = TRUE)
            } else {
                stop("Invalid label position specified. Use 'median', 'mean', 'first', 'last', 'random', or 'center'.")
            }
        }

        if (!is.null(facet_by)) {
            label_df <- aggregate(data[, c(x, y)], by = list(data[[color_by]], data[[facet_by]]), FUN = label_pos)
            colnames(label_df)[1:2] <- c(".label", facet_by)
        } else {
            label_df <- aggregate(data[, c(x, y)], by = list(data[[color_by]]), FUN = label_pos)
            colnames(label_df)[1] <- ".label"
        }
        label_df <- label_df[!is.na(label_df[, ".label"]), , drop = FALSE]

        if (!isTRUE(label_insitu)) {
            label_df[, ".label"] <- seq_len(nrow(label_df))
        }

        if (isTRUE(label_repel)) {
            layers <- c(layers, list(
                geom_point(
                    data = label_df, mapping = aes(x = !!sym(x), y = !!sym(y)),
                    color = label_pt_color, size = label_pt_size
                ),
                ggrepel::geom_text_repel(
                    data = label_df, aes(x = !!sym(x), y = !!sym(y), label = !!sym(".label")),
                    point.size = label_pt_size, max.overlaps = 100, force = label_repulsion,
                    color = label_fg, bg.color = label_bg, bg.r = label_bg_r, size = label_size, inherit.aes = FALSE
                )
            ))
        } else {
            layers <- c(layers, list(
                ggrepel::geom_text_repel(
                    data = label_df, aes(x = !!sym(x), y = !!sym(y), label = !!sym(".label")),
                    fontface = "bold", min.segment.length = 0, segment.color = label_segment_color,
                    point.size = NA, max.overlaps = 100, force = 0,
                    color = label_fg, bg.color = label_bg, bg.r = label_bg_r, size = label_size, inherit.aes = FALSE
                )
            ))
        }
    }

    # Set scale attribute for layers conflicts
    attr(layers, "scales") <- unique(scales_used)

    if (return_layer) {
        return(layers)
    }

    p <- .wrap_spatial_layers(layers,
        ext = ext, flip_y = flip_y,
        legend.position = legend.position, legend.direction = legend.direction,
        title = title, subtitle = subtitle, xlab = xlab, ylab = ylab,
        theme = theme, theme_args = theme_args
    )

    if (!is.null(facet_by)) {
        p <- facet_plot(p, facet_by, facet_scales, facet_nrow, facet_ncol, facet_byrow,
                       legend.position = legend.position,
                       legend.direction = legend.direction)
    }

    p
}
