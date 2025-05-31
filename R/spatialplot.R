#' @title Flip values on the y-axis direction, and negate the Y-Coordinates of SpatRaster and SpatVector Objects
#'
#' @description
#' These internal functions flip the y-coordinates of `SpatRaster` and `SpatVector` objects from the `terra` package.
#' For rasters, the function vertically flips the raster and adjusts its extent accordingly.
#' For vectors, the function negates the y-coordinates of all geometries, supporting both point and polygon/line types.
#'
#' @param data A `SpatRaster` or `SpatVector` object from the `terra` package.
#'
#' @return
#' For `flip_y_spatraster`, a `SpatRaster` object with flipped y-coordinates and adjusted extent.
#' For `flip_y_spatvector`, a `SpatVector` object with y-coordinates negated.
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
#' @rdname flip_y_spat
flip_y_spatraster <- function(data) {
    if (!inherits(data, "SpatRaster")) {
        stop("'data' must be a SpatRaster object")
    }

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
#' @rdname flip_y_spat
flip_y_spatvector <- function(data) {
    if (!inherits(data, "SpatVector")) {
        stop("'data' must be a SpatVector object")
    }

    # Get the extent of the original data
    ext_orig <- terra::ext(data)

    # Manual coordinate transformation approach (more reliable)
    # Get all geometries and transform coordinates
    # Expecting polygons
    geom_type <- terra::geomtype(data)

    # For polygons/lines, use terra geometric operations
    # Get all coordinates and apply the same transformation as raster
    coords_list <- terra::geom(data)
    coords_list[, "y"] <- -coords_list[, "y"]

    # Recreate the SpatVector with flipped coordinates
    data_flipped <- terra::vect(coords_list, type = geom_type, atts = terra::values(data))

    return(data_flipped)
}

#' Plots for spatial elements
#'
#' * `SpatialImagePlot`: Plot a SpatRaster object as an image.
#' * `SpatialMasksPlot`: Plot a SpatRaster object as masks.
#' * `SpatialShapesPlot`: Plot a SpatVector object as shapes.
#' * `SpatialPointsPlot`: Plot a data.frame of points with spatial coordinates.
#'
#' @rdname spatialplots
#' @concept spatial
#' @inheritParams common_args
#' @param data A `SpatRaster` or `SpatVector` object from the `terra` package, or a data.frame for `SpatialPointsPlot`.
#' @param ext A numeric vector of length 4 specifying the extent as `c(xmin, xmax, ymin, ymax)`. Default is NULL.
#' @param flip_y Whether to flip the y-axis direction. Default is TRUE.
#' This is useful for visualizing spatial data with the origin at the top left corner.
#' @param palette_reverse Whether to reverse the color palette. Default is FALSE.
#' @param fill_by A character string or vector specifying the column(s) to fill the shapes in `SpatialShapesPlot`.
#' @param fill_name A character string for the fill legend title.
#' @param color_by A character string specifying the column to color the points in `SpatialPointsPlot`.
#' @param color_name A character string for the color legend title in `SpatialPointsPlot`.
#' @param size_by A character string specifying the column to size the points in `SpatialPointsPlot`.
#' @param size_name A character string for the size legend title in `SpatialPointsPlot`.
#' @param shape A numeric value or character string specifying the shape of the points in `SpatialPointsPlot`.
#' @param add_border Whether to add a border around the masks in `SpatialMasksPlot`. Default is TRUE.
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
#' @param return_layer Whether to return the layer or the plot. Default is FALSE.
#' @importFrom rlang %||%
#' @importFrom ggplot2 scale_fill_gradientn aes guide_colorbar element_blank
#' @importFrom ggplot2 geom_sf ylim labs coord_sf geom_hex stat_summary_hex
#' @export
#' @examples
#' \donttest{
#' set.seed(8525)
#' # --- SpatialImagePlot ---
#' # Generate a sample SpatRaster
#' r <- terra::rast(
#'     nrows = 50, ncols = 40, vals = runif(2000),
#'     xmin = 0, xmax = 40, ymin = 0, ymax = 50,
#'     crs = ""
#' )
#' SpatialImagePlot(r)
#' SpatialImagePlot(r, raster = TRUE, raster_dpi = 20)
#' SpatialImagePlot(r, alpha = 0.5, theme = "theme_blank",
#'     theme_args = list(add_coord = FALSE), fill_name = "value")
#' SpatialImagePlot(r, ext = c(0, 10, 0, 10), flip_y = FALSE, palette = "viridis")
#'
#' # --- SpatialMasksPlot ---
#' m <- terra::rast(
#'    nrows = 50, ncols = 40,
#'    vals = sample(c(1:5, NA), 2000, replace = TRUE, prob = c(rep(0.04, 5), 0.8)),
#'    xmin = 0, xmax = 40, ymin = 0, ymax = 50,
#'    crs = ""
#' )
#' SpatialMasksPlot(m, border_color = "red")
#' SpatialMasksPlot(m, ext = c(0, 15, 0, 20), add_border = FALSE,
#'     palette_reverse = TRUE, fill_name = "value")
#'
#' # --- SpatialShapesPlot ---
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
#' SpatialShapesPlot(polygons)
#' SpatialShapesPlot(polygons, ext = c(0, 20, 0, 20))
#' SpatialShapesPlot(polygons, border_color = "red", border_size = 2)
#' SpatialShapesPlot(polygons, fill_by = "cat", fill_name = "category")
#' # Let border color be determined by fill
#' SpatialShapesPlot(polygons, fill_by = "cat", alpha = 0.6, border_color = TRUE)
#' SpatialShapesPlot(polygons, fill_by = "feat1")
#' SpatialShapesPlot(polygons, fill_by = c("feat1", "feat2"), palette = "RdYlBu")
#'
#' # --- SpatialPointsPlot ---
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
#' SpatialPointsPlot(points)
#' SpatialPointsPlot(points, color_by = "gene", size_by = "size", shape = 22,
#'   border_size = 1)
#' SpatialPointsPlot(points, raster = TRUE, raster_dpi = 30, color_by = "feat1")
#' SpatialPointsPlot(points, color_by = c("feat1", "feat2"), size_by = "size")
#' SpatialPointsPlot(points, color_by = "feat1", hex = TRUE)
#'
#' # --- Use the `return_layer` argument to get the ggplot layer
#' ext = c(0, 40, 0, 50)
#' ggplot2::ggplot() +
#'   SpatialImagePlot(r, return_layer = TRUE, alpha = 0.2, ext = ext) +
#'   SpatialShapesPlot(polygons, return_layer = TRUE, ext = ext, fill_by = "white") +
#'   SpatialPointsPlot(points, return_layer = TRUE, ext = ext, color_by = "feat1") +
#'   theme_this() +
#'   ggplot2::coord_sf(expand = 0) +
#'   ggplot2::scale_y_continuous(labels = function(x) -x)
#'
#' }
SpatialImagePlot <- function(
    data,
    ext = NULL, raster = NULL, raster_dpi = NULL, flip_y = TRUE,
    palette = "turbo", palcolor = NULL, palette_reverse = FALSE,
    alpha = 1, fill_name = NULL, return_layer = FALSE,
    theme = "theme_this", theme_args = list(),
    legend.position = ifelse(return_layer, "none", "right"), legend.direction = "vertical",
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL, seed = 8525,
    ...
) {
    set.seed(seed)
    ggplot <- if (getOption("plotthis.gglogger.enabled", FALSE)) {
        gglogger::ggplot
    } else {
        ggplot2::ggplot
    }

    stopifnot("'data' must be a SpatRaster object" = inherits(data, "SpatRaster"))

    if (!is.null(ext)) {
        if (is.numeric(ext) && length(ext) == 4) {
            # Convert to terra extent
            ext <- terra::ext(ext[1], ext[2], ext[3], ext[4])
        }
        stopifnot("'ext' must be a valid extent or 4 numeric values" = inherits(ext, "SpatExtent"))
        data <- terra::crop(data, ext)
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

    if (flip_y) {
        data <- flip_y_spatraster(data)
    }

    layer <- list(
        ggplot2::geom_raster(
            data = stats::setNames(terra::as.data.frame(data, xy = TRUE), c("x", "y", "value")),
            aes(x = !!sym("x"), y = !!sym("y"), fill = !!sym("value")),
            alpha = alpha
        ),
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
    )
    # If we add another layer, we need the ggnewscale::new_scale_fill() to avoid conflicts
    attr(layer, "scales") <- "fill"

    if (return_layer) {
        return(layer)
    }

    p <- ggplot() +
        layer +
        coord_sf(expand = 0) +
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
        p <- p + scale_y_continuous(labels = function(x) -x)
    }

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

    attr(p, "height") <- base_height
    attr(p, "width") <- base_width

    return(p)
}

#' @rdname spatialplots
#' @concept spatial
#' @export
SpatialMasksPlot <- function(
    data,
    ext = NULL, flip_y = TRUE, add_border = TRUE, border_color = "black",
    border_size = 0.5, border_alpha = 1,
    palette = "turbo", palcolor = NULL, palette_reverse = FALSE,
    alpha = 1, fill_name = NULL, return_layer = FALSE,
    theme = "theme_this", theme_args = list(),
    legend.position = "right", legend.direction = "vertical",
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL, seed = 8525,
    ...
) {
    set.seed(seed)
    ggplot <- if (getOption("plotthis.gglogger.enabled", FALSE)) {
        gglogger::ggplot
    } else {
        ggplot2::ggplot
    }

    stopifnot("'data' must be a SpatRaster object" = inherits(data, "SpatRaster"))

    if (!is.null(ext)) {
        if (is.numeric(ext) && length(ext) == 4) {
            # Convert to terra extent
            ext <- terra::ext(ext[1], ext[2], ext[3], ext[4])
        }
        stopifnot("'ext' must be a valid extent or 4 numeric values" = inherits(ext, "SpatExtent"))
        data <- terra::crop(data, ext)
    }

    if (flip_y) {
        data <- flip_y_spatraster(data)
    }

    # Set background (0 values) to NA for transparency
    data[data == 0] <- NA

    layer <- list(
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
        layer <- c(
            layer,
            list(ggplot2::geom_sf(
                data = sf::st_as_sf(polys),
                fill = NA,
                color = border_color,
                linewidth = border_size,
                alpha = border_alpha
            ))
        )
    }

    layer <- c(
        layer,
        scale_fill_gradientn(
            name = fill_name %||% names(data)[1],
            n.breaks = 4,
            colors = palette_this(palette = palette, n = 256, reverse = palette_reverse, palcolor = palcolor),
            guide = if (identical(legend.position, "none")) "none" else guide_colorbar(
                frame.colour = "black", ticks.colour = "black", title.hjust = 0
            ),
            na.value = "transparent"
        )
    )
    # If we add another layer, we need the ggnewscale::new_scale_fill() to avoid conflicts
    attr(layer, "scales") <- "fill"

    if (return_layer) {
        return(layer)
    }

    p <- ggplot() +
        layer +
        coord_sf(expand = 0) +
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
        p <- p + scale_y_continuous(labels = function(x) -x)
    }

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

    attr(p, "height") <- base_height
    attr(p, "width") <- base_width

    return(p)
}

#' @rdname spatialplots
#' @concept spatial
#' @export
SpatialShapesPlot <- function(
    data,
    ext = NULL, flip_y = TRUE,
    fill_by = NULL, border_color = "black", border_size = 0.5, border_alpha = 1,
    palette = NULL, palcolor = NULL, palette_reverse = FALSE,
    alpha = 1, fill_name = NULL,
    facet_scales = "fixed", facet_nrow = NULL, facet_ncol = NULL, facet_byrow = TRUE,
    return_layer = FALSE,
    theme = "theme_this", theme_args = list(),
    legend.position = ifelse(return_layer, "none", "right"), legend.direction = "vertical",
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL, seed = 8525,
    ...
) {
    set.seed(seed)
    ggplot <- if (getOption("plotthis.gglogger.enabled", FALSE)) {
        gglogger::ggplot
    } else {
        ggplot2::ggplot
    }

    stopifnot("'data' must be a SpatVector object" = inherits(data, "SpatVector"))

    if (!is.null(ext)) {
        if (is.numeric(ext) && length(ext) == 4) {
            # Convert to terra extent
            ext <- terra::ext(ext[1], ext[2], ext[3], ext[4])
        }
        stopifnot("'ext' must be a valid extent or 4 numeric values" = inherits(ext, "SpatExtent"))
        data <- terra::crop(data, ext)
    }

    if (flip_y) {
        data <- flip_y_spatvector(data)
    }

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
            geom_params$color <- border_color
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
    layer <- list(geom_layer)

    # Add appropriate scales
    if (fill_by_is_column) {
        if (is.numeric(data_sf[[fill_by]])) {
            # Numeric data - use gradient scale
            layer <- c(layer, list(
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
            layer <- c(layer, list(
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
            layer <- c(layer, list(
                scale_color_gradientn(
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
            layer <- c(layer, list(
                ggplot2::scale_color_manual(
                    values = palette_this(levels(data_sf[[fill_by]]), palette = palette, reverse = palette_reverse, palcolor = palcolor),
                    guide = "none",  # Always hide guide for border color
                    na.value = "transparent"
                )
            ))
        }
    }

    # Set scale attribute for layer conflicts
    attr(layer, "scales") <- c(
        if (!is.null(fill_by)) "fill",
        if (border_aes == "same_as_fill") "color"
    )

    if (return_layer) {
        return(layer)
    }

    p <- ggplot() +
        layer +
        coord_sf(expand = 0) +
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
        p <- p + scale_y_continuous(labels = function(x) -x)
    }

    # Set default width and height based on extent proportions (before faceting)
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

    attr(p, "height") <- base_height
    attr(p, "width") <- base_width

    # Use facet_plot instead of raw ggplot2::facet_wrap
    if (!is.null(facet_by)) {
        p <- facet_plot(p, facet_by, facet_scales, facet_nrow, facet_ncol, facet_byrow,
                       legend.position = legend.position,
                       legend.direction = legend.direction)
    }

    return(p)
}

#' @rdname spatialplots
#' @concept spatial
#' @export
SpatialPointsPlot <- function(
    data, x = NULL, y = NULL,
    ext = NULL, flip_y = TRUE, color_by = NULL, size_by = NULL, fill_by = NULL,
    palette = NULL, palcolor = NULL, palette_reverse = FALSE,
    alpha = 1, color_name = NULL, size_name = NULL, shape = 16,
    border_color = "black", border_size = 0.5, border_alpha = 1,
    raster = NULL, raster_dpi = c(512, 512),
    hex = FALSE, hex_linewidth = 0.5, hex_count = FALSE, hex_bins = 50, hex_binwidth = NULL,
    facet_scales = "fixed", facet_nrow = NULL, facet_ncol = NULL, facet_byrow = TRUE,
    return_layer = FALSE, theme = "theme_this", theme_args = list(),
    legend.position = ifelse(return_layer, "none", "right"), legend.direction = "vertical",
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL, seed = 8525,
    ...
) {
    set.seed(seed)
    ggplot <- if (getOption("plotthis.gglogger.enabled", FALSE)) {
        gglogger::ggplot
    } else {
        ggplot2::ggplot
    }

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
    raster <- raster %||% (nrow(data) > 1e5)

    # Apply extent cropping if specified
    if (!is.null(ext)) {
        if (is.numeric(ext) && length(ext) == 4) {
            # Convert to named extent: c(xmin, xmax, ymin, ymax)
            ext_vals <- ext
        } else {
            stop("'ext' must be 4 numeric values: c(xmin, xmax, ymin, ymax)")
        }

        data <- data[
            data[[x]] >= ext_vals[1] & data[[x]] <= ext_vals[2] &
            data[[y]] >= ext_vals[3] & data[[y]] <= ext_vals[4], , drop = FALSE]
    }

    # Apply y-axis flipping
    if (flip_y) {
        data[[y]] <- -data[[y]]
    }

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
        if (color_by_is_column && !is.null(color_by) && color_by %in% names(data)) {
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

    # Create geom layer with raster and hex support
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
        if (raster_is_null && raster && !identical(raster_dpi, c(512, 512))) {
            message("[SpatialPointsPlot] 'raster' is enabled. Point size (size_by) is ignore, try 'raster_dpi' to control resolution.")
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
                    color = if (border_aes == "fixed_color") border_color else "black",
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
                geom_params$color <- border_color
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
        layer <- list(geom_layer)
    } else {
        layer <- geom_layer
    }

    # Add appropriate scales
    if (color_by_is_column && !is.null(color_by)) {
        if ("fill" %in% scales_used) {
            if (is.numeric(data[[color_by]])) {
                layer <- c(layer, list(
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
                layer <- c(layer, list(
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
                layer <- c(layer, list(
                    scale_color_gradientn(
                        name = color_name %||% color_by,
                        n.breaks = 4,
                        colors = palette_this(palette = palette, n = 256, reverse = palette_reverse, palcolor = palcolor),
                        guide = if (identical(legend.position, "none") || "fill" %in% scales_used) "none" else guide_colorbar(
                            frame.colour = "black", ticks.colour = "black", title.hjust = 0
                        ),
                        na.value = "transparent"
                    )
                ))
            } else {
                layer <- c(layer, list(
                    ggplot2::scale_color_manual(
                        name = color_name %||% color_by,
                        values = palette_this(levels(data[[color_by]]), palette = palette, reverse = palette_reverse, palcolor = palcolor),
                        guide = if (identical(legend.position, "none") || "fill" %in% scales_used) "none" else "legend",
                        na.value = "transparent"
                    )
                ))
            }
        }
    }

    if ("size" %in% scales_used) {
        layer <- c(layer, list(
            ggplot2::scale_size_continuous(
                name = size_name %||% size_by,
                guide = if (identical(legend.position, "none")) "none" else "legend"
            )
        ))
    }

    # Set scale attribute for layer conflicts
    attr(layer, "scales") <- unique(scales_used)

    if (return_layer) {
        return(layer)
    }

    p <- ggplot() +
        layer +
        coord_sf(expand = 0) +
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
        p <- p + scale_y_continuous(labels = function(x) -x)
    }

    # Set default width and height based on extent proportions (before faceting)
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

    attr(p, "height") <- base_height
    attr(p, "width") <- base_width

    # Use facet_plot instead of raw ggplot2::facet_wrap
    if (!is.null(facet_by)) {
        p <- facet_plot(p, facet_by, facet_scales, facet_nrow, facet_ncol, facet_byrow,
                       legend.position = legend.position,
                       legend.direction = legend.direction)
    }

    return(p)
}
