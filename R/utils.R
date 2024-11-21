#' Check the columns if columns found in the data
#' @keywords internal
#' @param df A data frame
#' @param columns A character vector of column names
#' @param force_factor Whether to force the columns to be factors
#' @param allow_multi Whether to allow multiple columns
#' @param concat_multi Whether to concatenate multiple columns
#' @param concat_sep The separator to use for concatenation
#' @return A character string of the valid column
#' @importFrom tidyr unite
#' @importFrom rlang syms
check_columns <- function(
    df,
    columns,
    force_factor = FALSE,
    allow_multi = FALSE,
    concat_multi = FALSE,
    concat_sep = "_") {
    if (is.null(df)) {
        stop("The data is NULL.")
    }
    if (is.null(columns)) {
        return(NULL)
    }
    df_name <- deparse(substitute(df))
    param_name <- deparse(substitute(columns))
    if (isFALSE(allow_multi)) {
        if (length(columns) > 1) {
            stop(paste0("Only one column is allowed in '", param_name, "'"))
        }
        if (!columns %in% colnames(df)) {
            stop(paste0("'", columns, "' is/are not in the data."))
        }
    } else {
        notfound <- setdiff(columns, colnames(df))
        if (length(notfound) > 0) {
            stop(paste0("'", paste0(notfound, collapse = ", "), "' is/are not in the data."))
        }
        if (isTRUE(concat_multi) && length(columns) > 1) {
            message(
                "Multiple columns are provided in '", param_name,
                "'. They will be concatenated into one column."
            )
            new_col <- paste(columns, collapse = concat_sep)
            df <- unite(df, !!sym(new_col), !!!syms(columns), sep = concat_sep, remove = FALSE)
            # Try keep the order of levels
            if (isTRUE(force_factor)) {
                all_levels <- lapply(columns, function(col) {
                    if (is.factor(df[[col]])) {
                        levels(df[[col]])
                    } else {
                        unique(df[[col]])
                    }
                })
                all_levels <- do.call(expand_grid, all_levels)
                all_levels <- apply(all_levels, 1, paste, collapse = concat_sep)
                df[[new_col]] <- droplevels(factor(df[[new_col]], levels = unique(all_levels)))
            }
            columns <- new_col
        }
    }
    if (isTRUE(force_factor)) {
        p <- parent.frame()
        for (col in columns) {
            if (!is.factor(df[[col]])) {
                p[[df_name]][[col]] <- factor(df[[col]], levels = unique(df[[col]]))
            } else if (!col %in% colnames(p[[df_name]])) {
                p[[df_name]][[col]] <- df[[col]]
            }
        }
    }
    return(columns)
}


#' Expand the plot area with CSS-like padding
#' @keywords internal
#' @param expand A numeric vector of length 1, 2, 3, or 4
#'   The values to expand the x and y axes. It is like CSS padding.
#'   When a single value is provided, it is used for both axes on both sides.
#'   When two values are provided, the first value is used for the top/bottom side and the second value is used for the left/right side.
#'   When three values are provided, the first value is used for the top side, the second value is used for the left/right side, and the third value is used for the bottom side.
#'   When four values are provided, the values are used for the top, right, bottom, and left sides, respectively.
#'   You can also use a named vector to specify the values for each side.
#'   When the axis is discrete, the values will be applied as 'add' to the 'expansion' function.
#'   When the axis is continuous, the values will be applied as 'mult' to the 'expansion' function.
#'   See also \url{https://ggplot2.tidyverse.org/reference/expansion.html}
#' @param x_type The type of x-axis, either "continuous" or "discrete"
#' @param y_type The type of y-axis, either "continuous" or "discrete"
#' @return A list with x and y values for expand
#' @importFrom ggplot2 expansion
norm_expansion <- function(expand, x_type, y_type, continuous_default = c(0.05, 0), discrete_default = c(0, 0.6)) {
    .expand_by_type <- function(ex, type, both = FALSE) {
        if (type == "continuous" && !is.null(ex)) {
            ret <- c(ex, 0)
        } else if (type == "continuous" && is.null(ex)) {
            ret <- continuous_default
        } else if (type == "discrete" && !is.null(ex)) {
            ret <- c(0, ex)
        } else {
            # type == "discrete" && is.null(ex)
            ret <- discrete_default
        }
        if (both) {
            return(c(ret, ret))
        }
        return(ret)
    }

    if (is.null(expand)) {
        return(list(
            x = .expand_by_type(NULL, x_type, both = TRUE),
            y = .expand_by_type(NULL, y_type, both = TRUE)
        ))
    }
    if (is.null(names(expand))) {
        if (length(expand) == 1) {
            expand <- c(top = expand, right = expand, bottom = expand, left = expand)
        } else if (length(expand) == 2) {
            expand <- c(top = expand[1], right = expand[2], bottom = expand[1], left = expand[2])
        } else if (length(expand) == 3) {
            expand <- c(top = expand[1], right = expand[2], bottom = expand[3], left = expand[2])
        } else if (length(expand) == 4) {
            expand <- c(top = expand[1], right = expand[2], bottom = expand[3], left = expand[4])
        } else {
            stop("Invalid length (", length(expand), ") of 'expand'")
        }
    }

    expand <- as.list(expand)
    if ("x" %in% names(expand) && ("left" %in% names(expand) || "right" %in% names(expand))) {
        stop("Cannot have both 'x' and 'left'/'right' in 'expand'")
    }
    if ("y" %in% names(expand) && ("top" %in% names(expand) || "bottom" %in% names(expand))) {
        stop("Cannot have both 'y' and 'top'/'bottom' in 'expand'")
    }
    if ("x" %in% names(expand)) {
        expand$left <- expand$right <- expand$x
        expand$x <- NULL
    }
    if ("y" %in% names(expand)) {
        expand$bottom <- expand$top <- expand$y
        expand$y <- NULL
    }
    return(list(
        x = c(.expand_by_type(expand$left, x_type), .expand_by_type(expand$right, x_type)),
        y = c(.expand_by_type(expand$bottom, y_type), .expand_by_type(expand$top, y_type))
    ))
}

#' Calculate hjust and vjust based on angle
#' @param angle A numeric value of the angle
#' @return A list with h and v values
#' @keywords internal
calc_just <- function(angle) {
    angle <- angle %% 360
    if (angle < 0) {
        angle <- angle + 360
    }
    if (angle < 10) {
        h <- 0.5
        v <- 1
    } else if (angle < 90) {
        h <- 1
        v <- 1
    } else if (angle < 135) {
        h <- 1
        v <- 0.5
    } else if (angle < 180) {
        h <- 1
        v <- 0.5
    } else if (angle < 225) {
        h <- 0
        v <- 0
    } else if (angle < 270) {
        h <- 0
        v <- 0
    } else if (angle < 315) {
        h <- 0
        v <- 0.5
    } else if (angle < 360) {
        h <- 0
        v <- 1
    } else {
        h <- 0.5
        v <- 1
    }
    list(h = h, v = v)
}


#' Facetting a plot
#'
#' @param plot The plot to facet or a list list(plot, height, width) if guess_size is TRUE
#' @param facet_by The column(s) to split data by and plot separately or facet by
#'   If NULL, no faceting will be done
#' @param facet_scales Whether to scale the axes of facets.
#' @param nrow The number of rows in facet_wrap
#' @param ncol The number of columns in facet_wrap
#' @param byrow Whether to fill the plots by row
#' @param legend.position The position of the legend
#' @param legend.direction The direction of the legend
#' @param recalc_size Whether to re-calculate the size of the plot
#' @param ... Additional arguments to pass to facet_wrap or facet_grid
#' @return The faceted plot. If guess_size is TRUE, attr(p, "height") and attr(p, "width") will be set
#' @importFrom rlang sym
#' @importFrom ggplot2 facet_wrap facet_grid ggplot_build vars
#' @keywords internal
facet_plot <- function(plot, facet_by, facet_scales, nrow, ncol, byrow,
    legend.position = "right", legend.direction = "vertical", recalc_size = TRUE, ...) {
    if (is.null(facet_by)) {
        return(plot)
    }

    if (recalc_size) {
        p <- facet_plot(plot, facet_by, facet_scales, nrow, ncol, byrow, recalc_size = FALSE, ...)
        d <- wrap_dims(length(unique(ggplot_build(p)$data[[1]]$PANEL)))
        height <- d[1] * attr(plot, "height")
        width <- d[2] * attr(plot, "width")
        if (!identical(legend.position, "none")) {
            # The legends are merged
            if (legend.position %in% c("left", "right")) {
                portion <- ifelse(legend.direction == "vertical", 0.2, 0.3)
                width <- width - (d[2] - 1) * (width / d[2] * portion)
            } else {
                portion <- ifelse(legend.direction == "vertical", 0.3, 0.2)
                height <- height - (d[1] - 1) * (height / d[1] * portion)
            }
        }
        attr(p, "height") <- height
        attr(p, "width") <- width
        return(p)
    }

    if (length(facet_by) == 1) {
        plot <- plot + ggplot2::facet_wrap(facets = facet_by, scales = facet_scales, nrow = nrow, ncol = ncol, dir = if (byrow) "h" else "v", ...)
    } else {
        plot <- plot + ggplot2::facet_grid(vars(!!sym(facet_by[[1]])), vars(!!sym(facet_by[[2]])), scales = facet_scales, ...)
    }

    return(plot)
}


#' Combine plots into one
#'
#' @keywords internal
#' @param plots A list of plots
#' @param combine Whether to combine the plots into one
#' @param nrow The number of rows in the combined plot
#' @param ncol The number of columns in the combined plot
#' @param byrow Whether to fill the plots by row
#' @param recalc_size Whether to re-calculate the size of the combined plot
#' @return The faceted plot. If guess_size is TRUE, attr(p, "height") and attr(p, "width") will be set
#' @importFrom patchwork wrap_plots
#' @importFrom rlang %||%
#' @importFrom ggplot2 wrap_dims
combine_plots <- function(plots, combine, nrow, ncol, byrow, recalc_size = TRUE) {
    if (isFALSE(combine)) {
        return(plots)
    }

    if (recalc_size) {
        d <- wrap_dims(length(plots), nrow, ncol)
        nrow <- d[1]
        ncol <- d[2]
        p <- combine_plots(plots, TRUE, nrow, ncol, byrow, recalc_size = FALSE)
        attr(p, "height") <- nrow * max(sapply(plots, function(x) attr(x, "height")))
        attr(p, "width") <- ncol * max(sapply(plots, function(x) attr(x, "width")))
        return(p)
    }
    # When it's gTree, also run wrap_plots to convert it to a patchwork object
    if (length(plots) == 1 && !inherits(plots[[1]], "gTree")) {
        return(plots[[1]])
    }

    wrap_plots(plots, nrow = nrow, ncol = ncol, byrow = byrow)
}


#' Get a ggplot layer for background
#' @keywords internal
#' @param data A data frame
#' @param x A character string specifying the column name of the data frame to plot for the x-axis
#' @param palette A character string specifying the palette to use
#' @param palcolor A character string specifying the color to use in the palette
#' @param alpha A numeric value specifying the transparency of the plot
#' @param keep_empty A logical value indicating whether to keep empty groups
#' @param facet_by A character string specifying the column name(s) of the data frame to facet the plot
#' @param direction A character string specifying the direction for the background
#' @return A ggplot layer for background
#' @importFrom ggplot2 geom_rect
#' @importFrom dplyr distinct
#' @importFrom tidyr expand_grid
bg_layer <- function(data, x, palette, palcolor, alpha, keep_empty, facet_by, direction = "vertical") {
    fct <- data[[x]]
    if (isFALSE(keep_empty)) {
        fct <- droplevels(fct)
    }
    bg_color <- palette_this(levels(fct), palette = palette, palcolor = palcolor)

    bg_data <- data.frame(x = factor(levels(fct), levels = levels(fct)))
    bg_data$x <- as.numeric(bg_data$x)
    bg_data$xmin <- ifelse(bg_data$x == min(bg_data$x), -Inf, bg_data$x - 0.5)
    bg_data$xmax <- ifelse(bg_data$x == max(bg_data$x), Inf, bg_data$x + 0.5)
    bg_data$ymin <- -Inf
    bg_data$ymax <- Inf
    bg_data$fill <- bg_color[levels(fct)]

    if (!is.null(facet_by)) {
        unique_facet_values <- distinct(data, !!!syms(facet_by))
        bg_data <- expand_grid(bg_data, unique_facet_values)
        for (fb in facet_by) {
            bg_data[[fb]] <- factor(bg_data[[fb]], levels = levels(data[[fb]]))
        }
    }

    if (direction == "vertical") {
        geom_rect(
            data = bg_data,
            aes(xmin = !!sym("xmin"), xmax = !!sym("xmax"), ymin = !!sym("ymin"), ymax = !!sym("ymax")),
            fill = bg_data$fill, alpha = alpha, inherit.aes = FALSE
        )
    } else {
        geom_rect(
            data = bg_data,
            aes(xmin = !!sym("ymin"), xmax = !!sym("ymax"), ymin = !!sym("xmin"), ymax = !!sym("xmax")),
            fill = bg_data$fill, alpha = alpha, inherit.aes = FALSE
        )
    }
}

#' Convert RGBA to RGB
#' @keywords internal
rgba_to_rgb <- function(RGBA, BackGround = c(1, 1, 1)) {
    A <- RGBA[[length(RGBA)]]
    RGB <- RGBA[[-length(RGBA)]] * A + BackGround * (1 - A)
    return(RGB)
}

#' Blend two colors
#' @keywords internal
blend_to_color <- function(C1, C2, mode = "blend") {
    c1 <- C1[[1]]
    c1a <- C1[[2]]
    c2 <- C2[[1]]
    c2a <- C2[[2]]
    A <- 1 - (1 - c1a) * (1 - c2a)
    if (A < 1.0e-6) {
        return(list(c(0, 0, 0), 1))
    }
    if (mode == "blend") {
        out <- (c1 * c1a + c2 * c2a * (1 - c1a)) / A
        A <- 1
    }
    if (mode == "average") {
        out <- (c1 + c2) / 2
        out[out > 1] <- 1
    }
    if (mode == "screen") {
        out <- 1 - (1 - c1) * (1 - c2)
    }
    if (mode == "multiply") {
        out <- c1 * c2
    }
    return(list(out, A))
}

#' Blend a list of colors
#' @keywords internal
blend_rgblist <- function(Clist, mode = "blend", RGB_BackGround = c(1, 1, 1)) {
    N <- length(Clist)
    ClistUse <- Clist
    while (N != 1) {
        temp <- ClistUse
        ClistUse <- list()
        for (C in temp[1:(length(temp) - 1)]) {
            c1 <- C[[1]]
            a1 <- C[[2]]
            c2 <- temp[[length(temp)]][[1]]
            a2 <- temp[[length(temp)]][[2]]
            ClistUse <- append(ClistUse, list(blend_to_color(C1 = list(c1, a1 * (1 - 1 / N)), C2 = list(c2, a2 * 1 / N), mode = mode)))
        }
        N <- length(ClistUse)
    }
    Result <- list(ClistUse[[1]][[1]], ClistUse[[1]][[2]])
    Result <- rgba_to_rgb(Result, BackGround = RGB_BackGround)
    return(Result)
}

#' Blend colors
#'
#' This function blends a list of colors using the specified blend mode.
#'
#' @param colors Color vectors.
#' @param mode Blend mode. One of "blend", "average", "screen", or "multiply".
#'
#' @keywords internal
#' @return The blended color.
#' @importFrom grDevices col2rgb
blend_colors <- function(colors, mode = c("blend", "average", "screen", "multiply")) {
    mode <- match.arg(mode)
    colors <- colors[!is.na(colors)]
    if (length(colors) == 0) {
        return(NA)
    }
    if (length(colors) == 1) {
        return(colors)
    }
    rgb <- as.list(as.data.frame(col2rgb(colors) / 255))
    Clist <- lapply(rgb, function(x) {
        list(x, 1)
    })
    blend_color <- blend_rgblist(Clist, mode = mode)
    blend_color <- grDevices::rgb(blend_color[1], blend_color[2], blend_color[3])
    return(blend_color)
}

#' @importFrom grid is.grob grobWidth grobHeight
#' @importFrom gtable is.gtable gtable_add_rows gtable_add_cols gtable_add_grob
add_grob <- function(gtable, grob, position = c("top", "bottom", "left", "right", "none"), space = NULL, clip = "on") {
    position <- match.arg(position)
    if (position == "none" || is.null(grob)) {
        return(gtable)
    }

    if (is.null(space)) {
        if (is.gtable(grob)) {
            if (position %in% c("top", "bottom")) {
                space <- sum(grob$heights)
            } else {
                space <- sum(grob$widths)
            }
        } else if (is.grob(grob)) {
            if (position %in% c("top", "bottom")) {
                space <- grobHeight(grob)
            } else {
                space <- grobWidth(grob)
            }
        }
    }

    if (position == "top") {
        gtable <- gtable_add_rows(gtable, space, 0)
        gtable <- gtable_add_grob(gtable, grob, t = 1, l = mean(gtable$layout[grepl(pattern = "panel", x = gtable$layout$name), "l"]), clip = clip)
    }
    if (position == "bottom") {
        gtable <- gtable_add_rows(gtable, space, -1)
        gtable <- gtable_add_grob(gtable, grob, t = dim(gtable)[1], l = mean(gtable$layout[grepl(pattern = "panel", x = gtable$layout$name), "l"]), clip = clip)
    }
    if (position == "left") {
        gtable <- gtable_add_cols(gtable, space, 0)
        gtable <- gtable_add_grob(gtable, grob, t = mean(gtable$layout[grep("panel", gtable$layout$name), "t"]), l = 1, clip = clip)
    }
    if (position == "right") {
        gtable <- gtable_add_cols(gtable, space, -1)
        gtable <- gtable_add_grob(gtable, grob, t = mean(gtable$layout[grep("panel", gtable$layout$name), "t"]), l = dim(gtable)[2], clip = clip)
    }
    return(gtable)
}

#' Convert a color with arbitrary transparency to a fixed color
#'
#' This function takes a vector of colors and an alpha level and converts the colors
#' to fixed colors with the specified alpha level.
#'
#' @param colors Color vectors.
#' @param alpha Alpha level ranging from 0 to 1.
#' @return The colors with the specified alpha level.
#' @keywords internal
#' @importFrom grDevices col2rgb rgb
adjcolors <- function(colors, alpha) {
    has_names <- !is.null(names(colors))
    color_df <- as.data.frame(col2rgb(colors) / 255)
    colors_out <- sapply(color_df, function(color) {
        color_rgb <- rgba_to_rgb(list(color, alpha))
        return(rgb(color_rgb[1], color_rgb[2], color_rgb[3]))
    })
    if (has_names) {
        names(colors_out) <- names(colors)
    } else {
        names(colors_out) <- NULL
    }
    return(colors_out)
}

#' Process theme to allow 'ggplot2::theme_minimal' to work
#'
#' @param theme The theme to process
#' @return The processed theme
#' @keywords internal
#' @importFrom utils getFromNamespace
process_theme <- function(theme) {
    if (is.function(theme)) {
        return(theme)
    }

    if (!is.character(theme)) {
        stop("The theme must be a character string or a theme function.")
    }

    if (!grepl("::", theme)) {
        return(theme)
    }

    parts <- unlist(strsplit(theme, "::"))
    if (length(parts) != 2) {
        stop("Invalid theme '", theme, "'")
    }

    return(getFromNamespace(parts[2], parts[1]))
}


#' check_palette
#' Check if the palette can be properly used
#' @param palette palette
#' @param datas_name names of the split data
#' @keywords internal
#' @return named list containing palette names
check_palette <- function(palette, datas_name) {
    palette <- as.list(palette)
    stopifnot("'palette' must be specified" = length(palette) > 0)
    if (length(palette) == 1 && length(datas_name) > 1) {
        palette <- rep(palette, length(datas_name))
    }
    if (length(palette) < length(datas_name)) {
        stop("The length of 'palette' (", length(palette), ") is less than the number ",
            "(", length(datas_name), ") of unique values in 'split_by'")
    }
    if (is.null(names(palette))) {
        names(palette)[1:length(datas_name)] <- datas_name
    } else if (length(setdiff(datas_name, names(palette))) > 0) {
        stop("Values in 'split_by' (",
            paste(setdiff(datas_name, names(palette)), collapse = ", "), ") ",
            "have no corresponding palette assigned in 'palette'")
    }
    return(palette)
}


#' check_palcolor
#' Check if the palcolor can be properly used
#' @param palcolor palcolor
#' @param datas_name names of the split data
#' @keywords internal
#' @return named list containing color names
check_palcolor <- function(palcolor, datas_name) {
    if (is.null(palcolor)) { return(NULL) }
    # as.list() will turn c("red", "blue") into list("red", "blue")
    # but we need list(c("red", "blue"))
    if (!is.list(palcolor)) { palcolor <- list(palcolor) }
    if (length(palcolor) == 1 && length(datas_name) > 1) {
        palcolor <- rep(palcolor, length(datas_name))
    }
    if (is.null(names(palcolor))) {
        names(palcolor)[1:length(datas_name)] <- datas_name
    }
    # It's okay that some split_by values have no corresponding palcolor
    # because they can be NULL
    return(palcolor)
}
