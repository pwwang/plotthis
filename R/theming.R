#' A ggplot2 theme and palettes for plotthis
#' Borrowed from the `theme_this` function in the `SCP` pipeline
#'
#' @seealso \url{https://github.com/zhanghao-njmu/SCP}
#'
#' @param aspect.ratio The aspect ratio of the plot
#' @param base_size The base size of the text
#' If not specified, it will use the value from `getOption("theme_this.base_size", 12)`.
#' If you want to change the default base size, you can set the option `theme_this.base_size`.
#' This is applied to all plots using this theme.
#' @param font_family The font family of the text
#' If not specified, it will use the value from `getOption("theme_this.font_family")`.
#' If you want to change the default font family, you can set the option `theme_this.font_family`.
#' This is applied to all plots using this theme.
#' You can choose from the built-in fonts in `plotthis`, system fonts, or Google fonts.
#' See \code{\link{list_fonts}()} for available fonts.
#' @param ... Other arguments for `theme()`
#'
#' @return A ggplot2 theme
#' @importFrom methods formalArgs
#' @importFrom ggplot2 element_text element_rect margin theme element_blank unit
#' @export
theme_this <- function(aspect.ratio = NULL, base_size = NULL, font_family = NULL, ...) {
    base_size <- base_size %||% getOption("theme_this.base_size", 12)
    font_family <- font_family %||% getOption("theme_this.font_family")
    text_size_scale <- base_size / 12
    import_font(font_family)
    args1 <- list(
        aspect.ratio = aspect.ratio,
        text = element_text(size = 12 * text_size_scale, family = font_family, color = "black"),
        plot.title = element_text(size = 14 * text_size_scale, family = font_family, colour = "black", vjust = 1),
        plot.subtitle = element_text(size = 13 * text_size_scale, family = font_family, hjust = 0, margin = margin(b = 3)),
        plot.background = element_rect(fill = "white", color = "white"),
        plot.margin = margin(10, 10, 10, 10),
        axis.line = element_blank(),
        axis.title = element_text(size = 13 * text_size_scale, family = font_family, colour = "black"),
        axis.text = element_text(size = 12 * text_size_scale, family = font_family, colour = "black"),
        strip.text = element_text(size = 12.5 * text_size_scale, family = font_family, colour = "black", hjust = 0.5, margin = margin(3, 3, 3, 3)),
        strip.background = element_rect(fill = "transparent", linetype = 0),
        strip.switch.pad.grid = unit(-1, "pt"),
        strip.switch.pad.wrap = unit(-1, "pt"),
        strip.placement = "outside",
        legend.title = element_text(size = 12 * text_size_scale, family = font_family, colour = "black", hjust = 0),
        legend.text = element_text(size = 11 * text_size_scale, family = font_family, colour = "black"),
        legend.key = element_rect(fill = "transparent", color = "transparent"),
        legend.key.size = unit(10, "pt"),
        legend.background = element_blank(),
        panel.background = element_rect(fill = "white", color = "white"),
        panel.border = element_rect(fill = "transparent", colour = "black", linewidth = 1)
    )
    args2 <- as.list(match.call())[-1]
    call.envir <- parent.frame(1)
    args2 <- lapply(args2, function(arg) {
        if (is.symbol(arg)) {
            eval(arg, envir = call.envir)
        } else if (is.call(arg)) {
            eval(arg, envir = call.envir)
        } else {
            arg
        }
    })
    for (n in names(args2)) {
        args1[[n]] <- args2[[n]]
    }
    args <- args1[names(args1) %in% formalArgs(theme)]
    do.call(what = theme, args = args)
}

#' Blank theme
#'
#' This function creates a theme with all elements blank except for axis lines and labels.
#' It can optionally add coordinate axes in the plot.
#'
#' @param add_coord Whether to add coordinate arrows. Default is \code{TRUE}.
#' @param xlen_npc The length of the x-axis arrow in "npc".
#' @param ylen_npc The length of the y-axis arrow in "npc".
#' @param xlab x-axis label.
#' @param ylab y-axis label.
#' @param lab_size Label size.
#' @param ... Arguments passed to the \code{\link[ggplot2]{theme}}.
#' @return A ggplot2 theme.
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(x = wt, y = mpg, colour = factor(cyl))) +
#'     geom_point()
#' p + theme_blank()
#' p + theme_blank(xlab = "x-axis", ylab = "y-axis", lab_size = 16)
#' @importFrom methods formalArgs
#' @importFrom ggplot2 theme element_blank margin annotation_custom coord_cartesian
#' @importFrom grid grobTree gList linesGrob textGrob arrow gpar
#' @export
theme_blank <- function(add_coord = TRUE, xlen_npc = 0.15, ylen_npc = 0.15, xlab = "", ylab = "", lab_size = 12, ...) {
    args1 <- list(
        panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.background = element_blank(),
        legend.box.margin = margin(0, 0, 0, 0),
        legend.margin = margin(0, 0, 0, 0),
        legend.key.size = unit(10, "pt"),
        plot.margin = margin(lab_size + 2, lab_size + 2, lab_size + 2, lab_size + 2, unit = "points")
    )
    args2 <- as.list(match.call())[-1]
    call.envir <- parent.frame(1)
    args2 <- lapply(args2, function(arg) {
        if (is.symbol(arg)) {
            eval(arg, envir = call.envir)
        } else if (is.call(arg)) {
            eval(arg, envir = call.envir)
        } else {
            arg
        }
    })
    for (n in names(args2)) {
        args1[[n]] <- args2[[n]]
    }
    args <- args1[names(args1) %in% formalArgs(theme)]
    out <- do.call(
        what = theme,
        args = args
    )
    if (isTRUE(add_coord)) {
        g <- grobTree(gList(
            linesGrob(x = unit(c(0, xlen_npc), "npc"), y = unit(c(0, 0), "npc"), arrow = arrow(length = unit(0.02, "npc")), gp = gpar(lwd = 2)),
            textGrob(label = xlab, x = unit(0, "npc"), y = unit(0, "npc"), vjust = 4 / 3, hjust = 0, gp = gpar(fontsize = lab_size)),
            linesGrob(x = unit(c(0, 0), "npc"), y = unit(c(0, ylen_npc), "npc"), arrow = arrow(length = unit(0.02, "npc")), gp = gpar(lwd = 2)),
            textGrob(label = ylab, x = unit(0, "npc"), y = unit(0, "npc"), vjust = -2 / 3, hjust = 0, rot = 90, gp = gpar(fontsize = lab_size))
        ))
        return(list(
            list(annotation_custom(g)),
            list(theme_this() + out),
            list(coord_cartesian(clip = "off"))
        ))
    } else {
        return(list(
            list(theme_this() + out)
        ))
    }
}

#' Box theme
#'
#' This function creates a theme with all elements blank except for axis lines
#' like a box around the plot.
#'
#' @param xlen_npc The length of the x-axis arrow in "npc".
#' @param ylen_npc The length of the y-axis arrow in "npc".
#' @param xlab x-axis label.
#' @param ylab y-axis label.
#' @param lab_size Label size.
#' @param ... Arguments passed to the \code{\link[ggplot2]{theme}}.
#' @return A ggplot2 theme.
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(x = wt, y = mpg, colour = factor(cyl))) +
#'     geom_point()
#' p + theme_box()
#' @importFrom methods formalArgs
#' @importFrom ggplot2 theme element_blank margin annotation_custom coord_cartesian
#' @importFrom grid grobTree gList linesGrob textGrob arrow gpar
#' @export
theme_box <- function(xlen_npc = 0.15, ylen_npc = 0.15, xlab = "", ylab = "", lab_size = 12, ...) {
    args1 <- list(
        panel.border = element_rect(fill = "transparent", colour = "black", linewidth = 1),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.background = element_blank(),
        legend.box.margin = margin(0, 0, 0, 0),
        legend.margin = margin(0, 0, 0, 0),
        legend.key.size = unit(10, "pt"),
        plot.margin = margin(lab_size + 2, lab_size + 2, lab_size + 2, lab_size + 2, unit = "points")
    )
    args2 <- as.list(match.call())[-1]
    call.envir <- parent.frame(1)
    args2 <- lapply(args2, function(arg) {
        if (is.symbol(arg)) {
            eval(arg, envir = call.envir)
        } else if (is.call(arg)) {
            eval(arg, envir = call.envir)
        } else {
            arg
        }
    })
    for (n in names(args2)) {
        args1[[n]] <- args2[[n]]
    }
    args <- args1[names(args1) %in% formalArgs(theme)]
    out <- do.call(
        what = theme,
        args = args
    )

    return(list(
        list(theme_this() + out)
    ))
}

#' Color palettes collected in plotthis.
#'
#' @param x A vector of character/factor or numeric values. If missing, numeric values 1:n will be used as x.
#' @param n The number of colors to return for numeric values.
#' @param palette Palette name. All available palette names can be queried with \code{show_palettes()}.
#' @param palcolor Custom colors used to create a color palette.
#' @param type Type of \code{x}. Can be one of "auto", "discrete" or "continuous". The default is "auto", which automatically detects if \code{x} is a numeric value.
#' @param keep_names Whether to keep the names of the color vector.
#' @param alpha The alpha value of the colors. Default is 1.
#' @param matched If \code{TRUE}, will return a color vector of the same length as \code{x}.
#' @param reverse Whether to invert the colors.
#' @param NA_keep Whether to keep the color assignment to NA in \code{x}.
#' @param NA_color Color assigned to NA if NA_keep is \code{TRUE}.
#' @param transparent Whether to make the colors transparent when alpha < 1.
#'  When `TRUE`, [ggplot2::alpha()] is used to make the colors transparent.
#'  Otherwise, `adjcolors` is used to adjust the colors based on the alpha. The color will be not be actually transparent.
#'  For example, `ggplot2::alpha("red", 0.5) == "#FF000080"`; while `adjcolors("red", 0.5) == "#FF8080"`.
#' @return A vector of colors.
#' @importFrom grDevices colorRampPalette
#' @importFrom stats setNames
#' @export
#'
palette_this <- function(
    x, n = 100, palette = "Paired", palcolor = NULL, type = "auto", keep_names = TRUE, alpha = 1,
    matched = FALSE, reverse = FALSE, NA_keep = FALSE, NA_color = "grey80", transparent = TRUE) {
    palette_list <- plotthis::palette_list
    if (missing(x)) {
        x <- 1:n
        type <- "continuous"
    }
    if (!palette %in% names(palette_list)) {
        stop("The palette name (", palette, ") is invalid! You can check the available palette names with 'show_palettes()'. Or pass palette colors via the 'palcolor' parameter.")
    }
    if (is.list(palcolor)) {
        palcolor <- unlist(palcolor)
    }
    if (all(palcolor == "")) {
        palcolor <- palette_list[[palette]]
    }
    if (is.null(palcolor) || length(palcolor) == 0) {
        palcolor <- palette_list[[palette]]
    }
    if (!is.null(names(palcolor))) {
        mypal <- palcolor[intersect(names(palcolor), x)]
        # palcolor partially matches x
        if (length(mypal) < length(x) && length(mypal) > 0) {
            palcolor <- palette_this(
                x = x,
                n = n,
                palette = palette,
                palcolor = NULL,
                type = type,
                keep_names = TRUE,
                alpha = 1,
                matched = matched,
                reverse = reverse,
                NA_keep = NA_keep,
                NA_color = NA_color,
                transparent = transparent
            )
            palcolor[names(mypal)] <- mypal
            # already reversed, specified palcolor won't be reversed
            reverse <- FALSE
        } else if (length(mypal) == length(x)) {
            palcolor <- mypal
        }
    }
    pal_n <- length(palcolor)

    if (!type %in% c("auto", "discrete", "continuous")) {
        stop("'type' must be one of 'auto','discrete' and 'continuous'.")
    }
    if (type == "auto") {
        if (is.numeric(x)) {
            type <- "continuous"
        } else {
            type <- "discrete"
        }
    }

    if (type == "discrete") {
        if (!is.factor(x)) {
            x <- factor(x, levels = unique(x))
        }
        n_x <- nlevels(x)
        if (isTRUE(attr(palcolor, "type") == "continuous")) {
            color <- colorRampPalette(palcolor)(n_x)
            names(color) <- levels(x)
        } else if (!is.null(names(palcolor))) {
            color <- palcolor[intersect(names(palcolor), levels(x))]
        } else {
            color <- ifelse(rep(n_x, n_x) <= pal_n,
                palcolor[1:n_x],
                colorRampPalette(palcolor)(n_x)
            )
            names(color) <- levels(x)
        }

        if (any(is.na(x))) {
            color <- c(color, setNames(NA_color, "NA"))
        }
        if (isTRUE(matched)) {
            color <- color[x]
            color[is.na(color)] <- NA_color
        }
    } else if (type == "continuous") {
        if (!is.numeric(x) && all(!is.na(x))) {
            stop("'x' must be type of numeric when use continuous color palettes.")
        }
        if (all(is.na(x))) {
            values <- as.factor(rep(0, n))
        } else if (length(unique(na.omit(as.numeric(x)))) == 1) {
            values <- as.factor(rep(unique(na.omit(as.numeric(x))), n))
        } else {
            if (isTRUE(matched)) {
                values <- cut(x, breaks = seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), length.out = n + 1), include.lowest = TRUE)
            } else {
                values <- cut(1:100, breaks = seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), length.out = n + 1), include.lowest = TRUE)
            }
        }

        n_x <- nlevels(values)
        color <- ifelse(rep(n_x, n_x) <= pal_n,
            palcolor[1:n_x],
            colorRampPalette(palcolor)(n_x)
        )
        names(color) <- levels(values)
        if (any(is.na(x))) {
            color <- c(color, setNames(NA_color, "NA"))
        }
        if (isTRUE(matched)) {
            if (all(is.na(x))) {
                color <- NA_color
            } else if (length(unique(na.omit(x))) == 1) {
                color <- color[as.character(unique(na.omit(x)))]
                color[is.na(color)] <- NA_color
            } else {
                color <- color[as.character(values)]
                color[is.na(color)] <- NA_color
            }
        }
    }

    if (isTRUE(reverse)) {
        if (!is.null(names(color))) {
            color <- setNames(rev(color), names(color))
        } else {
            color <- rev(color)
        }
    }
    if (!isTRUE(NA_keep)) {
        color <- color[names(color) != "NA"]
    }
    if (!isTRUE(keep_names)) {
        names(color) <- NULL
    }
    if (alpha < 1) {
        if (isTRUE(transparent)) {
            color <- ggplot2::alpha(color, alpha)
        } else {
            color <- adjcolors(color, alpha)
        }
    }
    return(color)
}

#' Show the color palettes
#'
#' This function displays color palettes using ggplot2.
#'
#' @param palettes A list of color palettes. If `NULL`, uses default palettes.
#' @param type A character vector specifying the type of palettes to include. Default is "discrete".
#' @param index A numeric vector specifying the indices of the palettes to include. Default is `NULL`.
#' @param palette_names A character vector specifying the names of the SCP palettes to include. Default is `NULL`.
#' @param return_names A logical value indicating whether to return the names of the selected palettes. Default is `TRUE`.
#' @param return_palettes A logical value indicating whether to return the colors of selected palettes. Default is `FALSE`.
#' @return A list of palette names or a list of palettes.
#'
#' @seealso \code{\link{palette_list}}
#' @seealso \href{../articles/all-palettes.html}{All available palettes}
#'
#' @examples
#' show_palettes(palettes = list(c("red", "blue", "green"), c("yellow", "purple", "orange")))
#' all_palettes <- show_palettes(return_palettes = TRUE)
#' names(all_palettes)
#' all_palettes[["simspec"]]
#' show_palettes(index = 1:10)
#' show_palettes(type = "discrete", index = 1:10)
#' show_palettes(type = "continuous", index = 1:10)
#' show_palettes(
#'     palette_names = c("Paired", "nejm", "simspec", "Spectral", "jet"),
#'     return_palettes = TRUE
#' )
#'
#' @importFrom ggplot2 geom_col scale_fill_manual scale_x_continuous element_blank aes element_text margin element_rect unit theme
#' @export
show_palettes <- function(palettes = NULL, type = c("discrete", "continuous"), index = NULL, palette_names = NULL, return_names = TRUE, return_palettes = FALSE) {
    ggplot <- if (getOption("plotthis.gglogger.enabled", FALSE)) {
        gglogger::ggplot
    } else {
        ggplot2::ggplot
    }

    if (!is.null(palettes)) {
        palette_list <- palettes
    } else {
        palette_list <- plotthis::palette_list[unlist(lapply(palette_list, function(x) isTRUE(attr(x, "type") %in% type)))]
    }
    index <- index[index %in% seq_along(palette_list)]
    if (!is.null(index)) {
        palette_list <- palette_list[index]
    }
    if (is.null(names(palette_list))) {
        names(palette_list) <- seq_along(palette_list)
    }
    if (is.null(palette_names)) {
        palette_names <- palette_names %||% names(palette_list)
    }
    if (any(!palette_names %in% names(palette_list))) {
        stop(paste("Can not find the palettes: ", paste0(palette_names[!palette_names %in% names(palette_list)], collapse = ",")))
    }
    palette_list <- palette_list[palette_names]

    df <- data.frame(palette = rep(names(palette_list), sapply(palette_list, length)), color = unlist(palette_list))
    df$palette <- factor(df$palette, levels = rev(unique(df$palette)))
    df$color_order <- factor(seq_len(nrow(df)), levels = seq_len(nrow(df)))
    df$proportion <- as.numeric(1 / table(df$palette)[df$palette])
    p <- ggplot(data = df, aes(y = !!sym("palette"), x = !!sym("proportion"), fill = !!sym("color_order"))) +
        geom_col(show.legend = FALSE) +
        scale_fill_manual(values = df[["color"]]) +
        scale_x_continuous(expand = c(0, 0), trans = "reverse") +
        theme_this(
            axis.title = element_blank(),
            axis.ticks = element_blank(),
            axis.text.x = element_blank(),
            panel.border = element_blank()
        )
    print(p)

    if (isTRUE(return_palettes)) {
        return(palette_list)
    }
    if (isTRUE(return_names)) {
        return(palette_names)
    }
}


#' Theme element that add a box to the text
#'
#' Code grabbed from the `ggtext` package. See the original code at:
#' https://github.com/wilkelab/ggtext
#' This is used to create a text box around the text, primarily to be used in `CorPairsPlot`.
#'
#' @rdname element_textbox
#' @param family Font family
#' @param face Font face
#' @param size Font size (in pt)
#' @param colour,color Text color
#' @param fill Fill color of the enclosing box
#' @param box.colour,box.color Line color of the enclosing box (if different from the text color)
#' @param linetype Line type of the enclosing box (like `lty` in base R)
#' @param linewidth Line width of the enclosing box (measured in mm, just like `size` in
#'   [ggplot2::element_line()]).
#' @param hjust Horizontal justification
#' @param vjust Vertical justification
#' @param halign Horizontal justification
#' @param valign Vertical justification
#' @param lineheight Line height, in multiples of the font size
#' @param width,height Unit objects specifying the width and height
#'   of the textbox, as in [gridtext::textbox_grob()].
#' @param minwidth,minheight,maxwidth,maxheight Min and max values for width and height.
#'   Set to NULL to impose neither a minimum nor a maximum.
#' @param padding,margin Padding and margins around the text box.
#'   See [gridtext::textbox_grob()] for details.
#' @param r Unit value specifying the corner radius of the box
#' @param orientation Orientation of the text box. See [gridtext::textbox_grob()] for details.
#' @param debug Not implemented.
#' @param inherit.blank See [ggplot2::margin()] for details.
#' @return A ggplot2 theme element that can be used inside a [ggplot2::theme()] call.
#' @export
#' @importFrom grid unit
element_textbox <- function(
    family = NULL, face = NULL, size = NULL, colour = NULL,
    fill = NULL, box.colour = NULL, linetype = NULL, linewidth = NULL,
    hjust = NULL, vjust = NULL, halign = NULL, valign = NULL,
    lineheight = NULL, margin = NULL, padding = NULL, width = NULL,
    height = NULL, minwidth = NULL, maxwidth = NULL, minheight = NULL,
    maxheight = NULL, r = NULL, orientation = NULL, color = NULL,
    box.color = NULL, debug = FALSE, inherit.blank = FALSE) {
    if (!is.null(color)) {
        colour <- color
    }

    if (!is.null(box.color)) {
        box.colour <- box.color
    }
    family <- family %||% getOption("theme_this.font_family")
    import_font(family)
    structure(
        list(
            family = family, face = face, size = size, colour = colour, fill = fill, box.colour = box.colour,
            linetype = linetype, linewidth = linewidth,
            hjust = hjust, vjust = vjust, halign = halign, valign = valign, lineheight = lineheight,
            margin = margin, padding = padding, width = width, height = height, minwidth = minwidth,
            maxwidth = maxwidth, minheight = minheight, maxheight = maxheight,
            r = r, orientation = orientation,
            debug = debug, inherit.blank = inherit.blank
        ),
        class = c("element_textbox", "element_text", "element")
    )
}

#' @rdname element_textbox
#' @param element A theme element created by [element_textbox()].
#' @param label Text to display in the textbox.
#' @param x,y Position of the textbox.
#' @param ... Other arguments passed to [gridtext::textbox_grob()].
#' @importFrom grid gpar
#' @importFrom gridtext textbox_grob
#' @importFrom ggplot2 zeroGrob .pt element_grob
#' @export
element_grob.element_textbox <- function(
    element, label = "", x = NULL, y = NULL,
    family = NULL, face = NULL, colour = NULL, size = NULL,
    hjust = NULL, vjust = NULL, lineheight = NULL,
    margin = NULL, ...) {
    if (is.null(label)) {
        return(zeroGrob())
    }

    hj <- hjust %||% element$hjust
    vj <- vjust %||% element$vjust
    halign <- element$halign %||% 0
    valign <- element$valign %||% 1
    padding <- element$padding %||% ggplot2::margin(0, 0, 0, 0)
    margin <- margin %||% element$margin %||% ggplot2::margin(0, 0, 0, 0)
    orientation <- element$orientation %||% "upright"
    r <- element$r %||% unit(0, "pt")
    # The gp settings can override element_gp
    gp <- gpar(
        fontsize = size %||% element$size,
        col = colour %||% element$colour,
        fontfamily = family %||% element$family,
        fontface = face %||% element$face,
        lineheight = lineheight %||% element$lineheight
    )

    box_gp <- gpar(
        col = element$box.colour %||% gp$col,
        fill = element$fill %||% NA,
        lty = element$linetype %||% 0,
        lwd = (element$linewidth %||% 0.5) * .pt
    )

    textbox_grob(
        label,
        x = x, y = y, hjust = hj, vjust = vj, halign = halign, valign = valign,
        width = element$width, height = element$height,
        minwidth = element$minwidth, minheight = element$minheight,
        maxwidth = element$maxwidth, maxheight = element$maxheight,
        margin = margin, padding = padding, r = r,
        orientation = orientation,
        gp = gp, box_gp = box_gp
    )
}


#' @title List fonts
#' @description Lists available fonts.
#' This function lists the fonts available in the `plotthis` package,
#' as well as system fonts and Google fonts if specified.
#' @details
#' This is inspired from the `plummy` package.
#' Those fonts will be automatically imported when used in `theme_this()`, typically via the `theme_args`
#' argument in plotting functions. For example, `BarPlot(..., theme_args = list(font_family = "Barlow"))`.
#' You can also use `import_font("Barlow")` to import the font manually before using it in plots separately.
#' For example, `BarPlot(...) + theme(text = element_text(family = "Barlow"))`.
#' See \code{\link{import_font}()} for more details on importing fonts.
#' @param family_only Only list the font family names.
#' @param source Source of the fonts to include. Can be one of "builtin", "system", "google", or "all".
#' Default is "all".
#' @return A list of font family names, where the names are the source of the fonts
#' (system or google) if `family_only` is `TRUE`. If `family_only` is `FALSE`,
#' returns a data frame with font family names and their paths.#'
#' @export
#' @examples
#' \donttest{
#' list_fonts(family_only = TRUE, source = "all")
#' list_fonts(family_only = FALSE, source = "builtin")
#' list_fonts(family_only = FALSE, source = "system")
#' list_fonts(family_only = FALSE, source = "google")
#' }
list_fonts <- function(family_only = FALSE, source = c("all", "builtin", "system", "google")) {
    source <- match.arg(source)

    dfr_pkg <- NULL
    if (source %in% c("builtin", "all")) {
        fn_rd <- function(font = NULL) {
            fn_wgt <- function(x) {
                wgts <- c("Thin" = 100, "ExtraLight" = 200, "Light" = 300, "Regular" = 400, "Italic" = 400, "Medium" = 500, "SemiBold" = 600, "Bold" = 700, "ExtraBold" = 800, "Black" = 900)
                return(wgts[grepl(paste0("^", x, "$"), names(wgts))])
            }

            paths <- list.files(system.file("fonts", font, package = "plotthis"), pattern = ".ttf", full.names = TRUE)
            bnames <- basename(paths)
            fam <- sapply(strsplit(bnames, split = "-"), "[[", 1)
            fnt <- sub(".ttf", "", sapply(strsplit(bnames, split = "-"), "[[", 2))
            wt <- as.integer(unlist(sapply(sub("Italic", "", sub("^Italic$", "Regular", fnt)), fn_wgt)))

            dfr <- data.frame(
                name = paste0(tolower(fam), "-", wt, "-", tolower(fnt)),
                family = fam,
                font = fnt,
                weight = wt,
                path = paths
            )

            return(dfr)
        }

        dfr_pkg <- Reduce(
            rbind,
            lapply(
                list.files(path = system.file("fonts", package = "plotthis")),
                fn_rd
            )
        )
        dfr_pkg$source <- "builtin"
    }

    dfr_sys <- NULL
    if (source %in% c("system", "all")) {
        ff <- tryCatch(sysfonts::font_files(), error = function(e) NULL)
        if (!is.null(ff) && is.data.frame(ff) && nrow(ff) > 0) {
            # Exclude font family with no Regular face
            has_regular <- ff$family[ff$face %in% c("Regular", "normal", NA)]
            ff <- ff[ff$family %in% has_regular, , drop = FALSE]
            dfr_sys <- data.frame(
                name = tolower(tools::file_path_sans_ext(ff$file)),
                family = ff$family,
                font = ff$face,
                weight = {
                    w <- rep(400L, length(ff$face))
                    ff_face <- ifelse(is.na(ff$face), "", ff$face)
                    w[grepl("light", ff_face, ignore.case = TRUE)] <- 300L
                    w[grepl("demi|semibold", ff_face, ignore.case = TRUE)] <- 600L
                    w[grepl("bold", ff_face, ignore.case = TRUE)] <- 700L
                    w
                },
                path = file.path(ff$path, ff$file),
                source = "system",
                stringsAsFactors = FALSE
            )
        }
    }

    dfr_google <- NULL
    if (source %in% c("google", "all")) {
        fams_g <- tryCatch(sysfonts::font_families_google(), error = function(e) NULL)
        if (!is.null(fams_g) && length(fams_g) > 0) {
            dfr_google <- data.frame(
                name = tolower(fams_g),
                family = fams_g,
                font = fams_g,
                weight = 400L,
                path = NA_character_,
                source = "google",
                stringsAsFactors = FALSE
            )
        }
    }

    if (isTRUE(family_only)) {
        font_list <- list()
        if (!is.null(dfr_pkg)) {
            font_list$builtin <- unique(dfr_pkg$family)
        }
        if (!is.null(dfr_sys)) {
            font_list$system <- unique(dfr_sys$family)
        }
        if (!is.null(dfr_google)) {
            font_list$google <- unique(dfr_google$family)
        }
        return(font_list)
    } else {
        dfr_all <- do.call(rbind, list(dfr_pkg, dfr_sys, dfr_google))
        rownames(dfr_all) <- NULL
        return(dfr_all)
    }
}


#' @title Import a font
#' @description Imports a font for use in plots.
#' This function imports a font from the `plotthis` package,
#' system fonts, or Google fonts.
#' This is borrowed from the `plummy` package.
#' To check available fonts, use `list_fonts()`.
#' @param font Font family name to import.
#' @return Invisible `TRUE`.
#' @export
import_font <- function(font) {
    # check if font is already added
    if (is.null(font) || font %in% sysfonts::font_families()) {
        return(invisible(TRUE))
    }
    builtin_fonts <- list_fonts(family_only = TRUE, source = "builtin")
    if (font %in% builtin_fonts$builtin) {
        font_dir <- system.file("fonts", font, package = "plotthis")
        sysfonts::font_add(
            family = font,
            regular = file.path(font_dir, paste0(font, "-Regular.ttf")),
            bold = file.path(font_dir, paste0(font, "-Bold.ttf")),
            italic = file.path(font_dir, paste0(font, "-Italic.ttf")),
            bolditalic = file.path(font_dir, paste0(font, "-BoldItalic.ttf"))
        )
    } else if (font %in% sysfonts::font_families_google()) {
        sysfonts::font_add_google(font)
    } else {
        fonts <- list_fonts(family_only = FALSE, source = "system")
        if (font %in% fonts$family) {
            font_df <- fonts[fonts$family == font, , drop = FALSE]
            font_df$font <- tolower(font_df$font)
            regular_ttf <- font_df$path[grepl("regular", font_df$font)][1]
            bolditalic_ttf <- font_df$path[grepl("bolditalic", font_df$font)]
            if (length(bolditalic_ttf) == 0) {
                bolditalic_ttf <- NULL
            } else {
                bolditalic_ttf <- bolditalic_ttf[1]
            }
            italic_ttf <- font_df$path[grepl("italic", font_df$font)]
            if (length(italic_ttf) == 0) {
                italic_ttf <- NULL
            } else {
                italic_ttf <- italic_ttf[italic_ttf != bolditalic_ttf]
                if (length(italic_ttf) == 0) {
                    italic_ttf <- NULL
                } else {
                    italic_ttf <- italic_ttf[1]
                }
            }
            bold_ttf <- font_df$path[grepl("bold", font_df$font)]
            if (length(bold_ttf) == 0) {
                bold_ttf <- NULL
            } else {
                bold_ttf <- bold_ttf[bold_ttf != bolditalic_ttf]
                if (length(bold_ttf) == 0) {
                    bold_ttf <- NULL
                } else {
                    bold_ttf <- bold_ttf[1]
                }
            }

            sysfonts::font_add(
                family = font,
                regular = regular_ttf,
                bold = bold_ttf,
                italic = italic_ttf,
                bolditalic = bolditalic_ttf
            )
            return(invisible(TRUE))
        } else {
            warning("Font '", font, "' not found in builtin fonts, system fonts, or Google fonts.")
        }
    }
    invisible(TRUE)
}
