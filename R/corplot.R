#' Atomic Correlation Plot
#'
#' @description Generate scatter correlation plot for two variables.
#' @inheritParams common_args
#' @param x, y The column names of the data to be plotted.
#' @param group_by The column name of the data to be used for grouping.
#'  Different groups will be plotted in different colors.
#' @param group_by_sep The separator used to concatenate multiple columns in `group_by`.
#' @param group_name The name of the group in the legend.
#' @param pt_size The size of the points.
#' @param pt_shape The shape of the points.
#' @param raster Whether to use raster graphics for plotting.
#' @param raster_dpi The DPI of the raster graphics.
#' @param highlight The items to be highlighted.
#'  Could be either a vector of rownames if data has rownames, or a vector of indices, or
#'  An expression that can be evaluated by \code{dplyr::filter} to get the highlighted items.
#' @param highlight_color The color of the highlighted points.
#' @param highlight_size The size of the highlighted points.
#' @param highlight_alpha The alpha of the highlighted points.
#' @param highlight_stroke The stroke of the highlighted points.
#' @param anno_items The items to be annotated on the plot.
#'  Available items: "eq", "r2", "p", "spearman", "pearson", "kendall", "n".
#' @param anno_size The size of the annotation text.
#' @param anno_fg The color of the annotation text.
#' @param anno_bg The background color of the annotation text.
#' @param anno_bg_r The radius of the background of the annotation text.
#' @param anno_position The position of the annotation text.
#'  Available positions: "topleft", "topright", "bottomleft", "bottomright".
#'  Shortcuts: "tl", "tr", "bl", "br".
#' @param add_smooth Whether to add a linear regression line.
#' @param smooth_color The color of the regression line.
#' @param smooth_width The width of the regression line.
#' @param smooth_se Whether to add the standard error band to the regression line.
#' @return A ggplot object.
#' @keywords internal
#' @importFrom rlang syms sym
#' @importFrom dplyr group_modify filter
#' @importFrom ggplot2 geom_point geom_smooth geom_text scale_color_manual labs
CorPlotAtomic <- function(
    data, x, y, group_by = NULL, group_by_sep = "_", group_name = NULL,
    pt_size = 2, pt_shape = 16, alpha = 1, raster = FALSE, raster_dpi = c(512, 512),
    highlight = NULL, highlight_color = "black", highlight_size = 1, highlight_alpha = 1, highlight_stroke = 0.8,
    anno_items = c("eq", "r2", "p"), anno_size = 3, anno_fg = "black", anno_bg = "white", anno_bg_r = 0.1,
    anno_position = c("topleft", "topright", "bottomleft", "bottomright", "tl", "tr", "bl", "br"),
    add_smooth = TRUE, smooth_color = "red2", smooth_width = 1.5, smooth_se = FALSE,
    theme = "theme_this", theme_args = list(), palette = ifelse(is.null(group_by), "Spectral", "Paired"), palcolor = NULL,
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL,
    facet_by = NULL, facet_scales = "fixed", facet_ncol = NULL, facet_nrow = NULL, facet_byrow = TRUE,
    aspect.ratio = 1, legend.position = waiver(), legend.direction = "vertical", seed = 8525,
    ...
) {
    set.seed(seed)
    anno_position <- match.arg(anno_position)
    anno_position <- switch(anno_position,
        tl = "topleft", tr = "topright", bl = "bottomleft", br = "bottomright",
        anno_position
    )
    if (length(raster_dpi) == 1) {
        raster_dpi <- rep(raster_dpi, 2)
    }

    x <- check_columns(data, x)
    y <- check_columns(data, y)
    group_by <- check_columns(data, group_by, force_factor = TRUE, allow_multi = TRUE,
        concat_multi = TRUE, concat_sep = group_by_sep)

    if (is.null(group_by)) {
        group_by <- ".group"
        data[[group_by]] <- factor("")
        legend.position <- ifelse(inherits(legend.position, "waiver"), "none", "right")
    } else {
        legend.position <- ifelse(inherits(legend.position, "waiver"), "right", legend.position)
    }

    base_size <- theme_args$base_size %||% 12
    text_size_scale <- base_size / 12

    # Calcuate the equation of the line, r-squared, and rho
    annodata <- data %>%
        dplyr::group_by(!!!syms(facet_by)) %>%
        group_modify(function(dat, g) {
            m <- lm(data[[y]] ~ data[[x]])
            anno <- c()
            for (item in anno_items) {
                if (item == "eq") {
                    a <- format(as.numeric(coef(m)[1]), digits = 2)
                    b <- format(as.numeric(abs(coef(m)[2])), digits = 2)
                    if (coef(m)[2] >= 0) {
                        anno_eq <- substitute(italic(y) == a + b %.% italic(x), list(a = a, b = b))
                    } else {
                        anno_eq <- substitute(italic(y) == a - b %.% italic(x), list(a = a, b = b))
                    }
                    anno <- c(anno, as.character(as.expression(anno_eq)))
                } else if (item == "r2") {
                    anno_r2 <- substitute(
                        italic(r)^2 ~ "=" ~ r2,
                        list(r2 = format(summary(m)$r.squared, digits = 2))
                    )
                    anno <- c(anno, as.character(as.expression(anno_r2)))
                } else if (item == "p") {
                    anno_p <- substitute(
                        italic("coeff.") ~ italic(p) ~ "=" ~ pvalue,
                        list(pvalue = format(summary(m)$coefficients[2, 4], digits = 2))
                    )
                    anno <- c(anno, as.character(as.expression(anno_p)))
                } else if (item == "spearman") {
                    rho <- cor(dat[[x]], dat[[y]], method = "spearman")
                    anno_rho <- substitute(italic("spearman's") ~ italic(rho) ~ "=" ~ value, list(value = format(rho, digits = 2)))
                    anno <- c(anno, as.character(as.expression(anno_rho)))
                } else if (item == "pearson") {
                    r <- cor(dat[[x]], dat[[y]], method = "pearson")
                    anno_r <- substitute(italic("pearson's") ~ italic(r) ~ "=" ~ value, list(value = format(r, digits = 2)))
                    anno <- c(anno, as.character(as.expression(anno_r)))
                } else if (item == "kendall") {
                    tau <- cor(dat[[x]], dat[[y]], method = "kendall")
                    anno_tau <- substitute(italic("kendall's") ~ italic(tau) ~ "=" ~ value, list(value = format(tau, digits = 2)))
                    anno <- c(anno, as.character(as.expression(anno_tau)))
                } else if (item == "n") {
                    n <- nrow(dat)
                    anno_n <- substitute(italic(N) ~ "=" ~ value, list(value = n))
                    anno <- c(anno, as.character(as.expression(anno_n)))
                } else {
                    stop("Unknown annotation item: ", item, ". Expect: eq, r2, p, spearman, pearson, kendall, n")
                }
            }
            data.frame(anno = anno)
        })

    p <- ggplot(data, aes(x = !!sym(x), y = !!sym(y)))

    data$.highlight <- FALSE
    if (!is.null(highlight)) {
        if (isTRUE(highlight)) {
            data$.highlight <- TRUE
        } else if (length(highlight == 1 && is.character(highlight))) {
            data <- data %>% mutate(.highlight = eval(parse(text = highlight)))
        } else {
            all_inst <- rownames(data) %||% 1:nrow(data)
            if (!any(highlight %in% all_inst)) {
                stop("No highlight items found in the data (rownames).")
            }
            if (!all(highlight %in% all_inst)) {
                warning("Some highlight items not found in the data (rownames).")
            }
            data$.highlight <- all_inst %in% highlight
            rm(all_inst)
        }
    }

    if (isTRUE(raster)) {
        # Normal points
        if (sum(data$.highlight == FALSE) > 0) {
            p <- p + scattermore::geom_scattermore(
                data = data[data$.highlight == FALSE, , drop = FALSE], aes(color = !!sym(group_by)),
                pointsize = ceiling(pt_size), alpha = alpha, pixels = raster_dpi
            )
        }
        # Highlight points
        if (sum(data$.highlight) > 0) {
            hi_df <- data[data$.highlight, , drop = FALSE]
            p <- p +
                scattermore::geom_scattermore(
                    data = hi_df, aes(x = !!sym(x), y = !!sym(y)), color = highlight_color, pointsize = floor(highlight_size) + highlight_stroke,
                    alpha = highlight_alpha, pixels = raster_dpi, inherit.aes = FALSE) +
                scattermore::geom_scattermore(
                    data = hi_df, aes(color = !!sym(group_by)), pointsize = floor(highlight_size),
                    alpha = highlight_alpha, pixels = raster_dpi)
            rm(hi_df)
        }
    } else {
        # Normal points
        p <- p + geom_point(aes(color = !!sym(group_by)), size = pt_size, shape = pt_shape, alpha = alpha)
        # Highlight points
        if (sum(data$.highlight) > 0) {
            hi_df <- data[data$.highlight, , drop = FALSE]
            p <- p +
                geom_point(data = hi_df, aes(x = !!sym(x), y = !!sym(y)), color = highlight_color, size = highlight_size + highlight_stroke,
                    shape = pt_shape, alpha = highlight_alpha, inherit.aes = FALSE) +
                geom_point(data = hi_df, aes(color = !!sym(group_by)), size = highlight_size, shape = pt_shape,
                    alpha = highlight_alpha)
            rm(hi_df)
        }
    }

    anno_x <- ifelse(grepl("left", anno_position), -Inf, Inf)
    anno_y <- ifelse(grepl("top", anno_position), Inf, -Inf)
    p <- p +
        geom_smooth(method = "lm", formula = y ~ x, se = smooth_se, color = smooth_color, linewidth = smooth_width, alpha = 0.5) +
        geom_text_repel(
            data = annodata, parse = TRUE, hjust = 0, direction = "y",
            aes(label = anno), x = anno_x, y = anno_y, seed = seed,
            size = text_size_scale * anno_size,
            bg.color = anno_bg, bg.r = anno_bg_r, color = anno_fg,
            force = 0.5, max.overlaps = 100, segment.color = "transparent"
        ) +
        scale_color_manual(
            name = group_name %||% group_by,
            values = palette_this(levels(data[[group_by]]), palette = palette, palcolor = palcolor)
        ) +
        labs(title = title, subtitle = subtitle, x = xlab %||% x, y = ylab %||% y) +
        do.call(theme, theme_args) +
        ggplot2::theme(
            aspect.ratio = aspect.ratio,
            legend.position = legend.position,
            legend.direction = legend.direction
        )

    height <- width <- 4.5
    if (!identical(legend.position, "none")) {
        if (legend.position %in% c("right", "left")) {
            width <- width + 1
        } else if (legend.direction == "horizontal") {
            height <- height + 1
        } else {
            width <- width + 2
        }
    }

    attr(p, "height") <- height
    attr(p, "width") <- width

    facet_plot(p, facet_by, facet_scales, facet_nrow, facet_ncol, facet_byrow,
        legend.position = legend.position, legend.direction = legend.direction)
}

#' CorPlot
#'
#' @description Generate scatter correlation plot for two variables.
#' @inheritParams common_args
#' @inheritParams CorPlotAtomic
#' @return A ggplot object or a list of ggplot objects if `combine` is `FALSE`.
#' @export
#' @examples
#' data(iris)
#' CorPlot(iris, "Sepal.Length", "Sepal.Width", group_by = "Species")
#' CorPlot(iris, "Sepal.Length", "Sepal.Width", group_by = "Species",
#'  highlight = 'Species == "setosa"', highlight_stroke = 1.5,
#'  anno_items = c("eq", "pearson"), anno_position = "bottomright")
#' CorPlot(iris, "Sepal.Length", "Sepal.Width", facet_by = "Species", facet_scales = "free")
CorPlot <- function(
    data, x, y, group_by = NULL, group_by_sep = "_", group_name = NULL, split_by = NULL, split_by_sep = "_",
    pt_size = 2, pt_shape = 16, raster = FALSE, alpha = 1, raster_dpi = c(512, 512),
    highlight = NULL, highlight_color = "black", highlight_size = 1, highlight_alpha = 1, highlight_stroke = 0.8,
    anno_items = c("eq", "r2", "p"), anno_size = 3, anno_fg = "black", anno_bg = "white", anno_bg_r = 0.1,
    anno_position = c("topleft", "topright", "bottomleft", "bottomright", "tl", "tr", "bl", "br"),
    add_smooth = TRUE, smooth_color = "red2", smooth_width = 1.5, smooth_se = FALSE,
    theme = "theme_this", theme_args = list(), palette = ifelse(is.null(group_by), "Spectral", "Paired"), palcolor = NULL,
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL,
    facet_by = NULL, facet_scales = "fixed", facet_ncol = NULL, facet_nrow = NULL, facet_byrow = TRUE,
    aspect.ratio = 1, legend.position = waiver(), legend.direction = "vertical", seed = 8525,
    combine = TRUE, nrow = NULL, ncol = NULL, byrow = TRUE, ...
) {
    validate_common_args(seed, facet_by = facet_by)
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
            CorPlotAtomic(
                data = datas[[nm]], x = x, y = y, group_by = group_by, group_by_sep = group_by_sep, group_name = group_name,
                pt_size = pt_size, pt_shape = pt_shape, raster = raster, raster_dpi = raster_dpi,
                highlight = highlight, highlight_color = highlight_color, highlight_size = highlight_size, highlight_alpha = highlight_alpha, highlight_stroke = highlight_stroke,
                anno_items = anno_items, anno_size = anno_size, anno_fg = anno_fg, anno_bg = anno_bg, anno_bg_r = anno_bg_r,
                anno_position = anno_position, add_smooth = add_smooth, smooth_color = smooth_color, smooth_width = smooth_width, smooth_se = smooth_se,
                theme = theme, theme_args = theme_args, palette = palette, palcolor = palcolor, alpha = alpha,
                title = title, subtitle = subtitle, xlab = xlab, ylab = ylab,
                facet_by = facet_by, facet_scales = facet_scales, facet_ncol = facet_ncol, facet_nrow = facet_nrow, facet_byrow = facet_byrow,
                aspect.ratio = aspect.ratio, legend.position = legend.position, legend.direction = legend.direction, seed = seed,
                ...
            )
        }
    )

    combine_plots(plots, combine = combine, nrow = nrow, ncol = ncol, byrow = byrow)
}

#' Atomic Correlation Pairs Plot
#'
#' @description Generate a grid of scatter correlation plots for all pairs of variables.
#' @inheritParams common_args
#' @param columns The column names of the data to be plotted.
#'  If NULL, all columns, except `group_by`, will be used.
#' @param group_by The column name of the data to be used for grouping.
#'  Different groups will be plotted in different colors.
#' @param group_by_sep The separator used to concatenate multiple columns in `group_by`.
#' @param group_name The name of the group in the legend.
#' @param diag_type The type of the diagonal plots.
#'  Available types: "density", "violin", "histogram", "box", "none".
#' @param diag_args A list of additional arguments to be passed to the diagonal plots.
#' @param layout The layout of the plots.
#'  Available layouts: ".\\", "\\.", "/.", "./".
#'  * '\\' or '/' means the diagonal plots are on the top-left to bottom-right diagonal.
#'  * '.' means where the scatter plots are.
#' @param cor_method The method to calculate the correlation.
#'  Available methods: "pearson", "spearman", "kendall".
#'  The correlation will be shown in the other triangle of the scatter plots.
#' @param cor_palette The color palette for the correlation tile plots.
#' @param cor_palcolor Custom colors used to create a color palette for the correlation tile plots.
#' @param cor_size The size of the correlation text.
#' @param cor_format The format of the correlation text. Default is "corr: %.2f".
#'  It will be formatted using \code{sprintf(cor_format, corr)}.
#' @param cor_fg The color of the correlation text.
#' @param cor_bg The background color of the correlation text.
#' @param cor_bg_r The radius of the background of the correlation text.
#' @param palette The color palette for the scatter plots and default palette for the diagonal plots.
#' @param palcolor Custom colors used to create a color palette for the scatter plots and diagonal plots.
#' @param ... Additional arguments to pass to \code{\link{CorPlot}}.
#' @details `theme` and `theme_args` are also supported, they will be passed to each individual plot.
#' @return A `patch_work::wrap_plots` object.
#' @importFrom gglogger ggplot
#' @importFrom ggplot2 waiver geom_tile element_blank scale_fill_gradientn guide_colorbar scale_x_continuous
#' @importFrom ggplot2 geom_line scale_y_continuous
#' @importFrom ggrepel geom_text_repel
#' @importFrom patchwork wrap_plots plot_layout plot_annotation
#' @keywords internal
CorPairsPlotAtomic <- function(
    data, columns = NULL, group_by = NULL, group_by_sep = "_", group_name = NULL,
    diag_type = NULL, diag_args = list(), layout = c(".\\", "\\.", "/.", "./"),
    cor_method = c("pearson", "spearman", "kendall"), cor_palette = "RdBu", cor_palcolor = NULL,
    cor_size = 3, cor_format = "corr: {round(corr, 2)}", cor_fg = "black", cor_bg = "white", cor_bg_r = 0.1,
    theme = "theme_this", theme_args = list(), palette = ifelse(is.null(group_by), "Spectral", "Paired"), palcolor = NULL,
    title = NULL, subtitle = NULL,
    facet_by = NULL, legend.position = "right", legend.direction = "vertical", seed = 8525,
    ...
) {
    set.seed(seed)
    if (!is.null(facet_by)) {
        stop("'facet_by' is not supported in CorPairsPlot. Consider using 'split_by'.")
    }
    if (is.null(columns)) {
        if (!is.null(group_by)) {
            columns <- setdiff(colnames(data), group_by)
        } else {
            columns <- colnames(data)
        }
    } else {
        columns <- check_columns(data, columns, allow_multi = TRUE)
    }
    if (length(columns) < 2) {
        stop("At least two columns are required.")
    }
    if (is.null(diag_type)) {
        diag_type <- ifelse(is.null(group_by), "density", "violin")
    }
    diag_type <- match.arg(diag_type, c("density", "violin", "histogram", "box", "none"))
    layout <- match.arg(layout)
    cor_method <- match.arg(cor_method)
    base_size <- theme_args$base_size %||% 12
    text_size_scale <- base_size / 12

    # get the plot info (plotype, axis position, etc.) based on the layout
    get_plot_info <- function(i, j, j1) {
        diag_no_x_axis <- diag_type %in% c("box", "violin", "none")
        xaxis <- yaxis <- xlab <- ylab <- "none"
        if (i == j1) {  # diagonal
            if (layout %in% c(".\\", "/.")) {
                if (i == 1) {  # top corner
                    yaxis <- ifelse(diag_type == "none", "none", ifelse(layout == ".\\", "left", "right"))
                    xlab <- "top"
                } else if (i == length(columns)) {  # bottom corner
                    xaxis <- ifelse(diag_no_x_axis, "none", "bottom")
                    ylab <- ifelse(layout == ".\\", "right", "left")
                }
            } else {  # layout %in% c("\\.", "./")
                if (i == 1) {  # top corner
                    xaxis <- ifelse(diag_no_x_axis, "none", "top")
                    xlab <- "none"
                    ylab <- ifelse(layout == "\\.", "left", "right")
                } else if (i == length(columns)) {  # bottom corner
                    yaxis <- ifelse(diag_type == "none", "none", ifelse(layout == "\\.", "right", "left"))
                    xlab <- "bottom"
                }
            }
            return(list(type = layout, xaxis = xaxis, yaxis = yaxis, xlab = xlab, ylab = ylab))
        } else if (i < j1) {  # upper triangle
            if (layout %in% c(".\\", "/.")) {
                xlab <- ifelse(i > 1, "none", "top")
                ylab <- ifelse(j1 < length(columns), "none", ifelse(layout == "/.", "left", "right"))
                return(list(type = "fill", xaxis = xaxis, yaxis = yaxis, xlab = xlab, ylab = ylab))
            } else {  # layout %in% c("\\.", "./")
                xaxis <- ifelse(i > 1, "none", "top")
                yaxis <- ifelse(j1 < length(columns), "none", ifelse(layout == "\\.", "right", "left"))
                return(list(type = "cor", xaxis = xaxis, yaxis = yaxis, xlab = xlab, ylab = ylab))
            }
        } else {  # i > j1, lower triangle
            if (layout %in% c(".\\", "/.")) {
                xaxis <- ifelse(i < length(columns), "none", "bottom")
                yaxis <- ifelse(j1 > 1, "none", ifelse(layout == "/.", "right", "left"))
                return(list(type = "cor", xaxis = xaxis, yaxis = yaxis, xlab = xlab, ylab = ylab))
            } else {  # layout %in% c("\\.", "./")
                xlab <- ifelse(i < length(columns), "none", "bottom")
                ylab <- ifelse(j1 > 1, "none", ifelse(layout == "\\.", "left", "right"))
                return(list(type = "fill", xaxis = xaxis, yaxis = yaxis, xlab = xlab, ylab = ylab))
            }
        }
    }

    get_plot <- function(x, y, info) {
        # Use facet to add label
        expand <- c(0, 0)
        if (diag_type == "none") {
            if (info$type %in% c("./", "/.")) {
                p <- ggplot(data.frame(x = c(0, 1), y = c(0, 1)), aes(x = x, y = y)) +
                    geom_line(color = "darkred", linewidth = 1.5) +
                    scale_x_continuous(expand = expand) +
                    scale_y_continuous(expand = expand)
            } else if (info$type %in% c(".\\", "\\.")) {
                p <- ggplot(data.frame(x = c(0, 1), y = c(1, 0)), aes(x = x, y = y)) +
                    geom_line(color = "darkred", linewidth = 1.5) +
                    scale_x_continuous(expand = expand) +
                    scale_y_continuous(expand = expand)
            }
        } else if (info$type %in% c(".\\", "\\.", "/.", "./")) {
            info$type <- diag_type
        }
        if (info$type == "density") {
            # Use guides instead of legend.position to remove the legend
            # This way & theme(legend.position = ...) can work as expected
            args <- diag_args
            args$data <- data
            args$x <- x
            args$group_by <- group_by
            args$add_lines <- args$add_lines %||% FALSE
            args$palette <- args$palette %||% palette
            args$palcolor <- args$palcolor %||% palcolor
            p <- do.call(DensityPlot, args)
            if (is.null(group_by) || (identical(args$palette, palette) && identical(args$palcolor, palcolor))) {
                p <- p + guides(fill = "none", color = "none")
            }
        } else if (info$type == "violin") {
            if (is.null(group_by)) {
                stop("No 'group_by' is specified for 'violin' plot on the diagnal cells. Consider using 'density' instead.")
            }
            args <- diag_args
            args$data <- data
            args$x <- group_by
            args$y <- x
            args$palette <- args$palette %||% palette
            args$palcolor <- args$palcolor %||% palcolor
            p <- do.call(ViolinPlot, args)
            if (identical(args$palette, palette) && identical(args$palcolor, palcolor)) {
                p <- p + guides(fill = "none")
            }
        } else if (info$type == "histogram") {
            args <- diag_args
            args$data <- data
            args$x <- x
            args$group_by <- group_by
            args$palette <- args$palette %||% palette
            args$palcolor <- args$palcolor %||% palcolor
            p <- do.call(Histogram, args)
            if (is.null(group_by) || (identical(args$palette, palette) && identical(args$palcolor, palcolor))) {
                p <- p + guides(fill = "none", color = "none")
            }
        } else if (info$type == "box") {
            if (is.null(group_by)) {
                stop("No 'group_by' is specified for 'box' plot on the diagnal cells. Consider using 'histogram' instead.")
            }
            args <- diag_args
            args$data <- data
            args$x <- group_by
            args$y <- x
            args$palette <- args$palette %||% palette
            args$palcolor <- args$palcolor %||% palcolor
            p <- do.call(BoxPlot, args)
            if (identical(args$palette, palette) && identical(args$palcolor, palcolor)) {
                p <- p + guides(fill = "none")
            }
        } else if (info$type == "cor") {
            expand <- waiver()
            p <- CorPlot(data, x = x, y = y, group_by = group_by, group_name = group_name,
                palette = palette, palcolor = palcolor,
                legend.position = legend.position, legend.direction = legend.direction, seed = seed, ...)
            if (is.null(group_by)) {
                p <- p + guides(fill = "none", color = "none")
            }
        } else if (info$type == "fill") {
            corr <- cor(data[[x]], data[[y]], method = cor_method)
            # x, y for cor_format
            p <- ggplot(data.frame(i = 0.5, j = 0.5, x = y, y = x, corr = corr), aes(x = i, y = j, fill = corr)) +
                geom_tile(width = 1, height = 1) +
                geom_text_repel(
                    mapping = aes(label = glue(cor_format)),
                    segment.color = "transparent", force = 0,
                    color = cor_fg, bg.color = cor_bg, bg.r = cor_bg_r,
                    size = text_size_scale * cor_size, seed = seed) +
                scale_fill_gradientn(
                    colors = palette_this(c(-1, 1), palette = cor_palette, palcolor = cor_palcolor),
                    limits = c(-1, 1), breaks = c(-1, 0, 1), labels = scales::number_format(accuracy = 0.1),
                    guide = guide_colorbar(frame.colour = "black", ticks.colour = "black", title.hjust = 0)
                ) +
                scale_x_continuous(expand = expand) +
                scale_y_continuous(expand = expand)
        }
        p <- p + do.call(theme, theme_args)

        suppressMessages({
            # place the axis and labels
            if (info$type %in% c("violin", "box")) {
                scale_x <- scale_x_discrete
                expand <- waiver()
            } else {
                scale_x <- scale_x_continuous
            }
            if (info$xlab == "top") {
                p <- p + ggplot2::xlab(x) + scale_x(position = "top", expand = expand) +
                    ggplot2::theme(axis.title.x = element_textbox(face = "bold"))
            } else if (info$xlab == "bottom") {
                p <- p + ggplot2::xlab(x) + scale_x(expand = expand) +
                    ggplot2::theme(axis.title.x = element_textbox(face = "bold"))
            } else {
                p <- p + ggplot2::theme(axis.title.x = element_blank())
            }
            if (info$ylab == "left") {
                p <- p + ggplot2::ylab(y) + scale_y_continuous(expand = expand) +
                    ggplot2::theme(axis.title.y = element_textbox(orientation = "left-rotated"))
            } else if (info$ylab == "right") {
                p <- p + ggplot2::ylab(y) + scale_y_continuous(position = "right", expand = expand) +
                    ggplot2::theme(axis.title.y = element_textbox(orientation = "right-rotated"))
            } else {
                p <- p + ggplot2::theme(axis.title.y = ggplot2::element_blank())
            }
            if (info$xaxis == "bottom") {
                p <- p + scale_x(expand = expand)
            } else if (info$xaxis == "top") {
                p <- p + scale_x(position = "top", expand = expand)
            } else {
                p <- p + ggplot2::theme(axis.text.x = ggplot2::element_blank(), axis.ticks.x = ggplot2::element_blank())
            }
            if (info$yaxis == "left") {
                p <- p + scale_y_continuous(expand = expand)
            } else if (info$yaxis == "right") {
                p <- p + scale_y_continuous(position = "right", expand = expand)
            } else {
                p <- p + ggplot2::theme(axis.text.y = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank())
            }
        })
        p + ggplot2::theme(plot.margin = margin(1, 1, 1, 1), aspect.ratio = 1)
    }

    plots <- list()
    for (i in seq_along(columns)) {
        for (j in seq_along(columns)) {
            j1 <- ifelse(layout %in% c("./", "/."), length(columns) - j + 1, j)
            info <- get_plot_info(i, j, j1)
            # For debugging
            # info$i <- i
            # info$j <- j
            # info$j1 <- j1
            # str(info)
            p <- get_plot(columns[j1], columns[i], info)
            plots <- c(plots, list(p))
        }
    }

    p <- wrap_plots(plots, ncol = length(columns), byrow = TRUE) +
        plot_layout(guides = "collect") +
        plot_annotation(title = title, subtitle = subtitle) &
        ggplot2::theme(legend.position = legend.position, legend.direction = legend.direction)
    # so that the plot title can be displayed
    p <- patchwork::wrap_elements(p)

    height <- width <- sqrt(length(columns)) * 4
    if (!identical(legend.position, "none")) {
        if (legend.position %in% c("right", "left")) {
            width <- width + 1
        } else if (legend.direction == "horizontal") {
            height <- height + 1
        } else {
            width <- width + 2
        }
    }

    attr(p, "height") <- height
    attr(p, "width") <- width
    p
}

#' CorPairsPlot
#'
#' @description Generate a grid of scatter correlation plots for all pairs of variables.
#' @inheritParams common_args
#' @inheritParams CorPairsPlotAtomic
#' @return A `patch_work::wrap_plots` object or a list of them if `combine` is `FALSE`.
#' @export
#' @importFrom glue glue
#' @examples
#' set.seed(8525)
#' data <- data.frame(x = rnorm(100))
#' data$y <- rnorm(100, 10, sd = 0.5)
#' data$z <- -data$x + data$y + rnorm(100, 20, 1)
#' data$g <- sample(1:4, 100, replace = TRUE)
#'
#' CorPairsPlot(data, diag_type = "histogram", diag_args = list(bins = 30, palette = "Paired"),
#'  layout = "/.")
#'
#' CorPairsPlot(data, group_by = "g", diag_type = "none", layout = "./",
#'  theme_args = list(axis.title = element_textbox(
#'      color = "black", box.color = "grey20", size = 16, halign = 0.5, fill = "grey90",
#'      linetype = 1, width = grid::unit(1, "npc"), padding = ggplot2::margin(5, 5, 5, 5))))
#'
#' CorPairsPlot(data, group_by = "g", diag_type = "violin", layout = "\\.",
#'   cor_format = "{x}\n{y}\ncorr: {round(corr, 2)}")
#'
#' CorPairsPlot(data, split_by = "g", diag_type = "none", layout = ".\\",
#'  legend.position = "bottom", legend.direction = "horizontal", group_name = "group")
CorPairsPlot <- function(
    data, columns = NULL, group_by = NULL, group_by_sep = "_", group_name = NULL, split_by = NULL, split_by_sep = "_",
    diag_type = NULL, diag_args = list(), layout = c(".\\", "\\.", "/.", "./"),
    cor_method = c("pearson", "spearman", "kendall"), cor_palette = "RdBu", cor_palcolor = NULL,
    cor_size = 3, cor_format = "corr: {round(corr, 2)}", cor_fg = "black", cor_bg = "white", cor_bg_r = 0.1,
    theme = "theme_this", theme_args = list(), palette = ifelse(is.null(group_by), "Spectral", "Paired"), palcolor = NULL,
    title = NULL, subtitle = NULL, facet_by = NULL, legend.position = "right", legend.direction = "vertical", seed = 8525,
    combine = TRUE, nrow = NULL, ncol = NULL, byrow = TRUE, ...
) {
    validate_common_args(seed)
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
            if (!is.null(split_by)) {
                datas[[nm]][[split_by]] <- NULL
            }

            CorPairsPlotAtomic(
                data = datas[[nm]], columns = columns, group_by = group_by, group_by_sep = group_by_sep, group_name = group_name,
                diag_type = diag_type, diag_args = diag_args, layout = layout,
                cor_method = cor_method, cor_palette = cor_palette, cor_palcolor = cor_palcolor,
                cor_size = cor_size, cor_format = cor_format, cor_fg = cor_fg, cor_bg = cor_bg, cor_bg_r = cor_bg_r,
                theme = theme, theme_args = theme_args, palette = palette, palcolor = palcolor,
                title = title, subtitle = subtitle, legend.position = legend.position, legend.direction = legend.direction, seed = seed,
                ...
            )
        }
    )

    combine_plots(plots, combine = combine, nrow = nrow, ncol = ncol, byrow = byrow)
}
