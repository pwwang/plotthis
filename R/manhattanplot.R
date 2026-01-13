#' ManhattanPlotAtomic
#'
#' Plot a Manhattan plot for atomic data (without splitting).
#' This function is borrowed from `ggmanh::manhattan_plot()` with following customizations:
#'
#' * The dots in argument names are replaced with underscores wherever possible.
#' * `chr.colname`, `pos.colname`, `pval.colname` and `label.colname` are replaced with
#'   `chr_by`, `pos_by`, `pval_by` and `label_by` respectively.
#' * The `chromosome` and `chr.order` arguments are merged into a single argument `chromosomes`.
#' * The `highlight.colname` argument is replaced with `highlight`, which can be a vector of indices
#'   or a character of expression to select the variants to be highlighted, instead of a column name.
#' * `point.size` is replaced with `pt_size`
#' * When `highlight` is specified, the colors of the points
#'   will be controled by `pt_color` and `highlight_color` arguments.
#' * The labels get more controled by `label_*` arguments.
#' * The highlighted points get more controled by `highlight_*` arguments.
#' * The `pval_log_transform` argument is replaced with `pval_transform`, which allows to specify
#'  a function to transform the p-values.
#'
#' @inheritParams common_args
#' @param data A data frame or `GenomicRanges::GRanges` containing the data to be plotted.
#' @param chr_by Column name for chromosome (default: "chr").
#' @param pos_by Column name for position (default: "pos").
#' @param pval_by Column name for p-value (default: "pval").
#' @param label_by Column name for the variants to be labeled (default: NULL).
#' Only the variants with values in this column will be labeled.
#' @param chromosomes A vector of chromosomes to be plotted (default: NULL).
#' If NULL, all chromosomes will be plotted.
#' It is more of a combination of the `chromosome` and `chr.order` arguments of
#' `ggmanh::manhattan_plot()`.
#' We can use it to select chromosomes to be plotted or to set the order of the chromosomes.
#' @param pt_size A numeric value to specify the size of the points in the plot.
#' @param pt_color A character string to specify the color of the points in the plot.
#' By default, the color of the points will be controled by `palette` or `palcolor` arguments.
#' This is useful to color the background points when `highlight` and `highlight_color`
#' are specified.
#' @param pt_alpha A numeric value to specify the transparency of the points in the plot.
#' @param pt_shape A numeric value to specify the shape of the points in the plot.
#' @param label_size A numeric value to specify the size of the labels in the plot.
#' @param label_fg A character string to specify the color of the labels in the plot.
#' If NULL, the color of the labels will be the same as the points.
#' @param highlight Either a vector of indices or a character of expression to select
#' the variants to be highlighted (default: NULL).
#' If NULL, no variants will be highlighted.
#' @param highlight_color A character string to specify the color of the highlighted points.
#' @param highlight_size A numeric value to specify the size of the highlighted points.
#' @param highlight_alpha A numeric value to specify the transparency of the highlighted points.
#' @param highlight_shape A numeric value to specify the shape of the highlighted points.
#' @param preserve_position If TRUE, the width of each chromosome reflect the number of variants
#' and the position of each variant is correctly scaled?
#' If FALSE, the width of each chromosome is equal and the variants are equally spaced.
#' @param chr_gap_scaling A numeric value to specify the scaling of the gap between chromosomes.
#' It is used to adjust the gap between chromosomes in the plot.
#' @param pval_transform A function to transform the p-values (default: -log10).
#' If it is a character, it will be evaluated as a function.
#' @param signif A vector of significance thresholds (default: c(5e-08, 1e-05)).
#' @param signif_color A character vector of equal length as signif.
#' It contains colors for the lines drawn at signif.
#' If NULL, the smallest value is colored black while others are grey.
#' @param signif_rel_pos A numeric between 0.1 and 0.9. If the plot is rescaled,
#' @param signif_label A logical value indicating whether to label the significance thresholds (default: TRUE).
#' @param signif_label_size A numeric value to specify the size of the significance labels.
#' @param signif_label_pos A character string specifying the position of the significance labels.
#' where should the significance threshold be positioned?
#' It can be either "left" or "right" (default: "left").
#' @param thin A logical value indicating whether to thin the data (default: NULL).
#' Defaults to TRUE when `chromosomes` is specified and the length of
#' it is less than the number of chromosomes in the data. Defaults to FALSE otherwise.
#' @param thin_n Number of max points per horizontal partitions of the plot. Defaults to 1000.
#' @param thin_bins Number of bins to partition the data. Defaults to 200.
#' @param rescale A logical value indicating whether to rescale the plot (default: TRUE).
#' @param rescale_ratio_threshold A numeric value to specify the ratio threshold for rescaling.
#' @param palreverse A logical value indicating whether to reverse the palette for chromosomes (default: FALSE).
#' @param alpha Alias of `pt_alpha`.
#' @keywords internal
#' @importFrom utils getFromNamespace
#' @importFrom rlang sym %||%
#' @importFrom ggplot2 waiver aes geom_point scale_y_continuous scale_color_manual annotate
#' @importFrom ggplot2 scale_x_continuous geom_hline element_line coord_cartesian geom_text
#' @importFrom ggrepel geom_label_repel
#' @return A ggplot object.
ManhattanPlotAtomic <- function(
    data, chr_by, pos_by, pval_by, label_by = NULL,
    chromosomes = NULL, pt_size = 0.75, pt_color = NULL, pt_alpha = alpha, pt_shape = 19,
    label_size = 3, label_fg = NULL, highlight = NULL, highlight_color = NULL,
    highlight_size = 1.5, highlight_alpha = 1, highlight_shape = 19,
    preserve_position = TRUE, chr_gap_scaling = 1, pval_transform = "-log10",
    signif = c(5e-08, 1e-05), signif_color = NULL, signif_rel_pos = 0.2, signif_label = TRUE,
    signif_label_size = 3.5, signif_label_pos = c("left", "right"),
    thin = NULL, thin_n = 1000, thin_bins = 200, rescale = TRUE, rescale_ratio_threshold = 5,
    palette = "Dark2", palcolor = NULL, palreverse = FALSE, alpha = 1,
    theme = "theme_this", theme_args = list(), title = NULL, subtitle = NULL,
    xlab = NULL, ylab = expression("\u002d" * log[10](p)), ...) {

    ggplot <- if (getOption("plotthis.gglogger.enabled", FALSE)) {
        gglogger::ggplot
    } else {
        ggplot2::ggplot
    }
    signif_label_pos <- match.arg(signif_label_pos)

    mpdata <- suppressWarnings({
        # Some chromosomes are not specified in chr.order
        ggmanh::manhattan_data_preprocess(
            x = data,
            chromosome = if (length(chromosomes) == 1) chromosomes else NULL,
            signif = signif,
            pval.colname = pval_by,
            chr.colname = chr_by,
            pos.colname = pos_by,
            chr.order = chromosomes,
            signif.col = signif_color,
            preserve.position = preserve_position,
            thin = thin,
            thin.n = thin_n,
            thin.bins = thin_bins,
            pval.log.transform = TRUE,
            chr.gap.scaling = chr_gap_scaling,
        )
    })
    label_by <- check_columns(mpdata$data, label_by, force_factor = TRUE)

    if ((!is.null(highlight) && !is.null(highlight_color)) || !is.null(pt_color)) {
        pt_color <- pt_color %||% "grey80"
        mpdata$chr.col <- rep(pt_color, length(levels(mpdata$data[[chr_by]])))
        names(mpdata$chr.col) <- levels(mpdata$data[[chr_by]])
    } else {
        mpdata$chr.col <- palette_this(
            levels(mpdata$data[[chr_by]]),
            palette = palette, palcolor = palcolor, reverse = palreverse, alpha = pt_alpha
        )
    }

    if (is.character(pval_transform) && startsWith(pval_transform, "-")) {
        pval_transform <- sub("^-", "", pval_transform)
        pval_transform_fn <- eval(parse(text = pval_transform))
        pval_transform <- function(x) -pval_transform_fn(x)
    }
    if (is.character(pval_transform)) {
        pval_transform <- eval(parse(text = pval_transform))
    }
    stopifnot("[ManhattanPlot] 'pval_transform' must be a function" = is.function(pval_transform))
    mpdata$data$log10pval <- pval_transform(mpdata$data[[pval_by]])
    mpdata$pval.colname <- "log10pval"

    trans <- list(trans = "identity", breaks = waiver())
    if (isTRUE(rescale)) {
        get_transform_jump <- getFromNamespace("get_transform_jump", "ggmanh")
        get_transform <- getFromNamespace("get_transform", "ggmanh")
        jump <- get_transform_jump(pval_transform(mpdata$signif))
        if ((ceiling(max(mpdata$data[[mpdata$pval.colname]]) / 5) * 5) / jump > rescale_ratio_threshold) {
            trans <- get_transform(mpdata$data, jump, mpdata$pval.colname, jump.rel.pos = signif_rel_pos)
        }
    }

    ylimit <- c(0, ifelse(identical(trans$trans, "identity"), NA, max(trans$breaks)))

    if (length(unique(mpdata$data[[chr_by]])) == 1) {
        pos <- mpdata$true.pos.colname
        x_break <- waiver()
        x_break_label <- waiver()
        x_limits <- NULL
        chrname <- as.character(unique(mpdata$data[[chr_by]]))
        xlab <- xlab %||% ifelse(
            startsWith(chrname, "chr") || startsWith(chrname, "Chr"),
            chrname,
            paste0("Chromosome ", chrname)
        )
    } else {
        calc_new_pos_ <- getFromNamespace("calc_new_pos_", "ggmanh")
        if (mpdata$pos.colname == "new_pos_unscaled") {
            mpdata$data$new_pos_unscaled <- calc_new_pos_(
                mpdata$data$new_pos_unscaled,
                mpdata$data[[mpdata$chr.colname]],
                mpdata$chr.pos.info
            )
        }
        pos <- mpdata$pos.colname
        x_break <- mpdata$chr.pos.info$center_pos
        x_break_label <- mpdata$chr.labels
        x_limits <- c(min(mpdata$chr.pos.info$start_pos), max(mpdata$chr.pos.info$end_pos))
    }
    xlab <- xlab %||% "Chromosome"

    expand <- c(0.02, 0.01, 0.025, 0.01)
    if (isTRUE(signif_label)) {
        expand[1] <- 0.1
    }
    expand <- norm_expansion(expand, "continuous", "continuous")

    p <- ggplot(
            mpdata$data,
            aes(x = !!sym(pos), y = !!sym(mpdata$pval.colname), color = !!sym(chr_by))) +
        geom_point(size = pt_size) +
        scale_color_manual(values = mpdata$chr.col, guide = "none") +
        scale_y_continuous(
            trans = trans$trans,
            breaks = trans$breaks,
            expand = expand$y,
            limits = ylimit) +
        scale_x_continuous(
            name = xlab,
            breaks = x_break,
            labels = x_break_label,
            expand = expand$x,
            limits = x_limits) +
        geom_hline(
            yintercept = pval_transform(mpdata$signif),
            linetype = 'dashed',
            color = mpdata$signif.col) +
        do.call(theme, theme_args) +
        ggplot2::theme(
            panel.grid.major = ggplot2::element_blank(),
            panel.grid.minor = ggplot2::element_blank(),
            legend.position = "none") +
        labs(x = xlab, y = ylab) +
        ggtitle(label = title, subtitle = subtitle)

    if (isTRUE(signif_label)) {
        p <- p + geom_text(
            data.frame(
                x = if (signif_label_pos == "left") -Inf else Inf,
                y = pval_transform(mpdata$signif),
                label = mpdata$signif,
                color = signif_color %||% c("black", rep("grey80", length(mpdata$signif) - 1))
            ),
            mapping = aes(x = !!sym("x"), y = !!sym("y"), label = !!sym("label"), color = I(!!sym("color"))),
            hjust = if (signif_label_pos == "left") -0.5 else 1.5,
            vjust = -0.5,
            size = signif_label_size
        )
    }

    if (!is.null(label_by)) {
        if (is.null(label_fg)) {
            p <- p + geom_label_repel(
                aes(label = !!sym(label_by), color = !!sym(chr_by)), size = label_size,
                min.segment.length = 0, max.overlaps = 100
            )
        } else {
            p <- p + geom_label_repel(
                aes(label = !!sym(label_by)), color = label_fg, size = label_size,
                min.segment.length = 0, max.overlaps = 100
            )
        }
    }

    if (rescale & !identical(trans$trans, "identity")) {
        # if the plot is rescaled, change the tick at the "jump" to double line
        jump_tick_size <- 3.5

        p <- p +
            ggplot2::theme(
                axis.ticks.y = element_line(linetype = trans$y_axis_linetype)) +
            annotate(geom = "point", shape = "=", x = -Inf, y = trans$jump, size = jump_tick_size) +
            coord_cartesian(clip = "off")
    }

    if (is.numeric(highlight)) {
        hidata <- mpdata$data[highlight, , drop = FALSE]
    } else if (is.character(highlight)) {
        hidata <- mpdata$data %>% filter(!!rlang::parse_expr(highlight))
    } else {
        hidata <- NULL
    }
    if (!is.null(hidata)) {
        if (is.null(highlight_color)) {
            p <- p + geom_point(
                data = hidata,
                aes(x = !!sym(pos), y = !!sym(mpdata$pval.colname), color = !!sym(chr_by)),
                size = highlight_size, shape = highlight_shape, alpha = highlight_alpha
            )
        } else {
            p <- p + geom_point(
                data = hidata,
                aes(x = !!sym(pos), y = !!sym(mpdata$pval.colname)),
                size = highlight_size, shape = highlight_shape,
                color = highlight_color, alpha = highlight_alpha
            )
        }
    }

    attr(p, "height") <- 4.5
    attr(p, "width") <- 0.4 * length(levels(mpdata$data[[chr_by]]))

    p
}

#' ManhattanPlot
#'
#' This function is borrowed from `ggmanh::manhattan_plot()` with following customizations:
#'
#' * The dots in argument names are replaced with underscores wherever possible.
#' * `chr.colname`, `pos.colname`, `pval.colname` and `label.colname` are replaced with
#'   `chr_by`, `pos_by`, `pval_by` and `label_by` respectively.
#' * The `chromosome` and `chr.order` arguments are merged into a single argument `chromosomes`.
#' * The `highlight.colname` argument is replaced with `highlight`, which can be a vector of indices
#'   or a character of expression to select the variants to be highlighted, instead of a column name.
#' * `point.size` is replaced with `pt_size`
#' * When `highlight` is specified, the colors of the points
#'   will be controled by `pt_color` and `highlight_color` arguments.
#' * The labels get more controled by `label_*` arguments.
#' * The highlighted points get more controled by `highlight_*` arguments.
#' * The `pval_log_transform` argument is replaced with `pval_transform`, which allows to specify
#'  a function to transform the p-values.
#'
#' @inheritParams common_args
#' @inheritParams ManhattanPlotAtomic
#' @returns A ggplot object or wrap_plots object or a list of ggplot objects.
#'  If no `split_by` is provided, a single plot (ggplot object) will be returned.
#'  If 'combine' is TRUE, a wrap_plots object will be returned.
#'  If 'combine' is FALSE, a list of ggplot objects will be returned.
#' @export
#' @examples
#' \donttest{
#' set.seed(1000)
#'
#' nsim <- 50000
#'
#' simdata <- data.frame(
#'   "chromosome" = sample(c(1:22,"X"), size = nsim, replace = TRUE),
#'   "position" = sample(1:100000000, size = nsim),
#'   "P.value" = rbeta(nsim, shape1 = 5, shape2 = 1)^7,
#'   "cohort" = sample(c("A", "B"), size = nsim, replace = TRUE)
#' )
#' simdata$chromosome <- factor(simdata$chromosome, c(1:22, "X"))
#' options(repr.plot.width=10, repr.plot.height=5)
#'
#' if (requireNamespace("ggmanh", quietly = TRUE)) {
#' ManhattanPlot(
#'    simdata, pval_by = "P.value", chr_by = "chromosome", pos_by = "position",
#'    title = "Simulated P.Values", ylab = "P")
#' }
#'
#' if (requireNamespace("ggmanh", quietly = TRUE)) {
#' # split_by
#' ManhattanPlot(
#'    simdata, pval_by = "P.value", chr_by = "chromosome", pos_by = "position",
#'    title = "Simulated P.Values", ylab = "P", split_by = "cohort", ncol = 1)
#' }
#'
#' if (requireNamespace("ggmanh", quietly = TRUE)) {
#' # Customized p-value transformation and significance threshold line colors
#' ManhattanPlot(
#'    simdata, pval_by = "P.value", chr_by = "chromosome", pos_by = "position",
#'    title = "Simulated -Log2 P.Values", ylab = "-log2(P)", pval_transform = "-log2",
#'    signif_color = c("red", "blue"))
#' }
#'
#' if (requireNamespace("ggmanh", quietly = TRUE)) {
#' # Use a different palette and don't show significance threshold labels
#' ManhattanPlot(
#'    simdata, pval_by = "P.value", chr_by = "chromosome", pos_by = "position",
#'    palette = "Set1", signif_label = FALSE)
#' }
#'
#' if (requireNamespace("ggmanh", quietly = TRUE)) {
#' # Reverse the palette and show significance threshold labels on the right
#' ManhattanPlot(
#'    simdata, pval_by = "P.value", chr_by = "chromosome", pos_by = "position",
#'    palette = "Set1", palreverse = TRUE, signif_label_pos = "right")
#' }
#'
#' if (requireNamespace("ggmanh", quietly = TRUE)) {
#' # Use chromosomes to show a single selected chromosome
#' ManhattanPlot(
#'    simdata, pval_by = "P.value", chr_by = "chromosome", pos_by = "position",
#'    title = "Simulated P.Values", chromosomes = 5)
#' }
#'
#' if (requireNamespace("ggmanh", quietly = TRUE)) {
#' # Subset and reorder chromosomes
#' ManhattanPlot(
#'    simdata, pval_by = "P.value", chr_by = "chromosome", pos_by = "position",
#'    title = "Simulated P.Values", chromosomes = c(20, 4, 6))
#' }
#'
#' tmpdata <- data.frame(
#'   "chromosome" = c(rep(5, 10), rep(21, 5)),
#'   "position" = c(sample(250000:250100, 10, replace = FALSE),
#'     sample(590000:600000, 5, replace = FALSE)),
#'   "P.value" = c(10^-(rnorm(10, 100, 3)), 10^-rnorm(5, 9, 1)),
#'   "cohort" = c(rep("A", 10), rep("B", 5))
#' )
#'
#' simdata <- rbind(simdata, tmpdata)
#' simdata$chromosome <- factor(simdata$chromosome, c(1:22, "X"))
#'
#' if (requireNamespace("ggmanh", quietly = TRUE)) {
#' # Don't rescale the plot (y-axis)
#' ManhattanPlot(
#'     simdata, pval_by = "P.value", chr_by = "chromosome", pos_by = "position",
#'     title = "Simulated P.Values - Significant", rescale = FALSE)
#' }
#'
#' if (requireNamespace("ggmanh", quietly = TRUE)) {
#' # Rescale the plot (y-axis) and put the breaking point in the middle of the y-axis
#' ManhattanPlot(
#'     simdata, pval_by = "P.value", chr_by = "chromosome", pos_by = "position",
#'     title = "Simulated P.Values - Significant", rescale = TRUE, signif_rel_pos = 0.5)
#' }
#'
#' sig <- simdata$P.value < 5e-07
#'
#' simdata$label <- ""
#' simdata$label[sig] <- sprintf("Label: %i", 1:sum(sig))
#' simdata$label2 <- ""
#' i <- (simdata$chromosome == 5) & (simdata$P.value < 5e-8)
#' simdata$label2[i] <- paste("Chromosome 5 label", 1:sum(i))
#'
#' if (requireNamespace("ggmanh", quietly = TRUE)) {
#' # Label the points with labels
#' ManhattanPlot(simdata, label_by = "label", pval_by = "P.value", chr_by = "chromosome",
#'     pos_by = "position", title = "Simulated P.Values with labels", label_size = 4)
#' }
#'
#' if (requireNamespace("ggmanh", quietly = TRUE)) {
#' # Label the points with labels and use a different color for the labels
#' ManhattanPlot(simdata, label_by = "label2", pval_by = "P.value", chr_by = "chromosome",
#'     pos_by = "position", title = "Simulated P.Values with labels",
#'     label_size = 3, label_fg = "black")
#' }
#'
#' simdata$color <- "Not Significant"
#' simdata$color[simdata$P.value <= 5e-8] <- "Significant"
#'
#' if (requireNamespace("ggmanh", quietly = TRUE)) {
#' # Highlight points with shapes
#' ManhattanPlot(simdata, title = "Highlight Points with shapes",
#'     pval_by = "P.value", chr_by = "chromosome", pos_by = "position",
#'     highlight = "color == 'Significant'", highlight_color = NULL, highlight_shape = 6,
#'     highlight_size = 5, pt_alpha = 0.2, pt_size = 1)
#' }
#'
#' if (requireNamespace("ggmanh", quietly = TRUE)) {
#' # Highlight points with colors
#' ManhattanPlot(simdata, title = "Highlight Points",
#'     pval_by = "P.value", chr_by = "chromosome", pos_by = "position",
#'     highlight = "color == 'Significant'", highlight_color = "black",
#'     pt_color = "lightblue", pt_alpha = 0.2, pt_size = 0.1)
#' }
#' }
ManhattanPlot <- function(
    data, chr_by, pos_by, pval_by, split_by = NULL, split_by_sep = "_", label_by = NULL,
    chromosomes = NULL, pt_size = 0.75, pt_color = NULL, pt_alpha = alpha, pt_shape = 19,
    label_size = 3, label_fg = NULL, highlight = NULL, highlight_color = NULL,
    highlight_size = 1.5, highlight_alpha = 1, highlight_shape = 19,
    preserve_position = TRUE, chr_gap_scaling = 1, pval_transform = "-log10",
    signif = c(5e-08, 1e-05), signif_color = NULL, signif_rel_pos = 0.2, signif_label = TRUE,
    signif_label_size = 3.5, signif_label_pos = c("left", "right"),
    thin = NULL, thin_n = 1000, thin_bins = 200, rescale = TRUE, rescale_ratio_threshold = 5,
    palette = "Dark2", palcolor = NULL, palreverse = FALSE, alpha = 1,
    theme = "theme_this", theme_args = list(), title = NULL, subtitle = NULL,
    xlab = NULL, ylab = expression("\u002d" * log[10](p)), seed = 8525,
    combine = TRUE, nrow = NULL, ncol = NULL, byrow = TRUE,
    axes = NULL, axis_titles = axes, guides = NULL, facet_by = NULL, design = NULL, ...) {

    validate_common_args(seed = seed)
    if (!is.null(facet_by)) {
        warning("[ManhattanPlot] 'facet_by' is not supported, using 'split_by' instead.")}

    theme <- process_theme(theme)
    if (is.data.frame(data)) {
        split_by <- check_columns(data, split_by, force_factor = TRUE, allow_multi = TRUE,
            concat_multi = TRUE, concat_sep = split_by_sep)
    } else if (inherits(data, "GRanges")) {
        metadata <- data@elementMetadata
        split_by <- check_columns(metadata, split_by, force_factor = TRUE, allow_multi = TRUE,
            concat_multi = TRUE, concat_sep = split_by_sep)
        data@elementMetadata <- metadata
    } else {
        stop("[ManhattanPlot] 'data' must be a data frame or a GRanges object.")
    }

    if (!is.null(split_by)) {
        data[[split_by]] <- droplevels(data[[split_by]])
        datas <- split(data, data[[split_by]])
        # keep the order of levels
        datas <- datas[levels(data[[split_by]])]
    } else {
        datas <- list(data)
        names(datas) <- "..."
    }

    palette <- check_palette(palette, names(datas))
    palcolor <- check_palcolor(palcolor, names(datas))

    plots <- lapply(
        names(datas), function(nm) {
            default_title <- if (length(datas) == 1 && identical(nm, "...")) NULL else nm
            if (is.function(title)) {
                title <- title(default_title)
            } else {
                title <- title %||% default_title
            }
            ManhattanPlotAtomic(
                data = datas[[nm]], chr_by = chr_by, pos_by = pos_by, pval_by = pval_by,
                label_by = label_by, chromosomes = chromosomes, pt_size = pt_size,
                pt_color = pt_color, pt_alpha = pt_alpha, pt_shape = pt_shape,
                label_size = label_size, label_fg = label_fg, highlight = highlight,
                highlight_color = highlight_color, highlight_size = highlight_size,
                highlight_alpha = highlight_alpha, highlight_shape = highlight_shape,
                preserve_position = preserve_position, chr_gap_scaling = chr_gap_scaling,
                pval_transform = pval_transform, signif = signif,
                signif_color = signif_color, signif_rel_pos = signif_rel_pos,
                signif_label = signif_label, signif_label_size = signif_label_size,
                signif_label_pos = signif_label_pos, thin = thin, thin_n = thin_n,
                thin_bins = thin_bins, rescale = rescale,
                rescale_ratio_threshold = rescale_ratio_threshold,
                palette = palette[[nm]], palcolor = palcolor[[nm]], palreverse = palreverse,
                alpha = alpha, theme_args = theme_args, theme = theme,
                title = title, subtitle = subtitle, xlab = xlab, ylab = ylab
            )
        }
    )

    combine_plots(plots, combine = combine, nrow = nrow, ncol = ncol, byrow = byrow,
        axes = axes, axis_titles = axis_titles, guides = guides, design = design)
}
