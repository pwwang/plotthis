#' Atomic Manhattan plot (internal)
#'
#' @description
#' Core implementation for drawing a GWAS-style Manhattan plot.  This is the
#' internal workhorse dispatched by the exported \code{\link{ManhattanPlot}}
#' function --- it takes a **single** data frame (no \code{split_by} support)
#' and returns a \code{ggplot} object.  The plot displays genetic association
#' p-values across chromosomes, with \eqn{-\log_{10}(p)} on the y-axis and
#' genomic position on the x-axis.  Each chromosome is rendered in alternating
#' colours, and configurable horizontal dashed lines mark genome-wide
#' significance thresholds.
#'
#' The function is adapted from \code{ggmanh::manhattan_plot()} with the
#' following enhancements:
#' \itemize{
#'   \item Dot-separated argument names are converted to underscores
#'   (e.g. \code{chr.colname} \eqn{\rightarrow} \code{chr_by}).
#'   \item \code{chromosomes} merges the original \code{chromosome} and
#'   \code{chr.order} arguments into a single parameter for subsetting
#'   and reordering.
#'   \item \code{highlight} accepts index vectors or R expressions (via a
#'   character string) instead of a column name.
#'   \item Dedicated \code{pt_*}, \code{label_*}, and \code{highlight_*}
#'   parameter families give granular control over point appearance,
#'   label styling, and highlight styling.
#'   \item \code{pval_transform} accepts any function (or a character
#'   string parsed as a function) rather than a fixed log-transform toggle.
#' }
#'
#' Key features include per-chromosome alternating colours, configurable
#' significance threshold lines with labels, optional data thinning for
#' dense SNP sets, automatic y-axis rescaling (broken-axis) when a small
#' number of highly significant points would otherwise compress the
#' majority of the data, and support for highlighting and labeling
#' specific variants.
#'
#' @section Architecture:
#'
#' \strong{ManhattanPlotAtomic} executes the following steps:
#' \enumerate{
#'   \item \strong{ggplot dispatch} --- selects \code{gglogger::ggplot} or
#'   \code{ggplot2::ggplot} based on
#'   \code{getOption("plotthis.gglogger.enabled")}.
#'   \item \strong{signif_label_pos normalisation} --- \code{match.arg()}
#'   resolves \code{signif_label_pos} to \code{"left"} or \code{"right"}.
#'   \item \strong{Data preprocessing} ---
#'   \code{ggmanh::manhattan_data_preprocess()} performs chromosome
#'   ordering, optional thinning (\code{thin_n} points per
#'   \code{thin_bins} horizontal partitions), chromosome gap scaling
#'   (\code{chr_gap_scaling}), position preservation
#'   (\code{preserve_position}), and significance threshold colour
#'   assignment.  When \code{chromosomes} is a single value, it is
#'   passed as the \code{chromosome} filter; otherwise it is used for
#'   ordering via \code{chr.order}.
#'   \item \strong{label_by validation} --- \code{\link{check_columns}()}
#'   validates and factors the \code{label_by} column if provided.
#'   \item \strong{Chromosome colour assignment} --- if \code{highlight}
#'   and \code{highlight_color} are both set, or if \code{pt_color} is
#'   given, \code{pt_color} (defaulting to \code{"grey80"}) is used as
#'   the base colour for all chromosomes; otherwise
#'   \code{\link{palette_this}()} generates distinct colours per
#'   chromosome from \code{palette} / \code{palcolor}.
#'   \item \strong{pval_transform resolution} --- when a character string
#'   starting with \code{"-"} (e.g. \code{"-log10"}), the minus sign is
#'   stripped, the remainder is evaluated as a function, and the result
#'   is negated.  Plain character strings are evaluated directly.  The
#'   result is applied to the p-value column to produce \code{log10pval}.
#'   \item \strong{Y-axis rescaling check} --- when \code{rescale = TRUE},
#'   the ratio of the ceiling-scaled maximum to the significance-threshold
#'   jump is checked against \code{rescale_ratio_threshold}.  If the
#'   ratio exceeds the threshold, a broken y-axis is constructed via
#'   \code{ggmanh::get_transform()} that compresses the empty space
#'   between the main data and the extreme points.
#'   \item \strong{Single-chromosome branch} --- when only one chromosome
#'   is present, the original position column is used directly, x-axis
#'   breaks and labels use \code{waiver()}, and the x-axis label is set
#'   to the chromosome name (or \code{"Chromosome <name>"}).
#'   \item \strong{Multi-chromosome branch} --- position coordinates are
#'   recalculated across chromosome boundaries via
#'   \code{ggmanh::calc_new_pos_()}.  Chromosome centre positions serve
#'   as x-axis breaks, chromosome labels are displayed, and limits span
#'   the full genomic range.
#'   \item \strong{Base plot assembly} --- creates a \code{ggplot} object
#'   with \code{geom_point()} (chromosome-coloured dots),
#'   \code{scale_color_manual()}, \code{scale_y_continuous()} (with
#'   optional broken-axis transform), \code{scale_x_continuous()},
#'   \code{geom_hline()} for significance thresholds, the resolved theme
#'   (with hidden grid lines and suppressed legend), and \code{labs()}.
#'   \item \strong{Significance labels} --- when \code{signif_label = TRUE},
#'   \code{geom_text()} annotates each significance threshold at the
#'   left or right edge of the plot (controlled by
#'   \code{signif_label_pos}), coloured by \code{signif_color} (smallest
#'   threshold in black, others in grey by default).
#'   \item \strong{Variant labels} --- when \code{label_by} is provided,
#'   \code{ggrepel::geom_label_repel()} adds labels for variants that
#'   have non-empty values in the \code{label_by} column.  If \code{label_fg}
#'   is set, all labels receive that colour; otherwise each label inherits
#'   the chromosome colour of its point.
#'   \item \strong{Rescale tick marks} --- when y-axis rescaling is
#'   active, the axis tick style is changed (via \code{axis.ticks.y})
#'   and a double-equals annotation is placed at the jump point to
#'   indicate the axis break, with \code{coord_cartesian(clip = "off")}.
#'   \item \strong{Highlight overlay} --- when \code{highlight} is
#'   numeric (row indices) or a character string (R expression), the
#'   matching points are overlaid with a separate \code{geom_point()}
#'   layer styled by \code{highlight_*} parameters, optionally in a
#'   distinct colour when \code{highlight_color} is specified.
#'   \item \strong{Dimension calculation} ---
#'   \code{\link{calculate_plot_dimensions}()} computes height and width
#'   from the number of chromosomes (\code{base_height = 4.5},
#'   \code{x_scale_factor = 0.4}), and stores them as \code{height} /
#'   \code{width} attributes on the plot.
#' }
#'
#' @inheritParams common_args
#' @param chr_by A character string specifying the column name for chromosome
#'   identifiers.  Default: \code{"chr"}.
#' @param pos_by A character string specifying the column name for genomic
#'   positions (integer or numeric).  Default: \code{"pos"}.
#' @param pval_by A character string specifying the column name for p-values
#'   (numeric).  Default: \code{"pval"}.
#' @param label_by A character string specifying the column name for variant
#'   labels.  Only variants with non-empty values in this column will be
#'   labelled.  Default: \code{NULL} (no labels).
#' @param chromosomes A character or numeric vector specifying which
#'   chromosomes to include and/or their display order.  When \code{NULL}
#'   (the default), all chromosomes present in the data are plotted in
#'   their natural factor order.  A single value filters to that
#'   chromosome; a vector reorders and subsets.
#' @param pt_size A numeric value specifying the size of the points.
#'   Default: \code{0.75}.
#' @param pt_color A character string specifying a single colour for all
#'   background (non-highlighted) points.  When \code{NULL} (the default),
#'   alternating chromosome colours from \code{palette} / \code{palcolor}
#'   are used.  Typically set to \code{"grey80"} when \code{highlight} is
#'   used with a distinct \code{highlight_color}.
#' @param pt_alpha A numeric value in \code{[0, 1]} specifying the
#'   transparency of the points.  Default: \code{alpha} (aliased
#'   parameter).
#' @param pt_shape A numeric value specifying the shape of the points.
#'   Default: \code{19} (filled circle).
#' @param label_size A numeric value specifying the font size of the
#'   variant labels.  Default: \code{3}.
#' @param label_fg A character string specifying the colour of the variant
#'   labels.  When \code{NULL} (the default), each label inherits the
#'   colour of its corresponding point.
#' @param highlight Either a numeric vector of row indices or a character
#'   string containing an R expression (parsed via
#'   \code{rlang::parse_expr()}) to select variants to highlight.
#'   Default: \code{NULL} (no highlighting).
#' @param highlight_color A character string specifying the colour of
#'   highlighted points.  When \code{NULL} (the default), highlighted
#'   points inherit the chromosome colour from the underlying
#'   \code{geom_point()} layer.
#' @param highlight_size A numeric value specifying the size of
#'   highlighted points.  Default: \code{1.5}.
#' @param highlight_alpha A numeric value in \code{[0, 1]} specifying the
#'   transparency of highlighted points.  Default: \code{1}.
#' @param highlight_shape A numeric value specifying the shape of
#'   highlighted points.  Default: \code{19} (filled circle).
#' @param preserve_position A logical value.  When \code{TRUE} (the
#'   default), the width of each chromosome segment reflects its number
#'   of variants and variant positions are correctly scaled.  When
#'   \code{FALSE}, all chromosomes have equal width and variants are
#'   equally spaced.
#' @param chr_gap_scaling A numeric scaling factor for the gap between
#'   chromosomes.  Larger values increase the gap.  Default: \code{1}.
#' @param pval_transform A function or character string that can be
#'   evaluated to a function for transforming p-values.  Default:
#'   \code{"-log10"}, which computes \eqn{-\log_{10}(p)}.  Other
#'   examples: \code{"-log2"} or a custom
#'   \code{function(x) -log10(x)}.
#' @param signif A numeric vector of significance thresholds to draw as
#'   horizontal dashed lines.  Default: \code{c(5e-8, 1e-5)}.
#' @param signif_color A character vector of colours for the significance
#'   threshold lines, of equal length as \code{signif}.  When \code{NULL}
#'   (the default), the smallest threshold is coloured black and the rest
#'   grey.
#' @param signif_rel_pos A numeric value between \code{0.1} and \code{0.9}
#'   specifying the relative position of the y-axis jump when rescaling
#'   is active.  Default: \code{0.2}.
#' @param signif_label A logical value.  When \code{TRUE} (the default),
#'   significance threshold values are annotated on the plot.
#' @param signif_label_size A numeric value for the font size of the
#'   significance threshold labels.  Default: \code{3.5}.
#' @param signif_label_pos A character string specifying where to place
#'   the significance threshold labels: \code{"left"} (default) or
#'   \code{"right"}.
#' @param thin A logical value indicating whether to thin dense data by
#'   sampling points per horizontal partition.  Defaults to \code{TRUE}
#'   when \code{chromosomes} selects fewer chromosomes than in the data,
#'   and \code{FALSE} otherwise.
#' @param thin_n An integer specifying the maximum number of points per
#'   horizontal partition after thinning.  Default: \code{1000}.
#' @param thin_bins An integer specifying the number of horizontal bins
#'   for thinning.  Default: \code{200}.
#' @param rescale A logical value.  When \code{TRUE} (the default), the
#'   y-axis is automatically rescaled (broken axis) if extreme
#'   significance values would otherwise compress the main data cloud.
#' @param rescale_ratio_threshold A numeric threshold for triggering
#'   y-axis rescaling.  The ratio is computed as
#'   \code{ceiling(max(log10pval) / 5) * 5 / signif_jump}.
#'   Default: \code{5}.
#' @keywords internal
#' @return A \code{ggplot} object with \code{height} and \code{width}
#'   attributes (in inches) attached.
#' @importFrom utils getFromNamespace
#' @importFrom rlang sym %||%
#' @importFrom ggplot2 waiver aes geom_point scale_y_continuous scale_color_manual annotate
#' @importFrom ggplot2 scale_x_continuous geom_hline element_line coord_cartesian geom_text
#' @importFrom ggrepel geom_label_repel
ManhattanPlotAtomic <- function(
    data,
    chr_by,
    pos_by,
    pval_by,
    label_by = NULL,
    chromosomes = NULL,
    pt_size = 0.75,
    pt_color = NULL,
    pt_alpha = alpha,
    pt_shape = 19,
    label_size = 3,
    label_fg = NULL,
    highlight = NULL,
    highlight_color = NULL,
    highlight_size = 1.5,
    highlight_alpha = 1,
    highlight_shape = 19,
    preserve_position = TRUE,
    chr_gap_scaling = 1,
    pval_transform = "-log10",
    signif = c(5e-08, 1e-05),
    signif_color = NULL,
    signif_rel_pos = 0.2,
    signif_label = TRUE,
    signif_label_size = 3.5,
    signif_label_pos = c("left", "right"),
    thin = NULL,
    thin_n = 1000,
    thin_bins = 200,
    rescale = TRUE,
    rescale_ratio_threshold = 5,
    palette = "Dark2",
    palcolor = NULL,
    palreverse = FALSE,
    alpha = 1,
    theme = "theme_this",
    theme_args = list(),
    title = NULL,
    subtitle = NULL,
    xlab = NULL,
    ylab = expression("\u002d" * log[10](p)),
    ...
) {
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

    if (
        (!is.null(highlight) && !is.null(highlight_color)) || !is.null(pt_color)
    ) {
        pt_color <- pt_color %||% "grey80"
        mpdata$chr.col <- rep(pt_color, length(levels(mpdata$data[[chr_by]])))
        names(mpdata$chr.col) <- levels(mpdata$data[[chr_by]])
    } else {
        mpdata$chr.col <- palette_this(
            levels(mpdata$data[[chr_by]]),
            palette = palette,
            palcolor = palcolor,
            reverse = palreverse,
            alpha = pt_alpha
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
    stopifnot(
        "[ManhattanPlot] 'pval_transform' must be a function" = is.function(
            pval_transform
        )
    )
    mpdata$data$log10pval <- pval_transform(mpdata$data[[pval_by]])
    mpdata$pval.colname <- "log10pval"

    trans <- list(trans = "identity", breaks = waiver())
    if (isTRUE(rescale)) {
        get_transform_jump <- getFromNamespace("get_transform_jump", "ggmanh")
        get_transform <- getFromNamespace("get_transform", "ggmanh")
        jump <- get_transform_jump(pval_transform(mpdata$signif))
        if (
            (ceiling(max(mpdata$data[[mpdata$pval.colname]]) / 5) * 5) / jump >
                rescale_ratio_threshold
        ) {
            trans <- get_transform(
                mpdata$data,
                jump,
                mpdata$pval.colname,
                jump.rel.pos = signif_rel_pos
            )
        }
    }

    ylimit <- c(
        0,
        ifelse(identical(trans$trans, "identity"), NA, max(trans$breaks))
    )

    if (length(unique(mpdata$data[[chr_by]])) == 1) {
        pos <- mpdata$true.pos.colname
        x_break <- waiver()
        x_break_label <- waiver()
        x_limits <- NULL
        chrname <- as.character(unique(mpdata$data[[chr_by]]))
        xlab <- xlab %||%
            ifelse(
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
        x_limits <- c(
            min(mpdata$chr.pos.info$start_pos),
            max(mpdata$chr.pos.info$end_pos)
        )
    }
    xlab <- xlab %||% "Chromosome"

    expand <- c(0.02, 0.01, 0.025, 0.01)
    if (isTRUE(signif_label)) {
        expand[1] <- 0.1
    }
    expand <- norm_expansion(expand, "continuous", "continuous")

    p <- ggplot(
        mpdata$data,
        aes(
            x = !!sym(pos),
            y = !!sym(mpdata$pval.colname),
            color = !!sym(chr_by)
        )
    ) +
        geom_point(size = pt_size) +
        scale_color_manual(values = mpdata$chr.col, guide = "none") +
        scale_y_continuous(
            trans = trans$trans,
            breaks = trans$breaks,
            expand = expand$y,
            limits = ylimit
        ) +
        scale_x_continuous(
            name = xlab,
            breaks = x_break,
            labels = x_break_label,
            expand = expand$x,
            limits = x_limits
        ) +
        geom_hline(
            yintercept = pval_transform(mpdata$signif),
            linetype = 'dashed',
            color = mpdata$signif.col
        ) +
        do_call(theme, theme_args) +
        ggplot2::theme(
            panel.grid.major = ggplot2::element_blank(),
            panel.grid.minor = ggplot2::element_blank(),
            legend.position = "none"
        ) +
        labs(x = xlab, y = ylab) +
        ggtitle(label = title, subtitle = subtitle)

    if (isTRUE(signif_label)) {
        p <- p +
            geom_text(
                data.frame(
                    x = if (signif_label_pos == "left") -Inf else Inf,
                    y = pval_transform(mpdata$signif),
                    label = mpdata$signif,
                    color = signif_color %||%
                        c("black", rep("grey80", length(mpdata$signif) - 1))
                ),
                mapping = aes(
                    x = !!sym("x"),
                    y = !!sym("y"),
                    label = !!sym("label"),
                    color = I(!!sym("color"))
                ),
                hjust = if (signif_label_pos == "left") -0.5 else 1.5,
                vjust = -0.5,
                size = signif_label_size
            )
    }

    if (!is.null(label_by)) {
        if (is.null(label_fg)) {
            p <- p +
                geom_label_repel(
                    aes(label = !!sym(label_by), color = !!sym(chr_by)),
                    size = label_size,
                    min.segment.length = 0,
                    max.overlaps = 100
                )
        } else {
            p <- p +
                geom_label_repel(
                    aes(label = !!sym(label_by)),
                    color = label_fg,
                    size = label_size,
                    min.segment.length = 0,
                    max.overlaps = 100
                )
        }
    }

    if (rescale & !identical(trans$trans, "identity")) {
        # if the plot is rescaled, change the tick at the "jump" to double line
        jump_tick_size <- 3.5

        p <- p +
            ggplot2::theme(
                axis.ticks.y = element_line(linetype = trans$y_axis_linetype)
            ) +
            annotate(
                geom = "point",
                shape = "=",
                x = -Inf,
                y = trans$jump,
                size = jump_tick_size
            ) +
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
            p <- p +
                geom_point(
                    data = hidata,
                    aes(
                        x = !!sym(pos),
                        y = !!sym(mpdata$pval.colname),
                        color = !!sym(chr_by)
                    ),
                    size = highlight_size,
                    shape = highlight_shape,
                    alpha = highlight_alpha
                )
        } else {
            p <- p +
                geom_point(
                    data = hidata,
                    aes(x = !!sym(pos), y = !!sym(mpdata$pval.colname)),
                    size = highlight_size,
                    shape = highlight_shape,
                    color = highlight_color,
                    alpha = highlight_alpha
                )
        }
    }

    n_chrom <- length(levels(mpdata$data[[chr_by]]))
    dims <- calculate_plot_dimensions(
        base_height = 4.5,
        aspect.ratio = NULL,
        n_x = n_chrom,
        x_scale_factor = 0.4,
        legend.position = "none"
    )

    attr(p, "height") <- dims$height
    attr(p, "width") <- dims$width

    p
}

#' Manhattan plot
#'
#' @description
#' Renders a publication-quality Manhattan plot for genetic association
#' results.  The y-axis displays \eqn{-\log_{10}(p)} (or a user-specified
#' transformation) of p-values, and the x-axis shows genomic positions
#' organised by chromosome.  Each chromosome is rendered in alternating
#' colours, and configurable horizontal dashed lines mark genome-wide
#' significance thresholds.
#'
#' The function is adapted from \code{ggmanh::manhattan_plot()} with
#' extended control over point appearance, variant labels, highlighting,
#' data thinning, y-axis rescaling, and \code{split_by} support for
#' creating multi-panel layouts (e.g. faceted by cohort or phenotype).
#'
#' @section split_by Workflow:
#'
#' When \code{split_by} is provided:
#' \enumerate{
#'   \item \strong{Column validation} --- \code{\link{check_columns}()}
#'   resolves \code{split_by} with \code{force_factor = TRUE},
#'   \code{allow_multi = TRUE}, and \code{concat_multi = TRUE}.  For
#'   \code{GRanges} inputs, validation is performed on the
#'   \code{@elementMetadata} slot.
#'   \item \strong{GRanges support} --- \code{data} can be a
#'   \code{data.frame} or a \code{GenomicRanges::GRanges} object.
#'   When \code{GRanges} is used, \code{split_by} is read from the
#'   metadata columns.
#'   \item \strong{Data splitting} --- drops unused \code{split_by}
#'   levels, splits \code{data} by \code{split_by} (preserving factor
#'   level order), and wraps into a named list.  When \code{split_by}
#'   is \code{NULL}, the data is wrapped as a single-element list with
#'   name \code{"..."}.
#'   \item \strong{Per-split palette / colour} ---
#'   \code{\link{check_palette}()} and \code{\link{check_palcolor}()}
#'   resolve per-split palette and colour overrides.
#'   \item \strong{Per-split title} --- when \code{title} is a function,
#'   it receives the default title (the split level name) and can return
#'   a custom string; otherwise \code{title \%||\% split_level} is used.
#'   \item \strong{Dispatch} --- each split subset is passed to
#'   \code{\link{ManhattanPlotAtomic}}.
#'   \item \strong{Combination} --- \code{\link{combine_plots}()}
#'   assembles the list of plots via \code{patchwork::wrap_plots},
#'   honouring \code{nrow} / \code{ncol} / \code{byrow} / \code{design}.
#' }
#'
#' @note \code{facet_by} is not supported by this plot type and triggers
#'   a warning if provided.  Use \code{split_by} instead to produce
#'   comparable multi-panel layouts.
#'
#' @inheritParams common_args
#' @inheritParams ManhattanPlotAtomic
#' @param split_by The column(s) to split data by and produce separate
#'   sub-plots.  Multiple columns are concatenated with
#'   \code{split_by_sep}.
#' @param split_by_sep A character string used to concatenate multiple
#'   \code{split_by} column values.  Default: \code{"_"}.
#' @param seed A numeric seed for reproducibility.  Passed to
#'   \code{\link{validate_common_args}()}.  Default: \code{8525}.
#' @param combine A logical value.  When \code{TRUE} (the default), the
#'   list of per-split plots is combined into a single \code{patchwork}
#'   object.  When \code{FALSE}, returns the raw list.
#' @param nrow,ncol,byrow Integers controlling the layout of combined
#'   plots via \code{patchwork::wrap_plots()}.  \code{byrow = TRUE}
#'   fills the layout row-wise.
#' @param axes,axis_titles Strings controlling how axes and axis titles
#'   are handled across combined plots.  Passed to
#'   \code{\link{combine_plots}()}.  See \code{?patchwork::wrap_plots}
#'   for options (\code{"keep"}, \code{"collect"}, \code{"collect_x"},
#'   \code{"collect_y"}).
#' @param guides A string controlling guide collection across combined
#'   plots.  Passed to \code{\link{combine_plots}()}.
#' @param design A custom layout specification for combined plots.
#'   Passed to \code{\link{combine_plots}()}.  When specified,
#'   \code{nrow}, \code{ncol}, and \code{byrow} are ignored.
#' @return A \code{ggplot} object (single plot, no \code{split_by}), a
#'   \code{patchwork} object (when \code{combine = TRUE} with
#'   \code{split_by}), or a named list of \code{ggplot} objects (when
#'   \code{combine = FALSE}).  Each individual plot carries
#'   \code{height} and \code{width} attributes.
#' @export
#' @examples
#' \donttest{
#' set.seed(1000)
#'
#' nsim <- 50000
#'
#' # --- Data simulation ---
#' simdata <- data.frame(
#'   "chromosome" = sample(c(1:22,"X"), size = nsim, replace = TRUE),
#'   "position" = sample(1:100000000, size = nsim),
#'   "P.value" = rbeta(nsim, shape1 = 5, shape2 = 1)^7,
#'   "cohort" = sample(c("A", "B"), size = nsim, replace = TRUE)
#' )
#' simdata$chromosome <- factor(simdata$chromosome, c(1:22, "X"))
#' options(repr.plot.width=10, repr.plot.height=5)
#'
#' # --- Basic Manhattan plot ---
#' if (requireNamespace("ggmanh", quietly = TRUE)) {
#' ManhattanPlot(
#'    simdata, pval_by = "P.value", chr_by = "chromosome", pos_by = "position",
#'    title = "Simulated P.Values", ylab = "P")
#' }
#'
#' # --- split_by ---
#' if (requireNamespace("ggmanh", quietly = TRUE)) {
#' ManhattanPlot(
#'    simdata, pval_by = "P.value", chr_by = "chromosome", pos_by = "position",
#'    title = "Simulated P.Values", ylab = "P", split_by = "cohort", ncol = 1)
#' }
#'
#' # --- Customized p-value transformation and significance threshold line colors ---
#' if (requireNamespace("ggmanh", quietly = TRUE)) {
#' ManhattanPlot(
#'    simdata, pval_by = "P.value", chr_by = "chromosome", pos_by = "position",
#'    title = "Simulated -Log2 P.Values", ylab = "-log2(P)", pval_transform = "-log2",
#'    signif_color = c("red", "blue"))
#' }
#'
#' # --- Different palette and no significance threshold labels ---
#' if (requireNamespace("ggmanh", quietly = TRUE)) {
#' ManhattanPlot(
#'    simdata, pval_by = "P.value", chr_by = "chromosome", pos_by = "position",
#'    palette = "Set1", signif_label = FALSE)
#' }
#'
#' # --- Reverse palette and label position on the right ---
#' if (requireNamespace("ggmanh", quietly = TRUE)) {
#' ManhattanPlot(
#'    simdata, pval_by = "P.value", chr_by = "chromosome", pos_by = "position",
#'    palette = "Set1", palreverse = TRUE, signif_label_pos = "right")
#' }
#'
#' # --- Single chromosome ---
#' if (requireNamespace("ggmanh", quietly = TRUE)) {
#' ManhattanPlot(
#'    simdata, pval_by = "P.value", chr_by = "chromosome", pos_by = "position",
#'    title = "Simulated P.Values", chromosomes = 5)
#' }
#'
#' # --- Chromosome subset and reorder ---
#' if (requireNamespace("ggmanh", quietly = TRUE)) {
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
#' # --- Disable y-axis rescaling ---
#' if (requireNamespace("ggmanh", quietly = TRUE)) {
#' ManhattanPlot(
#'     simdata, pval_by = "P.value", chr_by = "chromosome", pos_by = "position",
#'     title = "Simulated P.Values - Significant", rescale = FALSE)
#' }
#'
#' # --- Y-axis rescaling with custom break position ---
#' if (requireNamespace("ggmanh", quietly = TRUE)) {
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
#' # --- Variant labels ---
#' if (requireNamespace("ggmanh", quietly = TRUE)) {
#' ManhattanPlot(simdata, label_by = "label", pval_by = "P.value", chr_by = "chromosome",
#'     pos_by = "position", title = "Simulated P.Values with labels", label_size = 4)
#' }
#'
#' # --- Variant labels with custom color ---
#' if (requireNamespace("ggmanh", quietly = TRUE)) {
#' ManhattanPlot(simdata, label_by = "label2", pval_by = "P.value", chr_by = "chromosome",
#'     pos_by = "position", title = "Simulated P.Values with labels",
#'     label_size = 3, label_fg = "black")
#' }
#'
#' simdata$color <- "Not Significant"
#' simdata$color[simdata$P.value <= 5e-8] <- "Significant"
#'
#' # --- Highlight points with custom shape ---
#' if (requireNamespace("ggmanh", quietly = TRUE)) {
#' ManhattanPlot(simdata, title = "Highlight Points with shapes",
#'     pval_by = "P.value", chr_by = "chromosome", pos_by = "position",
#'     highlight = "color == 'Significant'", highlight_color = NULL, highlight_shape = 6,
#'     highlight_size = 5, pt_alpha = 0.2, pt_size = 1)
#' }
#'
#' # --- Highlight points with custom color ---
#' if (requireNamespace("ggmanh", quietly = TRUE)) {
#' ManhattanPlot(simdata, title = "Highlight Points",
#'     pval_by = "P.value", chr_by = "chromosome", pos_by = "position",
#'     highlight = "color == 'Significant'", highlight_color = "black",
#'     pt_color = "lightblue", pt_alpha = 0.2, pt_size = 0.1)
#' }
#' }
ManhattanPlot <- function(
    data,
    chr_by,
    pos_by,
    pval_by,
    split_by = NULL,
    split_by_sep = "_",
    label_by = NULL,
    chromosomes = NULL,
    pt_size = 0.75,
    pt_color = NULL,
    pt_alpha = alpha,
    pt_shape = 19,
    label_size = 3,
    label_fg = NULL,
    highlight = NULL,
    highlight_color = NULL,
    highlight_size = 1.5,
    highlight_alpha = 1,
    highlight_shape = 19,
    preserve_position = TRUE,
    chr_gap_scaling = 1,
    pval_transform = "-log10",
    signif = c(5e-08, 1e-05),
    signif_color = NULL,
    signif_rel_pos = 0.2,
    signif_label = TRUE,
    signif_label_size = 3.5,
    signif_label_pos = c("left", "right"),
    thin = NULL,
    thin_n = 1000,
    thin_bins = 200,
    rescale = TRUE,
    rescale_ratio_threshold = 5,
    palette = "Dark2",
    palcolor = NULL,
    palreverse = FALSE,
    alpha = 1,
    theme = "theme_this",
    theme_args = list(),
    title = NULL,
    subtitle = NULL,
    xlab = NULL,
    ylab = expression("\u002d" * log[10](p)),
    seed = 8525,
    combine = TRUE,
    nrow = NULL,
    ncol = NULL,
    byrow = TRUE,
    axes = NULL,
    axis_titles = axes,
    guides = NULL,
    facet_by = NULL,
    design = NULL,
    ...
) {
    validate_common_args(seed = seed)
    if (!is.null(facet_by)) {
        warning(
            "[ManhattanPlot] 'facet_by' is not supported, using 'split_by' instead."
        )
    }

    theme <- process_theme(theme)
    if (is.data.frame(data)) {
        split_by <- check_columns(
            data,
            split_by,
            force_factor = TRUE,
            allow_multi = TRUE,
            concat_multi = TRUE,
            concat_sep = split_by_sep
        )
    } else if (inherits(data, "GRanges")) {
        metadata <- data@elementMetadata
        split_by <- check_columns(
            metadata,
            split_by,
            force_factor = TRUE,
            allow_multi = TRUE,
            concat_multi = TRUE,
            concat_sep = split_by_sep
        )
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
            ManhattanPlotAtomic(
                data = datas[[nm]],
                chr_by = chr_by,
                pos_by = pos_by,
                pval_by = pval_by,
                label_by = label_by,
                chromosomes = chromosomes,
                pt_size = pt_size,
                pt_color = pt_color,
                pt_alpha = pt_alpha,
                pt_shape = pt_shape,
                label_size = label_size,
                label_fg = label_fg,
                highlight = highlight,
                highlight_color = highlight_color,
                highlight_size = highlight_size,
                highlight_alpha = highlight_alpha,
                highlight_shape = highlight_shape,
                preserve_position = preserve_position,
                chr_gap_scaling = chr_gap_scaling,
                pval_transform = pval_transform,
                signif = signif,
                signif_color = signif_color,
                signif_rel_pos = signif_rel_pos,
                signif_label = signif_label,
                signif_label_size = signif_label_size,
                signif_label_pos = signif_label_pos,
                thin = thin,
                thin_n = thin_n,
                thin_bins = thin_bins,
                rescale = rescale,
                rescale_ratio_threshold = rescale_ratio_threshold,
                palette = palette[[nm]],
                palcolor = palcolor[[nm]],
                palreverse = palreverse,
                alpha = alpha,
                theme_args = theme_args,
                theme = theme,
                title = title,
                subtitle = subtitle,
                xlab = xlab,
                ylab = ylab
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
