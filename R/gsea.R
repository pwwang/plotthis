#' Get the running enrichment score of a gene set
#'
#' @param genes A vector of genes
#' @param gene_ranks A numeric vector of gene ranks with names
#' @param exponent A numeric value to raise the gene ranks to
#' @param hits_only A logical value to return only the running enrichment score of the hits
#' @return A numeric vector of the running enrichment score
#' @keywords internal
gsea_running_score <- function(
    genes,
    gene_ranks,
    exponent = 1,
    hits_only = TRUE
) {
    genes <- intersect(genes, names(gene_ranks))
    N <- length(gene_ranks)
    Nh <- length(genes)
    Phit <- Pmiss <- numeric(N)
    hits <- names(gene_ranks) %in% genes
    Phit[hits] <- abs(gene_ranks[hits])^exponent
    NR <- sum(Phit)
    if (NR == 0) {
        NR <- 1e-3
    }
    Phit <- cumsum(Phit) / NR
    Pmiss[!hits] <- 1 / (N - Nh)
    Pmiss <- cumsum(Pmiss)
    runningScore <- Phit - Pmiss
    if (hits_only) {
        runningScore <- runningScore[hits]
        names(runningScore) <- names(gene_ranks)[hits]
        runningScore[genes]
    } else {
        names(runningScore) <- names(gene_ranks)
        runningScore
    }
}

#' Prepare fgsea result for plotting
#'
#' @param data A data frame of fgsea results
#' @return A data frame with the desired columns for plotting and the gene ranks and gene sets as attributes
#' @keywords internal
prepare_fgsea_result <- function(data) {
    data$ID <- data$pathway
    data$Description <- data$pathway
    data$pathway <- NULL
    data$pvalue <- data$pval
    data$pval <- NULL
    data$p.adjust <- data$padj
    data$p.adjust[is.na(data$p.adjust)] <- 1
    data$padj <- NULL
    if (is.character(data$leadingEdge)) {
        data$leadingEdge <- strsplit(data$leadingEdge, ",")
    }
    data$core_enrichment <- sapply(data$leadingEdge, paste0, collapse = "/")
    data$leadingEdge <- NULL
    data$NES[is.na(data$NES)] <- 0

    data
}

#' GSEA summary dot plot
#'
#' @description
#' Produces a summary dot plot of GSEA (Gene Set Enrichment Analysis) results.
#' Each row represents a gene set (term), positioned along the x-axis by its
#' Normalized Enrichment Score (NES). Dot colour encodes the significance level
#' (typically \code{-log10(p.adjust)}) on a continuous gradient, and each row
#' includes a miniature line plot showing the gene ranks or running enrichment
#' score for that term's gene set.
#'
#' The function supports both \code{DOSE} and \code{fgsea} package output
#' formats via the \code{in_form} parameter. Terms can be ranked and selected
#' by a significance metric (\code{top_term}, \code{metric}), with
#' non-significant terms rendered in grey. The per-term line plots can show
#' either the raw preranked gene statistics (\code{line_by = "prerank"}) or
#' the running enrichment score (\code{line_by = "running_score"}).
#'
#' @rdname gsea
#' @inheritParams common_args
#' @param in_form The format of the input data. Options:
#'  \itemize{
#'    \item{\code{"auto"} (default): Automatically detect the format. If the
#'      data contains a column named \code{"leadingEdge"}, it is treated as
#'      \code{"fgsea"}; if it contains \code{"core_enrichment"}, it is treated
#'      as \code{"dose"}.}
#'    \item{\code{"fgsea"}: The input data is from the \code{fgsea} package.
#'      The \code{prepare_fgsea_result()} helper renames columns internally.}
#'    \item{\code{"dose"}: The input data is from the \code{DOSE} package.}
#'  }
#' @param gene_ranks A named numeric vector of gene-level rank statistics, with
#'  gene identifiers as names. Used to construct the per-term line plots.
#'  If a character string starting with \code{"@"}, the attribute of \code{data}
#'  with that name (minus the \code{"@"}) is used as the gene ranks vector.
#' @param gene_sets A named list of gene sets. Each name must correspond to an
#'  \code{ID} in \code{data}, and each element is a character vector of gene
#'  identifiers. Typically read from a GMT file. If a character string starting
#'  with \code{"@"}, the attribute of \code{data} with that name (minus the
#'  \code{"@"}) is used.
#' @param top_term Integer specifying the number of top terms to display, ranked
#'  by \code{metric}. If \code{NULL}, all terms are shown.
#' @param metric Character string specifying the column name used to rank terms
#'  and assess significance. Typically \code{"p.adjust"} or \code{"pvalue"}.
#'  Terms are ranked by this column (ascending, lower is better) when
#'  \code{top_term} is set. The same column is transformed to
#'  \code{-log10(metric)} for the colour gradient.
#' @param cutoff Numeric threshold for the \code{metric} column. Terms with
#'  values below this cutoff are coloured on a gradient; terms above are drawn
#'  in grey (\code{"grey80"}) and labelled as insignificant via
#'  \code{nonsig_name}. Default is \code{0.05}. If \code{NULL}, all terms are
#'  treated as significant.
#' @param character_width Integer specifying the maximum character width for
#'  wrapping term descriptions on the y-axis. Default is \code{50}.
#' @param line_plot_size Numeric controlling the size of the per-term miniature
#'  enrichment plots embedded in each row. Expressed as a fraction of the plot
#'  panel dimensions. Default is \code{0.25}.
#' @param metric_name Character string for the colour bar legend title.
#'  Defaults to the value of \code{metric}.
#' @param nonsig_name Character string for the legend entry label used for
#'  non-significant terms. Default is \code{"Insignificant"}.
#' @param linewidth Numeric specifying the line width within the per-term
#'  miniature enrichment plots. Default is \code{0.2}.
#' @param line_by The method used to compute the per-term line plots:
#'  \itemize{
#'    \item{\code{"prerank"} (default): Use the gene ranks as the bar heights
#'      (raw ranking metric).}
#'    \item{\code{"running_score"}: Use the running enrichment score computed
#'      by \code{gsea_running_score()}.}
#'  }
#' @importFrom scales pretty_breaks scientific
#' @importFrom ggplot2 geom_linerange layer_scales theme_void ylim
#' @export
#' @return A \code{ggplot} object with \code{height} and \code{width}
#'  attributes (in inches) attached.
#' @examples
#' \donttest{
#' data(gsea_example)
#'
#' # Default summary dot plot with preranked gene statistics
#' GSEASummaryPlot(gsea_example)
#'
#' # Use running enrichment score for per-term line plots
#' GSEASummaryPlot(gsea_example, line_by = "running_score")
#'
#' # Raise the significance cutoff (all terms are coloured)
#' GSEASummaryPlot(gsea_example, cutoff = 0.01)
#' }
GSEASummaryPlot <- function(
    data,
    in_form = c("auto", "dose", "fgsea"),
    gene_ranks = "@gene_ranks",
    gene_sets = "@gene_sets",
    top_term = 10,
    metric = "p.adjust",
    cutoff = 0.05,
    character_width = 50,
    line_plot_size = 0.25,
    metric_name = metric,
    nonsig_name = "Insignificant",
    linewidth = 0.2,
    line_by = c("prerank", "running_score"),
    title = NULL,
    subtitle = NULL,
    xlab = NULL,
    ylab = NULL,
    alpha = 0.6,
    aspect.ratio = 1,
    legend.position = "right",
    legend.direction = "vertical",
    theme = "theme_this",
    theme_args = list(),
    palette = "Spectral",
    palcolor = NULL,
    palreverse = FALSE,
    seed = 8525,
    ...
) {
    set.seed(seed)
    in_form <- match.arg(in_form)
    theme <- process_theme(theme)
    ggplot <- if (getOption("plotthis.gglogger.enabled", FALSE)) {
        gglogger::ggplot
    } else {
        ggplot2::ggplot
    }
    line_by <- match.arg(line_by)
    if (inherits(data, "gseaResult")) {
        data <- as.data.frame(data)
    }

    if (
        is.character(gene_ranks) &&
            length(gene_ranks) == 1 &&
            startsWith(gene_ranks, "@")
    ) {
        gene_ranks <- attr(data, substring(gene_ranks, 2))
    }
    if (is.null(gene_ranks)) {
        stop("'gene_ranks' must be provided")
    }
    if (is.null(names(gene_ranks))) {
        stop("'gene_ranks' must have names")
    }
    if (!is.numeric(gene_ranks)) {
        stop("'gene_ranks' must be numeric")
    }
    gene_ranks <- gene_ranks[order(-gene_ranks)]

    if (
        is.character(gene_sets) &&
            length(gene_sets) == 1 &&
            startsWith(gene_sets, "@")
    ) {
        gene_sets <- attr(data, substring(gene_sets, 2))
    }
    if (is.null(gene_sets)) {
        stop("'gene_sets' must be provided")
    }
    if (!is.list(gene_sets)) {
        stop("'gene_sets' must be a list")
    }

    if (in_form == "auto") {
        if ("leadingEdge" %in% colnames(data)) {
            in_form <- "fgsea"
        } else if ("core_enrichment" %in% colnames(data)) {
            in_form <- "dose"
        } else {
            stop(
                "Cannot detect the input format. Please set 'in_form' to 'fgsea' or 'dose'."
            )
        }
    }
    if (in_form == "fgsea") {
        data <- prepare_fgsea_result(data)
    }
    if (!is.null(top_term)) {
        data <- slice_min(data, !!sym(metric), n = top_term, with_ties = FALSE)
    }
    data$ID <- factor(data$ID, levels = rev(unique(data$ID)))
    data <- data[order(data$ID), , drop = FALSE]

    if (!is.null(cutoff)) {
        # data <- data[data[[metric]] < cutoff, , drop = FALSE]
        data$.signif <- data[[metric]] < cutoff
    } else {
        data$.signif <- TRUE
    }
    data$metric <- -log10(data[[metric]])
    check_columns(data, "Description", force_factor = TRUE)
    data$Description <- droplevels(data$Description)
    # data <- data[order(data$Description), , drop = FALSE]
    data$Description <- str_wrap(data$Description, width = character_width)
    data$Description <- factor(
        data$Description,
        levels = unique(data$Description)
    )
    data$y <- as.integer(data$Description)
    sig_metrics <- data$metric[data$.signif]

    if (all(data$.signif)) {
        p <- ggplot(data, aes(x = !!sym("NES"), y = !!sym("y")))
    } else {
        p <- ggplot(data, aes(x = !!sym("NES"), y = !!sym("y"), fill = "")) +
            guides(
                fill = guide_legend(
                    title = nonsig_name %||% "Insignificant",
                    override.aes = list(color = "grey80", shape = 15, size = 4),
                    order = 2
                )
            )
    }
    # need a layer to get the scales of the plot
    p <- p + geom_point(aes(color = !!sym("metric")), size = 0)
    x_range <- diff(layer_scales(p)$x$range$range)
    y_range <- diff(layer_scales(p)$y$range$range)
    colors <- palette_this(
        sig_metrics,
        n = sum(data$.signif),
        palette = palette,
        palcolor = palcolor,
        alpha = alpha,
        type = "continuous",
        transparent = FALSE,
        reverse = palreverse
    )
    line_plot_list <- list()

    for (i in seq_len(nrow(data))) {
        if (isTRUE(data$.signif[i])) {
            # since the values are continuous, number of colors is not equal to number of points
            color <- colors[ceiling(
                data$metric[i] * length(colors) / max(sig_metrics)
            )]
        } else {
            color <- "grey80"
        }
        hits <- intersect(
            gene_sets[[as.character(data$ID[i])]],
            names(gene_ranks)
        )
        if (line_by == "running_score") {
            scores <- gsea_running_score(hits, gene_ranks)
        } else {
            scores <- gene_ranks[match(hits, names(gene_ranks))]
        }

        df <- data.frame(
            x = factor(hits, levels = names(gene_ranks)),
            y = 0,
            ymin = pmin(scores, 0),
            ymax = pmax(scores, 0)
        )
        yr <- max(max(df$ymax), abs(min(df$ymin))) * 1.05
        x_min <- -0.05 * length(gene_ranks)
        x_max <- 1.05 * length(gene_ranks)

        lp <- ggplot(df, aes(x = !!sym("x"), y = !!sym("y"))) +
            geom_rect(
                xmin = x_min,
                xmax = x_max,
                ymin = -yr,
                ymax = yr,
                fill = color
            ) +
            geom_linerange(
                aes(ymin = !!sym("ymin"), ymax = !!sym("ymax")),
                linewidth = linewidth
            ) +
            ylim(-yr, yr) +
            theme_void() +
            ggplot2::theme(
                axis.text = element_blank(),
                axis.title = element_blank(),
                axis.ticks = element_blank(),
                legend.position = "none"
            )

        line_plot_list[[i]] <- annotation_custom(
            ggplotGrob(lp),
            xmin = data$NES[i] - x_range * line_plot_size / 2,
            xmax = data$NES[i] + x_range * line_plot_size / 2,
            ymin = i - y_range * line_plot_size / 8,
            ymax = i + y_range * line_plot_size / 8
        )
    }

    if (any(data$NES > 0) && any(data$NES < 0)) {
        p <- p + geom_vline(xintercept = 0, linetype = 2, color = "grey80")
    }

    p <- p + line_plot_list
    if (length(colors) == 0) {
        # in case all terms are not significant
        p <- p + scale_color_gradientn(colors = "grey80", guide = "none")
    } else {
        p <- p +
            scale_color_gradientn(
                name = metric_name,
                colors = colors,
                breaks = pretty_breaks(n = 4),
                labels = function(x) scientific(10^(-x), digits = 2),
                guide = guide_colorbar(
                    frame.colour = "black",
                    ticks.colour = "black",
                    title.hjust = 0,
                    order = 1
                )
            )
    }
    p <- p +
        scale_y_continuous(
            breaks = seq_len(nrow(data)),
            labels = data$Description
        ) +
        scale_x_continuous(expand = c(0.05, x_range * line_plot_size / 2)) +
        labs(
            title = title,
            subtitle = subtitle,
            x = xlab %||% "NES",
            y = ylab %||% ""
        ) +
        do_call(theme, theme_args) +
        ggplot2::theme(
            aspect.ratio = aspect.ratio,
            legend.position = legend.position,
            legend.direction = legend.direction,
            panel.grid.major.y = element_line(colour = "grey80", linetype = 2),
        )

    max_nchar_y <- min(max(nchar(levels(data$Description))), character_width)
    dims <- calculate_plot_dimensions(
        base_height = nrow(data) * 0.65,
        aspect.ratio = aspect.ratio,
        legend.position = legend.position,
        legend.direction = legend.direction
    )
    attr(p, "height") <- dims$height
    attr(p, "width") <- dims$width + max_nchar_y * 0.1

    p
}

#' Atomic GSEA plot (internal)
#'
#' @description
#' Core implementation for drawing a single GSEA (Gene Set Enrichment Analysis)
#' ridge plot. This is the workhorse behind the exported \code{\link{GSEAPlot}}
#' function — it takes a single gene set and produces a three-panel figure
#' showing the running enrichment score, hit positions, and the ranked list
#' metric.
#'
#' The plot consists of three vertically stacked panels:
#' \enumerate{
#'   \item \strong{Running score panel} — traces the running enrichment score
#'   across the ranked gene list, with a peak annotation and optional gene
#'   labels for core enrichment genes.
#'   \item \strong{Hit indicator panel} — marks the positions of gene set
#'   members with vertical lines, colour-coded by the rank metric value
#'   (red for positive, blue for negative).
#'   \item \strong{Gene ranking panel} — shows every gene's rank as a vertical
#'   segment, with annotations indicating positively and negatively correlated
#'   tails and the zero-crossing point.
#' }
#'
#' The running enrichment score is computed via \code{\link{gsea_running_score}()}
#' using the \code{gene_ranks} and \code{genes} provided. Core enrichment genes
#' (from the \code{core_enrichment} column) can be labelled on the score panel
#' using \code{\link[ggrepel]{geom_text_repel}()}.
#'
#' @section Architecture:
#' \enumerate{
#'   \item \strong{ggplot dispatch} — selects \code{gglogger::ggplot} or
#'   \code{ggplot2::ggplot} based on
#'   \code{getOption("plotthis.gglogger.enabled")}.
#'   \item \strong{Data subsetting} — if \code{data} contains multiple gene
#'   sets (\code{nrow > 1}), it is filtered to the single row matching
#'   \code{gs}. An error is raised if no rows match.
#'   \item \strong{Gene rank resolution} — resolves \code{gene_ranks} from its
#'   \code{"@"}-prefixed attribute reference, validates that it is a named
#'   numeric vector, and sorts it in descending order.
#'   \item \strong{Title and subtitle construction} — \code{title} defaults to
#'   \code{data$Description}. \code{subtitle} is built from the NES value and
#'   the \code{metric} column with significance stars (\code{ns/*/**/***/****}).
#'   \item \strong{Plot data preparation} — builds a data frame with position
#'   (1:N), gene names, running enrichment score (via
#'   \code{gsea_running_score()} with \code{hits_only = FALSE}), raw ranks,
#'   and a hit indicator column.
#'   \item \strong{Running score panel} — renders the top panel with a red/blue
#'   background (positive/negative score regions), a horizontal baseline at
#'   \code{y = 0}, the running score line, and annotations for the peak
#'   (dashed reference lines and an upward/downward triangle). Title and
#'   subtitle are added via \code{\link[ggplot2]{ggtitle}()}.
#'   \item \strong{Gene labeling} — when \code{n_coregenes > 1} or
#'   \code{genes_label} is provided, core enrichment genes are extracted from
#'   \code{data$core_enrichment} and labelled on the running score panel using
#'   \code{geom_text_repel()}. Missing genes trigger a warning.
#'   \item \strong{Hit indicator panel} — renders the middle panel: vertical
#'   lines (\code{geom_linerange}) at each hit position, with coloured
#'   rectangles beneath showing the rank metric gradient (red for positive,
#'   blue for negative).
#'   \item \strong{Gene ranking panel} — renders the bottom panel: vertical
#'   segments (\code{geom_segment}) for every gene's rank, with annotations
#'   for positively/negatively correlated tails. If both signs exist, the
#'   zero-crossing point is marked with a dashed vertical line.
#'   \item \strong{Panel assembly} — the three panels are stacked vertically
#'   via \code{\link[patchwork]{wrap_plots}()} with spacer panels and
#'   proportional heights (\code{3.5 / 1 / 1.5}). When \code{ylab} is provided,
#'   a rotated \code{\link[grid]{textGrob}()} is added to the right side and
#'   the plot width increases from 7.5 to 8 inches.
#'   \item \strong{Dimensions} — height is fixed at 6.5 inches. Width is 7.5
#'   inches (without \code{ylab}) or 8 inches (with \code{ylab}). Attributes
#'   \code{height} and \code{width} are stored on the returned object.
#' }
#'
#' @inheritParams common_args
#' @param gene_ranks A named numeric vector of gene-level rank statistics, with
#'  gene identifiers as names. Used to calculate the running enrichment score
#'  and the ranked list metric. If a character string starting with
#'  \code{"@"}, the attribute of \code{data} with that name (minus the
#'  \code{"@"}) is used.
#' @param gs Character string specifying the \code{ID} of the gene set to plot.
#'  Must match a value in \code{data$ID}.
#' @param genes Character vector of gene identifiers belonging to the gene set.
#'  These are intersected with \code{names(gene_ranks)} for the running score
#'  calculation.
#' @param metric Character string specifying the column name in \code{data}
#'  used to compute significance stars in the subtitle. Default is
#'  \code{"p.adjust"}.
#' @param sample_coregenes Logical; if \code{TRUE}, core enrichment genes are
#'  sampled randomly for labelling. If \code{FALSE} (default), the first
#'  \code{n_coregenes} core enrichment genes are used.
#' @param line_width Numeric specifying the line width for the running
#'  enrichment score curve. Default is \code{1.5}.
#' @param line_alpha Numeric alpha transparency for the running score line and
#'  hit indicator bars. Default is \code{1}.
#' @param line_color Character string specifying the colour of the running
#'  enrichment score line. Default is \code{"#6BB82D"}.
#' @param n_coregenes Integer specifying the number of core enrichment genes to
#'  label on the running score plot. Default is \code{10}. Ignored when
#'  \code{genes_label} is provided.
#' @param genes_label Character vector of specific gene names to label on the
#'  running score plot. When provided, \code{n_coregenes} is ignored.
#' @param label_fg Character string specifying the text colour of gene labels.
#'  Default is \code{"black"}.
#' @param label_bg Character string specifying the background colour of gene
#'  labels. Default is \code{"white"}.
#' @param label_bg_r Numeric specifying the corner radius of the label
#'  background. Default is \code{0.1}.
#' @param label_size Numeric specifying the font size of the label text.
#'  Default is \code{4}.
#' @param ylab Character string for a right-side y-axis label (rotated 90
#'  degrees). When provided, an extra text grob is added to the right of the
#'  assembled plot, and the plot width increases from 7.5 to 8 inches.
#' @keywords internal
#' @return A \code{patchwork} object with \code{height} and \code{width}
#'  attributes (in inches) attached.
#' @importFrom scales alpha
#' @importFrom ggplot2 ggtitle theme_classic annotate
#' @importFrom patchwork plot_layout wrap_plots plot_spacer
GSEAPlotAtomic <- function(
    data,
    gene_ranks = "@gene_ranks",
    gs,
    genes,
    metric = "p.adjust",
    sample_coregenes = FALSE,
    line_width = 1.5,
    line_alpha = 1,
    line_color = "#6BB82D",
    n_coregenes = 10,
    genes_label = NULL,
    label_fg = "black",
    label_bg = "white",
    label_bg_r = 0.1,
    label_size = 4,
    title = NULL,
    subtitle = NULL,
    xlab = NULL,
    ylab = NULL,
    ...
) {
    ggplot <- if (getOption("plotthis.gglogger.enabled", FALSE)) {
        gglogger::ggplot
    } else {
        ggplot2::ggplot
    }
    if (nrow(data) > 1) {
        data <- data[data$ID == gs, , drop = FALSE]
    }
    if (nrow(data) == 0) {
        stop("Gene set ", gs, " is not in the data")
    }
    if (
        is.character(gene_ranks) &&
            length(gene_ranks) == 1 &&
            startsWith(gene_ranks, "@")
    ) {
        gene_ranks <- attr(data, substring(gene_ranks, 2))
    }
    if (is.null(gene_ranks)) {
        stop("'gene_ranks' must be provided")
    }
    if (is.null(names(gene_ranks))) {
        stop("'gene_ranks' must have names")
    }
    if (!is.numeric(gene_ranks)) {
        stop("'gene_ranks' must be numeric")
    }
    gene_ranks <- gene_ranks[order(-gene_ranks)]

    title <- title %||% data$Description
    sig <- case_when(
        data[[metric]] > 0.05 ~ "ns",
        data[[metric]] <= 0.05 & data[[metric]] > 0.01 ~ "*",
        data[[metric]] <= 0.01 & data[[metric]] > 0.001 ~ "**",
        data[[metric]] <= 0.001 & data[[metric]] > 0.0001 ~ "***",
        data[[metric]] <= 0.0001 ~ "****"
    )
    subtitle <- subtitle %||%
        paste0(
            "(NES=",
            round(data$NES, 3),
            ", ",
            metric,
            "=",
            format(data[[metric]], digits = 3, scientific = TRUE),
            ", ",
            sig,
            ")"
        )

    df <- data.frame(
        x = 1:length(gene_ranks),
        genes = names(gene_ranks),
        runningScore = gsea_running_score(genes, gene_ranks, hits_only = FALSE),
        ranks = gene_ranks,
        position = as.integer(names(gene_ranks) %in% genes)
    )

    index_max <- which.max(abs(df$runningScore))
    p <- ggplot(df, aes(x = !!sym("x"))) +
        ggplot2::xlab(NULL) +
        theme_classic(base_size = 12) +
        ggplot2::theme(
            panel.grid.major = element_line(colour = "grey90", linetype = 2),
            panel.grid.minor = element_line(colour = "grey90", linetype = 2)
        ) +
        scale_x_continuous(expand = c(0.01, 0))

    ############# The Running Score Panel #############
    p1 <- p +
        # background
        geom_rect(
            data = data.frame(
                xmin = -Inf,
                xmax = Inf,
                ymin = c(0, -Inf),
                ymax = c(Inf, 0),
                fill = c(alpha("#C40003", 0.2), alpha("#1D008F", 0.2))
            ),
            mapping = aes(
                xmin = !!sym("xmin"),
                xmax = !!sym("xmax"),
                ymin = !!sym("ymin"),
                ymax = !!sym("ymax"),
                fill = I(!!sym("fill"))
            ),
            inherit.aes = FALSE
        ) +
        geom_hline(yintercept = 0, linetype = 1, color = "grey40") +
        # running score
        geom_line(
            aes(y = !!sym("runningScore")),
            color = line_color,
            linewidth = line_width,
            alpha = line_alpha
        ) +
        annotate(
            geom = "segment",
            x = 0,
            xend = df$x[index_max],
            y = df$runningScore[index_max],
            yend = df$runningScore[index_max],
            linetype = 2
        ) +
        annotate(
            geom = "segment",
            x = df$x[index_max],
            xend = df$x[index_max],
            y = 0,
            yend = df$runningScore[index_max],
            linetype = 2
        ) +
        annotate(
            geom = "point",
            x = df$x[index_max],
            y = df$runningScore[index_max],
            fill = ifelse(data$NES < 0, "#5E34F5", "#F52323"),
            color = "black",
            size = 2.5,
            shape = ifelse(data$NES < 0, 25, 24)
        ) +
        ggplot2::ylab("Enrichment Score") +
        ggplot2::theme(
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.line = element_blank(),
            panel.border = element_rect(
                color = "black",
                fill = "transparent",
                linewidth = 1
            ),
            plot.margin = margin(t = 0.2, r = 0.2, b = 0, l = 0.2, unit = "cm"),
            legend.position = "none",
            plot.subtitle = element_text(face = "italic")
        ) +
        ggtitle(title, subtitle)

    # label the genes
    if (
        (is.numeric(n_coregenes) && n_coregenes > 1) || length(genes_label) > 0
    ) {
        if (length(genes_label) == 0) {
            genes_label_tmp <- unlist(strsplit(data$core_enrichment, "/"))
            n_coregenes <- min(n_coregenes, length(genes_label_tmp))
            if (isTRUE(sample_coregenes)) {
                genes_label_tmp <- sample(
                    genes_label_tmp,
                    n_coregenes,
                    replace = FALSE
                )
            } else {
                genes_label_tmp <- df$genes[df$genes %in% genes_label_tmp][
                    1:n_coregenes
                ]
            }
        } else {
            genes_label_tmp <- genes_label
        }
        df_gene <- df[
            df$position == 1 & df$genes %in% genes_label_tmp,
            ,
            drop = FALSE
        ]
        gene_drop <- genes_label_tmp[!genes_label_tmp %in% df_gene$genes]
        if (length(gene_drop) > 0) {
            if (identical(data$ID, data$Description)) {
                warning(
                    "Gene ",
                    paste(gene_drop, collapse = ","),
                    " is not in the geneset ",
                    data$ID,
                    immediate. = TRUE
                )
            } else {
                warning(
                    "Gene ",
                    paste(gene_drop, collapse = ","),
                    " is not in the geneset ",
                    data$ID,
                    ": ",
                    data$Description,
                    immediate. = TRUE
                )
            }
        }
        x_nudge <- diff(range(df$x)) * 0.05
        y_nudge <- diff(range(df$runningScore)) * 0.05
        p1 <- p1 +
            geom_point(
                data = df_gene,
                mapping = aes(y = !!sym("runningScore")),
                color = "black"
            ) +
            geom_text_repel(
                data = df_gene,
                mapping = aes(
                    y = !!sym("runningScore"),
                    label = !!sym("genes")
                ),
                min.segment.length = 0,
                max.overlaps = 100,
                segment.colour = "grey40",
                color = label_fg,
                bg.color = label_bg,
                bg.r = label_bg_r,
                size = label_size,
                nudge_x = ifelse(df_gene$runningScore >= 0, x_nudge, -x_nudge),
                nudge_y = ifelse(df_gene$runningScore > 0, -y_nudge, y_nudge)
            )
    }

    ############# The Line Plot Panel #############
    p2 <- ggplot(df, aes(x = !!sym("x"))) +
        geom_linerange(
            aes(ymax = !!sym("position")),
            ymin = 0,
            alpha = line_alpha
        ) +
        ggplot2::xlab(NULL) +
        ggplot2::ylab(NULL) +
        theme_classic(base_size = 12) +
        ggplot2::theme(
            legend.position = "none",
            plot.margin = margin(
                t = -0.1,
                b = 0,
                r = 0.2,
                l = 0.2,
                unit = "cm"
            ),
            panel.border = element_rect(
                color = "black",
                fill = "transparent",
                linewidth = 1
            ),
            axis.line.y = element_blank(),
            axis.line.x = element_blank(),
            axis.ticks = element_blank(),
            axis.text = element_blank()
        ) +
        scale_x_continuous(expand = c(0.01, 0)) +
        scale_y_continuous(expand = c(0, 0))

    x <- df$x
    y <- y_raw <- df$ranks
    y[y > quantile(y_raw, 0.98)] <- quantile(y_raw, 0.98)
    y[y < quantile(y_raw, 0.02)] <- quantile(y_raw, 0.02)
    col <- rep("white", length(y))
    y_pos <- which(y > 0)
    if (length(y_pos) > 0) {
        y_pos_i <- cut(
            y[y_pos],
            breaks = seq(
                min(y[y_pos], na.rm = TRUE),
                max(y[y_pos], na.rm = TRUE),
                len = 100
            ),
            include.lowest = TRUE
        )
        col[y_pos] <- colorRampPalette(c("#F5DCDC", "#C40003"))(100)[y_pos_i]
    }

    y_neg <- which(y < 0)
    if (length(y_neg) > 0) {
        y_neg_i <- cut(
            y[y_neg],
            breaks = seq(
                min(y[y_neg], na.rm = TRUE),
                max(y[y_neg], na.rm = TRUE),
                len = 100
            ),
            include.lowest = TRUE
        )
        col[y_neg] <- colorRampPalette(c("#1D008F", "#DDDCF5"))(100)[y_neg_i]
    }
    xmin <- which(!duplicated(col))
    xmax <- xmin + as.numeric(table(col)[as.character(unique(col))])
    d <- data.frame(
        ymin = 0,
        ymax = 0.3,
        xmin = xmin,
        xmax = xmax,
        col = unique(col)
    )
    p2 <- p2 +
        geom_rect(
            aes(
                xmin = !!sym("xmin"),
                xmax = !!sym("xmax"),
                ymin = !!sym("ymin"),
                ymax = !!sym("ymax"),
                fill = I(!!sym("col"))
            ),
            data = d,
            alpha = 0.95,
            inherit.aes = FALSE
        )

    ############# The gene ranking panel #############
    p3 <- p +
        geom_segment(
            aes(
                x = !!sym("x"),
                xend = !!sym("x"),
                y = !!sym("ranks"),
                yend = 0
            ),
            color = "grey30"
        )

    cross_x <- median(df$x[which.min(abs(df$ranks))])
    if (max(df$ranks) > 0) {
        p3 <- p3 +
            annotate(
                geom = "text",
                x = 0,
                y = Inf,
                vjust = 1.4,
                hjust = 0,
                color = "#C81A1F",
                size = 4,
                label = " Positively correlated"
            )
    }
    if (min(df$ranks) < 0) {
        p3 <- p3 +
            annotate(
                geom = "text",
                x = Inf,
                y = -Inf,
                vjust = -0.5,
                hjust = 1.02,
                color = "#3C298C",
                size = 4,
                label = "Negtively correlated "
            )
    }
    if (max(df$ranks) > 0 && min(df$ranks) < 0) {
        p3 <- p3 +
            geom_vline(xintercept = cross_x, linetype = 2, color = "black") +
            annotate(
                geom = "text",
                y = 0,
                x = cross_x,
                vjust = ifelse(diff(abs(range(df$ranks))) > 0, -0.3, 1.3),
                size = 4,
                label = paste0("Zero cross at ", cross_x)
            )
    }
    p3 <- p3 +
        ggplot2::ylab("Ranked List Metric") +
        ggplot2::xlab(xlab %||% "Rank in Ordered Dataset") +
        ggplot2::theme(
            plot.margin = margin(
                t = -0.1,
                r = 0.2,
                b = 0.2,
                l = 0.2,
                unit = "cm"
            ),
            axis.line = element_blank(),
            axis.line.x = element_blank(),
            panel.border = element_rect(
                color = "black",
                fill = "transparent",
                linewidth = 1
            )
        )

    if (!is.null(ylab)) {
        p4 <- textGrob(label = ylab, rot = -90, hjust = 0.5)

        p <- wrap_plots(
            p1,
            plot_spacer(),
            p2,
            plot_spacer(),
            p3,
            p4,
            heights = c(3.5, -0.19, 1, -0.24, 1.5),
            widths = c(12, .5)
        ) +
            plot_layout(axes = "collect", design = "AF\nBF\nCF\nDF\nEF")
        dims <- calculate_plot_dimensions(
            base_height = 6.5,
            aspect.ratio = NULL,
            legend.position = "none"
        )
        attr(p, "height") <- dims$height
        attr(p, "width") <- 8
    } else {
        p <- wrap_plots(
            p1,
            plot_spacer(),
            p2,
            plot_spacer(),
            p3,
            ncol = 1,
            heights = c(3.5, -0.19, 1, -0.24, 1.5)
        ) +
            plot_layout(axes = "collect")
        dims <- calculate_plot_dimensions(
            base_height = 6.5,
            aspect.ratio = NULL,
            legend.position = "none"
        )
        attr(p, "height") <- dims$height
        attr(p, "width") <- 7.5
    }
    p
}

#' @rdname gsea
#' @inheritParams common_args
#' @inheritParams GSEAPlotAtomic
#' @param in_form The format of the input data. See \code{\link{GSEASummaryPlot}}
#'  for details.
#' @param gene_sets A named list of gene sets. Each name must correspond to an
#'  \code{ID} in \code{data}, and each element is a character vector of gene
#'  identifiers. A GSEA ridge plot is generated for each gene set in the list.
#'  If you only want to plot a subset of gene sets, subset the list before
#'  passing it to this function. If a character string starting with
#'  \code{"@"}, the attribute of \code{data} with that name (minus the
#'  \code{"@"}) is used.
#' @param gs Character vector of gene set \code{ID}s to plot. If \code{NULL}
#'  (default), all gene sets in \code{gene_sets} that appear in \code{data$ID}
#'  are plotted.
#' @param combine Logical; when \code{TRUE} (default), returns a combined
#'  \code{patchwork} object. When \code{FALSE}, returns a named list of
#'  individual \code{patchwork} objects (one per gene set).
#' @param ncol,nrow Integer number of columns / rows for the combined layout
#'  (passed to \code{\link{combine_plots}()}).
#' @param byrow Logical; fill the combined layout by row. Default \code{TRUE}
#'  (passed to \code{\link{combine_plots}()}).
#' @param seed A numeric seed for reproducibility. Passed to
#'  \code{\link{validate_common_args}()}.
#' @param axes A character string specifying how axes should be treated across
#'  the combined layout (passed to \code{\link{combine_plots}()}).
#' @param axis_titles A character string specifying how axis titles should be
#'  treated across the combined layout. Defaults to \code{axes}.
#' @param guides A character string specifying how guides (legends) should be
#'  collected across panels (passed to \code{\link{combine_plots}()}).
#' @param design A custom layout design for the combined plot (passed to
#'  \code{\link{combine_plots}()}).
#' @export
#' @return A \code{patchwork} object when \code{combine = TRUE}, or a named
#'  list of \code{patchwork} objects when \code{combine = FALSE}. Each
#'  individual plot has \code{height} and \code{width} attributes in inches.
#' @examples
#' \donttest{
#' data(gsea_example)
#'
#' # Single gene set
#' GSEAPlot(gsea_example, gene_sets = attr(gsea_example, "gene_sets")[1])
#'
#' # Multiple gene sets arranged in a grid
#' GSEAPlot(gsea_example, gene_sets = attr(gsea_example, "gene_sets")[1:4])
#' }
GSEAPlot <- function(
    data,
    in_form = c("auto", "dose", "fgsea"),
    gene_ranks = "@gene_ranks",
    gene_sets = "@gene_sets",
    gs = NULL,
    sample_coregenes = FALSE,
    line_width = 1.5,
    line_alpha = 1,
    line_color = "#6BB82D",
    n_coregenes = 10,
    genes_label = NULL,
    label_fg = "black",
    label_bg = "white",
    label_bg_r = 0.1,
    label_size = 4,
    title = NULL,
    subtitle = NULL,
    xlab = NULL,
    ylab = NULL,
    combine = TRUE,
    nrow = NULL,
    ncol = NULL,
    byrow = TRUE,
    seed = 8525,
    axes = NULL,
    axis_titles = axes,
    guides = NULL,
    design = NULL,
    ...
) {
    set.seed(seed)
    in_form <- match.arg(in_form)
    if (inherits(data, "gseaResult")) {
        data <- as.data.frame(data)
    }
    if (
        is.character(gene_ranks) &&
            length(gene_ranks) == 1 &&
            startsWith(gene_ranks, "@")
    ) {
        gene_ranks <- attr(data, substring(gene_ranks, 2))
    }
    if (is.null(gene_ranks)) {
        stop("'gene_ranks' must be provided")
    }
    if (is.null(names(gene_ranks))) {
        stop("'gene_ranks' must have names")
    }
    if (!is.numeric(gene_ranks)) {
        stop("'gene_ranks' must be numeric")
    }
    gene_ranks <- gene_ranks[order(-gene_ranks)]

    if (
        is.character(gene_sets) &&
            length(gene_sets) == 1 &&
            startsWith(gene_sets, "@")
    ) {
        gene_sets <- attr(data, substring(gene_sets, 2))
    }
    if (is.null(gene_sets)) {
        stop("'gene_sets' must be provided")
    }
    if (!is.list(gene_sets)) {
        stop("'gene_sets' must be a list")
    }

    if (in_form == "auto") {
        if ("leadingEdge" %in% colnames(data)) {
            in_form <- "fgsea"
        } else if ("core_enrichment" %in% colnames(data)) {
            in_form <- "dose"
        } else {
            stop(
                "Cannot detect the input format. Please set 'in_form' to 'fgsea' or 'dose'."
            )
        }
    }
    if (in_form == "fgsea") {
        data <- prepare_fgsea_result(data)
    }
    gsnames <- intersect(as.character(data$ID), names(gene_sets))
    gene_sets <- gene_sets[gsnames]
    data <- data[as.character(data$ID) %in% gsnames, , drop = FALSE]
    theme <- process_theme(theme)
    gs <- gs %||% names(gene_sets)

    plots <- lapply(gs, function(g) {
        GSEAPlotAtomic(
            data,
            gene_ranks = gene_ranks,
            gs = g,
            genes = gene_sets[[g]],
            sample_coregenes = sample_coregenes,
            line_width = line_width,
            line_alpha = line_alpha,
            line_color = line_color,
            n_coregenes = n_coregenes,
            genes_label = genes_label,
            label_fg = label_fg,
            label_bg = label_bg,
            label_bg_r = label_bg_r,
            label_size = label_size,
            title = title,
            subtitle = subtitle,
            xlab = xlab,
            ylab = ylab,
            ...
        )
    })
    names(plots) <- gs

    combine_plots(
        plots,
        combine = combine,
        nrow = nrow,
        ncol = ncol,
        byrow = byrow,
        axes = axes,
        axis_titles = axis_titles,
        guides = guides,
        design = design
    )
}
