#' Get the running enrichment score of a gene set
#'
#' @param genes A vector of genes
#' @param gene_ranks A numeric vector of gene ranks with names
#' @param exponent A numeric value to raise the gene ranks to
#' @param hits_only A logical value to return only the running enrichment score of the hits
#' @return A numeric vector of the running enrichment score
#' @keywords internal
gsea_running_score <- function(genes, gene_ranks, exponent = 1, hits_only = TRUE) {
    genes <- intersect(genes, names(gene_ranks))
    N <- length(gene_ranks)
    Nh <- length(genes)
    Phit <- Pmiss <- numeric(N)
    hits <- names(gene_ranks) %in% genes
    Phit[hits] <- abs(gene_ranks[hits])^exponent
    NR <- sum(Phit)
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

#' GSEA plots
#'
#' @description
#'  * `GSEASummaryPlot` is used to plot a summary of the results of a GSEA analysis.
#'  * `GSEAPlot` is used to plot the results of a GSEA analysis.
#'  * `PrepareFGSEA` is used to prepare the result of GSEA from the `fgsea` package for plotting.
#'
#' @rdname gsea
#' @param data A data frame of fgsea results
#' @param gene_ranks A numeric vector of gene ranks with names
#' @param gene_sets A list of gene sets, typically from a record of a GMT file
#' @return A data frame with the desired columns for plotting and the gene ranks and gene sets as attributes
#' @export
PrepareFGSEAResult <- function(data, gene_ranks, gene_sets) {
    data$ID <- data$pathway
    data$Description <- data$pathway
    data$pathway <- NULL
    data$pvalue <- data$pval
    data$pval <- NULL
    data$p.adjust <- data$padj
    data$padj <- NULL
    data$core_enrichment <- sapply(data$leadingEdge, paste0, collapse = "/")
    data$leadingEdge <- NULL

    attr(data, "gene_ranks") <- gene_ranks
    attr(data, "gene_sets") <- gene_sets
    data
}

#' @rdname gsea
#'
#' @inheritParams common_args
#' @param data A data frame of GSEA results
#'  For example, from `DOSE::gseDO()`.
#'  Required columns are `ID`, `Description`, `NES`, `p.adjust`, `pvalue`.
#'  The `ID` column is used to match the gene sets.
#' @param gene_ranks A numeric vector of gene ranks with genes as names
#'  The gene ranks are used to plot the gene sets.
#'  If `gene_ranks` is a character vector starting with `@`, the gene ranks will be taken from the attribute of `data`.
#' @param gene_sets A list of gene sets, typically from a record of a GMT file
#'  The names of the list should match the `ID` column of `data`.
#'  If `gene_sets` is a character vector starting with `@`, the gene sets will be taken from the attribute of `data`.
#' @param top_term An integer to select the top terms
#' @param metric The metric to use for the significance of the terms
#' @param cutoff The cutoff for the significance of the terms
#'  The terms will not be filtered with this cutoff; they are only filtered by the `top_term` ranked by the `metric`.
#'  The cutoff here is used to show the significance of the terms on the plot.
#'  For the terms that are not significant, the color will be grey.
#' @param character_width The width of the characters in the y-axis
#' @param line_plot_size The size of the line plots
#' @param metric_name The name of the metric to show in the color bar
#' @param nonsig_name The name of the legend for the nonsignificant terms
#' @param linewidth The width of the lines in the line plots
#' @param line_by The method to calculate the line plots.
#'  * `prerank`: Use the gene ranks as heights to plot the line plots.
#'  * `running_score`: Use the running score to plot the line plots.
#' @importFrom ggplot2 geom_linerange layer_scales theme_void ylim
#' @export
#' @examples
#' \dontrun{
#' data(gsea_example)
#' GSEASummaryPlot(gsea_example)
#' GSEASummaryPlot(gsea_example, line_by = "running_score")
#' GSEASummaryPlot(gsea_example, cutoff = 0.01)
#' }
GSEASummaryPlot <- function(
    data, gene_ranks = "@gene_ranks", gene_sets = "@gene_sets", top_term = 10, metric = "p.adjust",
    cutoff = 0.05, character_width = 50, line_plot_size = 0.25, metric_name = paste0("-log10(", metric, ")"),
    nonsig_name = "Non-significant", linewidth = 0.2, line_by = c("prerank", "running_score"),
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL,
    alpha = 0.6, aspect.ratio = 1, legend.position = "right", legend.direction = "vertical",
    theme = "theme_this", theme_args = list(), palette = "Spectral", palcolor = NULL,
    seed = 8525, ...) {
    set.seed(seed)
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
    if (is.character(gene_ranks) && length(gene_ranks) == 1 && startsWith(gene_ranks, "@")) {
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

    if (is.character(gene_sets) && length(gene_sets) == 1 && startsWith(gene_sets, "@")) {
        gene_sets <- attr(data, substring(gene_sets, 2))
    }
    if (is.null(gene_sets)) {
        stop("'gene_sets' must be provided")
    }
    if (!is.list(gene_sets)) {
        stop("'gene_sets' must be a list")
    }

    if (!is.null(top_term)) {
        data <- slice_min(data, !!sym(metric), n = top_term, with_ties = FALSE)
    }
    if (!is.null(cutoff)) {
        # data <- data[data[[metric]] < cutoff, , drop = FALSE]
        data$.signif <- data[[metric]] < cutoff
    } else {
        data$.signif <- TRUE
    }
    data$metric <- -log10(data[[metric]])
    check_columns(data, "Description", force_factor = TRUE)
    data$Description <- droplevels(data$Description)
    data <- data[order(data$Description), , drop = FALSE]
    data$Description <- str_wrap(data$Description, width = character_width)
    data$Description <- factor(data$Description, levels = unique(data$Description))
    data$y <- as.integer(data$Description)

    if (all(data$.signif)) {
        p <- ggplot(data, aes(x = !!sym("NES"), y = !!sym("y")))
    } else {
        p <- ggplot(data, aes(x = !!sym("NES"), y = !!sym("y"), fill = "")) +
            guides(fill = guide_legend(
                title = nonsig_name %||% "Non-significant",
                override.aes = list(color = "grey80", shape = 15, size = 4),
                order = 2
            ))
    }
    # need a layer to get the scales of the plot
    p <- p + geom_point(aes(color = !!sym("metric")), size = 0)
    x_range <- diff(layer_scales(p)$x$range$range)
    y_range <- diff(layer_scales(p)$y$range$range)
    colors <- palette_this(
        data$metric[data$.signif],
        n = sum(data$.signif), palette = palette, palcolor = palcolor,
        alpha = alpha, type = "continuous"
    )
    line_plot_list <- list()

    ci <- 1
    for (i in seq_len(nrow(data))) {
        if (isTRUE(data$.signif[i])) {
            color <- colors[ci]
            ci <- ci + 1
        } else {
            color <- "grey80"
        }
        hits <- intersect(gene_sets[[data$ID[i]]], names(gene_ranks))
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
                xmin = x_min, xmax = x_max, ymin = -yr, ymax = yr,
                fill = color
            ) +
            geom_linerange(aes(ymin = !!sym("ymin"), ymax = !!sym("ymax")), linewidth = linewidth) +
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

    p <- p +
        line_plot_list +
        scale_color_gradientn(
            name = metric_name,
            colors = colors,
            guide = guide_colorbar(
                frame.colour = "black", ticks.colour = "black", title.hjust = 0, order = 1
            )
        ) +
        scale_y_continuous(breaks = seq_len(nrow(data)), labels = data$Description) +
        scale_x_continuous(expand = c(0.05, x_range * line_plot_size / 2)) +
        labs(title = title, subtitle = subtitle, x = xlab %||% "NES", y = ylab %||% "") +
        do.call(theme, theme_args) +
        ggplot2::theme(
            aspect.ratio = aspect.ratio,
            legend.position = legend.position,
            legend.direction = legend.direction,
            panel.grid.major.y = element_line(colour = "grey80", linetype = 2),
        )

    max_nchar_y <- min(max(nchar(levels(data$Description))), character_width)
    height <- nrow(data) * 0.65
    width <- max_nchar_y * 0.1 + 5
    if (!identical(legend.position, "none")) {
        if (legend.position %in% c("right", "left")) {
            width <- width + 1.5
        } else if (legend.direction == "horizontal") {
            height <- height + 2
        } else {
            height <- height + 3.5
        }
    }
    attr(p, "height") <- height
    attr(p, "width") <- width

    p
}

#' GSEA plot for a single term
#'
#' @inheritParams common_args
#' @inheritParams GSEASummaryPlot
#' @param gs The name of the gene set
#' @param genes The genes in the gene set
#' @param metric The metric to show in the subtitle
#' @param sample_coregenes A logical value to sample the core genes from the core_enrichment; if `FALSE`, the first `n_coregenes` will be used
#' @param line_width The width of the line in the running score plot
#' @param line_alpha The alpha of the line in the running score plot
#' @param line_color The color of the line in the running score plot
#' @param n_coregenes The number of core genes to label
#' @param genes_label The genes to label. If set, `n_coregenes` will be ignored
#' @param label_fg The color of the label text
#' @param label_bg The background color of the label
#' @param label_bg_r The radius of the background color of the label
#' @param label_size The size of the label text
#' @param ylab The label of the y-axis, will be shown on the right side
#' @keywords internal
#' @importFrom scales alpha
#' @importFrom ggplot2 ggtitle theme_classic annotate
#' @importFrom patchwork plot_layout wrap_plots plot_spacer
GSEAPlotAtomic <- function(
    data, gene_ranks = "@gene_ranks", gs, genes, metric = "p.adjust", sample_coregenes = FALSE,
    line_width = 1.5, line_alpha = 1, line_color = "#6BB82D", n_coregenes = 10, genes_label = NULL,
    label_fg = "black", label_bg = "white", label_bg_r = 0.1, label_size = 4,
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL, ...) {
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
    if (is.character(gene_ranks) && length(gene_ranks) == 1 && startsWith(gene_ranks, "@")) {
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
    subtitle <- subtitle %||% paste0(
        "(NES=", round(data$NES, 3), ", ",
        metric, "=", format(data[[metric]], digits = 3, scientific = TRUE), ", ",
        sig, ")"
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
                xmin = -Inf, xmax = Inf, ymin = c(0, -Inf), ymax = c(Inf, 0),
                fill = c(alpha("#C40003", 0.2), alpha("#1D008F", 0.2))
            ),
            mapping = aes(xmin = !!sym("xmin"), xmax = !!sym("xmax"), ymin = !!sym("ymin"), ymax = !!sym("ymax"),
                fill = I(!!sym("fill"))),
            inherit.aes = FALSE
        ) +
        geom_hline(yintercept = 0, linetype = 1, color = "grey40") +
        # running score
        geom_line(aes(y = !!sym("runningScore")), color = line_color, linewidth = line_width, alpha = line_alpha) +
        annotate(
            geom = "segment", x = 0, xend = df$x[index_max],
            y = df$runningScore[index_max], yend = df$runningScore[index_max], linetype = 2
        ) +
        annotate(
            geom = "segment", x = df$x[index_max], xend = df$x[index_max],
            y = 0, yend = df$runningScore[index_max], linetype = 2
        ) +
        annotate(
            geom = "point", x = df$x[index_max], y = df$runningScore[index_max],
            fill = ifelse(data$NES < 0, "#5E34F5", "#F52323"), color = "black", size = 2.5,
            shape = ifelse(data$NES < 0, 25, 24)
        ) +
        ggplot2::ylab("Enrichment Score") +
        ggplot2::theme(
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.line = element_blank(),
            panel.border = element_rect(color = "black", fill = "transparent", linewidth = 1),
            plot.margin = margin(t = 0.2, r = 0.2, b = 0, l = 0.2, unit = "cm"),
            legend.position = "none",
            plot.subtitle = element_text(face = "italic")
        ) +
        ggtitle(title, subtitle)

    # label the genes
    if ((is.numeric(n_coregenes) && n_coregenes > 1) || length(genes_label) > 0) {
        if (length(genes_label) == 0) {
            genes_label_tmp <- unlist(strsplit(data$core_enrichment, "/"))
            n_coregenes <- min(n_coregenes, length(genes_label_tmp))
            if (isTRUE(sample_coregenes)) {
                genes_label_tmp <- sample(genes_label_tmp, n_coregenes, replace = FALSE)
            } else {
                genes_label_tmp <- df$genes[df$genes %in% genes_label_tmp][1:n_coregenes]
            }
        } else {
            genes_label_tmp <- genes_label
        }
        df_gene <- df[df$position == 1 & df$genes %in% genes_label_tmp, , drop = FALSE]
        gene_drop <- genes_label_tmp[!genes_label_tmp %in% df_gene$genes]
        if (length(gene_drop) > 0) {
            warning("Gene ", paste(gene_drop, collapse = ","), " is not in the geneset ", data$ID, ": ", data$Description, immediate. = TRUE)
        }
        x_nudge <- diff(range(df$x)) * 0.05
        y_nudge <- diff(range(df$runningScore)) * 0.05
        p1 <- p1 + geom_point(
            data = df_gene,
            mapping = aes(y = !!sym("runningScore")), color = "black"
        ) +
            geom_text_repel(
                data = df_gene,
                mapping = aes(y = !!sym("runningScore"), label = !!sym("genes")),
                min.segment.length = 0, max.overlaps = 100, segment.colour = "grey40",
                color = label_fg, bg.color = label_bg, bg.r = label_bg_r, size = label_size,
                nudge_x = ifelse(df_gene$runningScore >= 0, x_nudge, -x_nudge),
                nudge_y = ifelse(df_gene$runningScore > 0, -y_nudge, y_nudge)
            )
    }

    ############# The Line Plot Panel #############
    p2 <- ggplot(df, aes(x = !!sym("x"))) +
        geom_linerange(aes(ymax = !!sym("position")), ymin = 0, alpha = line_alpha) +
        ggplot2::xlab(NULL) +
        ggplot2::ylab(NULL) +
        theme_classic(base_size = 12) +
        ggplot2::theme(
            legend.position = "none",
            plot.margin = margin(t = -0.1, b = 0, r = 0.2, l = 0.2, unit = "cm"),
            panel.border = element_rect(color = "black", fill = "transparent", linewidth = 1),
            axis.line.y = element_blank(), axis.line.x = element_blank(),
            axis.ticks = element_blank(), axis.text = element_blank()
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
        y_pos_i <- cut(y[y_pos],
            breaks = seq(min(y[y_pos], na.rm = TRUE), max(y[y_pos], na.rm = TRUE), len = 100),
            include.lowest = TRUE
        )
        col[y_pos] <- colorRampPalette(c("#F5DCDC", "#C40003"))(100)[y_pos_i]
    }

    y_neg <- which(y < 0)
    if (length(y_neg) > 0) {
        y_neg_i <- cut(y[y_neg],
            breaks = seq(min(y[y_neg], na.rm = TRUE), max(y[y_neg], na.rm = TRUE), len = 100),
            include.lowest = TRUE
        )
        col[y_neg] <- colorRampPalette(c("#1D008F", "#DDDCF5"))(100)[y_neg_i]
    }
    xmin <- which(!duplicated(col))
    xmax <- xmin + as.numeric(table(col)[as.character(unique(col))])
    d <- data.frame(ymin = 0, ymax = 0.3, xmin = xmin, xmax = xmax, col = unique(col))
    p2 <- p2 +
        geom_rect(
            aes(xmin = !!sym("xmin"), xmax = !!sym("xmax"), ymin = !!sym("ymin"), ymax = !!sym("ymax"), fill = I(!!sym("col"))),
            data = d,
            alpha = 0.95, inherit.aes = FALSE
        )

    ############# The gene ranking panel #############
    p3 <- p +
        geom_segment(aes(x = !!sym("x"), xend = !!sym("x"), y = !!sym("ranks"), yend = 0), color = "grey30")

    cross_x <- median(df$x[which.min(abs(df$ranks))])
    if (max(df$ranks) > 0) {
        p3 <- p3 + annotate(geom = "text", x = 0, y = Inf, vjust = 1.4, hjust = 0, color = "#C81A1F", size = 4, label = " Positively correlated")
    }
    if (min(df$ranks) < 0) {
        p3 <- p3 + annotate(geom = "text", x = Inf, y = -Inf, vjust = -0.5, hjust = 1.02, color = "#3C298C", size = 4, label = "Negtively correlated ")
    }
    if (max(df$ranks) > 0 && min(df$ranks) < 0) {
        p3 <- p3 + geom_vline(xintercept = cross_x, linetype = 2, color = "black") +
            annotate(geom = "text", y = 0, x = cross_x, vjust = ifelse(diff(abs(range(df$ranks))) > 0, -0.3, 1.3), size = 4, label = paste0("Zero cross at ", cross_x))
    }
    p3 <- p3 + ggplot2::ylab("Ranked List Metric") + ggplot2::xlab(xlab %||% "Rank in Ordered Dataset") +
        ggplot2::theme(
            plot.margin = margin(t = -0.1, r = 0.2, b = 0.2, l = 0.2, unit = "cm"),
            axis.line = element_blank(), axis.line.x = element_blank(),
            panel.border = element_rect(color = "black", fill = "transparent", linewidth = 1)
        )

    if (!is.null(ylab)) {
        p4 <- textGrob(label = ylab, rot = -90, hjust = 0.5)

        p <- wrap_plots(p1, plot_spacer(), p2, plot_spacer(), p3, p4, heights = c(3.5, -0.19, 1, -0.24, 1.5), widths = c(12, .5)) +
            plot_layout(axes = "collect", design = "AF\nBF\nCF\nDF\nEF")
        attr(p, "height") <- 6.5
        attr(p, "width") <- 8
    } else {
        p <- wrap_plots(p1, plot_spacer(), p2, plot_spacer(), p3, ncol = 1, heights = c(3.5, -0.19, 1, -0.24, 1.5)) +
            plot_layout(axes = "collect")
        attr(p, "height") <- 6.5
        attr(p, "width") <- 7.5
    }
    p
}

#' @rdname gsea
#' @inheritParams common_args
#' @inheritParams GSEAPlotAtomic
#' @param gene_sets A list of gene sets, typically from a record of a GMT file
#'  The names of the list should match the `ID` column of `data`.
#'  If `gene_sets` is a character vector starting with `@`, the gene sets will be taken from the attribute of `data`.
#'  The GSEA plots will be plotted for each gene set. So, the number of plots will be the number of gene sets.
#'  If you only want to plot a subset of gene sets, you can subset the `gene_sets` before passing it to this function.
#' @export
#' @examples
#' \dontrun{
#' GSEAPlot(gsea_example, gene_sets = attr(gsea_example, "gene_sets")[1])
#' GSEAPlot(gsea_example, gene_sets = attr(gsea_example, "gene_sets")[1:4])
#' }
GSEAPlot <- function(
    data, gene_ranks = "@gene_ranks", gene_sets = "@gene_sets", sample_coregenes = FALSE,
    line_width = 1.5, line_alpha = 1, line_color = "#6BB82D", n_coregenes = 10, genes_label = NULL,
    label_fg = "black", label_bg = "white", label_bg_r = 0.1, label_size = 4,
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL,
    combine = TRUE, nrow = NULL, ncol = NULL, byrow = TRUE, seed = 8525, ...) {
    set.seed(seed)
    if (inherits(data, "gseaResult")) {
        data <- as.data.frame(data)
    }
    if (is.character(gene_ranks) && length(gene_ranks) == 1 && startsWith(gene_ranks, "@")) {
        gene_ranks <- attr(data, substring(gene_ranks, 2))
    }
    if (is.null(gene_ranks)) { stop("'gene_ranks' must be provided") }
    if (is.null(names(gene_ranks))) { stop("'gene_ranks' must have names") }
    if (!is.numeric(gene_ranks)) { stop("'gene_ranks' must be numeric") }
    gene_ranks <- gene_ranks[order(-gene_ranks)]

    if (is.character(gene_sets) && length(gene_sets) == 1 && startsWith(gene_sets, "@")) {
        gene_sets <- attr(data, substring(gene_sets, 2))
    }
    if (is.null(gene_sets)) { stop("'gene_sets' must be provided") }
    if (!is.list(gene_sets)) { stop("'gene_sets' must be a list") }

    gsnames <- intersect(as.character(data$ID), names(gene_sets))
    gene_sets <- gene_sets[gsnames]
    data <- data[as.character(data$ID) %in% gsnames, , drop = FALSE]
    theme <- process_theme(theme)

    plots <- lapply(names(gene_sets), function(gs) {
        GSEAPlotAtomic(
            data,
            gene_ranks = gene_ranks, gs = gs, genes = gene_sets[[gs]],
            sample_coregenes = sample_coregenes, line_width = line_width, line_alpha = line_alpha,
            line_color = line_color, n_coregenes = n_coregenes, genes_label = genes_label,
            label_fg = label_fg, label_bg = label_bg, label_bg_r = label_bg_r, label_size = label_size,
            title = title, subtitle = subtitle, xlab = xlab, ylab = ylab, ...
        )
    })

    combine_plots(plots, combine = combine, nrow = nrow, ncol = ncol, byrow = byrow)
}
