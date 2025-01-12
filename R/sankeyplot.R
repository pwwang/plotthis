#' Atomic Sankey plot
#'
#' @description Plot a Sankey plot without splitting the data.
#' @inheritParams common_args
#' @param data A data frame in three possible formats:
#' * "long" or "lodes": A long format with columns for `x`, `stratum`, `alluvium`, and `y`.
#'   `x` (required, single columns or concatenated by `x_sep`) is the column name to plot on the x-axis,
#'   `stratum` (defaults to `links_fill_by`) is the column name
#'   to group the nodes for each `x`, `alluvium` (required) is the column name to define the links, and `y`
#'   is the frequency of each `x`, `stratum`, and `alluvium`.
#' * "wide" or "alluvia": A wide format with columns for `x`.
#'   `x` (required, multiple columns, `x_sep` won't be used) are the columns to plot on the x-axis,
#'   `stratum` and `alluvium` will be ignored.
#'   See [ggalluvial::to_lodes_form] for more details.
#' * "counts": A format with counts being provides under each `x`.
#'  `x` (required, multiple columns, `x_sep` won't be used) are the columns to plot on the x-axis.
#'  When the first element of `x` is ".", values of `links_fill_by` (required) will be added to the plot as the first column of nodes.
#'  It is useful to show how the links are flowed from the source to the targets.
#' * "auto" (default): Automatically determine the format based on the columns provided.
#' When the length of `x` is greater than 1 and all `x` columns are numeric, "counts" format will be used.
#' When the length of `x` is greater than 1 and [ggalluvial::is_alluvia_form] returns TRUE, "alluvia" format will be used.
#' Otherwise, "lodes" format will be tried.
#' @param in_form A character string to specify the format of the data.
#' Possible values are "auto", "long", "lodes", "wide", "alluvia", and "counts".
#' @param x A character string of the column name to plot on the x-axis.
#' See `data` for more details.
#' @param x_sep A character string to concatenate the columns in `x`, if multiple columns are provided.
#' @param y A character string of the column name to plot on the y-axis.
#' When `in_form` is "counts", `y` will be ignored. Otherwise, it defaults to the count of each `x`, `stratum`, `alluvium` and `links_fill_by`.
#' @param stratum A character string of the column name to group the nodes for each `x`.
#' See `data` for more details.
#' @param stratum_sep A character string to concatenate the columns in `stratum`, if multiple columns are provided.
#' @param alluvium A character string of the column name to define the links.
#' See `data` for more details.
#' @param alluvium_sep A character string to concatenate the columns in `alluvium`, if multiple columns are provided.
#' @param flow A logical value to use [ggalluvial::geom_flow] instead of [ggalluvial::geom_alluvium].
#' @param nodes_color A character string to color the nodes.
#' Use a special value ".fill" to use the same color as the fill.
#' @param links_fill_by A character string of the column name to fill the links.
#' @param links_fill_by_sep A character string to concatenate the columns in `links_fill_by`, if multiple columns are provided.
#' @param links_name A character string to name the legend of links.
#' @param links_color A character string to color the borders of links.
#' Use a special value ".fill" to use the same color as the fill.
#' @param nodes_palette A character string to specify the palette of nodes fill.
#' @param nodes_palcolor A character vector to specify the colors of nodes fill.
#' @param nodes_alpha A numeric value to specify the transparency of nodes fill.
#' @param nodes_label A logical value to show the labels on the nodes.
#' @param nodes_label_miny A numeric value to specify the minimum y (frequency) to show the labels.
#' @param nodes_width A numeric value to specify the width of nodes.
#' @param nodes_legend Controls how the legend of nodes will be shown. Possible values are:
#' * "merge": Merge the legends of nodes. That is only one legend will be shown for all nodes.
#' * "separate": Show the legends of nodes separately. That is, nodes on each `x` will have their own legend.
#' * "none": Do not show the legend of nodes.
#' * "auto": Automatically determine how to show the legend.
#' When `nodes_label` is TRUE, "none" will apply.
#' When `nodes_label` is FALSE, and if stratum is the same as links_fill_by, "none" will apply.
#' If there is any overlapping values between the nodes on different `x`,
#' "merge" will apply. Otherwise, "separate" will apply.
#' @param links_palette A character string to specify the palette of links fill.
#' @param links_palcolor A character vector to specify the colors of links fill.
#' @param links_alpha A numeric value to specify the transparency of links fill.
#' @param legend.box A character string to specify the box of the legend, either "vertical" or "horizontal".
#' @param keep_empty A logical value to keep the empty nodes.
#' @param flip A logical value to flip the plot.
#' @param ... Other arguments to pass to [ggalluvial::geom_alluvium] or [ggalluvial::geom_flow].
#' @return A ggplot object
#' @keywords internal
#' @importFrom utils combn
#' @importFrom rlang syms %||% dots_n
#' @importFrom dplyr %>% group_by summarise n ungroup cur_group_id mutate add_count
#' @importFrom tidyr pivot_wider pivot_longer
#' @importFrom ggnewscale new_scale_fill
#' @importFrom ggplot2 geom_col scale_fill_manual geom_label after_stat scale_x_discrete scale_y_continuous labs coord_flip
SankeyPlotAtomic <- function(
    data, in_form = c("auto", "long", "lodes", "wide", "alluvia", "counts"), x, x_sep = "_", y = NULL,
    stratum = NULL, stratum_sep = "_", alluvium = NULL, alluvium_sep = "_", flow = FALSE,
    nodes_color = "grey30", links_fill_by = NULL, links_fill_by_sep = "_", links_name = NULL, links_color = "gray80",
    nodes_palette = "Paired", nodes_palcolor = NULL, nodes_alpha = 1, nodes_label = FALSE, nodes_width = 0.25,  nodes_label_miny = 0,
    nodes_legend = c("auto", "separate", "merge", "none"), expand = c(0, 0, 0, 0), links_palette = "Paired", links_palcolor = NULL,
    links_alpha = 0.6, legend.box = "vertical", keep_empty = TRUE, x_text_angle = 0, aspect.ratio = 1, legend.position = "right",
    legend.direction = "vertical", flip = FALSE, theme = "theme_this", theme_args = list(), title = NULL, subtitle = NULL,
    xlab = NULL, ylab = NULL, facet_by = NULL, facet_scales = "fixed", facet_ncol = NULL, facet_nrow = NULL, facet_byrow = TRUE,
    ...
) {
    ggplot <- if (getOption("plotthis.gglogger.enabled", FALSE)) {
        gglogger::ggplot
    } else {
        ggplot2::ggplot
    }

    is_flowcounts <- FALSE
    nodes_legend <- match.arg(nodes_legend)
    in_form <- match.arg(in_form)
    if (in_form == "long") in_form <- "lodes"
    if (in_form == "wide") in_form <- "alluvia"
    if (in_form == "lodes") {
        x <- check_columns(data, x, force_factor = TRUE, allow_multi = TRUE, concat_multi = TRUE, concat_sep = x_sep)
        alluvium <- check_columns(data, alluvium, force_factor = TRUE, allow_multi = TRUE,
            concat_multi = TRUE, concat_sep = alluvium_sep)
        links_fill_by <- check_columns(data, links_fill_by, force_factor = TRUE, allow_multi = TRUE,
            concat_multi = TRUE, concat_sep = links_fill_by_sep) %||% alluvium
        stratum <- check_columns(data, stratum, force_factor = TRUE, allow_multi = TRUE,
            concat_multi = TRUE, concat_sep = stratum_sep) %||% links_fill_by
        if (is.null(y)) {
            data <- add_count(data, !!!syms(unique(c(x, stratum, alluvium, links_fill_by, facet_by))), name = ".y")
            y <- ".y"
        }
    } else if (!identical(x[1], ".") &&
        (in_form == "counts" || (in_form == "auto" && length(x) > 1 && all(sapply(data[, x, drop = FALSE], is.numeric))))) {
        x <- check_columns(data, x, allow_multi = TRUE)
        # if (!is.null(stratum)) warning("[SankeyPlot] 'stratum' is ignored in 'counts' format.")
        if (!is.null(y)) warning("[SankeyPlot] 'y' is ignored in 'counts' format.")
        if (is.null(alluvium)) {
            alluvium <- "alluvium"
            data[[alluvium]] <- as.character(1:nrow(data))
        } else {
            alluvium <- check_columns(data, alluvium, force_factor = TRUE, allow_multi = TRUE,
                concat_multi = TRUE, concat_sep = alluvium_sep)
        }
        links_fill_by <- check_columns(data, links_fill_by, force_factor = TRUE, allow_multi = TRUE,
            concat_multi = TRUE, concat_sep = links_fill_by_sep) %||% alluvium
        data <- pivot_longer(data, cols = x, names_to = "x", values_to = "Frequency")
        data$x <- factor(data$x, levels = x)
        stratum <- check_columns(data, stratum, force_factor = TRUE, allow_multi = TRUE,
            concat_multi = TRUE, concat_sep = stratum_sep) %||% links_fill_by
        x <- "x"
        y <- "Frequency"
    } else if (identical(x[1], ".") &&
        (in_form == "counts" || (in_form == "auto" && length(x) > 1 && all(sapply(data[, x[-1], drop = FALSE], is.numeric))))) {
        is_flowcounts <- TRUE
        x <- check_columns(data, x[-1], allow_multi = TRUE)
        stratum <- check_columns(data, stratum, force_factor = TRUE, allow_multi = TRUE,
            concat_multi = TRUE, concat_sep = stratum_sep)
        stratum_levels <- if (is.null(stratum)) NULL else levels(data[[stratum]])
        if (!is.null(y)) warning("[SankeyPlot] 'y' is ignored in 'counts' format.")
        if (is.null(alluvium)) {
            alluvium <- "alluvium"
            data[[alluvium]] <- as.character(1:nrow(data))
        } else {
            alluvium <- check_columns(data, alluvium, force_factor = TRUE, allow_multi = TRUE,
                concat_multi = TRUE, concat_sep = alluvium_sep)
        }
        links_fill_by <- check_columns(data, links_fill_by, force_factor = TRUE, allow_multi = TRUE,
            concat_multi = TRUE, concat_sep = links_fill_by_sep)
        stopifnot("[SankeyPlot] The 'links_fill_by' must be provided in 'counts' format." = !is.null(links_fill_by))
        data <- pivot_longer(data, cols = x, names_to = "x", values_to = "Frequency")
        if (is.null(stratum)) {
            stratum <- links_fill_by
        } else {
            data[[stratum]] <- factor(data[[stratum]], levels = stratum_levels)
        }
        dotdata <- data[data$x == x[1], , drop = FALSE]
        dotdata$x <- links_fill_by
        dotdata[[stratum]] <- dotdata[[links_fill_by]]
        data <- rbind(data, dotdata)
        data$x <- factor(data$x, levels = unique(c(links_fill_by, x)))
        data[[stratum]] <- factor(data[[stratum]], levels = unique(c(levels(data[[links_fill_by]]), stratum_levels)))
        x <- "x"
        y <- "Frequency"
    } else if (in_form == "alluvia" || (in_form == "auto" && length(x) > 1 && ggalluvial::is_alluvia_form(data, axes = x, weight = y))) {
        x <- check_columns(data, x, force_factor = TRUE, allow_multi = TRUE)
        stopifnot("[SankeyPlot] 'x' must be at least 2 columns in 'alluvia' format." = length(x) >= 2)
        if (!is.null(stratum)) warning("[SankeyPlot] 'stratum' is ignored in 'alluvia' format.")
        if (!is.null(alluvium)) warning("[SankeyPlot] 'alluvium' is ignored in 'alluvia' format.")
        links_fill_by <- check_columns(data, links_fill_by, force_factor = TRUE, allow_multi = TRUE,
            concat_multi = TRUE, concat_sep = links_fill_by_sep)
        if (is.null(y)) {
            data <- add_count(data, !!!syms(unique(c(x, links_fill_by, facet_by))), name = ".y")
            y <- ".y"
        }
        # make a copy of links_fill_by in case it's one of x or alluvium that gets transformed later
        if (!is.null(links_fill_by) && links_fill_by %in% x) {
            is_flowcounts <- identical(links_fill_by, x[1])
            data <- ggalluvial::to_lodes_form(data, axes = x, diffuse = links_fill_by)
        } else {
            data <- ggalluvial::to_lodes_form(data, axes = x)
        }
        x <- "x"
        alluvium <- "alluvium"
        stratum <- "stratum"
    } else {  # maybe lodes
        x <- check_columns(data, x, force_factor = TRUE, allow_multi = TRUE, concat_multi = TRUE, concat_sep = x_sep)
        alluvium <- check_columns(data, alluvium, force_factor = TRUE, allow_multi = TRUE,
            concat_multi = TRUE, concat_sep = alluvium_sep)
        links_fill_by <- check_columns(data, links_fill_by, force_factor = TRUE, allow_multi = TRUE,
            concat_multi = TRUE, concat_sep = links_fill_by_sep) %||% alluvium
        stratum <- check_columns(data, stratum, force_factor = TRUE, allow_multi = TRUE,
            concat_multi = TRUE, concat_sep = stratum_sep) %||% links_fill_by
        if (!ggalluvial::is_lodes_form(data, key = x, value = stratum, id = alluvium)) {
            stop("[SankeyPlot] 'data' must be in 'lodes/long' or 'alluvia/wide' format.")
        }
        if (is.null(y)) {
            data <- add_count(data, !!!syms(unique(c(x, stratum, alluvium, links_fill_by, facet_by))), name = ".y")
            y <- ".y"
        }
    }

    nodes_colors <- palette_this(levels(data[[stratum]]), palette = nodes_palette, palcolor = nodes_palcolor)
    links_colors <- palette_this(levels(data[[links_fill_by]]), palette = links_palette, palcolor = links_palcolor)
    if (is_flowcounts) {
        if (identical(nodes_colors[names(links_colors)], links_colors)) {
            links_guide <- "none"
        } else if (identical(links_palette, nodes_palette) && identical(links_palcolor, nodes_palcolor)) {
            links_guide = guide_legend(title = paste0(links_fill_by, " (links)"), order = 1,
                override.aes = list(alpha = min(links_alpha + 0.2, 1), color = "transparent"))
            warning(
                "[SankeyPlot] Plotting the flow of the first column of nodes. Links guide is still showing because ",
                "the first column of nodes have different colors as the links. It is probably because nodes_palette ",
                "does not have enough colors. Please use a palette with more colors for both ",
                "nodes_palette and links_palette.")
        }
    } else {
        links_guide = guide_legend(order = 1, override.aes = list(alpha = min(links_alpha + 0.2, 1), color = "transparent"))
    }

    just <- calc_just(x_text_angle)
    base_size <- theme_args$base_size %||% 12
    text_size_scale <- base_size / 12
    expand <- norm_expansion(expand, x_type = "discrete", y_type = "continuous")
    if (nodes_legend == "auto") {
        if (isTRUE(nodes_label) || (identical(stratum, links_fill_by) && identical(nodes_colors, links_colors))) {
            nodes_legend <- "none"
        } else {
            stratum_values <- lapply(levels(data[[x]]), function(xval) {
                as.character(unique(data[data[[x]] == xval, stratum, drop = TRUE]))
            })
            idxes <- combn(seq_along(stratum_values), 2)
            nodes_legend <- ifelse(
                !is_flowcounts &&
                any(sapply(
                    as.data.frame(idxes),
                    function(idx) length(intersect(stratum_values[[idx[1]]], stratum_values[[idx[2]]])) > 0
                )),
                "merge",
                "separate"
            )
        }
    }

    p <- ggplot(
        data = data,
        aes(x = !!sym(x), stratum = !!sym(stratum), alluvium = !!sym(alluvium), y = !!sym(y)))

    # fill nodes on each x so they can have different legends/guides
    # but when stratum and alluvium are the same, they should be filled with the same palette
    if (!identical(stratum, links_fill_by) && nodes_legend == "separate") {
        xs <- levels(data[[x]])
        for (i in seq_along(xs)) {
            xdf <- filter(data, !!sym(x) == xs[i])
            xdf <- xdf[order(xdf[[stratum]]), , drop = FALSE]
            p <- p +
                geom_col(
                    data = xdf, inherit.aes = FALSE,
                    aes(x = !!sym(x), fill = !!sym(stratum), y = 0), width = 0) +
                scale_fill_manual(name = xs[i], values = nodes_colors, breaks = unique(xdf[[stratum]]),
                    guide = guide_legend(order = i + 1, override.aes = list(alpha = min(nodes_alpha + 0.2, 1)))) +
                new_scale_fill()
        }
    }

    if (!isTRUE(flow)) {
        if (identical(links_color, ".fill")) {
            p <- p + ggalluvial::geom_alluvium(aes(fill = !!sym(links_fill_by), color = !!sym(links_fill_by)), width = nodes_width,
                alpha = links_alpha, na.rm = !keep_empty, ...) +
                scale_color_manual(guide = "none", values = links_colors)
        } else {
            p <- p + ggalluvial::geom_alluvium(aes(fill = !!sym(links_fill_by)), width = nodes_width, alpha = links_alpha,
                color = links_color, na.rm = !keep_empty, ...)
        }
    } else {
        if (identical(links_color, ".fill")) {
            # stratum changed to "stratum" after flow stat
            if (identical(stratum, links_fill_by)) {
                p <- p + ggalluvial::geom_flow(aes(fill = after_stat(!!sym("stratum")), color = after_stat(!!sym("stratum"))),
                    width = nodes_width, alpha = links_alpha, na.rm = !keep_empty, ...) +
                    scale_color_manual(guide = "none", values = links_colors)
            } else {

                p <- p + ggalluvial::geom_flow(aes(fill = !!sym(links_fill_by), color = !!sym(links_fill_by)), width = nodes_width,
                    alpha = links_alpha, na.rm = !keep_empty, ...) +
                    scale_color_manual(guide = "none", values = links_colors)
            }
        } else {
            if (identical(stratum, links_fill_by)) {
                p <- p + ggalluvial::geom_flow(aes(fill = after_stat(!!sym("stratum"))), width = nodes_width, alpha = links_alpha,
                    color = links_color, na.rm = !keep_empty, ...) +
                    scale_color_manual(guide = "none", values = links_colors)
            } else if (dots_n(...) == 0 || !"stat" %in% names(list(...))) {
                p <- p + ggalluvial::geom_flow(aes(fill = !!sym(links_fill_by)), width = nodes_width, alpha = links_alpha,
                    color = links_color, na.rm = !keep_empty, stat = "alluvium", ...)
            } else {
                warning(
                    "[SankeyPlot] You probably see no color filling for the links. ",
                    paste0("This is because 'flow' stat of ggalluvial::geom_flow loses '", links_fill_by, "' "),
                    "while building the plot. Please use 'stat = 'alluvium' instead.")
                p <- p + ggalluvial::geom_flow(aes(fill = !!sym(links_fill_by)), width = nodes_width, alpha = links_alpha,
                    color = links_color, na.rm = !keep_empty, ...)
            }
        }
    }

    p <- p +
        scale_fill_manual(
            name = links_name %||% links_fill_by,
            values = links_colors, breaks = levels(data[[links_fill_by]]),
            guide = links_guide) +
        new_scale_fill()

    if (identical(nodes_color, ".fill")) {
        p <- p +
            ggalluvial::geom_stratum(aes(fill = !!sym(stratum), color = !!sym(stratum)), alpha = nodes_alpha,
                width = nodes_width, na.rm = !keep_empty) +
            scale_color_manual(guide = "none", values = nodes_colors)
    } else {
        p <- p +
            ggalluvial::geom_stratum(aes(fill = !!sym(stratum)), alpha = nodes_alpha, width = nodes_width,
                color = nodes_color, na.rm = !keep_empty)
    }

    p <- p +
        scale_fill_manual(
            values = nodes_colors,
            breaks = levels(data[[stratum]]),
            guide = ifelse(nodes_legend %in% c("none", "separate"), "none", "legend")
        )

    if (isTRUE(nodes_label)) {
        p <- p + geom_label(
            aes(label = !!sym(stratum)),
            stat = ggalluvial::StatStratum,
            min.y = nodes_label_miny,
            size = text_size_scale * 3)
    }

    p <- p +
        scale_x_discrete(expand = expand$x) +
        scale_y_continuous(expand = expand$y) +
        do.call(theme, theme_args) +
        labs(title = title, subtitle = subtitle, x = xlab %||% x,
            y = ylab %||% ifelse(identical(y, ".y"), links_fill_by, "Frequency")) +
        ggplot2::theme(
            aspect.ratio = aspect.ratio,
            legend.position = legend.position,
            legend.direction = legend.direction,
            legend.box = legend.box,
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            axis.ticks.x = element_blank(),
            axis.text.x = element_text(angle = x_text_angle, hjust = just$h, vjust = just$v)
        )

    if (isTRUE(flip)) {
        p <- p + coord_flip()
        attr(p, "height") <- nlevels(data[[stratum]]) * ifelse(nlevels(data[[stratum]]) < 5, 2, 1.5)
        attr(p, "width") <- 6
    } else {
        attr(p, "height") <- 6
        attr(p, "width") <- nlevels(data[[x]]) * ifelse(nlevels(data[[x]]) < 5, 2, 1.5)
    }

    facet_plot(p, facet_by, facet_scales, facet_nrow, facet_ncol, facet_byrow,
        legend.position = legend.position, legend.direction = legend.direction)
}


#' Sankey / Alluvial Plot
#'
#' @description A plot visualizing flow/movement/change from one state to another or one time to another.
#'  `AlluvialPlot` is an alias of `SankeyPlot`.
#' @inheritParams common_args
#' @inheritParams SankeyPlotAtomic
#' @return A ggplot object or wrap_plots object or a list of ggplot objects
#' @export
#' @rdname sankeyplot
#' @examples
#' \donttest{
#' # Reproduce the examples in ggalluvial
#' set.seed(8525)
#'
#' data(UCBAdmissions, package = "datasets")
#' UCBAdmissions <- as.data.frame(UCBAdmissions)
#' SankeyPlot(as.data.frame(UCBAdmissions), x = c("Gender", "Dept"),
#'     y = "Freq", nodes_width = 1/12, links_fill_by = "Admit", nodes_label = TRUE,
#'     nodes_palette = "simspec", links_palette = "Set1", links_alpha = 0.5,
#'     nodes_palcolor = "black", links_color = "transparent")
#'
#' data(HairEyeColor, package = "datasets")
#' SankeyPlot(as.data.frame(HairEyeColor), x = c("Hair", "Eye", "Sex"),
#'     y = "Freq", links_fill_by = "Eye", nodes_width = 1/8, nodes_alpha = 0.4,
#'     flip = TRUE, reverse = FALSE, knot.pos = 0, links_color = "transparent",
#'     ylab = "Freq", links_alpha = 0.5, links_name = "Eye (links)", links_palcolor = c(
#'         Brown = "#70493D", Hazel = "#E2AC76", Green = "#3F752B", Blue = "#81B0E4"))
#'
#' data(Refugees, package = "alluvial")
#' country_regions <- c(
#'     Afghanistan = "Middle East",
#'     Burundi = "Central Africa",
#'     `Congo DRC` = "Central Africa",
#'     Iraq = "Middle East",
#'     Myanmar = "Southeast Asia",
#'     Palestine = "Middle East",
#'     Somalia = "Horn of Africa",
#'     Sudan = "Central Africa",
#'     Syria = "Middle East",
#'     Vietnam = "Southeast Asia"
#' )
#' Refugees$region <- country_regions[Refugees$country]
#' SankeyPlot(Refugees, x = "year", y = "refugees", alluvium = "country",
#'     links_fill_by = "country", links_color = ".fill", links_alpha = 0.75,
#'     links_palette = "Set3", facet_by = "region", x_text_angle = -45, nodes_legend = "none",
#'     theme_args = list(strip.background = ggplot2::element_rect(fill="grey80")),
#'     decreasing = FALSE, nodes_width = 0, nodes_color = "transparent", ylab = "refugees",
#'     title = "Refugee volume by country and region of origin")
#'
#' data(majors, package = "ggalluvial")
#' majors$curriculum <- as.factor(majors$curriculum)
#' SankeyPlot(majors, x = "semester", stratum = "curriculum", alluvium = "student",
#'     links_fill_by = "curriculum", flow = TRUE, stat = "alluvium", nodes_palette = "Set2",
#'     links_palette = "Set2")
#'
#' data(vaccinations, package = "ggalluvial")
#' vaccinations <- transform(vaccinations,
#'     response = factor(response, rev(levels(response))))
#' SankeyPlot(vaccinations, x = "survey", stratum = "response", alluvium = "subject",
#'     y = "freq", links_fill_by = "response", nodes_label = TRUE, nodes_alpha = 0.5,
#'     nodes_palette = "seurat", links_palette = "seurat", links_alpha = 0.5,
#'     legend.position = "none", flow = TRUE, expand = c(0, 0, 0, .15), stat = "alluvium",
#'     title = "vaccination survey responses at three points in time")
#'
#' data(Titanic, package = "datasets")
#' SankeyPlot(as.data.frame(Titanic), x = c("Class", "Sex"), y = "Freq",
#'     links_fill_by = "Survived", flow = TRUE, facet_by = "Age", facet_scales = "free_y",
#'     nodes_label = TRUE, expand = c(0.05, 0), xlab = "", links_palette = "Set1",
#'     nodes_palcolor = "white", nodes_label_miny = 10)
#'
#' # Simulated examples
#' df <- data.frame(
#'     Clone = paste0("clone", 1:10),
#'     Timepoint1 = sample(c(rep(0, 30), 1:100), 10),
#'     Timepoint2 = sample(c(rep(0, 30), 1:100), 10)
#' )
#' SankeyPlot(df, x = c("Timepoint1", "Timepoint2"), alluvium = "Clone",
#'     links_color = ".fill")
#'
#' df <- data.frame(
#'     Clone = rep(paste0("clone", 1:6), each = 2),
#'     Timepoint1 = sample(c(rep(0, 30), 1:100), 6),
#'     Timepoint2 = sample(c(rep(0, 30), 1:100), 6),
#'     Group = rep(c("A", "B"), 6)
#' )
#' SankeyPlot(df, x = c(".", "Timepoint1", "Timepoint2"),
#'     stratum = "Group", links_fill_by = "Clone", links_color = ".fill")
#' }
SankeyPlot <- function(
    data, in_form = c("auto", "long", "lodes", "wide", "alluvia", "counts"), x, x_sep = "_", y = NULL,
    stratum = NULL, stratum_sep = "_", alluvium = NULL, alluvium_sep = "_",
    split_by = NULL, split_by_sep = "_", keep_empty = TRUE, flow = FALSE, expand = c(0, 0, 0, 0),
    nodes_legend = c("auto", "separate", "merge", "none"), nodes_color = "grey30", links_fill_by = NULL,
    links_fill_by_sep = "_", links_name = NULL, links_color = "gray80", nodes_palette = "Paired", nodes_palcolor = NULL,
    nodes_alpha = 1, nodes_label = FALSE, nodes_label_miny = 0, nodes_width = 0.25, links_palette = "Paired",
    links_palcolor = NULL, links_alpha = 0.6, legend.box = "vertical", x_text_angle = 0, aspect.ratio = 1,
    legend.position = "right", legend.direction = "vertical", flip = FALSE, theme = "theme_this", theme_args = list(),
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL,
    facet_by = NULL, facet_scales = "fixed", facet_ncol = NULL, facet_nrow = NULL, facet_byrow = TRUE,
    seed = 8525, combine = TRUE, nrow = NULL, ncol = NULL, byrow = TRUE, ...
) {
    validate_common_args(seed, facet_by = facet_by)
    theme <- process_theme(theme)
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
            SankeyPlotAtomic(datas[[nm]],
                in_form = in_form, x = x, x_sep = x_sep, y = y, stratum = stratum, stratum_sep = stratum_sep,
                alluvium = alluvium, alluvium_sep = alluvium_sep, keep_empty = keep_empty, nodes_legend = nodes_legend,
                nodes_color = nodes_color, links_fill_by = links_fill_by, links_fill_by_sep = links_fill_by_sep,
                links_name = links_name, links_color = links_color, expand = expand, nodes_label_miny = nodes_label_miny,
                nodes_palette = nodes_palette, nodes_palcolor = nodes_palcolor, nodes_alpha = nodes_alpha,
                nodes_label = nodes_label, nodes_width = nodes_width, flow = flow,
                links_palette = links_palette, links_palcolor = links_palcolor, links_alpha = links_alpha,
                legend.box = legend.box, x_text_angle = x_text_angle, aspect.ratio = aspect.ratio,
                legend.position = legend.position, legend.direction = legend.direction, flip = flip,
                theme = theme, theme_args = theme_args, title = title, subtitle = subtitle, xlab = xlab, ylab = ylab,
                facet_by = facet_by, facet_scales = facet_scales, facet_ncol = facet_ncol, facet_nrow = facet_nrow, facet_byrow = facet_byrow,
                ...
            )
        }
    )

    combine_plots(plots, combine = combine, nrow = nrow, ncol = ncol, byrow = byrow)
}


#' @rdname sankeyplot
#' @export
AlluvialPlot <- SankeyPlot
