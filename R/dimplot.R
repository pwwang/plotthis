#' Internal helper to create a 3D dimension reduction plot using plotly
#'
#' @description
#' Renders an interactive 3D scatter plot via **plotly** for dimension reduction
#' data with three ordination axes. Called automatically by `DimPlotAtomic()`
#' when `length(dims) == 3`.
#'
#' The function supports two visualisation modes:
#' - **group_by** — discrete group colouring with one trace per group, matching
#'   the 2D legend behaviour via `label_use`.
#' - **features** — continuous colour scale with a `plotly` colorscale bar.
#'
#' Graphs / networks are rendered as 3D line segments (with NA separators for
#' line breaks). Lineage curves are fitted via `stats::loess()` per lineage
#' column and drawn as smoothed paths. For datasets exceeding 100 000 points,
#' per-point hover text is disabled to reduce the JSON payload.
#'
#' @section Architecture:
#' **DimPlotAtomic3D** executes the following steps:
#'
#' 1. **plotly availability check** — errors early if the `plotly` package is
#'    not installed.
#' 2. **Axis labels** — defaults `xlab` / `ylab` / `zlab` from `dims`.
#' 3. **Marker size scaling** — multiplies `pt_size` by 2 to convert from
#'    ggplot2 mm units to plotly pixel units.
#' 4. **Large-data detection** — disables per-point hover text when
#'    `nrow(data) > 1e5`.
#' 5. **Base plotly object** — `plotly::plot_ly()` initialisation.
#' 6. **Graph / network edges** (if `graph` is provided) — resolves the graph
#'    source (`@attribute`, Graph object, matrix, data.frame, column indices,
#'    or column names), aligns row/column names with `data`, sets upper-triangle
#'    and zero entries to `NA`, melts into an edge data frame, builds x/y/z
#'    coordinate vectors (interleaved with NA for line breaks), and adds
#'    `scatter3d` line traces.
#' 7. **Main scatter traces**:
#'    - *group_by mode*: iterates over `labels_tb` entries, creating one trace
#'      per group with the resolved colour. NA groups use `bg_color`. Legend
#'      labels use `label_use` (with count annotations if `show_stat = TRUE`).
#'    - *features mode*: builds a plotly-style colorscale from the continuous
#'      palette. NA points are rendered first in `bg_color` without a colour
#'      bar; non-NA points follow with a `showscale = TRUE` colour bar.
#' 8. **Highlight** — if `highlight` is specified, highlighted points are
#'    plotted as open circles with increased marker size (to account for
#'    stroke). Supports `TRUE` (all points), a filter expression string, or
#'    a vector of row names / numeric indices.
#' 9. **Lineages** — each lineage column is validated, trimmed to
#'    `[lineages_trim[1], lineages_trim[2]]` quantiles, ordered by value,
#'    and a `loess(span = lineages_span, degree = 2)` smooth is fitted per
#'    dimension. The smoothed curve is rendered as a 3D line trace.
#' 10. **Labels** — when `label = TRUE` and `group_by` is set, group median
#'     coordinates are computed via `aggregate()`. In 3D, plotly does not
#'     support text background/outline, so `label_bg` is used directly as
#'     the text colour. Text size is scaled by 3× for plotly.
#' 11. **Layout** — subtitle is composited with an optional "Showing N sampled
#'     points" note. The full title is rendered as HTML (`<br><sup>...</sup>`).
#'     The scene sets `aspectratio = list(x = 1, y = 1, z = 1)` and axis
#'     titles.
#'
#' @inheritParams DimPlotAtomic
#' @param colorby A character string naming the column used for colour mapping
#'   (either the `group_by` factor or the `features` numeric column).
#' @param colors A named character vector mapping group levels to hex colours.
#'   Only used in group_by mode.
#' @param feat_colors_value A numeric vector of the colour-scale anchor values
#'   for feature mode, as returned by `prepare_continuous_color_scale()`.
#' @param label_use A character vector of formatted legend labels (with
#'   optional count annotations) used in group_by mode.
#' @param labels_tb A table of group counts for trace iteration in group_by
#'   mode.
#' @param keep_empty_group A logical value. If `TRUE`, empty factor levels are
#'   preserved; traces with zero rows are still added to the legend.
#' @param n_sampled An integer indicating how many points were retained after
#'   down-sampling (displayed in the subtitle). `NULL` if no sampling occurred.
#' @keywords internal
DimPlotAtomic3D <- function(
    data,
    dims,
    group_by = NULL,
    features = NULL,
    colorby,
    colors = NULL,
    feat_colors_value = NULL,
    label_use = NULL,
    labels_tb = NULL,
    keep_empty_group = FALSE,
    bg_color = "grey80",
    color_name = "",
    pt_size = NULL,
    pt_alpha = 1,
    show_stat = TRUE,
    title = NULL,
    subtitle = NULL,
    xlab = NULL,
    ylab = NULL,
    label = FALSE,
    label_insitu = FALSE,
    label_size = 4,
    label_fg = "white",
    label_bg = "black",
    highlight = NULL,
    highlight_color = "black",
    highlight_size = 1,
    highlight_stroke = 0.8,
    highlight_alpha = 1,
    graph = NULL,
    edge_size = c(0.05, 0.5),
    edge_alpha = 0.1,
    edge_color = "grey40",
    lineages = NULL,
    lineages_trim = c(0.01, 0.99),
    lineages_span = 0.75,
    lineages_palette = "Dark2",
    lineages_palcolor = NULL,
    palette = "Spectral",
    palcolor = NULL,
    palreverse = FALSE,
    n_sampled = NULL
) {
    if (!requireNamespace("plotly", quietly = TRUE)) {
        stop(
            "Package 'plotly' is required for 3D plots. Install it with: install.packages('plotly')"
        )
    }

    xlab <- xlab %||% dims[1]
    ylab <- ylab %||% dims[2]
    zlab <- dims[3]

    # Scale pt_size for plotly (plotly marker sizes are in pixels, ggplot sizes are in mm)
    marker_size <- max(pt_size * 2, 1)

    # Disable per-point hover text for large datasets to reduce JSON payload
    large_data <- nrow(data) > 1e5
    hover_mode <- if (large_data) "none" else "text"

    p <- plotly::plot_ly()

    ## Graph / network edges (rendered first so they appear behind points)
    if (!is.null(graph)) {
        if (
            is.character(graph) && length(graph) == 1 && startsWith(graph, "@")
        ) {
            graph_name <- substring(graph, 2)
            net_mat <- attr(data, graph_name)
            if (is.null(net_mat)) {
                stop(paste0(
                    "[DimPlot] The graph '",
                    graph_name,
                    "' is not found in the data attributes."
                ))
            }
        } else if (inherits(graph, "Graph")) {
            net_mat <- as.matrix(graph)
        } else if (
            is.matrix(graph) ||
                is.data.frame(graph) ||
                inherits(graph, "dgCMatrix")
        ) {
            net_mat <- graph
        } else if (is.numeric(graph)) {
            graph_cols <- colnames(data)[graph]
            net_mat <- data[graph_cols]
        } else if (is.character(graph)) {
            net_mat <- data[graph]
        } else {
            stop(
                "[DimPlot] The 'graph' should be a matrix, data.frame, Graph object, indexes, or column names."
            )
        }

        if (!is.matrix(net_mat)) {
            net_mat <- as.matrix(net_mat)
        }

        if (!is.null(rownames(net_mat)) && !is.null(colnames(net_mat))) {
            net_mat <- net_mat[rownames(data), rownames(data)]
        } else if (is.null(rownames(net_mat))) {
            stop("The graph matrix should have rownames and colnames.")
        } else if (is.null(rownames(data))) {
            stop("'graph' requires the input data to have rownames.")
        }
        net_mat[net_mat == 0] <- NA
        net_mat[upper.tri(net_mat)] <- NA

        net_df <- reshape2::melt(
            net_mat,
            na.rm = TRUE,
            stringsAsFactors = FALSE
        )
        net_df$value <- as.numeric(net_df$value)
        net_df$Var1 <- as.character(net_df$Var1)
        net_df$Var2 <- as.character(net_df$Var2)

        # Build edge coordinates with NA separators for line breaks
        edge_x <- as.vector(rbind(
            data[net_df$Var1, dims[1]],
            data[net_df$Var2, dims[1]],
            NA
        ))
        edge_y <- as.vector(rbind(
            data[net_df$Var1, dims[2]],
            data[net_df$Var2, dims[2]],
            NA
        ))
        edge_z <- as.vector(rbind(
            data[net_df$Var1, dims[3]],
            data[net_df$Var2, dims[3]],
            NA
        ))

        # Scale edge width: use mean of edge_size range, scaled for plotly
        edge_width <- mean(edge_size) * 10

        p <- plotly::add_trace(
            p = p,
            x = edge_x,
            y = edge_y,
            z = edge_z,
            type = "scatter3d",
            mode = "lines",
            line = list(color = edge_color, width = edge_width),
            opacity = edge_alpha,
            name = "edges",
            showlegend = FALSE,
            hoverinfo = "none"
        )
    }

    ## Main scatter traces
    if (!is.null(group_by)) {
        # Group-by mode: one trace per group for consistent legend with 2D (label_use)
        for (i in seq_along(labels_tb)) {
            grp <- names(labels_tb)[i]
            is_na_grp <- grp == "NA"

            if (is_na_grp) {
                grp_data <- data[is.na(data[[group_by]]), , drop = FALSE]
                grp_color <- bg_color
            } else {
                grp_data <- data[
                    !is.na(data[[group_by]]) &
                        as.character(data[[group_by]]) == grp,
                    ,
                    drop = FALSE
                ]
                grp_color <- colors[grp]
            }

            if (nrow(grp_data) == 0 && !isTRUE(keep_empty_group)) {
                next
            }

            legend_label <- if (!is.null(label_use)) label_use[i] else grp

            trace_args <- list(
                p = p,
                x = grp_data[[dims[1]]],
                y = grp_data[[dims[2]]],
                z = grp_data[[dims[3]]],
                type = "scatter3d",
                mode = "markers",
                marker = list(
                    size = marker_size,
                    color = grp_color,
                    opacity = pt_alpha
                ),
                name = legend_label,
                legendgroup = grp,
                showlegend = TRUE,
                hoverinfo = hover_mode
            )
            if (!large_data) {
                trace_args$text <- if (is_na_grp) {
                    paste0("Cell: ", rownames(grp_data))
                } else {
                    paste0("Cell: ", rownames(grp_data), "\nGroup: ", grp)
                }
            }
            p <- do_call(plotly::add_trace, trace_args)
        }
    } else {
        # Feature mode: continuous coloring
        feat_colors <- palette_this(
            palette = palette,
            palcolor = palcolor,
            type = "continuous",
            reverse = palreverse
        )
        n_colors <- length(feat_colors)
        plotly_colorscale <- lapply(seq_len(n_colors), function(i) {
            list((i - 1) / (n_colors - 1), feat_colors[i])
        })

        na_mask <- is.na(data[[features]])
        # Plot NA/background points
        if (any(na_mask)) {
            data_na <- data[na_mask, , drop = FALSE]
            na_trace_args <- list(
                p = p,
                x = data_na[[dims[1]]],
                y = data_na[[dims[2]]],
                z = data_na[[dims[3]]],
                type = "scatter3d",
                mode = "markers",
                marker = list(
                    size = marker_size,
                    color = bg_color,
                    opacity = pt_alpha
                ),
                name = "NA",
                showlegend = FALSE,
                hoverinfo = hover_mode
            )
            if (!large_data) {
                na_trace_args$text <- paste0("Cell: ", rownames(data_na))
            }
            p <- do_call(plotly::add_trace, na_trace_args)
        }
        # Plot non-NA points with colorscale
        data_valid <- data[!na_mask, , drop = FALSE]
        if (nrow(data_valid) > 0) {
            colorbar_title <- if (nchar(color_name) > 0) color_name else colorby
            feat_trace_args <- list(
                p = p,
                x = data_valid[[dims[1]]],
                y = data_valid[[dims[2]]],
                z = data_valid[[dims[3]]],
                type = "scatter3d",
                mode = "markers",
                marker = list(
                    size = marker_size,
                    color = data_valid[[features]],
                    colorscale = plotly_colorscale,
                    cmin = min(feat_colors_value, na.rm = TRUE),
                    cmax = max(feat_colors_value, na.rm = TRUE),
                    opacity = pt_alpha,
                    showscale = TRUE,
                    colorbar = list(
                        title = list(text = colorbar_title)
                    )
                ),
                showlegend = FALSE,
                hoverinfo = hover_mode
            )
            if (!large_data) {
                feat_trace_args$text <- paste0(
                    "Cell: ",
                    rownames(data_valid),
                    "\nValue: ",
                    round(data_valid[[features]], 3)
                )
            }
            p <- do_call(plotly::add_trace, feat_trace_args)
        }
    }

    ## Highlight
    if (!is.null(highlight)) {
        if (isTRUE(highlight)) {
            hi_df <- data
        } else if (length(highlight) == 1 && is.character(highlight)) {
            hi_df <- eval(parse(
                text = paste0("dplyr::filter(data, ", highlight, ")")
            ))
        } else {
            all_inst <- if (is.numeric(highlight)) {
                seq_len(nrow(data))
            } else {
                rownames(data)
            }
            if (!any(highlight %in% all_inst)) {
                stop("No highlight items found in the data (rownames).")
            }
            if (!all(highlight %in% all_inst)) {
                warning(
                    "Not all highlight items found in the data (rownames).",
                    immediate. = TRUE
                )
            }
            hi_df <- data[intersect(highlight, all_inst), , drop = FALSE]
        }
        if (nrow(hi_df) > 0) {
            hi_trace_args <- list(
                p = p,
                x = hi_df[[dims[1]]],
                y = hi_df[[dims[2]]],
                z = hi_df[[dims[3]]],
                type = "scatter3d",
                mode = "markers",
                marker = list(
                    size = highlight_size * 2 + highlight_stroke,
                    color = highlight_color,
                    opacity = highlight_alpha,
                    symbol = "circle-open"
                ),
                name = "highlight",
                showlegend = FALSE,
                hoverinfo = hover_mode
            )
            if (!large_data) {
                hi_trace_args$text <- paste0("Cell: ", rownames(hi_df))
            }
            p <- do_call(plotly::add_trace, hi_trace_args)
        }
    }

    ## Lineages
    if (!is.null(lineages)) {
        lineages <- unique(check_columns(
            data,
            lineages,
            force_factor = FALSE,
            allow_multi = TRUE
        ))
        lineage_colors <- palette_this(
            lineages,
            palette = lineages_palette,
            palcolor = lineages_palcolor,
            reverse = palreverse
        )

        for (l in lineages) {
            trim_pass <- (data[[l]] >
                quantile(data[[l]], lineages_trim[1], na.rm = TRUE) &
                data[[l]] < quantile(data[[l]], lineages_trim[2], na.rm = TRUE))
            na_pass <- !is.na(data[[l]])
            index <- which(trim_pass & na_pass)
            index <- index[order(data[index, l])]
            dat_sub <- data[index, , drop = FALSE]
            weights_used <- rep(1, nrow(dat_sub))

            fitted_vals <- lapply(dims, function(x) {
                loess(
                    formula(paste(x, l, sep = "~")),
                    weights = weights_used,
                    data = dat_sub,
                    span = lineages_span,
                    degree = 2
                )$fitted
            })
            names(fitted_vals) <- dims
            dat_smooth <- as.data.frame(fitted_vals)
            dat_smooth <- unique(na.omit(dat_smooth))

            if (nrow(dat_smooth) > 0) {
                p <- plotly::add_trace(
                    p = p,
                    x = dat_smooth[[dims[1]]],
                    y = dat_smooth[[dims[2]]],
                    z = dat_smooth[[dims[3]]],
                    text = paste0("Lineage: ", l),
                    type = "scatter3d",
                    mode = "lines",
                    line = list(width = 6, color = lineage_colors[l]),
                    name = l,
                    showlegend = TRUE,
                    hoverinfo = "text"
                )
            }
        }
    }

    ## Labels
    if (isTRUE(label) && !is.null(group_by)) {
        # Compute label positions at group median coordinates (same as 2D)
        label_data <- data[!is.na(data[[group_by]]), , drop = FALSE]
        label_df <- aggregate(
            label_data[, dims, drop = FALSE],
            by = list(label_data[[group_by]]),
            FUN = median
        )
        colnames(label_df)[1] <- ".group"
        if (!isTRUE(label_insitu)) {
            label_df[, ".label"] <- seq_len(nrow(label_df))
        } else {
            label_df[, ".label"] <- label_df[, ".group"]
        }
        # In 3D, plotly doesn't support text background/outline like geom_text_repel,
        # so we use label_bg as the text color for visibility
        p <- plotly::add_trace(
            p = p,
            x = label_df[[dims[1]]],
            y = label_df[[dims[2]]],
            z = label_df[[dims[3]]],
            text = as.character(label_df[[".label"]]),
            type = "scatter3d",
            mode = "text",
            textfont = list(
                size = label_size * 3,
                color = label_bg
            ),
            textposition = "middle center",
            hoverinfo = "text",
            showlegend = FALSE
        )
    }

    ## Layout
    # Use subtitle from parent (already includes show_stat "N = ..." if applicable)
    subtitle_text <- subtitle %||% ""
    if (!is.null(n_sampled)) {
        sample_note <- paste0("Showing ", n_sampled, " sampled points")
        if (nchar(subtitle_text) > 0) {
            subtitle_text <- paste0(subtitle_text, "; ", sample_note)
        } else {
            subtitle_text <- sample_note
        }
    }
    title_text <- title %||% ""
    if (nchar(title_text) > 0 && nchar(subtitle_text) > 0) {
        full_title <- paste0(title_text, "<br><sup>", subtitle_text, "</sup>")
    } else if (nchar(subtitle_text) > 0) {
        full_title <- paste0("<sup>", subtitle_text, "</sup>")
    } else {
        full_title <- title_text
    }

    p <- plotly::layout(
        p = p,
        title = list(
            text = full_title,
            font = list(size = 16, color = "black"),
            y = 0.95
        ),
        showlegend = TRUE,
        legend = list(
            itemsizing = "constant",
            y = 0.5,
            x = 1,
            xanchor = "left"
        ),
        scene = list(
            xaxis = list(title = xlab),
            yaxis = list(title = ylab),
            zaxis = list(title = zlab),
            aspectratio = list(x = 1, y = 1, z = 1)
        )
    )

    return(p)
}

#' Atomic Dimension Reduction Plot without splitting the data
#'
#' @description
#' Core implementation for dimension reduction visualisation. This is the
#' internal workhorse dispatched by both `DimPlot()` (group-based) and
#' `FeatureDimPlot()` (continuous feature expression). It renders a 2D
#' scatter plot of ordination axes with extensive annotation capabilities,
#' and automatically delegates to `DimPlotAtomic3D()` for interactive 3D
#' plots when three `dims` are provided.
#'
#' The function supports two primary colouring modes:
#' - **group_by** — discrete factor colouring with a legend of group levels,
#'   optional density/statistical overlays, and group mark / label annotations.
#' - **features** — continuous numeric colouring with a gradient colour bar,
#'   multi-feature faceting via `tidyr::pivot_longer()`, and optional cutoff
#'   / quantile trimming via `prepare_continuous_color_scale()`.
#'
#' Additional annotation layers include: graph / network edges drawn as
#' segments between connected nodes, 2D density contours (filled or outline),
#' group marks (hull, ellipse, rect, circle via `ggforce`), lineage curves
#' (LOESS-smoothed paths with optional whiskers), velocity / RNA-velocity
#' arrows (raw, grid, or stream via `VelocityPlot()`), statistical summary
#' mini-plots (pie, ring, bar, line) embedded at group centroids, background
#' points from other facets (faded context), and group labels with repulsion
#' (via `ggrepel::geom_text_repel()`).
#'
#' Rendering scales automatically: scatter points for small datasets,
#' `scattermore::geom_scattermore()` raster for `n > 1e5`, or hex-binned
#' aggregation (`stat_summary_hex()` / `geom_hex()`). Legend assembly uses
#' `cowplot::get_plot_component()` with independent guide-boxes for base
#' groups, lineages, velocity, and stat-by annotations — combined via
#' `rbind` / `cbind` and re-inserted with `add_grob()`.
#'
#' @section Architecture:
#' **DimPlotAtomic** executes the following steps:
#'
#' 1. **ggplot dispatch** — selects `gglogger::ggplot` or `ggplot2::ggplot`.
#' 2. **Order validation** — `match.arg(order)`.
#' 3. **Dimension resolution** — converts numeric indices to column names,
#'    validates exactly 2 or 3 dims via `check_columns()`.
#' 4. **Column resolution** — validates `group_by` (force_factor, concat_multi),
#'    `features`, and `facet_by`. Requires at least one of `group_by` or
#'    `features`. Blocks `facet_by` when multiple features are present (features
#'    themselves become facets).
#' 5. **Auto defaults** — `pt_size` defaults to `min(3000 / nrow(data), 0.6)`;
#'    `raster` auto-enables when `nrow(data) > 1e5`; `theme_blank` suppresses
#'    axis labels.
#' 6. **Label force-enable** — if `label_repel` or `label_insitu` is `TRUE`,
#'    `label` is forced to `TRUE` with a message.
#' 7. **NA / empty handling** — `process_keep_na_empty()` filters the data;
#'    `keep_empty` values extracted for group_by and facet_by.
#' 8. **group_by preprocessing** — group values / colours resolved via
#'    `palette_this()`. NA levels mapped to literal `"NA"` string.
#'    `label_use` is constructed based on `label`, `label_insitu`, and
#'    `show_stat` combinations. Facet labeller is set to `as_labeller()` with
#'    per-facet N annotations when `show_stat = TRUE`.
#' 9. **Multi-feature pivot** — when `length(features) > 1`, data is pivoted
#'    to long format via `tidyr::pivot_longer()` with `.feature` as the facet
#'    variable and `.value` as the numeric column.
#' 10. **Continuous colour scale** — when `features` is set,
#'     `prepare_continuous_color_scale()` applies quantile/cutoff trimming
#'     and returns anchor values for the gradient.
#' 11. **Point ordering** — applies the selected `order` strategy (as-is,
#'     reverse, high-top, low-top, random) by sorting or shuffling rows.
#' 12. **3D branch** — if `length(dims) == 3`, emits a warning for unsupported
#'     features, optionally down-samples with stratified group sampling (when
#'     `raster = TRUE`), and delegates to `DimPlotAtomic3D()`.
#' 13. **Group marks** (2D only, when `add_mark = TRUE`) — dispatches to
#'     `ggforce::geom_mark_hull/ellipse/rect/circle`, with `new_scale_fill()`
#'     / `new_scale_color()` to isolate mark scales.
#' 14. **Graph / network** (2D only) — resolves the graph source (same logic
#'     as 3D), melts the matrix, handles faceted data by per-facet edge
#'     splitting, and adds `geom_segment(aes(linewidth = value))` with
#'     `scale_linewidth_continuous()`.
#' 15. **Density overlay** (2D only) — filled (`stat_density_2d(geom = "raster")`)
#'     or outline (`geom_density_2d()`).
#' 16. **Base scales and theme** — x / y limits from data range,
#'     `do_call(theme, theme_args)`, aspect ratio and legend position.
#' 17. **Background points for facet context** — for faceted plots (excluding
#'     multi-feature auto-faceting), points from other facets are added as
#'     faded background (raster / hex / point depending on settings).
#' 18. **Main point layer** — dispatches by rendering mode:
#'     - *raster*: `scattermore::geom_scattermore()` with separate NA / non-NA
#'       group layers.
#'     - *hex*: `geom_hex()` (group_by with optional count alpha, or
#'       `stat_summary_hex()` for features). Raises `has_fill = TRUE`.
#'     - *standard*: `geom_point()`.
#' 19. **Highlight** (2D only) — resolves highlight specification (TRUE, filter
#'     expression, row names, indices), errors on hex + highlight combo, renders
#'     highlight points with stroke (outer ring + inner colour).
#' 20. **Colour scales** — `scale_color_manual()` for group_by (with
#'     `keep_empty`-aware `breaks`/`limits`/`drop`), or
#'     `scale_color_gradientn()` for features. `scale_fill_manual()` /
#'     `scale_fill_gradientn()` added when `has_fill` is TRUE.
#' 21. **Base legend** — extracted via `cowplot::get_plot_component("guide-box-bottom")`.
#' 22. **Lineages** (2D only, no facet_by) — per-lineage LOESS fitting
#'     (`span = lineages_span, degree = 2`) with optional whiskers connecting
#'     smoothed to raw coordinates. Rendered as `geom_path()` with background
#'     stroke + foreground colour, plus `scale_color_manual()`. Legend extracted
#'     as a separate guide-box.
#' 23. **Velocity** (2D only, no facet_by) — delegates to `VelocityPlot()` with
#'     `return_layer = TRUE`. Adds `new_scale_color()` when the velocity layer
#'     has its own colour scale. Legend extracted separately.
#' 24. **Stat-by mini-plots** (2D only, no facet_by, no features) — dispatches
#'     to `PieChart()` / `RingPlot()` / `BarPlot()` / `LinePlot()` via
#'     `do_call()`. Each mini-plot is rendered as `annotation_custom()` at the
#'     group's median coordinates, scaled by `stat_plot_size * range`.
#'     Legend extracted from the stat plot.
#' 25. **Group labels** (2D only, no features) — `geom_text_repel()` with
#'     optional repulsion (`force = label_repulsion`) or fixed segments
#'     (`force = 0`, bold, `min.segment.length = 0`). Labels positioned at
#'     group median coordinates (computed per facet if applicable).
#' 26. **Legend assembly** — when additional legends exist (lineages, velocity,
#'     stat_by), they are combined with the base legend via `cbind` (vertical
#'     direction) or `rbind` (horizontal). The combined legend is re-inserted
#'     via `add_grob(gtable, legend, legend.position)`.
#' 27. **Dimension calculation** — `calculate_plot_dimensions(base_height = 5.5,
#'     aspect.ratio, legend_n, legend_nchar)` sets `height` / `width` attributes.
#' 28. **Faceting** — `facet_plot()` is called only when no additional legends
#'     were assembled (otherwise the combined-legend grob is returned directly).
#'
#' @inheritParams common_args
#' @inheritParams VelocityPlot
#' @param dims A character vector of the column names to plot on the x, y (and optionally z) axes or a numeric vector
#'  of the column indices. When 3 dimensions are provided, a 3D interactive plot is created using plotly.
#'  Supported in 3D: group_by, features, labels, highlight, lineages, graph/network, show_stat, order.
#'  Not supported in 3D: add_mark, stat_by, add_density, velocity, hex, facet_by, raster.
#' @param features A character vector of the column names to plot as features (continuous colouring).
#'  When multiple features are provided and `facet_by` is not set, the data is pivoted to long format and
#'  faceted by feature name.
#' @param group_by A character string of the column name to group the data by for discrete colouring.
#'  A character/factor column is expected. If multiple columns are provided, the columns will be concatenated with `group_by_sep`.
#' @param group_by_sep A character string to concatenate the columns in `group_by`, if multiple columns are provided.
#' @param pt_size A numeric value of the point size. If NULL (default), the point size is auto-calculated as
#'  `min(3000 / nrow(data), 0.6)` so large datasets automatically get smaller points.
#' @param pt_alpha A numeric value in `[0, 1]` for the point transparency. Default is `1`.
#' @param bg_color A character string specifying the colour used for NA-valued points and
#'  background context points drawn from other facets. Default is `"grey80"`.
#' @param label_insitu A logical value. If `TRUE`, the raw group names are placed at the group median
#'  coordinates instead of numeric indices. Forces `label = TRUE`. Default is `FALSE`.
#' @param show_stat A logical value. If `TRUE` (default), the number of points per group is shown
#'  in the legend labels and subtitle. Ignored when `theme = "theme_blank"`.
#' @param order A character string controlling the draw order of points:
#'  * `"as-is"` (default) — the row order in the data is preserved.
#'  * `"reverse"` — rows are reversed.
#'  * `"high-top"` — points with high values (last factor levels for group_by) are drawn last (on top).
#'  * `"low-top"` — points with low values (first factor levels) are drawn last.
#'  * `"random"` — rows are randomly shuffled.
#'
#'  For `high-top` and `low-top`, NA values are always plotted at the bottom.
#'  When applied to `group_by`, only the draw order changes — legend colours and order
#'  are unaffected. Within the same level, point order is preserved. For precise
#'  control, set factor levels before plotting.
#'  See <https://github.com/pwwang/scplotter/issues/29#issuecomment-3009694130> for examples.
#' @param label A logical value. If `TRUE`, group labels (numeric indices by default, or group
#'  names when `label_insitu = TRUE`) are placed at the median coordinates of each group.
#'  Forced to `TRUE` when `label_repel` or `label_insitu` is set.
#' @param label_size A numeric value for the label text size. Passed to `ggrepel::geom_text_repel()`.
#'  Default is `4`.
#' @param label_fg A character string for the label text (foreground) colour. Default is `"white"`.
#' @param label_bg A character string for the label background / outline colour. Default is `"black"`.
#' @param label_bg_r A numeric value for the background fill ratio of the label bounding box.
#'  Passed to `ggrepel::geom_text_repel(bg.r = ...)`. Default is `0.1`.
#' @param label_repel A logical value. If `TRUE`, labels are repelled from each other with force
#'  `label_repulsion`. A visible point anchor is drawn. Forces `label = TRUE`.
#' @param label_repulsion A numeric value for the repulsion force when `label_repel = TRUE`.
#'  Passed to `ggrepel::geom_text_repel(force = ...)`. Default is `20`.
#' @param label_pt_size A numeric value for the size of the anchor point drawn when
#'  `label_repel = TRUE`. Default is `1`.
#' @param label_pt_color A character string for the colour of the label anchor point. Default is `"black"`.
#' @param label_segment_color A character string for the colour of the line segment connecting
#'  the label to the anchor. Used in non-repel mode (`label_repel = FALSE`) where
#'  `min.segment.length = 0`. Default is `"black"`.
#' @param highlight A specification for highlighted points:
#'  - `NULL` (default): no highlighting.
#'  - `TRUE`: highlight all points (adds a dark outline around every point).
#'  - A character string: a dplyr filter expression (e.g., `"clusters == 'Ductal'"`).
#'  - A character vector: row names to highlight.
#'  - A numeric vector: row indices to highlight.
#' @param highlight_alpha A numeric value in `[0, 1]` for the transparency of highlighted points.
#'  Default is `1`.
#' @param highlight_size A numeric value for the size of the inner (coloured) highlight point.
#'  Default is `1`.
#' @param highlight_color A character string for the colour of the outer highlight ring.
#'  Default is `"black"`.
#' @param highlight_stroke A numeric value for the thickness of the outer highlight ring (the
#'  difference between the outer ring size and `highlight_size`). Default is `0.8`.
#' @param add_mark A logical value. If `TRUE`, group boundaries are drawn around points using
#'  `ggforce` marks. Requires `group_by`. Only supported in 2D.
#' @param mark_type A character string specifying the mark shape. Options:
#'  `"hull"` (convex hull, default), `"ellipse"`, `"rect"`, or `"circle"`.
#' @param mark_expand A unit value for the outward expansion of the mark boundary.
#'  Passed to `ggforce::geom_mark_*(expand = ...)`. Default is `unit(3, "mm")`.
#' @param mark_alpha A numeric value in `[0, 1]` for the transparency of the mark fill.
#'  Default is `0.1`.
#' @param mark_linetype A numeric value for the line type of the mark boundary. Default is `1` (solid).
#' @param stat_by A character string naming a column used to compute per-group statistical summary
#'  mini-plots embedded at group centroid positions. Only supported with `group_by` (not `features`).
#'  Only supported in 2D without `facet_by`.
#' @param stat_plot_type A character string specifying the mini-plot type. Options:
#'  `"pie"` (default), `"ring"`, `"bar"`, or `"line"`.
#' @param stat_plot_size A numeric value for the size of the stat mini-plot, expressed as a
#'  fraction of the axis range. Default is `0.1`.
#' @param stat_args A list of additional arguments passed to the stat plot function
#'  (e.g., `list(palette = "Set1")`). Default is `list(palette = "Set1")`.
#' @param graph A specification for network / graph edges to overlay. Sources:
#'  - A character string starting with `"@"` (e.g., `"@graph"`): extracts the attribute
#'    named `"graph"` from `attributes(data)`.
#'  - A `Graph` object (e.g., Seurat): coerced to dense matrix via `as.matrix()`.
#'  - A `matrix`, `data.frame`, or `dgCMatrix`: used directly as the adjacency matrix.
#'  - Numeric indices or character column names: extracts columns from `data`.
#'  Edges are drawn for non-zero, lower-triangle entries. Requires `data` to have row names
#'  matching the matrix dimnames.
#' @param edge_size A numeric vector of length 2 specifying the range `[min, max]` for
#'  `scale_linewidth_continuous(range = ...)` applied to edge widths. Default is `c(0.05, 0.5)`.
#' @param edge_alpha A numeric value in `[0, 1]` for the transparency of graph edges.
#'  Default is `0.1`.
#' @param edge_color A character string for the colour of graph edges. Default is `"grey40"`.
#' @param add_density A logical value. If `TRUE`, a 2D density layer is overlaid. Only supported in 2D.
#' @param density_color A character string for the colour of the density contour lines.
#'  Used when `density_filled = FALSE`. Default is `"grey80"`.
#' @param density_filled A logical value. If `TRUE`, the density is rendered as a filled raster
#'  (`stat_density_2d(geom = "raster")`) instead of contour lines. A separate fill scale is used.
#' @param density_filled_palette A character string naming the palette for the filled density layer.
#'  Default is `"Greys"`.
#' @param density_filled_palcolor A character vector of specific colours for the filled density
#'  palette. Default is `NULL` (auto-resolved from `density_filled_palette`).
#' @param lineages A character vector of column names representing pseudotime / trajectory lineages.
#'  Each column is fitted with a LOESS smooth (`span = lineages_span, degree = 2`) across the
#'  2D embedding, after trimming the top and bottom `lineages_trim` quantiles.
#'  Only supported in 2D without `facet_by`.
#' @param lineages_trim A numeric vector of length 2 specifying the lower and upper quantile
#'  thresholds `[0, 1]` for trimming lineage values before LOESS fitting.
#'  Default is `c(0.01, 0.99)`.
#' @param lineages_span A numeric value passed as `span` to `stats::loess()` controlling the
#'  smoothness of the lineage curve. Smaller values follow the data more closely.
#'  Default is `0.75`.
#' @param lineages_palette A character string naming the palette for lineage colours.
#'  Default is `"Dark2"`.
#' @param lineages_palcolor A character vector of specific colours for lineage curves.
#'  Default is `NULL` (auto-resolved from `lineages_palette`).
#' @param lineages_arrow A ggplot2 `arrow` specification applied to the end of lineage paths.
#'  Default is `arrow(length = unit(0.1, "inches"))`.
#' @param lineages_linewidth A numeric value for the width of the lineage curve lines.
#'  Default is `1`.
#' @param lineages_line_bg A character string for the colour of the background (wider) stroke
#'  drawn behind each lineage curve for improved visibility. Default is `"white"`.
#' @param lineages_line_bg_stroke A numeric value for the additional width of the background
#'  stroke relative to `lineages_linewidth`. The background line has total width
#'  `lineages_linewidth + lineages_line_bg_stroke`. Default is `0.5`.
#' @param lineages_whiskers A logical value. If `TRUE`, short line segments connect the
#'  smoothed lineage curve to the original data coordinates of the fitted points.
#'  Default is `FALSE`.
#' @param lineages_whiskers_linewidth A numeric value for the width of the whisker lines.
#'  Default is `0.5`.
#' @param lineages_whiskers_alpha A numeric value in `[0, 1]` for the transparency of the
#'  whisker lines. Default is `0.5`.
#' @param velocity A specification for RNA-velocity arrows. Can be:
#'  - `NULL` (default): no velocity overlay.
#'  - A character / integer vector: column names or indices in `data` for the velocity embedding.
#'  - A data frame or matrix: the velocity embedding itself (must align with `data` rows).
#'  Only supported in 2D without `facet_by`.
#' @param velocity_plot_type A character string specifying the velocity rendering style.
#'  Options: `"raw"` (arrows from embedding), `"grid"` (grid-based arrows), or `"stream"`
#'  (streamlines). Default is `"raw"`.
#' @param velocity_n_neighbors A numeric value for the number of neighbours used in the
#'  velocity grid computation. Default is `NULL` (auto).
#' @param velocity_density A numeric value for the velocity kernel density bandwidth.
#'  Default is `1`.
#' @param velocity_smooth A numeric value for the velocity smoothing parameter.
#'  Default is `0.5`.
#' @param velocity_scale A numeric value for scaling the velocity arrows. Default is `1`.
#' @param velocity_min_mass A numeric value for the minimum cell mass threshold in velocity
#'  grid computation. Default is `1`.
#' @param velocity_cutoff_perc A numeric value for the velocity cutoff percentage.
#'  Default is `5`.
#' @param velocity_group_palette A character string naming the palette for velocity group
#'  colours (used in `"raw"` plot type). Default is `"Set2"`.
#' @param velocity_group_palcolor A character vector of specific colours for velocity groups.
#'  Default is `NULL` (auto-resolved from `velocity_group_palette`).
#' @param raster A logical value. If `TRUE`, points are rendered via
#'  `scattermore::geom_scattermore()` for efficient rasterised plotting.
#'  Default is `NULL`, which auto-enables when `nrow(data) > 1e5`.
#' @param raster_dpi A numeric vector of length 2 `[x_dpi, y_dpi]` specifying the raster
#'  resolution in pixels. Passed to `scattermore::geom_scattermore(pixels = ...)`.
#'  Default is `c(512, 512)`. If a single value is provided it is recycled to both dimensions.
#' @param hex A logical value. If `TRUE`, points are rendered as hexagonal bins via
#'  `geom_hex()` / `stat_summary_hex()`. Not supported with highlight.
#'  Default is `FALSE`. Only supported in 2D.
#' @param hex_linewidth A numeric value for the width of the hexagon boundary lines.
#'  Default is `0.5`.
#' @param hex_count A logical value. If `TRUE` and `group_by` is set, hex fill alpha
#'  is mapped to `after_stat(count)` so denser bins are more opaque.
#'  For `features` mode hex_count is ignored. Default is `!is.null(group_by)`.
#' @param hex_bins A numeric value for the number of hex bins along each axis.
#'  Passed to `geom_hex(bins = ...)`. Default is `50`.
#' @param hex_binwidth A numeric value for the width of individual hex bins.
#'  Passed to `geom_hex(binwidth = ...)`. Takes precedence over `hex_bins` when set.
#' @param bg_cutoff A numeric threshold. Feature values with absolute value below this
#'  cutoff are set to `NA` (and therefore rendered in `bg_color`). Default is `NULL`.
#' @param color_name A character string used as the title for the continuous colour bar
#'  in feature mode. Default is `""`.
#' @return A ggplot object or a plotly object (when 3 dimensions are provided)
#' @keywords internal
#' @importFrom scales rescale
#' @importFrom stats loess formula na.omit aggregate
#' @importFrom dplyr %>% group_by group_map pull filter
#' @importFrom tidyr pivot_longer
#' @importFrom ggplot2 scale_linewidth_continuous geom_density_2d stat_density_2d geom_hex
#' @importFrom ggplot2 ggplotGrob theme_void as_labeller stat_summary_hex
#' @importFrom cowplot get_plot_component
DimPlotAtomic <- function(
    data,
    dims = 1:2,
    group_by = NULL,
    group_by_sep = "_",
    features = NULL,
    lower_quantile = 0,
    upper_quantile = 0.99,
    lower_cutoff = NULL,
    upper_cutoff = NULL,
    pt_size = NULL,
    pt_alpha = 1,
    bg_color = "grey80",
    bg_cutoff = NULL,
    color_name = "",
    label_insitu = FALSE,
    show_stat = !identical(theme, "theme_blank"),
    label = FALSE,
    label_size = 4,
    label_fg = "white",
    label_bg = "black",
    label_bg_r = 0.1,
    label_repel = FALSE,
    label_repulsion = 20,
    label_pt_size = 1,
    label_pt_color = "black",
    label_segment_color = "black",
    order = c("as-is", "reverse", "high-top", "low-top", "random"),
    highlight = NULL,
    highlight_alpha = 1,
    highlight_size = 1,
    highlight_color = "black",
    highlight_stroke = 0.8,
    add_mark = FALSE,
    mark_type = c("hull", "ellipse", "rect", "circle"),
    mark_expand = unit(3, "mm"),
    mark_alpha = 0.1,
    mark_linetype = 1,
    stat_by = NULL,
    stat_plot_type = c("pie", "ring", "bar", "line"),
    stat_plot_size = 0.1,
    stat_palette = "Set1",
    stat_args = list(),
    graph = NULL,
    edge_size = c(0.05, 0.5),
    edge_alpha = 0.1,
    edge_color = "grey40",
    add_density = FALSE,
    density_color = "grey80",
    density_filled = FALSE,
    density_filled_palette = "Greys",
    density_filled_palcolor = NULL,
    lineages = NULL,
    lineages_trim = c(0.01, 0.99),
    lineages_span = 0.75,
    lineages_palette = "Dark2",
    lineages_palcolor = NULL,
    lineages_arrow = ggplot2::arrow(length = unit(0.1, "inches")),
    lineages_linewidth = 1,
    lineages_line_bg = "white",
    lineages_line_bg_stroke = 0.5,
    lineages_whiskers = FALSE,
    lineages_whiskers_linewidth = 0.5,
    lineages_whiskers_alpha = 0.5,
    velocity = NULL,
    velocity_plot_type = c("raw", "grid", "stream"),
    velocity_n_neighbors = NULL,
    velocity_density = 1,
    velocity_smooth = 0.5,
    velocity_scale = 1,
    velocity_min_mass = 1,
    velocity_cutoff_perc = 5,
    velocity_group_palette = "Set2",
    velocity_group_palcolor = NULL,
    arrow_angle = 20,
    arrow_color = "black",
    streamline_l = 5,
    streamline_minl = 1,
    streamline_res = 1,
    streamline_n = 15,
    arrow_alpha = 1,
    streamline_width = c(0, 0.8),
    streamline_alpha = 1,
    streamline_color = NULL,
    streamline_palette = "RdYlBu",
    streamline_palcolor = NULL,
    streamline_bg_color = "white",
    streamline_bg_stroke = 0.5,
    keep_na = FALSE,
    keep_empty = FALSE,
    facet_by = NULL,
    facet_scales = "fixed",
    facet_nrow = NULL,
    facet_ncol = NULL,
    facet_byrow = TRUE,
    title = NULL,
    subtitle = NULL,
    xlab = NULL,
    ylab = NULL,
    theme = "theme_this",
    theme_args = list(),
    aspect.ratio = 1,
    legend.position = "right",
    legend.direction = "vertical",
    raster = NULL,
    raster_dpi = c(512, 512),
    hex = FALSE,
    hex_linewidth = 0.5,
    hex_count = !is.null(group_by),
    hex_bins = 50,
    hex_binwidth = NULL,
    palette = ifelse(is.null(features), "Paired", "Spectral"),
    palcolor = NULL,
    palreverse = FALSE,
    seed = 8525,
    ...
) {
    ggplot <- if (getOption("plotthis.gglogger.enabled", FALSE)) {
        gglogger::ggplot
    } else {
        ggplot2::ggplot
    }
    order <- match.arg(order)
    ## Setting up the parameters
    if (is.numeric(dims)) {
        dims <- colnames(data)[dims]
    }
    if (!length(dims) %in% c(2, 3)) {
        stop("Only 2 or 3 dimensions are allowed for dimension reduction plot.")
    }
    dims <- check_columns(data, dims, allow_multi = TRUE)
    if (length(raster_dpi) == 1) {
        raster_dpi <- rep(raster_dpi, 2)
    }

    group_by <- check_columns(
        data,
        group_by,
        force_factor = TRUE,
        allow_multi = TRUE,
        concat_multi = TRUE,
        concat_sep = group_by_sep
    )
    features <- check_columns(data, features, allow_multi = TRUE)
    if (is.null(group_by) && is.null(features)) {
        stop("Either 'group_by' or 'features' should be specified.")
    }
    facet_by <- check_columns(
        data,
        facet_by,
        force_factor = TRUE,
        allow_multi = TRUE
    )
    if (length(features) > 1 && !is.null(facet_by)) {
        stop(
            "Cannot specify 'facet_by' with multiple features. The plot will be faceted by features."
        )
    }
    pt_size <- pt_size %||% min(3000 / nrow(data), 0.6)
    raster <- raster %||% (nrow(data) > 1e5)
    xlab <- xlab %||% dims[1]
    ylab <- ylab %||% dims[2]
    if (identical(theme, "theme_blank")) {
        theme_args[["xlab"]] <- xlab
        theme_args[["ylab"]] <- ylab
    }
    if ((isTRUE(label_repel) || isTRUE(label_insitu)) && !isTRUE(label)) {
        message(
            "Forcing label to be TRUE when label_repel or label_insitu is TRUE."
        )
        label <- TRUE
    }

    data <- process_keep_na_empty(data, keep_na, keep_empty)
    keep_empty_group <- if (!is.null(group_by)) keep_empty[[group_by]] else NULL
    keep_empty_facet <- if (!is.null(facet_by)) {
        keep_empty[[facet_by[1]]]
    } else {
        NULL
    }
    if (length(facet_by) > 1) {
        keep_empty_facet2 <- if (!is.null(facet_by[2])) {
            keep_empty[[facet_by[2]]]
        } else {
            NULL
        }
        stopifnot(
            "[(Feature)DimPlot] `keep_empty` for `facet_by` variables must be identical." = identical(
                keep_empty_facet,
                keep_empty_facet2
            )
        )
    }

    facet_labeller <- "label_value"
    if (!is.null(group_by)) {
        group_vals <- levels(data[[group_by]])
        if (anyNA(data[[group_by]])) {
            group_vals <- c(group_vals, NA)
        }

        colors <- palette_this(
            group_vals,
            palette = palette,
            palcolor = palcolor,
            NA_keep = TRUE,
            reverse = palreverse
        )
        names(colors)[is.na(names(colors))] <- "NA"
        group_vals[is.na(group_vals)] <- "NA"
        labels_tb <- table(data[[group_by]], useNA = "ifany")
        # labels_tb <- labels_tb[labels_tb != 0]
        # convert NA names to "NA"
        names(labels_tb)[is.na(names(labels_tb))] <- "NA"
        labels_tb <- labels_tb[group_vals]

        if (isTRUE(label_insitu)) {
            if (isTRUE(show_stat)) {
                label_use <- paste0(names(labels_tb), "(", labels_tb, ")")
            } else {
                label_use <- paste0(names(labels_tb))
            }
        } else {
            if (isTRUE(label)) {
                if (isTRUE(show_stat)) {
                    label_use <- paste0(
                        seq_along(labels_tb),
                        ": ",
                        names(labels_tb),
                        "(",
                        labels_tb,
                        ")"
                    )
                } else {
                    label_use <- paste0(
                        seq_along(labels_tb),
                        ": ",
                        names(labels_tb)
                    )
                }
            } else {
                if (isTRUE(show_stat)) {
                    label_use <- paste0(names(labels_tb), "(", labels_tb, ")")
                } else {
                    label_use <- paste0(names(labels_tb))
                }
            }
        }
        if (isTRUE(show_stat)) {
            if (is.null(facet_by)) {
                subtitle <- subtitle %||%
                    paste0("N = ", sum(!is.na(data[[group_by]])))
            } else if (length(facet_by) == 1) {
                facet_labeller <- as_labeller(function(values) {
                    sapply(values, function(v) {
                        data_sub <- data[data[[facet_by]] == v, , drop = FALSE]
                        paste0(
                            v,
                            " (N = ",
                            sum(!is.na(data_sub[[group_by]])),
                            ")"
                        )
                    })
                })
            }
        }
    }

    ## Making long data for features when length(features) > 1
    if (length(dims) == 3 && length(features) > 1) {
        stop(
            "Multiple features are not supported for 3D plots. ",
            "Use `split_by = TRUE` in `FeatureDimPlot()` to plot features separately with `combine = FALSE`."
        )
    }
    multifeats <- length(features) > 1
    if (multifeats) {
        data <- data %>%
            pivot_longer(
                cols = features,
                names_to = ".feature",
                values_to = ".value"
            )
        data$.feature <- factor(data$.feature, levels = features)
        facet_by <- ".feature"
        features <- ".value"
    }
    if (!is.null(features)) {
        result <- prepare_continuous_color_scale(
            data,
            features,
            lower_quantile = lower_quantile,
            upper_quantile = upper_quantile,
            lower_cutoff = lower_cutoff,
            upper_cutoff = upper_cutoff,
            bg_cutoff = bg_cutoff
        )
        data <- result$data
        feat_colors_value <- result$feat_colors_value
    }
    colorby <- ifelse(is.null(features), group_by, features)
    if (order == "reverse") {
        data <- data[nrow(data):1, , drop = FALSE]
    } else if (order == "high-top") {
        data <- dplyr::arrange(data, !is.na(!!sym(colorby)), !!sym(colorby))
    } else if (order == "low-top") {
        data <- dplyr::arrange(
            data,
            !is.na(!!sym(colorby)),
            dplyr::desc(!!sym(colorby))
        )
    } else if (order == "random") {
        data <- data[sample(nrow(data)), , drop = FALSE]
    }

    ## 3D plot using plotly
    if (length(dims) == 3) {
        unsupported_args <- c(
            if (isTRUE(add_mark)) "add_mark",
            if (!is.null(stat_by)) "stat_by",
            if (isTRUE(add_density)) "add_density",
            if (!is.null(velocity)) "velocity",
            if (isTRUE(hex)) "hex",
            if (!is.null(facet_by)) "facet_by"
        )
        if (length(unsupported_args) > 0) {
            warning(
                "The following features are not supported for 3D plots and will be ignored: ",
                paste(unsupported_args, collapse = ", "),
                immediate. = TRUE
            )
        }

        # Downsample for large datasets (analogous to raster in 2D)
        n_sampled <- NULL
        if (isTRUE(raster) && nrow(data) > prod(raster_dpi)) {
            set.seed(seed)
            max_pts <- prod(raster_dpi)
            if (!is.null(group_by)) {
                # Stratified sampling: proportional to group size, at least 1 per non-empty group
                grp_vals_all <- as.character(data[[group_by]])
                grp_vals_all[is.na(grp_vals_all)] <- "NA"
                grp_counts <- table(grp_vals_all)
                grp_n <- pmax(
                    1L,
                    as.integer(round(grp_counts / sum(grp_counts) * max_pts))
                )
                names(grp_n) <- names(grp_counts)
                sampled_idx <- unlist(lapply(names(grp_counts), function(g) {
                    idx <- which(grp_vals_all == g)
                    sample(idx, min(length(idx), grp_n[g]))
                }))
                data <- data[sort(sampled_idx), , drop = FALSE]
            } else {
                data <- data[sort(sample(nrow(data), max_pts)), , drop = FALSE]
            }
            n_sampled <- nrow(data)
        }

        return(DimPlotAtomic3D(
            data = data,
            dims = dims,
            group_by = group_by,
            features = features,
            colorby = colorby,
            colors = if (!is.null(group_by)) colors else NULL,
            feat_colors_value = if (!is.null(features)) {
                feat_colors_value
            } else {
                NULL
            },
            label_use = if (!is.null(group_by)) label_use else NULL,
            labels_tb = if (!is.null(group_by)) labels_tb else NULL,
            keep_empty_group = isTRUE(keep_empty_group),
            bg_color = bg_color,
            color_name = color_name,
            pt_size = pt_size,
            pt_alpha = pt_alpha,
            show_stat = show_stat,
            title = title,
            subtitle = subtitle,
            xlab = xlab,
            ylab = ylab,
            label = label,
            label_insitu = label_insitu,
            label_size = label_size,
            label_fg = label_fg,
            label_bg = label_bg,
            highlight = highlight,
            highlight_color = highlight_color,
            highlight_size = highlight_size,
            highlight_stroke = highlight_stroke,
            highlight_alpha = highlight_alpha,
            graph = graph,
            edge_size = edge_size,
            edge_alpha = edge_alpha,
            edge_color = edge_color,
            lineages = lineages,
            lineages_trim = lineages_trim,
            lineages_span = lineages_span,
            lineages_palette = lineages_palette,
            lineages_palcolor = lineages_palcolor,
            palette = palette,
            palcolor = palcolor,
            palreverse = palreverse,
            n_sampled = n_sampled
        ))
    }

    # Do we have fill scale?
    has_fill <- FALSE
    p <- ggplot(data)

    ## Adding the mark
    if (isTRUE(add_mark)) {
        if (is.null(group_by)) {
            stop("Cannot add mark without 'group_by'.")
        }
        # if (!requireNamespace("ggforce", quietly = TRUE)) {
        #     stop("'ggforce' package is required for adding mark to the plot.")
        # }
        mark_type <- match.arg(mark_type)
        mark_fun <- switch(
            mark_type,
            hull = ggforce::geom_mark_hull,
            ellipse = ggforce::geom_mark_ellipse,
            rect = ggforce::geom_mark_rect,
            circle = ggforce::geom_mark_circle
        )
        p <- p +
            mark_fun(
                data = data[!is.na(data[[group_by]]), , drop = FALSE],
                mapping = aes(
                    x = !!sym(dims[1]),
                    y = !!sym(dims[2]),
                    color = !!sym(group_by),
                    fill = !!sym(group_by)
                ),
                expand = mark_expand,
                alpha = mark_alpha,
                linetype = mark_linetype,
                show.legend = FALSE
            ) +
            scale_fill_manual(
                values = colors[names(labels_tb)],
                guide = "none"
            ) +
            scale_color_manual(
                values = colors[names(labels_tb)],
                guide = "none"
            ) +
            new_scale_fill() +
            new_scale_color()
    }

    ## Adding the graph/network
    if (!is.null(graph)) {
        if (
            is.character(graph) && length(graph) == 1 && startsWith(graph, "@")
        ) {
            graph <- substring(graph, 2)
            net_mat <- attr(data, graph)
            if (is.null(net_mat)) {
                stop(paste0(
                    "[DimPlot] The graph '",
                    graph,
                    "' is not found in the data attributes."
                ))
            }
        } else if (inherits(graph, "Graph")) {
            # SeuratObject Graph
            net_mat <- as.matrix(graph)
        } else if (
            is.matrix(graph) ||
                is.data.frame(graph) ||
                inherits(graph, "dgCMatrix")
        ) {
            net_mat <- graph
        } else if (is.numeric(graph)) {
            graph <- colnames(data)[graph]
            net_mat <- data[graph]
        } else if (is.character(graph)) {
            net_mat <- data[graph]
        } else {
            stop(
                "[DimPlot] The 'graph' should be a matrix, data.frame, Graph object, indexes, or column names."
            )
        }

        if (!is.matrix(net_mat)) {
            net_mat <- as.matrix(net_mat)
        }

        if (!is.null(rownames(net_mat)) && !is.null(colnames(net_mat))) {
            net_mat <- net_mat[rownames(data), rownames(data)]
        } else if (is.null(rownames(net_mat))) {
            stop("The graph matrix should have rownames and colnames.")
        } else if (is.null(rownames(data))) {
            stop("'graph' requires the input data to have rownames.")
        }
        net_mat[net_mat == 0] <- NA
        net_mat[upper.tri(net_mat)] <- NA

        handle_single_facet_value <- function(mat) {
            net_df <- reshape2::melt(
                mat,
                na.rm = TRUE,
                stringsAsFactors = FALSE
            )
            net_df$value <- as.numeric(net_df$value)
            net_df$Var1 <- as.character(net_df$Var1)
            net_df$Var2 <- as.character(net_df$Var2)
            net_df$x <- data[net_df$Var1, dims[1]]
            net_df$y <- data[net_df$Var1, dims[2]]
            net_df$xend <- data[net_df$Var2, dims[1]]
            net_df$yend <- data[net_df$Var2, dims[2]]
            return(net_df)
        }

        if (!is.null(facet_by)) {
            net_df <- do_call(
                rbind,
                lapply(split(data, data[, facet_by]), function(d) {
                    d <- handle_single_facet_value(net_mat[
                        rownames(d),
                        rownames(d)
                    ])
                    d[, facet_by] <- d[1, facet_by]
                    d
                })
            )
        } else {
            net_df <- handle_single_facet_value(net_mat)
        }

        p <- p +
            geom_segment(
                data = net_df,
                mapping = aes(
                    x = !!sym("x"),
                    y = !!sym("y"),
                    xend = !!sym("xend"),
                    yend = !!sym("yend"),
                    linewidth = !!sym("value")
                ),
                color = edge_color,
                alpha = edge_alpha,
                show.legend = FALSE
            ) +
            scale_linewidth_continuous(range = edge_size, guide = "none")
    }

    ## Adding the density plot
    if (isTRUE(add_density)) {
        if (isTRUE(density_filled)) {
            filled_color <- palette_this(
                palette = density_filled_palette,
                palcolor = density_filled_palcolor,
                reverse = palreverse
            )
            p <- p +
                stat_density_2d(
                    geom = "raster",
                    aes(
                        x = !!sym(dims[1]),
                        y = !!sym(dims[2]),
                        fill = after_stat(!!sym("density"))
                    ),
                    contour = FALSE,
                    inherit.aes = FALSE,
                    show.legend = FALSE
                ) +
                scale_fill_gradientn(name = "Density", colours = filled_color) +
                new_scale_fill()
        } else {
            p <- p +
                geom_density_2d(
                    aes(x = !!sym(dims[1]), y = !!sym(dims[2])),
                    color = density_color,
                    inherit.aes = FALSE,
                    show.legend = FALSE
                )
        }
    }

    ## Setting up the theme
    x_min <- min(data[[dims[1]]], na.rm = TRUE)
    x_max <- max(data[[dims[1]]], na.rm = TRUE)
    y_min <- min(data[[dims[2]]], na.rm = TRUE)
    y_max <- max(data[[dims[2]]], na.rm = TRUE)
    p <- p +
        labs(title = title, subtitle = subtitle, x = xlab, y = ylab) +
        scale_x_continuous(limits = c(x_min, x_max)) +
        scale_y_continuous(limits = c(y_min, y_max)) +
        do_call(theme, theme_args) +
        ggplot2::theme(
            aspect.ratio = aspect.ratio,
            legend.position = legend.position,
            legend.direction = legend.direction
        )

    ## Adding the background points for each facet
    if (!is.null(facet_by) && isFALSE(multifeats)) {
        # Add the points from other subplots as background
        all_fc_values <- distinct(data, !!!syms(facet_by))
        colnames(all_fc_values) <- paste0(".facet_", colnames(all_fc_values))
        fc_data <- expand_grid(data, all_fc_values)
        # Remove the points inside the facet
        fc_indicator <- TRUE
        for (fc in facet_by) {
            fc_indicator <- fc_indicator &
                (fc_data[[fc]] == fc_data[[paste0(".facet_", fc)]])
        }
        fc_data <- fc_data[!fc_indicator, , drop = FALSE]
        fc_data[, facet_by, drop = FALSE] <- fc_data[,
            paste0(".facet_", facet_by),
            drop = FALSE
        ]

        if (isTRUE(raster)) {
            p <- p +
                scattermore::geom_scattermore(
                    data = fc_data,
                    mapping = aes(x = !!sym(dims[1]), y = !!sym(dims[2])),
                    size = pt_size,
                    alpha = pt_alpha / 2,
                    color = bg_color,
                    pixels = raster_dpi
                )
        } else if (isTRUE(hex)) {
            p <- p +
                geom_hex(
                    data = fc_data,
                    mapping = aes(x = !!sym(dims[1]), y = !!sym(dims[2])),
                    linewidth = hex_linewidth,
                    bins = hex_bins,
                    binwidth = hex_binwidth,
                    fill = bg_color,
                    alpha = pt_alpha / 2
                )
        } else {
            p <- p +
                geom_point(
                    data = fc_data,
                    mapping = aes(x = !!sym(dims[1]), y = !!sym(dims[2])),
                    size = pt_size,
                    alpha = pt_alpha / 2,
                    color = bg_color
                )
        }
        rm(fc_data)
    }

    ## Raserting the plot/Adding the points
    if (isTRUE(raster)) {
        # if (!requireNamespace("scattermore", quietly = TRUE)) {
        #     stop("'scattermore' package is required to raster the plot.")
        # }
        if (!is.null(group_by)) {
            p <- p +
                scattermore::geom_scattermore(
                    data = data[is.na(data[[group_by]]), , drop = FALSE],
                    mapping = aes(x = !!sym(dims[1]), y = !!sym(dims[2])),
                    color = bg_color,
                    pointsize = ceiling(pt_size),
                    alpha = pt_alpha,
                    pixels = raster_dpi,
                    show.legend = TRUE
                ) +
                scattermore::geom_scattermore(
                    data = data[!is.na(data[[group_by]]), , drop = FALSE],
                    mapping = aes(
                        x = !!sym(dims[1]),
                        y = !!sym(dims[2]),
                        color = !!sym(group_by)
                    ),
                    pointsize = ceiling(pt_size),
                    alpha = pt_alpha,
                    pixels = raster_dpi,
                    show.legend = TRUE
                )
        } else {
            # features
            p <- p +
                scattermore::geom_scattermore(
                    mapping = aes(x = !!sym(dims[1]), y = !!sym(dims[2])),
                    color = bg_color,
                    pointsize = ceiling(pt_size),
                    alpha = pt_alpha,
                    pixels = raster_dpi
                ) +
                scattermore::geom_scattermore(
                    mapping = aes(
                        x = !!sym(dims[1]),
                        y = !!sym(dims[2]),
                        color = !!sym(colorby)
                    ),
                    pointsize = ceiling(pt_size),
                    alpha = pt_alpha,
                    pixels = raster_dpi
                )
        }
    } else if (isTRUE(hex)) {
        # if (!requireNamespace("hexbin", quietly = TRUE)) {
        #     stop("'hexbin' package is required to add hexgons the plot.")
        # }
        has_fill <- TRUE
        if (isTRUE(hex_count)) {
            if (!is.null(features)) {
                stop(
                    "Don't know how to count for the hex when 'group_by' is not provided."
                )
            }
            p <- p +
                geom_hex(
                    mapping = aes(
                        x = !!sym(dims[1]),
                        y = !!sym(dims[2]),
                        color = !!sym(group_by),
                        fill = !!sym(group_by),
                        alpha = after_stat(!!sym("count"))
                    ),
                    linewidth = hex_linewidth,
                    bins = hex_bins,
                    binwidth = hex_binwidth
                )
        } else if (!is.null(group_by)) {
            p <- p +
                geom_hex(
                    mapping = aes(
                        x = !!sym(dims[1]),
                        y = !!sym(dims[2]),
                        color = !!sym(group_by),
                        fill = !!sym(group_by)
                    ),
                    linewidth = hex_linewidth,
                    bins = hex_bins,
                    binwidth = hex_binwidth,
                    show.legend = TRUE
                )
        } else {
            # features
            data_na <- data[is.na(data[[features]]), , drop = FALSE]
            if (nrow(data_na) > 0) {
                p <- p +
                    geom_hex(
                        data = data_na,
                        mapping = aes(x = !!sym(dims[1]), y = !!sym(dims[2])),
                        fill = bg_color,
                        linewidth = hex_linewidth,
                        bins = hex_bins,
                        binwidth = hex_binwidth,
                        alpha = pt_alpha / 2
                    )
            }
            p <- p +
                stat_summary_hex(
                    data = data[!is.na(data[[features]]), , drop = FALSE],
                    mapping = aes(
                        x = !!sym(dims[1]),
                        y = !!sym(dims[2]),
                        z = !!sym(colorby)
                    ),
                    linewidth = hex_linewidth,
                    bins = hex_bins,
                    binwidth = hex_binwidth,
                    alpha = pt_alpha
                )
        }
    } else {
        p <- p +
            geom_point(
                mapping = aes(
                    x = !!sym(dims[1]),
                    y = !!sym(dims[2]),
                    color = !!sym(colorby)
                ),
                size = pt_size,
                alpha = pt_alpha,
                show.legend = TRUE
            )
    }

    ## Adding the highlight
    if (!is.null(highlight)) {
        if (isTRUE(hex)) {
            stop("Highlight is not supported for hex plot.")
        }
        if (isTRUE(highlight)) {
            hi_df <- data
        } else if (length(highlight) == 1 && is.character(highlight)) {
            hi_df <- eval(parse(text = paste0('filter(data, ', highlight, ')')))
        } else {
            all_inst <- if (is.numeric(highlight)) {
                1:nrow(data)
            } else {
                rownames(data)
            }
            if (!any(highlight %in% all_inst)) {
                stop("No highlight items found in the data (rownames).")
            }
            if (!all(highlight %in% all_inst)) {
                warning(
                    "Not all highlight items found in the data (rownames).",
                    immediate. = TRUE
                )
            }
            hi_df <- data[intersect(highlight, all_inst), , drop = FALSE]
            rm(all_inst)
        }
        if (nrow(hi_df) > 0) {
            if (isTRUE(raster)) {
                p <- p +
                    scattermore::geom_scattermore(
                        data = hi_df,
                        aes(x = !!sym(dims[1]), y = !!sym(dims[2])),
                        color = highlight_color,
                        pointsize = floor(highlight_size) + highlight_stroke,
                        alpha = highlight_alpha,
                        pixels = raster_dpi
                    ) +
                    scattermore::geom_scattermore(
                        data = hi_df,
                        aes(
                            x = !!sym(dims[1]),
                            y = !!sym(dims[2]),
                            color = !!sym(colorby)
                        ),
                        pointsize = floor(highlight_size),
                        alpha = highlight_alpha,
                        pixels = raster_dpi
                    )
            } else {
                p <- p +
                    geom_point(
                        data = hi_df,
                        aes(x = !!sym(dims[1]), y = !!sym(dims[2])),
                        color = highlight_color,
                        size = highlight_size + highlight_stroke,
                        alpha = highlight_alpha
                    ) +
                    geom_point(
                        data = hi_df,
                        aes(
                            x = !!sym(dims[1]),
                            y = !!sym(dims[2]),
                            color = !!sym(colorby)
                        ),
                        size = highlight_size,
                        alpha = highlight_alpha
                    )
            }
        }
    }

    if (!is.null(group_by)) {
        if (!isFALSE(keep_empty_group)) {
            p <- p +
                scale_color_manual(
                    values = colors[names(labels_tb)],
                    labels = label_use,
                    na.value = bg_color,
                    breaks = group_vals,
                    limits = group_vals,
                    drop = FALSE,
                    guide = guide_legend(
                        title.hjust = 0,
                        order = 1,
                        override.aes = list(size = 3, alpha = 1)
                    )
                )
            if (has_fill) {
                p <- p +
                    scale_fill_manual(
                        values = colors[names(labels_tb)],
                        labels = label_use,
                        na.value = bg_color,
                        breaks = group_vals,
                        limits = group_vals,
                        drop = FALSE,
                        guide = guide_legend(
                            title.hjust = 0,
                            order = 1
                        )
                    )
            }
        } else {
            p <- p +
                scale_color_manual(
                    values = colors[names(labels_tb)],
                    labels = label_use,
                    na.value = bg_color,
                    guide = guide_legend(
                        title.hjust = 0,
                        order = 1,
                        override.aes = list(size = 3, alpha = 1)
                    )
                )
            if (has_fill) {
                p <- p +
                    scale_fill_manual(
                        values = colors[names(labels_tb)],
                        labels = label_use,
                        na.value = bg_color,
                        guide = guide_legend(
                            title.hjust = 0,
                            order = 1
                        )
                    )
            }
        }
    } else {
        # features
        p <- p +
            scale_color_gradientn(
                name = color_name,
                colors = palette_this(
                    palette = palette,
                    palcolor = palcolor,
                    type = "continuous",
                    reverse = palreverse
                ),
                values = rescale(feat_colors_value),
                limits = range(feat_colors_value),
                na.value = bg_color,
                guide = guide_colorbar(
                    frame.colour = "black",
                    ticks.colour = "black",
                    title.hjust = 0
                )
            )
        if (has_fill) {
            p <- p +
                scale_fill_gradientn(
                    name = color_name,
                    colors = palette_this(
                        palette = palette,
                        palcolor = palcolor,
                        type = "continuous",
                        reverse = palreverse
                    ),
                    values = rescale(feat_colors_value),
                    limits = range(feat_colors_value),
                    na.value = bg_color,
                    guide = guide_colorbar(
                        frame.colour = "black",
                        ticks.colour = "black",
                        title.hjust = 0
                    )
                )
        }
    }
    # There may be warnings about fonts, but we don't care here
    legend_base <- suppressWarnings(get_plot_component(
        p +
            theme_this(
                legend.position = "bottom",
                legend.direction = legend.direction
            ),
        "guide-box-bottom"
    ))

    legend_list <- list()
    ## Adding the lineages
    if (!is.null(lineages)) {
        if (!is.null(facet_by)) {
            stop("'lineages' is not supported when 'facet_by' is not NULL.")
        }
        lineages <- unique(check_columns(
            data,
            lineages,
            force_factor = FALSE,
            allow_multi = TRUE
        ))
        lineage_colors <- palette_this(
            lineages,
            palette = lineages_palette,
            palcolor = lineages_palcolor,
            reverse = palreverse
        )
        lineage_layers <- lapply(lineages, function(l) {
            trim_pass <- (data[[l]] >
                quantile(data[[l]], lineages_trim[1], na.rm = TRUE) &
                data[[l]] < quantile(data[[l]], lineages_trim[2], na.rm = TRUE))
            na_pass <- !is.na(data[[l]])
            index <- which(trim_pass & na_pass)
            index <- index[order(data[index, l])]
            dat_sub <- data[index, , drop = FALSE]
            # if (is.null(weights)) {
            weights_used <- rep(1, nrow(dat_sub))
            # } else {
            # weights_used <- dat_sub[[weights]]
            # }
            fitted <- lapply(dims, function(x) {
                loess(
                    formula(paste(x, l, sep = "~")),
                    weights = weights_used,
                    data = dat_sub,
                    span = lineages_span,
                    degree = 2
                )$fitted
            })
            names(fitted) <- dims
            fitted[["index"]] <- index

            dat_smooth <- as.data.frame(fitted)
            colnames(dat_smooth) <- c(
                paste0("Axis_", 1:(ncol(dat_smooth) - 1)),
                "index"
            )
            dat_smooth[, "Lineages"] <- factor(l, levels = lineages)
            dat_smooth <- unique(na.omit(dat_smooth))
            curve <- list()
            if (isTRUE(lineages_whiskers)) {
                dat_smooth[, "raw_Axis_1"] <- data[
                    dat_smooth[, "index"],
                    dims[1]
                ]
                dat_smooth[, "raw_Axis_2"] <- data[
                    dat_smooth[, "index"],
                    dims[2]
                ]
                curve <- c(
                    curve,
                    geom_segment(
                        data = dat_smooth,
                        mapping = aes(
                            x = !!sym("Axis_1"),
                            y = !!sym("Axis_2"),
                            xend = !!sym("raw_Axis_1"),
                            yend = !!sym("raw_Axis_2"),
                            color = !!sym("Lineages")
                        ),
                        linewidth = lineages_whiskers_linewidth,
                        alpha = lineages_whiskers_alpha,
                        show.legend = TRUE,
                        inherit.aes = FALSE
                    )
                )
            }
            curve <- c(
                curve,
                geom_path(
                    data = dat_smooth,
                    mapping = aes(x = !!sym("Axis_1"), y = !!sym("Axis_2")),
                    color = lineages_line_bg,
                    linewidth = lineages_linewidth + lineages_line_bg_stroke,
                    arrow = lineages_arrow,
                    show.legend = TRUE,
                    inherit.aes = FALSE
                ),
                geom_path(
                    data = dat_smooth,
                    mapping = aes(
                        x = !!sym("Axis_1"),
                        y = !!sym("Axis_2"),
                        color = !!sym("Lineages")
                    ),
                    linewidth = lineages_linewidth,
                    arrow = lineages_arrow,
                    show.legend = TRUE,
                    inherit.aes = FALSE
                )
            )
            return(curve)
        })
        lineage_layers[[length(lineage_layers) + 1]] <- scale_color_manual(
            values = lineage_colors
        )

        suppressMessages({
            legend_list$lineages <- suppressWarnings(get_plot_component(
                ggplot() +
                    lineage_layers +
                    theme_this(
                        legend.position = "bottom",
                        legend.direction = legend.direction
                    ),
                "guide-box-bottom"
            ))
        })

        p <- suppressWarnings({
            p +
                new_scale_color() +
                lineage_layers +
                ggplot2::theme(legend.position = "none")
        })
        if (is.null(legend_list$lineages)) {
            legend_list["lineages"] <- list(NULL)
        }
    }

    if (!is.null(velocity)) {
        if (!is.null(facet_by)) {
            stop("'velocity' is not supported when 'facet_by' is not NULL.")
        }
        velocity_plot_type <- match.arg(velocity_plot_type)

        if (is.data.frame(velocity) || is.matrix(velocity)) {
            v_embedding <- velocity
        } else {
            v_embedding <- data[, velocity, drop = FALSE]
        }

        velocity_layers <- VelocityPlot(
            embedding = data[, dims, drop = FALSE],
            v_embedding = v_embedding,
            plot_type = velocity_plot_type,
            group_by = if (velocity_plot_type == "raw") {
                data[[group_by]]
            } else {
                NULL
            },
            group_name = group_by,
            group_palette = velocity_group_palette,
            group_palcolor = velocity_group_palcolor,
            n_neighbors = velocity_n_neighbors,
            density = velocity_density,
            keep_na = keep_na[[group_by]],
            keep_empty = keep_empty[[group_by]],
            smooth = velocity_smooth,
            scale = velocity_scale,
            min_mass = velocity_min_mass,
            cutoff_perc = velocity_cutoff_perc,
            arrow_angle = arrow_angle,
            arrow_color = arrow_color,
            streamline_l = streamline_l,
            streamline_minl = streamline_minl,
            arrow_alpha = arrow_alpha,
            streamline_res = streamline_res,
            streamline_n = streamline_n,
            streamline_width = streamline_width,
            streamline_alpha = streamline_alpha,
            streamline_color = streamline_color,
            streamline_palette = streamline_palette,
            streamline_palcolor = streamline_palcolor,
            streamline_bg_color = streamline_bg_color,
            streamline_bg_stroke = streamline_bg_stroke,
            return_layer = TRUE
        )
        velocity_scales <- attr(velocity_layers, "scales")
        if (!is.null(velocity_scales) && "color" %in% velocity_scales) {
            p <- p + new_scale_color() + velocity_layers
            legend_list$velocity <- suppressWarnings(get_plot_component(
                ggplot() +
                    velocity_layers +
                    theme_this(
                        legend.position = "bottom",
                        legend.direction = legend.direction
                    ),
                "guide-box-bottom"
            ))
        } else {
            p <- p + velocity_layers
            legend_list["velocity"] <- list(NULL)
        }
    }

    ## Adding the statistic plots
    if (!is.null(stat_by)) {
        if (!is.null(facet_by)) {
            stop("'stat_by' is not supported when 'facet_by' is not NULL.")
        }
        if (!is.null(features)) {
            stop("'stat_by' is not supported without 'group_by'.")
        }
        stat_by <- check_columns(data, stat_by, force_factor = TRUE)
        stat_plot_type <- match.arg(stat_plot_type)
        stat_args$data <- data
        stat_args$split_by <- group_by
        stat_args$legend.position <- "bottom"
        stat_args$legend.direction <- legend.direction
        stat_args$combine <- FALSE
        stat_args$theme <- theme
        stat_args$theme_args <- theme_args
        stat_args$title <- character(0)
        # keep all levels so they appear in the legend, even if some levels have no data
        # (in case some stat plots don't show all levels due to filtering)
        stat_args$keep_empty <- TRUE
        stat_args$palette <- stat_palette
        if (stat_plot_type == "pie") {
            stat_args$x <- stat_by
            stat_plots <- do_call(PieChart, stat_args)
        } else if (stat_plot_type == "ring") {
            stat_args$group_by <- stat_by
            stat_plots <- do_call(RingPlot, stat_args)
        } else if (stat_plot_type == "bar") {
            stat_args$x <- stat_by
            stat_plots <- do_call(BarPlot, stat_args)
        } else if (stat_plot_type == "line") {
            stat_args$x <- stat_by
            stat_plots <- do_call(LinePlot, stat_args)
        }

        coord_df <- aggregate(
            p$data[, dims],
            by = list(p$data[[group_by]]),
            FUN = median
        )
        colnames(coord_df)[1] <- group_by
        x_range <- diff(layer_scales(p)$x$range$range)
        y_range <- diff(layer_scales(p)$y$range$range)

        stat_plot_list <- list()
        for (i in seq_len(nrow(coord_df))) {
            stat_plot_list[[i]] <- annotation_custom(
                ggplotGrob(
                    stat_plots[[coord_df[i, group_by]]] +
                        theme_void() +
                        ggplot2::theme(legend.position = "none")
                ),
                xmin = coord_df[i, dims[1]] - x_range * stat_plot_size / 2,
                ymin = coord_df[i, dims[2]] - y_range * stat_plot_size / 2,
                xmax = coord_df[i, dims[1]] + x_range * stat_plot_size / 2,
                ymax = coord_df[i, dims[2]] + y_range * stat_plot_size / 2
            )
        }
        p <- p + stat_plot_list
        legend_list$stat_by <- suppressWarnings(get_plot_component(
            stat_plots[[coord_df[i, group_by]]] +
                ggplot2::theme(legend.position = "bottom"),
            "guide-box-bottom"
        ))
    }

    ## Adding the labels
    if (isTRUE(label)) {
        if (!is.null(features)) {
            stop("Adding labels is not supported when 'features' is specified.")
        }
        if (!is.null(facet_by)) {
            label_df <- aggregate(
                data[, dims],
                by = list(data[[group_by]], data[[facet_by]]),
                FUN = median
            )
            colnames(label_df)[2:length(facet_by) + 1] <- facet_by
        } else {
            label_df <- aggregate(
                data[, dims],
                by = list(data[[group_by]]),
                FUN = median
            )
        }
        colnames(label_df)[1] <- ".label"
        label_df <- label_df[!is.na(label_df[, ".label"]), , drop = FALSE]
        if (!isTRUE(label_insitu)) {
            label_df[, ".label"] <- seq_len(nrow(label_df))
        }
        if (isTRUE(label_repel)) {
            p <- p +
                geom_point(
                    data = label_df,
                    mapping = aes(x = !!sym(dims[1]), y = !!sym(dims[2])),
                    color = label_pt_color,
                    size = label_pt_size
                ) +
                geom_text_repel(
                    data = label_df,
                    aes(
                        x = !!sym(dims[1]),
                        y = !!sym(dims[2]),
                        label = !!sym(".label")
                    ),
                    point.size = label_pt_size,
                    max.overlaps = 100,
                    force = label_repulsion,
                    color = label_fg,
                    bg.color = label_bg,
                    bg.r = label_bg_r,
                    size = label_size,
                    inherit.aes = FALSE
                )
        } else {
            p <- p +
                geom_text_repel(
                    data = label_df,
                    aes(
                        x = !!sym(dims[1]),
                        y = !!sym(dims[2]),
                        label = !!sym(".label")
                    ),
                    fontface = "bold",
                    min.segment.length = 0,
                    segment.color = label_segment_color,
                    point.size = NA,
                    max.overlaps = 100,
                    force = 0,
                    color = label_fg,
                    bg.color = label_bg,
                    bg.r = label_bg_r,
                    size = label_size,
                    inherit.aes = FALSE
                )
        }
    }

    ## Posing all legends
    if (length(legend_list) > 0) {
        legend_list <- legend_list[!sapply(legend_list, is.null)]
        if (legend.direction == "vertical") {
            legend <- do_call(cbind, c(list(base = legend_base), legend_list))
        } else {
            legend <- do_call(rbind, c(list(base = legend_base), legend_list))
        }
        gtable <- ggplotGrob(p + ggplot2::theme(legend.position = "none"))
        gtable <- add_grob(gtable, legend, legend.position)
        p <- wrap_plots(gtable)
    }

    ## Putting reasonable height and width
    dims <- calculate_plot_dimensions(
        base_height = 5.5,
        aspect.ratio = aspect.ratio,
        legend.position = legend.position,
        legend.direction = legend.direction,
        legend_n = if (!is.null(group_by)) length(group_vals) else 1,
        legend_nchar = if (!is.null(group_by)) max(nchar(label_use)) else 5
    )

    attr(p, "height") <- dims$height
    attr(p, "width") <- dims$width

    if (length(legend_list) == 0) {
        p <- facet_plot(
            p,
            facet_by,
            facet_scales,
            facet_nrow,
            facet_ncol,
            facet_byrow,
            labeller = facet_labeller,
            legend.position = legend.position,
            legend.direction = legend.direction,
            drop = !isTRUE(keep_empty_facet)
        )
    }
    p
}

#' DimPlot / FeatureDimPlot
#'
#' @description
#' Visualise dimension reduction data (PCA, t-SNE, UMAP, etc.) as a 2D or 3D
#' scatter plot. `DimPlot()` colours points by a discrete grouping variable
#' (e.g., clusters), while `FeatureDimPlot()` colours points by a continuous
#' numeric feature (e.g., gene expression, lineage scores).
#'
#' Both functions share the same internal engine (`DimPlotAtomic`) and support
#' an extensive set of annotation layers: group boundary marks, network/graph
#' edges, 2D density contours, lineage/trajectory curves, RNA-velocity arrows
#' (raw, grid, or stream), statistical summary mini-plots at group centroids,
#' point highlighting, background context points from other facets, and
#' flexible label positioning.
#'
#' When `dims` has 3 elements, both functions automatically return an
#' interactive **plotly** 3D scatter plot (via `DimPlotAtomic3D`). Certain
#' 2D-only features are silently ignored in 3D mode (see `@param dims` for
#' the full list).
#'
#' Rendering scales with dataset size: standard `geom_point()` for small data,
#' automatic rasterisation via `scattermore::geom_scattermore()` when
#' `nrow(data) > 1e5`, or hex-bin aggregation (`geom_hex()` /
#' `stat_summary_hex()`).
#'
#' @section split_by Workflow (DimPlot):
#'
#' When `split_by` is specified, `DimPlot()` executes the following pipeline:
#'
#' 1. **Argument validation** — `validate_common_args()` checks the seed and
#'    blocks `split_by` + `velocity` combinations.
#' 2. **NA / empty normalisation** — `check_keep_na()` / `check_keep_empty()`
#'    convert `keep_na` / `keep_empty` to per-column lists.
#' 3. **Theme resolution** — `process_theme()` resolves the theme string to a
#'    theme function.
#' 4. **Split column resolution** — `check_columns()` validates `split_by`
#'    (force_factor, concat_multi).
#' 5. **Pre-filtering** — `process_keep_na_empty()` removes NA / empty levels
#'    from the split column, then `data` is split by `split_by` levels (order
#'    preserved). When `graph` references an attribute (`@graph`), the graph
#'    matrix is also subset per split.
#' 6. **Per-split parameter resolution** — `check_palette()`,
#'    `check_palcolor()`, `check_legend()` resolve palette, palcolor,
#'    legend.position, and legend.direction for each split.
#' 7. **Per-split dispatch** — each split is passed to `DimPlotAtomic()` with
#'    its resolved parameters. Title defaults to the split level name unless
#'    `title` is a function.
#' 8. **Combination** — `combine_plots()` assembles the list of plots via
#'    `patchwork::wrap_plots()`, applying `nrow`, `ncol`, `byrow`, `axes`,
#'    `axis_titles`, `guides`, and `design`.
#'
#' @section split_by Workflow (FeatureDimPlot):
#'
#' `FeatureDimPlot()` supports two forms of splitting:
#'
#' **A. `split_by = TRUE` (split by features)**
#'
#' 1. Each feature in `features` is dispatched individually to
#'    `DimPlotAtomic()`, producing one plot per feature. The plot title
#'    defaults to the feature name.
#' 2. Plots are combined via `combine_plots()` with `split_by = ".features"`.
#'
#' **B. `split_by` as a column name (split by data column)**
#'
#' 1. Data is split by the named column's levels (same pipeline as DimPlot
#'    steps 1–8 above). Graph attribute splitting is supported.
#'
#' @rdname dimplot
#' @inheritParams common_args
#' @inheritParams DimPlotAtomic
#' @return A `ggplot` object (single plot), a `patchwork` / `wrap_plots` object
#'   (when `split_by` is provided and `combine = TRUE`), or a list of `ggplot`
#'   objects (when `split_by` is provided and `combine = FALSE`).
#'   When `dims` has 3 elements, a **plotly** object is returned instead.
#' @export
#' @seealso \code{\link{VelocityPlot}}
#' @examples
#' \donttest{
#' data(dim_example)
#'
#' # basic dim plot
#' DimPlot(dim_example, group_by = "clusters")
#' DimPlot(dim_example, group_by = "clusters", theme = "theme_blank")
#' DimPlot(dim_example, group_by = "clusters", theme = ggplot2::theme_classic,
#'     theme_args = list(base_size = 16), palette = "seurat")
#'
#' # raster and highlighting
#' DimPlot(dim_example, group_by = "clusters", raster = TRUE, raster_dpi = 50)
#' DimPlot(dim_example, group_by = "clusters", highlight = 1:20,
#'     highlight_color = "black", highlight_stroke = 2)
#' DimPlot(dim_example, group_by = "clusters", highlight = TRUE, facet_by = "group",
#'     theme = "theme_blank")
#'
#' # labels
#' DimPlot(dim_example, group_by = "clusters", label = TRUE,
#'     label_size = 5, label_bg_r = 0.2)
#' DimPlot(dim_example, group_by = "clusters", label = TRUE, label_fg = "red",
#'     label_bg = "yellow", label_size = 5)
#' DimPlot(dim_example, group_by = "clusters", label = TRUE, label_insitu = TRUE)
#'
#' # group marks
#' DimPlot(dim_example, group_by = "clusters", add_mark = TRUE)
#' DimPlot(dim_example, group_by = "clusters", add_mark = TRUE, mark_linetype = 2)
#' DimPlot(dim_example, group_by = "clusters", add_mark = TRUE, mark_type = "ellipse")
#'
#' # density overlays
#' DimPlot(dim_example, group_by = "clusters", add_density = TRUE)
#' DimPlot(dim_example, group_by = "clusters", add_density = TRUE, density_filled = TRUE)
#' DimPlot(dim_example, group_by = "clusters", add_density = TRUE, density_filled = TRUE,
#'     density_filled_palette = "Blues", highlight = TRUE)
#'
#' # statistics at group centroids
#' DimPlot(dim_example, group_by = "clusters", stat_by = "group")
#' DimPlot(dim_example, group_by = "clusters", stat_by = "group",
#'     stat_plot_type = "bar", stat_plot_size = 0.06)
#'
#' # hex bins
#' DimPlot(dim_example, group_by = "clusters", hex = TRUE)
#' DimPlot(dim_example, group_by = "clusters", hex = TRUE, hex_bins = 20)
#' DimPlot(dim_example, group_by = "clusters", hex = TRUE, hex_count = FALSE)
#'
#' # graph / network edges
#' DimPlot(dim_example, group_by = "clusters", graph = "@graph", edge_color = "grey80")
#'
#' # lineages / trajectories
#' DimPlot(dim_example, group_by = "clusters", lineages = c("stochasticbasis_1", "stochasticbasis_2"))
#' DimPlot(dim_example, group_by = "clusters", lineages = c("stochasticbasis_1", "stochasticbasis_2"),
#'     lineages_whiskers = TRUE, lineages_whiskers_linewidth = 0.1)
#' DimPlot(dim_example, group_by = "clusters", lineages = c("stochasticbasis_1", "stochasticbasis_2"),
#'     lineages_span = 0.4)
#'
#' # split_by
#' DimPlot(dim_example, group_by = "clusters",  split_by = "group",
#'     palette = list(A = "Paired", B = "Set1"))
#'
#' # velocity
#' DimPlot(dim_example, group_by = "clusters", velocity = c("stochasticbasis_1", "stochasticbasis_2"),
#'     pt_alpha = 0)
#' DimPlot(dim_example, group_by = "clusters", velocity = 3:4,
#'     velocity_plot_type = "grid", arrow_alpha = 0.6)
#' DimPlot(dim_example, group_by = "clusters", velocity = 3:4,
#'     velocity_plot_type = "stream")
#'
#' # 3D plots (returns a plotly object)
#' DimPlot(dim_example, dims = 1:3, group_by = "clusters")
#' DimPlot(dim_example, dims = 1:3, group_by = "clusters", label = TRUE,
#'     label_insitu = TRUE)
#' DimPlot(dim_example, dims = c("basis_1", "basis_2", "stochasticbasis_1"),
#'     group_by = "clusters", graph = "@graph", edge_color = "grey80")
#'
#' # keep_na and keep_empty
#' dim_example$clusters[dim_example$clusters == "Ductal"] <- NA
#'
#' DimPlot(dim_example, group_by = "clusters", keep_na = FALSE, keep_empty = TRUE)
#' DimPlot(dim_example, group_by = "clusters", keep_na = TRUE, keep_empty = TRUE)
#' DimPlot(dim_example, group_by = "clusters", keep_na = TRUE, keep_empty = FALSE)
#' }
DimPlot <- function(
    data,
    dims = 1:2,
    group_by,
    group_by_sep = "_",
    split_by = NULL,
    split_by_sep = "_",
    pt_size = NULL,
    pt_alpha = 1,
    bg_color = "grey80",
    label_insitu = FALSE,
    show_stat = !identical(theme, "theme_blank"),
    label = FALSE,
    label_size = 4,
    label_fg = "white",
    label_bg = "black",
    label_bg_r = 0.1,
    label_repel = FALSE,
    label_repulsion = 20,
    label_pt_size = 1,
    label_pt_color = "black",
    label_segment_color = "black",
    order = c("as-is", "reverse", "high-top", "low-top", "random"),
    highlight = NULL,
    highlight_alpha = 1,
    highlight_size = 1,
    highlight_color = "black",
    highlight_stroke = 0.8,
    add_mark = FALSE,
    mark_type = c("hull", "ellipse", "rect", "circle"),
    mark_expand = unit(3, "mm"),
    mark_alpha = 0.1,
    mark_linetype = 1,
    stat_by = NULL,
    stat_plot_type = c("pie", "ring", "bar", "line"),
    stat_plot_size = 0.1,
    stat_args = list(palette = "Set1"),
    graph = NULL,
    edge_size = c(0.05, 0.5),
    edge_alpha = 0.1,
    edge_color = "grey40",
    add_density = FALSE,
    density_color = "grey80",
    density_filled = FALSE,
    density_filled_palette = "Greys",
    density_filled_palcolor = NULL,
    lineages = NULL,
    lineages_trim = c(0.01, 0.99),
    lineages_span = 0.75,
    lineages_palette = "Dark2",
    lineages_palcolor = NULL,
    lineages_arrow = arrow(length = unit(0.1, "inches")),
    lineages_linewidth = 1,
    lineages_line_bg = "white",
    lineages_line_bg_stroke = 0.5,
    lineages_whiskers = FALSE,
    lineages_whiskers_linewidth = 0.5,
    lineages_whiskers_alpha = 0.5,
    velocity = NULL,
    velocity_plot_type = c("raw", "grid", "stream"),
    velocity_n_neighbors = NULL,
    velocity_density = 1,
    velocity_smooth = 0.5,
    velocity_scale = 1,
    velocity_min_mass = 1,
    velocity_cutoff_perc = 5,
    velocity_group_palette = "Set2",
    velocity_group_palcolor = NULL,
    arrow_angle = 20,
    arrow_color = "black",
    arrow_alpha = 1,
    streamline_l = 5,
    streamline_minl = 1,
    streamline_res = 1,
    streamline_n = 15,
    streamline_width = c(0, 0.8),
    streamline_alpha = 1,
    streamline_color = NULL,
    streamline_palette = "RdYlBu",
    streamline_palcolor = NULL,
    streamline_bg_color = "white",
    streamline_bg_stroke = 0.5,
    keep_na = FALSE,
    keep_empty = FALSE,
    facet_by = NULL,
    facet_scales = "fixed",
    facet_nrow = NULL,
    facet_ncol = NULL,
    facet_byrow = TRUE,
    title = NULL,
    subtitle = NULL,
    xlab = NULL,
    ylab = NULL,
    theme = "theme_this",
    theme_args = list(),
    aspect.ratio = 1,
    legend.position = "right",
    legend.direction = "vertical",
    raster = NULL,
    raster_dpi = c(512, 512),
    hex = FALSE,
    hex_linewidth = 0.5,
    hex_count = TRUE,
    hex_bins = 50,
    hex_binwidth = NULL,
    palette = "Paired",
    palcolor = NULL,
    palreverse = FALSE,
    seed = 8525,
    combine = TRUE,
    nrow = NULL,
    ncol = NULL,
    byrow = TRUE,
    axes = NULL,
    axis_titles = axes,
    guides = NULL,
    design = NULL,
    ...
) {
    validate_common_args(seed, facet_by = facet_by)
    keep_na <- check_keep_na(keep_na, c(facet_by, group_by, split_by, stat_by))
    keep_empty <- check_keep_empty(
        keep_empty,
        c(facet_by, group_by, split_by, stat_by)
    )
    theme <- process_theme(theme)
    split_by <- check_columns(
        data,
        split_by,
        force_factor = TRUE,
        allow_multi = TRUE,
        concat_multi = TRUE,
        concat_sep = split_by_sep
    )

    stopifnot(
        "[DimPlot] 'split_by' is not supported for velocity plot." = is.null(
            velocity
        ) ||
            is.null(split_by)
    )

    if (!is.null(split_by)) {
        data <- process_keep_na_empty(data, keep_na, keep_empty, col = split_by)
        keep_na[[split_by]] <- NULL
        keep_empty[[split_by]] <- NULL
        datas <- split(data, data[[split_by]])
        # keep the order of levels
        datas <- datas[levels(data[[split_by]])]
        if (
            is.character(graph) && length(graph) == 1 && startsWith(graph, "@")
        ) {
            # split the graph as well
            datas <- lapply(datas, function(d) {
                gh <- attr(data, substring(graph, 2))[rownames(d), rownames(d)]
                attr(d, substring(graph, 2)) <- gh
                d
            })
        }
    } else {
        datas <- list(data)
        split_by <- names(datas) <- "..."
    }
    palette <- check_palette(palette, names(datas))
    palcolor <- check_palcolor(palcolor, names(datas))
    legend.direction <- check_legend(
        legend.direction,
        names(datas),
        "legend.direction"
    )
    legend.position <- check_legend(
        legend.position,
        names(datas),
        "legend.position"
    )

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
            DimPlotAtomic(
                datas[[nm]],
                dims = dims,
                group_by = group_by,
                group_by_sep = group_by_sep,
                pt_size = pt_size,
                pt_alpha = pt_alpha,
                bg_color = bg_color,
                label_insitu = label_insitu,
                show_stat = show_stat,
                label = label,
                label_size = label_size,
                label_fg = label_fg,
                label_bg = label_bg,
                label_bg_r = label_bg_r,
                label_repel = label_repel,
                label_repulsion = label_repulsion,
                label_pt_size = label_pt_size,
                label_pt_color = label_pt_color,
                label_segment_color = label_segment_color,
                order = order,
                highlight = highlight,
                highlight_alpha = highlight_alpha,
                highlight_size = highlight_size,
                highlight_color = highlight_color,
                highlight_stroke = highlight_stroke,
                add_mark = add_mark,
                mark_type = mark_type,
                mark_expand = mark_expand,
                mark_alpha = mark_alpha,
                mark_linetype = mark_linetype,
                stat_by = stat_by,
                stat_plot_type = stat_plot_type,
                stat_plot_size = stat_plot_size,
                stat_args = stat_args,
                graph = graph,
                edge_size = edge_size,
                edge_alpha = edge_alpha,
                edge_color = edge_color,
                add_density = add_density,
                density_color = density_color,
                density_filled = density_filled,
                density_filled_palette = density_filled_palette,
                density_filled_palcolor = density_filled_palcolor,
                lineages = lineages,
                lineages_trim = lineages_trim,
                lineages_span = lineages_span,
                lineages_palette = lineages_palette,
                lineages_palcolor = lineages_palcolor,
                lineages_arrow = lineages_arrow,
                lineages_linewidth = lineages_linewidth,
                lineages_line_bg = lineages_line_bg,
                lineages_line_bg_stroke = lineages_line_bg_stroke,
                lineages_whiskers = lineages_whiskers,
                lineages_whiskers_linewidth = lineages_whiskers_linewidth,
                lineages_whiskers_alpha = lineages_whiskers_alpha,
                velocity = velocity,
                velocity_plot_type = velocity_plot_type,
                velocity_n_neighbors = velocity_n_neighbors,
                velocity_density = velocity_density,
                velocity_smooth = velocity_smooth,
                velocity_scale = velocity_scale,
                velocity_min_mass = velocity_min_mass,
                velocity_cutoff_perc = velocity_cutoff_perc,
                arrow_alpha = arrow_alpha,
                velocity_group_palette = velocity_group_palette,
                velocity_group_palcolor = velocity_group_palcolor,
                arrow_angle = arrow_angle,
                arrow_color = arrow_color,
                streamline_l = streamline_l,
                streamline_minl = streamline_minl,
                streamline_res = streamline_res,
                streamline_n = streamline_n,
                streamline_width = streamline_width,
                streamline_alpha = streamline_alpha,
                streamline_color = streamline_color,
                streamline_palette = streamline_palette,
                streamline_palcolor = streamline_palcolor,
                streamline_bg_color = streamline_bg_color,
                streamline_bg_stroke = streamline_bg_stroke,
                keep_na = keep_na,
                keep_empty = keep_empty,
                facet_by = facet_by,
                facet_scales = facet_scales,
                facet_nrow = facet_nrow,
                facet_ncol = facet_ncol,
                facet_byrow = facet_byrow,
                title = title,
                subtitle = subtitle,
                xlab = xlab,
                ylab = ylab,
                theme = theme,
                theme_args = theme_args,
                aspect.ratio = aspect.ratio,
                legend.position = legend.position[[nm]],
                legend.direction = legend.direction[[nm]],
                raster = raster,
                raster_dpi = raster_dpi,
                hex = hex,
                hex_linewidth = hex_linewidth,
                hex_count = hex_count,
                hex_bins = hex_bins,
                hex_binwidth = hex_binwidth,
                palette = palette[[nm]],
                palcolor = palcolor[[nm]],
                palreverse = palreverse,
                seed = seed,
                ...
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

#' @description
#' Feature expression on dimension reduction plots. Colours points by a
#' continuous numeric variable (e.g., gene expression, module score, lineage
#' pseudotime) using a gradient colour scale, with optional quantile trimming
#' and background cutoff.
#'
#' When multiple `features` are provided and `facet_by` is not set, the data
#' is automatically pivoted to long format and faceted by feature name.
#' `split_by = TRUE` dispatches each feature to a separate plot for
#' independent layout control. `split_by` as a column name splits by that
#' column's levels, producing one plot per level with per-split palette
#' support.
#'
#' For detailed split_by workflows, see the main `DimPlot / FeatureDimPlot`
#' documentation (`@section split_by Workflow (FeatureDimPlot)`).
#'
#' @export
#' @rdname dimplot
#' @inheritParams common_args
#' @inheritParams DimPlotAtomic
#' @param split_by A character vector of column names to split the data by and
#'   plot separately. If `TRUE`, the data is split by `features` — each feature
#'   is plotted in its own panel. Use this instead of `facet_by` when you need
#'   independent layout control (`nrow`, `ncol`) or per-feature palettes via
#'   `split_by = TRUE` combined with `palette` / `palcolor`.
#' @return A `ggplot` object (single plot), a `patchwork` / `wrap_plots` object
#'   (when `split_by` is provided and `combine = TRUE`), or a list of `ggplot`
#'   objects (when `split_by` is provided and `combine = FALSE`).
#'   When `dims` has 3 elements, a **plotly** object is returned instead.
#' @export
#' @examples
#' \donttest{
#' data(dim_example)
#'
#' # single feature
#' FeatureDimPlot(dim_example, features = "stochasticbasis_1", pt_size = 2)
#' FeatureDimPlot(dim_example, features = "stochasticbasis_1", pt_size = 2, bg_cutoff = 0)
#' FeatureDimPlot(dim_example, features = "stochasticbasis_1", raster = TRUE, raster_dpi = 30)
#'
#' # multiple features (auto-pivoted to long, faceted by feature)
#' FeatureDimPlot(dim_example, features = c("stochasticbasis_1", "stochasticbasis_2"),
#'  pt_size = 2)
#'
#' # single feature with facet_by (facet_by works when only 1 feature)
#' FeatureDimPlot(dim_example, features = c("stochasticbasis_1"), pt_size = 2,
#'  facet_by = "group")
#'
#' # multiple features with split_by for independent layout
#' FeatureDimPlot(dim_example, features = c("stochasticbasis_1", "stochasticbasis_2"),
#'  split_by = "group", nrow = 2)
#'
#' # highlight and hex
#' FeatureDimPlot(dim_example, features = c("stochasticbasis_1", "stochasticbasis_2"),
#'  highlight = TRUE)
#' FeatureDimPlot(dim_example, features = c("stochasticbasis_1", "stochasticbasis_2"),
#'  hex = TRUE, hex_bins = 15)
#' FeatureDimPlot(dim_example, features = c("stochasticbasis_1", "stochasticbasis_2"),
#'  hex = TRUE, hex_bins = 15, split_by = "group", palette = list(A = "Reds", B = "Blues"))
#'
#' # 3D plots (returns a plotly object)
#' FeatureDimPlot(dim_example, dims = 1:3, features = "stochasticbasis_2", pt_size = 2)
#' FeatureDimPlot(dim_example, dims = c("basis_1", "basis_2", "stochasticbasis_1"),
#'  features = "stochasticbasis_2")
#' }
FeatureDimPlot <- function(
    data,
    dims = 1:2,
    features,
    split_by = NULL,
    split_by_sep = "_",
    lower_quantile = 0,
    upper_quantile = 0.99,
    lower_cutoff = NULL,
    upper_cutoff = NULL,
    pt_size = NULL,
    pt_alpha = 1,
    bg_color = "grey80",
    bg_cutoff = NULL,
    label_insitu = FALSE,
    show_stat = !identical(theme, "theme_blank"),
    color_name = "",
    label = FALSE,
    label_size = 4,
    label_fg = "white",
    label_bg = "black",
    label_bg_r = 0.1,
    label_repel = FALSE,
    label_repulsion = 20,
    label_pt_size = 1,
    label_pt_color = "black",
    label_segment_color = "black",
    order = c("as-is", "reverse", "high-top", "low-top", "random"),
    highlight = NULL,
    highlight_alpha = 1,
    highlight_size = 1,
    highlight_color = "black",
    highlight_stroke = 0.8,
    add_mark = FALSE,
    mark_type = c("hull", "ellipse", "rect", "circle"),
    mark_expand = unit(3, "mm"),
    mark_alpha = 0.1,
    mark_linetype = 1,
    keep_na = FALSE,
    keep_empty = FALSE,
    stat_by = NULL,
    stat_plot_type = c("pie", "ring", "bar", "line"),
    stat_plot_size = 0.1,
    stat_args = list(palette = "Set1"),
    graph = NULL,
    edge_size = c(0.05, 0.5),
    edge_alpha = 0.1,
    edge_color = "grey40",
    add_density = FALSE,
    density_color = "grey80",
    density_filled = FALSE,
    density_filled_palette = "Greys",
    density_filled_palcolor = NULL,
    lineages = NULL,
    lineages_trim = c(0.01, 0.99),
    lineages_span = 0.75,
    lineages_palette = "Dark2",
    lineages_palcolor = NULL,
    lineages_arrow = arrow(length = unit(0.1, "inches")),
    lineages_linewidth = 1,
    lineages_line_bg = "white",
    lineages_line_bg_stroke = 0.5,
    lineages_whiskers = FALSE,
    lineages_whiskers_linewidth = 0.5,
    lineages_whiskers_alpha = 0.5,
    velocity = NULL,
    velocity_plot_type = c("raw", "grid", "stream"),
    velocity_n_neighbors = NULL,
    velocity_density = 1,
    velocity_smooth = 0.5,
    velocity_scale = 1,
    velocity_min_mass = 1,
    velocity_cutoff_perc = 5,
    velocity_group_palette = "Set2",
    velocity_group_palcolor = NULL,
    arrow_angle = 20,
    arrow_color = "black",
    arrow_alpha = 1,
    streamline_l = 5,
    streamline_minl = 1,
    streamline_res = 1,
    streamline_n = 15,
    streamline_width = c(0, 0.8),
    streamline_alpha = 1,
    streamline_color = NULL,
    streamline_palette = "RdYlBu",
    streamline_palcolor = NULL,
    streamline_bg_color = "white",
    streamline_bg_stroke = 0.5,
    facet_by = NULL,
    facet_scales = "fixed",
    facet_nrow = NULL,
    facet_ncol = NULL,
    facet_byrow = TRUE,
    title = NULL,
    subtitle = NULL,
    xlab = NULL,
    ylab = NULL,
    theme = "theme_this",
    theme_args = list(),
    aspect.ratio = 1,
    legend.position = "right",
    legend.direction = "vertical",
    raster = NULL,
    raster_dpi = c(512, 512),
    hex = FALSE,
    hex_linewidth = 0.5,
    hex_count = FALSE,
    hex_bins = 50,
    hex_binwidth = NULL,
    palette = "Spectral",
    palcolor = NULL,
    palreverse = FALSE,
    seed = 8525,
    combine = TRUE,
    nrow = NULL,
    ncol = NULL,
    byrow = TRUE,
    axes = NULL,
    axis_titles = axes,
    guides = NULL,
    design = NULL,
    ...
) {
    validate_common_args(seed, facet_by = facet_by)
    theme <- process_theme(theme)

    if (isTRUE(split_by)) {
        keep_na <- check_keep_na(keep_na, c(facet_by, stat_by))
        keep_empty <- check_keep_empty(keep_empty, c(facet_by, stat_by))
        plots <- lapply(
            features,
            function(feature) {
                DimPlotAtomic(
                    data,
                    dims = dims,
                    lower_quantile = lower_quantile,
                    upper_quantile = upper_quantile,
                    lower_cutoff = lower_cutoff,
                    upper_cutoff = upper_cutoff,
                    pt_size = pt_size,
                    pt_alpha = pt_alpha,
                    bg_color = bg_color,
                    color_name = color_name,
                    label_insitu = label_insitu,
                    show_stat = show_stat,
                    features = feature,
                    bg_cutoff = bg_cutoff,
                    label = label,
                    label_size = label_size,
                    label_fg = label_fg,
                    label_bg = label_bg,
                    label_bg_r = label_bg_r,
                    label_repel = label_repel,
                    label_repulsion = label_repulsion,
                    label_pt_size = label_pt_size,
                    label_pt_color = label_pt_color,
                    label_segment_color = label_segment_color,
                    order = order,
                    keep_na = keep_na,
                    keep_empty = keep_empty,
                    highlight = highlight,
                    highlight_alpha = highlight_alpha,
                    highlight_size = highlight_size,
                    highlight_color = highlight_color,
                    highlight_stroke = highlight_stroke,
                    add_mark = add_mark,
                    mark_type = mark_type,
                    mark_expand = mark_expand,
                    mark_alpha = mark_alpha,
                    mark_linetype = mark_linetype,
                    stat_by = stat_by,
                    stat_plot_type = stat_plot_type,
                    stat_plot_size = stat_plot_size,
                    stat_args = stat_args,
                    graph = graph,
                    edge_size = edge_size,
                    edge_alpha = edge_alpha,
                    edge_color = edge_color,
                    add_density = add_density,
                    density_color = density_color,
                    density_filled = density_filled,
                    density_filled_palette = density_filled_palette,
                    density_filled_palcolor = density_filled_palcolor,
                    lineages = lineages,
                    lineages_trim = lineages_trim,
                    lineages_span = lineages_span,
                    lineages_palette = lineages_palette,
                    lineages_palcolor = lineages_palcolor,
                    lineages_arrow = lineages_arrow,
                    lineages_linewidth = lineages_linewidth,
                    lineages_line_bg = lineages_line_bg,
                    lineages_line_bg_stroke = lineages_line_bg_stroke,
                    lineages_whiskers = lineages_whiskers,
                    lineages_whiskers_linewidth = lineages_whiskers_linewidth,
                    lineages_whiskers_alpha = lineages_whiskers_alpha,
                    velocity = velocity,
                    velocity_plot_type = velocity_plot_type,
                    velocity_n_neighbors = velocity_n_neighbors,
                    velocity_density = velocity_density,
                    velocity_smooth = velocity_smooth,
                    velocity_scale = velocity_scale,
                    velocity_min_mass = velocity_min_mass,
                    velocity_cutoff_perc = velocity_cutoff_perc,
                    arrow_alpha = arrow_alpha,
                    velocity_group_palette = velocity_group_palette,
                    velocity_group_palcolor = velocity_group_palcolor,
                    arrow_angle = arrow_angle,
                    arrow_color = arrow_color,
                    streamline_l = streamline_l,
                    streamline_minl = streamline_minl,
                    streamline_res = streamline_res,
                    streamline_n = streamline_n,
                    streamline_width = streamline_width,
                    streamline_alpha = streamline_alpha,
                    streamline_color = streamline_color,
                    streamline_palette = streamline_palette,
                    streamline_palcolor = streamline_palcolor,
                    streamline_bg_color = streamline_bg_color,
                    streamline_bg_stroke = streamline_bg_stroke,
                    facet_by = facet_by,
                    facet_scales = facet_scales,
                    facet_nrow = facet_nrow,
                    facet_ncol = facet_ncol,
                    facet_byrow = facet_byrow,
                    title = title %||% feature,
                    subtitle = subtitle,
                    xlab = xlab,
                    ylab = ylab,
                    theme = theme,
                    theme_args = theme_args,
                    aspect.ratio = aspect.ratio,
                    legend.position = legend.position,
                    legend.direction = legend.direction,
                    raster = raster,
                    raster_dpi = raster_dpi,
                    hex = hex,
                    hex_linewidth = hex_linewidth,
                    hex_count = hex_count,
                    hex_bins = hex_bins,
                    hex_binwidth = hex_binwidth,
                    palette = palette,
                    palcolor = palcolor,
                    palreverse = palreverse,
                    seed = seed,
                    ...
                )
            }
        )
        names(plots) <- features
        split_by <- ".features"
    } else {
        keep_na <- check_keep_na(keep_na, c(facet_by, stat_by, split_by))
        keep_empty <- check_keep_empty(keep_empty, c(facet_by, stat_by, split_by))

        split_by <- check_columns(
            data,
            split_by,
            force_factor = TRUE,
            allow_multi = TRUE,
            concat_multi = TRUE,
            concat_sep = split_by_sep
        )

        if (!is.null(split_by)) {
            data <- process_keep_na_empty(
                data,
                keep_na,
                keep_empty,
                col = split_by
            )
            keep_na[[split_by]] <- NULL
            keep_empty[[split_by]] <- NULL
            datas <- split(data, data[[split_by]])
            # keep the order of levels
            datas <- datas[levels(data[[split_by]])]
            if (
                is.character(graph) &&
                    length(graph) == 1 &&
                    startsWith(graph, "@")
            ) {
                # split the graph as well
                datas <- lapply(datas, function(d) {
                    gh <- attr(data, substring(graph, 2))[
                        rownames(d),
                        rownames(d)
                    ]
                    attr(d, substring(graph, 2)) <- gh
                    d
                })
            }
        } else {
            datas <- list(data)
            split_by <- names(datas) <- "..."
        }
        palette <- check_palette(palette, names(datas))
        palcolor <- check_palcolor(palcolor, names(datas))
        legend.direction <- check_legend(
            legend.direction,
            names(datas),
            "legend.direction"
        )
        legend.position <- check_legend(
            legend.position,
            names(datas),
            "legend.position"
        )

        plots <- lapply(
            names(datas),
            function(nm) {
                default_title <- if (
                    length(datas) == 1 && identical(nm, "...")
                ) {
                    NULL
                } else {
                    nm
                }
                if (is.function(title)) {
                    title <- title(default_title)
                } else {
                    title <- title %||% default_title
                }
                DimPlotAtomic(
                    datas[[nm]],
                    dims = dims,
                    features = features,
                    lower_quantile = lower_quantile,
                    upper_quantile = upper_quantile,
                    lower_cutoff = lower_cutoff,
                    upper_cutoff = upper_cutoff,
                    pt_size = pt_size,
                    pt_alpha = pt_alpha,
                    bg_color = bg_color,
                    color_name = color_name,
                    label_insitu = label_insitu,
                    show_stat = show_stat,
                    bg_cutoff = bg_cutoff,
                    label = label,
                    label_size = label_size,
                    label_fg = label_fg,
                    label_bg = label_bg,
                    label_bg_r = label_bg_r,
                    label_repel = label_repel,
                    label_repulsion = label_repulsion,
                    label_pt_size = label_pt_size,
                    label_pt_color = label_pt_color,
                    label_segment_color = label_segment_color,
                    order = order,
                    keep_na = keep_na,
                    keep_empty = keep_empty,
                    highlight = highlight,
                    highlight_alpha = highlight_alpha,
                    highlight_size = highlight_size,
                    highlight_color = highlight_color,
                    highlight_stroke = highlight_stroke,
                    add_mark = add_mark,
                    mark_type = mark_type,
                    mark_expand = mark_expand,
                    mark_alpha = mark_alpha,
                    mark_linetype = mark_linetype,
                    stat_by = stat_by,
                    stat_plot_type = stat_plot_type,
                    stat_plot_size = stat_plot_size,
                    stat_args = stat_args,
                    graph = graph,
                    edge_size = edge_size,
                    edge_alpha = edge_alpha,
                    edge_color = edge_color,
                    add_density = add_density,
                    density_color = density_color,
                    density_filled = density_filled,
                    density_filled_palette = density_filled_palette,
                    density_filled_palcolor = density_filled_palcolor,
                    lineages = lineages,
                    lineages_trim = lineages_trim,
                    lineages_span = lineages_span,
                    lineages_palette = lineages_palette,
                    lineages_palcolor = lineages_palcolor,
                    lineages_arrow = lineages_arrow,
                    lineages_linewidth = lineages_linewidth,
                    lineages_line_bg = lineages_line_bg,
                    lineages_line_bg_stroke = lineages_line_bg_stroke,
                    lineages_whiskers = lineages_whiskers,
                    lineages_whiskers_linewidth = lineages_whiskers_linewidth,
                    lineages_whiskers_alpha = lineages_whiskers_alpha,
                    velocity = velocity,
                    velocity_plot_type = velocity_plot_type,
                    velocity_n_neighbors = velocity_n_neighbors,
                    velocity_density = velocity_density,
                    velocity_smooth = velocity_smooth,
                    velocity_scale = velocity_scale,
                    velocity_min_mass = velocity_min_mass,
                    velocity_cutoff_perc = velocity_cutoff_perc,
                    arrow_alpha = arrow_alpha,
                    velocity_group_palette = velocity_group_palette,
                    velocity_group_palcolor = velocity_group_palcolor,
                    arrow_angle = arrow_angle,
                    arrow_color = arrow_color,
                    streamline_l = streamline_l,
                    streamline_minl = streamline_minl,
                    streamline_res = streamline_res,
                    streamline_n = streamline_n,
                    streamline_width = streamline_width,
                    streamline_alpha = streamline_alpha,
                    streamline_color = streamline_color,
                    streamline_palette = streamline_palette,
                    streamline_palcolor = streamline_palcolor,
                    streamline_bg_color = streamline_bg_color,
                    streamline_bg_stroke = streamline_bg_stroke,
                    facet_by = facet_by,
                    facet_scales = facet_scales,
                    facet_nrow = facet_nrow,
                    facet_ncol = facet_ncol,
                    facet_byrow = facet_byrow,
                    title = title,
                    subtitle = subtitle,
                    xlab = xlab,
                    ylab = ylab,
                    theme = theme,
                    theme_args = theme_args,
                    aspect.ratio = aspect.ratio,
                    legend.position = legend.position[[nm]],
                    legend.direction = legend.direction[[nm]],
                    raster = raster,
                    raster_dpi = raster_dpi,
                    hex = hex,
                    hex_linewidth = hex_linewidth,
                    hex_count = hex_count,
                    hex_bins = hex_bins,
                    hex_binwidth = hex_binwidth,
                    palette = palette[[nm]],
                    palcolor = palcolor[[nm]],
                    palreverse = palreverse,
                    seed = seed,
                    ...
                )
            }
        )
        names(plots) <- names(datas)
    }

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
