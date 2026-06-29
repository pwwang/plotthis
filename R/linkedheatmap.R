# ──────────────────────────────────────────────────────────────────────────────
# LinkedHeatmap — Two heatmaps connected by intensity-weighted link lines
# ──────────────────────────────────────────────────────────────────────────────
#
# Architecture: 3-column grid.layout
#   Col 1: Left heatmap  (e.g., ligands × sources)
#   Col 2: Link lines    (curved lines connecting matching rows)
#   Col 3: Right heatmap (e.g., receptors × targets)
#
# Link alignment is achieved through deterministic sizing: every heatmap
# component receives an explicit unit(), draw() padding is set to zero,
# and ComplexHeatmap's internal component-level gaps (DENDROGRAM_PADDING,
# COLUMN_ANNO_PADDING, DIMNAME_PADDING, TITLE_PADDING) are all budgeted
# in the body_top_offset calculation.
# ──────────────────────────────────────────────────────────────────────────────

#' @importFrom dplyr %>% group_by summarise across all_of everything
#' @importFrom dplyr distinct filter mutate pull n
#' @importFrom tidyr pivot_wider unite expand
#' @importFrom circlize colorRamp2
#' @importFrom grid grid.newpage pushViewport popViewport viewport
#' @importFrom grid grid.layout grid.xspline grid.grabExpr
#' @importFrom grid grid.points grid.lines unit gpar
#' @importFrom rlang %||% sym syms
#' @importFrom stats hclust dist as.dendrogram quantile
NULL


# ═══════════════════════════════════════════════════════════════════════════════
# Layer 1: Matrix building utility
# ═══════════════════════════════════════════════════════════════════════════════

#' Build a matrix from long-format data
#'
#' Pivots data from long to wide format, grouping by rows_by/columns_by
#' and aggregating values_by.
#'
#' @param data A data frame
#' @param rows_by Column name for rows
#' @param columns_by Column name for columns
#' @param values_by Column name for values
#' @param cell_agg Aggregation function (default: nanmean)
#' @param values_fill Fill value for missing combinations
#' @return A list with \code{matrix}, \code{row_names}, \code{col_names}
#' @keywords internal
.build_linked_matrix <- function(data, rows_by, columns_by, values_by,
                                  cell_agg = NULL, values_fill = NA) {
    cell_agg <- cell_agg %||% function(x) mean(x, na.rm = TRUE)

    mat <- data %>%
        group_by(!!sym(rows_by), !!sym(columns_by)) %>%
        summarise(.value = cell_agg(!!sym(values_by)), .groups = "drop") %>%
        pivot_wider(
            names_from = !!sym(columns_by),
            values_from = ".value",
            values_fill = values_fill
        ) %>%
        as.data.frame()

    row_names <- mat[[rows_by]]
    mat[[rows_by]] <- NULL
    mat <- as.matrix(mat)
    rownames(mat) <- row_names

    # Preserve factor level order
    col_order <- data %>%
        distinct(!!sym(columns_by)) %>%
        pull(!!sym(columns_by)) %>%
        as.character() %>%
        intersect(colnames(mat))
    row_order <- data %>%
        distinct(!!sym(rows_by)) %>%
        pull(!!sym(rows_by)) %>%
        as.character() %>%
        intersect(rownames(mat))
    mat <- mat[row_order, col_order, drop = FALSE]

    list(
        matrix = mat,
        row_names = rownames(mat),
        col_names = colnames(mat)
    )
}


# ═══════════════════════════════════════════════════════════════════════════════
# Layer 2: Core atomic function
# ═══════════════════════════════════════════════════════════════════════════════

#' Atomic linked heatmap (internal)
#'
#' Draws two heatmaps side-by-side with link lines connecting matching rows.
#'
#' @param data A data frame
#' @param left A named list with \code{rows_by}, \code{columns_by},
#'   \code{values_by}, \code{name}, and optionally \code{palette},
#'   \code{palcolor}, \code{cluster_rows}, \code{cluster_columns}.
#' @param right Same structure as \code{left}.
#' @param link_left_by Column in \code{data} that matches \code{left$rows_by}.
#'   Each unique value maps to one row in the left heatmap.
#' @param link_right_by Column in \code{data} that matches \code{right$rows_by}.
#' @param link_width_by Column for link line width scaling.
#' @param link_width_scale Scaling factor for link widths (default 5).
#' @param link_color Color of link lines.
#' @param link_alpha Alpha transparency for link lines.
#' @param gap_width Width (inches) of the gap column between heatmaps.
#' @param palette Palette for heatmap color scale.
#' @param palcolor Custom color vector for heatmap.
#' @param palreverse Reverse the palette.
#' @param lower_quantile,upper_quantile Quantile cutoffs for color scale.
#' @param lower_cutoff,upper_cutoff Explicit cutoffs (override quantiles).
#' @param cluster_rows,cluster_columns Whether to cluster rows/columns.
#' @param show_row_names,show_column_names Whether to show names.
#' @param row_names_side,column_names_side Side for names.
#' @param base_size Scaling factor for cell dimensions.
#' @param left_aspect.ratio,right_aspect.ratio Aspect ratio (height/width) for
#'   cells in the left and right heatmaps. Default 1 gives square cells.
#' @param seed Random seed.
#' @param ... Additional arguments passed to ComplexHeatmap::Heatmap.
#'
#' @return A patchwork-wrapped grob with height/width attributes.
#' @keywords internal
LinkedHeatmapAtomic <- function(
    data,
    left_values_by,
    right_values_by,
    left_rows_by,
    right_rows_by,
    left_columns_by,
    right_columns_by,
    left_columns_split_by = NULL,
    right_columns_split_by = NULL,
    left_pie_group_by = NULL,
    right_pie_group_by = NULL,

    rows_split_by = NULL,
    values_fill = NA,

    # palettes
    palette = "RdBu",
    palcolor = NULL,
    palreverse = FALSE,

    # cell_type: pies
    pie_size_name = "size",
    pie_size = NULL,
    pie_values = "length",
    pie_palette = "Spectral",
    pie_palcolor = NULL,

    # cell_type: bars
    bars_sample = 100,
    # cell_type: label
    label = identity,
    label_size = 10,
    label_color = "black",
    label_name = "label",
    # cell_type: mark
    mark = identity,
    mark_color = "black",
    mark_size = 1,
    mark_name = "mark",
    # cell_type: violin
    violin_fill = NULL,
    # cell_type: boxplot
    boxplot_fill = NULL,
    # cell_type: dot
    dot_size = 8,
    dot_size_name = "size",
    # legend
    legend_items = NULL,
    legend_discrete = FALSE,
    legend.position = "right",
    legend.direction = "vertical",
    # values
    lower_quantile = 0,
    upper_quantile = 0.99,
    lower_cutoff = NULL,
    upper_cutoff = NULL,
    # bg
    add_bg = FALSE,
    bg_alpha = 0.5,
    keep_na = FALSE,
    keep_empty = FALSE,
    # reticle
    add_reticle = FALSE,
    reticle_color = "grey",

    left_cluster_rows = NULL,
    right_cluster_rows = NULL,
    cluster_columns = NULL,
    show_row_names = NULL,
    show_column_names = NULL,
    border = TRUE,
    title = NULL,
    column_title = NULL,
    row_title = NULL,
    na_col = "grey85",
    left_row_names_side = "left",
    right_row_names_side = "right",
    column_names_side = "bottom",
    column_annotation = NULL,
    column_annotation_side = "top",
    column_annotation_palette = "Paired",
    column_annotation_palcolor = NULL,
    column_annotation_type = "auto",
    column_annotation_params = list(),
    column_annotation_agg = NULL,
    row_annotation = NULL,
    left_row_annotation_side = "left",
    right_row_annotation_side = "right",
    row_annotation_palette = "Paired",
    row_annotation_palcolor = NULL,
    row_annotation_type = "auto",
    row_annotation_params = list(),
    row_annotation_agg = NULL,

    links_width_by = NULL,
    link_width_scale = 5,
    link_color = "grey30",
    link_alpha = 0.8,

    alpha = 1,
    seed = 8525,
    padding = 15,
    base_size = 1,
    aspect.ratio = NULL,
    draw_opts = list(),
    # cell customization
    layer_fun_callback = NULL,
    cell_type = c(
        "tile",
        "bars",
        "label",
        "mark",
        "label+mark",
        "mark+label",
        "dot",
        "violin",
        "boxplot",
        "pie"
    ),
    cell_agg = NULL,
    ...
) {
    set.seed(seed)

    rest_args <- list(...)
    # The arguments in rest_args that don't begin with "right_" prefix
    left_args <- rest_args[!grepl("^right_", names(rest_args))]
    # Replace all left_ prefix
    names(left_args) <- gsub("^left_", "", names(left_args))

    right_args <- rest_args[!grepl("^left_", names(rest_args))]
    names(right_args) <- gsub("^right_", "", names(right_args))

    # ── Resolve left/right parameters ──
    left_args$data <- data
    left_args$values_by <- left_values_by
    left_args$values_fill <- left_args$values_fill %||% values_fill
    left_args$rows_by <- left_rows_by
    left_args$rows_split_by <- rows_split_by
    left_args$columns_by <- left_columns_by
    left_args$columns_split_by <- left_columns_split_by
    left_args$palette <- left_args$palette %||% palette
    left_args$palcolor <- left_args$palcolor %||% palcolor
    left_args$palreverse <- left_args$palreverse %||% palreverse
    left_args$pie_size_name <- left_args$pie_size_name %||% pie_size_name
    left_args$pie_size <- left_args$pie_size %||% pie_size
    left_args$pie_values <- left_args$pie_values %||% pie_values
    left_args$pie_group_by <- left_pie_group_by
    left_args$pie_palette <- left_args$pie_palette %||% pie_palette
    left_args$pie_palcolor <- left_args$pie_palcolor %||% pie_palcolor
    left_args$bars_sample <- left_args$bars_sample %||% bars_sample
    left_args$label <- left_args$label %||% label
    left_args$label_size <- left_args$label_size %||% label_size
    left_args$label_color <- left_args$label_color %||% label_color
    left_args$label_name <- left_args$label_name %||% label_name
    left_args$mark <- left_args$mark %||% mark
    left_args$mark_color <- left_args$mark_color %||% mark_color
    left_args$mark_size <- left_args$mark_size %||% mark_size
    left_args$mark_name <- left_args$mark_name %||% mark_name
    left_args$violin_fill <- left_args$violin_fill %||% violin_fill
    left_args$boxplot_fill <- left_args$boxplot_fill %||% boxplot_fill
    left_args$dot_size <- left_args$dot_size %||% dot_size
    left_args$dot_size_name <- left_args$dot_size_name %||% dot_size_name
    left_args$legend_items <- left_args$legend_items %||% legend_items
    left_args$legend_discrete <- left_args$legend_discrete %||% legend_discrete
    left_args$legend.position <- left_args$legend.position %||% legend.position
    left_args$legend.direction <- left_args$legend.direction %||% legend.direction
    left_args$lower_quantile <- left_args$lower_quantile %||% lower_quantile
    left_args$upper_quantile <- left_args$upper_quantile %||% upper_quantile
    left_args$lower_cutoff <- left_args$lower_cutoff %||% lower_cutoff
    left_args$upper_cutoff <- left_args$upper_cutoff %||% upper_cutoff
    left_args$add_bg <- left_args$add_bg %||% add_bg
    left_args$bg_alpha <- left_args$bg_alpha %||% bg_alpha
    left_args$add_reticle <- left_args$add_reticle %||% add_reticle
    left_args$reticle_color <- left_args$reticle_color %||% reticle_color
    left_args$cluster_columns <- left_args$cluster_columns %||% cluster_columns
    left_args$cluster_rows <- left_cluster_rows
    left_args$show_row_names <- left_args$show_row_names %||% show_row_names
    left_args$show_column_names <- left_args$show_column_names %||% show_column_names
    left_args$border <- left_args$border %||% border
    left_args$title <- left_args$title %||% title
    left_args$column_title <- left_args$column_title %||% column_title
    left_args$row_title <- left_args$row_title %||% row_title
    left_args$na_col <- left_args$na_col %||% na_col
    left_args$row_names_side <- left_row_names_side
    left_args$column_names_side <- left_args$column_names_side %||% column_names_side
    left_args$column_annotation <- left_args[["column_annotation"]] %||% column_annotation
    left_args$column_annotation_side <- left_args$column_annotation_side %||% column_annotation_side
    left_args$column_annotation_palette <- left_args$column_annotation_palette %||% column_annotation_palette
    left_args$column_annotation_palcolor <- left_args$column_annotation_palcolor %||% column_annotation_palcolor
    left_args$column_annotation_type <- left_args$column_annotation_type %||% column_annotation_type
    left_args$column_annotation_params <- left_args$column_annotation_params %||% column_annotation_params
    left_args$column_annotation_agg <- left_args$column_annotation_agg %||% column_annotation_agg
    left_args$row_annotation <- left_args[["row_annotation"]] %||% row_annotation
    left_args$row_annotation_side <- left_args$row_annotation_side %||% row_annotation_side
    left_args$row_annotation_palette <- left_args$row_annotation_palette %||% row_annotation_palette
    left_args$row_annotation_palcolor <- left_args$row_annotation_palcolor %||% row_annotation_palcolor
    left_args$row_annotation_type <- left_args$row_annotation_type %||% row_annotation_type
    left_args$row_annotation_params <- left_args$row_annotation_params %||% row_annotation_params
    left_args$row_annotation_agg <- left_args$row_annotation_agg %||% row_annotation_agg
    # flip
    left_args$alpha <- left_args$alpha %||% alpha
    left_args$padding <- left_args$padding %||% padding
    left_args$base_size <- left_args$base_size %||% base_size
    left_args$aspect.ratio <- left_args$aspect.ratio %||% aspect.ratio
    left_args$draw_opts <- left_args$draw_opts %||% draw_opts
    left_args$layer_fun_callback <- left_args$layer_fun_callback %||% layer_fun_callback
    left_args$cell_type <- left_args$cell_type %||% cell_type
    left_args$cell_agg <- left_args$cell_agg %||% cell_agg
    left_args$return_ht <- TRUE

    right_args$data <- data
    right_args$values_by <- right_values_by
    right_args$values_fill <- right_args$values_fill %||% values_fill
    right_args$rows_by <- right_rows_by
    right_args$rows_split_by <- rows_split_by
    right_args$columns_by <- right_columns_by
    right_args$columns_split_by <- right_columns_split_by
    right_args$palette <- right_args$palette %||% palette
    right_args$palcolor <- right_args$palcolor %||% palcolor
    right_args$palreverse <- right_args$palreverse %||% palreverse
    right_args$pie_size_name <- right_args$pie_size_name %||% pie_size_name
    right_args$pie_size <- right_args$pie_size %||% pie_size
    right_args$pie_values <- right_args$pie_values %||% pie_values
    right_args$pie_group_by <- right_pie_group_by
    right_args$pie_palette <- right_args$pie_palette %||% pie_palette
    right_args$pie_palcolor <- right_args$pie_palcolor %||% pie_palcolor
    right_args$bars_sample <- right_args$bars_sample %||% bars_sample
    right_args$label <- right_args$label %||% label
    right_args$label_size <- right_args$label_size %||% label_size
    right_args$label_color <- right_args$label_color %||% label_color
    right_args$label_name <- right_args$label_name %||% label_name
    right_args$mark <- right_args$mark %||% mark
    right_args$mark_color <- right_args$mark_color %||% mark_color
    right_args$mark_size <- right_args$mark_size %||% mark_size
    right_args$mark_name <- right_args$mark_name %||% mark_name
    right_args$violin_fill <- right_args$violin_fill %||% violin_fill
    right_args$boxplot_fill <- right_args$boxplot_fill %||% boxplot_fill
    right_args$dot_size <- right_args$dot_size %||% dot_size
    right_args$dot_size_name <- right_args$dot_size_name %||% dot_size_name
    right_args$legend_items <- right_args$legend_items %||% legend_items
    right_args$legend_discrete <- right_args$legend_discrete %||% legend_discrete
    right_args$legend.position <- right_args$legend.position %||% legend.position
    right_args$legend.direction <- right_args$legend.direction %||% legend.direction
    right_args$lower_quantile <- right_args$lower_quantile %||% lower_quantile
    right_args$upper_quantile <- right_args$upper_quantile %||% upper_quantile
    right_args$lower_cutoff <- right_args$lower_cutoff %||% lower_cutoff
    right_args$upper_cutoff <- right_args$upper_cutoff %||% upper_cutoff
    right_args$add_bg <- right_args$add_bg %||% add_bg
    right_args$bg_alpha <- right_args$bg_alpha %||% bg_alpha
    right_args$add_reticle <- right_args$add_reticle %||% add_reticle
    right_args$reticle_color <- right_args$reticle_color %||% reticle_color
    right_args$cluster_columns <- right_args$cluster_columns %||% cluster_columns
    right_args$cluster_rows <- right_cluster_rows
    right_args$show_row_names <- right_args$show_row_names %||% show_row_names
    right_args$show_column_names <- right_args$show_column_names %||% show_column_names
    right_args$border <- right_args$border %||% border
    right_args$title <- right_args$title %||% title
    right_args$column_title <- right_args$column_title %||% column_title
    right_args$row_title <- right_args$row_title %||% row_title
    right_args$na_col <- right_args$na_col %||% na_col
    right_args$row_names_side <- right_row_names_side
    right_args$column_names_side <- right_args$column_names_side %||% column_names_side
    right_args$column_annotation <- right_args[["column_annotation"]] %||% column_annotation
    right_args$column_annotation_side <- right_args$column_annotation_side %||% column_annotation_side
    right_args$column_annotation_palette <- right_args$column_annotation_palette %||% column_annotation_palette
    right_args$column_annotation_palcolor <- right_args$column_annotation_palcolor %||% column_annotation_palcolor
    right_args$column_annotation_type <- right_args$column_annotation_type %||% column_annotation_type
    right_args$column_annotation_params <- right_args$column_annotation_params %||% column_annotation_params
    right_args$column_annotation_agg <- right_args$column_annotation_agg %||% column_annotation_agg
    right_args$row_annotation <- right_args[["row_annotation"]] %||% row_annotation
    right_args$row_annotation_side <- right_args$row_annotation_side %||% row_annotation_side
    right_args$row_annotation_palette <- right_args$row_annotation_palette %||% row_annotation_palette
    right_args$row_annotation_palcolor <- right_args$row_annotation_palcolor %||% row_annotation_palcolor
    right_args$row_annotation_type <- right_args$row_annotation_type %||% row_annotation_type
    right_args$row_annotation_params <- right_args$row_annotation_params %||% row_annotation_params
    right_args$row_annotation_agg <- right_args$row_annotation_agg %||% row_annotation_agg
    # flip
    right_args$alpha <- right_args$alpha %||% alpha
    right_args$padding <- right_args$padding %||% padding
    right_args$base_size <- right_args$base_size %||% base_size
    right_args$aspect.ratio <- right_args$aspect.ratio %||% aspect.ratio
    right_args$draw_opts <- right_args$draw_opts %||% draw_opts
    right_args$layer_fun_callback <- right_args$layer_fun_callback %||% layer_fun_callback
    right_args$cell_type <- right_args$cell_type %||% cell_type
    right_args$cell_agg <- right_args$cell_agg %||% cell_agg
    right_args$return_ht <- TRUE

    # ── Pre-compute cell dimensions (same formula as HeatmapAtomic) ──
    # Pass explicit width/height to guarantee exact cell sizing.
    # Without this, ComplexHeatmap uses null units and cells fill
    # whatever space is available in the viewport.
    .ct <- match.arg(cell_type)
    .ct <- sub("mark+label", "label+mark", .ct, fixed = TRUE)
    .cell_w <- switch(.ct,
        violin = 0.5, boxplot = 0.5, pie = 0.5, bars = 0.35,
        label = 0.6, mark = 0.25, `label+mark` = 0.6, 0.25)
    .aspect_default <- switch(.ct,
        violin = 2, boxplot = 2, pie = 1, bars = 0.5,
        label = 0.6, mark = 1, `label+mark` = 0.6, 1)

    left_bs <- left_args$base_size %||% base_size %||% 1
    left_ar <- left_args$aspect.ratio %||% .aspect_default
    left_cell_w_pre <- .cell_w * left_bs
    left_cell_h_pre <- left_cell_w_pre * left_ar

    right_bs <- right_args$base_size %||% base_size %||% 1
    right_ar <- right_args$aspect.ratio %||% .aspect_default
    right_cell_w_pre <- .cell_w * right_bs
    right_cell_h_pre <- right_cell_w_pre * right_ar

    # Pre-build matrices to get row/column counts
    left_mat_info <- .build_linked_matrix(
        data, left_rows_by, left_columns_by, left_values_by,
        left_args$cell_agg, values_fill)
    right_mat_info <- .build_linked_matrix(
        data, right_rows_by, right_columns_by, right_values_by,
        right_args$cell_agg, values_fill)

    left_ncols_pre <- length(left_mat_info$col_names)
    left_nrows_pre <- length(left_mat_info$row_names)
    right_ncols_pre <- length(right_mat_info$col_names)
    right_nrows_pre <- length(right_mat_info$row_names)

    left_args$width <- unit(left_ncols_pre * left_cell_w_pre, "inches")
    left_args$height <- unit(left_nrows_pre * left_cell_h_pre, "inches")
    right_args$width <- unit(right_ncols_pre * right_cell_w_pre, "inches")
    right_args$height <- unit(right_nrows_pre * right_cell_h_pre, "inches")

    left_ht <- do_call(HeatmapAtomic, left_args)
    right_ht <- do_call(HeatmapAtomic, right_args)

    n_left_rows <- nrow(left_ht@matrix)
    n_left_cols <- ncol(left_ht@matrix)
    n_right_rows <- nrow(right_ht@matrix)
    n_right_cols <- ncol(right_ht@matrix)

    # Get the order of the rows in the left heatmap after clustering (if any)
    left_row_order <- left_ht@row_order
    right_row_order <- right_ht@row_order

    left_cell_w <- attr(left_ht, "cell_w")
    left_cell_h <- attr(left_ht, "cell_h")
    right_cell_w <- attr(right_ht, "cell_w")
    right_cell_h <- attr(right_ht, "cell_h")

    gap_width <- 0.5

    # ── Extract exact dimensions from ComplexHeatmap prepared objects ──
    # component_height() returns a 9-element list (all internal gaps included):
    #   [1] column_title_top    [2] column_dend_top    [3] column_names_top
    #   [4] column_anno_top     [5] heatmap_body       [6] column_anno_bot
    #   [7] column_names_bot    [8] column_dend_bot    [9] column_title_bot
    # component_width() returns a 9-element list:
    #   [1] row_title_left      [2] row_dend_left      [3] row_names_left
    #   [4] row_anno_left       [5] heatmap_body       [6] row_anno_right
    #   [7] row_names_right     [8] row_dend_right     [9] row_title_right
    .ch_to_in <- function(comp_list) {
        vapply(comp_list, function(u) {
            if (is.null(u)) return(0)
            grid::convertUnit(u, "inches", valueOnly = TRUE)
        }, numeric(1))
    }

    left_ch  <- .ch_to_in(ComplexHeatmap:::component_height(left_ht))
    right_ch <- .ch_to_in(ComplexHeatmap:::component_height(right_ht))
    left_cw  <- .ch_to_in(ComplexHeatmap:::component_width(left_ht))
    right_cw <- .ch_to_in(ComplexHeatmap:::component_width(right_ht))

    # Body dimensions (component [5] is the body)
    left_body_h  <- left_ch[5]
    right_body_h <- right_ch[5]
    left_body_w  <- left_cw[5]
    right_body_w <- right_cw[5]

    # body_top_offset = sum of all components ABOVE the body [1:4]
    body_top_offset_left  <- sum(left_ch[1:4])
    body_top_offset_right <- sum(right_ch[1:4])

    # below_h = sum of all components BELOW the body [6:9]
    left_below_h  <- sum(left_ch[6:9])
    right_below_h <- sum(right_ch[6:9])

    # Total heatmap dimensions
    left_total_w  <- sum(left_cw)
    left_total_h  <- sum(left_ch)
    right_total_w <- sum(right_cw)
    right_total_h <- sum(right_ch)

    total_w <- left_total_w + gap_width + right_total_w
    total_h <- max(left_total_h, right_total_h)

    # ── Legend flag (needed later) ──
    legend_gap <- 0.15
    show_legend <- !identical(legend.position, "none")

    # ── Build link table ──
    left_row_names_ordered <- rownames(left_ht@matrix)[left_row_order]
    right_row_names_ordered <- rownames(right_ht@matrix)[right_row_order]

    link_table <- data %>%
        distinct(!!sym(left_rows_by), !!sym(right_rows_by),
                 .keep_all = TRUE) %>%
        mutate(
            pos_left = match(
                as.character(!!sym(left_rows_by)), left_row_names_ordered
            ),
            pos_right = match(
                as.character(!!sym(right_rows_by)), right_row_names_ordered
            )
        ) %>%
        filter(!is.na(.data$pos_left), !is.na(.data$pos_right))

    if (!is.null(links_width_by) && links_width_by %in% colnames(link_table)) {
        raw_intensity <- link_table[[links_width_by]]
        if (max(raw_intensity, na.rm = TRUE) >
                min(raw_intensity, na.rm = TRUE)) {
            link_table$intensity <- (raw_intensity -
                min(raw_intensity, na.rm = TRUE)) /
                (max(raw_intensity, na.rm = TRUE) -
                     min(raw_intensity, na.rm = TRUE))
        } else {
            link_table$intensity <- rep(1, nrow(link_table))
        }
        link_table$intensity[is.na(link_table$intensity)] <- 0.5
    } else {
        link_table$intensity <- rep(1, nrow(link_table))
    }

    # ── Collect legends (before link position computation so legend
    #     dimensions can be factored into plot_h for top/bottom) ──
    combined_legend <- NULL
    legend_w <- 0
    legend_h <- 0

    if (show_legend) {
        legends <- c(attr(left_ht, "legends"), attr(right_ht, "legends"))

        if (length(legends) > 0) {
            combined_legend <- ComplexHeatmap::packLegend(
                list = legends,
                direction = legend.direction,
                gap = unit(4, "mm")
            )
            # Estimate legend width from label text widths.
            # Discrete legends have @labels slot; continuous (colorRamp2)
            # legends do not — use fixed estimate for those.
            label_widths <- vapply(legends, function(lgd) {
                if (methods::.hasSlot(lgd, "labels") &&
                    is.character(lgd@labels) && length(lgd@labels) > 0) {
                    convertUnit(
                        ComplexHeatmap::max_text_width(lgd@labels),
                        "inches", valueOnly = TRUE
                    )
                } else {
                    0.6  # continuous legend: fixed label width
                }
            }, numeric(1))
            max_label_w <- max(label_widths, 0.5)
            title_w <- max(vapply(legends, function(lgd) {
                if (methods::.hasSlot(lgd, "title") &&
                    is.character(lgd@title) &&
                    nchar(lgd@title) > 0) {
                    convertUnit(
                        ComplexHeatmap::max_text_width(lgd@title),
                        "inches", valueOnly = TRUE
                    )
                } else 0
            }, numeric(1)), 0)
            legend_w <- max(max_label_w, title_w) + 0.6

            # Estimate legend height:
            # ~0.7 in per legend (title + colour bar + labels + gaps)
            # When vertical they stack; when horizontal single row.
            n_legends <- length(legends)
            mm_gap_in <- convertUnit(unit(4, "mm"), "inches", valueOnly = TRUE)
            if (legend.direction == "vertical") {
                legend_h <- n_legends * 0.7 + (n_legends - 1) * mm_gap_in
            } else {
                legend_h <- 0.7
            }
        }
    }

    # ── Plot dimensions (heatmap body + legend) ──
    plot_w <- total_w
    plot_h <- total_h
    if (show_legend) {
        plot_w <- total_w + legend_gap + legend_w
        plot_h <- total_h + legend_gap + legend_h
    }

    # ── Link position computation ──
    # NPC coords use total_h (heatmap-only height) because each heatmap
    # viewport receives that exact height from the grid layout.
    # Each heatmap is drawn inside a centered viewport of its exact native
    # size, so the body top position includes the centering offset:
    #   body_top_px = (total_h - heatmap_total_h) / 2 + body_top_offset

    left_center_offset  <- (total_h - left_total_h) / 2
    right_center_offset <- (total_h - right_total_h) / 2

    left_body_top_npc  <- 1 - (left_center_offset + body_top_offset_left) / total_h
    left_body_bot_npc  <- 1 - (left_center_offset + body_top_offset_left + left_body_h) / total_h
    left_body_range    <- left_body_top_npc - left_body_bot_npc

    right_body_top_npc <- 1 - (right_center_offset + body_top_offset_right) / total_h
    right_body_bot_npc <- 1 - (right_center_offset + body_top_offset_right + right_body_h) / total_h
    right_body_range   <- right_body_top_npc - right_body_bot_npc

    compute_y <- function(pos, n_rows, top_npc, range_npc) {
        top_npc - (pos - 0.5) / n_rows * range_npc
    }

    # ── Draw composite ──
    # Build grid layout dynamically based on legend.position.
    # Heatmaps always occupy one row of 3 columns (left | gap | right).
    # The legend gets an extra column (left/right) or an extra row (top/bottom).
    pos_left <- legend.position == "left"
    pos_right <- legend.position == "right"
    pos_top <- legend.position == "top"
    pos_bottom <- legend.position == "bottom"

    n_hm_cols <- 3L
    hm_col_widths <- unit(c(left_total_w, gap_width, right_total_w), "inches")
    hm_col_left <- 1L
    hm_col_gap <- 2L
    hm_col_right <- 3L

    if (show_legend && (pos_left || pos_right)) {
        n_cols <- 4L
        if (pos_left) {
            col_widths <- unit(c(legend_w + legend_gap, left_total_w,
                                 gap_width, right_total_w), "inches")
            lg_col <- 1L
            hm_col_left <- 2L
            hm_col_gap <- 3L
            hm_col_right <- 4L
        } else {  # right
            col_widths <- unit(c(left_total_w, gap_width, right_total_w,
                                 legend_w + legend_gap), "inches")
            lg_col <- 4L
        }
        n_rows <- 1L
        row_heights <- unit(total_h, "inches")
        hm_row <- 1L
        lg_row <- 1L
    } else if (show_legend && (pos_top || pos_bottom)) {
        n_cols <- 3L
        col_widths <- hm_col_widths
        n_rows <- 2L
        if (pos_top) {
            row_heights <- unit(c(legend_h + legend_gap, total_h), "inches")
            lg_row <- 1L
            hm_row <- 2L
        } else {  # bottom
            row_heights <- unit(c(total_h, legend_h + legend_gap), "inches")
            hm_row <- 1L
            lg_row <- 2L
        }
    } else {
        n_cols <- 3L
        col_widths <- hm_col_widths
        n_rows <- 1L
        row_heights <- unit(total_h, "inches")
        hm_row <- 1L
    }

    p <- grid.grabExpr({
        grid.newpage()
        pushViewport(viewport(
            layout = grid.layout(n_rows, n_cols,
                widths = col_widths,
                heights = row_heights
            )
        ))

        # Left heatmap — centered exact-size viewport so ComplexHeatmap
        # sizes the body to exactly n_left_rows × cell_h
        pushViewport(viewport(layout.pos.row = hm_row,
                              layout.pos.col = hm_col_left))
        pushViewport(viewport(
            x = unit(0, "npc"),
            y = unit(0.5, "npc"),
            width = unit(left_total_w, "inches"),
            height = unit(left_total_h, "inches"),
            just = c("left", "centre")
        ))
        ComplexHeatmap::draw(
            left_ht,
            newpage = FALSE,
            show_heatmap_legend = FALSE,
            show_annotation_legend = FALSE,
            padding = unit(c(0, 0, 0, 0), "mm")
        )
        popViewport(2)

        # Right heatmap — centered exact-size viewport so ComplexHeatmap
        # sizes the body to exactly n_right_rows × cell_h
        pushViewport(viewport(layout.pos.row = hm_row,
                              layout.pos.col = hm_col_right))
        pushViewport(viewport(
            x = unit(0, "npc"),
            y = unit(0.5, "npc"),
            width = unit(right_total_w, "inches"),
            height = unit(right_total_h, "inches"),
            just = c("left", "centre")
        ))
        ComplexHeatmap::draw(
            right_ht,
            newpage = FALSE,
            show_heatmap_legend = FALSE,
            show_annotation_legend = FALSE,
            padding = unit(c(0, 0, 0, 0), "mm")
        )
        popViewport(2)

        # Legend
        if (!is.null(combined_legend)) {
            if (pos_left || pos_right) {
                pushViewport(viewport(layout.pos.row = lg_row,
                                      layout.pos.col = lg_col))
                ComplexHeatmap::draw(
                    combined_legend,
                    x = unit(0.05, "npc"),
                    just = "left"
                )
            } else {
                # top / bottom: legend spans all heatmap columns
                pushViewport(viewport(layout.pos.row = lg_row,
                                      layout.pos.col = 1:3))
                ComplexHeatmap::draw(
                    combined_legend,
                    x = unit(0.5, "npc"),
                    just = "centre"
                )
            }
            popViewport()
        }

        # Links in middle column
        pushViewport(viewport(layout.pos.row = hm_row,
                              layout.pos.col = hm_col_gap))
        if (nrow(link_table) > 0) {
            for (k in seq_len(nrow(link_table))) {
                y_left <- compute_y(
                    link_table$pos_left[k], n_left_rows,
                    left_body_top_npc, left_body_range
                )
                y_right <- compute_y(
                    link_table$pos_right[k], n_right_rows,
                    right_body_top_npc, right_body_range
                )
                grid.xspline(
                    x = unit(c(0.15, 0.5, 0.85), "npc"),
                    y = unit(
                        c(y_left, mean(c(y_left, y_right)), y_right),
                        "npc"
                    ),
                    shape = 0.5,
                    gp = gpar(
                        lwd = link_table$intensity[k] * link_width_scale,
                        col = link_color,
                        alpha = link_alpha
                    )
                )
            }
        }
        popViewport()
    })

    p <- patchwork::wrap_plots(p)

    # ── Dimension attributes ──
    # Legend dimensions are fixed (don't scale with the heatmap body), so we
    # compute the aspect ratio from the body only, clamp body dimensions
    # (reserving space for the legend), apply ratio correction, then add
    # legend space back to get the final display dimensions.
    min_size_in <- 4
    max_size_in <- 64
    display_h <- max(min(plot_h, max_size_in), min_size_in)
    display_w <- max(min(plot_w, max_size_in), min_size_in)
    ratio <- plot_h / plot_w
    if (ratio > 1 && display_h == max_size_in) {
        display_w <- display_h / ratio
    } else if (ratio < 1 && display_w == max_size_in) {
        display_h <- display_w * ratio
    }
    attr(p, "height") <- display_h + 1
    attr(p, "width")  <- display_w + 1

    p$data <- data
    p
}


# ═══════════════════════════════════════════════════════════════════════════════
# Layer 3: Exported public API
# ═══════════════════════════════════════════════════════════════════════════════

#' Linked Heatmap
#'
#' Draw two heatmaps side-by-side with link lines connecting matching rows.
#' The left heatmap typically shows ligands by sources, and the right heatmap
#' shows receptors by targets. Link lines connect matching ligand-receptor pairs.
#'
#' @param data A data frame in long format.
#' @param left A named list for the left heatmap. Must contain \code{rows_by},
#'   \code{columns_by}, \code{values_by}, and \code{name}. Optionally:
#'   \code{palette}, \code{palcolor}, \code{cluster_rows}, \code{cluster_columns},
#'   \code{show_row_names}, \code{show_column_names}.
#' @param right A named list for the right heatmap. Same structure as \code{left}.
#' @param link_by A character vector of length 2: the column linking left rows
#'   (e.g., \code{"ligand"}) and the column linking right rows
#'   (e.g., \code{"receptor"}).
#' @param link_width_by Column for link line width scaling.
#' @param link_width_scale Scaling factor for link widths (default 5).
#' @param link_color Color of link lines (default "grey40").
#' @param link_alpha Alpha transparency for link lines (default 0.6).
#' @param split_by Column to split data by for faceted linked heatmaps.
#' @param split_by_sep Separator for concatenated split_by columns.
#' @param combine Whether to combine subplots via patchwork (default TRUE).
#' @param nrow,ncol,byrow Grid layout for combined plots.
#' @param palette Palette for heatmap color scale (default "RdBu").
#' @param palcolor Custom color vector.
#' @param palreverse Reverse palette direction.
#' @param lower_quantile,upper_quantile Quantile cutoffs.
#' @param lower_cutoff,upper_cutoff Explicit cutoffs.
#' @param cluster_rows,cluster_columns Cluster heatmap rows/columns.
#' @param show_row_names,show_column_names Show row/column labels.
#' @param base_size Scaling factor for cell size.
#' @param left_aspect.ratio,right_aspect.ratio Aspect ratio (height/width) for
#'   cells in the left and right heatmaps. Default 1 gives square cells.
#'   Can also be specified inside the \code{left} / \code{right} list.
#' @param gap_width Width of gap between heatmaps (inches).
#' @param seed Random seed.
#' @inheritParams common_args
#' @param ... Other arguments passed to \code{\link[ComplexHeatmap]{Heatmap}}.
#'
#' @return A patchwork-wrapped grob.
#' @export
#' @examples
#' \donttest{
#' set.seed(8525)
#' ligands <- paste0("Ligand", 1:10)
#' receptors <- paste0("Receptor", 1:15)
#' sources <- paste0("Source", 1:4)
#' targets <- paste0("Target", 1:6)
#'
#' data <- expand.grid(
#'     ligand = ligands, receptor = receptors,
#'     source = sources, target = targets,
#'     stringsAsFactors = FALSE
#' )
#' data$ligand_expr <- runif(nrow(data), 0, 10)
#' data$receptor_expr <- runif(nrow(data), 0, 10)
#' data$intensity <- runif(nrow(data), 0, 1)
#'
#' if (requireNamespace("ComplexHeatmap", quietly = TRUE)) {
#'     LinkedHeatmap(data,
#'         left  = list(rows_by = "ligand", columns_by = "source",
#'                      values_by = "ligand_expr", name = "Ligand"),
#'         right = list(rows_by = "receptor", columns_by = "target",
#'                      values_by = "receptor_expr", name = "Receptor"),
#'         link_by = c("ligand", "receptor"),
#'         link_width_by = "intensity")
#' }
#' }
LinkedHeatmap <- function(
    data,
    values_by,
    values_fill = NA,
    name = NULL,
    split_by = NULL,
    split_by_sep = "_",
    # data definition
    rows_by = NULL,
    rows_by_sep = "_",
    rows_split_by = NULL,
    rows_split_by_sep = "_",
    columns_by = NULL,
    columns_by_sep = "_",
    columns_split_by = NULL,
    columns_split_by_sep = "_",
    rows_data = NULL,
    columns_data = NULL,
    keep_na = FALSE,
    keep_empty = FALSE,
    rows_orderby = NULL,
    columns_orderby = NULL,
    # names
    columns_name = NULL,
    columns_split_name = NULL,
    rows_name = NULL,
    rows_split_name = NULL,
    # palettes
    palette = "RdBu",
    palcolor = NULL,
    palreverse = FALSE,
    # cell_type: pies
    pie_size_name = "size",
    pie_size = NULL,
    pie_values = "length",
    pie_name = NULL,
    pie_group_by = NULL,
    pie_group_by_sep = "_",
    pie_palette = "Spectral",
    pie_palcolor = NULL,
    # cell_type: bars
    bars_sample = 100,
    # cell_type: label
    label = identity,
    label_size = 10,
    label_color = "black",
    label_name = "label",
    # cell_type: mark
    mark = identity,
    mark_color = "black",
    mark_size = 1,
    mark_name = "mark",
    # cell_type: violin
    violin_fill = NULL,
    # cell_type: boxplot
    boxplot_fill = NULL,
    # cell_type: dot
    dot_size = 8,
    dot_size_name = "size",
    # legend
    legend_items = NULL,
    legend_discrete = FALSE,
    legend.position = "right",
    legend.direction = "vertical",
    # values
    lower_quantile = 0,
    upper_quantile = 0.99,
    lower_cutoff = NULL,
    upper_cutoff = NULL,
    # bg
    add_bg = FALSE,
    bg_alpha = 0.5,
    # reticle
    add_reticle = FALSE,
    reticle_color = "grey",
    # passed to ComplexHeatmap::Heatmap
    cluster_columns = NULL,
    cluster_rows = NULL,
    show_row_names = NULL,
    show_column_names = NULL,
    border = TRUE,
    title = NULL,
    column_title = NULL,
    row_title = NULL,
    na_col = "grey85",
    row_names_side = "right",
    column_names_side = "bottom",
    column_annotation = NULL,
    column_annotation_side = "top",
    column_annotation_palette = "Paired",
    column_annotation_palcolor = NULL,
    column_annotation_type = "auto",
    column_annotation_params = list(),
    column_annotation_agg = NULL,
    row_annotation = NULL,
    row_annotation_side = "left",
    row_annotation_palette = "Paired",
    row_annotation_palcolor = NULL,
    row_annotation_type = "auto",
    row_annotation_params = list(),
    row_annotation_agg = NULL,
    # links
    links_width_by = NULL,
    link_width_scale = 5,
    link_color = "grey40",
    link_alpha = 0.6,

    # misc
    flip = FALSE,
    alpha = 1,
    seed = 8525,
    padding = 15,
    base_size = 1,
    aspect.ratio = NULL,
    draw_opts = list(),
    # cell customization
    layer_fun_callback = NULL,
    cell_type = c(
        "tile",
        "bars",
        "label",
        "mark",
        "label+mark",
        "mark+label",
        "dot",
        "violin",
        "boxplot",
        "pie"
    ),
    cell_agg = NULL,
    # subplots
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
    # ── Validate ──
    validate_common_args(seed)
    stopifnot(
        "[LinkedHeatmap] `flip` is not supported for linked heatmaps; please set `flip = FALSE`" =
        isFALSE(flip)
    )

    cell_type <- match.arg(cell_type)
    cell_type <- sub("mark+label", "label+mark", cell_type, fixed = TRUE)

    args <- list(...)
    left_rows_orderby <- args$left_rows_orderby %||% rows_orderby
    if (!is.null(left_rows_orderby)) {
        left_cluster_rows <- args$left_cluster_rows %||% cluster_rows %||% FALSE
        stopifnot(
            "[LinkedHeatmap] `left_rows_orderby` can't be used with `left_cluster_rows/cluster_rows = TRUE`" =
            !left_cluster_rows
        )
    } else {
        left_cluster_rows <- args$left_cluster_rows %||% cluster_rows %||% TRUE
    }

    right_rows_orderby <- args$right_rows_orderby %||% rows_orderby
    if (!is.null(right_rows_orderby)) {
        right_cluster_rows <- args$right_cluster_rows %||% cluster_rows %||% FALSE
        stopifnot(
            "[LinkedHeatmap] `right_rows_orderby` can't be used with `right_cluster_rows/cluster_rows = TRUE`" =
            !right_cluster_rows
        )
    } else {
        right_cluster_rows <- args$right_cluster_rows %||% cluster_rows %||% TRUE
    }

    # ── Preprocess data ──
    hmdata <- process_linkedheatmap_data(
        data,
        split_by = split_by,
        split_by_sep = split_by_sep,
        rows_split_by = rows_split_by,
        rows_split_by_sep = rows_split_by_sep,
        rows_split_name = rows_split_name,
        left_values_by = args$left_values_by %||% values_by,
        left_name = args$left_name %||% if (is.null(name)) NULL else paste0(name, " (left)"),
        left_rows_by = args$left_rows_by %||% rows_by,
        left_rows_by_sep = args$left_rows_by_sep %||% rows_by_sep,
        left_rows_name = args$left_rows_name %||% rows_name,
        left_rows_orderby = left_rows_orderby,
        left_columns_orderby = args$left_columns_orderby %||% columns_orderby,
        left_columns_by = args$left_columns_by %||% columns_by,
        left_columns_by_sep = args$left_columns_by_sep %||% columns_by_sep,
        left_columns_name = args$left_columns_name %||% columns_name,
        left_columns_split_by = args$left_columns_split_by %||% columns_split_by,
        left_columns_split_by_sep = args$left_columns_split_by_sep %||% columns_split_by_sep,
        left_columns_split_name = args$left_columns_split_name %||% columns_split_name,
        left_pie_group_by = args$left_pie_group_by %||% pie_group_by,
        left_pie_group_by_sep = args$left_pie_group_by_sep %||% pie_group_by_sep,
        left_pie_name = args$left_pie_name %||% pie_name,
        left_rows_data = args$left_rows_data %||% rows_data,
        left_columns_data = args$left_columns_data %||% columns_data,
        right_values_by = args$right_values_by %||% values_by,
        right_name = args$right_name %||% if (is.null(name)) NULL else paste0(name, " (right)"),
        right_rows_by = args$right_rows_by %||% rows_by,
        right_rows_by_sep = args$right_rows_by_sep %||% rows_by_sep,
        right_rows_name = args$right_rows_name %||% rows_name,
        right_rows_orderby = right_rows_orderby,
        right_columns_orderby = args$right_columns_orderby %||% columns_orderby,
        right_columns_by = args$right_columns_by %||% columns_by,
        right_columns_by_sep = args$right_columns_by_sep %||% columns_by_sep,
        right_columns_name = args$right_columns_name %||% columns_name,
        right_columns_split_by = args$right_columns_split_by %||% columns_split_by,
        right_columns_split_by_sep = args$right_columns_split_by_sep %||% columns_split_by_sep,
        right_columns_split_name = args$right_columns_split_name %||% columns_split_name,
        right_pie_group_by = args$right_pie_group_by %||% pie_group_by,
        right_pie_group_by_sep = args$right_pie_group_by_sep %||% pie_group_by_sep,
        right_pie_name = args$right_pie_name %||% pie_name,
        right_rows_data = args$right_rows_data %||% rows_data,
        right_columns_data = args$right_columns_data %||% columns_data,
        keep_na = keep_na,
        keep_empty = keep_empty
    )

    split_by <- split_by %||% "..."

    palette <- check_palette(palette, names(hmdata$data))
    palcolor <- check_palcolor(palcolor, names(hmdata$data))
    pie_palette <- check_palette(pie_palette, names(hmdata$data))
    pie_palcolor <- check_palcolor(pie_palcolor, names(hmdata$data))

    # ── Validate per-split legend params ──
    legend.position <- check_legend(
        legend.position, names(hmdata$data), "legend.position"
    )
    legend.direction <- check_legend(
        legend.direction, names(hmdata$data), "legend.direction"
    )

    # ── Build per-split plots ──
    plots <- lapply(names(hmdata$data), function(nm) {
        default_title <- if (
            length(hmdata$data) == 1 && identical(nm, "...")
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

        args_atomic <- args

        args_atomic$data <- hmdata$data[[nm]]

        args_atomic$left_values_by <- hmdata$left_values_by
        args_atomic$right_values_by <- hmdata$right_values_by
        args_atomic$left_rows_by <- hmdata$left_rows_by
        args_atomic$right_rows_by <- hmdata$right_rows_by
        args_atomic$rows_split_by <- hmdata$rows_split_by
        args_atomic$left_columns_by <- hmdata$left_columns_by
        args_atomic$right_columns_by <- hmdata$right_columns_by
        args_atomic$left_columns_split_by <- hmdata$left_columns_split_by
        args_atomic$right_columns_split_by <- hmdata$right_columns_split_by
        args_atomic$left_pie_group_by <- hmdata$left_pie_group_by
        args_atomic$right_pie_group_by <- hmdata$right_pie_group_by

        args_atomic$palette <- palette[[nm]]
        args_atomic$palcolor <- palcolor[[nm]]
        args_atomic$palreverse <- palreverse

        args_atomic$pie_size_name <- pie_size_name
        args_atomic$pie_size <- pie_size
        args_atomic$pie_values <- pie_values
        args_atomic$pie_palette <- pie_palette[[nm]]
        args_atomic$pie_palcolor <- pie_palcolor[[nm]]

        args_atomic$bars_sample <- bars_sample
        args_atomic$label <- label
        args_atomic$label_size <- label_size
        args_atomic$label_color <- label_color
        args_atomic$mark <- mark
        args_atomic$mark_color <- mark_color
        args_atomic$mark_size <- mark_size
        args_atomic$violin_fill <- violin_fill
        args_atomic$boxplot_fill <- boxplot_fill
        args_atomic$dot_size <- dot_size
        args_atomic$dot_size_name <- dot_size_name

        args_atomic$legend_items <- legend_items
        args_atomic$legend_discrete <- legend_discrete
        args_atomic$legend.position <- legend.position[[nm]]
        args_atomic$legend.direction <- legend.direction[[nm]]

        args_atomic$lower_quantile <- lower_quantile
        args_atomic$upper_quantile <- upper_quantile
        args_atomic$lower_cutoff <- lower_cutoff
        args_atomic$upper_cutoff <- upper_cutoff

        args_atomic$add_bg <- add_bg
        args_atomic$bg_alpha <- bg_alpha
        args_atomic$keep_na <- hmdata$keep_na
        args_atomic$keep_empty <- hmdata$keep_empty

        args_atomic$add_reticle <- add_reticle
        args_atomic$reticle_color <- reticle_color

        args_atomic$left_cluster_rows <- left_cluster_rows
        args_atomic$right_cluster_rows <- right_cluster_rows
        args_atomic$cluster_columns <- cluster_columns
        args_atomic$show_row_names <- show_row_names
        args_atomic$show_column_names <- show_column_names
        args_atomic$border <- border
        args_atomic$title <- title
        args_atomic$column_title <- column_title
        args_atomic$row_title <- row_title
        args_atomic$na_col <- na_col
        args_atomic$row_names_side <- row_names_side
        args_atomic$column_names_side <- column_names_side
        args_atomic$column_annotation <- column_annotation
        args_atomic$column_annotation_side <- column_annotation_side
        args_atomic$column_annotation_palette <- column_annotation_palette
        args_atomic$column_annotation_palcolor <- column_annotation_palcolor
        args_atomic$column_annotation_type <- column_annotation_type
        args_atomic$column_annotation_params <- column_annotation_params
        args_atomic$column_annotation_agg <- column_annotation_agg
        args_atomic$row_annotation <- row_annotation
        args_atomic$row_annotation_side <- row_annotation_side
        args_atomic$row_annotation_palette <- row_annotation_palette
        args_atomic$row_annotation_palcolor <- row_annotation_palcolor
        args_atomic$row_annotation_type <- row_annotation_type
        args_atomic$row_annotation_params <- row_annotation_params
        args_atomic$row_annotation_agg <- row_annotation_agg

        args_atomic$links_width_by <- links_width_by
        args_atomic$link_width_scale <- link_width_scale
        args_atomic$link_color <- link_color
        args_atomic$link_alpha <- link_alpha

        args_atomic$alpha <- alpha
        args_atomic$seed <- seed
        args_atomic$base_size <- base_size
        args_atomic$aspect.ratio <- aspect.ratio
        args_atomic$draw_opts <- draw_opts
        args_atomic$layer_fun_callback <- layer_fun_callback
        args_atomic$cell_type <- cell_type
        args_atomic$cell_agg <- cell_agg

        do_call(LinkedHeatmapAtomic, args_atomic)
    })

    names(plots) <- names(hmdata$data)

    # ── Combine ──
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
