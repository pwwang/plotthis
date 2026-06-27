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
#' @param seed Random seed.
#' @param ... Additional arguments passed to ComplexHeatmap::Heatmap.
#'
#' @return A patchwork-wrapped grob with height/width attributes.
#' @keywords internal
LinkedHeatmapAtomic <- function(
    data,
    left,
    right,
    link_left_by,
    link_right_by,
    link_width_by = NULL,
    link_width_scale = 5,
    link_color = "grey40",
    link_alpha = 0.6,
    gap_width = 0.5,
    palette = "RdBu",
    palcolor = NULL,
    palreverse = FALSE,
    lower_quantile = 0,
    upper_quantile = 0.99,
    lower_cutoff = NULL,
    upper_cutoff = NULL,
    cluster_rows = TRUE,
    cluster_columns = TRUE,
    show_row_names = TRUE,
    show_column_names = TRUE,
    row_names_side = NULL,
    column_names_side = NULL,
    base_size = 1,
    seed = 8525,
    legend.position = "right",
    legend.direction = "vertical",
    ...
) {
    set.seed(seed)

    # ── Resolve left/right parameters ──
    left_name     <- left$name     %||% "left"
    right_name    <- right$name    %||% "right"
    left_palette  <- left$palette  %||% palette
    right_palette <- right$palette %||% palette
    left_palcolor  <- left$palcolor  %||% palcolor
    right_palcolor <- right$palcolor %||% palcolor
    left_cluster_rows    <- left$cluster_rows    %||% cluster_rows
    right_cluster_rows   <- right$cluster_rows   %||% cluster_rows
    left_cluster_columns <- left$cluster_columns %||% cluster_columns
    right_cluster_columns <- right$cluster_columns %||% cluster_columns
    left_show_row_names    <- left$show_row_names    %||% show_row_names
    right_show_row_names   <- right$show_row_names   %||% show_row_names
    left_show_column_names <- left$show_column_names %||% show_column_names
    right_show_column_names <- right$show_column_names %||% show_column_names

    # Default sides: left heatmap annotations on left, right on right
    left_row_names_side <- row_names_side %||% "left"
    right_row_names_side <- row_names_side %||% "right"
    left_column_names_side <- column_names_side %||% "bottom"
    right_column_names_side <- column_names_side %||% "bottom"

    # ── Build left and right matrices ──
    left_mat_info <- .build_linked_matrix(
        data, left$rows_by, left$columns_by, left$values_by
    )
    right_mat_info <- .build_linked_matrix(
        data, right$rows_by, right$columns_by, right$values_by
    )

    left_mat  <- left_mat_info$matrix
    right_mat <- right_mat_info$matrix
    n_left_rows  <- nrow(left_mat)
    n_left_cols  <- ncol(left_mat)
    n_right_rows <- nrow(right_mat)
    n_right_cols <- ncol(right_mat)

    # ── Pre-compute clustering ──
    left_row_order <- seq_len(n_left_rows)
    left_row_dend  <- NULL
    if (isTRUE(left_cluster_rows) && n_left_rows > 1) {
        left_hc <- hclust(dist(left_mat))
        left_row_order <- left_hc$order
        left_row_dend <- as.dendrogram(left_hc)
    }

    right_row_order <- seq_len(n_right_rows)
    right_row_dend  <- NULL
    if (isTRUE(right_cluster_rows) && n_right_rows > 1) {
        right_hc <- hclust(dist(right_mat))
        right_row_order <- right_hc$order
        right_row_dend <- as.dendrogram(right_hc)
    }

    # ── Prepare color scales ──
    get_col_fun <- function(mat, pal, palcol, lower_q, upper_q,
                             lower_c, upper_c) {
        lower_c <- lower_c %||% quantile(
            mat[is.finite(mat)], lower_q, na.rm = TRUE
        )
        upper_c <- upper_c %||% quantile(
            mat[is.finite(mat)], upper_q, na.rm = TRUE
        )
        if (upper_c == lower_c) {
            upper_c <- if (upper_c == 0) 1e-3 else upper_c + upper_c * 1e-3
        }
        colorRamp2(
            seq(lower_c, upper_c, length = 100),
            palette_this(
                palette = pal, palcolor = palcol,
                alpha = 1, reverse = palreverse, transparent = FALSE
            )
        )
    }

    left_col_fun <- get_col_fun(
        left_mat, left_palette, left_palcolor,
        lower_quantile, upper_quantile,
        lower_cutoff, upper_cutoff
    )
    right_col_fun <- get_col_fun(
        right_mat, right_palette, right_palcolor,
        lower_quantile, upper_quantile,
        lower_cutoff, upper_cutoff
    )

    # ── Dimension constants ──
    mm_to_in <- 0.0393701
    cell_w <- 0.25 * base_size
    cell_h <- cell_w  # aspect ratio 1 for tiles

    dendro_h <- if (isTRUE(cluster_columns)) 0.5 else 0
    dendro_w <- if (isTRUE(cluster_rows)) 0.5 else 0
    # DENDROGRAM_PADDING (0.5mm) baked into dendrogram height by ComplexHeatmap
    dendro_h_actual <- dendro_h + if (isTRUE(cluster_columns)) 0.5 * mm_to_in else 0
    dendro_w_actual <- dendro_w + if (isTRUE(cluster_rows)) 0.5 * mm_to_in else 0

    # Column names height (rotated 90°, height = text width)
    left_colname_h <- if (isTRUE(left_show_column_names)) {
        cw <- ComplexHeatmap::max_text_width(colnames(left_mat))
        convertUnit(cw, "inches", valueOnly = TRUE)
    } else 0
    right_colname_h <- if (isTRUE(right_show_column_names)) {
        cw <- ComplexHeatmap::max_text_width(colnames(right_mat))
        convertUnit(cw, "inches", valueOnly = TRUE)
    } else 0

    # Row names width
    # Row names width + DIMNAME_PADDING*2 (2mm), matching CH layout:
    #   row_names_width = anno@width + ht_opt$DIMNAME_PADDING*2
    left_rowname_w <- if (isTRUE(left_show_row_names)) {
        convertUnit(
            ComplexHeatmap::max_text_width(rownames(left_mat)),
            "inches", valueOnly = TRUE
        ) + 2.0 * mm_to_in
    } else 0
    right_rowname_w <- if (isTRUE(right_show_row_names)) {
        convertUnit(
            ComplexHeatmap::max_text_width(rownames(right_mat)),
            "inches", valueOnly = TRUE
        ) + 2.0 * mm_to_in
    } else 0

    # ── Internal gap accounting ──
    # DIMNAME_PADDING: 1mm above + 1mm below
    # DIMNAME_PADDING = 1mm per side, so *2 = 2mm total
    left_gap_dimname  <- if (isTRUE(left_show_column_names))  2.0 * mm_to_in else 0
    right_gap_dimname <- if (isTRUE(right_show_column_names)) 2.0 * mm_to_in else 0

    # No column annotations in initial scope → gap_col_anno = 0
    left_gap_col_anno  <- 0
    right_gap_col_anno <- 0

    # No title → gap_title = 0, title_h = 0
    title_h   <- 0
    gap_title <- 0

    # ── Body dimensions ──
    left_body_w  <- n_left_cols  * cell_w
    left_body_h  <- n_left_rows  * cell_h
    right_body_w <- n_right_cols * cell_w
    right_body_h <- n_right_rows * cell_h

    # ── COMPLETE body_top_offset ──
    # ComplexHeatmap vertical layout (top→bottom):
    #   [1] column_title_top       (if title set)
    #   [2] column_dend_top        (if cluster_columns && dend_side="top")
    #   [3] column_names_top       (if show_column_names && names_side="top")
    #   [4] column_anno_top        (if column_annotation_side="top")
    #   [5] ═══════ HEATMAP BODY ═══════
    #   [6] column_anno_bottom     (if column_annotation_side="bottom")
    #   [7] column_names_bottom    (if show_column_names && names_side="bottom")
    #   [8] column_dend_bottom     (if cluster_columns && dend_side="bottom")
    #   [9] column_title_bottom    (if title set)
    #
    # Only components in slots [1]-[4] count toward body_top_offset.
    # Column dendrograms default to "top", column names default to "bottom".

    # Helper: should a component be placed ABOVE the body?
    is_top <- function(side) identical(side, "top")

    body_top_offset_left <- 0
    if (title_h > 0)            body_top_offset_left <- body_top_offset_left + title_h
    if (gap_title > 0)          body_top_offset_left <- body_top_offset_left + gap_title
    if (is_top("top") && dendro_h_actual > 0)
                                body_top_offset_left <- body_top_offset_left + dendro_h_actual
    if (is_top(left_column_names_side) && left_colname_h > 0)
                                body_top_offset_left <- body_top_offset_left + left_colname_h
    if (is_top(left_column_names_side) && left_gap_dimname > 0)
                                body_top_offset_left <- body_top_offset_left + left_gap_dimname
    if (left_gap_col_anno > 0)  body_top_offset_left <- body_top_offset_left + left_gap_col_anno
    body_top_offset_left <- body_top_offset_left

    body_top_offset_right <- 0
    if (title_h > 0)            body_top_offset_right <- body_top_offset_right + title_h
    if (gap_title > 0)          body_top_offset_right <- body_top_offset_right + gap_title
    if (is_top("top") && dendro_h_actual > 0)
                                body_top_offset_right <- body_top_offset_right + dendro_h_actual
    if (is_top(right_column_names_side) && right_colname_h > 0)
                                body_top_offset_right <- body_top_offset_right + right_colname_h
    if (is_top(right_column_names_side) && right_gap_dimname > 0)
                                body_top_offset_right <- body_top_offset_right + right_gap_dimname
    if (right_gap_col_anno > 0) body_top_offset_right <- body_top_offset_right + right_gap_col_anno
    body_top_offset_right <- body_top_offset_right

    # ── Total heatmap dimensions ──
    # left_total_h / right_total_h: FULL height including ALL components
    # (above body + body + below body). Used for centering and total_h.
    # Below-body components (added for total height but NOT body_top_offset):
    left_below_h <- 0
    if (!is_top(left_column_names_side)) {
        left_below_h <- left_below_h + left_colname_h + left_gap_dimname
    }
    right_below_h <- 0
    if (!is_top(right_column_names_side)) {
        right_below_h <- right_below_h + right_colname_h + right_gap_dimname
    }

    left_total_w  <- dendro_w_actual + left_rowname_w  + left_body_w
    left_total_h  <- body_top_offset_left  + left_body_h + left_below_h
    right_total_w <- right_body_w + right_rowname_w + dendro_w_actual
    right_total_h <- body_top_offset_right + right_body_h + right_below_h

    total_w <- left_total_w + gap_width + right_total_w
    total_h <- max(left_total_h, right_total_h)

    # ── Pre-estimate legend dimensions for scale_factor ──
    # The actual legends are built later from ComplexHeatmap objects,
    # but we can estimate their footprint now from the heatmap names
    legend_gap <- 0.15
    legend_w_est <- 0
    legend_h_est <- 0
    show_legend <- !identical(legend.position, "none")

    if (show_legend) {
        title_w <- max(
            convertUnit(
                ComplexHeatmap::max_text_width(left_name),
                "inches", valueOnly = TRUE
            ),
            convertUnit(
                ComplexHeatmap::max_text_width(right_name),
                "inches", valueOnly = TRUE
            ),
            0
        )
        legend_w_est <- max(0.6, title_w) + 0.6
        if (legend.direction == "vertical") {
            legend_h_est <- 2 * 0.7 + 0.157  # 2 legends stacked + 4mm gap
        } else {
            legend_h_est <- 0.7
        }
    }

    plot_w_est <- total_w
    plot_h_est <- total_h
    if (show_legend) {
        if (legend.position == "right" || legend.position == "left") {
            plot_w_est <- total_w + legend_gap + legend_w_est
        } else if (legend.position == "top" || legend.position == "bottom") {
            plot_h_est <- total_h + legend_gap + legend_h_est
        }
    }

    body_top_offset_left <- 0
    if (title_h > 0)            body_top_offset_left <- body_top_offset_left + title_h
    if (gap_title > 0)          body_top_offset_left <- body_top_offset_left + gap_title
    if (is_top("top") && dendro_h_actual > 0)
                                body_top_offset_left <- body_top_offset_left + dendro_h_actual
    if (is_top(left_column_names_side) && left_colname_h > 0)
                                body_top_offset_left <- body_top_offset_left + left_colname_h
    if (is_top(left_column_names_side) && left_gap_dimname > 0)
                                body_top_offset_left <- body_top_offset_left + left_gap_dimname
    if (left_gap_col_anno > 0)  body_top_offset_left <- body_top_offset_left + left_gap_col_anno
    body_top_offset_left <- body_top_offset_left
    body_top_offset_right <- 0
    if (title_h > 0)            body_top_offset_right <- body_top_offset_right + title_h
    if (gap_title > 0)          body_top_offset_right <- body_top_offset_right + gap_title
    if (is_top("top") && dendro_h_actual > 0)
                                body_top_offset_right <- body_top_offset_right + dendro_h_actual
    if (is_top(right_column_names_side) && right_colname_h > 0)
                                body_top_offset_right <- body_top_offset_right + right_colname_h
    if (is_top(right_column_names_side) && right_gap_dimname > 0)
                                body_top_offset_right <- body_top_offset_right + right_gap_dimname
    if (right_gap_col_anno > 0) body_top_offset_right <- body_top_offset_right + right_gap_col_anno
    body_top_offset_right <- body_top_offset_right

    # Recompute below-body totals after scaling
    left_below_h <- 0
    if (!is_top(left_column_names_side)) {
        left_below_h <- left_below_h + left_colname_h + left_gap_dimname
    }
    right_below_h <- 0
    if (!is_top(right_column_names_side)) {
        right_below_h <- right_below_h + right_colname_h + right_gap_dimname
    }

    left_total_w  <- dendro_w_actual + left_rowname_w  + left_body_w
    left_total_h  <- body_top_offset_left  + left_body_h + left_below_h
    right_total_w <- right_body_w + right_rowname_w + dendro_w_actual
    right_total_h <- body_top_offset_right + right_body_h + right_below_h
    total_w <- left_total_w + gap_width + right_total_w + 0.5
    total_h <- max(left_total_h, right_total_h) + 1

    # ── Build link table ──
    left_row_names_ordered <- rownames(left_mat)[left_row_order]
    right_row_names_ordered <- rownames(right_mat)[right_row_order]

    link_table <- data %>%
        distinct(!!sym(link_left_by), !!sym(link_right_by),
                 .keep_all = TRUE) %>%
        mutate(
            pos_left = match(
                as.character(!!sym(link_left_by)), left_row_names_ordered
            ),
            pos_right = match(
                as.character(!!sym(link_right_by)), right_row_names_ordered
            )
        ) %>%
        filter(!is.na(.data$pos_left), !is.na(.data$pos_right))

    if (!is.null(link_width_by) && link_width_by %in% colnames(link_table)) {
        raw_intensity <- link_table[[link_width_by]]
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

    # ── Build ComplexHeatmap objects ──
    left_hm_args <- list(
        matrix = left_mat,
        name = left_name,
        col = left_col_fun,
        cluster_rows = FALSE,  # pre-clustered
        cluster_columns = left_cluster_columns,
        row_order = left_row_order,
        row_dend = left_row_dend,
        show_row_names = left_show_row_names,
        show_column_names = left_show_column_names,
        row_names_side = left_row_names_side,
        column_names_side = left_column_names_side,
        column_title = NULL,
        row_title = NULL,
        row_dend_side = "left",
        border = TRUE,
        width = unit(left_body_w, "inches"),
        height = unit(left_body_h, "inches"),
        row_dend_width = unit(dendro_w, "inches"),
        column_dend_height = unit(dendro_h, "inches"),
        ...
    )

    right_hm_args <- list(
        matrix = right_mat,
        name = right_name,
        col = right_col_fun,
        cluster_rows = FALSE,
        cluster_columns = right_cluster_columns,
        row_order = right_row_order,
        row_dend = right_row_dend,
        show_row_names = right_show_row_names,
        show_column_names = right_show_column_names,
        row_names_side = right_row_names_side,
        column_names_side = right_column_names_side,
        column_title = NULL,
        row_title = NULL,
        row_dend_side = "right",
        border = TRUE,
        width = unit(right_body_w, "inches"),
        height = unit(right_body_h, "inches"),
        row_dend_width = unit(dendro_w, "inches"),
        column_dend_height = unit(dendro_h, "inches"),
        ...
    )

    # Filter unknown args
    hm_formals <- methods::formalArgs(ComplexHeatmap::Heatmap)
    left_hm_args  <- left_hm_args[intersect(names(left_hm_args), hm_formals)]
    right_hm_args <- right_hm_args[intersect(names(right_hm_args), hm_formals)]

    left_ht  <- do_call(ComplexHeatmap::Heatmap, left_hm_args)
    right_ht <- do_call(ComplexHeatmap::Heatmap, right_hm_args)

    # ── Collect legends (before link position computation so legend
    #     dimensions can be factored into plot_h for top/bottom) ──
    combined_legend <- NULL
    legend_w <- 0
    legend_h <- 0

    if (show_legend) {
        legends <- list()
        if (!is.null(left_ht@matrix_color_mapping)) {
            left_lgd <- ComplexHeatmap::color_mapping_legend(
                left_ht@matrix_color_mapping, plot = FALSE,
                legend_direction = legend.direction
            )
            if (!is.null(left_lgd)) legends <- c(legends, list(left_lgd))
        }
        if (!is.null(right_ht@matrix_color_mapping)) {
            right_lgd <- ComplexHeatmap::color_mapping_legend(
                right_ht@matrix_color_mapping, plot = FALSE,
                legend_direction = legend.direction
            )
            if (!is.null(right_lgd)) legends <- c(legends, list(right_lgd))
        }

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
        if (legend.position == "right" || legend.position == "left") {
            plot_w <- total_w + legend_gap + legend_w
        } else if (legend.position == "top" || legend.position == "bottom") {
            plot_h <- total_h + legend_gap + legend_h
        }
    }

    # ── Link position computation ──
    # NPC coords use total_h (heatmap-only height) because each heatmap
    # viewport receives that exact height from the grid layout.
    left_center_offset  <- (total_h - left_total_h)  / 2
    right_center_offset <- (total_h - right_total_h) / 2

    left_body_top_npc  <- 1 - (body_top_offset_left  + left_center_offset)  / total_h
    left_body_bot_npc  <- 1 - (body_top_offset_left  + left_center_offset + left_body_h)  / total_h
    left_body_range    <- left_body_top_npc - left_body_bot_npc

    right_body_top_npc <- 1 - (body_top_offset_right + right_center_offset) / total_h
    right_body_bot_npc <- 1 - (body_top_offset_right + right_center_offset + right_body_h) / total_h
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

        # Left heatmap
        pushViewport(viewport(layout.pos.row = hm_row,
                              layout.pos.col = hm_col_left))
        ComplexHeatmap::draw(
            left_ht,
            newpage = FALSE,
            show_heatmap_legend = FALSE,
            show_annotation_legend = FALSE,
            padding = unit(c(0, 0, 0, 0), "mm")
        )
        popViewport()

        # Right heatmap
        pushViewport(viewport(layout.pos.row = hm_row,
                              layout.pos.col = hm_col_right))
        ComplexHeatmap::draw(
            right_ht,
            newpage = FALSE,
            show_heatmap_legend = FALSE,
            show_annotation_legend = FALSE,
            padding = unit(c(0, 0, 0, 0), "mm")
        )
        popViewport()

        # Legend
        if (!is.null(combined_legend)) {
            if (pos_left || pos_right) {
                pushViewport(viewport(layout.pos.row = lg_row,
                                      layout.pos.col = lg_col))
                ComplexHeatmap::draw(
                    combined_legend,
                    x = unit(0, "npc"),
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
                    x = unit(c(0, 0.5, 1), "npc"),
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
    min_size_in <- 4
    max_size_in <- 80
    display_h <- max(min(plot_h, max_size_in), min_size_in)
    display_w <- max(min(plot_w, max_size_in), min_size_in)
    ratio <- plot_h / plot_w
    if (ratio > 1 && display_h == max_size_in) {
        display_w <- display_h / ratio
    } else if (ratio < 1 && display_w == max_size_in) {
        display_h <- display_w * ratio
    }
    attr(p, "height") <- display_h
    attr(p, "width")  <- display_w

    # Attach data (store matrices in a non-conflicting attribute name
    # to avoid breaking patchwork/ggplot2 rich display rendering)
    attr(p, "linkedheatmap_data") <- list(
        left = as.data.frame(left_mat),
        right = as.data.frame(right_mat)
    )
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
    left,
    right,
    link_by = NULL,
    link_width_by = NULL,
    link_width_scale = 5,
    link_color = "grey40",
    link_alpha = 0.6,
    split_by = NULL,
    split_by_sep = "_",
    combine = TRUE,
    nrow = NULL,
    ncol = NULL,
    byrow = TRUE,
    palette = "RdBu",
    palcolor = NULL,
    palreverse = FALSE,
    lower_quantile = 0,
    upper_quantile = 0.99,
    lower_cutoff = NULL,
    upper_cutoff = NULL,
    cluster_rows = TRUE,
    cluster_columns = TRUE,
    show_row_names = TRUE,
    show_column_names = TRUE,
    base_size = 1,
    gap_width = 0.5,
    seed = 8525,
    legend.position = "right",
    legend.direction = "vertical",
    ...
) {
    # ── Validate ──
    if (is.null(link_by) || length(link_by) != 2) {
        stop("[LinkedHeatmap] 'link_by' must be a character vector of length 2")
    }
    link_left_by  <- link_by[1]
    link_right_by <- link_by[2]

    required_left  <- c("rows_by", "columns_by", "values_by", "name")
    required_right <- c("rows_by", "columns_by", "values_by", "name")
    missing_left  <- setdiff(required_left, names(left))
    missing_right <- setdiff(required_right, names(right))
    if (length(missing_left) > 0) {
        stop("[LinkedHeatmap] 'left' is missing: ",
             paste(missing_left, collapse = ", "))
    }
    if (length(missing_right) > 0) {
        stop("[LinkedHeatmap] 'right' is missing: ",
             paste(missing_right, collapse = ", "))
    }

    validate_common_args(seed)

    # ── Handle split_by ──
    split_by <- check_columns(
        data, split_by,
        force_factor = TRUE, allow_multi = TRUE,
        concat_multi = TRUE, concat_sep = split_by_sep
    )

    if (!is.null(split_by)) {
        datas <- split(data, data[[split_by]])
        datas <- datas[levels(data[[split_by]])]
    } else {
        datas <- list(data)
        names(datas) <- "..."
    }

    # ── Validate per-split legend params ──
    legend.position <- check_legend(
        legend.position, names(datas), "legend.position"
    )
    legend.direction <- check_legend(
        legend.direction, names(datas), "legend.direction"
    )

    # ── Build per-split plots ──
    plots <- lapply(names(datas), function(nm) {
        LinkedHeatmapAtomic(
            datas[[nm]],
            left = left,
            right = right,
            link_left_by = link_left_by,
            link_right_by = link_right_by,
            link_width_by = link_width_by,
            link_width_scale = link_width_scale,
            link_color = link_color,
            link_alpha = link_alpha,
            gap_width = gap_width,
            palette = left$palette %||% right$palette %||% palette,
            palcolor = left$palcolor %||% right$palcolor %||% palcolor,
            palreverse = palreverse,
            lower_quantile = lower_quantile,
            upper_quantile = upper_quantile,
            lower_cutoff = lower_cutoff,
            upper_cutoff = upper_cutoff,
            cluster_rows = cluster_rows,
            cluster_columns = cluster_columns,
            show_row_names = show_row_names,
            show_column_names = show_column_names,
            base_size = base_size,
            seed = seed,
            legend.position = legend.position[[nm]],
            legend.direction = legend.direction[[nm]],
            ...
        )
    })

    names(plots) <- names(datas)

    # ── Combine ──
    combine_plots(
        plots,
        combine = combine,
        split_by = split_by,
        nrow = nrow, ncol = ncol, byrow = byrow
    )
}
