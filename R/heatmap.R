#' Atomic heatmap (internal)
#'
#' @description
#' Core implementation for drawing a single heatmap using
#' \code{ComplexHeatmap::Heatmap}.  This function takes pre-processed data
#' (already in long format with resolved row/column identifiers) and handles
#' colour mapping, cell rendering, annotation construction, and the final
#' \code{draw()} call.  It is the workhorse behind the exported
#' \code{\link{Heatmap}} function and is also reused by
#' \code{\link{LinkedHeatmapAtomic}} (via \code{return_ht = TRUE}).
#'
#' @section Architecture:
#' \enumerate{
#'   \item \strong{Annotation resolution} — row and column annotation
#'         parameters (side, type, palette, params, aggregation) are
#'         resolved with alias support (\code{.row}, \code{.col},
#'         \code{.rows.split}, etc.) so that built-in name/split annotations
#'         and user-defined annotations can be configured uniformly.
#'   \item \strong{Keep-NA / keep-empty handling} — \code{keep_na} and
#'         \code{keep_empty} are processed per column to control whether
#'         \code{NA} values and empty factor levels are preserved in the
#'         data before constructing the heatmap matrix.
#'   \item \strong{Flip} — when \code{flip = TRUE}, row and column
#'         parameters are swapped transparently so the caller does not need
#'         to manually swap every argument.
#'   \item \strong{Cell dimension calculation} — cell width and height are
#'         pre-computed from \code{cell_type}, \code{aspect.ratio},
#'         \code{base_size}, and the unique row/column counts (accounting
#'         for split groups).  These are passed as explicit
#'         \code{"inches"} units to \code{ComplexHeatmap::Heatmap()} so
#'         cells have guaranteed physical sizes.
#'   \item \strong{Colour mapping} — the main colour scale is built from
#'         \code{palette} / \code{palcolor} with optional quantile-based
#'         or explicit cutoff clamping.
#'   \item \strong{Cell function dispatch} — depending on
#'         \code{cell_type}, the appropriate \code{cell_fun} or
#'         \code{layer_fun} is installed (tile, bars, label, mark,
#'         label+mark, dot, violin, boxplot, pie).
#'   \item \strong{Annotation construction} — row and column annotations
#'         are built via \code{ComplexHeatmap::HeatmapAnnotation()} using
#'         the resolved type and parameters.
#'   \item \strong{Drawing} — the heatmap is drawn via
#'         \code{ComplexHeatmap::draw()} with controlled padding and legend
#'         collection.  When \code{return_ht = TRUE}, the prepared
#'         \code{Heatmap} object is returned without drawing.
#'   \item \strong{Dimension attributes} — the returned object carries
#'         \code{height} / \code{width} attributes (in inches) and
#'         \code{cell_w} / \code{cell_h} attributes for downstream layout
#'         calculations.
#' }
#'
#' @inheritParams common_args
#' @inheritParams process_heatmap_data
#' @param data A data frame in long format. Each row represents one
#'  observation; columns specify row/column membership and the value to
#'  encode as colour.
#' @param values_fill A value used to fill missing cells in the matrix.
#'  Default \code{NA}.  Missing values prevent clustering when not filled.
#' @param border A logical value indicating whether to draw borders around
#'  the heatmap.  If \code{TRUE}, slice borders are also drawn.  Default
#'  \code{TRUE}.
#' @param title The global (column) title of the heatmap.
#' @param lower_quantile,upper_quantile,lower_cutoff,upper_cutoff
#'  Quantile or explicit cutoffs for clipping the colour scale.  Applied
#'  to aggregated values for \code{tile} / \code{label} cell types; applied
#'  to raw values for \code{bars} / \code{violin} / \code{boxplot} types.
#' @param cluster_columns Logical; cluster the columns.  If \code{TRUE} and
#'  \code{columns_split_by} is provided, clustering is applied within each
#'  split group.
#' @param cluster_rows Logical; cluster the rows.  If \code{TRUE} and
#'  \code{rows_split_by} is provided, clustering is applied within each
#'  split group.
#' @param legend_items A named numeric vector specifying custom legend
#'  entries for the main colour scale.  Names become the displayed labels.
#' @param legend_discrete Logical; if \code{TRUE}, treat the main colour
#'  scale as discrete.
#' @param show_row_names Logical; show row names.  If \code{TRUE}, the
#'  legend of the row group annotation is hidden.
#' @param show_column_names Logical; show column names.  If \code{TRUE},
#'  the legend of the column group annotation is hidden.
#' @param column_title Character string/vector used as the column group
#'  annotation title.
#' @param row_title Character string/vector used as the row group
#'  annotation title.
#' @param na_col Colour for \code{NA} cells.  Default \code{"grey85"}.
#' @param row_names_side Side for row names.  Default \code{"right"}.
#' @param column_names_side Side for column names.  Default \code{"bottom"}.
#' @param bars_sample Number of observations sampled per cell when
#'  \code{cell_type = "bars"}.  Default 100.
#' @param flip Logical; if \code{TRUE}, swap rows and columns
#'  transparently.  The caller does not need to swap row- and
#'  column-related arguments manually.
#' @param pie_palette,pie_palcolor Palette and custom colours for pie slice
#'  fill colours.
#' @param pie_values A function or string (convertible via
#'  \code{\link[base]{match.arg}}) to compute the value represented by
#'  each pie slice.  Default \code{"length"} counts observations per
#'  group.
#' @param pie_size A numeric value or function returning the pie radius.
#'  When a function, it receives the count of groups in the pie.
#' @param pie_size_name Legend title for the pie size.
#' @param label A function to compute text labels when
#'  \code{cell_type = "label"} (or \code{"label+mark"}).  Receives the
#'  aggregated value for a cell and optionally row/column indices and
#'  names.  See below for the full dispatch contract.
#' @param label_size Default point size for label text (used as fallback
#'  when the \code{label} function does not return a \code{size} field).
#' @param label_color Default colour for label text (fallback).
#' @param label_name Legend title for the label colour scale.  The legend
#'  is shown automatically when the \code{label} function returns a
#'  \code{legend} field for at least one cell.
#' @param mark A function to compute mark symbols when
#'  \code{cell_type = "mark"} (or \code{"label+mark"}).  Same dispatch
#'  contract as \code{label}.
#' @param mark_size Default mark stroke width (\code{lwd}) in pt (fallback).
#' @param mark_color Default mark colour (fallback).
#' @param mark_name Legend title for the mark colour scale.
#' @param layer_fun_callback A function to add custom graphical layers on
#'  top of each heatmap cell.  Receives \code{j}, \code{i}, \code{x},
#'  \code{y}, \code{w}, \code{h}, \code{fill}, \code{sr}, \code{sc}.
#'  See \code{\link[ComplexHeatmap]{Heatmap}} for details.
#' @param cell_type The type of cell to render.  One of \code{"tile"}
#'  (default), \code{"bars"}, \code{"label"}, \code{"mark"},
#'  \code{"label+mark"} (or \code{"mark+label"}), \code{"dot"},
#'  \code{"violin"}, \code{"boxplot"}, \code{"pie"}.  See the
#'  \strong{Cell types} section for details.
#' @param cell_agg A function to aggregate values within each cell when
#'  \code{cell_type = "tile"} or \code{"label"}.  Default is
#'  \code{\link[base]{mean}}.
#' @param add_bg Logical; if \code{TRUE}, add a background fill behind
#'  non-tile cell types.  Not used for \code{cell_type = "tile"} or
#'  \code{"bars"}.
#' @param bg_alpha Numeric in \eqn{[0, 1]} for background transparency.
#' @param violin_fill A character vector of colours to use as fill for
#'  violin plots when \code{cell_type = "violin"}.  If \code{NULL}, the
#'  annotation colour is used.
#' @param boxplot_fill A character vector of colours to use as fill for
#'  boxplots when \code{cell_type = "boxplot"}.  If \code{NULL}, the
#'  annotation colour is used.
#' @param dot_size Dot size when \code{cell_type = "dot"}.  Can be a
#'  numeric value or a function.
#' @param dot_size_name Legend title for the dot size.
#' @param column_annotation A character vector of column names, or a named
#'  list, specifying column annotations.  See the \strong{Annotations}
#'  section for the full specification.
#' @param column_annotation_side A character string or named list
#'  specifying which side each column annotation is placed on.  Accepts
#'  \code{"top"} (default) or \code{"bottom"}.  With a named list, use
#'  keys \code{.col}, \code{.col.split}, and \code{.default} for
#'  per-annotation control.
#' @param column_annotation_palette,column_annotation_palcolor Palette and
#'  custom colours for column annotations.  Can be a named list keyed by
#'  annotation name.
#' @param column_annotation_type Annotation type: \code{"auto"} (default),
#'  \code{"simple"}, \code{"pie"}, \code{"ring"}, \code{"bar"},
#'  \code{"violin"}, \code{"boxplot"}, \code{"density"}, \code{"label"},
#'  \code{"points"}, \code{"lines"}.  Can be a named list for
#'  per-annotation control.  Aliases: \code{.col.split}, \code{.col}.
#' @param column_annotation_params A named list of additional parameters
#'  passed to each column annotation function.  Use aliases
#'  \code{.col}/\code{.cols} for \code{columns_by} and
#'  \code{.col.split}/\code{.cols.split} for \code{columns_split_by}.
#'  Setting a key to \code{FALSE} disables that annotation;
#'  \code{$<key>$show_legend} controls its legend visibility.
#'  See \code{\link[ComplexHeatmap]{HeatmapAnnotation}} for details.
#' @param column_annotation_agg A function or named list of functions to
#'  aggregate values for each column annotation.  Defaults vary by
#'  annotation type.
#' @param row_annotation,row_annotation_side,row_annotation_palette,row_annotation_palcolor,row_annotation_type,row_annotation_params,row_annotation_agg
#'  Row annotation equivalents of the \code{column_annotation_*}
#'  parameters.  Sides default to \code{"left"}.  Aliases: \code{.row}
#'  /\code{.rows} for \code{rows_by}, \code{.rows.split}/\code{.row.split}
#'  for \code{rows_split_by}.
#' @param add_reticle Logical; if \code{TRUE}, draw a reticle (crosshair
#'  pattern) over the heatmap.
#' @param reticle_color Colour for the reticle lines.
#' @param palette A character string naming a palette (see
#'  \code{\link{show_palettes}}) or a character vector of colours for the
#'  main heatmap colour scale.  Default \code{"RdBu"}.
#' @param palcolor A custom colour vector overriding \code{palette}.
#' @param alpha Alpha transparency for heatmap cells in \eqn{[0, 1]}.
#' @param padding Padding around the heatmap in CSS order (top, right,
#'  bottom, left).  Supports 1–4 values.  Default 15 (mm).  Note that
#'  this is different from \code{ComplexHeatmap::draw()}'s \code{padding}
#'  argument which uses bottom-left-top-right order.
#' @param base_size A positive numeric scalar used as a scaling factor for
#'  the overall heatmap size.  Default 1 (no scaling).  Values > 1 enlarge
#'  all cell dimensions proportionally.
#' @param aspect.ratio Height-to-width ratio of a single heatmap cell.
#'  When \code{NULL} (default), sensible per-\code{cell_type} defaults are
#'  used: 1 for \code{tile}/\code{label}/\code{dot}, 0.5 for \code{bars},
#'  and 2 for \code{violin}/\code{boxplot}/\code{pie}.  The ratio is
#'  constrained by the overall plot dimensions.
#' @param return_ht Logical; if \code{TRUE}, return the prepared
#'  \code{ComplexHeatmap::Heatmap} object without drawing.  Used
#'  internally by \code{\link{LinkedHeatmapAtomic}}.
#' @param draw_opts A named list of additional arguments passed to
#'  \code{\link[ComplexHeatmap]{draw,HeatmapList-method}}.  Internally
#'  managed arguments take precedence.
#' @param ... Additional arguments passed to
#'  \code{\link[ComplexHeatmap]{Heatmap}}.  When
#'  \code{row_names_max_width} is passed, a \code{unit} is expected but
#'  numeric values (default unit \code{"inches"}) or strings like
#'  \code{"5inches"} are also accepted.  Unmatched arguments produce a
#'  warning.
#'
#' @section Label / mark dispatch contract:
#' The \code{label} and \code{mark} functions can take 1, 3, or 5
#' arguments.  The first argument is always the aggregated value for a
#' single cell.  With 3 arguments, the second and third are the row and
#' column indices.  With 5 arguments, the fourth and fifth are the row
#' and column names.  The function should return one of:
#' \itemize{
#'   \item \code{NA} — nothing is drawn for this cell.
#'   \item A character scalar — used as the label text / mark type;
#'         default size and colour are used.
#'   \item A named list with fields \code{label}/\code{mark},
#'         \code{size}, \code{color}, \code{legend}, and \code{order}
#'         (all optional; smaller \code{order} appears first in the
#'         legend).
#' }
#' For label cells with custom indices, use
#' \code{ComplexHeatmap::pindex()} to translate between data and heatmap
#' coordinates.
#'
#' @section Supported mark types:
#' \itemize{
#'   \item \strong{Primitives:} \code{-} (h-line), \code{|} (v-line),
#'         \code{+} (cross), \code{/} (l-diag), \code{\\} (r-diag),
#'         \code{x} (both diags), \code{o} (circle), \code{()}
#'         (edge-touching circle), \code{<>} (diamond).
#'   \item \strong{With rectangular border:} \code{[]}, \code{[-]},
#'         \code{[|]}, \code{[+]}, \code{[/]}, \code{[\\]}, \code{[x]},
#'         \code{[o]}, \code{[()]}, \code{[<>]}.
#'   \item \strong{With full circle:} \code{(-)}, \code{(|)},
#'         \code{(+)}, \code{(/)}, \code{(\\)}, \code{(x)}, \code{(o)},
#'         \code{(<>)}.
#'   \item \strong{With diamond:} \code{<->}, \code{<|>}, \code{<+>},
#'         \code{</>}, \code{<\\>}, \code{<x>}, \code{<o>}.
#'   \item \strong{Octagon:} \code{\{\}}, \code{\{-\}}, \code{\{|\}},
#'         \code{\{+\}}, \code{\{/\}}, \code{\{\\\}}, \code{\{x\}},
#'         \code{\{o\}}, \code{\{()\}}, \code{\{<>\}}.
#'   \item \strong{Combinations:} e.g. \code{[(|)]}, \code{[(-)]},
#'         \code{[(+)]}, \code{[(/)]}, \code{[(\\]}, \code{[(x)]},
#'         \code{[(o)]}, \code{[(<>)]}.
#' }
#'
#' @section Annotations:
#' Row and column annotations are built via
#' \code{\link[ComplexHeatmap]{HeatmapAnnotation}}.  Built-in name
#' annotations (for \code{rows_by} / \code{columns_by}) and split
#' annotations (for \code{rows_split_by} / \code{columns_split_by}) are
#' added automatically and can be configured using the aliases
#' \code{.row} / \code{.col} and \code{.rows.split} / \code{.col.split}
#' in \code{row_annotation_params}, \code{column_annotation_type}, etc.
#' Setting an alias to \code{FALSE} in \code{*_params} disables that
#' annotation.  Ordering within each side: name annotations are closest
#' to the body, split annotations farthest away, user-defined annotations
#' in between.
#'
#' @note Removed parameters: \code{rows_palette}, \code{rows_palcolor},
#'  \code{columns_palette}, \code{columns_palcolor},
#'  \code{columns_split_palette}, \code{columns_split_palcolor},
#'  \code{rows_split_palette}, \code{rows_split_palcolor} — use
#'  \code{row_annotation_palette}/\code{row_annotation_palcolor} with
#'  key \code{.row} or \code{.rows.split}, and
#'  \code{column_annotation_palette}/\code{column_annotation_palcolor}
#'  with \code{.col} or \code{.col.split}.  Also removed:
#'  \code{row_name_annotation}, \code{row_name_legend},
#'  \code{column_name_annotation}, \code{column_name_legend} — set
#'  \code{row_annotation_params$.row} / \code{column_annotation_params$.col}
#'  to \code{FALSE} and use \code{$show_legend} within the param entry.
#' @importFrom circlize colorRamp2
#' @importFrom dplyr group_by across ungroup %>% all_of summarise first everything group_map
#' @importFrom tidyr pivot_longer pivot_wider unite expand_grid
#' @importFrom ggplot2 ggplotGrob theme_void
#' @importFrom grid grid.rect grid.text grid.lines grid.points viewport gpar unit grid.draw is.unit
#' @importFrom grid convertUnit grid.grabExpr grid.segments grid.circle grid.polygon unit.c
#' @return An object of heatmap that is wrapped by `patchwork::wrap_plots()`
#' @keywords internal
HeatmapAtomic <- function(
    data,
    values_by,
    values_fill = NA,
    # data definition
    rows_by = NULL,
    rows_split_by = NULL,
    columns_by = NULL,
    columns_split_by = NULL,
    # palettes
    palette = "RdBu",
    palcolor = NULL,
    palreverse = FALSE,
    # cell_type: pies
    pie_size_name = "size",
    pie_size = NULL,
    pie_values = "length",
    pie_group_by = NULL,
    pie_palette = "Spectral",
    pie_palcolor = NULL,
    # cell_type: bars
    bars_sample = 100,
    # cell_type: label
    label = scales::label_number_auto(),
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
    # passed to ComplexHeatmap::Heatmap
    cluster_columns = TRUE,
    cluster_rows = TRUE,
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
    # misc
    flip = FALSE,
    alpha = 1,
    seed = 8525,
    padding = 15,
    base_size = 1,
    aspect.ratio = NULL,
    draw_opts = list(),
    return_ht = FALSE,
    # cell customization
    layer_fun_callback = NULL,
    cell_type = "tile",
    cell_agg = NULL,
    ...
) {
    # Data was validated in `plotthis::Heatmap()`

    # Resolve annotation parameters, palettes, types, and aggregation functions with alias support.
    if (is.character(row_annotation_side)) {
        row_annotation_side <- list(.default = row_annotation_side)
    }
    if (is.character(column_annotation_side)) {
        column_annotation_side <- list(.default = column_annotation_side)
    }

    .check_annotation(
        data,
        row_annotation,
        rows_by,
        rows_split_by,
        which = "row"
    )
    .check_annotation(
        data,
        column_annotation,
        rows_by,
        rows_split_by,
        which = "column"
    )

    row_annotation_params <- .resolve_anno_aliases(
        row_annotation_params,
        rows_by,
        rows_split_by,
        columns_by,
        columns_split_by
    )
    row_annotation_palette <- .resolve_anno_aliases(
        row_annotation_palette,
        rows_by,
        rows_split_by,
        columns_by,
        columns_split_by
    )
    row_annotation_palcolor <- .resolve_anno_aliases(
        row_annotation_palcolor,
        rows_by,
        rows_split_by,
        columns_by,
        columns_split_by
    )
    row_annotation_type <- .resolve_anno_aliases(
        row_annotation_type,
        rows_by,
        rows_split_by,
        columns_by,
        columns_split_by
    )
    row_annotation_agg <- .resolve_anno_aliases(
        row_annotation_agg,
        rows_by,
        rows_split_by,
        columns_by,
        columns_split_by
    )

    column_annotation_params <- .resolve_anno_aliases(
        column_annotation_params,
        rows_by,
        rows_split_by,
        columns_by,
        columns_split_by
    )
    column_annotation_palette <- .resolve_anno_aliases(
        column_annotation_palette,
        rows_by,
        rows_split_by,
        columns_by,
        columns_split_by
    )
    column_annotation_palcolor <- .resolve_anno_aliases(
        column_annotation_palcolor,
        rows_by,
        rows_split_by,
        columns_by,
        columns_split_by
    )
    column_annotation_type <- .resolve_anno_aliases(
        column_annotation_type,
        rows_by,
        rows_split_by,
        columns_by,
        columns_split_by
    )
    column_annotation_agg <- .resolve_anno_aliases(
        column_annotation_agg,
        rows_by,
        rows_split_by,
        columns_by,
        columns_split_by
    )
    row_annotation_side <- .resolve_anno_aliases(
        row_annotation_side,
        rows_by,
        rows_split_by,
        columns_by,
        columns_split_by
    )
    column_annotation_side <- .resolve_anno_aliases(
        column_annotation_side,
        rows_by,
        rows_split_by,
        columns_by,
        columns_split_by
    )

    # Determine whether the name annotations are enabled.
    # A params entry of FALSE disables, but type "label" takes precedence
    # (setting the type implies the user wants the annotation visible).
    row_name_anno_enabled <- !is.null(rows_by) &&
        (!isFALSE(row_annotation_params[[rows_by]] %||% TRUE) ||
            (!identical(row_annotation_type, "auto") &&
                row_annotation_type[[rows_by]] %||% "simple" == "label"))
    col_name_anno_enabled <- !is.null(columns_by) &&
        (!isFALSE(column_annotation_params[[columns_by]] %||% TRUE) ||
            (!identical(column_annotation_type, "auto") &&
                column_annotation_type[[columns_by]] %||% "simple" == "label"))
    show_row_names <- show_row_names %||% !row_name_anno_enabled
    show_column_names <- show_column_names %||% !col_name_anno_enabled

    # Convert to the format that ComplexHeatmap::Heatmap can understand
    if (
        is.null(row_title) &&
            !is.null(rows_split_by) &&
            !is.null(row_annotation_type)
    ) {
        # Let's check if we need row_title
        if (identical(row_annotation_type, "auto")) {
            row_title <- TRUE
        } else {
            row_title <- identical(
                row_annotation_type[[rows_split_by]] %||% "simple",
                "simple"
            )
        }
    }
    if (
        is.null(column_title) &&
            !is.null(columns_split_by) &&
            !is.null(column_annotation_type)
    ) {
        if (identical(column_annotation_type, "auto")) {
            column_title <- TRUE
        } else {
            column_title <- identical(
                column_annotation_type[[columns_split_by]] %||% "simple",
                "simple"
            )
        }
    }
    if (isFALSE(column_title)) {
        column_title <- NULL
    }
    if (isFALSE(row_title)) {
        row_title <- NULL
    }
    if (isTRUE(column_title)) {
        column_title <- character(0)
    }
    if (isTRUE(row_title)) {
        row_title <- character(0)
    }
    stopifnot(
        "[Heatmap] 'padding' should be a numeric vector of length 1, 2, 3, or 4." = length(
            padding
        ) %in%
            1:4
    )

    get_col_fun <- function(lower, upper, a = alpha) {
        # If the lower and upper cutoff are the same, we need to adjust the upper cutoff
        if (upper == lower) {
            if (upper == 0) {
                upper <- 1e-3
            } else {
                upper <- upper + upper * 1e-3
            }
        }
        colorRamp2(
            seq(lower, upper, length = 100),
            palette_this(
                palette = palette,
                palcolor = palcolor,
                alpha = a,
                reverse = palreverse,
                transparent = FALSE
            )
        )
    }

    flip_side <- function(side) {
        match.arg(side, c("left", "right", "top", "bottom"))
        if (side == "left") {
            return("top")
        }
        if (side == "right") {
            return("bottom")
        }
        if (side == "top") {
            return("left")
        }
        if (side == "bottom") return("right")
    }

    # Initialize the heatmap arguments
    hmargs <- list(
        # name = name,   # error when name has irregular characters (e.g. "-")
        heatmap_legend_param = list(title = values_by),
        border = border,
        na_col = na_col,
        cluster_columns = if (flip) cluster_rows else cluster_columns,
        cluster_rows = if (flip) cluster_columns else cluster_rows,
        cluster_column_slices = FALSE,
        cluster_row_slices = FALSE,
        show_heatmap_legend = FALSE,
        show_row_names = if (flip) show_column_names else show_row_names,
        show_column_names = if (flip) show_row_names else show_column_names,
        row_names_side = if (flip) {
            flip_side(column_names_side)
        } else {
            row_names_side
        },
        column_names_side = if (flip) {
            flip_side(row_names_side)
        } else {
            column_names_side
        },
        column_title = column_title,
        row_title = row_title,
        ...
    )

    # Set the row_names_max_width based on the length of the row names
    if (!is.null(hmargs$row_names_max_width)) {
        if (is.character(hmargs$row_names_max_width)) {
            if (grepl("^[0-9]+(\\.[0-9]+)?$", hmargs$row_names_max_width)) {
                hmargs$row_names_max_width <- unit(
                    as.numeric(hmargs$row_names_max_width),
                    "inches"
                )
            }
            stopifnot(
                "[Heatmap] 'row_names_max_width' should be in a format of '2inches' or '2cm' if given as a string." = grepl(
                    "^[0-9]+(\\.[0-9]+\\s*)?(npc|cm|centimetre|centimeter|in|inch|inches|mm|points|picas|bigpts|cicero|scalepts|lines|char|native|snpc|strwidth|strheight|grobwidth|grobheight|null)$",
                    hmargs$row_names_max_width
                )
            )

            # convert to grid::unit
            hmargs$row_names_max_width <- unit(
                as.numeric(gsub("[^0-9.]", "", hmargs$row_names_max_width)),
                gsub("[0-9.]", "", hmargs$row_names_max_width)
            )
        } else if (is.numeric(hmargs$row_names_max_width)) {
            hmargs$row_names_max_width <- unit(
                hmargs$row_names_max_width,
                "inches"
            )
        }
    } else if (flip) {
        hmargs$row_names_max_width <- ComplexHeatmap::max_text_width(levels(data[[
            columns_by
        ]]))
    } else {
        hmargs$row_names_max_width <- ComplexHeatmap::max_text_width(levels(data[[
            rows_by
        ]]))
    }

    # Collect the legends
    legends <- list()

    # Set the default cell aggregation function for pie chart (will be plotted as the background)
    cell_agg <- cell_agg %||% ifelse(cell_type == "pie", "nansum", "nanmean")
    if (is.character(cell_agg)) {
        if (startsWith(cell_agg, "nan")) {
            fn <- match.fun(substring(cell_agg, 4))
            cell_agg <- function(x) fn(x[is.finite(x)])
        } else {
            cell_agg <- match.fun(cell_agg)
        }
    }

    # Extract the matrix for the heatmap (aggregated values, for e.g. tile, label, pie background, etc)
    # We also need it for bars, because ComplexHeatmap::Heatmap need the matrix to plot anyway
    # rows_split_by  rows_by  columns_split_by1::columns_by1 columns_split_by2::columns_by2 ...
    # rows_split_by1 rows_by1 0.1                            0.2                            ...
    # rows_split_by2 rows_by2 0.3                            0.4                            ...
    # ...
    hmargs$matrix <- data %>%
        group_by(
            !!!syms(unique(c(
                rows_split_by,
                rows_by,
                columns_split_by,
                columns_by
            )))
        ) %>%
        summarise(.value = cell_agg(!!sym(values_by)), .groups = "drop") %>%
        unite(
            ".columns",
            !!!syms(unique(c(columns_split_by, columns_by))),
            sep = " // "
        ) %>%
        unite(
            ".rows",
            !!!syms(unique(c(rows_split_by, rows_by))),
            sep = " // "
        ) %>%
        pivot_wider(
            names_from = ".columns",
            values_from = ".value",
            values_fill = values_fill
        ) %>%
        as.data.frame()

    rownames(hmargs$matrix) <- hmargs$matrix$.rows
    hmargs$matrix$.rows <- NULL
    hmargs$matrix <- as.matrix(hmargs$matrix)
    hmargs$matrix[is.na(hmargs$matrix)] <- values_fill

    columns_order <- data %>%
        tidyr::expand(!!!syms(unique(c(columns_split_by, columns_by)))) %>%
        unite(
            ".columns",
            !!!syms(unique(c(columns_split_by, columns_by))),
            sep = " // "
        ) %>%
        dplyr::pull(".columns") %>%
        unique() %>%
        intersect(colnames(hmargs$matrix))
    rows_order <- data %>%
        tidyr::expand(!!!syms(unique(c(rows_split_by, rows_by)))) %>%
        unite(
            ".rows",
            !!!syms(unique(c(rows_split_by, rows_by))),
            sep = " // "
        ) %>%
        dplyr::pull(".rows") %>%
        unique() %>%
        intersect(rownames(hmargs$matrix))

    hmargs$matrix <- hmargs$matrix[rows_order, columns_order, drop = FALSE]

    if (flip) {
        hmargs$matrix <- t(hmargs$matrix)
    }

    r_split_by <- if (flip) columns_split_by else rows_split_by
    c_split_by <- if (flip) rows_split_by else columns_split_by
    r_by <- if (flip) columns_by else rows_by
    c_by <- if (flip) rows_by else columns_by
    r_split_levels <- if (!is.null(r_split_by)) levels(data[[r_split_by]])
    c_split_levels <- if (!is.null(c_split_by)) levels(data[[c_split_by]])
    r_levels <- if (!is.null(r_by)) levels(data[[r_by]])
    c_levels <- if (!is.null(c_by)) levels(data[[c_by]])
    if (!is.null(r_split_by)) {
        row_split_labels <- strsplit(
            rownames(hmargs$matrix),
            " // ",
            fixed = TRUE
        )
        hmargs$row_split <- factor(
            sapply(row_split_labels, `[`, 1),
            levels = r_split_levels
        )
        hmargs$row_labels <- factor(
            sapply(row_split_labels, `[`, 2),
            levels = r_levels
        )
    } else {
        hmargs$row_labels <- factor(rownames(hmargs$matrix), levels = r_levels)
    }

    if (!is.null(c_split_by)) {
        column_split_labels <- strsplit(
            colnames(hmargs$matrix),
            " // ",
            fixed = TRUE
        )
        hmargs$column_split <- factor(
            sapply(column_split_labels, `[`, 1),
            levels = c_split_levels
        )
        hmargs$column_labels <- factor(
            sapply(column_split_labels, `[`, 2),
            levels = c_levels
        )
    } else {
        hmargs$column_labels <- factor(
            colnames(hmargs$matrix),
            levels = c_levels
        )
    }

    if (cell_type == "bars") {
        # where multiple values are used, operating on data
        lower_cutoff <- lower_cutoff %||%
            quantile(
                data[[values_by]][is.finite(data[[values_by]])],
                lower_quantile,
                na.rm = TRUE
            )
        upper_cutoff <- upper_cutoff %||%
            quantile(
                data[[values_by]][is.finite(data[[values_by]])],
                upper_quantile,
                na.rm = TRUE
            )
        data[[values_by]][data[[values_by]] < lower_cutoff] <- lower_cutoff
        data[[values_by]][data[[values_by]] > upper_cutoff] <- upper_cutoff
    } else {
        # where aggregated values are used
        lower_cutoff <- lower_cutoff %||%
            quantile(
                hmargs$matrix[is.finite(hmargs$matrix)],
                lower_quantile,
                na.rm = TRUE
            )
        upper_cutoff <- upper_cutoff %||%
            quantile(
                hmargs$matrix[is.finite(hmargs$matrix)],
                upper_quantile,
                na.rm = TRUE
            )
    }

    # Set the color function for the heatmap cells
    hmargs$col <- get_col_fun(lower_cutoff, upper_cutoff)

    # Indices for data in layer_fun
    indices <- if (flip) {
        # 1-1, 2-1, 1-2, 2-2, ...
        expand.grid(1:nrow(hmargs$matrix), 1:ncol(hmargs$matrix))
    } else {
        # 1-1, 1-2, 2-1, 2-2, ...
        expand_grid(1:nrow(hmargs$matrix), 1:ncol(hmargs$matrix))
    }
    indices <- paste(indices[[1]], indices[[2]], sep = "-")

    # Compose the main legend
    get_main_legend <- function(allow_discreate = TRUE) {
        if (identical(legend.position, "none")) {
            return(NULL)
        }
        if (!allow_discreate && isTRUE(legend_discrete)) {
            stop("[Heatmap] 'legend_discrete = TRUE' is not allowed.")
        }

        if (isTRUE(legend_discrete)) {
            if (is.null(legend_items)) {
                lgd_items <- sort(
                    unique(as.vector(hmargs$matrix)),
                    decreasing = TRUE
                )
                names(lgd_items) <- as.character(lgd_items)
            } else {
                lgd_items <- unlist(legend_items)
            }
            ComplexHeatmap::Legend(
                title = values_by,
                at = lgd_items,
                labels = names(lgd_items),
                legend_gp = gpar(fill = hmargs$col(lgd_items)),
                border = TRUE,
                direction = legend.direction
            )
        } else {
            ComplexHeatmap::Legend(
                title = values_by,
                col_fun = hmargs$col,
                border = TRUE,
                direction = legend.direction
            )
        }
    }

    if (cell_type == "pie") {
        if (is.null(pie_group_by)) {
            stop(
                "[Heatmap] Please provide 'pie_group_by' to use 'cell_type = 'pie'."
            )
        }
        pie_values <- pie_values %||% "length"
        if (is.character(pie_values)) {
            if (startsWith(pie_values, "nan")) {
                fn <- match.fun(substring(pie_values, 4))
                pie_values <- function(x) fn(x[is.finite(x)])
            } else {
                pie_values <- match.fun(pie_values)
            }
        }
        keep_empty_pie_group <- if (isFALSE(keep_empty)) {
            FALSE
        } else {
            keep_empty[[pie_group_by]]
        }
        pie_group_levels <- levels(data[[pie_group_by]])
        if (anyNA(data[[pie_group_by]])) {
            pie_group_levels <- c(pie_group_by, NA)
        }
        pie_data <- data %>%
            group_by(
                !!!syms(unique(c(
                    rows_split_by,
                    rows_by,
                    columns_split_by,
                    columns_by,
                    pie_group_by
                ))),
                .drop = FALSE
            ) %>%
            summarise(
                .value = pie_values(!!sym(values_by)),
                .groups = "drop"
            ) %>%
            group_by(
                !!!syms(unique(c(
                    rows_split_by,
                    rows_by,
                    columns_split_by,
                    columns_by
                )))
            ) %>%
            group_map(
                ~ data.frame(Var = .x[[pie_group_by]], Freq = .x$.value)
            )
        names(pie_data) <- indices

        pie_colors <- palette_this(
            pie_group_levels,
            palette = pie_palette,
            palcolor = pie_palcolor,
            NA_keep = TRUE
        )
        if (is.character((pie_size))) {
            if (startsWith(pie_size, "nan")) {
                fn <- match.fun(substring(pie_size, 4))
                pie_size <- function(x) fn(x[is.finite(x)])
            } else {
                pie_size <- match.fun(pie_size)
            }
        }
        hmargs$layer_fun <- function(j, i, x, y, w, h, fill, sr, sc) {
            layer_white_bg(j, i, x, y, w, h, fill)
            if (isTRUE(add_bg)) {
                layer_bg(j, x, x, y, w, h, fill, alpha = bg_alpha)
            }
            if (isTRUE(add_reticle)) {
                layer_reticle(j, i, x, y, w, h, fill, color = reticle_color)
            }
            layer_pie(
                j,
                i,
                x,
                y,
                w,
                h,
                fill,
                palette = pie_palette,
                palcolor = pie_palcolor,
                data = pie_data,
                pie_size = pie_size
            )
            if (is.function(layer_fun_callback)) {
                layer_fun_callback(j, i, x, y, w, h, fill, sr, sc)
            }
        }

        if (!identical(legend.position, "none") && is.function(pie_size)) {
            pie_sizes <- sapply(pie_data, function(d) {
                pie_size(sum(d$Freq, na.rm = TRUE))
            })
            pie_size_min <- min(pie_sizes, na.rm = TRUE)
            pie_size_max <- max(pie_sizes, na.rm = TRUE)
            legends$.pie_size <- ComplexHeatmap::Legend(
                title = pie_size_name,
                labels = scales::number(
                    (seq(
                        pie_size_min,
                        pie_size_max,
                        length.out = ifelse(pie_size_max > pie_size_min, 4, 1)
                    ))
                ),
                type = "points",
                pch = 21,
                size = unit(8, "mm") * seq(0.2, 1, length.out = 4),
                grid_height = unit(8, "mm") * seq(0.2, 1, length.out = 4) * 0.8,
                grid_width = unit(8, "mm"),
                legend_gp = gpar(fill = "grey30"),
                border = FALSE,
                background = "transparent",
                direction = legend.direction
            )
        }
        if (isTRUE(add_bg)) {
            legends$.heatmap <- get_main_legend()
        }
        if (!identical(legend.position, "none")) {
            legends$.pie <- ComplexHeatmap::Legend(
                title = pie_group_by,
                direction = legend.direction,
                border = TRUE,
                labels = pie_group_levels,
                legend_gp = gpar(fill = pie_colors)
            )
        }
    } else if (cell_type == "bars") {
        if (isTRUE(add_bg)) {
            stop("[Heatmap] Cannot use 'add_bg' with 'cell_type = 'bars'.")
        }
        if (isTRUE(add_reticle)) {
            stop("[Heatmap] Cannot use 'add_reticle' with 'cell_type = 'bars'.")
        }

        bars_data <- data %>%
            group_by(
                !!!syms(unique(c(
                    rows_split_by,
                    rows_by,
                    columns_split_by,
                    columns_by
                )))
            ) %>%
            group_map(~ .x[[values_by]])

        names(bars_data) <- indices

        # plot bars in each cell
        hmargs$layer_fun <- function(j, i, x, y, w, h, fill, sr, sc) {
            layer_white_bg(j, i, x, y, w, h, fill)
            layer_bars(
                j,
                i,
                x,
                y,
                w,
                h,
                fill,
                flip = flip,
                col_fun = hmargs$col,
                data = bars_data,
                alpha = alpha
            )
            if (is.function(layer_fun_callback)) {
                layer_fun_callback(j, i, x, y, w, h, fill, sr, sc)
            }
        }
        # Override the main legend
        legends$.heatmap <- get_main_legend(FALSE)
    } else if (cell_type == "dot") {
        if (is.character(dot_size)) {
            if (startsWith(dot_size, "nan")) {
                fn <- match.fun(substring(dot_size, 4))
                dot_size <- function(x) fn(x[is.finite(x)])
            } else {
                dot_size <- match.fun(dot_size)
            }
        }
        # Store raw values for each cell to pass to dot_size function later
        dot_data <- data %>%
            group_by(
                !!!syms(unique(c(
                    rows_split_by,
                    rows_by,
                    columns_split_by,
                    columns_by
                )))
            ) %>%
            summarise(.value = list(!!sym(values_by)), .groups = "drop") %>%
            unite(
                ".columns",
                !!!syms(unique(c(columns_split_by, columns_by))),
                sep = " // "
            ) %>%
            unite(
                ".rows",
                !!!syms(unique(c(rows_split_by, rows_by))),
                sep = " // "
            ) %>%
            pivot_wider(names_from = ".columns", values_from = ".value") %>%
            select(-!!sym(".rows")) %>%
            as.data.frame()

        if (flip) {
            dot_data <- t(dot_data)
        }

        if (
            !identical(legend.position, "none") &&
                is.function(dot_size) &&
                !is.null(dot_size_name)
        ) {
            # Optimized: only compute min/max for legend, not all sizes
            nargs <- length(formalArgs(dot_size))
            dot_size_min <- Inf
            dot_size_max <- -Inf

            for (idx in seq_along(indices)) {
                cell_key <- indices[idx]
                # Parse indices from the key "i-j"
                ij <- as.integer(strsplit(cell_key, "-")[[1]])
                cell_values <- dot_data[ij[1], ij[2]][[1]]

                size_val <- if (nargs == 1 || is.primitive(dot_size)) {
                    dot_size(cell_values)
                } else if (nargs == 3) {
                    dot_size(cell_values, ij[1], ij[2])
                } else if (nargs == 5) {
                    dot_size(
                        cell_values,
                        ij[1],
                        ij[2],
                        rownames(hmargs$matrix)[ij[1]],
                        colnames(hmargs$matrix)[ij[2]]
                    )
                } else {
                    stop(
                        "[Heatmap] 'dot_size' function should take 1, 3 or 5 arguments."
                    )
                }

                if (is.finite(size_val)) {
                    if (size_val < dot_size_min) {
                        dot_size_min <- size_val
                    }
                    if (size_val > dot_size_max) dot_size_max <- size_val
                }
            }

            legends$.dot_size <- ComplexHeatmap::Legend(
                title = dot_size_name,
                labels = scales::number(
                    (seq(
                        dot_size_min,
                        dot_size_max,
                        length.out = ifelse(dot_size_max > dot_size_min, 4, 1)
                    ))
                ),
                type = "points",
                pch = 21,
                size = unit(8, "mm") * seq(0.2, 1, length.out = 4),
                grid_height = unit(8, "mm") * seq(0.2, 1, length.out = 4) * 0.8,
                grid_width = unit(8, "mm"),
                legend_gp = gpar(fill = "grey30"),
                border = FALSE,
                background = "transparent",
                direction = legend.direction
            )
        }

        hmargs$layer_fun <- function(j, i, x, y, w, h, fill, sr, sc) {
            layer_white_bg(j, i, x, y, w, h, fill)
            if (isTRUE(add_bg)) {
                layer_bg(j, i, x, y, w, h, fill, alpha = bg_alpha)
            }
            if (isTRUE(add_reticle)) {
                layer_reticle(j, i, x, y, w, h, fill, color = reticle_color)
            }
            # Compute dot sizes based on function arguments
            if (is.function(dot_size)) {
                nargs <- length(formalArgs(dot_size))
                sizes <- numeric(length(i))
                for (idx in seq_along(i)) {
                    cell_values <- dot_data[[i[idx], j[idx]]]
                    if (nargs == 1 || is.primitive(dot_size)) {
                        sizes[idx] <- dot_size(cell_values)
                    } else if (nargs == 3) {
                        sizes[idx] <- dot_size(cell_values, i[idx], j[idx])
                    } else if (nargs == 5) {
                        sizes[idx] <- dot_size(
                            cell_values,
                            i[idx],
                            j[idx],
                            rownames(hmargs$matrix)[i[idx]],
                            colnames(hmargs$matrix)[j[idx]]
                        )
                    } else {
                        stop(
                            "[Heatmap] 'dot_size' function should take 1, 3 or 5 arguments."
                        )
                    }
                }
                layer_dot(
                    j,
                    i,
                    x,
                    y,
                    w,
                    h,
                    fill,
                    data = dot_data,
                    dot_size = sizes,
                    alpha = alpha
                )
            } else {
                layer_dot(
                    j,
                    i,
                    x,
                    y,
                    w,
                    h,
                    fill,
                    data = dot_data,
                    dot_size = dot_size,
                    alpha = alpha
                )
            }
            if (is.function(layer_fun_callback)) {
                layer_fun_callback(j, i, x, y, w, h, fill, sr, sc)
            }
        }
        # Override the main legend
        legends$.heatmap <- get_main_legend()
    } else if (cell_type %in% c("violin", "boxplot")) {
        # df with multiple values in each cell
        vdata <- data %>%
            group_by(
                !!!syms(unique(c(
                    rows_split_by,
                    rows_by,
                    columns_split_by,
                    columns_by
                )))
            ) %>%
            group_map(~ .x[[values_by]])

        names(vdata) <- indices
        vcolors <- if (cell_type == "violin") violin_fill else boxplot_fill

        hmargs$layer_fun <- function(j, i, x, y, w, h, fill, sr, sc) {
            layer_white_bg(j, i, x, y, w, h, fill)
            if (isTRUE(add_bg)) {
                layer_bg(j, i, x, y, w, h, fill, alpha = bg_alpha)
            }
            if (isTRUE(add_reticle)) {
                layer_reticle(j, i, x, y, w, h, fill, color = reticle_color)
            }
            layer_fn <- if (cell_type == "violin") ViolinPlot else BoxPlot
            layer_boxviolin(
                j,
                i,
                x,
                y,
                w,
                h,
                fill,
                flip = flip,
                data = vdata,
                colors = vcolors,
                fn = layer_fn
            )
            if (is.function(layer_fun_callback)) {
                layer_fun_callback(j, i, x, y, w, h, fill, sr, sc)
            }
        }
        if (!identical(legend.position, "none")) {
            if (!is.null(legend_items)) {
                stop(
                    "[Heatmap] Cannot use 'legend_items' with 'cell_type = 'violin' or 'boxplot'."
                )
            }
            if (isTRUE(legend_discrete)) {
                stop(
                    "[Heatmap] Cannot use 'legend_discrete = TRUE' with 'cell_type = 'violin' or 'boxplot'."
                )
            }
            if (is.null(vcolors)) {
                legends$.heatmap <- ComplexHeatmap::Legend(
                    title = values_by,
                    col_fun = hmargs$col,
                    border = TRUE,
                    direction = legend.direction
                )
            } else if (isTRUE(add_bg)) {
                legends$.heatmap <- ComplexHeatmap::Legend(
                    title = values_by,
                    col_fun = get_col_fun(lower_cutoff, upper_cutoff, bg_alpha),
                    border = TRUE,
                    direction = legend.direction
                )
            }
        }
    } else if (cell_type == "tile") {
        if (isTRUE(add_bg)) {
            stop("[Heatmap] Cannot use 'add_bg' with 'cell_type = 'tile'.")
        }
        if (isTRUE(add_reticle)) {
            stop("[Heatmap] Cannot use 'add_reticle' with 'cell_type = 'tile'.")
        }
        hmargs$rect_gp <- gpar(col = "grey80", lwd = 0.1)
        hmargs$layer_fun <- layer_fun_callback
        legends$.heatmap <- get_main_legend()
    } else if (cell_type == "label") {
        if (isTRUE(add_bg)) {
            stop("[Heatmap] Cannot use 'add_bg' with 'cell_type = 'tile'.")
        }
        if (isTRUE(add_reticle)) {
            stop("[Heatmap] Cannot use 'add_reticle' with 'cell_type = 'tile'.")
        }
        # Helper: call label() for a single cell value, dispatching by nargs
        .call_label <- if (is.function(label)) {
            nargs_label <- length(formalArgs(label))
            rnames <- rownames(hmargs$matrix)
            cnames <- colnames(hmargs$matrix)
            if (nargs_label == 1 || is.primitive(label)) {
                function(val, ri, ci) label(val)
            } else if (nargs_label == 3) {
                function(val, ri, ci) label(val, ri, ci)
            } else if (nargs_label == 5) {
                function(val, ri, ci) label(val, ri, ci, rnames[ri], cnames[ci])
            } else {
                stop(
                    "[Heatmap] 'label' function should take 1, 3 or 5 arguments."
                )
            }
        } else {
            function(val, ri, ci) val
        }

        # Helper: extract label text, supporting unnamed first list element
        .extract_lbl <- function(r) {
            if (!is.list(r)) {
                return(
                    if (length(r) == 1 && is.na(r)) {
                        NA_character_
                    } else {
                        as.character(r)
                    }
                )
            }
            if (!is.null(r$label)) {
                return(as.character(r$label))
            }
            nms <- names(r)
            unnamed_idx <- which(is.null(nms) | nms == "")
            if (length(unnamed_idx) > 0) {
                return(as.character(r[[unnamed_idx[1]]]))
            }
            NA_character_
        }

        # Pre-compute over whole matrix to auto-detect legend entries
        if (!identical(legend.position, "none")) {
            lgnd_seen <- list() # named list: legend_key -> list(text, color)
            for (ri in seq_len(nrow(hmargs$matrix))) {
                for (ci in seq_len(ncol(hmargs$matrix))) {
                    r <- .call_label(hmargs$matrix[ri, ci], ri, ci)
                    if (is.list(r) && !is.null(r$legend) && !is.na(r$legend)) {
                        key <- r$legend
                        if (is.null(lgnd_seen[[key]])) {
                            lbl_txt <- .extract_lbl(r)
                            lgnd_seen[[key]] <- list(
                                text = if (!is.na(lbl_txt)) lbl_txt else key,
                                color = r$color %||% label_color,
                                order = r$order %||% NA_integer_
                            )
                        }
                    }
                }
            }

            if (length(lgnd_seen) > 0) {
                # Sort by order field if any entries carry it
                orders <- sapply(lgnd_seen, function(e) {
                    e$order %||% NA_integer_
                })
                if (!all(is.na(orders))) {
                    # Entries without order are placed after explicitly ordered ones
                    orders[is.na(orders)] <- max(orders, na.rm = TRUE) +
                        seq_len(sum(is.na(orders)))
                    lgnd_seen <- lgnd_seen[order(orders)]
                }
                lgnd_graphics <- lapply(lgnd_seen, function(entry) {
                    txt <- entry$text
                    col <- entry$color
                    sz <- label_size
                    function(x, y, w, h, fill) {
                        grid.text(
                            txt,
                            x,
                            y,
                            gp = gpar(
                                fontsize = sz,
                                col = col,
                                fontface = "bold"
                            )
                        )
                    }
                })
                legends$.label <- ComplexHeatmap::Legend(
                    title = label_name,
                    labels = names(lgnd_seen),
                    graphics = lgnd_graphics,
                    direction = legend.direction
                )
            }
        }

        hmargs$layer_fun <- function(j, i, x, y, w, h, fill, sr, sc) {
            raw_vals <- ComplexHeatmap::pindex(hmargs$matrix, i, j)
            results <- lapply(seq_along(i), function(k) {
                .call_label(raw_vals[k], i[k], j[k])
            })

            lbl <- sapply(results, .extract_lbl)
            sizes <- sapply(results, function(r) {
                if (is.list(r)) r$size %||% label_size else label_size
            })
            colors <- sapply(results, function(r) {
                if (is.list(r)) r$color %||% label_color else label_color
            })

            inds <- which(!is.na(lbl))
            if (length(inds) > 0) {
                theta <- seq(pi / 8, 2 * pi, length.out = 16)
                for (k in inds) {
                    sz <- sizes[k]
                    lapply(theta, function(a) {
                        grid.text(
                            lbl[k],
                            x = x[k] + unit(cos(a) * sz / 30, "mm"),
                            y = y[k] + unit(sin(a) * sz / 30, "mm"),
                            gp = gpar(fontsize = sz, col = "white")
                        )
                    })
                    grid.text(
                        lbl[k],
                        x[k],
                        y[k],
                        gp = gpar(fontsize = sz, col = colors[k])
                    )
                }
            }
            if (is.function(layer_fun_callback)) {
                layer_fun_callback(j, i, x, y, w, h, fill, sr, sc)
            }
        }
        legends$.heatmap <- get_main_legend()
    } else if (cell_type == "mark") {
        if (isTRUE(add_bg)) {
            stop("[Heatmap] Cannot use 'add_bg' with 'cell_type = 'mark'.")
        }
        if (isTRUE(add_reticle)) {
            stop("[Heatmap] Cannot use 'add_reticle' with 'cell_type = 'mark'.")
        }

        # ── Mark dispatch helper ─────────────────────────────────────────────
        .call_mark <- if (is.function(mark)) {
            nargs_mark <- length(formalArgs(mark))
            rnames <- rownames(hmargs$matrix)
            cnames <- colnames(hmargs$matrix)
            if (nargs_mark == 1 || is.primitive(mark)) {
                function(val, ri, ci) mark(val)
            } else if (nargs_mark == 3) {
                function(val, ri, ci) mark(val, ri, ci)
            } else if (nargs_mark == 5) {
                function(val, ri, ci) mark(val, ri, ci, rnames[ri], cnames[ci])
            } else {
                stop(
                    "[Heatmap] 'mark' function should take 1, 3 or 5 arguments."
                )
            }
        } else {
            function(val, ri, ci) val
        }

        # ── Extract mark type string ────────────────────────────────────────
        .extract_mark <- function(r) {
            if (!is.list(r)) {
                return(
                    if (length(r) == 1 && is.na(r)) {
                        NA_character_
                    } else {
                        as.character(r)
                    }
                )
            }
            if (!is.null(r$mark)) {
                return(as.character(r$mark))
            }
            nms <- names(r)
            unnamed_idx <- which(is.null(nms) | nms == "")
            if (length(unnamed_idx) > 0) {
                return(as.character(r[[unnamed_idx[1]]]))
            }
            NA_character_
        }

        # ── Mark type parser ────────────────────────────────────────────────
        # Parses a mark string into a vector of drawing primitives.
        # Outer wrappers: [] adds "rect", () adds "circle_full", <> adds "diamond".
        # Wrappers are stripped left-to-right; the remainder is an inner primitive.
        # Inner primitives: -, |, +, /, \, x, o, (), <>
        .parse_mark_type <- function(m) {
            if (is.na(m) || m == "" || m == "NA") {
                return(character(0))
            }
            prims <- character(0)
            repeat {
                if (nchar(m) >= 2 && startsWith(m, "[") && endsWith(m, "]")) {
                    prims <- c(prims, "rect")
                    m <- substr(m, 2L, nchar(m) - 1L)
                } else if (
                    nchar(m) >= 3 && startsWith(m, "(") && endsWith(m, ")")
                ) {
                    # "(-)" wrapper, but "()" alone is a primitive — handled below
                    prims <- c(prims, "circle_full")
                    m <- substr(m, 2L, nchar(m) - 1L)
                } else if (
                    nchar(m) >= 3 && startsWith(m, "<") && endsWith(m, ">")
                ) {
                    # "<->" wrapper, but "<>" alone is a primitive — handled below
                    prims <- c(prims, "diamond")
                    m <- substr(m, 2L, nchar(m) - 1L)
                } else if (
                    nchar(m) >= 3 && startsWith(m, "{") && endsWith(m, "}")
                ) {
                    # "{-}" wrapper, but "{}" alone is a primitive — handled below
                    prims <- c(prims, "octagon")
                    m <- substr(m, 2L, nchar(m) - 1L)
                } else {
                    break
                }
            }
            inner <- if (m == "") {
                character(0)
            } else {
                switch(
                    m,
                    "-" = "hline",
                    "|" = "vline",
                    "+" = c("hline", "vline"),
                    "/" = "ldiag",
                    "\\" = "rdiag",
                    "x" = c("ldiag", "rdiag"),
                    "o" = "circle_gap",
                    "()" = "circle_full",
                    "<>" = "diamond",
                    "{}" = "octagon",
                    stop(paste0("[Heatmap] Unknown mark type: '", m, "'"))
                )
            }
            c(prims, inner)
        }

        # ── Vectorized primitive renderer ───────────────────────────────────
        # Draws one primitive for a batch of cells (same mark type, different positions).
        .draw_mark_prim <- function(prim, xv, yv, wv, hv, col, lwd) {
            switch(
                prim,
                rect = grid.rect(
                    x = xv,
                    y = yv,
                    width = wv,
                    height = hv,
                    gp = gpar(col = col, fill = NA, lwd = lwd)
                ),

                hline = grid.segments(
                    x0 = xv - wv * 0.5,
                    y0 = yv,
                    x1 = xv + wv * 0.5,
                    y1 = yv,
                    gp = gpar(col = col, lwd = lwd)
                ),

                vline = grid.segments(
                    x0 = xv,
                    y0 = yv - hv * 0.5,
                    x1 = xv,
                    y1 = yv + hv * 0.5,
                    gp = gpar(col = col, lwd = lwd)
                ),

                ldiag = grid.segments(
                    x0 = xv - wv * 0.5,
                    y0 = yv - hv * 0.5,
                    x1 = xv + wv * 0.5,
                    y1 = yv + hv * 0.5,
                    gp = gpar(col = col, lwd = lwd)
                ),

                rdiag = grid.segments(
                    x0 = xv - wv * 0.5,
                    y0 = yv + hv * 0.5,
                    x1 = xv + wv * 0.5,
                    y1 = yv - hv * 0.5,
                    gp = gpar(col = col, lwd = lwd)
                ),

                # circle: diameter = 3/4 of the shorter physical edge, true circle
                # Use mm so the radius is invariant to viewport aspect ratio
                circle_gap = {
                    .r_mm <- min(
                        convertUnit(wv[1L], "mm", valueOnly = TRUE),
                        convertUnit(hv[1L], "mm", valueOnly = TRUE)
                    ) *
                        0.25
                    grid.circle(
                        x = xv,
                        y = yv,
                        r = unit(.r_mm, "mm"),
                        gp = gpar(col = col, fill = NA, lwd = lwd)
                    )
                },

                # ellipse touching all 4 cell edges (no gap), handles non-square cells
                circle_full = {
                    .theta <- seq(0, 2 * pi, length.out = 65L)[seq_len(64L)]
                    .cos_t <- cos(.theta)
                    .sin_t <- sin(.theta)
                    for (k in seq_along(xv)) {
                        grid.polygon(
                            x = xv[k] + wv[k] * 0.5 * .cos_t,
                            y = yv[k] + hv[k] * 0.5 * .sin_t,
                            gp = gpar(
                                col = if (length(col) == 1L) col else col[k],
                                fill = NA,
                                lwd = if (length(lwd) == 1L) lwd else lwd[k]
                            )
                        )
                    }
                },

                # diamond — loop per cell (grid.polygon id approach avoids this)
                diamond = {
                    n <- length(xv)
                    hw <- wv * 0.5
                    hh <- hv * 0.5
                    for (k in seq_len(n)) {
                        grid.polygon(
                            x = unit.c(
                                xv[k],
                                xv[k] + hw[k],
                                xv[k],
                                xv[k] - hw[k]
                            ),
                            y = unit.c(
                                yv[k] + hh[k],
                                yv[k],
                                yv[k] - hh[k],
                                yv[k]
                            ),
                            gp = gpar(col = col[k], fill = NA, lwd = lwd[k])
                        )
                    }
                },

                # octagon: 4 sides on the cell edges, 4 small corner cuts (cut factor = 0.2)
                # The corner cuts are closer to the actual corners than a regular octagon would be
                octagon = {
                    n <- length(xv)
                    hw <- wv * 0.5
                    hh <- hv * 0.5
                    f <- 0.2 # corner cut factor (0 = full rectangle, 0.5 = diamond)
                    for (k in seq_len(n)) {
                        cx <- xv[k]
                        cy <- yv[k]
                        dx <- hw[k] * f
                        dy <- hh[k] * f
                        grid.polygon(
                            x = unit.c(
                                cx - hw[k] + dx,
                                cx + hw[k] - dx, # top edge: left → right
                                cx + hw[k],
                                cx + hw[k], # right edge: top → bottom
                                cx + hw[k] - dx,
                                cx - hw[k] + dx, # bottom edge: right → left
                                cx - hw[k],
                                cx - hw[k] # left edge: bottom → top
                            ),
                            y = unit.c(
                                cy + hh[k],
                                cy + hh[k], # top edge
                                cy + hh[k] - dy,
                                cy - hh[k] + dy, # right edge
                                cy - hh[k],
                                cy - hh[k], # bottom edge
                                cy - hh[k] + dy,
                                cy + hh[k] - dy # left edge
                            ),
                            gp = gpar(col = col[k], fill = NA, lwd = lwd[k])
                        )
                    }
                }
            )
        }

        # ── Legend icon builder ─────────────────────────────────────────────
        # Returns a function(x, y, w, h, fill) that draws the mark in a legend key.
        .mark_legend_icon <- function(mark_str, col, lwd) {
            prims <- .parse_mark_type(mark_str)
            function(x, y, w, h, fill) {
                for (p in prims) {
                    .draw_mark_prim(p, x, y, w, h, col, lwd)
                }
            }
        }

        # ── Pre-scan for legend entries ─────────────────────────────────────
        if (!identical(legend.position, "none")) {
            mkl_seen <- list()
            for (ri in seq_len(nrow(hmargs$matrix))) {
                for (ci in seq_len(ncol(hmargs$matrix))) {
                    r <- .call_mark(hmargs$matrix[ri, ci], ri, ci)
                    if (is.list(r) && !is.null(r$legend) && !is.na(r$legend)) {
                        key <- r$legend
                        if (is.null(mkl_seen[[key]])) {
                            mk_str <- .extract_mark(r)
                            mkl_seen[[key]] <- list(
                                mark = if (!is.na(mk_str)) mk_str else key,
                                color = r$color %||% mark_color,
                                size = r$size %||% mark_size,
                                order = r$order %||% NA_integer_
                            )
                        }
                    }
                }
            }

            if (length(mkl_seen) > 0) {
                # Sort by order
                orders <- sapply(mkl_seen, function(e) e$order %||% NA_integer_)
                if (!all(is.na(orders))) {
                    orders[is.na(orders)] <- max(orders, na.rm = TRUE) +
                        seq_len(sum(is.na(orders)))
                    mkl_seen <- mkl_seen[order(orders)]
                }
                mkl_graphics <- lapply(mkl_seen, function(entry) {
                    .mark_legend_icon(entry$mark, entry$color, entry$size)
                })
                legends$.mark <- ComplexHeatmap::Legend(
                    title = mark_name,
                    labels = names(mkl_seen),
                    graphics = mkl_graphics,
                    direction = legend.direction,
                    row_gap = unit(1, "mm")
                )
            }
        }

        # ── layer_fun ───────────────────────────────────────────────────────
        hmargs$layer_fun <- function(j, i, x, y, w, h, fill, sr, sc) {
            raw_vals <- ComplexHeatmap::pindex(hmargs$matrix, i, j)
            results <- lapply(seq_along(i), function(k) {
                .call_mark(raw_vals[k], i[k], j[k])
            })

            marks_vec <- sapply(results, .extract_mark)
            colors_vec <- sapply(results, function(r) {
                if (is.list(r)) r$color %||% mark_color else mark_color
            })
            sizes_vec <- sapply(results, function(r) {
                if (is.list(r)) r$size %||% mark_size else mark_size
            })

            # Draw each unique mark type as a batch
            unique_marks <- unique(marks_vec[!is.na(marks_vec)])
            for (mk in unique_marks) {
                km <- which(marks_vec == mk)
                prms <- .parse_mark_type(mk)
                for (prim in prms) {
                    .draw_mark_prim(
                        prim,
                        x[km],
                        y[km],
                        w[km],
                        h[km],
                        colors_vec[km],
                        sizes_vec[km]
                    )
                }
            }

            if (is.function(layer_fun_callback)) {
                layer_fun_callback(j, i, x, y, w, h, fill, sr, sc)
            }
        }
        legends$.heatmap <- get_main_legend()
    } else if (cell_type == "label+mark") {
        if (isTRUE(add_bg)) {
            stop(
                "[Heatmap] Cannot use 'add_bg' with 'cell_type = \"label+mark\"'."
            )
        }
        if (isTRUE(add_reticle)) {
            stop(
                "[Heatmap] Cannot use 'add_reticle' with 'cell_type = \"label+mark\"'."
            )
        }
        # ── Label dispatch helper ────────────────────────────────────────────
        .call_label <- if (is.function(label)) {
            nargs_label <- length(formalArgs(label))
            rnames <- rownames(hmargs$matrix)
            cnames <- colnames(hmargs$matrix)
            if (nargs_label == 1 || is.primitive(label)) {
                function(val, ri, ci) label(val)
            } else if (nargs_label == 3) {
                function(val, ri, ci) label(val, ri, ci)
            } else if (nargs_label == 5) {
                function(val, ri, ci) label(val, ri, ci, rnames[ri], cnames[ci])
            } else {
                stop(
                    "[Heatmap] 'label' function should take 1, 3 or 5 arguments."
                )
            }
        } else {
            function(val, ri, ci) val
        }

        # Helper: extract label text, supporting unnamed first list element
        .extract_lbl <- function(r) {
            if (!is.list(r)) {
                return(
                    if (length(r) == 1 && is.na(r)) {
                        NA_character_
                    } else {
                        as.character(r)
                    }
                )
            }
            if (!is.null(r$label)) {
                return(as.character(r$label))
            }
            nms <- names(r)
            unnamed_idx <- which(is.null(nms) | nms == "")
            if (length(unnamed_idx) > 0) {
                return(as.character(r[[unnamed_idx[1]]]))
            }
            NA_character_
        }

        # ── Mark dispatch helper ─────────────────────────────────────────────
        .call_mark <- if (is.function(mark)) {
            nargs_mark <- length(formalArgs(mark))
            rnames <- rownames(hmargs$matrix)
            cnames <- colnames(hmargs$matrix)
            if (nargs_mark == 1 || is.primitive(mark)) {
                function(val, ri, ci) mark(val)
            } else if (nargs_mark == 3) {
                function(val, ri, ci) mark(val, ri, ci)
            } else if (nargs_mark == 5) {
                function(val, ri, ci) mark(val, ri, ci, rnames[ri], cnames[ci])
            } else {
                stop(
                    "[Heatmap] 'mark' function should take 1, 3 or 5 arguments."
                )
            }
        } else {
            function(val, ri, ci) val
        }

        # ── Extract mark type string ────────────────────────────────────────
        .extract_mark <- function(r) {
            if (!is.list(r)) {
                return(
                    if (length(r) == 1 && is.na(r)) {
                        NA_character_
                    } else {
                        as.character(r)
                    }
                )
            }
            if (!is.null(r$mark)) {
                return(as.character(r$mark))
            }
            nms <- names(r)
            unnamed_idx <- which(is.null(nms) | nms == "")
            if (length(unnamed_idx) > 0) {
                return(as.character(r[[unnamed_idx[1]]]))
            }
            NA_character_
        }

        # ── Mark type parser ────────────────────────────────────────────────
        .parse_mark_type <- function(m) {
            if (is.na(m) || m == "" || m == "NA") {
                return(character(0))
            }
            prims <- character(0)
            repeat {
                if (nchar(m) >= 2 && startsWith(m, "[") && endsWith(m, "]")) {
                    prims <- c(prims, "rect")
                    m <- substr(m, 2L, nchar(m) - 1L)
                } else if (
                    nchar(m) >= 3 && startsWith(m, "(") && endsWith(m, ")")
                ) {
                    prims <- c(prims, "circle_full")
                    m <- substr(m, 2L, nchar(m) - 1L)
                } else if (
                    nchar(m) >= 3 && startsWith(m, "<") && endsWith(m, ">")
                ) {
                    prims <- c(prims, "diamond")
                    m <- substr(m, 2L, nchar(m) - 1L)
                } else if (
                    nchar(m) >= 3 && startsWith(m, "{") && endsWith(m, "}")
                ) {
                    # "{-}" wrapper, but "{}" alone is a primitive — handled below
                    prims <- c(prims, "octagon")
                    m <- substr(m, 2L, nchar(m) - 1L)
                } else {
                    break
                }
            }
            inner <- if (m == "") {
                character(0)
            } else {
                switch(
                    m,
                    "-" = "hline",
                    "|" = "vline",
                    "+" = c("hline", "vline"),
                    "/" = "ldiag",
                    "\\" = "rdiag",
                    "x" = c("ldiag", "rdiag"),
                    "o" = "circle_gap",
                    "()" = "circle_full",
                    "<>" = "diamond",
                    "{}" = "octagon",
                    stop(paste0("[Heatmap] Unknown mark type: '", m, "'"))
                )
            }
            c(prims, inner)
        }

        # ── Vectorized primitive renderer ───────────────────────────────────
        .draw_mark_prim <- function(prim, xv, yv, wv, hv, col, lwd) {
            switch(
                prim,
                rect = grid.rect(
                    x = xv,
                    y = yv,
                    width = wv,
                    height = hv,
                    gp = gpar(col = col, fill = NA, lwd = lwd)
                ),

                hline = grid.segments(
                    x0 = xv - wv * 0.5,
                    y0 = yv,
                    x1 = xv + wv * 0.5,
                    y1 = yv,
                    gp = gpar(col = col, lwd = lwd)
                ),

                vline = grid.segments(
                    x0 = xv,
                    y0 = yv - hv * 0.5,
                    x1 = xv,
                    y1 = yv + hv * 0.5,
                    gp = gpar(col = col, lwd = lwd)
                ),

                ldiag = grid.segments(
                    x0 = xv - wv * 0.5,
                    y0 = yv - hv * 0.5,
                    x1 = xv + wv * 0.5,
                    y1 = yv + hv * 0.5,
                    gp = gpar(col = col, lwd = lwd)
                ),

                rdiag = grid.segments(
                    x0 = xv - wv * 0.5,
                    y0 = yv + hv * 0.5,
                    x1 = xv + wv * 0.5,
                    y1 = yv - hv * 0.5,
                    gp = gpar(col = col, lwd = lwd)
                ),

                circle_gap = {
                    .r_mm <- min(
                        convertUnit(wv[1L], "mm", valueOnly = TRUE),
                        convertUnit(hv[1L], "mm", valueOnly = TRUE)
                    ) *
                        0.25
                    grid.circle(
                        x = xv,
                        y = yv,
                        r = unit(.r_mm, "mm"),
                        gp = gpar(col = col, fill = NA, lwd = lwd)
                    )
                },

                circle_full = {
                    .theta <- seq(0, 2 * pi, length.out = 65L)[seq_len(64L)]
                    .cos_t <- cos(.theta)
                    .sin_t <- sin(.theta)
                    for (k in seq_along(xv)) {
                        grid.polygon(
                            x = xv[k] + wv[k] * 0.5 * .cos_t,
                            y = yv[k] + hv[k] * 0.5 * .sin_t,
                            gp = gpar(
                                col = if (length(col) == 1L) col else col[k],
                                fill = NA,
                                lwd = if (length(lwd) == 1L) lwd else lwd[k]
                            )
                        )
                    }
                },

                diamond = {
                    n <- length(xv)
                    hw <- wv * 0.5
                    hh <- hv * 0.5
                    for (k in seq_len(n)) {
                        grid.polygon(
                            x = unit.c(
                                xv[k],
                                xv[k] + hw[k],
                                xv[k],
                                xv[k] - hw[k]
                            ),
                            y = unit.c(
                                yv[k] + hh[k],
                                yv[k],
                                yv[k] - hh[k],
                                yv[k]
                            ),
                            gp = gpar(col = col[k], fill = NA, lwd = lwd[k])
                        )
                    }
                },

                # octagon: 4 sides on the cell edges, 4 small corner cuts (cut factor = 0.2)
                # The corner cuts are closer to the actual corners than a regular octagon would be
                octagon = {
                    n <- length(xv)
                    hw <- wv * 0.5
                    hh <- hv * 0.5
                    f <- 0.5 # corner cut factor (0 = full rectangle, 0.5 = diamond)
                    for (k in seq_len(n)) {
                        cx <- xv[k]
                        cy <- yv[k]
                        dx <- hw[k] * f
                        dy <- hh[k] * f
                        grid.polygon(
                            x = unit.c(
                                cx - hw[k] + dx,
                                cx + hw[k] - dx, # top edge: left → right
                                cx + hw[k],
                                cx + hw[k], # right edge: top → bottom
                                cx + hw[k] - dx,
                                cx - hw[k] + dx, # bottom edge: right → left
                                cx - hw[k],
                                cx - hw[k] # left edge: bottom → top
                            ),
                            y = unit.c(
                                cy + hh[k],
                                cy + hh[k], # top edge
                                cy + hh[k] - dy,
                                cy - hh[k] + dy, # right edge
                                cy - hh[k],
                                cy - hh[k], # bottom edge
                                cy - hh[k] + dy,
                                cy + hh[k] - dy # left edge
                            ),
                            gp = gpar(col = col[k], fill = NA, lwd = lwd[k])
                        )
                    }
                }
            )
        }

        # ── Legend icon builder ─────────────────────────────────────────────
        .mark_legend_icon <- function(mark_str, col, lwd) {
            prims <- .parse_mark_type(mark_str)
            function(x, y, w, h, fill) {
                for (p in prims) {
                    .draw_mark_prim(p, x, y, w, h, col, lwd)
                }
            }
        }

        # ── Pre-scan for label legend entries ───────────────────────────────
        if (!identical(legend.position, "none")) {
            lgnd_seen <- list()
            for (ri in seq_len(nrow(hmargs$matrix))) {
                for (ci in seq_len(ncol(hmargs$matrix))) {
                    r <- .call_label(hmargs$matrix[ri, ci], ri, ci)
                    if (is.list(r) && !is.null(r$legend) && !is.na(r$legend)) {
                        key <- r$legend
                        if (is.null(lgnd_seen[[key]])) {
                            lbl_txt <- .extract_lbl(r)
                            lgnd_seen[[key]] <- list(
                                text = if (!is.na(lbl_txt)) lbl_txt else key,
                                color = r$color %||% label_color,
                                order = r$order %||% NA_integer_
                            )
                        }
                    }
                }
            }

            if (length(lgnd_seen) > 0) {
                orders <- sapply(lgnd_seen, function(e) {
                    e$order %||% NA_integer_
                })
                if (!all(is.na(orders))) {
                    orders[is.na(orders)] <- max(orders, na.rm = TRUE) +
                        seq_len(sum(is.na(orders)))
                    lgnd_seen <- lgnd_seen[order(orders)]
                }
                lgnd_graphics <- lapply(lgnd_seen, function(entry) {
                    txt <- entry$text
                    col <- entry$color
                    sz <- label_size
                    function(x, y, w, h, fill) {
                        grid.text(
                            txt,
                            x,
                            y,
                            gp = gpar(
                                fontsize = sz,
                                col = col,
                                fontface = "bold"
                            )
                        )
                    }
                })
                legends$.label <- ComplexHeatmap::Legend(
                    title = label_name,
                    labels = names(lgnd_seen),
                    graphics = lgnd_graphics,
                    direction = legend.direction
                )
            }

            # ── Pre-scan for mark legend entries ─────────────────────────────
            mkl_seen <- list()
            for (ri in seq_len(nrow(hmargs$matrix))) {
                for (ci in seq_len(ncol(hmargs$matrix))) {
                    r <- .call_mark(hmargs$matrix[ri, ci], ri, ci)
                    if (is.list(r) && !is.null(r$legend) && !is.na(r$legend)) {
                        key <- r$legend
                        if (is.null(mkl_seen[[key]])) {
                            mk_str <- .extract_mark(r)
                            mkl_seen[[key]] <- list(
                                mark = if (!is.na(mk_str)) mk_str else key,
                                color = r$color %||% mark_color,
                                size = r$size %||% mark_size,
                                order = r$order %||% NA_integer_
                            )
                        }
                    }
                }
            }

            if (length(mkl_seen) > 0) {
                orders <- sapply(mkl_seen, function(e) e$order %||% NA_integer_)
                if (!all(is.na(orders))) {
                    orders[is.na(orders)] <- max(orders, na.rm = TRUE) +
                        seq_len(sum(is.na(orders)))
                    mkl_seen <- mkl_seen[order(orders)]
                }
                mkl_graphics <- lapply(mkl_seen, function(entry) {
                    .mark_legend_icon(entry$mark, entry$color, entry$size)
                })
                legends$.mark <- ComplexHeatmap::Legend(
                    title = mark_name,
                    labels = names(mkl_seen),
                    graphics = mkl_graphics,
                    direction = legend.direction,
                    row_gap = unit(1, "mm")
                )
            }
        }

        # ── layer_fun: marks first (background), then labels (foreground) ───
        hmargs$layer_fun <- function(j, i, x, y, w, h, fill, sr, sc) {
            raw_vals <- ComplexHeatmap::pindex(hmargs$matrix, i, j)

            # Draw marks
            mk_results <- lapply(seq_along(i), function(k) {
                .call_mark(raw_vals[k], i[k], j[k])
            })
            marks_vec <- sapply(mk_results, .extract_mark)
            mk_colors <- sapply(mk_results, function(r) {
                if (is.list(r)) r$color %||% mark_color else mark_color
            })
            mk_sizes <- sapply(mk_results, function(r) {
                if (is.list(r)) r$size %||% mark_size else mark_size
            })

            unique_marks <- unique(marks_vec[!is.na(marks_vec)])
            for (mk in unique_marks) {
                km <- which(marks_vec == mk)
                prms <- .parse_mark_type(mk)
                for (prim in prms) {
                    .draw_mark_prim(
                        prim,
                        x[km],
                        y[km],
                        w[km],
                        h[km],
                        mk_colors[km],
                        mk_sizes[km]
                    )
                }
            }

            # Draw labels on top
            lbl_results <- lapply(seq_along(i), function(k) {
                .call_label(raw_vals[k], i[k], j[k])
            })
            lbl <- sapply(lbl_results, .extract_lbl)
            sizes <- sapply(lbl_results, function(r) {
                if (is.list(r)) r$size %||% label_size else label_size
            })
            colors <- sapply(lbl_results, function(r) {
                if (is.list(r)) r$color %||% label_color else label_color
            })

            inds <- which(!is.na(lbl))
            if (length(inds) > 0) {
                theta <- seq(pi / 8, 2 * pi, length.out = 16)
                for (k in inds) {
                    sz <- sizes[k]
                    lapply(theta, function(a) {
                        grid.text(
                            lbl[k],
                            x = x[k] + unit(cos(a) * sz / 30, "mm"),
                            y = y[k] + unit(sin(a) * sz / 30, "mm"),
                            gp = gpar(fontsize = sz, col = "white")
                        )
                    })
                    grid.text(
                        lbl[k],
                        x[k],
                        y[k],
                        gp = gpar(fontsize = sz, col = colors[k])
                    )
                }
            }

            if (is.function(layer_fun_callback)) {
                layer_fun_callback(j, i, x, y, w, h, fill, sr, sc)
            }
        }
        legends$.heatmap <- get_main_legend()
    }

    # Use actual matrix dimensions: split_by groups partition their axis items,
    # they don't multiply them. hmargs$matrix is already transposed when flip=TRUE.
    nrows <- nrow(hmargs$matrix)
    ncols <- ncol(hmargs$matrix)

    # ── Cell dimensions ──────────────────────────────────────────────────────
    # cell_w / cell_h: width and height of one cell in the *non-flipped* orientation
    # (inches).  cell_h = cell_w * effective_aspect_ratio.
    # Defaults are chosen so embedded sub-plots (violin, boxplot, pie) have enough
    # room, and bar cells are wider than they are tall.
    cell_w <- switch(
        cell_type,
        violin = 0.5,
        boxplot = 0.5,
        pie = 0.5,
        bars = 0.35,
        label = 0.6,
        mark = 0.25,
        `label+mark` = 0.6,
        0.25 # tile, dot
    )
    cell_w <- cell_w * base_size

    aspect.ratio <- aspect.ratio %||%
        switch(
            cell_type,
            violin = 2, # taller to accommodate violin shape
            boxplot = 2, # slightly taller to accommodate boxplot shape
            pie = 1, # square for pie charts
            bars = 0.5, # wider-than-tall for bars
            label = 0.6, # shorter to fit text better
            mark = 1, # square for marks
            `label+mark` = 0.6, # same as label
            1 # square by default for all other types
        )
    cell_h <- cell_w * aspect.ratio

    ncol_annos <- sum(cluster_columns, show_column_names) * 4
    ncol_annos <- ncol_annos +
        ifelse(
            is.null(columns_split_by) ||
                isFALSE(column_annotation_params[[columns_split_by]]),
            0,
            1
        ) +
        ifelse(!col_name_anno_enabled, 0, 1)
    res <- .setup_annos(
        which = "column",
        names_side = ifelse(flip, column_names_side, row_names_side),
        anno_title = column_title,
        show_names = show_column_names,
        annotation = column_annotation,
        annotation_type = column_annotation_type,
        annotation_side = column_annotation_side,
        annotation_palette = column_annotation_palette,
        annotation_palcolor = column_annotation_palcolor,
        annotation_agg = column_annotation_agg,
        annotation_params = column_annotation_params,
        split_by = columns_split_by,
        splits = if (flip) hmargs$row_split else hmargs$column_split,
        by = columns_by,
        by_labels = if (flip) hmargs$row_labels else hmargs$column_labels,
        flip = flip,
        legend.direction = legend.direction,
        legend.position = legend.position,
        data = data
    )
    column_annos <- res$annos
    legends <- c(legends, res$legends)
    # Dispatch annotations to sides based on per-annotation column_annotation_side
    all_keys <- names(column_annos)
    all_keys <- setdiff(
        all_keys,
        c("annotation_name_side", "show_annotation_name")
    )
    builtin_keys <- unique(c(columns_split_by, columns_by))
    side_list <- column_annotation_side # already alias-resolved
    def_side <- if (is.list(side_list)) {
        side_list[[".default"]] %||% "top"
    } else {
        "top"
    }
    sna <- column_annos$show_annotation_name
    names_side <- column_annos$annotation_name_side
    top_annos <- list(
        annotation_name_side = names_side,
        show_annotation_name = list()
    )
    bottom_annos <- list(
        annotation_name_side = names_side,
        show_annotation_name = list()
    )
    for (k in all_keys) {
        if (is.list(side_list)) {
            side <- side_list[[k]] %||% def_side
        } else {
            side <- if (is.character(side_list)) side_list else def_side
        }
        if (side == "bottom") {
            bottom_annos[[k]] <- column_annos[[k]]
            if (!is.null(sna[[k]])) {
                bottom_annos$show_annotation_name[[k]] <- sna[[k]]
            }
        } else {
            top_annos[[k]] <- column_annos[[k]]
            if (!is.null(sna[[k]])) {
                top_annos$show_annotation_name[[k]] <- sna[[k]]
            }
        }
    }
    # Reorder: split annotation farthest from heatmap body, name annotation closest
    top_annos <- .reorder_anno_side(
        top_annos,
        columns_by,
        columns_split_by,
        side = "top"
    )
    bottom_annos <- .reorder_anno_side(
        bottom_annos,
        columns_by,
        columns_split_by,
        side = "bottom"
    )
    if (any(sapply(top_annos, inherits, "AnnotationFunction"))) {
        if (isTRUE(flip)) {
            hmargs$left_annotation <- do_call(
                ComplexHeatmap::rowAnnotation,
                top_annos
            )
        } else {
            hmargs$top_annotation <- do_call(
                ComplexHeatmap::HeatmapAnnotation,
                top_annos
            )
        }
    }
    if (any(sapply(bottom_annos, inherits, "AnnotationFunction"))) {
        if (isTRUE(flip)) {
            hmargs$right_annotation <- do_call(
                ComplexHeatmap::rowAnnotation,
                bottom_annos
            )
        } else {
            hmargs$bottom_annotation <- do_call(
                ComplexHeatmap::HeatmapAnnotation,
                bottom_annos
            )
        }
    }
    rm(top_annos, bottom_annos, column_annos)

    nrow_annos <- sum(cluster_rows, show_row_names) * 4
    nrow_annos <- nrow_annos +
        ifelse(
            is.null(rows_split_by) ||
                isFALSE(row_annotation_params[[rows_split_by]]),
            0,
            1
        ) +
        ifelse(!row_name_anno_enabled, 0, 1)
    res <- .setup_annos(
        which = "row",
        names_side = ifelse(flip, row_names_side, column_names_side),
        anno_title = row_title,
        show_names = show_row_names,
        annotation = row_annotation,
        annotation_type = row_annotation_type,
        annotation_side = row_annotation_side,
        annotation_palette = row_annotation_palette,
        annotation_palcolor = row_annotation_palcolor,
        annotation_agg = row_annotation_agg,
        annotation_params = row_annotation_params,
        split_by = rows_split_by,
        splits = if (flip) hmargs$column_split else hmargs$row_split,
        by = rows_by,
        by_labels = if (flip) hmargs$column_labels else hmargs$row_labels,
        flip = flip,
        legend.direction = legend.direction,
        legend.position = legend.position,
        data = data
    )
    row_annos <- res$annos
    legends <- c(legends, res$legends)
    # Dispatch annotations to sides based on per-annotation row_annotation_side
    all_keys <- names(row_annos)
    all_keys <- setdiff(
        all_keys,
        c("annotation_name_side", "show_annotation_name")
    )
    side_list <- row_annotation_side # already alias-resolved
    def_side <- if (is.list(side_list)) {
        side_list[[".default"]] %||% "left"
    } else {
        "left"
    }
    sna <- row_annos$show_annotation_name
    names_side <- row_annos$annotation_name_side
    left_side <- list(
        annotation_name_side = names_side,
        show_annotation_name = list()
    )
    right_side <- list(
        annotation_name_side = names_side,
        show_annotation_name = list()
    )
    for (k in all_keys) {
        side <- if (is.list(side_list)) {
            side_list[[k]] %||% def_side
        } else if (is.character(side_list)) {
            side_list
        } else {
            def_side
        }
        if (side == "right") {
            right_side[[k]] <- row_annos[[k]]
            if (!is.null(sna[[k]])) {
                right_side$show_annotation_name[[k]] <- sna[[k]]
            }
        } else {
            left_side[[k]] <- row_annos[[k]]
            if (!is.null(sna[[k]])) {
                left_side$show_annotation_name[[k]] <- sna[[k]]
            }
        }
    }
    # Reorder: split annotation farthest from heatmap body, name annotation closest
    left_side <- .reorder_anno_side(
        left_side,
        rows_by,
        rows_split_by,
        side = "left"
    )
    right_side <- .reorder_anno_side(
        right_side,
        rows_by,
        rows_split_by,
        side = "bottom"
    )
    has_left <- any(sapply(left_side, inherits, "AnnotationFunction"))
    has_right <- any(sapply(right_side, inherits, "AnnotationFunction"))
    # Fix: when row annotations are only on the right and row_names_side is "right",
    # ComplexHeatmap width() fails. Swap row_names_side to "left" to avoid the conflict.
    if (
        !has_left &&
            has_right &&
            !isTRUE(flip) &&
            hmargs$row_names_side == "right"
    ) {
        hmargs$row_names_side <- "left"
    }
    if (has_left) {
        if (isTRUE(flip)) {
            hmargs$top_annotation <- do_call(
                ComplexHeatmap::HeatmapAnnotation,
                left_side
            )
        } else {
            hmargs$left_annotation <- do_call(
                ComplexHeatmap::rowAnnotation,
                left_side
            )
        }
    }
    if (has_right) {
        if (isTRUE(flip)) {
            hmargs$bottom_annotation <- do_call(
                ComplexHeatmap::HeatmapAnnotation,
                right_side
            )
        } else {
            hmargs$right_annotation <- do_call(
                ComplexHeatmap::rowAnnotation,
                right_side
            )
        }
    }
    rm(row_annos, left_side, right_side, has_left, has_right)

    ## Fix for ComplexHeatmap annotation name / legend overlap bug:
    ## When show_row_names = FALSE but annotations have names on the right side
    ## (annotation_name_side = "right"), ComplexHeatmap does not allocate space for those
    ## annotation name labels, causing them to overlap the right-side legend.
    ## - flip = FALSE: column annotations (top_annotation) have names on row_names_side = "right"
    ## - flip = TRUE:  row annotations (top_annotation, from left_annos) have names on
    ##                 row_names_side = "right"; hmargs$row_names_side = flip_side(column_names_side)
    ## In both cases the offending names appear on the right, so inject a phantom invisible
    ## right annotation to reserve the required width.
    phantom_right_width_in <- 0
    if (
        !isTRUE(hmargs$show_row_names) &&
            hmargs$row_names_side == "right" &&
            legend.position == "right"
    ) {
        right_anno_names <- character(0)
        if (!isTRUE(flip)) {
            if (col_name_anno_enabled) {
                right_anno_names <- c(right_anno_names, columns_by)
            }
            if (
                !is.null(columns_split_by) &&
                    !isFALSE(column_annotation_params[[columns_split_by]])
            ) {
                right_anno_names <- c(right_anno_names, columns_split_by)
            }
            if (!is.null(column_annotation) && length(column_annotation) > 0) {
                col_anno_names <- if (is.list(column_annotation)) {
                    names(column_annotation)
                } else {
                    as.character(column_annotation)
                }
                right_anno_names <- c(right_anno_names, col_anno_names)
            }
        } else {
            if (row_name_anno_enabled) {
                right_anno_names <- c(right_anno_names, rows_by)
            }
            if (
                !is.null(rows_split_by) &&
                    !isFALSE(row_annotation_params[[rows_split_by]])
            ) {
                right_anno_names <- c(right_anno_names, rows_split_by)
            }
            if (!is.null(row_annotation) && length(row_annotation) > 0) {
                row_anno_names <- if (is.list(row_annotation)) {
                    names(row_annotation)
                } else {
                    as.character(row_annotation)
                }
                right_anno_names <- c(right_anno_names, row_anno_names)
            }
        }
        if (length(right_anno_names) > 0) {
            phantom_width <- ComplexHeatmap::max_text_width(right_anno_names)
            phantom_right_width_in <- convertUnit(
                phantom_width,
                "inches",
                valueOnly = TRUE
            )
            phantom_anno <- ComplexHeatmap::rowAnnotation(
                .gap = ComplexHeatmap::anno_empty(
                    border = FALSE,
                    width = phantom_width
                ),
                show_annotation_name = FALSE
            )
            if (is.null(hmargs$right_annotation)) {
                hmargs$right_annotation <- phantom_anno
            } else {
                hmargs$right_annotation <- hmargs$right_annotation +
                    phantom_anno
            }
        }
    }

    ## Set up the heatmap dimensions
    # Row names appear on left/right and add to width; only count when show_row_names is TRUE.
    # hmargs$show_row_names already accounts for flip (equals original show_column_names when
    # flip=TRUE, since original column labels become the row labels in the transposed matrix).
    # hmargs$row_names_max_width likewise accounts for flip (uses columns_by labels when flip=TRUE).
    rownames_width <- if (isTRUE(hmargs$show_row_names)) {
        max(
            convertUnit(
                hmargs$row_names_max_width,
                "inches",
                valueOnly = TRUE
            ) *
                0.5 -
                0.2,
            0
        )
    } else {
        0
    }

    # Estimate the dimension contribution of the original column labels.
    # Column labels are rendered rotated (vertical text), so their display height is proportional
    # to their text width. When flip=TRUE the matrix is transposed, so original column labels
    # end up as hmargs row labels (left/right) — but colnames_height lives inside col_overhead
    # which is routed to WIDTH in the flip case, so the geometry is still correct.
    # Always gate on the original show_column_names param (not hmargs$show_column_names, which
    # is swapped to show_row_names when flip=TRUE) to match the col_overhead accounting.
    # Use rownames(hmargs$matrix) when flip=TRUE because t() moved the original column labels
    # to row positions; use colnames(hmargs$matrix) otherwise.
    colnames_height <- if (isTRUE(show_column_names)) {
        orig_col_labels <- if (isTRUE(flip)) {
            rownames(hmargs$matrix)
        } else {
            colnames(hmargs$matrix)
        }
        col_max_width <- ComplexHeatmap::max_text_width(orig_col_labels)
        convertUnit(col_max_width, "inches", valueOnly = TRUE) * 0.25
    } else {
        0
    }

    # Annotation overhead per side:
    #   - row-side  (left/right): dendrogram + per-track bars → adds to width
    #   - col-side  (top/bottom): dendrogram + per-track bars → adds to height
    # nrow_annos / ncol_annos include: (cluster_* + show_*_names)*4 + n_splits + n_name_annos
    # We strip out the show_*_names contribution (already captured by rownames_width /
    # colnames_height above) and reduce the per-item coefficient to avoid double-counting.
    row_overhead <- (if (isTRUE(cluster_rows)) 0.5 else 0) +
        (nrow_annos - show_row_names * 4) * 0.15 +
        phantom_right_width_in
    col_overhead <- (if (isTRUE(cluster_columns)) 0.5 else 0) +
        (ncol_annos - show_column_names * 4) * 0.15 +
        colnames_height

    if (isTRUE(flip)) {
        body_width <- ncols * cell_h
        body_height <- nrows * cell_w
    } else {
        body_width <- ncols * cell_w
        body_height <- nrows * cell_h
    }

    padding <- if (inherits(padding, "unit")) padding else unit(padding, "mm")
    if (length(padding) == 1) {
        padding <- rep(padding, 4)
    } else if (length(padding) == 2) {
        # padding[1] -> top/bottom, padding[2] -> left/right
        padding <- rep(padding, 2)
    } else if (length(padding) == 3) {
        # padding[1] -> top, padding[2] -> left/right, padding[3] -> bottom
        padding <- c(padding, padding[2])
    }
    if (isTRUE(flip)) {
        width <- body_width +
            col_overhead +
            rownames_width +
            convertUnit(sum(padding[c(1, 3)]), "inches", valueOnly = TRUE)
        height <- body_height +
            row_overhead +
            convertUnit(sum(padding[c(2, 4)]), "inches", valueOnly = TRUE)
    } else {
        width <- body_width +
            row_overhead +
            rownames_width +
            convertUnit(sum(padding[c(2, 4)]), "inches", valueOnly = TRUE)
        height <- body_height +
            col_overhead +
            convertUnit(sum(padding[c(1, 3)]), "inches", valueOnly = TRUE)
    }
    # make padding from top, right, bottom, left to match the order in ComplexHeatmap::draw()
    # which is bottom, left, top and right.
    padding <- padding[c(3, 4, 1, 2)]

    # ── Precise legend size contribution (mirrors calculate_plot_dimensions) ──
    # Collect all candidate legend label strings to estimate max label width.
    # Sources:
    #   (a) main heatmap legend  – discrete item names, or formatted cutoff values
    #   (b) name annotations – row/column labels shown as legends when show_*_names=FALSE,
    #       and split annotations (rows_split_by / columns_split_by) which always show legends
    #   (c) extra row / column annotation legends
    .legend_label_cands <- character(0)
    if (isTRUE(legend_discrete) && !is.null(legend_items)) {
        .legend_label_cands <- c(.legend_label_cands, names(legend_items))
    } else {
        .legend_label_cands <- c(
            .legend_label_cands,
            formatC(c(lower_cutoff, upper_cutoff), digits = 3, format = "g")
        )
    }
    # Helper: extract label strings from a column of `data`
    .col_labels <- function(col) {
        if (is.null(col) || !col %in% colnames(data)) {
            return(character(0))
        }
        as.character(
            if (is.factor(data[[col]])) {
                levels(data[[col]])
            } else {
                unique(data[[col]])
            }
        )
    }
    # Name-annotation legends:
    #   – rows_by shows a legend when show_row_names=FALSE (and row name annotation is enabled)
    #   – columns_by shows a legend when show_column_names=FALSE (and col name annotation is enabled)
    #   – split_by annotations always show legends when present
    if (!isTRUE(show_row_names) && row_name_anno_enabled) {
        .legend_label_cands <- c(.legend_label_cands, .col_labels(rows_by))
    }
    if (!isTRUE(show_column_names) && col_name_anno_enabled) {
        .legend_label_cands <- c(.legend_label_cands, .col_labels(columns_by))
    }
    .legend_label_cands <- c(
        .legend_label_cands,
        .col_labels(rows_split_by),
        .col_labels(columns_split_by)
    )
    # Extra row / column annotation legends
    .scan_anno_labels <- function(anno) {
        if (is.null(anno)) {
            return(character(0))
        }
        cols <- if (is.list(anno)) unlist(anno) else as.character(anno)
        unlist(lapply(cols[cols %in% colnames(data)], .col_labels))
    }
    .legend_label_cands <- c(
        .legend_label_cands,
        .scan_anno_labels(row_annotation),
        .scan_anno_labels(column_annotation)
    )
    legend_nchar <- if (length(.legend_label_cands) > 0) {
        max(nchar(.legend_label_cands), na.rm = TRUE)
    } else {
        5L
    }
    # Number of distinct legend blocks rendered by ComplexHeatmap
    legend_n <- length(Filter(Negate(is.null), legends))

    # Use the same per-character metrics as calculate_plot_dimensions()
    legend_key_w <- 0.30 # key swatch width + internal margin
    legend_char_w <- 0.07 # inches per character of label text
    legend_row_h <- 0.30 # height of one stacked legend block
    legend_pad <- 0.35 # outer margin + optional title

    if (!identical(legend.position, "none")) {
        if (legend.position %in% c("right", "left")) {
            # Vertical panel on the side; width driven by label length.
            # When direction=="horizontal" the colorbar is rendered horizontally
            # (wider), so add an extra 0.5 in to account for the larger key.
            legend_extra <- if (legend.direction == "horizontal") 0.5 else 0
            legend_width <- max(
                1.0,
                legend_key_w + legend_nchar * legend_char_w
            ) +
                legend_extra
            width <- width + legend_width
        } else if (legend.direction == "horizontal") {
            # Each legend block goes on its own row at top / bottom.
            n_legend_rows <- max(1L, legend_n)
            legend_height <- max(1.0, legend_pad + n_legend_rows * legend_row_h)
            height <- height + legend_height
        } else {
            # Vertical blocks at top / bottom: add a legend-width column.
            legend_width <- max(
                1.0,
                legend_key_w + legend_nchar * legend_char_w
            )
            width <- width + legend_width
        }
    }
    # Fix body dimensions so ComplexHeatmap honours aspect.ratio regardless of canvas size.
    # Only set when the caller has not already provided explicit width/height via `...`.
    # Do NOT set the width and height so that the plot won't be truncated due to the device size limit; instead, we will set the width and height attributes on the returned object so that the downstream display method can choose how to handle it (e.g. scaling to fit the device).
    # if (is.null(hmargs$width)) {
    #     hmargs$width  <- unit(body_width,  "inches")
    # }
    # if (is.null(hmargs$height)) {
    #     hmargs$height <- unit(body_height, "inches")
    # }
    unknown_args <- setdiff(
        names(hmargs),
        methods::formalArgs(ComplexHeatmap::Heatmap)
    )
    if (length(unknown_args) > 0) {
        warning(
            "[Heatmap] Unknown arguments to ComplexHeatmap::Heatmap(): ",
            paste(unknown_args, collapse = ", ")
        )
        hmargs <- hmargs[setdiff(names(hmargs), unknown_args)]
    }
    p <- do_call(ComplexHeatmap::Heatmap, hmargs)
    # Move label/mark legends to the end so they appear last
    if (!is.null(legends$.label)) {
        legends <- c(legends[names(legends) != ".label"], legends[".label"])
    }
    if (!is.null(legends$.mark)) {
        legends <- c(legends[names(legends) != ".mark"], legends[".mark"])
    }
    if (return_ht) {
        p <- ComplexHeatmap::prepare(p)
        attr(p, "legends") <- legends
        return(p)
    }

    mat <- p@matrix
    draw_args_fixed <- list(
        annotation_legend_list = legends,
        padding = draw_opts$padding %||% padding,
        column_title = title,
        align_heatmap_legend = draw_opts$align_heatmap_legend %||%
            "heatmap_center",
        align_annotation_legend = draw_opts$align_annotation_legend %||%
            "heatmap_center"
    )
    if (identical(legend.position, "none")) {
        draw_args_fixed$show_annotation_legend <- draw_opts$show_annotation_legend %||%
            FALSE
        draw_args_fixed$show_heatmap_legend <- draw_opts$show_heatmap_legend %||%
            FALSE
    } else {
        draw_args_fixed$annotation_legend_side <- draw_opts$annotation_legend_side %||%
            legend.position
        draw_args_fixed$heatmap_legend_side <- draw_opts$heatmap_legend_side %||%
            legend.position
    }
    draw_args <- utils::modifyList(draw_opts, draw_args_fixed)

    p <- grid::grid.grabExpr(do_call(
        ComplexHeatmap::draw,
        c(list(p), draw_args)
    ))
    p <- patchwork::wrap_plots(p)

    min_size_in <- 4
    max_size_in <- 64
    attr(p, "height") <- max(min(height, max_size_in), min_size_in)
    attr(p, "width") <- max(min(width, max_size_in), min_size_in)

    # keep the ratio
    ratio <- height / width
    if (ratio > 1) {
        if (attr(p, "height") == max_size_in) {
            attr(p, "width") <- attr(p, "height") / ratio
        } else if (attr(p, "width") == min_size_in) {
            attr(p, "height") <- attr(p, "width") * ratio
        }
    } else if (ratio < 1) {
        if (attr(p, "width") == max_size_in) {
            attr(p, "height") <- attr(p, "width") * ratio
        } else if (attr(p, "height") == min_size_in) {
            attr(p, "width") <- attr(p, "height") / ratio
        }
    }
    p$data <- as.data.frame(mat)
    p
}

#' Heatmap
#'
#' @description
#' Draw a heatmap to visualise data in matrix form.  This is the public,
#' exported interface — it accepts data in multiple input formats (matrix,
#' wide, or long), preprocesses it via \code{\link{process_heatmap_data}},
#' and delegates to \code{\link{HeatmapAtomic}} for rendering.  Commonly
#' used in biology to visualise gene expression, but applicable to any
#' matrix-structured data.
#'
#' @section Input formats:
#' The \code{in_form} parameter controls how the input \code{data} is
#' interpreted:
#' \itemize{
#'   \item \code{"auto"} (default) — detects the format automatically.
#'   \item \code{"matrix"} — \code{data} is a matrix with row and column
#'         names.  It is melted to long form internally.
#'   \item \code{"wide-rows"} — each row is a feature, columns are samples.
#'   \item \code{"wide-columns"} — each column is a feature, rows are
#'         samples.
#'   \item \code{"long"} — tidy/long format with one observation per row.
#' }
#'
#' @section Split-by support:
#' When \code{split_by} is provided, the data is partitioned into subsets
#' and an independent heatmap is produced for each level.  Results are
#' combined via \code{\link[patchwork]{wrap_plots}} according to
#' \code{nrow}, \code{ncol}, \code{byrow}, and \code{design}.  Per-split
#' \code{palette}, \code{palcolor}, \code{legend.position}, and
#' \code{legend.direction} can be specified as named lists keyed by split
#' level.
#'
#' @inheritParams HeatmapAtomic
#' @inheritParams process_heatmap_data
#' @inheritParams common_args
#' @param data A data frame or matrix.  When a matrix, it is melted to
#'  long format internally (requires row and column names).
#' @param ... Additional arguments passed to
#'  \code{\link{HeatmapAtomic}}, which in turn forwards them to
#'  \code{\link[ComplexHeatmap]{Heatmap}}.
#' @return A \code{patchwork} object (class \code{wrap_plots}) with
#'  \code{height} and \code{width} attributes (in inches).  When
#'  \code{combine = FALSE}, a named list of such objects, one per
#'  \code{split_by} level.
#' @export
#' @importFrom patchwork wrap_plots
#' @seealso \code{\link{HeatmapAtomic}}, \code{\link{LinkedHeatmap}},
#'  \code{\link{anno_simple}}, \code{\link{anno_points}},
#'  \code{\link{anno_lines}}, \code{\link{anno_pie}},
#'  \code{\link{anno_violin}}, \code{\link{anno_boxplot}},
#'  \code{\link{anno_density}}
#' @examples
#' \donttest{
#' set.seed(8525)
#'
#' matrix_data <- matrix(rnorm(60), nrow = 6, ncol = 10)
#' rownames(matrix_data) <- paste0("R", 1:6)
#' colnames(matrix_data) <- paste0("C", 1:10)
#' if (requireNamespace("cluster", quietly = TRUE)) {
#'     Heatmap(matrix_data)
#' }
#' if (requireNamespace("cluster", quietly = TRUE)) {
#'     # use a different color palette
#'     # change the main legend title
#'     # show row names (legend will be hidden)
#'     # show column names
#'     # change the row name annotation name and side
#'     # change the column name annotation name
#'     Heatmap(matrix_data, palette = "viridis", values_by = "z-score",
#'        show_row_names = TRUE, show_column_names = TRUE,
#'        rows_name = "Features", row_names_side = "left",
#'        columns_name = "Samples")
#' }
#' if (requireNamespace("cluster", quietly = TRUE)) {
#'     # flip the heatmap
#'     Heatmap(matrix_data, palette = "viridis", values_by = "z-score",
#'        show_row_names = TRUE, show_column_names = TRUE,
#'        rows_name = "Features", row_names_side = "left",
#'        columns_name = "Samples", flip = TRUE)
#' }
#' if (requireNamespace("cluster", quietly = TRUE)) {
#'     # add annotations to the heatmap
#'     rows_data <- data.frame(
#'        rows = paste0("R", 1:6),
#'        group = sample(c("X", "Y", "Z"), 6, replace = TRUE)
#'     )
#'     Heatmap(matrix_data, rows_data = rows_data,
#'         row_annotation = list(Group = "group"),
#'         row_annotation_type = list(Group = "simple"),
#'         row_annotation_palette = list(Group = "Spectral")
#'     )
#' }
#' if (requireNamespace("cluster", quietly = TRUE)) {
#'     Heatmap(matrix_data, rows_data = rows_data,
#'         rows_split_by = "group"
#'     )
#' }
#' if (requireNamespace("cluster", quietly = TRUE)) {
#'     # use label annotation for split groups (shows group labels inside colored blocks)
#'     Heatmap(matrix_data, rows_data = rows_data,
#'         rows_split_by = "group",
#'         row_annotation_params = list(.rows.split = list(
#'             border = FALSE,
#'             labels_gp = grid::gpar(col = "white", fontsize = 12),
#'             labels_rot = 0
#'         )),
#'         row_annotation_type = list(.rows.split = "label")
#'     )
#' }
#' if (requireNamespace("cluster", quietly = TRUE)) {
#'     # label annotation for column splits
#'     columns_data <- data.frame(
#'         columns = paste0("C", 1:10),
#'         batch = rep(c("A", "B"), each = 5)
#'     )
#'     Heatmap(matrix_data, columns_data = columns_data,
#'         columns_split_by = "batch",
#'         column_annotation_type = list(.col.split = "label")
#'     )
#' }
#' rownames(matrix_data)[1] <- "R12345"
#' if (requireNamespace("cluster", quietly = TRUE)) {
#'     # label annotation for name annotations: show row/column names as colored labels
#'     Heatmap(matrix_data, rows_data = rows_data,
#'         row_annotation_type = list(.row = "label"),
#'         column_annotation_type = list(.col = "label"),
#'         column_annotation_params = list(.col = list(labels_rot = 90)),
#'         row_annotation_palette = list(.row = "Set2"),
#'         row_annotation_side = list(.row = "right"),
#'         row_annotation_params = list(.row = list(labels_rot = 150))
#'     )
#' }
#' if (requireNamespace("cluster", quietly = TRUE)) {
#'     # add labels to the heatmap
#'     Heatmap(matrix_data, rows_data = rows_data,
#'         rows_split_by = "group", cell_type = "label",
#'         base_size = 0.8,
#'         label = function(x) ifelse(
#'             x > 0, scales::number(x, accuracy = 0.01), NA
#'         )
#'     )
#' }
#' if (requireNamespace("cluster", quietly = TRUE)) {
#'     # add labels based on an external data
#'     pvalues <- matrix(runif(60, 0, 0.5), nrow = 6, ncol = 10)
#'     Heatmap(matrix_data, rows_data = rows_data,
#'         rows_split_by = "group", cell_type = "label",
#'         base_size = 0.8,
#'         label = function(x, i, j) {
#'             pv <- ComplexHeatmap::pindex(pvalues, i, j)
#'             ifelse(pv < 0.01, "***",
#'             ifelse(pv < 0.05, "**",
#'             ifelse(pv < 0.1, "*", NA)))
#'         }
#'     )
#' }
#' if (requireNamespace("cluster", quietly = TRUE)) {
#'     # Set label color, size, legend and order
#'     pvalues <- matrix(runif(60, 0, 0.5), nrow = 6, ncol = 10)
#'     Heatmap(matrix_data, rows_data = rows_data,
#'         rows_split_by = "group", cell_type = "label",
#'         base_size = 0.6,
#'         label_name = "Significance",
#'         label = function(x, i, j) {
#'             pv <- ComplexHeatmap::pindex(pvalues, i, j)
#'             if (pv < 0.01)
#'                list("***", color = "red", size = 12, legend = "p < 0.01", order = 1)
#'             else if (pv < 0.05)
#'                list("**", color = "orange", size = 10, legend = "p < 0.05", order = 3)
#'             else if (pv < 0.1)
#'                list("*", color = "yellow", size = 8, legend = "p < 0.1", order = 2)
#'             else NA
#'         }
#'     )
#' }
#' if (requireNamespace("cluster", quietly = TRUE)) {
#'     # add marks
#'     Heatmap(matrix_data, rows_data = rows_data,
#'         rows_split_by = "group", cell_type = "mark",
#'         mark = function(x, i, j) {
#'             pv <- ComplexHeatmap::pindex(pvalues, i, j)
#'             if(pv < 0.01) list("[x]", legend = "p < 0.01")
#'             else if (pv < 0.02) list("[o]", legend = "p < 0.02")
#'             else if (pv < 0.03) list("[-]", legend = "p < 0.03")
#'             else if (pv < 0.05) list("[()]", legend = "p < 0.05")
#'             else if (pv < 0.06) list("+", legend = "p < 0.06")
#'             else if (pv < 0.07) list("x", legend = "p < 0.07")
#'             else if (pv < 0.08) list("[/]", legend = "p < 0.08")
#'             else if (pv < 0.09) list("[\\]", legend = "p < 0.09")
#'             else NA
#'         }
#'     )
#' }
#' if (requireNamespace("cluster", quietly = TRUE)) {
#'     # add labels and marks
#'     Heatmap(matrix_data, rows_data = rows_data,
#'         rows_split_by = "group", cell_type = "mark+label",
#'         label = scales::label_number(accuracy = 0.01),
#'         mark = function(x, i, j) {
#'             pv <- ComplexHeatmap::pindex(pvalues, i, j)
#'             if(pv < 0.01) list("{}", legend = "p < 0.01")
#'             else if(pv < 0.05) list("[]", legend = "p < 0.05")
#'             else NA
#'         },
#'         mark_size = 1.5, mark_color = "red"
#'     )
#' }
#' if (requireNamespace("cluster", quietly = TRUE)) {
#'     # quickly simulate a GO board
#'     go <- matrix(sample(c(0, 1, NA), 81, replace = TRUE), ncol = 9)
#'
#'     Heatmap(
#'         go,
#'         # Do not cluster rows and columns and hide the name annotations
#'         # Use .row/.col aliases (or the actual rows_name/columns_name) in annotation_params
#'         cluster_rows = FALSE, cluster_columns = FALSE,
#'         row_annotation_params = list(.row = FALSE),
#'         column_annotation_params = list(.col = FALSE),
#'         show_row_names = FALSE, show_column_names = FALSE,
#'         # Set the legend items
#'         values_by = "Players", legend_discrete = TRUE,
#'         legend_items = c("Player 1" = 0, "Player 2" = 1),
#'         # Set the pawns
#'         cell_type = "dot", dot_size = function(x) ifelse(is.na(x), 0, 10),
#'         dot_size_name = NULL,  # hide the dot size legend
#'         palcolor = c("white", "black"),
#'         # Set the board
#'         add_reticle = TRUE,
#'         # Set the size of the board
#'         width = ggplot2::unit(105, "mm"), height = ggplot2::unit(105, "mm"))
#' }
#' if (requireNamespace("cluster", quietly = TRUE)) {
#'     # Make the row/column name annotation thicker using the .row/.col aliases
#'     Heatmap(matrix_data,
#'         column_annotation_params = list(.col = list(height = 5)),
#'         row_annotation_params = list(.row = list(width = 5)))
#' }
#' if (requireNamespace("cluster", quietly = TRUE)) {
#'     # Per-annotation side control: row name annotation on the right,
#'     # all other row annotations on the left (.default)
#'     rows_data2 <- data.frame(
#'         rows = paste0("R", 1:6),
#'         group = sample(c("X", "Y"), 6, replace = TRUE),
#'         score = runif(6)
#'     )
#'     Heatmap(matrix_data, rows_data = rows_data2,
#'         rows_split_by = "group",
#'         row_annotation = list(Score = "score"),
#'         row_annotation_side = list(.default = "left", .row = "right"),
#'         show_row_names = TRUE
#'     )
#' }
#' if (requireNamespace("cluster", quietly = TRUE)) {
#'     # Move all row annotations to the right side
#'     Heatmap(matrix_data, rows_data = rows_data2,
#'         rows_split_by = "group",
#'         row_annotation = list(Score = "score"),
#'         row_annotation_side = "right",
#'         show_row_names = TRUE
#'     )
#' }
#' if (requireNamespace("cluster", quietly = TRUE)) {
#'     # Split and name annotations on opposite sides:
#'     # split annotation on the default left, name annotation on the right
#'     Heatmap(matrix_data, rows_data = rows_data2,
#'         rows_split_by = "group",
#'         row_annotation_side = list(.default = "left", .row = "right"),
#'         show_row_names = TRUE
#'     )
#' }
#' if (requireNamespace("cluster", quietly = TRUE)) {
#'     # Row name label annotation on the right side (text rotated 90° clockwise)
#'     Heatmap(matrix_data, rows_data = rows_data2,
#'         row_annotation_type = list(.row = "label"),
#'         row_annotation_palette = list(.row = "Set2"),
#'         row_annotation_side = list(.row = "right"),
#'         show_row_names = TRUE
#'     )
#' }
#'
#' # Use long form data
#' N <- 500
#' data <- data.frame(
#'     value = rnorm(N),
#'     c = sample(letters[1:8], N, replace = TRUE),
#'     r = sample(LETTERS[1:5], N, replace = TRUE),
#'     p = sample(c("x", "y"), N, replace = TRUE),
#'     q = sample(c("X", "Y", "Z"), N, replace = TRUE),
#'     a = as.character(sample(1:5, N, replace = TRUE)),
#'     p1 = runif(N),
#'     p2 = runif(N)
#' )
#'
#' if (requireNamespace("cluster", quietly = TRUE)) {
#'     Heatmap(data, rows_by = "r", columns_by = "c", values_by = "value",
#'         rows_split_by = "p", columns_split_by = "q", show_column_names = TRUE)
#' }
#' if (requireNamespace("cluster", quietly = TRUE)) {
#'     # split into multiple heatmaps
#'     Heatmap(data,
#'         values_by = "value", columns_by = "c", rows_by = "r", split_by = "p",
#'         upper_cutoff = 2, lower_cutoff = -2, legend.position = c("none", "right"),
#'         design = "AAAAAA#BBBBBBB"
#'     )
#' }
#' if (requireNamespace("cluster", quietly = TRUE)) {
#'     # cell_type = "bars" (default is "tile")
#'     Heatmap(data, values_by = "value", rows_by = "r", columns_by = "c",
#'         cell_type = "bars")
#' }
#' if (requireNamespace("cluster", quietly = TRUE)) {
#'     p <- Heatmap(data, values_by = "value", rows_by = "r", columns_by = "c",
#'         cell_type = "dot", dot_size = length, dot_size_name = "data points",
#'         add_bg = TRUE, add_reticle = TRUE)
#'     p
#' }
#' if (requireNamespace("cluster", quietly = TRUE)) {
#'     dot_size_data <- as.matrix(p$data)
#'     # Make it big so we can see if we get the right indexing
#'     # for dot_size function
#'     dot_size_data["A", "a"] <- max(dot_size_data) * 2
#'
#'     Heatmap(data, values_by = "value", rows_by = "r", columns_by = "c",
#'         cell_type = "dot", dot_size_name = "data points",
#'         dot_size = function(x, i, j) ComplexHeatmap::pindex(dot_size_data, i, j),
#'         show_row_names = TRUE, show_column_names = TRUE,
#'         add_bg = TRUE, add_reticle = TRUE)
#' }
#' if (requireNamespace("cluster", quietly = TRUE)) {
#'     Heatmap(data, values_by = "value", rows_by = "r", columns_by = "c",
#'         cell_type = "pie", pie_group_by = "q", pie_size = sqrt,
#'         add_bg = TRUE, add_reticle = TRUE)
#' }
#' if (requireNamespace("cluster", quietly = TRUE)) {
#'     Heatmap(data, values_by = "value", rows_by = "r", columns_by = "c",
#'         cell_type = "violin", add_bg = TRUE, add_reticle = TRUE)
#' }
#' if (requireNamespace("cluster", quietly = TRUE)) {
#'     Heatmap(data, values_by = "value", rows_by = "r", columns_by = "c",
#'         cell_type = "boxplot", add_bg = TRUE, add_reticle = TRUE)
#' }
#' if (requireNamespace("cluster", quietly = TRUE)) {
#'     Heatmap(data,
#'         values_by = "value", rows_by = "r", columns_by = "c",
#'         column_annotation = list(r1 = "p", r2 = "q", r3 = "p1"),
#'         column_annotation_type = list(r1 = "ring", r2 = "bar", r3 = "violin"),
#'         column_annotation_params = list(
#'             r1 = list(height = grid::unit(10, "mm"), show_legend = FALSE),
#'             r3 = list(height = grid::unit(18, "mm"))
#'         ),
#'         row_annotation = c("q", "p2", "a"),
#'         row_annotation_side = "right",
#'         row_annotation_type = list(q = "pie", p2 = "density", a = "simple"),
#'         row_annotation_params = list(q = list(width = grid::unit(12, "mm"))),
#'         show_row_names = TRUE, show_column_names = TRUE
#'     )
#' }
#' if (requireNamespace("cluster", quietly = TRUE)) {
#'     Heatmap(data,
#'         values_by = "value", rows_by = "r", columns_by = "c",
#'         split_by = "p", palette = list(x = "Reds", y = "Blues")
#'     )
#' }
#' if (requireNamespace("cluster", quietly = TRUE)) {
#'     # implies in_form = "wide-rows"
#'     Heatmap(data, rows_by = c("p1", "p2"), columns_by = "c")
#' }
#' if (requireNamespace("cluster", quietly = TRUE)) {
#'     # implies wide-columns
#'     Heatmap(data, rows_by = "r", columns_by = c("p1", "p2"))
#' }
#' }
Heatmap <- function(
    data,
    values_by = NULL,
    values_fill = NA,
    name = NULL,
    # data definition
    in_form = c("auto", "matrix", "wide-columns", "wide-rows", "long"),
    split_by = NULL,
    split_by_sep = "_",
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
    validate_common_args(seed)
    in_form <- match.arg(in_form)
    cell_type <- match.arg(cell_type)
    cell_type <- sub("mark+label", "label+mark", cell_type, fixed = TRUE)

    if (!is.null(rows_orderby)) {
        cluster_rows <- cluster_rows %||% FALSE
        stopifnot(
            "[Heatmap] `rows_orderby` can't be used with `cluster_rows = TRUE`" = isFALSE(
                cluster_rows
            )
        )
    } else {
        cluster_rows <- cluster_rows %||% TRUE
    }

    if (!is.null(columns_orderby)) {
        cluster_columns <- cluster_columns %||% FALSE
        stopifnot(
            "[Heatmap] `columns_orderby` can't be used with `cluster_columns = TRUE`" = isFALSE(
                cluster_columns
            )
        )
    } else {
        cluster_columns <- cluster_columns %||% TRUE
    }

    hmdata <- process_heatmap_data(
        data,
        in_form = in_form,
        values_by = values_by,
        name = name,
        split_by = split_by,
        split_by_sep = split_by_sep,
        rows_orderby = rows_orderby,
        columns_orderby = columns_orderby,
        rows_by = rows_by,
        rows_by_sep = rows_by_sep,
        rows_name = rows_name,
        rows_split_by = rows_split_by,
        rows_split_by_sep = rows_split_by_sep,
        rows_split_name = rows_split_name,
        columns_by = columns_by,
        columns_by_sep = columns_by_sep,
        columns_name = columns_name,
        columns_split_by = columns_split_by,
        columns_split_by_sep = columns_split_by_sep,
        columns_split_name = columns_split_name,
        pie_group_by = pie_group_by,
        pie_group_by_sep = pie_group_by_sep,
        pie_name = pie_name,
        rows_data = rows_data,
        columns_data = columns_data,
        keep_na = keep_na,
        keep_empty = keep_empty
    )
    split_by <- split_by %||% "..."

    palette <- check_palette(palette, names(hmdata$data))
    palcolor <- check_palcolor(palcolor, names(hmdata$data))
    pie_palette <- check_palette(pie_palette, names(hmdata$data))
    pie_palcolor <- check_palcolor(pie_palcolor, names(hmdata$data))
    legend.direction <- check_legend(
        legend.direction,
        names(hmdata$data),
        "legend.direction"
    )
    legend.position <- check_legend(
        legend.position,
        names(hmdata$data),
        "legend.position"
    )

    plots <- lapply(
        names(hmdata$data),
        function(nm) {
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

            HeatmapAtomic(
                data = hmdata$data[[nm]],
                values_by = hmdata$values_by,
                values_fill = values_fill,
                rows_by = hmdata$rows_by,
                rows_split_by = hmdata$rows_split_by,
                columns_by = hmdata$columns_by,
                columns_split_by = hmdata$columns_split_by,

                palette = palette[[nm]],
                palcolor = palcolor[[nm]],
                palreverse = palreverse,

                pie_size_name = pie_size_name,
                pie_size = pie_size,
                pie_values = pie_values,
                pie_group_by = hmdata$pie_group_by,
                pie_palette = pie_palette[[nm]],
                pie_palcolor = pie_palcolor[[nm]],

                bars_sample = bars_sample,
                label = label,
                label_size = label_size,
                label_color = label_color,
                label_name = label_name,
                mark = mark,
                mark_color = mark_color,
                mark_size = mark_size,
                mark_name = mark_name,
                violin_fill = violin_fill,
                boxplot_fill = boxplot_fill,
                dot_size = dot_size,
                dot_size_name = dot_size_name,

                legend_items = legend_items,
                legend_discrete = legend_discrete,
                legend.position = legend.position[[nm]],
                legend.direction = legend.direction[[nm]],

                lower_quantile = lower_quantile,
                upper_quantile = upper_quantile,
                lower_cutoff = lower_cutoff,
                upper_cutoff = upper_cutoff,

                add_bg = add_bg,
                bg_alpha = bg_alpha,
                keep_na = hmdata$keep_na,
                keep_empty = hmdata$keep_empty,

                add_reticle = add_reticle,
                reticle_color = reticle_color,

                cluster_columns = cluster_columns,
                cluster_rows = cluster_rows,
                show_row_names = show_row_names,
                show_column_names = show_column_names,
                border = border,
                title = title,
                column_title = column_title,
                row_title = row_title,
                na_col = na_col,
                row_names_side = row_names_side,
                column_names_side = column_names_side,
                column_annotation = column_annotation,
                column_annotation_side = column_annotation_side,
                column_annotation_palette = column_annotation_palette,
                column_annotation_palcolor = column_annotation_palcolor,
                column_annotation_type = column_annotation_type,
                column_annotation_params = column_annotation_params,
                column_annotation_agg = column_annotation_agg,
                row_annotation = row_annotation,
                row_annotation_side = row_annotation_side,
                row_annotation_palette = row_annotation_palette,
                row_annotation_palcolor = row_annotation_palcolor,
                row_annotation_type = row_annotation_type,
                row_annotation_params = row_annotation_params,
                row_annotation_agg = row_annotation_agg,

                flip = flip,
                alpha = alpha,
                seed = seed,
                base_size = base_size,
                aspect.ratio = aspect.ratio,
                draw_opts = draw_opts,
                layer_fun_callback = layer_fun_callback,
                cell_type = cell_type,
                cell_agg = cell_agg,

                ...
            )
        }
    )
    names(plots) <- names(hmdata$data)

    p <- combine_plots(
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

    # Return the plot object
    # When return_grob = FALSE, p is a HeatmapList object with auto-printing behavior
    # The initial draw() in HeatmapAtomic was captured to a null device to prevent
    # printing during assignment, so this returned object can print normally when
    # called directly (e.g., in a Jupyter cell)
    p
}
