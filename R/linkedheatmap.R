#' Atomic linked heatmap (internal)
#'
#' @description
#' Draws two heatmaps side-by-side with spline link lines connecting matching
#' rows across the left and right heatmaps.  This is the core implementation
#' layer — it takes a **single** data frame and a full set of left/right
#' parameters, delegates to \code{\link{HeatmapAtomic}} (with
#' \code{return_ht = TRUE}) to obtain prepared \code{ComplexHeatmap::Heatmap}
#' objects, extracts exact dimension information, and then assembles
#' everything into a composite \code{grid} layout.
#'
#' @section Architecture:
#' \enumerate{
#'   \item \strong{Parameter resolution} — extra arguments passed via
#'         \code{...} are split into left-specific and right-specific groups
#'         by prefix (\code{left_} vs \code{right_}).  Each group is merged
#'         with the shared defaults using \code{\%||\%} fallback chains,
#'         giving the caller full per-side override capability.
#'   \item \strong{Cell dimension pre-computation} — the internal helper
#'         \code{.get_dim_pre()} calculates exact cell width and height
#'         (in \code{"inches"} units) from \code{cell_type},
#'         \code{aspect.ratio}, \code{base_size}, and the unique row/column
#'         counts (including split groups).  These are passed as explicit
#'         \code{width} / \code{height} arguments to
#'         \code{ComplexHeatmap::Heatmap()} so that cells have a guaranteed
#'         physical size rather than using \code{null} units.
#'   \item \strong{Delegation to \code{HeatmapAtomic}} — both the left and
#'         right heatmaps are created by calling
#'         \code{do_call(HeatmapAtomic, ...)} with \code{return_ht = TRUE}.
#'         This returns the prepared \code{Heatmap} object (with
#'         \code{cell_w}/\code{cell_h} attributes) without drawing it.
#'   \item \strong{Dimension extraction} — exact component heights and widths
#'         are read from the prepared \code{Heatmap} objects via
#'         \code{ComplexHeatmap:::component_height()} and
#'         \code{ComplexHeatmap:::component_width()}.  These 9-element vectors
#'         break down every part of the heatmap (title, dendrogram, names,
#'         annotations, body) so the composite layout can be sized precisely.
#'   \item \strong{Link table construction} — rows in the left and right
#'         heatmaps are matched by grouping the input data on the row
#'         identifier columns (plus optional \code{rows_split_by}).  Each
#'         link's position is mapped to the ordered row index in its
#'         respective heatmap (respecting clustering order).
#'   \item \strong{Legend collection} — legends from both heatmaps are
#'         collected, packed via \code{ComplexHeatmap::packLegend()}, and
#'         placed according to \code{legend.position} (left, right, top, or
#'         bottom).
#'   \item \strong{Composite drawing} — a \code{grid.layout} is built
#'         dynamically based on the legend position.  Each heatmap is drawn
#'         inside a centered, exact-size viewport so
#'         \code{ComplexHeatmap::draw()} sizes the body to the pre-computed
#'         row/column counts.  Link lines are drawn as \code{grid.xspline()}
#'         curves in a dedicated gap column between the two heatmaps.
#'   \item \strong{Dimension attributes} — the final \code{patchwork}-wrapped
#'         grob carries \code{height} and \code{width} attributes (in inches)
#'         for consistent rendering in downstream layouts.
#' }
#'
#' @param data A data frame in long format. Each row represents one
#'  observation; columns specify row/column membership for both left and
#'  right heatmaps as well as the values to encode as color.
#' @param left_values_by,right_values_by Column name whose values determine
#'  the fill color of cells in the left / right heatmap.  These are the
#'  primary data columns visualized by the colour scale.
#' @param left_rows_by,right_rows_by Column name that defines the rows of
#'  the left / right heatmap.  Each unique value becomes one row.
#' @param left_columns_by,right_columns_by Column name that defines the
#'  columns of the left / right heatmap.  Each unique value becomes one
#'  column.
#' @param left_columns_split_by,right_columns_split_by Optional column name
#'  to split the columns of the left / right heatmap into groups (passed to
#'  \code{ComplexHeatmap::Heatmap} as \code{column_split}).
#' @param left_pie_group_by,right_pie_group_by When \code{cell_type = "pie"},
#'  column(s) to group values by within each pie cell for the left / right
#'  heatmap.  Multiple columns are concatenated.
#' @param rows_split_by Optional column name to split the rows of **both**
#'  heatmaps into groups (passed as \code{row_split}).  When provided, row
#'  names in the link table are prefixed with the split level to
#'  disambiguate rows across splits.
#' @param values_fill A value used to fill missing cells in the matrix
#'  (passed to \code{HeatmapAtomic}).  Default is \code{NA} (cells with no
#'  data are left empty).
#'
#' @param palette A character string naming a palette (see
#'  \code{\link{show_palettes}}) or a character vector of colours for the
#'  main heatmap colour scale.  Default \code{"RdBu"}.  Applied to both
#'  heatmaps unless overridden per-side via \code{...}.
#' @param palcolor A custom colour vector that overrides \code{palette} for
#'  the main heatmap colour scale.  Applied to both heatmaps unless
#'  overridden per-side.
#' @param palreverse Logical; if \code{TRUE}, reverse the palette direction.
#'
#' @param pie_size_name Legend title for the pie size when
#'  \code{cell_type = "pie"}.
#' @param pie_size A numeric value or function returning the pie radius.
#'  When a function, it receives the count of groups in the pie and should
#'  return a radius.
#' @param pie_values A function or string (convertible via
#'  \code{\link[base]{match.arg}}) to compute the value represented by each
#'  pie slice.  Default \code{"length"} counts observations per group.
#' @param pie_palette,pie_palcolor Palette and custom colours for pie slice
#'  fill colours.
#'
#' @param bars_sample Number of observations sampled per cell when
#'  \code{cell_type = "bars"}.  Default 100.
#'
#' @param label A function to compute text labels when
#'  \code{cell_type = "label"} (or \code{"label+mark"}).  Receives the
#'  aggregated value for a cell and optionally row/column indices and names.
#'  See \code{\link{HeatmapAtomic}} for the full dispatch contract.
#' @param label_size Default point size for label text (used as fallback
#'  when the \code{label} function does not return a \code{size} field).
#' @param label_color Default colour for label text (used as fallback when
#'  the \code{label} function does not return a \code{color} field).
#' @param label_name Legend title for the label colour scale.
#'
#' @param mark A function to compute mark symbols when
#'  \code{cell_type = "mark"} (or \code{"label+mark"}).  Same dispatch
#'  contract as \code{label}.  See \code{\link{HeatmapAtomic}} for supported
#'  mark types.
#' @param mark_color Default mark colour (fallback).
#' @param mark_size Default mark stroke width in pt (fallback).
#' @param mark_name Legend title for the mark colour scale.
#'
#' @param violin_fill A character vector of colours to use as fill for
#'  violin plots when \code{cell_type = "violin"}.  If \code{NULL}, the
#'  annotation colour is used.
#' @param boxplot_fill A character vector of colours to use as fill for
#'  boxplots when \code{cell_type = "boxplot"}.  If \code{NULL}, the
#'  annotation colour is used.
#' @param dot_size Dot size when \code{cell_type = "dot"}.  Can be a
#'  numeric value or a function.
#' @param dot_size_name Legend title for the dot size.
#'
#' @param legend_items A named numeric vector specifying custom legend
#'  entries for the main colour scale.  Names become the displayed labels.
#' @param legend_discrete Logical; if \code{TRUE}, treat the main colour
#'  scale as discrete.
#' @param legend.position A character string specifying where to place the
#'  combined legend: \code{"right"} (default), \code{"left"}, \code{"top"},
#'  \code{"bottom"}, or \code{"none"}.
#' @param legend.direction Legend stacking direction:
#'  \code{"vertical"} (default) or \code{"horizontal"}.
#'
#' @param lower_quantile,upper_quantile Quantiles used for clipping the
#'  colour scale when \code{lower_cutoff} / \code{upper_cutoff} are
#'  \code{NULL}.  Defaults are 0 and 0.99 respectively.
#' @param lower_cutoff,upper_cutoff Explicit cutoffs for the colour scale.
#'  Values outside the range are clamped (winsorized).  Override
#'  \code{lower_quantile} / \code{upper_quantile} when set.
#'
#' @param add_bg Logical; if \code{TRUE}, add a background fill behind
#'  non-tile cell types.  Not used for \code{cell_type = "tile"} or
#'  \code{"bars"}.
#' @param bg_alpha Numeric in \eqn{[0, 1]} for background transparency.
#' @param keep_na,keep_empty Passed through to \code{HeatmapAtomic}.
#'  See \code{\link{common_args}} for details.
#'
#' @param add_reticle Logical; if \code{TRUE}, draw a reticle (crosshair
#'  pattern) over the heatmap.
#' @param reticle_color Colour for the reticle lines.
#'
#' @param left_cluster_rows,right_cluster_rows Logical controlling whether
#'  rows are clustered in the left / right heatmap.  \code{NULL} (default)
#'  lets \code{HeatmapAtomic} decide based on
#'  \code{rows_orderby}/\code{cluster_rows}.
#' @param cluster_columns Logical; cluster columns in both heatmaps.
#'  \code{NULL} lets \code{HeatmapAtomic} decide.
#' @param show_row_names,show_column_names Logical; show row/column names.
#' @param border Logical; draw a border around each heatmap.  Default
#'  \code{TRUE}.
#' @param title A character string for the overall plot title.  A function
#'  can be used to generate a dynamic title from the default.
#' @param column_title,row_title Character title displayed above the columns
#'  / beside the rows of each heatmap.
#' @param na_col Colour used for \code{NA} cells.  Default \code{"grey85"}.
#' @param left_row_names_side,right_row_names_side Side for row names in the
#'  left / right heatmap.  Default \code{"left"} and \code{"right"}
#'  respectively (names face outward).
#' @param column_names_side Side for column names.  Default
#'  \code{"bottom"}.
#'
#' @param column_annotation A character vector of column names, or a named
#'  list, specifying column annotations for both heatmaps.  See
#'  \code{\link{HeatmapAtomic}} for the full specification.
#' @param column_annotation_side Side for column annotations:
#'  \code{"top"} (default) or \code{"bottom"}.  Can also be a named list
#'  for per-annotation control.
#' @param column_annotation_palette,column_annotation_palcolor Palette and
#'  custom colours for column annotations.
#' @param column_annotation_type Annotation type: \code{"auto"} (default),
#'  \code{"simple"}, \code{"pie"}, \code{"ring"}, \code{"bar"},
#'  \code{"violin"}, \code{"boxplot"}, \code{"density"}, \code{"label"}.
#'  Can be a named list for per-annotation control.
#' @param column_annotation_params A named list of additional parameters
#'  passed to each column annotation function.  See
#'  \code{\link{HeatmapAtomic}} for details.
#' @param column_annotation_agg A function or named list of functions to
#'  aggregate values for each column annotation.
#'
#' @param row_annotation,row_annotation_side,row_annotation_palette,row_annotation_palcolor,row_annotation_type,row_annotation_params,row_annotation_agg
#'  Row annotation equivalents of the \code{column_annotation_*} parameters.
#' @param left_row_annotation_side,right_row_annotation_side Side for row
#'  annotations in the left / right heatmap.  Default \code{"left"} and
#'  \code{"right"} respectively.
#'
#' @param links_span Width (in inches) of the gap column between the two
#'  heatmaps where link curves are drawn.  Default 0.5.
#' @param links_width_by Optional column name in \code{data} whose values
#'  determine the stroke width of each link line (e.g. interaction
#'  strength).  Values are min-max scaled to \eqn{[0, 1]} and multiplied by
#'  \code{links_width_scale}.
#' @param links_width_scale Numeric scaling factor applied to the normalised
#'  link intensity values to produce final line widths (\code{lwd}).
#'  Default 5.
#' @param links_color Colour of the link spline curves.  Default
#'  \code{"grey30"}.
#' @param links_alpha Alpha transparency of link curves in \eqn{[0, 1]}.
#'  Default 0.8.
#'
#' @param alpha Alpha transparency for heatmap cells in \eqn{[0, 1]}.
#' @param seed Random seed for reproducibility.  Default 8525.
#' @param padding Padding around the heatmap in CSS order (top, right,
#'  bottom, left).  Supports 1–4 values.  Default 15 (mm).
#' @param base_size A positive numeric scalar used as a scaling factor for
#'  the overall heatmap size.  Default 1 (no scaling).  Values > 1 enlarge
#'  all cell dimensions proportionally.
#' @param aspect.ratio Height-to-width ratio of a single heatmap cell.
#'  When \code{NULL} (default), sensible defaults are chosen per
#'  \code{cell_type} (e.g. 1 for tiles, 0.5 for bars, 2 for violins).
#' @param draw_opts A named list of additional arguments passed to
#'  \code{\link[ComplexHeatmap]{draw,HeatmapList-method}}.  Internally
#'  managed arguments (\code{padding}, \code{show_heatmap_legend}, etc.)
#'  take precedence.
#' @param layer_fun_callback A function to add custom graphical layers on
#'  top of each heatmap cell.  Receives \code{j}, \code{i}, \code{x},
#'  \code{y}, \code{w}, \code{h}, \code{fill}, \code{sr}, \code{sc}.
#'  See \code{\link[ComplexHeatmap]{Heatmap}} for details.
#' @param cell_type The type of cell to render.  One of \code{"tile"}
#'  (default), \code{"bars"}, \code{"label"}, \code{"mark"},
#'  \code{"label+mark"} (or \code{"mark+label"}), \code{"dot"},
#'  \code{"violin"}, \code{"boxplot"}, \code{"pie"}.  Different cell types
#'  use different \code{cell_fun} / \code{layer_fun} implementations.
#' @param cell_agg A function to aggregate values within each cell when
#'  \code{cell_type = "tile"} or \code{"label"}.  Default is
#'  \code{\link[base]{mean}}.
#' @param ... Additional arguments passed to
#'  \code{\link[ComplexHeatmap]{Heatmap}}.  Arguments prefixed with
#'  \code{left_} or \code{right_} are routed to the corresponding heatmap
#'  (the prefix is stripped before passing).  Unprefixed arguments are
#'  passed to both heatmaps but can be overridden by a prefixed version.
#'  Notable examples:
#'  \itemize{
#'    \item \code{left_name}, \code{right_name} — legend titles for the
#'          left / right colour scales.
#'    \item \code{left_row_gap}, \code{right_row_gap} — per-side row gaps.
#'    \item \code{left_column_gap}, \code{right_column_gap} — per-side
#'          column gaps.
#'    \item \code{row_names_max_width}, \code{column_names_max_height} —
#'          passed through to \code{ComplexHeatmap::Heatmap}.
#'  }
#'
#' @return A \code{patchwork}-wrapped grob (class \code{wrap_plots}) with
#'  \code{height} and \code{width} attributes (in inches).  The original
#'  \code{data} is stored in \code{p$data}.
#' @keywords internal
#' @importFrom grid pushViewport popViewport grid.layout grid.xspline
#' @importFrom grid unit convertUnit grid.grabExpr grid.newpage
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

    links_span = 0.5,
    links_width_by = NULL,
    links_width_scale = 5,
    links_color = "grey30",
    links_alpha = 0.8,

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
    left_args$legend.position <- "right"
    left_args$legend.direction <- left_args$legend.direction %||%
        legend.direction
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
    left_args$show_column_names <- left_args$show_column_names %||%
        show_column_names
    left_args$border <- left_args$border %||% border
    left_args$title <- left_args$title %||% title
    left_args$column_title <- left_args$column_title %||% column_title
    left_args$row_title <- left_args$row_title %||% row_title
    left_args$na_col <- left_args$na_col %||% na_col
    left_args$row_names_side <- left_row_names_side
    left_args$column_names_side <- left_args$column_names_side %||%
        column_names_side
    left_args$column_annotation <- left_args[["column_annotation"]] %||%
        column_annotation
    left_args$column_annotation_side <- left_args$column_annotation_side %||%
        column_annotation_side
    left_args$column_annotation_palette <- left_args$column_annotation_palette %||%
        column_annotation_palette
    left_args$column_annotation_palcolor <- left_args$column_annotation_palcolor %||%
        column_annotation_palcolor
    left_args$column_annotation_type <- left_args$column_annotation_type %||%
        column_annotation_type
    left_args$column_annotation_params <- left_args$column_annotation_params %||%
        column_annotation_params
    left_args$column_annotation_agg <- left_args$column_annotation_agg %||%
        column_annotation_agg
    left_args$row_annotation <- left_args[["row_annotation"]] %||%
        row_annotation
    left_args$row_annotation_side <- left_args$row_annotation_side %||%
        row_annotation_side
    left_args$row_annotation_palette <- left_args$row_annotation_palette %||%
        row_annotation_palette
    left_args$row_annotation_palcolor <- left_args$row_annotation_palcolor %||%
        row_annotation_palcolor
    left_args$row_annotation_type <- left_args$row_annotation_type %||%
        row_annotation_type
    left_args$row_annotation_params <- left_args$row_annotation_params %||%
        row_annotation_params
    left_args$row_annotation_agg <- left_args$row_annotation_agg %||%
        row_annotation_agg
    # flip
    left_args$alpha <- left_args$alpha %||% alpha
    left_args$padding <- left_args$padding %||% padding
    left_args$base_size <- left_args$base_size %||% base_size
    left_args$aspect.ratio <- left_args$aspect.ratio %||% aspect.ratio
    left_args$draw_opts <- left_args$draw_opts %||% draw_opts
    left_args$layer_fun_callback <- left_args$layer_fun_callback %||%
        layer_fun_callback
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
    right_args$legend_discrete <- right_args$legend_discrete %||%
        legend_discrete
    right_args$legend.position <- "right"
    right_args$legend.direction <- right_args$legend.direction %||%
        legend.direction
    right_args$lower_quantile <- right_args$lower_quantile %||% lower_quantile
    right_args$upper_quantile <- right_args$upper_quantile %||% upper_quantile
    right_args$lower_cutoff <- right_args$lower_cutoff %||% lower_cutoff
    right_args$upper_cutoff <- right_args$upper_cutoff %||% upper_cutoff
    right_args$add_bg <- right_args$add_bg %||% add_bg
    right_args$bg_alpha <- right_args$bg_alpha %||% bg_alpha
    right_args$add_reticle <- right_args$add_reticle %||% add_reticle
    right_args$reticle_color <- right_args$reticle_color %||% reticle_color
    right_args$cluster_columns <- right_args$cluster_columns %||%
        cluster_columns
    right_args$cluster_rows <- right_cluster_rows
    right_args$show_row_names <- right_args$show_row_names %||% show_row_names
    right_args$show_column_names <- right_args$show_column_names %||%
        show_column_names
    right_args$border <- right_args$border %||% border
    right_args$title <- right_args$title %||% title
    right_args$column_title <- right_args$column_title %||% column_title
    right_args$row_title <- right_args$row_title %||% row_title
    right_args$na_col <- right_args$na_col %||% na_col
    right_args$row_names_side <- right_row_names_side
    right_args$column_names_side <- right_args$column_names_side %||%
        column_names_side
    right_args$column_annotation <- right_args[["column_annotation"]] %||%
        column_annotation
    right_args$column_annotation_side <- right_args$column_annotation_side %||%
        column_annotation_side
    right_args$column_annotation_palette <- right_args$column_annotation_palette %||%
        column_annotation_palette
    right_args$column_annotation_palcolor <- right_args$column_annotation_palcolor %||%
        column_annotation_palcolor
    right_args$column_annotation_type <- right_args$column_annotation_type %||%
        column_annotation_type
    right_args$column_annotation_params <- right_args$column_annotation_params %||%
        column_annotation_params
    right_args$column_annotation_agg <- right_args$column_annotation_agg %||%
        column_annotation_agg
    right_args$row_annotation <- right_args[["row_annotation"]] %||%
        row_annotation
    right_args$row_annotation_side <- right_args$row_annotation_side %||%
        row_annotation_side
    right_args$row_annotation_palette <- right_args$row_annotation_palette %||%
        row_annotation_palette
    right_args$row_annotation_palcolor <- right_args$row_annotation_palcolor %||%
        row_annotation_palcolor
    right_args$row_annotation_type <- right_args$row_annotation_type %||%
        row_annotation_type
    right_args$row_annotation_params <- right_args$row_annotation_params %||%
        row_annotation_params
    right_args$row_annotation_agg <- right_args$row_annotation_agg %||%
        row_annotation_agg
    # flip
    right_args$alpha <- right_args$alpha %||% alpha
    right_args$padding <- right_args$padding %||% padding
    right_args$base_size <- right_args$base_size %||% base_size
    right_args$aspect.ratio <- right_args$aspect.ratio %||% aspect.ratio
    right_args$draw_opts <- right_args$draw_opts %||% draw_opts
    right_args$layer_fun_callback <- right_args$layer_fun_callback %||%
        layer_fun_callback
    right_args$cell_type <- right_args$cell_type %||% cell_type
    right_args$cell_agg <- right_args$cell_agg %||% cell_agg
    right_args$return_ht <- TRUE

    # ── Pre-compute cell dimensions (same formula as HeatmapAtomic) ──
    # Pass explicit width/height to guarantee exact cell sizing.
    # Without this, ComplexHeatmap uses null units and cells fill
    # whatever space is available in the viewport.
    .get_dim_pre <- function(args) {
        ct <- sub("mark+label", "label+mark", args$cell_type, fixed = TRUE)
        cell_w <- switch(
            ct,
            violin = 0.5,
            boxplot = 0.5,
            pie = 0.5,
            bars = 0.35,
            label = 0.6,
            mark = 0.25,
            `label+mark` = 0.6,
            0.25
        )
        aspect_default <- switch(
            ct,
            violin = 2,
            boxplot = 2,
            pie = 1,
            bars = 0.5,
            label = 0.6,
            mark = 1,
            `label+mark` = 0.6,
            1
        )

        bs <- args$base_size %||% base_size %||% 1
        ar <- args$aspect.ratio %||% aspect_default
        cell_w_pre <- cell_w * bs
        cell_h_pre <- cell_w_pre * ar

        if (!is.null(args$rows_split_by)) {
            n_row_splits <- length(unique(args$data[[args$rows_split_by]]))
            n_rows <- args$data[,
                c(args$rows_by, args$rows_split_by),
                drop = FALSE
            ] %>%
                dplyr::n_distinct()
        } else {
            n_row_splits <- 1
            n_rows <- args$data[[args$rows_by]] %>% dplyr::n_distinct()
        }

        if (!is.null(args$columns_split_by)) {
            n_col_splits <- length(unique(args$data[[args$columns_split_by]]))
            n_cols <- args$data[,
                c(args$columns_by, args$columns_split_by),
                drop = FALSE
            ] %>%
                dplyr::n_distinct()
        } else {
            n_col_splits <- 1
            n_cols <- args$data[[args$columns_by]] %>% dplyr::n_distinct()
        }

        row_gap <- args$row_gap %||%
            convertUnit(unit(1, "mm"), "inches", valueOnly = TRUE)
        height <- unit(
            n_rows * cell_h_pre + (n_row_splits - 1) * row_gap,
            "inches"
        )

        column_gap <- args$column_gap %||%
            convertUnit(unit(1, "mm"), "inches", valueOnly = TRUE)
        width <- unit(
            n_cols * cell_w_pre + (n_col_splits - 1) * column_gap,
            "inches"
        )

        list(width = width, height = height)
    }
    left_dim_pre <- .get_dim_pre(left_args)
    right_dim_pre <- .get_dim_pre(right_args)

    left_args$width <- left_dim_pre$width
    left_args$height <- left_dim_pre$height
    right_args$width <- right_dim_pre$width
    right_args$height <- right_dim_pre$height

    left_ht <- do_call(HeatmapAtomic, left_args)
    right_ht <- do_call(HeatmapAtomic, right_args)

    n_left_rows <- nrow(left_ht@matrix)
    n_left_cols <- ncol(left_ht@matrix)
    n_right_rows <- nrow(right_ht@matrix)
    n_right_cols <- ncol(right_ht@matrix)

    # Get the order of the rows in the left heatmap after clustering (if any)
    left_row_order <- left_ht@row_order
    right_row_order <- right_ht@row_order

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
        vapply(
            comp_list,
            function(u) {
                if (is.null(u)) {
                    return(0)
                }
                convertUnit(u, "inches", valueOnly = TRUE)
            },
            numeric(1)
        )
    }

    left_ch <- .ch_to_in(ComplexHeatmap:::component_height(left_ht))
    right_ch <- .ch_to_in(ComplexHeatmap:::component_height(right_ht))
    left_cw <- .ch_to_in(ComplexHeatmap:::component_width(left_ht))
    right_cw <- .ch_to_in(ComplexHeatmap:::component_width(right_ht))

    # Body dimensions (component [5] is the body)
    left_body_h <- left_ch[5]
    right_body_h <- right_ch[5]
    left_body_w <- left_cw[5]
    right_body_w <- right_cw[5]

    # body_top_offset = sum of all components ABOVE the body [1:4]
    body_top_offset_left <- sum(left_ch[1:4])
    body_top_offset_right <- sum(right_ch[1:4])

    # below_h = sum of all components BELOW the body [6:9]
    left_below_h <- sum(left_ch[6:9])
    right_below_h <- sum(right_ch[6:9])

    # Total heatmap dimensions
    left_total_w <- sum(left_cw)
    left_total_h <- sum(left_ch)
    right_total_w <- sum(right_cw)
    right_total_h <- sum(right_ch)

    total_w <- left_total_w + links_span + right_total_w
    total_h <- max(left_total_h, right_total_h)

    # ── Legend flag (needed later) ──
    legend_gap <- 0.15
    show_legend <- !identical(legend.position, "none")

    # ── Build link table ──
    left_row_names_ordered <- rownames(left_ht@matrix)[left_row_order]
    right_row_names_ordered <- rownames(right_ht@matrix)[right_row_order]

    link_table <- data %>%
        dplyr::group_by(
            !!!syms(unique(c(left_rows_by, right_rows_by, rows_split_by)))
        )

    if (!is.null(links_width_by)) {
        link_table <- link_table %>%
            dplyr::summarise(
                !!sym(links_width_by) := mean(
                    !!sym(links_width_by),
                    na.rm = TRUE
                ),
                .groups = "drop"
            )
    } else {
        link_table <- link_table %>%
            dplyr::summarise(!!sym(links_width_by) := 1, .groups = "drop")
    }

    if (!is.null(rows_split_by)) {
        link_table <- link_table %>%
            dplyr::mutate(
                left_rows = paste0(
                    !!sym(rows_split_by),
                    " // ",
                    !!sym(left_rows_by)
                ),
                right_rows = paste0(
                    !!sym(rows_split_by),
                    " // ",
                    !!sym(right_rows_by)
                )
            )
    } else {
        link_table <- link_table %>%
            dplyr::mutate(
                left_rows = !!sym(left_rows_by),
                right_rows = !!sym(right_rows_by)
            )
    }
    link_table <- link_table %>%
        mutate(
            pos_left = match(
                as.character(!!sym("left_rows")),
                left_row_names_ordered
            ),
            pos_right = match(
                as.character(!!sym("right_rows")),
                right_row_names_ordered
            )
        ) %>%
        filter(!is.na(.data$pos_left), !is.na(.data$pos_right))

    if (!is.null(links_width_by) && links_width_by %in% colnames(link_table)) {
        raw_intensity <- link_table[[links_width_by]]
        if (
            max(raw_intensity, na.rm = TRUE) > min(raw_intensity, na.rm = TRUE)
        ) {
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
            label_widths <- vapply(
                legends,
                function(lgd) {
                    if (
                        methods::.hasSlot(lgd, "labels") &&
                            is.character(lgd@labels) &&
                            length(lgd@labels) > 0
                    ) {
                        convertUnit(
                            ComplexHeatmap::max_text_width(lgd@labels),
                            "inches",
                            valueOnly = TRUE
                        )
                    } else {
                        0.6 # continuous legend: fixed label width
                    }
                },
                numeric(1)
            )
            max_label_w <- max(label_widths, 0.5)
            title_w <- max(
                vapply(
                    legends,
                    function(lgd) {
                        if (
                            methods::.hasSlot(lgd, "title") &&
                                is.character(lgd@title) &&
                                nchar(lgd@title) > 0
                        ) {
                            convertUnit(
                                ComplexHeatmap::max_text_width(lgd@title),
                                "inches",
                                valueOnly = TRUE
                            )
                        } else {
                            0
                        }
                    },
                    numeric(1)
                ),
                0
            )
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

    left_center_offset <- (total_h - left_total_h) / 2
    right_center_offset <- (total_h - right_total_h) / 2

    left_body_top_npc <- 1 -
        (left_center_offset + body_top_offset_left) / total_h
    left_body_bot_npc <- 1 -
        (left_center_offset + body_top_offset_left + left_body_h) / total_h
    left_body_range <- left_body_top_npc - left_body_bot_npc

    right_body_top_npc <- 1 -
        (right_center_offset + body_top_offset_right) / total_h
    right_body_bot_npc <- 1 -
        (right_center_offset + body_top_offset_right + right_body_h) / total_h
    right_body_range <- right_body_top_npc - right_body_bot_npc

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
    hm_col_widths <- unit(c(left_total_w, links_span, right_total_w), "inches")
    hm_col_left <- 1L
    hm_col_gap <- 2L
    hm_col_right <- 3L

    if (show_legend && (pos_left || pos_right)) {
        n_cols <- 4L
        if (pos_left) {
            col_widths <- unit(
                c(
                    legend_w + legend_gap,
                    left_total_w,
                    links_span,
                    right_total_w
                ),
                "inches"
            )
            lg_col <- 1L
            hm_col_left <- 2L
            hm_col_gap <- 3L
            hm_col_right <- 4L
        } else {
            # right
            col_widths <- unit(
                c(
                    left_total_w,
                    links_span,
                    right_total_w,
                    legend_w + legend_gap
                ),
                "inches"
            )
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
        } else {
            # bottom
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
            layout = grid.layout(
                n_rows,
                n_cols,
                widths = col_widths,
                heights = row_heights
            )
        ))

        # Left heatmap — centered exact-size viewport so ComplexHeatmap
        # sizes the body to exactly n_left_rows × cell_h
        pushViewport(viewport(
            layout.pos.row = hm_row,
            layout.pos.col = hm_col_left
        ))
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
        pushViewport(viewport(
            layout.pos.row = hm_row,
            layout.pos.col = hm_col_right
        ))
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
                pushViewport(viewport(
                    layout.pos.row = lg_row,
                    layout.pos.col = lg_col
                ))
                ComplexHeatmap::draw(
                    combined_legend,
                    x = unit(0.05, "npc"),
                    just = "left"
                )
            } else {
                # top / bottom: legend spans all heatmap columns
                pushViewport(viewport(
                    layout.pos.row = lg_row,
                    layout.pos.col = 1:3
                ))
                ComplexHeatmap::draw(
                    combined_legend,
                    x = unit(0.5, "npc"),
                    just = "centre"
                )
            }
            popViewport()
        }

        # Links in middle column
        pushViewport(viewport(
            layout.pos.row = hm_row,
            layout.pos.col = hm_col_gap
        ))
        if (nrow(link_table) > 0) {
            for (k in seq_len(nrow(link_table))) {
                y_left <- compute_y(
                    link_table$pos_left[k],
                    n_left_rows,
                    left_body_top_npc,
                    left_body_range
                )
                y_right <- compute_y(
                    link_table$pos_right[k],
                    n_right_rows,
                    right_body_top_npc,
                    right_body_range
                )
                grid.xspline(
                    x = unit(c(0.15, 0.5, 0.85), "npc"),
                    y = unit(
                        c(y_left, mean(c(y_left, y_right)), y_right),
                        "npc"
                    ),
                    shape = 0.5,
                    gp = gpar(
                        lwd = link_table$intensity[k] * links_width_scale,
                        col = links_color,
                        alpha = links_alpha
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
    attr(p, "width") <- display_w + 1

    p$data <- data
    p
}

#' Linked Heatmap
#'
#' @description
#' Draw two heatmaps side-by-side with spline link lines connecting matching
#' rows across the two heatmaps.  This is the public, exported interface for
#' creating linked-heatmap visualisations.
#'
#' A typical use case is visualising ligand–receptor interactions: the
#' left heatmap shows ligand expression (rows = ligands, columns = cell
#' sources), the right heatmap shows receptor expression (rows = receptors,
#' columns = cell targets), and link curves connect each ligand to its
#' cognate receptor(s).
#'
#' @section Left / right specification:
#' Parameters that differ between the two heatmaps are prefixed with
#' \code{left_} or \code{right_}.  Shared parameters (e.g. \code{palette},
#' \code{cell_type}, \code{cluster_columns}) apply to both sides but can be
#' overridden per-side via the \code{...} argument.  Every parameter listed
#' below can also be passed with a \code{left_} or \code{right_} prefix in
#' \code{...} for full per-side control.
#'
#' The \code{...} argument is also forwarded to
#' \code{\link[ComplexHeatmap]{Heatmap}} after prefix-stripping, allowing
#' direct access to \code{ComplexHeatmap} parameters (e.g.
#' \code{left_row_names_gp}, \code{right_column_names_rot}).
#'
#' @section Split-by support:
#' When \code{split_by} is provided, the data is partitioned into subsets
#' and an independent linked-heatmap pair is produced for each level.  The
#' results are combined via \code{\link[patchwork]{wrap_plots}} according to
#' \code{nrow}, \code{ncol}, \code{byrow}, and \code{design}.
#' Per-split \code{palette}, \code{palcolor}, and \code{legend.position} can
#' be specified as named lists keyed by split level.
#'
#' @section Dimension calculation:
#' Cell dimensions are pre-computed from \code{cell_type},
#' \code{aspect.ratio}, \code{base_size}, and the unique row/column counts
#' in the data (accounting for split groups).  These exact dimensions are
#' passed to \code{ComplexHeatmap::Heatmap} so cells have guaranteed
#' physical sizes, ensuring that the two heatmaps' bodies align precisely
#' and that link line endpoints land on the correct rows.  The final
#' \code{height} / \code{width} attributes on the returned object include
#' legend space and are clamped to \eqn{[4, 64]} inches with aspect-ratio
#' correction.
#'
#' @inheritParams LinkedHeatmapAtomic
#' @inheritParams common_args
#' @param values_by Default column name for heatmap cell values.  Used as
#'  fallback when \code{left_values_by} / \code{right_values_by} are not
#'  explicitly provided via \code{...}.
#' @param name Default legend title for the colour scale.  Used as fallback
#'  when \code{left_name} / \code{right_name} are not provided via
#'  \code{...}.  The suffixes \code{" (left)"} / \code{" (right)"} are
#'  appended automatically.
#' @param rows_by Default column for rows in both heatmaps.  Used as
#'  fallback for \code{left_rows_by} / \code{right_rows_by}.
#' @param rows_by_sep Separator for concatenated \code{rows_by} columns.
#' @param rows_split_by_sep Separator for concatenated
#'  \code{rows_split_by} columns.
#' @param columns_by Default column for columns in both heatmaps.  Used as
#'  fallback for \code{left_columns_by} / \code{right_columns_by}.
#' @param columns_by_sep Separator for concatenated \code{columns_by}
#'  columns.
#' @param columns_split_by Default column to split columns into groups.
#'  Used as fallback for \code{left_columns_split_by} /
#'  \code{right_columns_split_by}.
#' @param columns_split_by_sep Separator for concatenated
#'  \code{columns_split_by} columns.
#' @param rows_data,columns_data Optional data frames providing additional
#'  row / column metadata for annotations.  Passed through to
#'  \code{HeatmapAtomic}.
#' @param rows_orderby,columns_orderby Column name to order rows / columns
#'  by (disables clustering when set).
#' @param columns_name Display name for the column annotation.
#' @param columns_split_name Display name for the column split annotation.
#' @param rows_name Display name for the row annotation.
#' @param rows_split_name Display name for the row split annotation.
#' @param pie_name Default name for the pie legend.  Used as fallback for
#'  \code{left_pie_name} / \code{right_pie_name}.
#' @param pie_group_by Default column(s) for pie grouping.  Used as fallback
#'  for \code{left_pie_group_by} / \code{right_pie_group_by}.
#' @param pie_group_by_sep Separator for concatenated \code{pie_group_by}
#'  columns.
#' @param cluster_rows Default clustering setting for rows.  Used as
#'  fallback for \code{left_cluster_rows} / \code{right_cluster_rows}.
#' @param row_names_side Default side for row names.  Used as fallback for
#'  \code{left_row_names_side} / \code{right_row_names_side}.  Default
#'  \code{"right"}.
#' @param flip Logical; must be \code{FALSE} for linked heatmaps (flipping
#'  is not supported).  Default \code{FALSE}.
#' @param ... Additional arguments passed to \code{\link{LinkedHeatmapAtomic}}.
#'  All parameters listed above (and those inherited from
#'  \code{LinkedHeatmapAtomic}) can be specified with \code{left_} or
#'  \code{right_} prefixes for per-side control (e.g.
#'  \code{left_palette = "Blues"}, \code{right_palette = "Reds"}).
#'  Unprefixed arguments apply to both sides.  Also forwarded to
#'  \code{\link[ComplexHeatmap]{Heatmap}} after prefix stripping.
#'
#' @return A \code{patchwork} object (class \code{wrap_plots}) with
#'  \code{height} and \code{width} attributes (in inches).  When
#'  \code{combine = FALSE}, a named list of such objects, one per
#'  \code{split_by} level.
#' @export
#' @seealso \code{\link{Heatmap}}
#' @examples
#' \donttest{
#' set.seed(8525)
#' # Define sparse ligand-receptor pairs
#' pairs_df <- data.frame(
#'     ligand = c("Ligand1", "Ligand2", "Ligand3", "Ligand4", "Ligand5",
#'                "Ligand1", "Ligand3", "Ligand5"),
#'     receptor = c("Receptor1", "Receptor2", "Receptor1", "Receptor3", "Receptor4",
#'                  "Receptor5", "Receptor2", "Receptor5"),
#'     stringsAsFactors = FALSE
#' )
#' sources <- paste0("Source", 1:4)
#' targets <- paste0("Target", 1:6)
#'
#' # Expand pairs across all sources and targets
#' data <- merge(
#'     merge(pairs_df, data.frame(source = sources, stringsAsFactors = FALSE)),
#'     data.frame(target = targets, stringsAsFactors = FALSE)
#' )
#' data$split <- sample(c("A", "B"), nrow(data), replace = TRUE)
#' data$ligand_expr <- runif(nrow(data), 0, 10)
#' data$receptor_expr <- runif(nrow(data), 0, 10)
#' data$intensity <- runif(nrow(data), 0, 1)
#'
#' if (requireNamespace("ComplexHeatmap", quietly = TRUE)) {
#'     LinkedHeatmap(
#'         data,
#'         column_names_side = "top",
#'         row_names_side = "right",
#'         right_cluster_rows = FALSE,
#'         left_show_row_names = TRUE,
#'         right_show_row_names = TRUE,
#'         left_row_names_side = "right",
#'         left_rows_by = "ligand",
#'         left_columns_by = "source",
#'         left_values_by = "ligand_expr",
#'         left_name = "Ligand",
#'         right_rows_by = "receptor",
#'         right_columns_by = "target",
#'         right_values_by = "receptor_expr",
#'         right_name = "Receptor",
#'         links_width_by = "intensity"
#'     )
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
    links_width_scale = 5,
    links_color = "grey40",
    links_alpha = 0.6,

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
        "[LinkedHeatmap] `flip` is not supported for linked heatmaps; please set `flip = FALSE`" = isFALSE(
            flip
        )
    )

    cell_type <- match.arg(cell_type)
    cell_type <- sub("mark+label", "label+mark", cell_type, fixed = TRUE)

    links_width_by <- check_columns(data, links_width_by)

    args <- list(...)
    left_rows_orderby <- args$left_rows_orderby %||% rows_orderby
    if (!is.null(left_rows_orderby)) {
        left_cluster_rows <- args$left_cluster_rows %||% cluster_rows %||% FALSE
        stopifnot(
            "[LinkedHeatmap] `left_rows_orderby` can't be used with `left_cluster_rows/cluster_rows = TRUE`" = !left_cluster_rows
        )
    } else {
        left_cluster_rows <- args$left_cluster_rows %||% cluster_rows %||% TRUE
    }

    right_rows_orderby <- args$right_rows_orderby %||% rows_orderby
    if (!is.null(right_rows_orderby)) {
        right_cluster_rows <- args$right_cluster_rows %||%
            cluster_rows %||%
            FALSE
        stopifnot(
            "[LinkedHeatmap] `right_rows_orderby` can't be used with `right_cluster_rows/cluster_rows = TRUE`" = !right_cluster_rows
        )
    } else {
        right_cluster_rows <- args$right_cluster_rows %||%
            cluster_rows %||%
            TRUE
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
        left_name = args$left_name %||%
            if (is.null(name)) NULL else paste0(name, " (left)"),
        left_rows_by = args$left_rows_by %||% rows_by,
        left_rows_by_sep = args$left_rows_by_sep %||% rows_by_sep,
        left_rows_name = args$left_rows_name %||% rows_name,
        left_rows_orderby = left_rows_orderby,
        left_columns_orderby = args$left_columns_orderby %||% columns_orderby,
        left_columns_by = args$left_columns_by %||% columns_by,
        left_columns_by_sep = args$left_columns_by_sep %||% columns_by_sep,
        left_columns_name = args$left_columns_name %||% columns_name,
        left_columns_split_by = args$left_columns_split_by %||%
            columns_split_by,
        left_columns_split_by_sep = args$left_columns_split_by_sep %||%
            columns_split_by_sep,
        left_columns_split_name = args$left_columns_split_name %||%
            columns_split_name,
        left_pie_group_by = args$left_pie_group_by %||% pie_group_by,
        left_pie_group_by_sep = args$left_pie_group_by_sep %||%
            pie_group_by_sep,
        left_pie_name = args$left_pie_name %||% pie_name,
        left_rows_data = args$left_rows_data %||% rows_data,
        left_columns_data = args$left_columns_data %||% columns_data,
        right_values_by = args$right_values_by %||% values_by,
        right_name = args$right_name %||%
            if (is.null(name)) NULL else paste0(name, " (right)"),
        right_rows_by = args$right_rows_by %||% rows_by,
        right_rows_by_sep = args$right_rows_by_sep %||% rows_by_sep,
        right_rows_name = args$right_rows_name %||% rows_name,
        right_rows_orderby = right_rows_orderby,
        right_columns_orderby = args$right_columns_orderby %||% columns_orderby,
        right_columns_by = args$right_columns_by %||% columns_by,
        right_columns_by_sep = args$right_columns_by_sep %||% columns_by_sep,
        right_columns_name = args$right_columns_name %||% columns_name,
        right_columns_split_by = args$right_columns_split_by %||%
            columns_split_by,
        right_columns_split_by_sep = args$right_columns_split_by_sep %||%
            columns_split_by_sep,
        right_columns_split_name = args$right_columns_split_name %||%
            columns_split_name,
        right_pie_group_by = args$right_pie_group_by %||% pie_group_by,
        right_pie_group_by_sep = args$right_pie_group_by_sep %||%
            pie_group_by_sep,
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
        legend.position,
        names(hmdata$data),
        "legend.position"
    )
    legend.direction <- check_legend(
        legend.direction,
        names(hmdata$data),
        "legend.direction"
    )

    # ── Build per-split plots ──
    plots <- lapply(names(hmdata$data), function(nm) {
        default_title <- if (length(hmdata$data) == 1 && identical(nm, "...")) {
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
        args_atomic$links_width_scale <- links_width_scale
        args_atomic$links_color <- links_color
        args_atomic$links_alpha <- links_alpha

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
