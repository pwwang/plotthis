#' Atomic heatmap without split
#'
#' @inheritParams process_heatmap_data
#' @inheritParams common_args
#' @param data A data frame used to create the heatmap.
#'  The data should be in a long form where each row represents a instance in the heatmap.
#' @param values_fill A value to fill in the missing values in the heatmap.
#' When there is missing value in the data, the cluster_rows and cluster_columns will fail.
#' @param border A logical value indicating whether to draw the border of the heatmap.
#'  If TRUE, the borders of the slices will be also drawn.
#' @param title The global (column) title of the heatmap
#' @param lower_quantile,upper_quantile,lower_cutoff,upper_cutoff Vector of minimum and maximum cutoff values or quantile values for each feature.
#'  It's applied to aggregated values when aggregated values are used (e.g. plot_type tile, label, etc).
#'  It's applied to raw values when raw values are used (e.g. plot_type bars, etc).
#' @note Removed parameters: `rows_palette`, `rows_palcolor`, `columns_palette`, `columns_palcolor`,
#'  `columns_split_palette`, `columns_split_palcolor`, `rows_split_palette`, `rows_split_palcolor` —
#'  use `row_annotation_palette`/`row_annotation_palcolor` with key `.row` (alias for `rows_by`) or
#'  `.rows.split` (alias for `rows_split_by`); similarly `column_annotation_palette`/
#'  `column_annotation_palcolor` with `.col`/`.column` or `.col.split`/`.column.split`.
#'  Also removed: `row_name_annotation`, `row_name_legend`, `column_name_annotation`, `column_name_legend` —
#'  set `row_annotation_params$.row` to `FALSE` to disable the row name annotation; use `$show_legend`
#'  inside the param entry to control legend visibility. Use `column_annotation_params$.col` similarly.
#' @param cluster_columns A logical value indicating whether to cluster the columns.
#'  If TRUE and columns_split_by is provided, the clustering will only be applied to the columns within the same split.
#' @param cluster_rows A logical value indicating whether to cluster the rows.
#'  If TRUE and rows_split_by is provided, the clustering will only be applied to the rows within the same split.
#' @param legend_items A numeric vector with names to specifiy the items in the main legend.
#'  The names will be working as the labels of the legend items.
#' @param legend_discrete A logical value indicating whether the main legend is discrete.
#' @param show_row_names A logical value indicating whether to show the row names.
#'  If TRUE, the legend of the row group annotation will be hidden.
#' @param show_column_names A logical value indicating whether to show the column names.
#'  If TRUE, the legend of the column group annotation will be hidden.
#' @param column_title A character string/vector of the column name(s) to use as the title of the column group annotation.
#' @param row_title A character string/vector of the column name(s) to use as the title of the row group annotation.
#' @param na_col A character string specifying the color for missing values.
#'  The default is "grey85".
#' @param row_names_side A character string specifying the side of the row names.
#'  The default is "right".
#' @param column_names_side A character string specifying the side of the column names.
#'  The default is "bottom".
#' @param bars_sample An integer specifying the number of samples to draw the bars.
#' @param flip A logical value indicating whether to flip the heatmap.
#' The idea is that, you can simply set `flip = TRUE` to flip the heatmap.
#' You don't need to swap the arguments related to rows and columns, except those you specify via `...`
#' that are passed to `ComplexHeatmap::Heatmap()` directly.
#' @param pie_palette A character string specifying the palette of the pie chart.
#' @param pie_palcolor A character vector of colors to override the palette of the pie chart.
#' @param pie_values A function or character that can be converted to a function by [match.arg()]
#' to calculate the values for the pie chart. Default is "length".
#' The function should take a vector of values as the argument and return a single value, for each
#' group in `pie_group_by`.
#' @param pie_size A numeric value or a function specifying the size of the pie chart.
#'  If it is a function, the function should take `count` as the argument and return the size.
#' @param pie_size_name A character string specifying the name of the legend for the pie size.
#' @param label_size A numeric value specifying the default size (pt) of the labels when `cell_type = "label"`.
#'  Used as fallback when the `label` function does not return a `size` field.
#' @param label_color A character string specifying the default color of the labels when `cell_type = "label"`.
#'  Used as fallback when the `label` function does not return a `color` field. Default is `"black"`.
#' @param label_name A character string specifying the title of the label legend. Default is `"label"`.
#'  The legend is shown automatically when the `label` function returns a list with a `legend` field for at least
#'  one cell — no extra configuration needed. Set `legend.position = "none"` to suppress all legends.
#' @param mark_size A numeric value specifying the default stroke width (lwd) of the marks when `cell_type = "mark"`.
#'  Used as fallback when the `mark` function does not return a `size` field. Default is `1`.
#' @param mark_color A character string specifying the default color of the marks when `cell_type = "mark"`.
#'  Used as fallback when the `mark` function does not return a `color` field. Default is `"black"`.
#' @param mark_name A character string specifying the title of the mark legend. Default is `"mark"`.
#'  The legend is shown automatically when the `mark` function returns a list with a `legend` field.
#' @param mark A function to calculate the marks drawn on top of heatmap cells when `cell_type = "mark"`.
#'  Same dispatch rules as `label` (1, 3, or 5 arguments).
#'  The function should return one of:
#'  * `NA` — no mark is drawn for this cell.
#'  * A character scalar — the mark type string; `mark_color` and `mark_size` are used for appearance.
#'  * A named list with any of the following fields:
#'    - `mark` (or first unnamed element): character scalar, the mark type string.
#'    - `size`: numeric stroke width (lwd), overrides `mark_size`.
#'    - `color`: character color string, overrides `mark_color`.
#'    - `legend`: character string used as the legend entry key.
#'    - `order`: integer controlling legend entry position (smaller = higher).
#'  **Supported mark types:**
#'  * Primitives: `-` (h-line), `|` (v-line), `+` (cross), `/` (l-diag), `\` (r-diag),
#'    `x` (both diags), `o` (circle with gap), `()` (circle touching edge), `<>` (diamond).
#'  * With rectangular border: `[]`, `[-]`, `[|]`, `[+]`, `[/]`, `[\]`, `[x]`, `[o]`, `[()]`, `[<>]`.
#'  * With full circle: `(-)`, `(|)`, `(+)`, `(/)`, `(\)`, `(x)`, `(o)`, `(<>)`.
#'  * With diamond: `<->`, `<|>`, `<+>`, `</>`, `<\>`, `<x>`, `<o>`.
#'  * Octagon (standalone or wrapper): `{}`, `{-}`, `{|}`, `{+}`, `{/}`, `{\\}`, `{x}`, `{o}`, `{()}`, `{<>}`.
#'  * Combinations: e.g. `[(|)]`, `[(-)]`, `[(+)]`, `[(/)]`, `[(\)]`, `[(x)]`, `[(o)]`, `[(<>)]`.
#' @param label A function to calculate the labels for the heatmap cells.
#'  It can take either 1, 3, or 5 arguments. The first argument is the aggregated value for a single cell.
#'  If it takes 3 arguments, the second and third arguments are the row and column indices of that cell.
#'  If it takes 5 arguments, the second and third arguments are the row and column indices,
#'  and the fourth and fifth arguments are the row and column names.
#'  The function should return one of:
#'  * `NA` — no label is drawn for this cell.
#'  * A character scalar — used as the label text; `label_size` and `label_color` are used for size and color.
#'  * A named list with any of the following fields:
#'    - `label`: character scalar for the label text.
#'    - `size`: numeric pt size (overrides `label_size`).
#'    - `color`: character color string (overrides `label_color`).
#'    - `legend`: character string used as the legend entry for this cell's color/label combination.
#'    - `order`: integer controlling the position of this legend entry — smaller values appear first (top) in the legend.
#'      Entries without an `order` are appended after all explicitly ordered entries.
#'  For the indices, if you have the same dimension of data (same order of rows and columns) as the heatmap, you need to use `ComplexHeatmap::pindex()` to get the correct values.
#' @param layer_fun_callback A function to add additional layers to the heatmap.
#'  The function should have the following arguments: `j`, `i`, `x`, `y`, `w`, `h`, `fill`, `sr` and `sc`.
#'  Please also refer to the `layer_fun` argument in `ComplexHeatmap::Heatmap`.
#' @param cell_type A character string specifying the type of the heatmap cells.
#'  The default is "tile" Other options are "bars", "label", "mark", "label+mark" (or equivalently "mark+label"),
#'  "dot", "violin", "boxplot" and "pie".
#'  Use "label+mark" to render both marks (drawn first, as background) and text labels (drawn on top)
#'  in each cell simultaneously, combining all `label_*` and `mark_*` parameters.
#'  Note that for pie chart, the values under columns specified by `rows` will not be used directly. Instead, the values
#'  will just be counted in different `pie_group_by` groups. `NA` values will not be counted.
#' @param cell_agg A function to aggregate the values in the cell, for the cell type "tile" and "label".
#'  The default is `mean`.
#' @param add_bg A logical value indicating whether to add a background to the heatmap.
#'  Does not work with `cell_type = "bars"` or `cell_type = "tile"`.
#' @param bg_alpha A numeric value between 0 and 1 specifying the transparency of the background.
#' @param violin_fill A character vector of colors to override the fill color of the violin plot.
#'  If NULL, the fill color will be the same as the annotion.
#' @param boxplot_fill A character vector of colors to override the fill color of the boxplot.
#'  If NULL, the fill color will be the same as the annotion.
#' @param dot_size A numeric value specifying the size of the dot or a function to calculate the size
#'  from the values in the cell or a function to calculate the size from the values in the cell.
#' @param dot_size_name A character string specifying the name of the legend for the dot size.
#' If NULL, the dot size legend will not be shown.
#' @param column_annotation A character string/vector of the column name(s) to use as the column annotation.
#'  Or a list with the keys as the names of the annotation and the values as the column names.
#' @param column_annotation_side A character string or named list specifying which side each column
#'  annotation is placed on. Accepts \code{"top"} (default) or \code{"bottom"}.
#'  \itemize{
#'    \item \strong{String:} All column annotations go to that side (e.g. \code{"bottom"}).
#'    \item \strong{Named list:} Per-annotation side control. Keys are annotation names or aliases
#'          (\code{.col}, \code{.col.split}, etc.). Values are \code{"top"} or \code{"bottom"}.
#'          Use the special \code{.default} key to set the side for unspecified annotations
#'          (e.g. \code{list(.default = "top", my_anno = "bottom")}).
#'    \item \strong{Ordering within each side:} Name annotations (\code{columns_by}) are always
#'          placed closest to the heatmap body; split annotations (\code{columns_split_by}) are
#'          placed farthest away; user-defined annotations sit in between.
#'  }
#'  \strong{Note:} Placing column annotations on \code{"bottom"} conflicts with
#'  \code{legend.position = "bottom"} — the legend may overlap the annotation names.
#'  Consider using a different legend position in that case.
#' @param column_annotation_palette A character string specifying the palette of the column annotation.
#'  The default is "Paired".
#'  Could be a list with the keys as the names of the annotation and the values as the palettes.
#' @param column_annotation_palcolor A character vector of colors to override the palette of the column annotation.
#'  Could be a list with the keys as the names of the annotation and the values as the palcolors.
#' @param column_annotation_type A character string specifying the type of the column annotation.
#'  The default is "auto". Other options are "simple", "pie", "ring", "bar", "violin", "boxplot", "density",
#'  "label".
#'  Could be a list with the keys as the names of the annotation and the values as the types.
#'  If the type is "auto", the type will be determined by the type and number of the column data.
#'  For split or name annotations, use aliases (e.g. `.col.split`, `.col`) to set the type.
#'  \itemize{
#'    \item \code{"simple"} — simple annotation via [anno_simple()] (for split/name annotations)
#'    \item \code{"label"} — Text label annotation via [anno_simple()]/[anno_block()] (for split/name annotations)
#'  }
#' @param column_annotation_params A list of parameters passed to the annotation function.
#'  Could be a list with the keys as the names of the annotation and the values as the parameters.
#'  For the name/split annotations, use aliases: `.col`/`.cols`/`.column`/`.columns` for `columns_by`, `.col.split`/`.cols.split`/`.column.split`/`.columns.split`
#'  for `columns_split_by`. Setting a key to `FALSE` disables that annotation.
#'  `$<key>$show_legend` controls the legend for that annotation.
#'  For \code{"label"} type annotations, use \code{labels_gp} to style the label text
#'  (e.g. \code{labels_gp = grid::gpar(col = "white", fontsize = 12)}).
#'  See [anno_pie()], [anno_ring()], [anno_bar()], [anno_violin()], [anno_boxplot()], [anno_density()], [anno_simple()], [anno_points()], [anno_lines()] and [anno_block()] for the parameters of each annotation function.
#' @param column_annotation_agg A function or named list of functions to aggregate values for
#'  each column annotation. If a single function, it applies to all annotations. If a named list,
#'  keys are annotation names. Defaults vary by annotation type: \code{dplyr::first} for
#'  \code{"simple"}/\code{"points"}/\code{"lines"}, \code{function(x) paste(unique(x), collapse = ", ")}
#'  for \code{"label"}, and no aggregation for others (e.g. \code{"pie"}, \code{"violin"}).
#' @param row_annotation A character string/vector of the column name(s) to use as the row annotation.
#' Or a list with the keys as the names of the annotation and the values as the column names.
#' @param row_annotation_side A character string or named list specifying which side each row
#'  annotation is placed on. Accepts \code{"left"} (default) or \code{"right"}.
#'  \itemize{
#'    \item \strong{String:} All row annotations go to that side (e.g. \code{"right"}).
#'    \item \strong{Named list:} Per-annotation side control. Keys are annotation names or aliases
#'          (\code{.row}, \code{.rows.split}, etc.). Values are \code{"left"} or \code{"right"}.
#'          Use the special \code{.default} key to set the side for unspecified annotations
#'          (e.g. \code{list(.default = "left", .row = "right")}).
#'    \item \strong{Ordering within each side:} Name annotations (\code{rows_by}) are always
#'          placed closest to the heatmap body; split annotations (\code{rows_split_by}) are
#'          placed farthest away; user-defined annotations sit in between.
#'  }
#' @param row_annotation_palette A character string specifying the palette of the row annotation.
#' The default is "Paired".
#' Could be a list with the keys as the names of the annotation and the values as the palettes.
#' @param row_annotation_palcolor A character vector of colors to override the palette of the row annotation.
#' Could be a list with the keys as the names of the annotation and the values as the palcolors.
#' @param row_annotation_type A character string specifying the type of the row annotation.
#' The default is "auto". Other options are "simple", "pie", "ring", "bar", "violin", "boxplot", "density",
#' "label".
#' Could be a list with the keys as the names of the annotation and the values as the types.
#' If the type is "auto", the type will be determined by the type and number of the row data.
#' For split or name annotations, use aliases (e.g. `.rows.split`, `.row`) to set the type.
#' \itemize{
#'   \item \code{"simple"} — Simple annotation via [anno_simple()]. Only valid for row/column name and
#'         split label annotation
#'   \item \code{"label"} — Text label annotation via [anno_simple()]/[anno_block()] (for split/name annotations)
#' }
#' @param row_annotation_params A list of parameters passed to the annotation function.
#'  Could be a list with the keys as the names of the annotation and the values as the parameters.
#'  For the name/split annotations, use aliases: `.row`/`.rows` for `rows_by`, `.rows.split`/`.row.split` for `rows_split_by`.
#'  Setting a key to `FALSE` disables that annotation. `$<key>$show_legend` controls the legend.
#'  For \code{"label"} type row (name) annotations, use \code{label_rot} to control text rotation
#'  (default \code{-90} on the left side, \code{90} on the right side).
#'  For \code{"label"} type, use \code{labels_gp} to style the label text.
#'  Same structure as \code{column_annotation_params}.
#' @param row_annotation_agg A function or named list of functions to aggregate values for
#'  each row annotation. Same behavior as \code{column_annotation_agg}.
#' @param add_reticle A logical value indicating whether to add a reticle to the heatmap.
#' @param reticle_color A character string specifying the color of the reticle.
#' @param palette A character string specifying the palette of the heatmap cells.
#' @param palcolor A character vector of colors to override the palette of the heatmap cells.
#' @param alpha A numeric value between 0 and 1 specifying the transparency of the heatmap cells.
#' @param padding A numeric vector of length 4 specifying the padding of the heatmap in the order of top, right, bottom, left.
#' Like padding in css. Note that it is different than the `padding` argument in `ComplexHeatmap::draw()`, which is the padding
#' in the order of bottom, left, top, right.
#' It also support 1, 2, 3 values like css padding.
#' When 1 element is provided, it will be used for all sides.
#' When 2 elements are provided, the first one will be used for top and bottom, and the second one will be used for left and right.
#' When 3 elements are provided, the first one will be used for top, the second one will be used for left and right, and the third one will be used for bottom.
#' When 4 elements are provided, they will be used for top, right, bottom, and left respectively.
#' If no unit is provided, the default unit will be "mm".
#' @param base_size A positive numeric scalar used as a scaling factor for the overall heatmap size.
#' Default is `1` (no scaling). Values greater than 1 enlarge the heatmap; values less than 1 shrink it.
#' Internally, all calculated cell dimensions are multiplied by this factor.
#' @param aspect.ratio A positive numeric scalar giving the height-to-width ratio of a single heatmap
#' cell. When `NULL` (default), sensible per-`cell_type` defaults are used:
#' * `tile`, `label`, `dot`: square cells (ratio = 1).
#' * `bars`: wider-than-tall cells (ratio = 0.5) so individual bars are legible.
#' * `violin`, `boxplot`, `pie`: square cells with a larger base size (0.5 in) so embedded
#'   sub-plots have enough room.
#' Provide an explicit value to override these defaults (e.g. `aspect.ratio = 2` for
#' portrait cells, `aspect.ratio = 0.5` for landscape cells).
#' Note that for `cell_type = "pie"` the cells are always drawn square by ComplexHeatmap
#' regardless of this setting; use it primarily to budget the figure size.
#' Note that the aspect ratio is not guaranteed to be perfectly preserved; it will also be restricted by the size and height/width ratio of the entire plot itself.
#' @param draw_opts A named list of additional arguments passed to [ComplexHeatmap::draw()]. Arguments already managed
#' internally (`annotation_legend_list`, `padding`, `show_annotation_legend`, `annotation_legend_side`,
#' `column_title`) take precedence over any values supplied here.
#' See <https://jokergoo.github.io/ComplexHeatmap/reference/draw-HeatmapList-method.html> for available options.
#' @param ... Other arguments passed to [ComplexHeatmap::Heatmap()]
#' When `row_names_max_width` is passed, a unit is expected. But you can also pass a numeric values,
#' with a default unit "inches", or a string like "5inches" to specify the number and unit directly.
#' Unmatched arguments will be warned and ignored.
#' @importFrom circlize colorRamp2
#' @importFrom dplyr group_by across ungroup %>% all_of summarise first slice_sample everything group_map
#' @importFrom tidyr pivot_longer pivot_wider unite expand_grid
#' @importFrom ggplot2 ggplotGrob theme_void
#' @importFrom grid grid.rect grid.text grid.lines grid.points viewport gpar unit grid.draw is.unit
#' @importFrom grid convertUnit grid.grabExpr
#' @return An object of heatmap that is wrapped by `patchwork::wrap_plots()`
#' @keywords internal
HeatmapAtomic <- function(
    data, values_by, values_fill = NA,
    # data definition
    rows_by = NULL, rows_split_by = NULL,
    columns_by = NULL, columns_split_by = NULL,
    # palettes
    palette = "RdBu", palcolor = NULL, palreverse = FALSE,
    # cell_type: pies
    pie_size_name = "size", pie_size = NULL, pie_values = "length",
    pie_group_by = NULL, pie_palette = "Spectral", pie_palcolor = NULL,
    # cell_type: bars
    bars_sample = 100,
    # cell_type: label
    label = scales::label_number_auto(), label_size = 10, label_color = "black", label_name = "label",
    # cell_type: mark
    mark = identity, mark_color = "black", mark_size = 1, mark_name = "mark",
    # cell_type: violin
    violin_fill = NULL,
    # cell_type: boxplot
    boxplot_fill = NULL,
    # cell_type: dot
    dot_size = 8, dot_size_name = "size",
    # legend
    legend_items = NULL, legend_discrete = FALSE,
    legend.position = "right", legend.direction = "vertical",
    # values
    lower_quantile = 0, upper_quantile = 0.99, lower_cutoff = NULL, upper_cutoff = NULL,
    # bg
    add_bg = FALSE, bg_alpha = 0.5, keep_na = FALSE, keep_empty = FALSE,
    # reticle
    add_reticle = FALSE, reticle_color = "grey",
    # passed to ComplexHeatmap::Heatmap
    cluster_columns = TRUE, cluster_rows = TRUE, show_row_names = NULL, show_column_names = NULL,
    border = TRUE, title = NULL, column_title = NULL, row_title = NULL, na_col = "grey85",
    row_names_side = "right", column_names_side = "bottom",
    column_annotation = NULL, column_annotation_side = "top", column_annotation_palette = "Paired", column_annotation_palcolor = NULL,
    column_annotation_type = "auto", column_annotation_params = list(), column_annotation_agg = NULL,
    row_annotation = NULL, row_annotation_side = "left", row_annotation_palette = "Paired", row_annotation_palcolor = NULL,
    row_annotation_type = "auto", row_annotation_params = list(), row_annotation_agg = NULL,
    # misc
    flip = FALSE, alpha = 1, seed = 8525, padding = 15, base_size = 1, aspect.ratio = NULL, draw_opts = list(),
    # cell customization
    layer_fun_callback = NULL, cell_type = "tile", cell_agg = NULL,
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

    .check_annotation(data, row_annotation, rows_by, rows_split_by, which = "row")
    .check_annotation(data, column_annotation, rows_by, rows_split_by, which = "column")

    row_annotation_params   <- .resolve_anno_aliases(row_annotation_params,   rows_by, rows_split_by, columns_by, columns_split_by)
    row_annotation_palette  <- .resolve_anno_aliases(row_annotation_palette,  rows_by, rows_split_by, columns_by, columns_split_by)
    row_annotation_palcolor <- .resolve_anno_aliases(row_annotation_palcolor, rows_by, rows_split_by, columns_by, columns_split_by)
    row_annotation_type     <- .resolve_anno_aliases(row_annotation_type,     rows_by, rows_split_by, columns_by, columns_split_by)
    row_annotation_agg      <- .resolve_anno_aliases(row_annotation_agg,      rows_by, rows_split_by, columns_by, columns_split_by)

    column_annotation_params   <- .resolve_anno_aliases(column_annotation_params,    rows_by, rows_split_by, columns_by, columns_split_by)
    column_annotation_palette  <- .resolve_anno_aliases(column_annotation_palette,   rows_by, rows_split_by, columns_by, columns_split_by)
    column_annotation_palcolor <- .resolve_anno_aliases(column_annotation_palcolor,  rows_by, rows_split_by, columns_by, columns_split_by)
    column_annotation_type     <- .resolve_anno_aliases(column_annotation_type,      rows_by, rows_split_by, columns_by, columns_split_by)
    column_annotation_agg      <- .resolve_anno_aliases(column_annotation_agg,       rows_by, rows_split_by, columns_by, columns_split_by)
    row_annotation_side        <- .resolve_anno_aliases(row_annotation_side,         rows_by, rows_split_by, columns_by, columns_split_by)
    column_annotation_side     <- .resolve_anno_aliases(column_annotation_side,      rows_by, rows_split_by, columns_by, columns_split_by)

    # Determine whether the name annotations are enabled.
    # A params entry of FALSE disables, but type "label" takes precedence
    # (setting the type implies the user wants the annotation visible).
    row_name_anno_enabled <- !is.null(rows_by) && (
        !isFALSE(row_annotation_params[[rows_by]] %||% TRUE) ||
        (!identical(row_annotation_type, "auto") && row_annotation_type[[rows_by]] %||% "simple" == "label")
    )
    col_name_anno_enabled <- !is.null(columns_by) && (
        !isFALSE(column_annotation_params[[columns_by]] %||% TRUE) ||
        (!identical(column_annotation_type, "auto") && column_annotation_type[[columns_by]] %||% "simple" == "label")
    )
    show_row_names    <- show_row_names    %||% !row_name_anno_enabled
    show_column_names <- show_column_names %||% !col_name_anno_enabled

    # Convert to the format that ComplexHeatmap::Heatmap can understand
    if (is.null(row_title) && !is.null(rows_split_by) && !is.null(row_annotation_type)) {
        # Let's check if we need row_title
        if (identical(row_annotation_type, "auto")) {
            row_title <- TRUE
        } else {
            row_title <- identical(row_annotation_type[[rows_split_by]] %||% "simple", "simple")
        }
    }
    if (is.null(column_title) && !is.null(columns_split_by) && !is.null(column_annotation_type)) {
        if (identical(column_annotation_type, "auto")) {
            column_title <- TRUE
        } else {
            column_title <- identical(column_annotation_type[[columns_split_by]] %||% "simple", "simple")
        }
    }
    if (isFALSE(column_title)) column_title <- NULL
    if (isFALSE(row_title)) row_title <- NULL
    if (isTRUE(column_title)) column_title <- character(0)
    if (isTRUE(row_title)) row_title <- character(0)
    stopifnot("[Heatmap] 'padding' should be a numeric vector of length 1, 2, 3, or 4." = length(padding) %in% 1:4)

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
            palette_this(palette = palette, palcolor = palcolor, alpha = a, reverse = palreverse, transparent = FALSE)
        )
    }

    flip_side <- function(side) {
        match.arg(side, c("left", "right", "top", "bottom"))
        if (side == "left") return("top")
        if (side == "right") return("bottom")
        if (side == "top") return("left")
        if (side == "bottom") return("right")
    }

    # Initialize the heatmap arguments
    hmargs <- list(
        # name = name,   # error when name has irregular characters (e.g. "-")
        heatmap_legend_param = list(title = values_by),
        border = border, na_col = na_col,
        cluster_columns = if (flip) cluster_rows else cluster_columns,
        cluster_rows = if (flip) cluster_columns else cluster_rows,
        cluster_column_slices = FALSE, cluster_row_slices = FALSE, show_heatmap_legend = FALSE,
        show_row_names = if (flip) show_column_names else show_row_names,
        show_column_names = if (flip) show_row_names else show_column_names,
        row_names_side = if (flip) flip_side(column_names_side) else row_names_side,
        column_names_side = if (flip) flip_side(row_names_side) else column_names_side,
        column_title = column_title, row_title = row_title,
        ...
    )

    # Set the row_names_max_width based on the length of the row names
    if (!is.null(hmargs$row_names_max_width)) {
        if (is.character(hmargs$row_names_max_width)) {
            if (grepl("^[0-9]+(\\.[0-9]+)?$", hmargs$row_names_max_width)) {
                hmargs$row_names_max_width <- unit(as.numeric(hmargs$row_names_max_width), "inches")
            }
            stopifnot(
                "[Heatmap] 'row_names_max_width' should be in a format of '2inches' or '2cm' if given as a string." =
                grepl("^[0-9]+(\\.[0-9]+\\s*)?(npc|cm|centimetre|centimeter|in|inch|inches|mm|points|picas|bigpts|cicero|scalepts|lines|char|native|snpc|strwidth|strheight|grobwidth|grobheight|null)$", hmargs$row_names_max_width)
            )

            # convert to grid::unit
            hmargs$row_names_max_width <- unit(
                as.numeric(gsub("[^0-9.]", "", hmargs$row_names_max_width)),
                gsub("[0-9.]", "", hmargs$row_names_max_width)
            )
        } else if (is.numeric(hmargs$row_names_max_width)) {
            hmargs$row_names_max_width <- unit(hmargs$row_names_max_width, "inches")
        }
    } else if (flip) {
        hmargs$row_names_max_width <- ComplexHeatmap::max_text_width(levels(data[[columns_by]]))
    } else {
        hmargs$row_names_max_width <- ComplexHeatmap::max_text_width(levels(data[[rows_by]]))
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
        group_by(!!!syms(unique(c(rows_split_by, rows_by, columns_split_by, columns_by)))) %>%
        summarise(.value = cell_agg(!!sym(values_by)), .groups = "drop") %>%
        unite(".columns", !!!syms(unique(c(columns_split_by, columns_by))), sep = " // ") %>%
        unite(".rows", !!!syms(unique(c(rows_split_by, rows_by))), sep = " // ") %>%
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
        unite(".columns", !!!syms(unique(c(columns_split_by, columns_by))), sep = " // ") %>%
        dplyr::pull(".columns") %>%
        unique() %>%
        intersect(colnames(hmargs$matrix))
    rows_order <- data %>%
        tidyr::expand(!!!syms(unique(c(rows_split_by, rows_by)))) %>%
        unite(".rows", !!!syms(unique(c(rows_split_by, rows_by))), sep = " // ") %>%
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
        row_split_labels <- strsplit(rownames(hmargs$matrix), " // ", fixed = TRUE)
        hmargs$row_split <- factor(sapply(row_split_labels, `[`, 1), levels = r_split_levels)
        hmargs$row_labels <- factor(sapply(row_split_labels, `[`, 2), levels = r_levels)
    } else {
        hmargs$row_labels <- factor(rownames(hmargs$matrix), levels = r_levels)
    }

    if (!is.null(c_split_by)) {
        column_split_labels <- strsplit(colnames(hmargs$matrix), " // ", fixed = TRUE)
        hmargs$column_split <- factor(sapply(column_split_labels, `[`, 1), levels = c_split_levels)
        hmargs$column_labels <- factor(sapply(column_split_labels, `[`, 2), levels = c_levels)
    } else {
        hmargs$column_labels <- factor(colnames(hmargs$matrix), levels = c_levels)
    }

    if (cell_type == "bars") { # where multiple values are used, operating on data
        lower_cutoff <- lower_cutoff %||% quantile(data[[values_by]][is.finite(data[[values_by]])], lower_quantile, na.rm = TRUE)
        upper_cutoff <- upper_cutoff %||% quantile(data[[values_by]][is.finite(data[[values_by]])], upper_quantile, na.rm = TRUE)
        data[[values_by]][data[[values_by]] < lower_cutoff] <- lower_cutoff
        data[[values_by]][data[[values_by]] > upper_cutoff] <- upper_cutoff
    } else { # where aggregated values are used
        lower_cutoff <- lower_cutoff %||% quantile(hmargs$matrix[is.finite(hmargs$matrix)], lower_quantile, na.rm = TRUE)
        upper_cutoff <- upper_cutoff %||% quantile(hmargs$matrix[is.finite(hmargs$matrix)], upper_quantile, na.rm = TRUE)
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
                lgd_items <- sort(unique(as.vector(hmargs$matrix)), decreasing = TRUE)
                names(lgd_items) <- as.character(lgd_items)
            } else {
                lgd_items <- unlist(legend_items)
            }
            ComplexHeatmap::Legend(
                title = values_by, at = lgd_items, labels = names(lgd_items),
                legend_gp = grid::gpar(fill = hmargs$col(lgd_items)), border = TRUE, direction = legend.direction
            )
        } else {
            ComplexHeatmap::Legend(title = values_by, col_fun = hmargs$col, border = TRUE, direction = legend.direction)
        }
    }

    if (cell_type == "pie") {
        if (is.null(pie_group_by)) {
            stop("[Heatmap] Please provide 'pie_group_by' to use 'cell_type = 'pie'.")
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
        keep_empty_pie_group <- if (isFALSE(keep_empty)) FALSE else keep_empty[[pie_group_by]]
        pie_group_levels <- levels(data[[pie_group_by]])
        if (anyNA(data[[pie_group_by]])) pie_group_levels <- c(pie_group_by, NA)
        pie_data <- data %>%
            group_by(!!!syms(unique(c(rows_split_by, rows_by, columns_split_by, columns_by, pie_group_by))), .drop = FALSE) %>%
            summarise(.value = pie_values(!!sym(values_by)), .groups = "drop") %>%
            group_by(!!!syms(unique(c(rows_split_by, rows_by, columns_split_by, columns_by)))) %>%
            group_map(
                ~ data.frame(Var = .x[[pie_group_by]], Freq = .x$.value)
            )
        names(pie_data) <- indices

        pie_colors <- palette_this(pie_group_levels, palette = pie_palette, palcolor = pie_palcolor, NA_keep = TRUE)
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
                j, i, x, y, w, h, fill,
                palette = pie_palette, palcolor = pie_palcolor, data = pie_data, pie_size = pie_size
            )
            if (is.function(layer_fun_callback)) {
                layer_fun_callback(j, i, x, y, w, h, fill, sr, sc)
            }
        }

        if (!identical(legend.position, "none") && is.function(pie_size)) {
            pie_sizes <- sapply(pie_data, function(d) pie_size(sum(d$Freq, na.rm = TRUE)))
            pie_size_min <- min(pie_sizes, na.rm = TRUE)
            pie_size_max <- max(pie_sizes, na.rm = TRUE)
            legends$.pie_size <- ComplexHeatmap::Legend(
                title = pie_size_name,
                labels = scales::number((seq(pie_size_min, pie_size_max, length.out = ifelse(pie_size_max > pie_size_min, 4, 1)))),
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
                title = pie_group_by, direction = legend.direction,
                border = TRUE, labels = pie_group_levels, legend_gp = gpar(fill = pie_colors)
            )
        }
    }
    else if (cell_type == "bars") {
        if (isTRUE(add_bg)) {
            stop("[Heatmap] Cannot use 'add_bg' with 'cell_type = 'bars'.")
        }
        if (isTRUE(add_reticle)) {
            stop("[Heatmap] Cannot use 'add_reticle' with 'cell_type = 'bars'.")
        }

        bars_data <- data %>%
            group_by(!!!syms(unique(c(rows_split_by, rows_by, columns_split_by, columns_by)))) %>%
            group_map(~ .x[[values_by]])

        names(bars_data) <- indices

        # plot bars in each cell
        hmargs$layer_fun <- function(j, i, x, y, w, h, fill, sr, sc) {
            layer_white_bg(j, i, x, y, w, h, fill)
            layer_bars(
                j, i, x, y, w, h, fill, flip = flip,
                col_fun = hmargs$col, data = bars_data, alpha = alpha
            )
            if (is.function(layer_fun_callback)) {
                layer_fun_callback(j, i, x, y, w, h, fill, sr, sc)
            }
        }
        # Override the main legend
        legends$.heatmap <- get_main_legend(FALSE)
    }
    else if (cell_type == "dot") {
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
            group_by(!!!syms(unique(c(rows_split_by, rows_by, columns_split_by, columns_by)))) %>%
            summarise(.value = list(!!sym(values_by)), .groups = "drop") %>%
            unite(".columns", !!!syms(unique(c(columns_split_by, columns_by))), sep = " // ") %>%
            unite(".rows", !!!syms(unique(c(rows_split_by, rows_by))), sep = " // ") %>%
            pivot_wider(names_from = ".columns", values_from = ".value") %>%
            select(-!!sym(".rows")) %>%
            as.data.frame()

        if (flip) {
            dot_data <- t(dot_data)
        }

        if (!identical(legend.position, "none") && is.function(dot_size) && !is.null(dot_size_name)) {
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
                    dot_size(cell_values, ij[1], ij[2],
                             rownames(hmargs$matrix)[ij[1]],
                             colnames(hmargs$matrix)[ij[2]])
                } else {
                    stop("[Heatmap] 'dot_size' function should take 1, 3 or 5 arguments.")
                }

                if (is.finite(size_val)) {
                    if (size_val < dot_size_min) dot_size_min <- size_val
                    if (size_val > dot_size_max) dot_size_max <- size_val
                }
            }

            legends$.dot_size <- ComplexHeatmap::Legend(
                title = dot_size_name,
                labels = scales::number((seq(dot_size_min, dot_size_max, length.out = ifelse(dot_size_max > dot_size_min, 4, 1)))),
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
                        sizes[idx] <- dot_size(cell_values, i[idx], j[idx],
                                               rownames(hmargs$matrix)[i[idx]],
                                               colnames(hmargs$matrix)[j[idx]])
                    } else {
                        stop("[Heatmap] 'dot_size' function should take 1, 3 or 5 arguments.")
                    }
                }
                layer_dot(
                    j, i, x, y, w, h, fill,
                    data = dot_data, dot_size = sizes, alpha = alpha
                )
            } else {
                layer_dot(
                    j, i, x, y, w, h, fill,
                    data = dot_data, dot_size = dot_size, alpha = alpha
                )
            }
            if (is.function(layer_fun_callback)) {
                layer_fun_callback(j, i, x, y, w, h, fill, sr, sc)
            }
        }
        # Override the main legend
        legends$.heatmap <- get_main_legend()
    }
    else if (cell_type %in% c("violin", "boxplot")) {
        # df with multiple values in each cell
        vdata <- data %>%
            group_by(!!!syms(unique(c(rows_split_by, rows_by, columns_split_by, columns_by)))) %>%
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
                j, i, x, y, w, h, fill,
                flip = flip, data = vdata, colors = vcolors, fn = layer_fn
            )
            if (is.function(layer_fun_callback)) {
                layer_fun_callback(j, i, x, y, w, h, fill, sr, sc)
            }
        }
        if (!identical(legend.position, "none")) {
            if (!is.null(legend_items)) {
                stop("[Heatmap] Cannot use 'legend_items' with 'cell_type = 'violin' or 'boxplot'.")
            }
            if (isTRUE(legend_discrete)) {
                stop("[Heatmap] Cannot use 'legend_discrete = TRUE' with 'cell_type = 'violin' or 'boxplot'.")
            }
            if (is.null(vcolors)) {
                legends$.heatmap <- ComplexHeatmap::Legend(
                    title = values_by, col_fun = hmargs$col, border = TRUE,
                    direction = legend.direction
                )
            } else if (isTRUE(add_bg)) {
                legends$.heatmap <- ComplexHeatmap::Legend(
                    title = values_by, col_fun = get_col_fun(lower_cutoff, upper_cutoff, bg_alpha),
                    border = TRUE, direction = legend.direction
                )
            }
        }
    }
    else if (cell_type == "tile") {
        if (isTRUE(add_bg)) {
            stop("[Heatmap] Cannot use 'add_bg' with 'cell_type = 'tile'.")
        }
        if (isTRUE(add_reticle)) {
            stop("[Heatmap] Cannot use 'add_reticle' with 'cell_type = 'tile'.")
        }
        hmargs$rect_gp <- gpar(col = "grey80", lwd = 0.1)
        hmargs$layer_fun <- layer_fun_callback
        legends$.heatmap <- get_main_legend()
    }
    else if (cell_type == "label") {
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
                stop("[Heatmap] 'label' function should take 1, 3 or 5 arguments.")
            }
        } else {
            function(val, ri, ci) val
        }

        # Helper: extract label text, supporting unnamed first list element
        .extract_lbl <- function(r) {
            if (!is.list(r)) return(if (length(r) == 1 && is.na(r)) NA_character_ else as.character(r))
            if (!is.null(r$label)) return(as.character(r$label))
            nms <- names(r)
            unnamed_idx <- which(is.null(nms) | nms == "")
            if (length(unnamed_idx) > 0) return(as.character(r[[unnamed_idx[1]]]))
            NA_character_
        }

        # Pre-compute over whole matrix to auto-detect legend entries
        if (!identical(legend.position, "none")) {
            lgnd_seen <- list()  # named list: legend_key -> list(text, color)
            for (ri in seq_len(nrow(hmargs$matrix))) {
                for (ci in seq_len(ncol(hmargs$matrix))) {
                    r <- .call_label(hmargs$matrix[ri, ci], ri, ci)
                    if (is.list(r) && !is.null(r$legend) && !is.na(r$legend)) {
                        key <- r$legend
                        if (is.null(lgnd_seen[[key]])) {
                            lbl_txt <- .extract_lbl(r)
                            lgnd_seen[[key]] <- list(
                                text  = if (!is.na(lbl_txt)) lbl_txt else key,
                                color = r$color %||% label_color,
                                order = r$order %||% NA_integer_
                            )
                        }
                    }
                }
            }

            if (length(lgnd_seen) > 0) {
                # Sort by order field if any entries carry it
                orders <- sapply(lgnd_seen, function(e) e$order %||% NA_integer_)
                if (!all(is.na(orders))) {
                    # Entries without order are placed after explicitly ordered ones
                    orders[is.na(orders)] <- max(orders, na.rm = TRUE) + seq_len(sum(is.na(orders)))
                    lgnd_seen <- lgnd_seen[order(orders)]
                }
                lgnd_graphics <- lapply(lgnd_seen, function(entry) {
                    txt <- entry$text
                    col <- entry$color
                    sz  <- label_size
                    function(x, y, w, h, fill) {
                        grid.text(txt, x, y, gp = gpar(fontsize = sz, col = col, fontface = "bold"))
                    }
                })
                legends$.label <- ComplexHeatmap::Legend(
                    title    = label_name,
                    labels   = names(lgnd_seen),
                    graphics = lgnd_graphics,
                    direction = legend.direction
                )
            }
        }

        hmargs$layer_fun <- function(j, i, x, y, w, h, fill, sr, sc) {
            raw_vals <- ComplexHeatmap::pindex(hmargs$matrix, i, j)
            results <- lapply(seq_along(i), function(k) .call_label(raw_vals[k], i[k], j[k]))

            lbl   <- sapply(results, .extract_lbl)
            sizes  <- sapply(results, function(r) if (is.list(r)) r$size  %||% label_size else label_size)
            colors <- sapply(results, function(r) if (is.list(r)) r$color %||% label_color else label_color)

            inds <- which(!is.na(lbl))
            if (length(inds) > 0) {
                theta <- seq(pi / 8, 2 * pi, length.out = 16)
                for (k in inds) {
                    sz <- sizes[k]
                    lapply(theta, function(a) {
                        grid.text(lbl[k],
                            x = x[k] + unit(cos(a) * sz / 30, "mm"),
                            y = y[k] + unit(sin(a) * sz / 30, "mm"),
                            gp = gpar(fontsize = sz, col = "white"))
                    })
                    grid.text(lbl[k], x[k], y[k], gp = gpar(fontsize = sz, col = colors[k]))
                }
            }
            if (is.function(layer_fun_callback)) {
                layer_fun_callback(j, i, x, y, w, h, fill, sr, sc)
            }
        }
        legends$.heatmap <- get_main_legend()
    }
    else if (cell_type == "mark") {
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
                stop("[Heatmap] 'mark' function should take 1, 3 or 5 arguments.")
            }
        } else {
            function(val, ri, ci) val
        }

        # ── Extract mark type string ────────────────────────────────────────
        .extract_mark <- function(r) {
            if (!is.list(r)) return(if (length(r) == 1 && is.na(r)) NA_character_ else as.character(r))
            if (!is.null(r$mark)) return(as.character(r$mark))
            nms <- names(r)
            unnamed_idx <- which(is.null(nms) | nms == "")
            if (length(unnamed_idx) > 0) return(as.character(r[[unnamed_idx[1]]]))
            NA_character_
        }

        # ── Mark type parser ────────────────────────────────────────────────
        # Parses a mark string into a vector of drawing primitives.
        # Outer wrappers: [] adds "rect", () adds "circle_full", <> adds "diamond".
        # Wrappers are stripped left-to-right; the remainder is an inner primitive.
        # Inner primitives: -, |, +, /, \, x, o, (), <>
        .parse_mark_type <- function(m) {
            if (is.na(m) || m == "" || m == "NA") return(character(0))
            prims <- character(0)
            repeat {
                if (nchar(m) >= 2 && startsWith(m, "[") && endsWith(m, "]")) {
                    prims <- c(prims, "rect")
                    m <- substr(m, 2L, nchar(m) - 1L)
                } else if (nchar(m) >= 3 && startsWith(m, "(") && endsWith(m, ")")) {
                    # "(-)" wrapper, but "()" alone is a primitive — handled below
                    prims <- c(prims, "circle_full")
                    m <- substr(m, 2L, nchar(m) - 1L)
                } else if (nchar(m) >= 3 && startsWith(m, "<") && endsWith(m, ">")) {
                    # "<->" wrapper, but "<>" alone is a primitive — handled below
                    prims <- c(prims, "diamond")
                    m <- substr(m, 2L, nchar(m) - 1L)
                } else if (nchar(m) >= 3 && startsWith(m, "{") && endsWith(m, "}")) {
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
                switch(m,
                    "-"  = "hline",
                    "|"  = "vline",
                    "+"  = c("hline", "vline"),
                    "/"  = "ldiag",
                    "\\" = "rdiag",
                    "x"  = c("ldiag", "rdiag"),
                    "o"  = "circle_gap",
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
            switch(prim,
                rect = grid::grid.rect(
                    x = xv, y = yv, width = wv, height = hv,
                    gp = gpar(col = col, fill = NA, lwd = lwd)),

                hline = grid::grid.segments(
                    x0 = xv - wv * 0.5, y0 = yv,
                    x1 = xv + wv * 0.5, y1 = yv,
                    gp = gpar(col = col, lwd = lwd)),

                vline = grid::grid.segments(
                    x0 = xv, y0 = yv - hv * 0.5,
                    x1 = xv, y1 = yv + hv * 0.5,
                    gp = gpar(col = col, lwd = lwd)),

                ldiag = grid::grid.segments(
                    x0 = xv - wv * 0.5, y0 = yv - hv * 0.5,
                    x1 = xv + wv * 0.5, y1 = yv + hv * 0.5,
                    gp = gpar(col = col, lwd = lwd)),

                rdiag = grid::grid.segments(
                    x0 = xv - wv * 0.5, y0 = yv + hv * 0.5,
                    x1 = xv + wv * 0.5, y1 = yv - hv * 0.5,
                    gp = gpar(col = col, lwd = lwd)),

                # circle: diameter = 3/4 of the shorter physical edge, true circle
                # Use mm so the radius is invariant to viewport aspect ratio
                circle_gap = {
                    .r_mm <- min(
                        convertUnit(wv[1L], "mm", valueOnly = TRUE),
                        convertUnit(hv[1L], "mm", valueOnly = TRUE)
                    ) * 0.25
                    grid::grid.circle(
                        x = xv, y = yv,
                        r = unit(.r_mm, "mm"),
                        gp = gpar(col = col, fill = NA, lwd = lwd))
                },

                # ellipse touching all 4 cell edges (no gap), handles non-square cells
                circle_full = {
                    .theta  <- seq(0, 2 * pi, length.out = 65L)[seq_len(64L)]
                    .cos_t  <- cos(.theta)
                    .sin_t  <- sin(.theta)
                    for (k in seq_along(xv)) {
                        grid::grid.polygon(
                            x = xv[k] + wv[k] * 0.5 * .cos_t,
                            y = yv[k] + hv[k] * 0.5 * .sin_t,
                            gp = gpar(
                                col  = if (length(col) == 1L) col else col[k],
                                fill = NA,
                                lwd  = if (length(lwd) == 1L) lwd else lwd[k]))
                    }
                },

                # diamond — loop per cell (grid.polygon id approach avoids this)
                diamond = {
                    n <- length(xv)
                    hw <- wv * 0.5; hh <- hv * 0.5
                    for (k in seq_len(n)) {
                        grid::grid.polygon(
                            x = grid::unit.c(xv[k], xv[k] + hw[k], xv[k], xv[k] - hw[k]),
                            y = grid::unit.c(yv[k] + hh[k], yv[k], yv[k] - hh[k], yv[k]),
                            gp = gpar(col = col[k], fill = NA, lwd = lwd[k]))
                    }
                },

                # octagon: 4 sides on the cell edges, 4 small corner cuts (cut factor = 0.2)
                # The corner cuts are closer to the actual corners than a regular octagon would be
                octagon = {
                    n  <- length(xv)
                    hw <- wv * 0.5; hh <- hv * 0.5
                    f  <- 0.2  # corner cut factor (0 = full rectangle, 0.5 = diamond)
                    for (k in seq_len(n)) {
                        cx <- xv[k]; cy <- yv[k]
                        dx <- hw[k] * f; dy <- hh[k] * f
                        grid::grid.polygon(
                            x = grid::unit.c(
                                cx - hw[k] + dx, cx + hw[k] - dx,  # top edge: left → right
                                cx + hw[k],       cx + hw[k],       # right edge: top → bottom
                                cx + hw[k] - dx, cx - hw[k] + dx,  # bottom edge: right → left
                                cx - hw[k],       cx - hw[k]        # left edge: bottom → top
                            ),
                            y = grid::unit.c(
                                cy + hh[k],       cy + hh[k],       # top edge
                                cy + hh[k] - dy,  cy - hh[k] + dy,  # right edge
                                cy - hh[k],       cy - hh[k],       # bottom edge
                                cy - hh[k] + dy,  cy + hh[k] - dy   # left edge
                            ),
                            gp = gpar(col = col[k], fill = NA, lwd = lwd[k]))
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
                                mark  = if (!is.na(mk_str)) mk_str else key,
                                color = r$color %||% mark_color,
                                size  = r$size  %||% mark_size,
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
                    orders[is.na(orders)] <- max(orders, na.rm = TRUE) + seq_len(sum(is.na(orders)))
                    mkl_seen <- mkl_seen[order(orders)]
                }
                mkl_graphics <- lapply(mkl_seen, function(entry) {
                    .mark_legend_icon(entry$mark, entry$color, entry$size)
                })
                legends$.mark <- ComplexHeatmap::Legend(
                    title       = mark_name,
                    labels      = names(mkl_seen),
                    graphics    = mkl_graphics,
                    direction   = legend.direction,
                    row_gap     = unit(1, "mm")
                )
            }
        }

        # ── layer_fun ───────────────────────────────────────────────────────
        hmargs$layer_fun <- function(j, i, x, y, w, h, fill, sr, sc) {
            raw_vals <- ComplexHeatmap::pindex(hmargs$matrix, i, j)
            results  <- lapply(seq_along(i), function(k) .call_mark(raw_vals[k], i[k], j[k]))

            marks_vec  <- sapply(results, .extract_mark)
            colors_vec <- sapply(results, function(r) if (is.list(r)) r$color %||% mark_color else mark_color)
            sizes_vec  <- sapply(results, function(r) if (is.list(r)) r$size  %||% mark_size  else mark_size)

            # Draw each unique mark type as a batch
            unique_marks <- unique(marks_vec[!is.na(marks_vec)])
            for (mk in unique_marks) {
                km   <- which(marks_vec == mk)
                prms <- .parse_mark_type(mk)
                for (prim in prms) {
                    .draw_mark_prim(prim, x[km], y[km], w[km], h[km], colors_vec[km], sizes_vec[km])
                }
            }

            if (is.function(layer_fun_callback)) {
                layer_fun_callback(j, i, x, y, w, h, fill, sr, sc)
            }
        }
        legends$.heatmap <- get_main_legend()
    }
    else if (cell_type == "label+mark") {
        if (isTRUE(add_bg)) {
            stop("[Heatmap] Cannot use 'add_bg' with 'cell_type = \"label+mark\"'.")
        }
        if (isTRUE(add_reticle)) {
            stop("[Heatmap] Cannot use 'add_reticle' with 'cell_type = \"label+mark\"'.")
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
                stop("[Heatmap] 'label' function should take 1, 3 or 5 arguments.")
            }
        } else {
            function(val, ri, ci) val
        }

        # Helper: extract label text, supporting unnamed first list element
        .extract_lbl <- function(r) {
            if (!is.list(r)) return(if (length(r) == 1 && is.na(r)) NA_character_ else as.character(r))
            if (!is.null(r$label)) return(as.character(r$label))
            nms <- names(r)
            unnamed_idx <- which(is.null(nms) | nms == "")
            if (length(unnamed_idx) > 0) return(as.character(r[[unnamed_idx[1]]]))
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
                stop("[Heatmap] 'mark' function should take 1, 3 or 5 arguments.")
            }
        } else {
            function(val, ri, ci) val
        }

        # ── Extract mark type string ────────────────────────────────────────
        .extract_mark <- function(r) {
            if (!is.list(r)) return(if (length(r) == 1 && is.na(r)) NA_character_ else as.character(r))
            if (!is.null(r$mark)) return(as.character(r$mark))
            nms <- names(r)
            unnamed_idx <- which(is.null(nms) | nms == "")
            if (length(unnamed_idx) > 0) return(as.character(r[[unnamed_idx[1]]]))
            NA_character_
        }

        # ── Mark type parser ────────────────────────────────────────────────
        .parse_mark_type <- function(m) {
            if (is.na(m) || m == "" || m == "NA") return(character(0))
            prims <- character(0)
            repeat {
                if (nchar(m) >= 2 && startsWith(m, "[") && endsWith(m, "]")) {
                    prims <- c(prims, "rect")
                    m <- substr(m, 2L, nchar(m) - 1L)
                } else if (nchar(m) >= 3 && startsWith(m, "(") && endsWith(m, ")")) {
                    prims <- c(prims, "circle_full")
                    m <- substr(m, 2L, nchar(m) - 1L)
                } else if (nchar(m) >= 3 && startsWith(m, "<") && endsWith(m, ">")) {
                    prims <- c(prims, "diamond")
                    m <- substr(m, 2L, nchar(m) - 1L)
                } else if (nchar(m) >= 3 && startsWith(m, "{") && endsWith(m, "}")) {
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
                switch(m,
                    "-"  = "hline",
                    "|"  = "vline",
                    "+"  = c("hline", "vline"),
                    "/"  = "ldiag",
                    "\\" = "rdiag",
                    "x"  = c("ldiag", "rdiag"),
                    "o"  = "circle_gap",
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
            switch(prim,
                rect = grid::grid.rect(
                    x = xv, y = yv, width = wv, height = hv,
                    gp = gpar(col = col, fill = NA, lwd = lwd)),

                hline = grid::grid.segments(
                    x0 = xv - wv * 0.5, y0 = yv,
                    x1 = xv + wv * 0.5, y1 = yv,
                    gp = gpar(col = col, lwd = lwd)),

                vline = grid::grid.segments(
                    x0 = xv, y0 = yv - hv * 0.5,
                    x1 = xv, y1 = yv + hv * 0.5,
                    gp = gpar(col = col, lwd = lwd)),

                ldiag = grid::grid.segments(
                    x0 = xv - wv * 0.5, y0 = yv - hv * 0.5,
                    x1 = xv + wv * 0.5, y1 = yv + hv * 0.5,
                    gp = gpar(col = col, lwd = lwd)),

                rdiag = grid::grid.segments(
                    x0 = xv - wv * 0.5, y0 = yv + hv * 0.5,
                    x1 = xv + wv * 0.5, y1 = yv - hv * 0.5,
                    gp = gpar(col = col, lwd = lwd)),

                circle_gap = {
                    .r_mm <- min(
                        convertUnit(wv[1L], "mm", valueOnly = TRUE),
                        convertUnit(hv[1L], "mm", valueOnly = TRUE)
                    ) * 0.25
                    grid::grid.circle(
                        x = xv, y = yv,
                        r = unit(.r_mm, "mm"),
                        gp = gpar(col = col, fill = NA, lwd = lwd))
                },

                circle_full = {
                    .theta  <- seq(0, 2 * pi, length.out = 65L)[seq_len(64L)]
                    .cos_t  <- cos(.theta)
                    .sin_t  <- sin(.theta)
                    for (k in seq_along(xv)) {
                        grid::grid.polygon(
                            x = xv[k] + wv[k] * 0.5 * .cos_t,
                            y = yv[k] + hv[k] * 0.5 * .sin_t,
                            gp = gpar(
                                col  = if (length(col) == 1L) col else col[k],
                                fill = NA,
                                lwd  = if (length(lwd) == 1L) lwd else lwd[k]))
                    }
                },

                diamond = {
                    n <- length(xv)
                    hw <- wv * 0.5; hh <- hv * 0.5
                    for (k in seq_len(n)) {
                        grid::grid.polygon(
                            x = grid::unit.c(xv[k], xv[k] + hw[k], xv[k], xv[k] - hw[k]),
                            y = grid::unit.c(yv[k] + hh[k], yv[k], yv[k] - hh[k], yv[k]),
                            gp = gpar(col = col[k], fill = NA, lwd = lwd[k]))
                    }
                },

                # octagon: 4 sides on the cell edges, 4 small corner cuts (cut factor = 0.2)
                # The corner cuts are closer to the actual corners than a regular octagon would be
                octagon = {
                    n  <- length(xv)
                    hw <- wv * 0.5; hh <- hv * 0.5
                    f  <- 0.5  # corner cut factor (0 = full rectangle, 0.5 = diamond)
                    for (k in seq_len(n)) {
                        cx <- xv[k]; cy <- yv[k]
                        dx <- hw[k] * f; dy <- hh[k] * f
                        grid::grid.polygon(
                            x = grid::unit.c(
                                cx - hw[k] + dx, cx + hw[k] - dx,  # top edge: left → right
                                cx + hw[k],       cx + hw[k],       # right edge: top → bottom
                                cx + hw[k] - dx, cx - hw[k] + dx,  # bottom edge: right → left
                                cx - hw[k],       cx - hw[k]        # left edge: bottom → top
                            ),
                            y = grid::unit.c(
                                cy + hh[k],       cy + hh[k],       # top edge
                                cy + hh[k] - dy,  cy - hh[k] + dy,  # right edge
                                cy - hh[k],       cy - hh[k],       # bottom edge
                                cy - hh[k] + dy,  cy + hh[k] - dy   # left edge
                            ),
                            gp = gpar(col = col[k], fill = NA, lwd = lwd[k]))
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
                                text  = if (!is.na(lbl_txt)) lbl_txt else key,
                                color = r$color %||% label_color,
                                order = r$order %||% NA_integer_
                            )
                        }
                    }
                }
            }

            if (length(lgnd_seen) > 0) {
                orders <- sapply(lgnd_seen, function(e) e$order %||% NA_integer_)
                if (!all(is.na(orders))) {
                    orders[is.na(orders)] <- max(orders, na.rm = TRUE) + seq_len(sum(is.na(orders)))
                    lgnd_seen <- lgnd_seen[order(orders)]
                }
                lgnd_graphics <- lapply(lgnd_seen, function(entry) {
                    txt <- entry$text
                    col <- entry$color
                    sz  <- label_size
                    function(x, y, w, h, fill) {
                        grid.text(txt, x, y, gp = gpar(fontsize = sz, col = col, fontface = "bold"))
                    }
                })
                legends$.label <- ComplexHeatmap::Legend(
                    title    = label_name,
                    labels   = names(lgnd_seen),
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
                                mark  = if (!is.na(mk_str)) mk_str else key,
                                color = r$color %||% mark_color,
                                size  = r$size  %||% mark_size,
                                order = r$order %||% NA_integer_
                            )
                        }
                    }
                }
            }

            if (length(mkl_seen) > 0) {
                orders <- sapply(mkl_seen, function(e) e$order %||% NA_integer_)
                if (!all(is.na(orders))) {
                    orders[is.na(orders)] <- max(orders, na.rm = TRUE) + seq_len(sum(is.na(orders)))
                    mkl_seen <- mkl_seen[order(orders)]
                }
                mkl_graphics <- lapply(mkl_seen, function(entry) {
                    .mark_legend_icon(entry$mark, entry$color, entry$size)
                })
                legends$.mark <- ComplexHeatmap::Legend(
                    title       = mark_name,
                    labels      = names(mkl_seen),
                    graphics    = mkl_graphics,
                    direction   = legend.direction,
                    row_gap     = unit(1, "mm")
                )
            }
        }

        # ── layer_fun: marks first (background), then labels (foreground) ───
        hmargs$layer_fun <- function(j, i, x, y, w, h, fill, sr, sc) {
            raw_vals <- ComplexHeatmap::pindex(hmargs$matrix, i, j)

            # Draw marks
            mk_results  <- lapply(seq_along(i), function(k) .call_mark(raw_vals[k], i[k], j[k]))
            marks_vec   <- sapply(mk_results, .extract_mark)
            mk_colors   <- sapply(mk_results, function(r) if (is.list(r)) r$color %||% mark_color else mark_color)
            mk_sizes    <- sapply(mk_results, function(r) if (is.list(r)) r$size  %||% mark_size  else mark_size)

            unique_marks <- unique(marks_vec[!is.na(marks_vec)])
            for (mk in unique_marks) {
                km   <- which(marks_vec == mk)
                prms <- .parse_mark_type(mk)
                for (prim in prms) {
                    .draw_mark_prim(prim, x[km], y[km], w[km], h[km], mk_colors[km], mk_sizes[km])
                }
            }

            # Draw labels on top
            lbl_results <- lapply(seq_along(i), function(k) .call_label(raw_vals[k], i[k], j[k]))
            lbl   <- sapply(lbl_results, .extract_lbl)
            sizes  <- sapply(lbl_results, function(r) if (is.list(r)) r$size  %||% label_size else label_size)
            colors <- sapply(lbl_results, function(r) if (is.list(r)) r$color %||% label_color else label_color)

            inds <- which(!is.na(lbl))
            if (length(inds) > 0) {
                theta <- seq(pi / 8, 2 * pi, length.out = 16)
                for (k in inds) {
                    sz <- sizes[k]
                    lapply(theta, function(a) {
                        grid.text(lbl[k],
                            x = x[k] + unit(cos(a) * sz / 30, "mm"),
                            y = y[k] + unit(sin(a) * sz / 30, "mm"),
                            gp = gpar(fontsize = sz, col = "white"))
                    })
                    grid.text(lbl[k], x[k], y[k], gp = gpar(fontsize = sz, col = colors[k]))
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
    cell_w <- switch(cell_type,
        violin      = 0.5,
        boxplot     = 0.5,
        pie         = 0.5,
        bars        = 0.35,
        label       = 0.6,
        mark        = 0.25,
        `label+mark` = 0.6,
        0.25  # tile, dot
    )
    cell_w <- cell_w * base_size

    aspect.ratio <- aspect.ratio %||% switch(cell_type,
        violin       = 2,    # taller to accommodate violin shape
        boxplot      = 2,    # slightly taller to accommodate boxplot shape
        pie          = 1,    # square for pie charts
        bars         = 0.5,  # wider-than-tall for bars
        label        = 0.6,  # shorter to fit text better
        mark         = 1,    # square for marks
        `label+mark` = 0.6,  # same as label
        1       # square by default for all other types
    )
    cell_h <- cell_w * aspect.ratio

    ncol_annos <- sum(cluster_columns, show_column_names) * 4
    ncol_annos <- ncol_annos +
        ifelse(is.null(columns_split_by) || isFALSE(column_annotation_params[[columns_split_by]]), 0, 1) +
        ifelse(!col_name_anno_enabled, 0, 1)
    res <- .setup_annos(
        which = "column", names_side = ifelse(flip, column_names_side, row_names_side),
        anno_title = column_title, show_names = show_column_names,
        annotation = column_annotation, annotation_type = column_annotation_type,
        annotation_side = column_annotation_side,
        annotation_palette = column_annotation_palette, annotation_palcolor = column_annotation_palcolor,
        annotation_agg = column_annotation_agg, annotation_params = column_annotation_params,
        split_by = columns_split_by, splits = if (flip) hmargs$row_split else hmargs$column_split,
        by = columns_by,
        by_labels = if (flip) hmargs$row_labels else hmargs$column_labels,
        flip = flip, legend.direction = legend.direction,
        legend.position = legend.position, data = data
    )
    column_annos <- res$annos
    legends <- c(legends, res$legends)
    # Dispatch annotations to sides based on per-annotation column_annotation_side
    all_keys <- names(column_annos)
    all_keys <- setdiff(all_keys, c("annotation_name_side", "show_annotation_name"))
    builtin_keys <- unique(c(columns_split_by, columns_by))
    side_list <- column_annotation_side  # already alias-resolved
    def_side <- if (is.list(side_list)) side_list[[".default"]] %||% "top" else "top"
    sna <- column_annos$show_annotation_name
    names_side <- column_annos$annotation_name_side
    top_annos <- list(annotation_name_side = names_side, show_annotation_name = list())
    bottom_annos <- list(annotation_name_side = names_side, show_annotation_name = list())
    for (k in all_keys) {
        if (is.list(side_list)) {
            side <- side_list[[k]] %||% def_side
        } else {
            side <- if (is.character(side_list)) side_list else def_side
        }
        if (side == "bottom") {
            bottom_annos[[k]] <- column_annos[[k]]
            if (!is.null(sna[[k]])) bottom_annos$show_annotation_name[[k]] <- sna[[k]]
        } else {
            top_annos[[k]] <- column_annos[[k]]
            if (!is.null(sna[[k]])) top_annos$show_annotation_name[[k]] <- sna[[k]]
        }
    }
    # Reorder: split annotation farthest from heatmap body, name annotation closest
    top_annos <- .reorder_anno_side(top_annos, columns_by, columns_split_by, side = "top")
    bottom_annos <- .reorder_anno_side(bottom_annos, columns_by, columns_split_by, side = "bottom")
    if (any(sapply(top_annos, inherits, "AnnotationFunction"))) {
        if (isTRUE(flip)) {
            hmargs$left_annotation <- do.call(ComplexHeatmap::rowAnnotation, top_annos)
        } else {
            hmargs$top_annotation <- do.call(ComplexHeatmap::HeatmapAnnotation, top_annos)
        }
    }
    if (any(sapply(bottom_annos, inherits, "AnnotationFunction"))) {
        if (isTRUE(flip)) {
            hmargs$right_annotation <- do.call(ComplexHeatmap::rowAnnotation, bottom_annos)
        } else {
            hmargs$bottom_annotation <- do.call(ComplexHeatmap::HeatmapAnnotation, bottom_annos)
        }
    }
    rm(top_annos, bottom_annos, column_annos)

    nrow_annos <- sum(cluster_rows, show_row_names) * 4
    nrow_annos <- nrow_annos +
        ifelse(is.null(rows_split_by) || isFALSE(row_annotation_params[[rows_split_by]]), 0, 1) +
        ifelse(!row_name_anno_enabled, 0, 1)
    res <- .setup_annos(
        which = "row", names_side = ifelse(flip, row_names_side, column_names_side),
        anno_title = row_title, show_names = show_row_names,
        annotation = row_annotation, annotation_type = row_annotation_type,
        annotation_side = row_annotation_side,
        annotation_palette = row_annotation_palette, annotation_palcolor = row_annotation_palcolor,
        annotation_agg = row_annotation_agg, annotation_params = row_annotation_params,
        split_by = rows_split_by, splits = if (flip) hmargs$column_split else hmargs$row_split,
        by = rows_by,
        by_labels = if (flip) hmargs$column_labels else hmargs$row_labels,
        flip = flip, legend.direction = legend.direction,
        legend.position = legend.position, data = data
    )
    row_annos <- res$annos
    legends <- c(legends, res$legends)
    # Dispatch annotations to sides based on per-annotation row_annotation_side
    all_keys <- names(row_annos)
    all_keys <- setdiff(all_keys, c("annotation_name_side", "show_annotation_name"))
    side_list <- row_annotation_side  # already alias-resolved
    def_side <- if (is.list(side_list)) side_list[[".default"]] %||% "left" else "left"
    sna <- row_annos$show_annotation_name
    names_side <- row_annos$annotation_name_side
    left_side <- list(annotation_name_side = names_side, show_annotation_name = list())
    right_side <- list(annotation_name_side = names_side, show_annotation_name = list())
    for (k in all_keys) {
        side <- if (is.list(side_list)) side_list[[k]] %||% def_side
                else if (is.character(side_list)) side_list else def_side
        if (side == "right") {
            right_side[[k]] <- row_annos[[k]]
            if (!is.null(sna[[k]])) right_side$show_annotation_name[[k]] <- sna[[k]]
        } else {
            left_side[[k]] <- row_annos[[k]]
            if (!is.null(sna[[k]])) left_side$show_annotation_name[[k]] <- sna[[k]]
        }
    }
    # Reorder: split annotation farthest from heatmap body, name annotation closest
    left_side <- .reorder_anno_side(left_side, rows_by, rows_split_by, side = "left")
    right_side <- .reorder_anno_side(right_side, rows_by, rows_split_by, side = "bottom")
    has_left  <- any(sapply(left_side, inherits, "AnnotationFunction"))
    has_right <- any(sapply(right_side, inherits, "AnnotationFunction"))
    # Fix: when row annotations are only on the right and row_names_side is "right",
    # ComplexHeatmap width() fails. Swap row_names_side to "left" to avoid the conflict.
    if (!has_left && has_right && !isTRUE(flip) && hmargs$row_names_side == "right") {
        hmargs$row_names_side <- "left"
    }
    if (has_left) {
        if (isTRUE(flip)) {
            hmargs$top_annotation <- do.call(ComplexHeatmap::HeatmapAnnotation, left_side)
        } else {
            hmargs$left_annotation <- do.call(ComplexHeatmap::rowAnnotation, left_side)
        }
    }
    if (has_right) {
        if (isTRUE(flip)) {
            hmargs$bottom_annotation <- do.call(ComplexHeatmap::HeatmapAnnotation, right_side)
        } else {
            hmargs$right_annotation <- do.call(ComplexHeatmap::rowAnnotation, right_side)
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
    if (!isTRUE(hmargs$show_row_names) &&
        hmargs$row_names_side == "right" &&
        legend.position == "right") {
        right_anno_names <- character(0)
        if (!isTRUE(flip)) {
            if (col_name_anno_enabled) {
                right_anno_names <- c(right_anno_names, columns_by)
            }
            if (!is.null(columns_split_by) && !isFALSE(column_annotation_params[[columns_split_by]])) {
                right_anno_names <- c(right_anno_names, columns_split_by)
            }
            if (!is.null(column_annotation) && length(column_annotation) > 0) {
                col_anno_names <- if (is.list(column_annotation)) names(column_annotation) else as.character(column_annotation)
                right_anno_names <- c(right_anno_names, col_anno_names)
            }
        } else {
            if (row_name_anno_enabled) {
                right_anno_names <- c(right_anno_names, rows_by)
            }
            if (!is.null(rows_split_by) && !isFALSE(row_annotation_params[[rows_split_by]])) {
                right_anno_names <- c(right_anno_names, rows_split_by)
            }
            if (!is.null(row_annotation) && length(row_annotation) > 0) {
                row_anno_names <- if (is.list(row_annotation)) names(row_annotation) else as.character(row_annotation)
                right_anno_names <- c(right_anno_names, row_anno_names)
            }
        }
        if (length(right_anno_names) > 0) {
            phantom_width <- ComplexHeatmap::max_text_width(right_anno_names)
            phantom_right_width_in <- convertUnit(phantom_width, "inches", valueOnly = TRUE)
            phantom_anno <- ComplexHeatmap::rowAnnotation(
                .gap = ComplexHeatmap::anno_empty(border = FALSE, width = phantom_width),
                show_annotation_name = FALSE
            )
            if (is.null(hmargs$right_annotation)) {
                hmargs$right_annotation <- phantom_anno
            } else {
                hmargs$right_annotation <- hmargs$right_annotation + phantom_anno
            }
        }
    }

    ## Set up the heatmap dimensions
    # Row names appear on left/right and add to width; only count when show_row_names is TRUE.
    # hmargs$show_row_names already accounts for flip (equals original show_column_names when
    # flip=TRUE, since original column labels become the row labels in the transposed matrix).
    # hmargs$row_names_max_width likewise accounts for flip (uses columns_by labels when flip=TRUE).
    rownames_width <- if (isTRUE(hmargs$show_row_names)) {
        max(convertUnit(hmargs$row_names_max_width, "inches", valueOnly = TRUE) * 0.5 - 0.2, 0)
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
        orig_col_labels <- if (isTRUE(flip)) rownames(hmargs$matrix) else colnames(hmargs$matrix)
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
    row_overhead <- (if (isTRUE(cluster_rows))    0.5 else 0) +
                    (nrow_annos - show_row_names * 4) * 0.15 + phantom_right_width_in
    col_overhead <- (if (isTRUE(cluster_columns)) 0.5 else 0) +
                    (ncol_annos - show_column_names * 4) * 0.15 + colnames_height

    if (isTRUE(flip)) {
        body_width  <- ncols * cell_h
        body_height <- nrows * cell_w
    } else {
        body_width  <- ncols * cell_w
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
        width  <- body_width  + col_overhead + rownames_width + convertUnit(sum(padding[c(1, 3)]), "inches", valueOnly = TRUE)
        height <- body_height + row_overhead + convertUnit(sum(padding[c(2, 4)]), "inches", valueOnly = TRUE)
    } else {
        width  <- body_width  + row_overhead + rownames_width + convertUnit(sum(padding[c(2, 4)]), "inches", valueOnly = TRUE)
        height <- body_height + col_overhead + convertUnit(sum(padding[c(1, 3)]), "inches", valueOnly = TRUE)
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
        .legend_label_cands <- c(.legend_label_cands,
            formatC(c(lower_cutoff, upper_cutoff), digits = 3, format = "g"))
    }
    # Helper: extract label strings from a column of `data`
    .col_labels <- function(col) {
        if (is.null(col) || !col %in% colnames(data)) return(character(0))
        as.character(if (is.factor(data[[col]])) levels(data[[col]]) else unique(data[[col]]))
    }
    # Name-annotation legends:
    #   – rows_by shows a legend when show_row_names=FALSE (and row name annotation is enabled)
    #   – columns_by shows a legend when show_column_names=FALSE (and col name annotation is enabled)
    #   – split_by annotations always show legends when present
    if (!isTRUE(show_row_names) && row_name_anno_enabled)
        .legend_label_cands <- c(.legend_label_cands, .col_labels(rows_by))
    if (!isTRUE(show_column_names) && col_name_anno_enabled)
        .legend_label_cands <- c(.legend_label_cands, .col_labels(columns_by))
    .legend_label_cands <- c(.legend_label_cands,
        .col_labels(rows_split_by),
        .col_labels(columns_split_by))
    # Extra row / column annotation legends
    .scan_anno_labels <- function(anno) {
        if (is.null(anno)) return(character(0))
        cols <- if (is.list(anno)) unlist(anno) else as.character(anno)
        unlist(lapply(cols[cols %in% colnames(data)], .col_labels))
    }
    .legend_label_cands <- c(.legend_label_cands,
        .scan_anno_labels(row_annotation),
        .scan_anno_labels(column_annotation))
    legend_nchar <- if (length(.legend_label_cands) > 0) {
        max(nchar(.legend_label_cands), na.rm = TRUE)
    } else { 5L }
    # Number of distinct legend blocks rendered by ComplexHeatmap
    legend_n <- length(Filter(Negate(is.null), legends))

    # Use the same per-character metrics as calculate_plot_dimensions()
    legend_key_w  <- 0.30   # key swatch width + internal margin
    legend_char_w <- 0.07   # inches per character of label text
    legend_row_h  <- 0.30   # height of one stacked legend block
    legend_pad    <- 0.35   # outer margin + optional title

    if (!identical(legend.position, "none")) {
        if (legend.position %in% c("right", "left")) {
            # Vertical panel on the side; width driven by label length.
            # When direction=="horizontal" the colorbar is rendered horizontally
            # (wider), so add an extra 0.5 in to account for the larger key.
            legend_extra <- if (legend.direction == "horizontal") 0.5 else 0
            legend_width <- max(1.0, legend_key_w + legend_nchar * legend_char_w) + legend_extra
            width <- width + legend_width
        } else if (legend.direction == "horizontal") {
            # Each legend block goes on its own row at top / bottom.
            n_legend_rows <- max(1L, legend_n)
            legend_height <- max(1.0, legend_pad + n_legend_rows * legend_row_h)
            height <- height + legend_height
        } else {
            # Vertical blocks at top / bottom: add a legend-width column.
            legend_width <- max(1.0, legend_key_w + legend_nchar * legend_char_w)
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
    unknown_args <- setdiff(names(hmargs), methods::formalArgs(ComplexHeatmap::Heatmap))
    if (length(unknown_args) > 0) {
        warning("[Heatmap] Unknown arguments to ComplexHeatmap::Heatmap(): ",
            paste(unknown_args, collapse = ", "))
        hmargs <- hmargs[setdiff(names(hmargs), unknown_args)]
    }
    p <- do.call(ComplexHeatmap::Heatmap, hmargs)
    mat <- p@matrix
    # Move label/mark legends to the end so they appear last
    if (!is.null(legends$.label)) {
        legends <- c(legends[names(legends) != ".label"], legends[".label"])
    }
    if (!is.null(legends$.mark)) {
        legends <- c(legends[names(legends) != ".mark"], legends[".mark"])
    }
    draw_args_fixed <- list(
        annotation_legend_list = legends,
        padding = draw_opts$padding %||% padding,
        column_title = title,
        align_heatmap_legend = draw_opts$align_heatmap_legend %||% "heatmap_center",
        align_annotation_legend = draw_opts$align_annotation_legend %||% "heatmap_center"
    )
    if (identical(legend.position, "none")) {
        draw_args_fixed$show_annotation_legend <- draw_opts$show_annotation_legend %||% FALSE
        draw_args_fixed$show_heatmap_legend <- draw_opts$show_heatmap_legend %||% FALSE
    } else {
        draw_args_fixed$annotation_legend_side <- draw_opts$annotation_legend_side %||% legend.position
        draw_args_fixed$heatmap_legend_side <- draw_opts$heatmap_legend_side %||% legend.position
    }
    draw_args <- utils::modifyList(draw_opts, draw_args_fixed)

    p <- grid::grid.grabExpr(do.call(ComplexHeatmap::draw, c(list(p), draw_args)))
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
#' @description Heatmap is a popular way to visualize data in matrix format. It is widely used in biology to visualize gene expression data in microarray and RNA-seq data. The heatmap is a matrix where rows represent the samples and columns represent the features. The color of each cell represents the value of the feature in the sample. The color can be continuous or discrete. The heatmap can be split by the columns or rows to show the subgroups in the data. The heatmap can also be annotated by the columns or rows to show the additional information of the samples or features.
#' @inheritParams process_heatmap_data
#' @inheritParams HeatmapAtomic
#' @inheritParams common_args
#' @param keep_na Whether we should keep NA groups in rows, columns and split_by variables. Default is FALSE.
#' FALSE to remove NA groups; TRUE to keep NA groups.
#' A vector of column names can also be provided to specify which columns to keep NA groups.
#' Note that the record will be removed if any of the grouping columns has NA and is not specified to keep NA.
#' @return A patchwork wrapped heatmap object if `combine` is `TRUE`;
#' otherwise a list of heatmap objects if `combine` is `FALSE` and `split_by` is specified.
#' @export
#' @importFrom patchwork wrap_plots
#' @seealso \code{\link{anno_simple}}, \code{\link{anno_points}}, \code{\link{anno_lines}}, \code{\link{anno_pie}}, \code{\link{anno_violin}}, \code{\link{anno_boxplot}}, \code{\link{anno_density}}
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
#'     dot_size_data <- p@data
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
    data, values_by = NULL, values_fill = NA, name = NULL,
    # data definition
    in_form = c("auto", "matrix", "wide-columns", "wide-rows", "long"),
    split_by = NULL, split_by_sep = "_",
    rows_by = NULL, rows_by_sep = "_", rows_split_by = NULL, rows_split_by_sep = "_",
    columns_by = NULL, columns_by_sep = "_", columns_split_by = NULL, columns_split_by_sep = "_",
    rows_data = NULL, columns_data = NULL, keep_na = FALSE, keep_empty = FALSE,
    rows_orderby = NULL, columns_orderby = NULL,
    # names
    columns_name = NULL, columns_split_name = NULL,
    rows_name = NULL, rows_split_name = NULL,
    # palettes
    palette = "RdBu", palcolor = NULL, palreverse = FALSE,
    # cell_type: pies
    pie_size_name = "size", pie_size = NULL, pie_values = "length", pie_name = NULL,
    pie_group_by = NULL, pie_group_by_sep = "_", pie_palette = "Spectral", pie_palcolor = NULL,
    # cell_type: bars
    bars_sample = 100,
    # cell_type: label
    label = identity, label_size = 10, label_color = "black", label_name = "label",
    # cell_type: mark
    mark = identity, mark_color = "black", mark_size = 1, mark_name = "mark",
    # cell_type: violin
    violin_fill = NULL,
    # cell_type: boxplot
    boxplot_fill = NULL,
    # cell_type: dot
    dot_size = 8, dot_size_name = "size",
    # legend
    legend_items = NULL, legend_discrete = FALSE,
    legend.position = "right", legend.direction = "vertical",
    # values
    lower_quantile = 0, upper_quantile = 0.99, lower_cutoff = NULL, upper_cutoff = NULL,
    # bg
    add_bg = FALSE, bg_alpha = 0.5,
    # reticle
    add_reticle = FALSE, reticle_color = "grey",
    # passed to ComplexHeatmap::Heatmap
    cluster_columns = NULL, cluster_rows = NULL, show_row_names = NULL, show_column_names = NULL,
    border = TRUE, title = NULL, column_title = NULL, row_title = NULL, na_col = "grey85",
    row_names_side = "right", column_names_side = "bottom",
    column_annotation = NULL, column_annotation_side = "top", column_annotation_palette = "Paired", column_annotation_palcolor = NULL,
    column_annotation_type = "auto", column_annotation_params = list(), column_annotation_agg = NULL,
    row_annotation = NULL, row_annotation_side = "left", row_annotation_palette = "Paired", row_annotation_palcolor = NULL,
    row_annotation_type = "auto", row_annotation_params = list(), row_annotation_agg = NULL,
    # misc
    flip = FALSE, alpha = 1, seed = 8525, padding = 15, base_size = 1, aspect.ratio = NULL, draw_opts = list(),
    # cell customization
    layer_fun_callback = NULL, cell_type = c("tile", "bars", "label", "mark", "label+mark", "mark+label", "dot", "violin", "boxplot", "pie"), cell_agg = NULL,
    # subplots
    combine = TRUE, nrow = NULL, ncol = NULL, byrow = TRUE, axes = NULL, axis_titles = axes, guides = NULL, design = NULL,
    ...
) {
    validate_common_args(seed)
    in_form <- match.arg(in_form)
    cell_type <- match.arg(cell_type)
    cell_type <- sub("mark+label", "label+mark", cell_type, fixed = TRUE)

    if (!is.null(rows_orderby)) {
        cluster_rows <- cluster_rows %||% FALSE
        stopifnot("[Heatmap] `rows_orderby` can't be used with `cluster_rows = TRUE`" = isFALSE(cluster_rows))
    } else {
        cluster_rows <- cluster_rows %||% TRUE
    }

    if (!is.null(columns_orderby)) {
        cluster_columns <- cluster_columns %||% FALSE
        stopifnot("[Heatmap] `columns_orderby` can't be used with `cluster_columns = TRUE`" = isFALSE(cluster_columns))
    } else {
        cluster_columns <- cluster_columns %||% TRUE
    }

    hmdata <- process_heatmap_data(
        data, in_form = in_form, values_by = values_by, name = name,
        split_by = split_by, split_by_sep = split_by_sep, rows_orderby = rows_orderby, columns_orderby = columns_orderby,
        rows_by = rows_by, rows_by_sep = rows_by_sep, rows_name = rows_name,
        rows_split_by = rows_split_by, rows_split_by_sep = rows_split_by_sep, rows_split_name = rows_split_name,
        columns_by = columns_by, columns_by_sep = columns_by_sep, columns_name = columns_name,
        columns_split_by = columns_split_by, columns_split_by_sep = columns_split_by_sep, columns_split_name = columns_split_name,
        pie_group_by = pie_group_by, pie_group_by_sep = pie_group_by_sep, pie_name = pie_name,
        rows_data = rows_data, columns_data = columns_data, keep_na = keep_na, keep_empty = keep_empty
    )
    split_by <- split_by %||% "..."

    palette <- check_palette(palette, names(hmdata$data))
    palcolor <- check_palcolor(palcolor, names(hmdata$data))
    pie_palette <- check_palette(pie_palette, names(hmdata$data))
    pie_palcolor <- check_palcolor(pie_palcolor, names(hmdata$data))
    legend.direction <- check_legend(legend.direction, names(hmdata$data), "legend.direction")
    legend.position <- check_legend(legend.position, names(hmdata$data), "legend.position")

    plots <- lapply(
        names(hmdata$data), function(nm) {
            default_title <- if (length(hmdata$data) == 1 && identical(nm, "...")) NULL else nm
            if (is.function(title)) {
                title <- title(default_title)
            } else {
                title <- title %||% default_title
            }

            HeatmapAtomic(
                data = hmdata$data[[nm]], values_by = hmdata$values_by, values_fill = values_fill,
                rows_by = hmdata$rows_by, rows_split_by = hmdata$rows_split_by,
                columns_by = hmdata$columns_by, columns_split_by = hmdata$columns_split_by,

                palette = palette[[nm]], palcolor = palcolor[[nm]], palreverse = palreverse,

                pie_size_name = pie_size_name, pie_size = pie_size, pie_values = pie_values,
                pie_group_by = hmdata$pie_group_by, pie_palette = pie_palette[[nm]], pie_palcolor = pie_palcolor[[nm]],

                bars_sample = bars_sample,
                label = label, label_size = label_size, label_color = label_color, label_name = label_name,
                mark = mark, mark_color = mark_color, mark_size = mark_size, mark_name = mark_name,
                violin_fill = violin_fill,
                boxplot_fill = boxplot_fill,
                dot_size = dot_size, dot_size_name = dot_size_name,

                legend_items = legend_items, legend_discrete = legend_discrete,
                legend.position = legend.position[[nm]], legend.direction = legend.direction[[nm]],

                lower_quantile = lower_quantile, upper_quantile = upper_quantile,
                lower_cutoff = lower_cutoff, upper_cutoff = upper_cutoff,

                add_bg = add_bg, bg_alpha = bg_alpha, keep_na = hmdata$keep_na, keep_empty = hmdata$keep_empty,

                add_reticle = add_reticle, reticle_color = reticle_color,

                cluster_columns = cluster_columns, cluster_rows = cluster_rows, show_row_names = show_row_names, show_column_names = show_column_names,
                border = border, title = title, column_title = column_title, row_title = row_title, na_col = na_col,
                row_names_side = row_names_side, column_names_side = column_names_side,
                column_annotation = column_annotation, column_annotation_side = column_annotation_side,
                column_annotation_palette = column_annotation_palette, column_annotation_palcolor = column_annotation_palcolor,
                column_annotation_type = column_annotation_type, column_annotation_params = column_annotation_params,
                column_annotation_agg = column_annotation_agg,
                row_annotation = row_annotation, row_annotation_side = row_annotation_side,
                row_annotation_palette = row_annotation_palette, row_annotation_palcolor = row_annotation_palcolor,
                row_annotation_type = row_annotation_type, row_annotation_params = row_annotation_params,
                row_annotation_agg = row_annotation_agg,

                flip = flip, alpha = alpha, seed = seed, base_size = base_size, aspect.ratio = aspect.ratio, draw_opts = draw_opts,
                layer_fun_callback = layer_fun_callback, cell_type = cell_type, cell_agg = cell_agg,

                ...
            )
        }
    )
    names(plots) <- names(hmdata$data)

    p <- combine_plots(plots,
        combine = combine, split_by = split_by, nrow = nrow, ncol = ncol, byrow = byrow,
        axes = axes, axis_titles = axis_titles, guides = guides, design = design
    )

    # Return the plot object
    # When return_grob = FALSE, p is a HeatmapList object with auto-printing behavior
    # The initial draw() in HeatmapAtomic was captured to a null device to prevent
    # printing during assignment, so this returned object can print normally when
    # called directly (e.g., in a Jupyter cell)
    p
}
