# NetworkAtomic

Core implementation for rendering a single network graph from a links
(edge list) data frame and an optional nodes (vertex metadata) data
frame. This is the workhorse behind the exported
[`Network`](https://pwwang.github.io/plotthis/reference/Network.md)
function – it takes a **single** pair of links/nodes data frames (no
`split_by` support) and returns a `ggplot` object.

The graph is constructed via
[`graph_from_data_frame`](https://r.igraph.org/reference/graph_from_data_frame.html),
laid out using igraph layout algorithms (`"circle"`, `"tree"`, `"grid"`,
or any named igraph layout such as `"fr"` or `"kk"`), and rendered with
[`ggraph`](https://ggraph.data-imaginist.com/reference/ggraph.html)
using
[`geom_edge_arc`](https://ggraph.data-imaginist.com/reference/geom_edge_arc.html)
for links and
[`geom_point`](https://ggplot2.tidyverse.org/reference/geom_point.html)
for nodes.

Key features include:

- **Directed / undirected graphs** with optional arrow heads.

- **Link styling**: variable width (weight), linetype, and colour, each
  constant or mapped from a column. Edge colours can follow source
  nodes, target nodes, or a dedicated column.

- **Node styling**: variable size, shape, colour, and fill, each
  constant or column-mapped.

- **Community detection** via igraph clustering algorithms with convex
  hull, ellipse, rectangle, or circle marks from
  [`ggforce`](https://ggforce.data-imaginist.com/reference/ggforce-package.html).

- **Automatic labels** via
  [`geom_text_repel`](https://ggrepel.slowkow.com/reference/geom_text_repel.html).

- **Self-loop edges** drawn as direction-sensitive arcs.

## Usage

``` r
NetworkAtomic(
  links,
  nodes = NULL,
  from = NULL,
  from_sep = "_",
  to = NULL,
  to_sep = "_",
  node_by = NULL,
  node_by_sep = "_",
  link_weight_by = 2,
  link_weight_name = NULL,
  link_type_by = "solid",
  link_type_name = NULL,
  node_size_by = 15,
  node_size_name = NULL,
  node_color_by = "black",
  node_color_name = NULL,
  node_shape_by = 21,
  node_shape_name = NULL,
  node_fill_by = "grey20",
  node_fill_name = NULL,
  link_alpha = 1,
  node_alpha = 0.95,
  node_stroke = 1.5,
  cluster_scale = c("fill", "color", "shape"),
  node_size_range = c(5, 20),
  link_weight_range = c(0.5, 5),
  link_arrow_offset = 20,
  link_curvature = 0,
  link_color_by = "from",
  link_color_name = NULL,
  palette = "Paired",
  palcolor = NULL,
  palreverse = FALSE,
  link_palette = ifelse(link_color_by %in% c("from", "to"), palette, "Set1"),
  link_palcolor = if (link_color_by %in% c("from", "to")) palcolor else NULL,
  directed = TRUE,
  layout = "circle",
  cluster = "none",
  add_mark = FALSE,
  mark_expand = ggplot2::unit(10, "mm"),
  mark_type = c("hull", "ellipse", "rect", "circle"),
  mark_alpha = 0.1,
  mark_linetype = 1,
  add_label = TRUE,
  label_size = 3,
  label_fg = "white",
  label_bg = "black",
  label_bg_r = 0.1,
  arrow = ggplot2::arrow(type = "closed", length = ggplot2::unit(0.1, "inches")),
  title = NULL,
  subtitle = NULL,
  xlab = NULL,
  ylab = NULL,
  aspect.ratio = 1,
  theme = "theme_this",
  theme_args = list(),
  legend.position = "right",
  legend.direction = "vertical",
  ...
)
```

## Arguments

- links:

  A data frame containing the edge list. Must contain the `from` and
  `to` columns specifying source and target node identifiers. Additional
  columns can be referenced by other parameters (e.g., `link_weight_by`,
  `link_type_by`, `link_color_by`).

- nodes:

  An optional data frame of node metadata. When provided, columns such
  as `node_size_by`, `node_color_by`, `node_shape_by`, and
  `node_fill_by` can reference its columns. When `NULL`, the node set is
  inferred from the unique values in the `from` and `to` columns. If a
  single character string starting with `"@"`, the nodes data frame is
  extracted from the corresponding attribute of `links` (e.g. `"@nodes"`
  extracts `attr(links, "nodes")`).

- from:

  A character string specifying the column name in `links` for the
  source node identifiers. Defaults to `"from"`, or the first column of
  `links` if that column name does not exist. Multiple columns can be
  provided; they are concatenated with `from_sep`.

- from_sep:

  A character string to join multiple `from` columns. Default `"_"`.
  Ignored when `from` is a single column.

- to:

  A character string specifying the column name in `links` for the
  target node identifiers. Defaults to `"to"`, or the second column of
  `links` if that column name does not exist. Multiple columns can be
  provided; they are concatenated with `to_sep`.

- to_sep:

  A character string to join multiple `to` columns. Default `"_"`.
  Ignored when `to` is a single column.

- node_by:

  A character string specifying the column name in `nodes` for the node
  identifiers. These must match the values in the `from` / `to` columns
  of `links`. Defaults to `"name"`, or the first column of `nodes` if
  that column name does not exist. Multiple columns can be provided;
  they are concatenated with `node_by_sep`.

- node_by_sep:

  A character string to join multiple `node_by` columns. Default `"_"`.
  Ignored when `node_by` is a single column.

- link_weight_by:

  A numeric value or a character string. If numeric, all edges receive
  that constant line width. If a column name, the edge line width is
  mapped to that column. Default `2`.

- link_weight_name:

  A character string for the link weight legend title. When `NULL`
  (default), the column name from `link_weight_by` is used. Only
  relevant when `link_weight_by` is a column name.

- link_type_by:

  A character string or a column name specifying the edge linetype. Can
  be `"solid"`, `"dashed"`, `"dotted"`, etc. If a column name from
  `links` is supplied, the linetype is mapped to that column (with a
  version check for ggplot2 4.0.0, where mapping is unsupported and a
  warning is issued). Default `"solid"`.

- link_type_name:

  A character string for the link linetype legend title. When `NULL`
  (default), the column name from `link_type_by` is used. Only relevant
  when `link_type_by` is a column name.

- node_size_by:

  A numeric value or a character string. If numeric, all nodes receive
  that constant point size. If a column name, the size is mapped to that
  column. Default `15`.

- node_size_name:

  A character string for the node size legend title. When `NULL`
  (default), the column name from `node_size_by` is used. Only relevant
  when `node_size_by` is a column name.

- node_color_by:

  A character string specifying the node colour. If a colour name or hex
  code (e.g. `"black"`), all nodes receive that constant colour. If a
  column name from `nodes` is supplied, the colour is mapped to that
  column. Default `"black"`.

- node_color_name:

  A character string for the node colour legend title. When `NULL`
  (default), the column name from `node_color_by` is used. Only relevant
  when `node_color_by` is a column name.

- node_shape_by:

  A numeric value or a character string. If numeric, all nodes receive
  that constant shape (see
  [`shape`](https://ggplot2.tidyverse.org/reference/aes_linetype_size_shape.html)).
  If a column name, the shape is mapped to that column (cast to factor).
  Default `21` (filled circle with border).

- node_shape_name:

  A character string for the node shape legend title. When `NULL`
  (default), the column name from `node_shape_by` is used. Only relevant
  when `node_shape_by` is a column name.

- node_fill_by:

  A character string specifying the node fill colour. If a colour name
  or hex code (e.g. `"grey20"`), all nodes receive that constant fill.
  If a column name from `nodes` is supplied, the fill is mapped to that
  column. Default `"grey20"`.

- node_fill_name:

  A character string for the node fill legend title. When `NULL`
  (default), the column name from `node_fill_by` is used. Only relevant
  when `node_fill_by` is a column name.

- link_alpha:

  A numeric value specifying the transparency (alpha) of the edge lines.
  Between `0` (invisible) and `1` (opaque). Default `1`.

- node_alpha:

  A numeric value specifying the fill transparency of the nodes. Only
  applies when `node_shape_by` is one of the filled shapes (21–25).
  Default `0.95`.

- node_stroke:

  A numeric value specifying the border stroke width of the node points.
  Default `1.5`.

- cluster_scale:

  A character string specifying which node aesthetic is overridden by
  cluster membership. One of `"fill"`, `"color"`, or `"shape"`. The
  value is matched via
  [`match.arg`](https://rdrr.io/r/base/match.arg.html); default is
  `"fill"`.

- node_size_range:

  A numeric vector of length 2 giving the minimum and maximum node size
  (in ggplot2 point units) when `node_size_by` is a column name. Default
  `c(5, 20)`.

- link_weight_range:

  A numeric vector of length 2 giving the minimum and maximum edge line
  width (in mm) when `link_weight_by` is a column name. Default
  `c(0.5, 5)`.

- link_arrow_offset:

  A numeric value (in points) specifying the offset distance for the
  arrow end cap from the target node. Prevents arrow heads from
  overlapping the node points. Only relevant when `directed = TRUE`.
  Default `20`.

- link_curvature:

  A numeric value controlling the curvature of the edges. `0` (default)
  produces straight edges; positive values curve them away from the
  direct path.

- link_color_by:

  A character string controlling how edge colour is determined. Options:

  - `"from"` (default) – colour follows the source node's fill or colour
    aesthetic.

  - `"to"` – colour follows the target node's fill or colour.

  - A column name from `links` – colour is mapped directly to that
    column.

- link_color_name:

  A character string for the edge colour legend title. Only used when
  `link_color_by` is a column name (not `"from"` or `"to"`). When `NULL`
  (default), the column name is used.

- palette:

  A character string specifying the palette to use. A named list or
  vector can be used to specify the palettes for different `split_by`
  values.

- palcolor:

  A character string specifying the color to use in the palette. A named
  list can be used to specify the colors for different `split_by`
  values. If some values are missing, the values from the palette will
  be used (palcolor will be NULL for those values).

- palreverse:

  A logical value indicating whether to reverse the palette. Default is
  FALSE.

- link_palette:

  A character string specifying the palette for edge colours when they
  are mapped. When `link_color_by` is `"from"` or `"to"`, defaults to
  the node `palette`. Otherwise defaults to `"Set1"`.

- link_palcolor:

  A character vector specifying custom colours for the edge palette.
  When `link_color_by` is `"from"` or `"to"`, defaults to the node
  `palcolor`. Otherwise defaults to `NULL`.

- directed:

  A logical value. When `TRUE`, edges are drawn with arrow heads and an
  end-cap offset. Default `TRUE`.

- layout:

  A character string or an `igraph_layout_spec` object specifying the
  node placement algorithm. Built-in shortcuts: `"circle"` (circular
  layout), `"tree"` (hierarchical tree), `"grid"` (grid layout). Any
  other string is prefixed with `"layout_with_"` and called as an igraph
  function (e.g. `"fr"` for Fruchterman–Reingold, `"kk"` for
  Kamada–Kawai). Default `"circle"`.

- cluster:

  A character string specifying the community detection algorithm. One
  of `"none"`, `"fast_greedy"`, `"walktrap"`, `"edge_betweenness"`,
  `"infomap"`, or a custom clustering function from igraph. When not
  `"none"`, cluster membership overrides the aesthetic selected by
  `cluster_scale`. Default `"none"`.

- add_mark:

  A logical value. When `TRUE` (and `cluster != "none"`), an enclosure
  mark is drawn around each cluster's nodes. Default `FALSE`.

- mark_expand:

  A [`unit`](https://rdrr.io/r/grid/unit.html) object specifying the
  extra space around points within a cluster mark. Default
  `unit(10, "mm")`.

- mark_type:

  A character string specifying the mark geometry. One of `"hull"`,
  `"ellipse"`, `"rect"`, or `"circle"`, corresponding to ggforce's
  `geom_mark_hull`, `geom_mark_ellipse`, `geom_mark_rect`, and
  `geom_mark_circle`. The value is matched via
  [`match.arg`](https://rdrr.io/r/base/match.arg.html); default is
  `"hull"`.

- mark_alpha:

  A numeric value for the fill transparency of cluster marks. Default
  `0.1`.

- mark_linetype:

  A numeric or character value specifying the border line type of the
  cluster marks. Default `1` (solid).

- add_label:

  A logical value. When `TRUE` (default), node identifiers are drawn as
  repulsive text labels via
  [`geom_text_repel`](https://ggrepel.slowkow.com/reference/geom_text_repel.html).

- label_size:

  A numeric value for the font size of node labels. Scaled by the theme
  base size. Default `3`.

- label_fg:

  A character string specifying the text colour of node labels. Default
  `"white"`.

- label_bg:

  A character string specifying the background colour of node labels.
  Default `"black"`.

- label_bg_r:

  A numeric value specifying the background box radius (as a fraction of
  label height). Passed to
  [`geom_text_repel`](https://ggrepel.slowkow.com/reference/geom_text_repel.html)'s
  `bg.r` argument. Default `0.1`.

- arrow:

  A [`arrow`](https://rdrr.io/r/grid/arrow.html) object for the link
  arrow heads. Only used when `directed = TRUE`. Default is
  `arrow(type = "closed", length = unit(0.1, "inches"))`.

- title:

  A character string specifying the title of the plot. A function can be
  used to generate the title based on the default title. This is useful
  when split_by is used and the title needs to be dynamic.

- subtitle:

  A character string specifying the subtitle of the plot.

- xlab:

  A character string specifying the x-axis label.

- ylab:

  A character string specifying the y-axis label.

- aspect.ratio:

  A numeric value specifying the aspect ratio of the plot.

- theme:

  A character string or a theme class (i.e. ggplot2::theme_classic)
  specifying the theme to use. Default is "theme_this".

- theme_args:

  A list of arguments to pass to the theme function.

- legend.position:

  A character string specifying the position of the legend. if
  `waiver()`, for single groups, the legend will be "none", otherwise
  "right".

- legend.direction:

  A character string specifying the direction of the legend.

- ...:

  Not used.

## Value

A `ggplot` object with `height` and `width` attributes (in inches).

## Architecture

1.  **Column resolution** – Default column names for `from`, `to`, and
    `node_by` are assigned when `NULL`, using the first or second column
    of the respective data frame. Each is validated via
    [`check_columns`](https://pwwang.github.io/plotthis/reference/check_columns.md);
    multi-column inputs are concatenated with their respective separator
    (`from_sep`, `to_sep`, `node_by_sep`) and force-converted to
    factors. Resolved columns are renamed to the canonical names
    `"from"`, `"to"`, and `"name"`.

2.  **Graph construction** – An
    [`igraph`](https://r.igraph.org/reference/aaa-igraph-package.html)
    object is built via
    [`graph_from_data_frame`](https://r.igraph.org/reference/graph_from_data_frame.html)
    using the links data frame and (optionally) the nodes data frame. If
    `nodes` is a string starting with `"@"`, it is extracted from the
    corresponding attribute on the links object.

3.  **Layout computation** – The layout is resolved: `"circle"` -\>
    [`layout_in_circle`](https://r.igraph.org/reference/layout_in_circle.html),
    `"tree"` -\>
    [`layout_as_tree`](https://r.igraph.org/reference/layout_as_tree.html),
    `"grid"` -\>
    [`layout_on_grid`](https://r.igraph.org/reference/layout_on_grid.html).
    Any other character string is prefixed with `"layout_with_"` and
    looked up in the igraph namespace (e.g. `"fr"` -\>
    `layout_with_fr`). If `layout` is already an `igraph_layout_spec`
    object it is applied directly via
    [`layout_`](https://r.igraph.org/reference/layout_.html).

4.  **Data extraction** – Vertex and edge data frames are extracted from
    the igraph object via
    [`as_data_frame`](https://r.igraph.org/reference/graph_from_data_frame.html).
    Layout coordinates are added as `x` and `y` columns to the vertex
    data.

5.  **Node aesthetic assembly** – Each of `size`, `color`, `shape`, and
    `fill` is resolved as either a constant value (numeric or character)
    or a mapping to a column of the node data. A per-aesthetic `"guide"`
    / `"none"` flag is tracked for legend construction.

6.  **Link aesthetic assembly** – `linewidth` (weight), `linetype`, and
    `color` are resolved similarly.

    - When `link_color_by = "from"`, edge colours are derived from the
      source node's fill (if node shape is filled, 21–25) or colour;
      when `"to"`, from the target node.

    - When `link_color_by` names a column in the links data frame (other
      than `"from"`/`"to"`), it is mapped directly.

    If `directed = TRUE`, a [`arrow`](https://rdrr.io/r/grid/arrow.html)
    is added and `end_cap` is set via
    [`circle`](https://ggraph.data-imaginist.com/reference/geometry.html)
    to prevent arrow overlap with nodes. Self-loop edges receive a
    `direction` aesthetic.

7.  **Plot initialisation** – A
    [`ggraph`](https://ggraph.data-imaginist.com/reference/ggraph.html)
    plot is created with `layout = "manual"` and vertex coordinates from
    the layout.

8.  **Clustering and marks** – When `cluster != "none"`, the specified
    igraph community-detection algorithm is run (`cluster_fast_greedy`,
    `cluster_walktrap`, `cluster_edge_betweenness`, `cluster_infomap`,
    or a custom function). Membership overrides the aesthetic specified
    by `cluster_scale` (`"fill"`, `"color"`, or `"shape"`) with a
    warning that the previous setting is discarded. If
    `add_mark = TRUE`, a mark enclosure (hull / ellipse / rect / circle)
    is drawn per cluster via the corresponding
    [`ggforce`](https://ggforce.data-imaginist.com/reference/ggforce-package.html)
    geom.

9.  **Link rendering** –
    [`geom_edge_arc`](https://ggraph.data-imaginist.com/reference/geom_edge_arc.html)
    draws the edges (with configurable `link_curvature`) and
    [`geom_edge_loop`](https://ggraph.data-imaginist.com/reference/geom_edge_loop.html)
    handles self-loop edges.

10. **Link scales** – Conditional on their guide status,
    `scale_edge_width_continuous`, `scale_edge_linetype_discrete`, and
    `scale_edge_color_manual` / `scale_edge_color_gradientn` are added
    for weight, linetype, and colour legends respectively.

11. **Node rendering** –
    [`geom_point`](https://ggplot2.tidyverse.org/reference/geom_point.html)
    draws the nodes with the assembled aesthetics.

12. **Node scales** – Conditional scale additions:
    `scale_size_continuous` (range from `node_size_range`),
    `scale_color_manual`, `scale_shape_manual`, and `scale_fill_manual`,
    each with their legend title and guide overrides.

13. **Labels** – When `add_label = TRUE`, node identifiers are rendered
    via
    [`geom_text_repel`](https://ggrepel.slowkow.com/reference/geom_text_repel.html)
    using `label_fg`, `label_bg`, and `label_bg_r`.

14. **Final theme and dimensions** – Coordinate expansion, axis labels,
    theme application, and legend positioning are applied. Plot height
    and width are computed via
    [`calculate_plot_dimensions`](https://pwwang.github.io/plotthis/reference/calculate_plot_dimensions.md)
    and stored as attributes.
