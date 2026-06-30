# Atomic Venn / Euler diagram (internal)

Core implementation for drawing a Venn or Euler diagram that visualises
the overlap relationships among multiple sets. Supports four input
formats (long, wide, list, and pre-computed `VennPlotData`) which are
normalised by
[`prepare_venn_data()`](https://pwwang.github.io/plotthis/reference/prepare_venn_data.md)
into the internal `VennPlotData` representation used by ggVennDiagram.

Intersection regions can be filled either by a continuous colour
gradient based on the element count (`fill_mode = "count"` or
`"count_rev"`) or by blended set colours (`fill_mode = "set"`). Region
labels can display the raw count, the percentage of total elements,
both, nothing, or a custom function result. Set labels always show the
set name and its total element count.

## Usage

``` r
VennDiagramAtomic(
  data,
  in_form = "auto",
  group_by = NULL,
  group_by_sep = "_",
  id_by = NULL,
  label = "count",
  label_fg = "black",
  label_size = NULL,
  label_bg = "white",
  label_bg_r = 0.1,
  fill_mode = "count",
  palreverse = FALSE,
  fill_name = NULL,
  aspect.ratio = 1,
  palette = ifelse(fill_mode == "set", "Paired", "Spectral"),
  palcolor = NULL,
  alpha = 1,
  theme = "theme_this",
  theme_args = list(),
  title = NULL,
  subtitle = NULL,
  legend.position = "right",
  legend.direction = "vertical",
  ...
)
```

## Arguments

- data:

  A data frame.

- in_form:

  A character string specifying the input format. One of `"auto"`
  (default; detect automatically via
  [`detect_venn_datatype()`](https://pwwang.github.io/plotthis/reference/detect_venn_datatype.md)),
  `"long"`, `"wide"`, `"list"`, or `"venn"`.

- group_by:

  A character string (or vector) specifying the column name(s) that
  define the set membership. For `in_form = "long"`, this is the
  grouping column; for `in_form = "wide"`, these are the set columns
  (must be logical or 0/1); when `NULL` and `data` is a data frame, all
  columns are treated as sets (wide format).

- group_by_sep:

  The separator for multiple group_by columns. See `group_by`

- id_by:

  A character string specifying the column name that identifies
  individual elements. Required for long-format data; ignored otherwise.

- label:

  A character string or function controlling the text shown in each
  intersection region. One of:

  - `"count"` (default) — the raw count of elements in that region.

  - `"percent"` — the percentage of the total element count.

  - `"both"` — count and percentage on separate lines.

  - `"none"` — no region labels are drawn.

  - A **function** — receives a data frame with columns `"id"`, `"X"`,
    `"Y"`, `"name"`, `"item"`, and `"count"`, and must return a
    character vector of labels.

- label_fg:

  A character string specifying the colour of the label text.

- label_size:

  A numeric value specifying the font size of the label text. When
  `NULL` (the default), auto-sized at 3.5 for region labels and 4 for
  set labels, scaled by `base_size / 12`.

- label_bg:

  A character string specifying the background colour of the label text
  (passed to
  [`geom_text_repel()`](https://ggrepel.slowkow.com/reference/geom_text_repel.html)
  as `bg.color`). Default `"white"`.

- label_bg_r:

  A numeric value specifying the corner radius of the label background
  rectangle (passed as `bg.r`). Default `0.1`.

- fill_mode:

  A character string specifying how intersection regions are coloured.
  One of:

  - `"count"` — continuous gradient based on element count (default
    palette: `"Spectral"`).

  - `"count_rev"` — continuous gradient with reversed count order.

  - `"set"` — discrete blended colours per set combination (default
    palette: `"Paired"`). No legend is drawn.

- palreverse:

  A logical value indicating whether to reverse the palette. Default is
  FALSE.

- fill_name:

  A character string for the colour bar legend title when `fill_mode` is
  `"count"` or `"count_rev"`. Ignored when `fill_mode = "set"`.

- aspect.ratio:

  A numeric value specifying the aspect ratio of the plot.

- palette:

  A character string specifying the palette to use. A named list or
  vector can be used to specify the palettes for different `split_by`
  values.

- palcolor:

  A character string specifying the color to use in the palette. A named
  list can be used to specify the colors for different `split_by`
  values. If some values are missing, the values from the palette will
  be used (palcolor will be NULL for those values).

- alpha:

  A numeric value specifying the transparency of the plot.

- theme:

  A character string or a theme class (i.e. ggplot2::theme_classic)
  specifying the theme to use. Default is "theme_this".

- theme_args:

  A list of arguments to pass to the theme function.

- title:

  A character string specifying the title of the plot. A function can be
  used to generate the title based on the default title. This is useful
  when split_by is used and the title needs to be dynamic.

- subtitle:

  A character string specifying the subtitle of the plot.

- legend.position:

  A character string specifying the position of the legend. if
  `waiver()`, for single groups, the legend will be "none", otherwise
  "right".

- legend.direction:

  A character string specifying the direction of the legend.

- ...:

  Additional arguments.

## Value

A `ggplot` object with `height` and `width` attributes (in inches)
attached.

## Architecture

1.  **Data type detection** —
    [`detect_venn_datatype()`](https://pwwang.github.io/plotthis/reference/detect_venn_datatype.md)
    identifies whether `data` is in long, wide, list, or `VennPlotData`
    format.

2.  **Data preparation** —
    [`prepare_venn_data()`](https://pwwang.github.io/plotthis/reference/prepare_venn_data.md)
    converts the input into a `VennPlotData` object suitable for
    rendering by ggVennDiagram.

3.  **Geometry extraction** — ggVennDiagram utilities
    (`venn_regionedge()`, `venn_setedge()`, `venn_regionlabel()`,
    `venn_setlabel()`) compute polygon vertices and label positions for
    all sets and their intersections.

4.  **Fill resolution** — When `fill_mode = "set"`,
    [`palette_this()`](https://pwwang.github.io/plotthis/reference/palette_this.md)
    assigns colours to each set; the
    [`blend_colors()`](https://pwwang.github.io/plotthis/reference/blend_colors.md)
    helper blends the colours of overlapping sets for each intersection
    region. When `fill_mode = "count"` or `"count_rev"`, a continuous
    gradient is applied across regions via `scale_fill_gradientn()` with
    a colour bar legend.

5.  **Label computation** — Region labels are formatted per the `label`
    parameter: raw count, percentage, both, none, or a custom function
    that receives a data frame with columns `"id"`, `"X"`, `"Y"`,
    `"name"`, `"item"`, and `"count"`. Set labels are formatted as
    `"setName\n(count)"`.

6.  **Plot assembly** — `geom_polygon()` draws filled regions;
    `geom_path()` draws set outlines; two
    [`geom_text_repel()`](https://ggrepel.slowkow.com/reference/geom_text_repel.html)
    layers add region labels and set labels. For `fill_mode = "set"`,
    colours are mapped directly (no aesthetic mapping, no legend); for
    count modes, a `scale_fill_gradientn()` continuous scale is used.

7.  **Theme and coordinate system** — `coord_equal()` enforces a square
    aspect ratio. The theme removes all axis text, ticks, titles, grid
    lines, and panel borders. The x-axis expansion is widened to
    accommodate set label text width.

8.  **Dimension calculation** —
    [`calculate_plot_dimensions()`](https://pwwang.github.io/plotthis/reference/calculate_plot_dimensions.md)
    computes `height` and `width` attributes (in inches) from
    `base_height`, `aspect.ratio`, and legend metrics. When
    `fill_mode = "set"`, the legend position is forced to `"none"`.
