# Venn / Euler diagram

Draws Venn or Euler diagrams that visualise the overlap relationships
among multiple sets. Supports four input formats: long (one row per
element-set pair), wide (logical/0-1 columns per set), a named list
(element vectors per set), and a pre-computed `VennPlotData` object.

Intersection regions can be filled by a continuous colour gradient
encoding the element count (`fill_mode = "count"` / `"count_rev"`) or by
blended set colours (`fill_mode = "set"`). Region labels can display
counts, percentages, both, or a custom function. Set labels always show
the set name and its total element count.

Use `split_by` to produce separate Venn diagrams for each level of a
grouping variable. Note that `split_by` is only supported when `data` is
a data frame (list and `VennPlotData` inputs cannot be split).

## Usage

``` r
VennDiagram(
  data,
  in_form = c("auto", "long", "wide", "list", "venn"),
  split_by = NULL,
  split_by_sep = "_",
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
  palette = ifelse(fill_mode == "set", "Paired", "Blues"),
  palcolor = NULL,
  alpha = 1,
  theme = "theme_this",
  theme_args = list(),
  title = NULL,
  subtitle = NULL,
  legend.position = "right",
  legend.direction = "vertical",
  aspect.ratio = 1,
  combine = TRUE,
  nrow = NULL,
  ncol = NULL,
  byrow = TRUE,
  seed = 8525,
  axes = NULL,
  axis_titles = axes,
  guides = NULL,
  design = NULL,
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

- split_by:

  The column(s) to split the data by and produce separate Venn diagrams
  per subgroup. Only supported when `data` is a data frame. Multiple
  columns are concatenated with `split_by_sep`.

- split_by_sep:

  A character string to separate concatenated `split_by` columns.
  Default `"_"`.

- group_by:

  Columns to group the data for plotting For those plotting functions
  that do not support multiple groups, They will be concatenated into
  one column, using `group_by_sep` as the separator

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

- aspect.ratio:

  A numeric value specifying the aspect ratio of the plot.

- combine:

  Logical; when `TRUE` (default), returns a combined `patchwork` object.
  When `FALSE`, returns a named list of individual `ggplot` objects.

- ncol, nrow:

  Integer number of columns / rows for the combined layout when
  `combine = TRUE`.

- byrow:

  Logical; fill the combined layout by row (default `TRUE`).

- seed:

  A numeric seed for reproducibility. Default `8525`.

- axes, axis_titles:

  Character strings specifying how axes and axis titles are handled
  across the combined layout.

- guides:

  A character string specifying how legends are collected across panels
  in the combined layout.

- design:

  A custom layout specification for the combined plot. Passed to
  [`wrap_plots()`](https://patchwork.data-imaginist.com/reference/wrap_plots.html).

- ...:

  Additional arguments.

## Value

A `ggplot` object (single split), a `patchwork` object (combined
sub-plots), or a named list of `ggplot` objects (when
`combine = FALSE`), each with `height` and `width` attributes in inches.

## split_by Workflow

When a non-`NULL` `split_by` is provided and the input is a data frame:

1.  **Validation** — an error is raised if `data` is not a data frame
    (list and `VennPlotData` input cannot be split).

2.  **Column resolution** — `split_by` is validated and optionally
    concatenated via
    [`check_columns()`](https://pwwang.github.io/plotthis/reference/check_columns.md)
    with `force_factor = TRUE` and `allow_multi = TRUE`.

3.  **Data splitting** — the data frame is split by the unique levels of
    the `split_by` column, preserving factor level order. Empty levels
    are dropped via
    [`droplevels()`](https://rdrr.io/r/base/droplevels.html).

4.  **Per-split colour and legend resolution** —
    [`check_palette()`](https://pwwang.github.io/plotthis/reference/check_palette.md),
    [`check_palcolor()`](https://pwwang.github.io/plotthis/reference/check_palcolor.md),
    and
    [`check_legend()`](https://pwwang.github.io/plotthis/reference/check_legend.md)
    resolve per-split palettes, custom colours, legend positions, and
    legend directions.

5.  **Atomic dispatch** —
    [`VennDiagramAtomic()`](https://pwwang.github.io/plotthis/reference/VennDiagramAtomic.md)
    is called for each subset. When `title` is a function, it receives
    the split level name for dynamic title generation.

6.  **Combination** — results are passed to
    [`combine_plots()`](https://pwwang.github.io/plotthis/reference/combine_plots.md)
    which returns a combined `patchwork` object (when `combine = TRUE`)
    or a named list of individual ggplot objects (when
    `combine = FALSE`).

## Examples

``` r
# \donttest{
set.seed(8525)
data <- list(
    A = sort(sample(letters, 8)),
    B = sort(sample(letters, 8)),
    C = sort(sample(letters, 8)),
    D = sort(sample(letters, 8))
)

# Basic Venn diagram with count labels
VennDiagram(data)


# Fill by set membership (blended colours)
VennDiagram(data, fill_mode = "set")


# Show both count and percentage
VennDiagram(data, label = "both")


# Custom label function using set names
VennDiagram(data, label = function(df) df$name)


# Custom palette and transparency
VennDiagram(data, palette = "material-indigo", alpha = 0.6)

# }
```
