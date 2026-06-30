# Clustree Plot

Creates a clustree (clustering tree) plot visualising how cluster
assignments change across increasing clustering resolutions. The plot
helps identify stable clustering solutions and understand the
hierarchical relationships among clusters at different resolution
thresholds.

The function expects a data frame with columns named by a common
`prefix` followed by numeric resolution values (e.g. `"res_0.1"`,
`"res_0.3"`, `"res_0.5"`). Each column contains cluster labels (factor
or character) for every observation at that resolution.

Internally, the function uses
[`clustree::clustree()`](https://lazappi.github.io/clustree/reference/clustree.html)
to compute a `ggraph`-based tree layout where nodes are clusters and
edges represent cells transitioning between clusters at adjacent
resolutions. Edge colour and width encode the number of transitioning
cells.

Key features:

- **Resolution-level node colouring** — each resolution receives a
  distinct colour from the selected `palette`.

- **Edge gradient** — edges are coloured by transition count using a
  separate `edge_palette` colour gradient.

- **Flip support** — `flip = TRUE` places resolutions on the x-axis for
  left-to-right reading.

- **Split by groups** — `split_by` generates per-group clustree plots
  that are combined via `patchwork`.

- **Automatic dimensions** — plot height and width are automatically
  computed based on the number of resolutions, clusters, and the legend
  configuration.

## Usage

``` r
ClustreePlot(
  data,
  prefix,
  flip = FALSE,
  split_by = NULL,
  split_by_sep = "_",
  palette = "Paired",
  palcolor = NULL,
  palreverse = FALSE,
  edge_palette = "Spectral",
  edge_palcolor = NULL,
  aspect.ratio = 1,
  legend.position = "right",
  legend.direction = "vertical",
  title = NULL,
  subtitle = NULL,
  xlab = NULL,
  ylab = NULL,
  expand = c(0.1, 0.1),
  theme = "theme_this",
  theme_args = list(),
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

- prefix:

  A character string specifying the common prefix of the resolution
  columns in `data`. All columns whose names start with this prefix are
  selected as resolution columns. The suffix after the prefix is parsed
  as a numeric resolution value. Supports `"_"` and `"."` as separators
  between the prefix and the resolution value (e.g. `"res_0.5"` or
  `"p.0.5"`).

- flip:

  A logical value. If `TRUE`, the tree is flipped so that resolutions
  are displayed on the x-axis (left to right) and cluster assignments
  are shown as row labels on the y-axis. Default: `FALSE`.

- split_by:

  The column(s) to split data by and generate separate clustree plots
  for each level. Each split level produces an independent clustree plot
  via
  [`ClustreePlotAtomic`](https://pwwang.github.io/plotthis/reference/ClustreePlotAtomic.md).

- split_by_sep:

  A character string used to concatenate multiple `split_by` column
  values when `split_by` specifies more than one column. Default: `"_"`.

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

- edge_palette:

  A character string specifying the palette name for the edge colour
  gradient. Edges are coloured by the number of transitioning cells
  between clusters at adjacent resolutions, using
  [`ggraph::scale_edge_color_gradientn()`](https://ggraph.data-imaginist.com/reference/scale_edge_colour.html).
  Default: `"Spectral"`.

- edge_palcolor:

  A character vector of custom colours for the edge colour gradient.
  When `NULL` (the default), colours are derived from `edge_palette`.

- aspect.ratio:

  A numeric value specifying the aspect ratio of the plot.

- legend.position:

  A character string specifying the position of the legend. if
  `waiver()`, for single groups, the legend will be "none", otherwise
  "right".

- legend.direction:

  A character string specifying the direction of the legend.

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

- expand:

  The values to expand the x and y axes. It is like CSS padding. When a
  single value is provided, it is used for both axes on both sides. When
  two values are provided, the first value is used for the top/bottom
  side and the second value is used for the left/right side. When three
  values are provided, the first value is used for the top side, the
  second value is used for the left/right side, and the third value is
  used for the bottom side. When four values are provided, the values
  are used for the top, right, bottom, and left sides, respectively. You
  can also use a named vector to specify the values for each side. When
  the axis is discrete, the values will be applied as 'add' to the
  'expansion' function. When the axis is continuous, the values will be
  applied as 'mult' to the 'expansion' function. See also
  <https://ggplot2.tidyverse.org/reference/expansion.html>

- theme:

  A character string or a theme class (i.e. ggplot2::theme_classic)
  specifying the theme to use. Default is "theme_this".

- theme_args:

  A list of arguments to pass to the theme function.

- combine:

  A logical value. If `TRUE` (the default), the list of per-split plots
  is combined into a single `patchwork` object. If `FALSE`, returns the
  raw list of `ggplot` objects.

- nrow, ncol, byrow:

  Integers controlling the layout of combined plots via
  [`patchwork::wrap_plots()`](https://patchwork.data-imaginist.com/reference/wrap_plots.html).
  `byrow = TRUE` (default) fills the layout row-wise. Ignored when
  `design` is provided.

- seed:

  The random seed for reproducibility. Passed to
  [`validate_common_args()`](https://pwwang.github.io/plotthis/reference/validate_common_args.md).
  Default: `8525`.

- axes, axis_titles:

  Strings controlling how axes and axis titles are handled across
  combined plots. Passed to
  [`combine_plots()`](https://pwwang.github.io/plotthis/reference/combine_plots.md).
  See
  [`?patchwork::wrap_plots`](https://patchwork.data-imaginist.com/reference/wrap_plots.html)
  for options (`"keep"`, `"collect"`, `"collect_x"`, `"collect_y"`).

- guides:

  A string controlling guide collection across combined plots. Passed to
  [`combine_plots()`](https://pwwang.github.io/plotthis/reference/combine_plots.md).

- design:

  A custom layout specification for combined plots. Passed to
  [`combine_plots()`](https://pwwang.github.io/plotthis/reference/combine_plots.md).
  When specified, `nrow`, `ncol`, and `byrow` are ignored.

- ...:

  Additional arguments passed to
  [`clustree::clustree()`](https://lazappi.github.io/clustree/reference/clustree.html).
  Commonly used overrides include `node_size_range`, `node_text_size`,
  `layout` (default: `"sugiyama"`), `show_axis`, and `node_text_colour`.
  Note that `x` (the data) and `prefix` are set internally and cannot be
  overridden here.

## Value

A `ggplot` object (single plot), a `patchwork` object (when
`combine = TRUE` with `split_by`), or a `list` of `ggplot` objects (when
`combine = FALSE`).

## split_by Workflow (ClustreePlot)

When `split_by` is provided, the following pipeline executes:

1.  **Argument validation** —
    [`validate_common_args()`](https://pwwang.github.io/plotthis/reference/validate_common_args.md)
    checks the `seed` value and sets the random seed.

2.  **Theme resolution** —
    [`process_theme()`](https://pwwang.github.io/plotthis/reference/process_theme.md)
    resolves the `theme` string or function to a theme function.

3.  **Split column validation** —
    [`check_columns()`](https://pwwang.github.io/plotthis/reference/check_columns.md)
    resolves `split_by` with
    `force_factor = TRUE, allow_multi = TRUE, concat_multi = TRUE`.

4.  **Data splitting** — splits `data` by `split_by` levels (unused
    levels dropped), preserving factor level order.

5.  **Per-split palette / colour / legend** —
    [`check_palette()`](https://pwwang.github.io/plotthis/reference/check_palette.md),
    [`check_palcolor()`](https://pwwang.github.io/plotthis/reference/check_palcolor.md),
    and
    [`check_legend()`](https://pwwang.github.io/plotthis/reference/check_legend.md)
    resolve per-split overrides for `palette`, `palcolor`,
    `legend.position`, and `legend.direction`.

6.  **Per-split title** — when `title` is a function, it receives the
    default title (the split level name) and can return a custom string;
    otherwise `title %||% split_level` is used.

7.  **Dispatch** — each split subset is passed to
    [`ClustreePlotAtomic`](https://pwwang.github.io/plotthis/reference/ClustreePlotAtomic.md)
    with the per-split parameters.

8.  **Combination** —
    [`combine_plots()`](https://pwwang.github.io/plotthis/reference/combine_plots.md)
    assembles the list of plots via
    [`patchwork::wrap_plots`](https://patchwork.data-imaginist.com/reference/wrap_plots.html),
    honouring `nrow`/`ncol`/`byrow`/`design`.

## Examples

``` r
# \donttest{
set.seed(8525)
N <- 100
data <- data.frame(
    p.0.4 = sample(LETTERS[1:5], N, replace = TRUE),
    p.0.5 = sample(LETTERS[1:6], N, replace = TRUE),
    p.0.6 = sample(LETTERS[1:7], N, replace = TRUE),
    p.0.7 = sample(LETTERS[1:8], N, replace = TRUE),
    p.0.8 = sample(LETTERS[1:9], N, replace = TRUE),
    p.0.9 = sample(LETTERS[1:10], N, replace = TRUE),
    p.1 = sample(LETTERS[1:30], N, replace = TRUE),
    split = sample(1:2, N, replace = TRUE)
)

# --- Basic clustree plot ---
ClustreePlot(data, prefix = "p")


# --- Flipped layout (resolutions on x-axis) ---
ClustreePlot(data, prefix = "p", flip = TRUE)


# --- Split by group ---
ClustreePlot(data, prefix = "p", split_by = "split")


# --- Split by group with per-split palettes ---
ClustreePlot(data, prefix = "p", split_by = "split",
             palette = c("1" = "Set1", "2" = "Paired"))

# }
```
