# Correlation pairs (scatterplot matrix)

Draws a grid of pairwise scatter plots for selected numeric columns,
arranged in a scatterplot matrix layout. The upper or lower triangle
displays correlation tiles while the opposite triangle shows scatter
plots with regression lines. Diagonal cells can show density plots,
violin plots, histograms, box plots, or a simple diagonal line.

**NOTE:** The `facet_by` parameter is **not supported** in CorPairsPlot
(an error is raised if provided). Use `split_by` instead to create
separate correlation pair matrices per group.

The function supports **four layout orientations** (`layout`), **three
correlation methods**, configurable diagonal plots via other plotthis
functions, custom correlation tile formatting, and splitting into
separate sub-plots via `split_by`.

## Usage

``` r
CorPairsPlot(
  data,
  columns = NULL,
  group_by = NULL,
  group_by_sep = "_",
  group_name = NULL,
  split_by = NULL,
  split_by_sep = "_",
  diag_type = NULL,
  diag_args = list(),
  layout = c(".\\", "\\.", "/.", "./"),
  cor_method = c("pearson", "spearman", "kendall"),
  cor_palette = "RdBu",
  cor_palcolor = NULL,
  cor_size = 3,
  cor_format = "corr: {round(corr, 2)}",
  cor_fg = "black",
  cor_bg = "white",
  cor_bg_r = 0.1,
  theme = "theme_this",
  theme_args = list(),
  palette = ifelse(is.null(group_by), "Spectral", "Paired"),
  palcolor = NULL,
  palreverse = FALSE,
  title = NULL,
  subtitle = NULL,
  facet_by = NULL,
  legend.position = "right",
  legend.direction = "vertical",
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
)
```

## Arguments

- data:

  A data frame.

- columns:

  A character vector of column names to include in the pairs plot. When
  `NULL` (default), all columns except `group_by` are used. At least two
  columns are required.

- group_by:

  Columns to group the data for plotting For those plotting functions
  that do not support multiple groups, They will be concatenated into
  one column, using `group_by_sep` as the separator

- group_by_sep:

  The separator for multiple group_by columns. See `group_by`

- group_name:

  A character string used as the colour legend title in the scatter
  plots. When `NULL`, the `group_by` column name is used.

- split_by:

  The column(s) to split the data by and produce separate sub-plots.
  Multiple columns are concatenated with `split_by_sep`.

- split_by_sep:

  A character string to separate concatenated `split_by` columns.
  Default `"_"`.

- diag_type:

  A character string specifying the plot type for diagonal cells. One of
  `"density"`, `"violin"`, `"histogram"`, `"box"`, or `"none"` (diagonal
  line). Default: `"density"` (no `group_by`) or `"violin"` (with
  `group_by`).

- diag_args:

  A named list of additional arguments passed to the diagonal plot
  function
  ([`DensityPlot`](https://pwwang.github.io/plotthis/reference/densityhistoplot.md),
  [`ViolinPlot`](https://pwwang.github.io/plotthis/reference/boxviolinplot.md),
  [`Histogram`](https://pwwang.github.io/plotthis/reference/densityhistoplot.md),
  or
  [`BoxPlot`](https://pwwang.github.io/plotthis/reference/boxviolinplot.md)).
  Default: [`list()`](https://rdrr.io/r/base/list.html).

- layout:

  A character string specifying the layout orientation. One of the
  following codes (dot = scatter, backslash/slash = diagonal): `.\`,
  `\\.`, `/.`, `./`. Default: `.\`.

- cor_method:

  A character string specifying the correlation method for the fill
  tiles. One of `"pearson"`, `"spearman"`, `"kendall"`. Default:
  `"pearson"`.

- cor_palette:

  A character string specifying the colour palette for the correlation
  fill tiles. Default: `"RdBu"`.

- cor_palcolor:

  A character vector of custom colours used to create the correlation
  tile palette. When `NULL`, the palette's default colours are used.

- cor_size:

  A numeric value specifying the font size of the correlation text in
  the fill tiles. Default: `3`.

- cor_format:

  A character string specifying a glue template for formatting the
  correlation text. The template is evaluated by
  [`glue::glue()`](https://glue.tidyverse.org/reference/glue.html) with
  access to `corr` (the correlation value), `x`, and `y` (the column
  names). Default: `"corr: \{round(corr, 2)\}"`.

- cor_fg:

  A character string specifying the colour of the correlation text.
  Default: `"black"`.

- cor_bg:

  A character string specifying the background colour of the correlation
  text boxes. Default: `"white"`.

- cor_bg_r:

  A numeric value specifying the corner radius of the correlation text
  background boxes. Default: `0.1`.

- theme:

  A character string or a theme class (i.e. ggplot2::theme_classic)
  specifying the theme to use. Default is "theme_this".

- theme_args:

  A list of arguments to pass to the theme function.

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

- title:

  A character string specifying the title of the plot. A function can be
  used to generate the title based on the default title. This is useful
  when split_by is used and the title needs to be dynamic.

- subtitle:

  A character string specifying the subtitle of the plot.

- facet_by:

  A character string specifying the column name of the data frame to
  facet the plot. Otherwise, the data will be split by `split_by` and
  generate multiple plots and combine them into one using
  [`patchwork::wrap_plots`](https://patchwork.data-imaginist.com/reference/wrap_plots.html)

- legend.position:

  A character string specifying the position of the legend. if
  `waiver()`, for single groups, the legend will be "none", otherwise
  "right".

- legend.direction:

  A character string specifying the direction of the legend.

- seed:

  A numeric seed for reproducibility.

- combine:

  Logical; when `TRUE` (default), returns a combined `patchwork` object.
  When `FALSE`, returns a named list of individual `patchwork` objects.

- ncol, nrow:

  Integer number of columns / rows for the combined layout.

- byrow:

  Logical; fill the combined layout by row. Default `TRUE`.

- axes:

  A character string specifying how axes should be treated across the
  combined layout.

- axis_titles:

  A character string specifying how axis titles should be treated across
  the combined layout. Defaults to `axes`.

- guides:

  A character string specifying how guides (legends) should be collected
  across panels.

- design:

  A custom layout design for the combined plot.

- ...:

  Additional arguments.

## Value

A `patchwork` object (when `combine = TRUE`) or a named list of
`patchwork` objects (when `combine = FALSE`), each with `height` and
`width` attributes in inches.

## split_by workflow

When `split_by` is provided:

1.  The `split_by` column is validated via
    [`check_columns()`](https://pwwang.github.io/plotthis/reference/check_columns.md)
    with `force_factor = TRUE`. Empty levels are dropped.

2.  The data frame is split by `split_by` (preserving level order). If
    `split_by` is `NULL`, the data is wrapped in a single-element list
    with name `"..."`. The `split_by` column is removed from each
    split's data before plotting.

3.  Per-split `palette`, `palcolor`, `legend.position`, and
    `legend.direction` are resolved via
    [`check_palette()`](https://pwwang.github.io/plotthis/reference/check_palette.md),
    [`check_palcolor()`](https://pwwang.github.io/plotthis/reference/check_palcolor.md),
    and
    [`check_legend()`](https://pwwang.github.io/plotthis/reference/check_legend.md).

4.  [`CorPairsPlotAtomic()`](https://pwwang.github.io/plotthis/reference/CorPairsPlotAtomic.md)
    is called for each split. When `title` is a function, it receives
    the split level name and can generate dynamic titles.

5.  Results are combined via
    [`combine_plots()`](https://pwwang.github.io/plotthis/reference/combine_plots.md)
    (when `combine = TRUE`) or returned as a named list.

## Examples

``` r
# \donttest{
set.seed(8525)
data <- data.frame(x = rnorm(100))
data$y <- rnorm(100, 10, sd = 0.5)
data$z <- -data$x + data$y + rnorm(100, 20, 1)
data$g <- sample(1:4, 100, replace = TRUE)

# Histogram diagonal, slash layout
CorPairsPlot(data, diag_type = "histogram",
    diag_args = list(bins = 30, palette = "Paired"),
    layout = "/.")


# No diagonal with axis title styling
CorPairsPlot(data, group_by = "g", diag_type = "none", layout = "./",
    theme_args = list(axis.title = element_textbox(
        color = "black", box.color = "grey20", size = 16, halign = 0.5,
        fill = "grey90", linetype = 1,
        width = grid::unit(1, "npc"),
        padding = ggplot2::margin(5, 5, 5, 5))))
#> Warning: no non-missing arguments to max; returning -Inf


# Violin diagonal with custom format
CorPairsPlot(data, group_by = "g", diag_type = "violin", layout = "\\.",
    cor_format = "{x}\n{y}\ncorr: {round(corr, 2)}")
#> Warning: no non-missing arguments to max; returning -Inf


# Per-split with bottom legend
CorPairsPlot(data, split_by = "g", diag_type = "none", layout = ".\\",
    legend.position = "bottom", legend.direction = "horizontal",
    group_name = "group")


# Per-split with custom palette colours
CorPairsPlot(data, split_by = "g",
    palcolor = list("1" = "red", "2" = "blue", "3" = "green",
                    "4" = "yellow"))

# }
```
