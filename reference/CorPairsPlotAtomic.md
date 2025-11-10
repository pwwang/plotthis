# Atomic Correlation Pairs Plot

Generate a grid of scatter correlation plots for all pairs of variables.

## Usage

``` r
CorPairsPlotAtomic(
  data,
  columns = NULL,
  group_by = NULL,
  group_by_sep = "_",
  group_name = NULL,
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
  title = NULL,
  subtitle = NULL,
  facet_by = NULL,
  legend.position = "right",
  legend.direction = "vertical",
  seed = 8525,
  ...
)
```

## Arguments

- data:

  A data frame.

- columns:

  The column names of the data to be plotted. If NULL, all columns,
  except `group_by`, will be used.

- group_by:

  The column name of the data to be used for grouping. Different groups
  will be plotted in different colors.

- group_by_sep:

  The separator used to concatenate multiple columns in `group_by`.

- group_name:

  The name of the group in the legend.

- diag_type:

  The type of the diagonal plots. Available types: "density", "violin",
  "histogram", "box", "none".

- diag_args:

  A list of additional arguments to be passed to the diagonal plots.

- layout:

  The layout of the plots. Available layouts: ".\\, "\\", "/.", "./".

  - '\\ or '/' means the diagonal plots are on the top-left to
    bottom-right diagonal.

  - '.' means where the scatter plots are.

- cor_method:

  The method to calculate the correlation. Available methods: "pearson",
  "spearman", "kendall". The correlation will be shown in the other
  triangle of the scatter plots.

- cor_palette:

  The color palette for the correlation tile plots.

- cor_palcolor:

  Custom colors used to create a color palette for the correlation tile
  plots.

- cor_size:

  The size of the correlation text.

- cor_format:

  The format of the correlation text. Default is "corr: %.2f". It will
  be formatted using `sprintf(cor_format, corr)`.

- cor_fg:

  The color of the correlation text.

- cor_bg:

  The background color of the correlation text.

- cor_bg_r:

  The radius of the background of the correlation text.

- theme:

  A character string or a theme class (i.e. ggplot2::theme_classic)
  specifying the theme to use. Default is "theme_this".

- theme_args:

  A list of arguments to pass to the theme function.

- palette:

  The color palette for the scatter plots and default palette for the
  diagonal plots.

- palcolor:

  Custom colors used to create a color palette for the scatter plots and
  diagonal plots.

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

  The random seed to use. Default is 8525.

- ...:

  Additional arguments to pass to
  [`CorPlot`](https://pwwang.github.io/plotthis/reference/CorPlot.md).

## Value

A `patch_work::wrap_plots` object.

## Details

`theme` and `theme_args` are also supported, they will be passed to each
individual plot.
