# Atomic Manhattan plot (internal)

Core implementation for drawing a GWAS-style Manhattan plot. This is the
internal workhorse dispatched by the exported
[`ManhattanPlot`](https://pwwang.github.io/plotthis/reference/ManhattanPlot.md)
function — it takes a **single** data frame (no `split_by` support) and
returns a `ggplot` object. The plot displays genetic association
p-values across chromosomes, with \\-\log\_{10}(p)\\ on the y-axis and
genomic position on the x-axis. Each chromosome is rendered in
alternating colours, and configurable horizontal dashed lines mark
genome-wide significance thresholds.

The function is adapted from
[`ggmanh::manhattan_plot()`](https://rdrr.io/pkg/ggmanh/man/manhattan_plot.html)
with the following enhancements:

- Dot-separated argument names are converted to underscores (e.g.
  `chr.colname` \\\rightarrow\\ `chr_by`).

- `chromosomes` merges the original `chromosome` and `chr.order`
  arguments into a single parameter for subsetting and reordering.

- `highlight` accepts index vectors or R expressions (via a character
  string) instead of a column name.

- Dedicated `pt_*`, `label_*`, and `highlight_*` parameter families give
  granular control over point appearance, label styling, and highlight
  styling.

- `pval_transform` accepts any function (or a character string parsed as
  a function) rather than a fixed log-transform toggle.

Key features include per-chromosome alternating colours, configurable
significance threshold lines with labels, optional data thinning for
dense SNP sets, automatic y-axis rescaling (broken-axis) when a small
number of highly significant points would otherwise compress the
majority of the data, and support for highlighting and labeling specific
variants.

## Usage

``` r
ManhattanPlotAtomic(
  data,
  chr_by,
  pos_by,
  pval_by,
  label_by = NULL,
  chromosomes = NULL,
  pt_size = 0.75,
  pt_color = NULL,
  pt_alpha = alpha,
  pt_shape = 19,
  label_size = 3,
  label_fg = NULL,
  highlight = NULL,
  highlight_color = NULL,
  highlight_size = 1.5,
  highlight_alpha = 1,
  highlight_shape = 19,
  preserve_position = TRUE,
  chr_gap_scaling = 1,
  pval_transform = "-log10",
  signif = c(5e-08, 1e-05),
  signif_color = NULL,
  signif_rel_pos = 0.2,
  signif_label = TRUE,
  signif_label_size = 3.5,
  signif_label_pos = c("left", "right"),
  thin = NULL,
  thin_n = 1000,
  thin_bins = 200,
  rescale = TRUE,
  rescale_ratio_threshold = 5,
  palette = "Dark2",
  palcolor = NULL,
  palreverse = FALSE,
  alpha = 1,
  theme = "theme_this",
  theme_args = list(),
  title = NULL,
  subtitle = NULL,
  xlab = NULL,
  ylab = expression("-" * log[10](p)),
  ...
)
```

## Arguments

- data:

  A data frame.

- chr_by:

  A character string specifying the column name for chromosome
  identifiers. Default: `"chr"`.

- pos_by:

  A character string specifying the column name for genomic positions
  (integer or numeric). Default: `"pos"`.

- pval_by:

  A character string specifying the column name for p-values (numeric).
  Default: `"pval"`.

- label_by:

  A character string specifying the column name for variant labels. Only
  variants with non-empty values in this column will be labelled.
  Default: `NULL` (no labels).

- chromosomes:

  A character or numeric vector specifying which chromosomes to include
  and/or their display order. When `NULL` (the default), all chromosomes
  present in the data are plotted in their natural factor order. A
  single value filters to that chromosome; a vector reorders and
  subsets.

- pt_size:

  A numeric value specifying the size of the points. Default: `0.75`.

- pt_color:

  A character string specifying a single colour for all background
  (non-highlighted) points. When `NULL` (the default), alternating
  chromosome colours from `palette` / `palcolor` are used. Typically set
  to `"grey80"` when `highlight` is used with a distinct
  `highlight_color`.

- pt_alpha:

  A numeric value in `[0, 1]` specifying the transparency of the points.
  Default: `alpha` (aliased parameter).

- pt_shape:

  A numeric value specifying the shape of the points. Default: `19`
  (filled circle).

- label_size:

  A numeric value specifying the font size of the variant labels.
  Default: `3`.

- label_fg:

  A character string specifying the colour of the variant labels. When
  `NULL` (the default), each label inherits the colour of its
  corresponding point.

- highlight:

  Either a numeric vector of row indices or a character string
  containing an R expression (parsed via
  [`rlang::parse_expr()`](https://rlang.r-lib.org/reference/parse_expr.html))
  to select variants to highlight. Default: `NULL` (no highlighting).

- highlight_color:

  A character string specifying the colour of highlighted points. When
  `NULL` (the default), highlighted points inherit the chromosome colour
  from the underlying `geom_point()` layer.

- highlight_size:

  A numeric value specifying the size of highlighted points. Default:
  `1.5`.

- highlight_alpha:

  A numeric value in `[0, 1]` specifying the transparency of highlighted
  points. Default: `1`.

- highlight_shape:

  A numeric value specifying the shape of highlighted points. Default:
  `19` (filled circle).

- preserve_position:

  A logical value. When `TRUE` (the default), the width of each
  chromosome segment reflects its number of variants and variant
  positions are correctly scaled. When `FALSE`, all chromosomes have
  equal width and variants are equally spaced.

- chr_gap_scaling:

  A numeric scaling factor for the gap between chromosomes. Larger
  values increase the gap. Default: `1`.

- pval_transform:

  A function or character string that can be evaluated to a function for
  transforming p-values. Default: `"-log10"`, which computes
  \\-\log\_{10}(p)\\. Other examples: `"-log2"` or a custom
  `function(x) -log10(x)`.

- signif:

  A numeric vector of significance thresholds to draw as horizontal
  dashed lines. Default: `c(5e-8, 1e-5)`.

- signif_color:

  A character vector of colours for the significance threshold lines, of
  equal length as `signif`. When `NULL` (the default), the smallest
  threshold is coloured black and the rest grey.

- signif_rel_pos:

  A numeric value between `0.1` and `0.9` specifying the relative
  position of the y-axis jump when rescaling is active. Default: `0.2`.

- signif_label:

  A logical value. When `TRUE` (the default), significance threshold
  values are annotated on the plot.

- signif_label_size:

  A numeric value for the font size of the significance threshold
  labels. Default: `3.5`.

- signif_label_pos:

  A character string specifying where to place the significance
  threshold labels: `"left"` (default) or `"right"`.

- thin:

  A logical value indicating whether to thin dense data by sampling
  points per horizontal partition. Defaults to `TRUE` when `chromosomes`
  selects fewer chromosomes than in the data, and `FALSE` otherwise.

- thin_n:

  An integer specifying the maximum number of points per horizontal
  partition after thinning. Default: `1000`.

- thin_bins:

  An integer specifying the number of horizontal bins for thinning.
  Default: `200`.

- rescale:

  A logical value. When `TRUE` (the default), the y-axis is
  automatically rescaled (broken axis) if extreme significance values
  would otherwise compress the main data cloud.

- rescale_ratio_threshold:

  A numeric threshold for triggering y-axis rescaling. The ratio is
  computed as `ceiling(max(log10pval) / 5) * 5 / signif_jump`. Default:
  `5`.

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

- xlab:

  A character string specifying the x-axis label.

- ylab:

  A character string specifying the y-axis label.

- ...:

  Additional arguments.

## Value

A `ggplot` object with `height` and `width` attributes (in inches)
attached.

## Architecture

**ManhattanPlotAtomic** executes the following steps:

1.  **ggplot dispatch** — selects `gglogger::ggplot` or
    [`ggplot2::ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)
    based on `getOption("plotthis.gglogger.enabled")`.

2.  **signif_label_pos normalisation** —
    [`match.arg()`](https://rdrr.io/r/base/match.arg.html) resolves
    `signif_label_pos` to `"left"` or `"right"`.

3.  **Data preprocessing** —
    [`ggmanh::manhattan_data_preprocess()`](https://rdrr.io/pkg/ggmanh/man/manhattan_data_preprocess.html)
    performs chromosome ordering, optional thinning (`thin_n` points per
    `thin_bins` horizontal partitions), chromosome gap scaling
    (`chr_gap_scaling`), position preservation (`preserve_position`),
    and significance threshold colour assignment. When `chromosomes` is
    a single value, it is passed as the `chromosome` filter; otherwise
    it is used for ordering via `chr.order`.

4.  **label_by validation** —
    [`check_columns()`](https://pwwang.github.io/plotthis/reference/check_columns.md)
    validates and factors the `label_by` column if provided.

5.  **Chromosome colour assignment** — if `highlight` and
    `highlight_color` are both set, or if `pt_color` is given,
    `pt_color` (defaulting to `"grey80"`) is used as the base colour for
    all chromosomes; otherwise
    [`palette_this()`](https://pwwang.github.io/plotthis/reference/palette_this.md)
    generates distinct colours per chromosome from `palette` /
    `palcolor`.

6.  **pval_transform resolution** — when a character string starting
    with `"-"` (e.g. `"-log10"`), the minus sign is stripped, the
    remainder is evaluated as a function, and the result is negated.
    Plain character strings are evaluated directly. The result is
    applied to the p-value column to produce `log10pval`.

7.  **Y-axis rescaling check** — when `rescale = TRUE`, the ratio of the
    ceiling-scaled maximum to the significance-threshold jump is checked
    against `rescale_ratio_threshold`. If the ratio exceeds the
    threshold, a broken y-axis is constructed via
    `ggmanh::get_transform()` that compresses the empty space between
    the main data and the extreme points.

8.  **Single-chromosome branch** — when only one chromosome is present,
    the original position column is used directly, x-axis breaks and
    labels use `waiver()`, and the x-axis label is set to the chromosome
    name (or `"Chromosome <name>"`).

9.  **Multi-chromosome branch** — position coordinates are recalculated
    across chromosome boundaries via `ggmanh::calc_new_pos_()`.
    Chromosome centre positions serve as x-axis breaks, chromosome
    labels are displayed, and limits span the full genomic range.

10. **Base plot assembly** — creates a `ggplot` object with
    `geom_point()` (chromosome-coloured dots), `scale_color_manual()`,
    `scale_y_continuous()` (with optional broken-axis transform),
    `scale_x_continuous()`, `geom_hline()` for significance thresholds,
    the resolved theme (with hidden grid lines and suppressed legend),
    and `labs()`.

11. **Significance labels** — when `signif_label = TRUE`, `geom_text()`
    annotates each significance threshold at the left or right edge of
    the plot (controlled by `signif_label_pos`), coloured by
    `signif_color` (smallest threshold in black, others in grey by
    default).

12. **Variant labels** — when `label_by` is provided,
    [`ggrepel::geom_label_repel()`](https://ggrepel.slowkow.com/reference/geom_text_repel.html)
    adds labels for variants that have non-empty values in the
    `label_by` column. If `label_fg` is set, all labels receive that
    colour; otherwise each label inherits the chromosome colour of its
    point.

13. **Rescale tick marks** — when y-axis rescaling is active, the axis
    tick style is changed (via `axis.ticks.y`) and a double-equals
    annotation is placed at the jump point to indicate the axis break,
    with `coord_cartesian(clip = "off")`.

14. **Highlight overlay** — when `highlight` is numeric (row indices) or
    a character string (R expression), the matching points are overlaid
    with a separate `geom_point()` layer styled by `highlight_*`
    parameters, optionally in a distinct colour when `highlight_color`
    is specified.

15. **Dimension calculation** —
    [`calculate_plot_dimensions()`](https://pwwang.github.io/plotthis/reference/calculate_plot_dimensions.md)
    computes height and width from the number of chromosomes
    (`base_height = 4.5`, `x_scale_factor = 0.4`), and stores them as
    `height` / `width` attributes on the plot.
