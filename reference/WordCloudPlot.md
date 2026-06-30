# Word cloud plot

Draws a word cloud plot that visualises word frequency and importance.
Words are displayed with font size proportional to a count variable and
colour based on a continuous score variable, using
[`geom_text_wordcloud`](https://lepennec.github.io/ggwordcloud/reference/geom_text_wordcloud.html)
for rendering.

The function supports **pre-tokenised words** (via `word_by`) and
**sentence splitting** (via `sentence_by`). Sentences are automatically
lowercased, stripped of punctuation, and split into individual words
before aggregation. Common stop words can be excluded via
`words_excluded`, and the number of displayed words is controlled by
`top_words`.

The word cloud can be **faceted** (via `facet_by`) or **split** into
separate sub-plots via `split_by`. When `split_by` is used, each split
level receives its own word cloud, and the results are combined into a
single layout via
[`combine_plots`](https://pwwang.github.io/plotthis/reference/combine_plots.md).

## Usage

``` r
WordCloudPlot(
  data,
  word_by = NULL,
  sentence_by = NULL,
  count_by = NULL,
  score_by = NULL,
  count_name = NULL,
  score_name = NULL,
  split_by = NULL,
  split_by_sep = "_",
  words_excluded = plotthis::words_excluded,
  score_agg = mean,
  minchar = 2,
  word_size = c(2, 8),
  top_words = 100,
  facet_by = NULL,
  facet_scales = "fixed",
  facet_ncol = NULL,
  facet_nrow = NULL,
  facet_byrow = TRUE,
  theme = "theme_this",
  theme_args = list(),
  palette = "Spectral",
  palcolor = NULL,
  alpha = 1,
  palreverse = FALSE,
  aspect.ratio = 1,
  legend.position = "right",
  legend.direction = "vertical",
  title = NULL,
  subtitle = NULL,
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

- word_by:

  A character string specifying the column name containing pre-tokenized
  words. A character column is expected. Use this when your data already
  has one word per row (or a list of words per row). Mutually exclusive
  with `sentence_by`.

- sentence_by:

  A character string specifying the column name containing sentences or
  phrases to be split into individual words. A character column is
  expected. The text is lowercased and punctuation is removed before
  splitting on whitespace boundaries. Mutually exclusive with `word_by`.

- count_by:

  A character string specifying the numeric column for the count or
  frequency of each word. When `NULL` (the default), each occurrence
  counts as 1. Must be `NULL` when `sentence_by` is used, as counts are
  derived from the number of occurrences after splitting.

- score_by:

  A character string specifying the numeric column for the score of each
  word, mapped to the text colour via a continuous gradient. When `NULL`
  (the default), all words receive a score of 1 and are coloured at the
  low end of the palette.

- count_name:

  A character string for the size legend title. When `NULL` (the
  default), `"Count"` is used.

- score_name:

  A character string for the colour-bar legend title. When `NULL` (the
  default), `"Score"` is used.

- split_by:

  The column(s) to split data by and plot separately.

- split_by_sep:

  The separator for multiple split_by columns. See `split_by`

- words_excluded:

  A character vector of words to exclude from the word cloud. Matching
  is case-insensitive. Defaults to
  [`plotthis::words_excluded`](https://pwwang.github.io/plotthis/reference/words_excluded.md),
  a built-in set of common English stop words.

- score_agg:

  A function to aggregate the scores when multiple observations of the
  same word exist. Default is `mean`. Other options include `sum`,
  `median`, or a custom function.

- minchar:

  A numeric value specifying the minimum number of characters a word
  must have to be included. Words with fewer characters are filtered
  out. Default: `2`.

- word_size:

  A numeric vector of length 2 specifying the range of font sizes
  (in mm) for the words. Passed to `scale_size(range = word_size)`.
  Default: `c(2, 8)`.

- top_words:

  A numeric value specifying the maximum number of words to display,
  selected by highest score. Default: `100`.

- facet_by:

  A character string specifying the column name of the data frame to
  facet the plot. Otherwise, the data will be split by `split_by` and
  generate multiple plots and combine them into one using
  [`patchwork::wrap_plots`](https://patchwork.data-imaginist.com/reference/wrap_plots.html)

- facet_scales:

  Whether to scale the axes of facets. Default is "fixed" Other options
  are "free", "free_x", "free_y". See
  [`ggplot2::facet_wrap`](https://ggplot2.tidyverse.org/reference/facet_wrap.html)

- facet_ncol:

  A numeric value specifying the number of columns in the facet. When
  facet_by is a single column and facet_wrap is used.

- facet_nrow:

  A numeric value specifying the number of rows in the facet. When
  facet_by is a single column and facet_wrap is used.

- facet_byrow:

  A logical value indicating whether to fill the plots by row. Default
  is TRUE.

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

- alpha:

  A numeric value specifying the transparency of the plot.

- palreverse:

  A logical value indicating whether to reverse the palette. Default is
  FALSE.

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

- seed:

  The random seed to use. Default is 8525.

- combine:

  Whether to combine the plots into one when facet is FALSE. Default is
  TRUE.

- nrow:

  A numeric value specifying the number of rows in the facet.

- ncol:

  A numeric value specifying the number of columns in the facet.

- byrow:

  A logical value indicating whether to fill the plots by row.

- axes:

  A string specifying how axes should be treated. Passed to
  [`patchwork::wrap_plots()`](https://patchwork.data-imaginist.com/reference/wrap_plots.html).
  Only relevant when `split_by` is used and `combine` is TRUE. Options
  are:

  - 'keep' will retain all axes in individual plots.

  - 'collect' will remove duplicated axes when placed in the same run of
    rows or columns of the layout.

  - 'collect_x' and 'collect_y' will remove duplicated x-axes in the
    columns or duplicated y-axes in the rows respectively.

- axis_titles:

  A string specifying how axis titltes should be treated. Passed to
  [`patchwork::wrap_plots()`](https://patchwork.data-imaginist.com/reference/wrap_plots.html).
  Only relevant when `split_by` is used and `combine` is TRUE. Options
  are:

  - 'keep' will retain all axis titles in individual plots.

  - 'collect' will remove duplicated titles in one direction and merge
    titles in the opposite direction.

  - 'collect_x' and 'collect_y' control this for x-axis titles and
    y-axis titles respectively.

- guides:

  A string specifying how guides should be treated in the layout. Passed
  to
  [`patchwork::wrap_plots()`](https://patchwork.data-imaginist.com/reference/wrap_plots.html).
  Only relevant when `split_by` is used and `combine` is TRUE. Options
  are:

  - 'collect' will collect guides below to the given nesting level,
    removing duplicates.

  - 'keep' will stop collection at this level and let guides be placed
    alongside their plot.

  - 'auto' will allow guides to be collected if a upper level tries, but
    place them alongside the plot if not.

- design:

  Specification of the location of areas in the layout, passed to
  [`patchwork::wrap_plots()`](https://patchwork.data-imaginist.com/reference/wrap_plots.html).
  Only relevant when `split_by` is used and `combine` is TRUE. When
  specified, `nrow`, `ncol`, and `byrow` are ignored. See
  [`patchwork::wrap_plots()`](https://patchwork.data-imaginist.com/reference/wrap_plots.html)
  for more details.

- ...:

  Additional arguments.

## Value

A `ggplot` object (when no `split_by` is used), a `patchwork` object
(when `combine = TRUE` and `split_by` is used), or a named list of
`ggplot` objects (when `combine = FALSE`), each with `height` and
`width` attributes in inches.

## split_by workflow

When `split_by` is provided:

1.  [`validate_common_args()`](https://pwwang.github.io/plotthis/reference/validate_common_args.md)
    validates the `seed` and `facet_by` constraints (maximum 2 facet
    columns).

2.  The `theme` argument is resolved via
    [`process_theme()`](https://pwwang.github.io/plotthis/reference/process_theme.md).

3.  The `split_by` column(s) are validated and transformed via
    [`check_columns()`](https://pwwang.github.io/plotthis/reference/check_columns.md)
    with `force_factor = TRUE` and `concat_multi = TRUE`.

4.  The data frame is split by `split_by` (preserving factor level
    order). If `split_by` is `NULL`, the data is wrapped in a
    single-element list with name `"..."`.

5.  Per-split `palette`, `palcolor`, `legend.position`, and
    `legend.direction` are resolved via
    [`check_palette()`](https://pwwang.github.io/plotthis/reference/check_palette.md),
    [`check_palcolor()`](https://pwwang.github.io/plotthis/reference/check_palcolor.md),
    and
    [`check_legend()`](https://pwwang.github.io/plotthis/reference/check_legend.md).

6.  [`WordCloudPlotAtomic()`](https://pwwang.github.io/plotthis/reference/WordCloudPlotAtomic.md)
    is called for each split. If `title` is a function, it receives the
    split level name and can generate dynamic titles per sub-plot.

7.  Results are combined via
    [`combine_plots()`](https://pwwang.github.io/plotthis/reference/combine_plots.md)
    (when `combine = TRUE`) or returned as a named list (when
    `combine = FALSE`).

## Examples

``` r
# \donttest{
set.seed(8525)
data <- data.frame(
    word = c("apple", "banana", "cherry", "date", "elderberry",
             "fig", "grape", "honeydew", "kiwi", "lemon"),
    count = c(10, 20, 30, 40, 50, 15, 25, 35, 45, 55),
    score = c(1, 2, 3, 4, 5, 1.5, 2.5, 3.5, 4.5, 5.5),
    facet = rep(c("Group1", "Group2"), each = 5),
    split = rep(c("A", "B"), 5)
)

# Basic word cloud with word, count, and score columns
WordCloudPlot(data, word_by = "word",
              count_by = "count", score_by = "score")


# Word cloud using sentence_by (sentences split into words)
data_sent <- data.frame(
    sentence = c("The quick brown fox jumps over the lazy dog",
                 "A quick brown dog jumps over a lazy fox"),
    score = c(10, 5)
)
WordCloudPlot(data_sent, sentence_by = "sentence", score_by = "score")


# Word cloud with faceting
WordCloudPlot(data, word_by = "word",
              count_by = "count", score_by = "score",
              facet_by = "facet")


# Word cloud split by a grouping variable
WordCloudPlot(data, word_by = "word",
              count_by = "count", score_by = "score",
              split_by = "split")

# }
```
