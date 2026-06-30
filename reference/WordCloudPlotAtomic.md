# Word cloud plot (internal)

Core implementation for drawing a word cloud plot. This is the workhorse
behind the exported
[`WordCloudPlot`](https://pwwang.github.io/plotthis/reference/WordCloudPlot.md)
function — it takes a **single** data frame (no `split_by` support) and
returns a `ggplot` object rendered via
[`geom_text_wordcloud`](https://lepennec.github.io/ggwordcloud/reference/geom_text_wordcloud.html).

The function supports two input modes:

- **Pre-tokenized words** (`word_by`) — each row contains a single word
  (or multiple words in a list column that is unnested).

- **Sentence splitting** (`sentence_by`) — each row contains a sentence
  or phrase; the function splits the text into individual words after
  removing punctuation and lowercasing.

Words are displayed with **font size** proportional to a count variable
(`count_by`) and **colour** based on a continuous score variable
(`score_by`). Text filtering options allow removal of short words,
common stop words, and bracketed patterns.

## Usage

``` r
WordCloudPlotAtomic(
  data,
  word_by = NULL,
  sentence_by = NULL,
  count_by = NULL,
  score_by = NULL,
  count_name = NULL,
  score_name = NULL,
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
  palette = "Paired",
  palcolor = NULL,
  alpha = 1,
  palreverse = FALSE,
  aspect.ratio = 1,
  legend.position = "right",
  legend.direction = "vertical",
  title = NULL,
  subtitle = NULL,
  seed = 8525,
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

- ...:

  Additional arguments.

## Value

A `ggplot` object with `height` and `width` attributes (in inches)
attached.

## Architecture

1.  **ggplot dispatch** — selects `gglogger::ggplot` or
    [`ggplot2::ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)
    based on `getOption("plotthis.gglogger.enabled")`.

2.  **Input validation** — ensures exactly one of `word_by` or
    `sentence_by` is specified. Errors if both are provided or neither
    is provided. When `sentence_by` is used, `count_by` must be `NULL`
    (counts are derived from occurrences after splitting).

3.  **Column resolution** — `facet_by`, `count_by`, and `score_by` are
    validated and transformed via
    [`check_columns`](https://pwwang.github.io/plotthis/reference/check_columns.md).

4.  **Default values** — when `score_by` is `NULL`, a synthetic `.score`
    column with value `1` is created. When `count_by` is `NULL`, a
    synthetic `.count` column with value `1` is created.

5.  **Sentence splitting branch** (when `sentence_by` is set):

    - The `sentence_by` column is validated via
      [`check_columns`](https://pwwang.github.io/plotthis/reference/check_columns.md).

    - Text is lowercased, punctuation is removed, and the string is
      split into individual words on whitespace boundaries.

    - Words are unnested into separate rows via
      [`unnest`](https://tidyr.tidyverse.org/reference/unnest.html).

    - Data is grouped by word (and `facet_by` columns when present) and
      aggregated: `sum(count_by)` and `score_agg(score_by)`. Separate
      aggregation templates handle 0, 1, or 2 `facet_by` columns.

6.  **Word branch** (when `word_by` is set):

    - The `word_by` column is validated via
      [`check_columns`](https://pwwang.github.io/plotthis/reference/check_columns.md).

    - Column values are unnested into separate rows (supports list
      columns where a row may contain multiple words).

    - Data is grouped by word (and `facet_by` columns when present) and
      aggregated similarly to the sentence branch.

7.  **Text filtering pipeline** — the aggregated data is processed
    sequentially:

    - Words matching the pattern `[.*]` (bracketed content) are removed.

    - Words with fewer than `minchar` characters are removed.

    - Words listed in `words_excluded` are removed (case-insensitive
      matching).

    - Duplicate rows are removed.

    - The top `top_words` words by score are retained via
      [`slice_max`](https://dplyr.tidyverse.org/reference/slice.html).

8.  **Angle assignment** — each word is randomly assigned a rotation
    angle of 0 (60\\ probability), creating the characteristic word
    cloud appearance.

9.  **Colour scale preparation** —
    [`palette_this()`](https://pwwang.github.io/plotthis/reference/palette_this.md)
    with `type = "continuous"` generates a smooth gradient for the
    `score` variable. A `colors_value` sequence is created spanning
    `min(score)` to `quantile(score, 0.99)` for colour interpolation.

10. **Plot assembly** — the `ggplot` object is built with:

    - [`ggwordcloud::geom_text_wordcloud()`](https://lepennec.github.io/ggwordcloud/reference/geom_text_wordcloud.html)
      renders the words with `rm_outside = TRUE`, square shape, and
      eccentricity of 1.

    - `scale_color_gradientn()` maps the score to the continuous colour
      gradient, with a colour-bar legend (titled by `score_name` or
      `"Score"`).

    - `scale_size()` maps the count to font size within the `word_size`
      range, with a size legend (titled by `count_name` or `"Count"`).

    - `coord_flip()` swaps the axes (word cloud convention).

    - `do_call(theme, theme_args)` applies the selected theme;
      `aspect.ratio`, `legend.position`, and `legend.direction` are set
      directly.

11. **Dimension calculation** —
    [`calculate_plot_dimensions()`](https://pwwang.github.io/plotthis/reference/calculate_plot_dimensions.md)
    computes plot height and width using `base_height = 4.5`,
    `aspect.ratio`, and legend metrics. The resulting `height` / `width`
    attributes are stored on the `ggplot` object.

12. **Faceting** —
    [`facet_plot()`](https://pwwang.github.io/plotthis/reference/facet_plot.md)
    wraps the plot with `facet_wrap` or `facet_grid` if `facet_by` is
    provided, up to a maximum of 2 facet columns.
