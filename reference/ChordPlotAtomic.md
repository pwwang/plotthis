# Atomic chord plot (internal)

Core implementation for drawing a chord diagram using
[`circlize::chordDiagram()`](https://rdrr.io/pkg/circlize/man/chordDiagram.html).
This is the workhorse behind the exported
[`ChordPlot`](https://pwwang.github.io/plotthis/reference/chordplot.md)
function — it takes a **single** data frame (no `split_by` support) and
returns a `patchwork` wrapped element.

Chord diagrams visualise relationships (flows) between two sets of
categories arranged around a circle. The width of each link is
proportional to the value (`y`) connecting its source and target nodes.

## Usage

``` r
ChordPlotAtomic(
  data,
  y = NULL,
  from = NULL,
  from_sep = "_",
  to = NULL,
  to_sep = "_",
  flip = FALSE,
  links_color = c("from", "to"),
  theme = "theme_this",
  theme_args = list(),
  palette = "Paired",
  palcolor = NULL,
  palreverse = FALSE,
  alpha = 0.5,
  labels_rot = FALSE,
  title = NULL,
  subtitle = NULL,
  keep_na = FALSE,
  keep_empty = FALSE,
  ...
)
```

## Arguments

- data:

  A data frame.

- y:

  A character string specifying the numeric column whose values
  determine link thickness. When `NULL`, the count of observations per
  (`from`, `to`) pair is used.

- from:

  A character string (or vector) specifying the column name(s) for the
  source nodes. Character/factor columns are expected. Multiple columns
  are concatenated with `from_sep`.

- from_sep:

  A character string to join multiple `from` columns. Default `"_"`.

- to:

  A character string (or vector) specifying the column name(s) for the
  target nodes. Character/factor columns are expected. Multiple columns
  are concatenated with `to_sep`.

- to_sep:

  A character string to join multiple `to` columns. Default `"_"`.

- flip:

  Logical; if `TRUE`, swap the source and target nodes, reversing the
  link direction.

- links_color:

  A character string controlling which node's colour each link ribbon
  takes: `"from"` (default) or `"to"`.

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

- alpha:

  Numeric transparency for the link ribbons (0–1). Default `0.5`.

- labels_rot:

  Logical; if `TRUE`, rotate sector labels by 90 degrees (clockwise).
  Default `FALSE` uses `niceFacing` for automatic orientation.

- title:

  A character string specifying the title of the plot. A function can be
  used to generate the title based on the default title. This is useful
  when split_by is used and the title needs to be dynamic.

- subtitle:

  A character string specifying the subtitle of the plot.

- keep_na:

  A logical value or a character to replace the NA values in the data.
  It can also take a named list to specify different behavior for
  different columns. If TRUE or NA, NA values will be replaced with NA.
  If FALSE, NA values will be removed from the data before plotting. If
  a character string is provided, NA values will be replaced with the
  provided string. If a named vector/list is provided, the names should
  be the column names to apply the behavior to, and the values should be
  one of TRUE, FALSE, or a character string. Without a named
  vector/list, the behavior applies to categorical/character columns
  used on the plot, for example, the `x`, `group_by`, `fill_by`, etc.

- keep_empty:

  One of FALSE, TRUE and "level". It can also take a named list to
  specify different behavior for different columns. Without a named
  list, the behavior applies to the categorical/character columns used
  on the plot, for example, the `x`, `group_by`, `fill_by`, etc.

  - `FALSE` (default): Drop empty factor levels from the data before
    plotting.

  - `TRUE`: Keep empty factor levels and show them as a separate
    category in the plot.

  - `"level"`: Keep empty factor levels, but do not show them in the
    plot. But they will be assigned colors from the palette to maintain
    consistency across multiple plots. Alias: `levels`

- ...:

  Additional arguments.

## Value

A `patchwork` wrapped element with `height` and `width` attributes (in
inches) attached. The original data is stored in the `p$data` field.

## Architecture

1.  **Column resolution** — `from`, `to`, and `y` are validated and
    transformed via
    [`check_columns`](https://pwwang.github.io/plotthis/reference/check_columns.md).
    Multi-column `from` and `to` are concatenated with their respective
    separators (`from_sep`, `to_sep`).

2.  **Count aggregation** — when `y = NULL`, the count of observations
    per (`from`, `to`) pair is computed as a new `.y` column. Factor
    levels from the original data are preserved.

3.  **NA / empty-level handling** —
    [`process_keep_na_empty()`](https://pwwang.github.io/plotthis/reference/process_keep_na_empty.md)
    applies `keep_na` and `keep_empty` policies.

4.  **Flip** — when `flip = TRUE`, the `from` and `to` columns are
    swapped, effectively reversing the link direction.

5.  **Colour mapping** —
    [`palette_this()`](https://pwwang.github.io/plotthis/reference/palette_this.md)
    assigns colours to all unique `from` and `to` values (the grid
    sectors). `NA` values are mapped to a literal `"NA"` level and
    assigned a colour from the palette.

6.  **Link colour resolution** — `links_color` controls whether each
    connecting ribbon takes its colour from the `"from"` side (default)
    or the `"to"` side.

7.  **circlize rendering** — two rendering paths exist, selected by
    `labels_rot`:

    - **Horizontal labels** (`labels_rot = FALSE`): track 1 has a fixed
      height of 1 mm and uses `niceFacing = TRUE` for automatic label
      rotation. Track 2 adds axis ticks on the outside for wide-enough
      sectors.

    - **Rotated labels** (`labels_rot = TRUE`): track 1 height is
      computed from the maximum string width of all node names, and
      labels are rendered with `facing = "clockwise"`. Track 2 adds axis
      ticks.

    Links use arrow heads (`link.arr.type = "big.arrow"`) and
    differentiated height
    (`direction.type = c("diffHeight", "arrows")`). Both tracks set
    `bg.border = NA` to prevent border lines from appearing between
    sectors.

8.  **Patchwork integration** — the circlize formula is wrapped via
    [`wrap_elements()`](https://patchwork.data-imaginist.com/reference/wrap_elements.html)
    so it can be composed with other ggplot objects. Title and subtitle
    are added via
    [`plot_annotation()`](https://patchwork.data-imaginist.com/reference/plot_annotation.html).

9.  **Dimension attributes** — `height` and `width` attributes (in
    inches) are set to a base size (7) scaled up by 2, 4, or 6 depending
    on the maximum label character width when `labels_rot = TRUE`.
