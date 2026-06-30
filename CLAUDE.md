# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working
with code in this repository.

## Project overview

`plotthis` is an R package providing high-level plotting functions built
on `ggplot2`. Each function produces publication-quality plots with
sensible defaults, automatic dimension calculation, and support for
faceting, splitting, and combining via `patchwork`.

## Development workflow

Package uses roxygen2 (markdown) for documentation. After editing R
source:

``` bash
# Regenerate NAMESPACE and .Rd files
Rscript -e 'devtools::document()'

# Run all tests
Rscript -e 'devtools::test()'

# Run a single test file
Rscript -e 'testthat::test_file("tests/testthat/test-barplot.R")'

# Check the package (full CRAN check)
Rscript -e 'devtools::check()'

# Build and install locally
Rscript -e 'devtools::install()'
```

## Architecture: three-layer function pattern

Nearly every plot type follows this layering:

1.  **`*Atomic()`** (internal) — Core implementation. Takes a single
    data frame and returns a `ggplot` object. Handles faceting via
    [`facet_plot()`](https://pwwang.github.io/plotthis/reference/facet_plot.md).
    Does NOT handle `split_by` or `combine`.
2.  **`*Plot()`** (exported) — The public API. Handles `split_by`
    (splitting data by a column, processing `keep_na`/`keep_empty`,
    dispatching per split to `*Atomic()`, then combining results via
    [`combine_plots()`](https://pwwang.github.io/plotthis/reference/combine_plots.md)).
    Handles per-split `palette`, `palcolor`, `legend.position`,
    `legend.direction`.
3.  **Optional intermediate functions** — e.g. `BarPlotSingle`,
    `BarPlotGrouped` — called by `*Atomic()` when there are
    significantly different rendering paths (with vs. without
    `group_by`).

Each plot file also contains separate **`*Atomic()`** and exported
**`*Plot()`** for related variants (e.g., `barplot.R` contains
`BarPlotAtomic`/`BarPlot` plus
`SplitBarPlotAtomic`/`SplitBarPlot`/`WaterfallPlot`).

## Key shared utilities

Located in `R/utils.R` (931 lines). Functions used across all plot
files:

| Function                                                                                                                                                                    | Purpose                                                                                                                       |
|-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------|-------------------------------------------------------------------------------------------------------------------------------|
| [`check_columns()`](https://pwwang.github.io/plotthis/reference/check_columns.md)                                                                                           | Validates columns exist in data; optionally forces factor, concatenates multi-column input                                    |
| [`check_keep_na()`](https://pwwang.github.io/plotthis/reference/check_keep_na.md) / [`check_keep_empty()`](https://pwwang.github.io/plotthis/reference/check_keep_empty.md) | Normalizes`keep_na`/`keep_empty` arguments (can be logical, character, or named list per column)                              |
| [`process_keep_na_empty()`](https://pwwang.github.io/plotthis/reference/process_keep_na_empty.md)                                                                           | Applies NA/empty handling to data before plotting                                                                             |
| [`norm_expansion()`](https://pwwang.github.io/plotthis/reference/norm_expansion.md)                                                                                         | CSS-padding-style expansion normalization for ggplot axes                                                                     |
| [`palette_this()`](https://pwwang.github.io/plotthis/reference/palette_this.md)                                                                                             | Resolves palette names or character vectors to actual color vectors                                                           |
| [`check_palette()`](https://pwwang.github.io/plotthis/reference/check_palette.md) / [`check_palcolor()`](https://pwwang.github.io/plotthis/reference/check_palcolor.md)     | Validates per-split palette/color specifications                                                                              |
| [`calculate_plot_dimensions()`](https://pwwang.github.io/plotthis/reference/calculate_plot_dimensions.md)                                                                   | Computes`height`/`width` attributes stored on ggplot objects for consistent rendering                                         |
| [`combine_plots()`](https://pwwang.github.io/plotthis/reference/combine_plots.md)                                                                                           | Wraps[`patchwork::wrap_plots`](https://patchwork.data-imaginist.com/reference/wrap_plots.html) with axis/guide/design options |
| [`facet_plot()`](https://pwwang.github.io/plotthis/reference/facet_plot.md)                                                                                                 | Unified faceting wrapper handling facet_wrap/facet_grid with keep_empty levels                                                |
| [`process_theme()`](https://pwwang.github.io/plotthis/reference/process_theme.md)                                                                                           | Resolves theme string to theme function                                                                                       |
| [`validate_common_args()`](https://pwwang.github.io/plotthis/reference/validate_common_args.md)                                                                             | Validates seed, facet_by combinations                                                                                         |
| [`check_legend()`](https://pwwang.github.io/plotthis/reference/check_legend.md)                                                                                             | Validates per-split legend parameters                                                                                         |

## Common parameters (inherited across all plots)

Defined via roxygen `@inheritParams common_args` in `R/common_args.R`:

- **Column references are strings**, not tidy-eval symbols:
  `x = "gene"`, `y = "expression"`
- `split_by` — split data into subplots (one per level); output combined
  via `patchwork`
- `facet_by` — ggplot2 native faceting within each subplot
- `group_by` — grouping variable within a single plot (e.g., fill color
  per group)
- `keep_na` / `keep_empty` — control NA and empty factor level behavior
  (can be `TRUE`/`FALSE`/`"level"` or per-column named list)
- `palette` / `palcolor` / `palreverse` — color control (palette from
  data.R palettes, or named list for per-split)
- `theme` / `theme_args` — theme selection (default: `"theme_this"`)
- `combine` — when `TRUE` (default), return combined patchwork; when
  `FALSE`, return list of individual plots

## Testing conventions

Uses `testthat`. Each test file in `tests/testthat/` creates a small
synthetic data frame (seed 8525) and tests:

- Returns a `ggplot` object (or `patchwork` when `combine = TRUE`, or
  `list` when `combine = FALSE`)
- Plot has `height`/`width` attributes (numeric)
- Parameter combinations produce valid outputs
- Wrapper aliases (e.g., `WaterfallPlot`, `LollipopPlot`) return correct
  class
- For smoke test, use
  [`devtools::load_all()`](https://devtools.r-lib.org/reference/load_all.html)
  to load the changes

## Theming and palettes

- `R/theming.R` —
  [`theme_this()`](https://pwwang.github.io/plotthis/reference/theme_this.md),
  [`theme_blank()`](https://pwwang.github.io/plotthis/reference/theme_blank.md),
  [`theme_box()`](https://pwwang.github.io/plotthis/reference/theme_box.md).
  Base size defaults to `getOption("theme_this.base_size", 12)`.
- `R/data.R` —
  [`palette_this()`](https://pwwang.github.io/plotthis/reference/palette_this.md)
  and
  [`show_palettes()`](https://pwwang.github.io/plotthis/reference/show_palettes.md).
  Palettes from RColorBrewer, viridis, ggsci, pals, etc.
- `gglogger` — when `getOption("plotthis.gglogger.enabled", FALSE)` is
  TRUE, uses `gglogger::ggplot` instead of
  [`ggplot2::ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)
  for debugging.

## NAMESPACE and imports

- **54 exported functions** (S3 methods + plot functions + theme/palette
  utilities)
- Heavy use of `@importFrom` (not `@import`) — each function explicitly
  imports only what it needs
- Key dependencies: `ggplot2`, `dplyr` (with `%>%`), `tidyr`, `rlang`,
  `patchwork`, `ggrepel`, `ggnewscale`, `cowplot`
- Bioconductor suggests: `ComplexHeatmap`, `ggmanh`, `ggtree`

## Important conventions

- **Column names as strings** — unlike modern tidyverse, this package
  uses string column references throughout
- **`%||%` operator** — `rlang::%||%` for null coalescing defaults
- **Dimension attributes** — every `*Atomic()` function sets
  `attr(p, "height")` and `attr(p, "width")` via
  [`calculate_plot_dimensions()`](https://pwwang.github.io/plotthis/reference/calculate_plot_dimensions.md)
- **No tidy evaluation** — uses `!!sym(x)` / `!!!syms(...)` for column
  references, not `{{ }}`
- **Manual color scales** — uses `scale_fill_manual()` /
  `scale_color_manual()` with
  [`palette_this()`](https://pwwang.github.io/plotthis/reference/palette_this.md)
  for discrete, `scale_fill_gradientn()` for continuous
