# Prepare and normalize annotation arguments

Consolidates the new structured annotation list format with
backward-compatible deprecation support for old flat arguments. Handles
`TRUE`/`FALSE` shortcuts, `.default` inheritance (with recursive
`params` merge), alias resolution, and shortcut expansion.

## Usage

``` r
.prep_annotations(
  which = c("row", "column"),
  annotation = NULL,
  annotation_side = NULL,
  annotation_palette = NULL,
  annotation_palcolor = NULL,
  annotation_type = NULL,
  annotation_params = NULL,
  annotation_agg = NULL,
  row_key = NULL,
  rsplit_key = NULL,
  col_key = NULL,
  csplit_key = NULL,
  data = NULL
)
```

## Arguments

- which:

  `"row"` or `"column"`.

- annotation:

  The new structured annotation list.

- annotation_side:

  Deprecated. Old side argument.

- annotation_palette:

  Deprecated. Old palette argument.

- annotation_palcolor:

  Deprecated. Old palcolor argument.

- annotation_type:

  Deprecated. Old type argument.

- annotation_params:

  Deprecated. Old params argument.

- annotation_agg:

  Deprecated. Old agg argument.

- row_key:

  The actual column name for `rows_by`.

- rsplit_key:

  The actual column name for `rows_split_by`.

- col_key:

  The actual column name for `columns_by`.

- csplit_key:

  The actual column name for `columns_split_by`.

- data:

  The data frame used for column validation.

## Value

A list with components `annotation`, `annotation_type`,
`annotation_side`, `annotation_palette`, `annotation_palcolor`,
`annotation_agg`, `annotation_params`, and `enabled`.
