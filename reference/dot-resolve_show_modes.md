# Resolve display modes for row/column names and split titles

`show_row_names`/`show_column_names` (kind = "names") and
`row_title`/`column_title` (kind = "titles") accept a character vector
combining the modes "inplace", "legend", "simple", "anno" (alias
"annotation"), and "none". Modes are lower-priority defaults: they only
fill annotation type/params the user has not configured (per-key or via
`.default`).

## Usage

``` r
.resolve_show_modes(
  show,
  kind = c("names", "titles"),
  which = c("row", "column"),
  by = NULL,
  split_by = NULL,
  annotation = list(),
  annotation_type = list(),
  annotation_params = list(),
  annotation_name = list(),
  legend.position = "right"
)
```

## Arguments

- show:

  The value of `show_row_names`/`show_column_names` or
  `row_title`/`column_title`

- kind:

  Which arguments are being resolved: "names" or "titles"

- which:

  The direction ("row" or "column")

- by:

  The name-annotation column (`rows_by`/`columns_by`)

- split_by:

  The split-annotation column (`rows_split_by`/`columns_split_by`)

- annotation, annotation_type, annotation_params:

  The prepared annotation components returned by
  [`.prep_annotations`](https://pwwang.github.io/plotthis/reference/dot-prep_annotations.md)

- legend.position:

  The legend position

## Value

A list with the possibly-updated annotation components plus the resolved
`show` flag, `title`, `explicit`, `by_eff`, and `enabled`
