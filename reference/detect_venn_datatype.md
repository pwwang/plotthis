# Detect the input data format for Venn diagram processing

Examines the structure of `data` and determines which of the four
supported formats it conforms to:

- `"long"` — a data frame with one row per element-set pair.

- `"wide"` — a data frame with logical/0-1 columns per set.

- `"list"` — a named list of element vectors per set.

- `"venn"` — a pre-computed `VennPlotData` object.

## Usage

``` r
detect_venn_datatype(data, group_by = NULL, id_by = NULL)
```

## Arguments

- data:

  A data frame, a named list, or a `VennPlotData` object. Data frames
  are classified as `"long"` when a single non-`NULL` `group_by` is
  provided, or `"wide"` when `group_by` has two or more columns or is
  `NULL`.

- group_by:

  A character string specifying the column name(s) identifying the set
  membership. A single non-`NULL` column indicates long format. Two or
  more columns, or `NULL`, indicate wide format.

- id_by:

  A character string specifying the column name that identifies
  individual elements. Required when `group_by` is a single column and
  `data` is a data frame. Ignored for other input types.

## Value

A character string indicating the detected data format: `"long"`,
`"wide"`, `"list"`, or `"venn"`. Stops with an error if the data does
not match any recognised type.
