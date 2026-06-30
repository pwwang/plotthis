# Detect the type of the input data for an UpSet plot

Inspects the structure of `data` and returns a classifier string that
downstream functions use to select the parsing strategy. When `data` is
an `UpsetPlotData` object (from
[`prepare_upset_data()`](https://pwwang.github.io/plotthis/reference/prepare_upset_data.md))
it short-circuits to `"upset"` immediately.

## Usage

``` r
detect_upset_datatype(data, group_by = NULL, id_by = NULL)
```

## Arguments

- data:

  A data frame or a named list of element vectors.

- group_by:

  A character string specifying the column name for the set-definition
  groups. Only meaningful when `data` is a data frame.

- id_by:

  A character string specifying the column name for instance
  identifiers. Required when `group_by` is a single column and `data` is
  a data frame.

## Value

A character string, one of `"long"`, `"wide"`, `"list"`, or `"upset"`.

- `"long"`:

  Long-format data — each row records one (set, element) pair.

- `"wide"`:

  Wide-format data — each row is an element and each set has its own
  logical/0-1 column.

- `"list"`:

  A named list of element vectors.

- `"upset"`:

  Already an `UpsetPlotData` object.
