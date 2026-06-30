# Prepare data for an UpSet plot

Converts raw input (long data frame, wide data frame, named list, or an
existing `UpsetPlotData` object) into the internal format expected by
[`UpsetPlotAtomic()`](https://pwwang.github.io/plotthis/reference/UpsetPlotAtomic.md).
This is the data-preparation workhorse that handles all input format
parsing.

## Usage

``` r
prepare_upset_data(
  data,
  in_form = "auto",
  group_by = NULL,
  group_by_sep = "_",
  id_by = NULL,
  specific = TRUE
)
```

## Arguments

- data:

  A data frame, a named list of element vectors, or an `UpsetPlotData`
  object.

- in_form:

  A character string specifying the input format. One of `"auto"`
  (default; detect from `data` structure), `"long"`, `"wide"`, `"list"`,
  or `"upset"`.

- group_by:

  A character vector of column name(s) defining the sets. In long
  format, this is the column with set labels. In wide format, these are
  the set-membership columns. When `NULL` in wide format, all columns
  except `id_by` are used. Ignored for list and upset inputs.

- group_by_sep:

  A character string to concatenate multiple `group_by` columns when
  `in_form = "long"`. Default: `"_"`.

- id_by:

  A character string specifying the column name for instance
  identifiers. Required for long format; optional for wide format (a
  synthetic `.id` column is created if omitted).

- specific:

  A logical value. When `TRUE` (default), only specific intersections
  are returned (elements belonging exclusively to the shown set
  combination). When `FALSE`, all overlapping items are included. See
  <https://github.com/gaospecial/ggVennDiagram/issues/64>.

## Value

An `UpsetPlotData` object — a data frame with an `Intersection`
list-column of set labels and one row per element. A `"group_order"`
attribute preserves the original set order.

## Input formats

**Long format** — one row per (set, element) pair:

    group_by id_by
    A        a1
    A        a2
    B        a1
    B        a3

Requires both `group_by` and `id_by`.

**Wide format** — each row is an element, each set has a logical or 0/1
membership column:

    A      B
    TRUE   TRUE
    TRUE   FALSE
    FALSE  TRUE

The set columns are identified by `group_by` (or all columns except
`id_by` when `group_by = NULL`).

**List format** — a named list of element vectors:

    list(A = c("a1", "a2"), B = c("a1", "a3"))

**UpsetPlotData** — a pre-processed object (returned by this function)
is returned unchanged (with a warning if `group_by` is also provided).
