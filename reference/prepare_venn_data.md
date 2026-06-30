# Prepare input data for Venn diagram rendering

Converts data in any supported input format into a `VennPlotData` object
suitable for rendering by ggVennDiagram. When `in_form = "auto"` (the
default), the format is detected via
[`detect_venn_datatype()`](https://pwwang.github.io/plotthis/reference/detect_venn_datatype.md).

## Usage

``` r
prepare_venn_data(
  data,
  in_form = "auto",
  group_by = NULL,
  group_by_sep = "_",
  id_by = NULL
)
```

## Arguments

- data:

  A data frame, a named list, or a `VennPlotData` object to be prepared
  for Venn diagram rendering. See the **Input formats** section for the
  expected structure of each format.

- in_form:

  A character string specifying the input format. One of `"auto"`
  (default; detect automatically via
  [`detect_venn_datatype()`](https://pwwang.github.io/plotthis/reference/detect_venn_datatype.md)),
  `"long"`, `"wide"`, `"list"`, or `"venn"`.

- group_by:

  A character string (or vector) specifying the column name(s)
  identifying set membership. For long-format data, a single column
  defines the set; multiple columns are concatenated with
  `group_by_sep`. For wide-format data, these are the set columns (must
  be logical or 0/1); when `NULL`, all columns are used as sets. Ignored
  for list and `VennPlotData` input.

- group_by_sep:

  A character string used to concatenate multiple `group_by` columns
  when `in_form = "long"`. Default `"_"`.

- id_by:

  A character string specifying the column name that identifies
  individual elements. Required for long-format data; ignored otherwise.

## Value

A `VennPlotData` object suitable for rendering by ggVennDiagram.

## Input formats

1.  **Long format** — a data frame with a grouping column (`group_by`)
    identifying the set and an ID column (`id_by`) identifying each
    element. Multiple `group_by` columns are concatenated with
    `group_by_sep`.

        group_by id_by
        A        a1
        A        a2
        B        a1
        B        a3

2.  **Wide format** — a data frame where each column represents a set
    and each row an element. Values must be logical or `0`/`1`. Columns
    specified in `group_by` define the sets; if `group_by` is `NULL`,
    all columns are used.

        A    B
        TRUE TRUE
        TRUE FALSE
        FALSE TRUE

3.  **List format** — a named list where each element is a vector of
    identifiers belonging to that set.

        list(A = c("a1", "a2"), B = c("a1", "a3"))

4.  **VennPlotData** — a pre-computed object returned by a previous call
    to `prepare_venn_data()`. Returned as-is.
