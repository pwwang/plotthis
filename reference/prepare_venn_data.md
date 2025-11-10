# Prepare data for Venn diagram

Prepare data for Venn diagram

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

  A data frame or a list or a VennPlotData object.

- in_form:

  A character string indicating the datatype of the input data. Possible
  values are "long", "wide", "list", "venn" or NULL. "long" indicates
  the data is in long format. "wide" indicates the data is in wide
  format. "list" indicates the data is a list. "venn" indicates the data
  is a VennPlotData object. "auto" indicates the function will detect
  the datatype of the input data.

  A long format data would look like:

      group_by id_by
      A        a1
      A        a2
      B        a1
      B        a3
      ...

  A wide format data would look like:

      A    B
      TRUE TRUE
      TRUE FALSE
      FALSE TRUE
      ...

  A list format data would look like:

      list(A = c("a1", "a2"), B = c("a1", "a3"))

- group_by:

  A character string specifying the column name of the data frame to
  group the data.

- group_by_sep:

  A character string to concatenate the columns in `group_by`, if
  multiple columns are provided and the in_form is "long".

- id_by:

  A character string specifying the column name of the data frame to
  identify the instances. Required when `group_by` is a single column
  and data is a data frame.

## Value

A VennPlotData object
