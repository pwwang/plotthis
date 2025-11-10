# Detect the datatype of the input data of Venn diagram

Detect the datatype of the input data of Venn diagram

## Usage

``` r
detect_venn_datatype(data, group_by = NULL, id_by = NULL)
```

## Arguments

- data:

  A data frame or a list or a VennPlotData object.

- group_by:

  A character string specifying the column name of the data frame to
  group the data.

- id_by:

  A character string specifying the column name of the data frame to
  identify the instances. Required when `group_by` is a single column
  and data is a data frame.

## Value

A character string indicating the datatype of the input data or error
message if invalid. Possible values are "long", "wide", "list" and
"venn". "long" indicates the data is in long format. "wide" indicates
the data is in wide format. "list" indicates the data is a list. "venn"
indicates the data is a VennPlotData object.
