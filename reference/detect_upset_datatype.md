# Detect the type of the input data for Upset plot

Detect the type of the input data for Upset plot

## Usage

``` r
detect_upset_datatype(data, group_by = NULL, id_by = NULL)
```

## Arguments

- data:

  A data frame or a list

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
"upset". "long" indicates the data is in long format. "wide" indicates
the data is in wide format. "list" indicates the data is a list. "upset"
indicates the data is a UpsetPlotData object.
