# Check the columns if columns found in the data

Check the columns if columns found in the data

## Usage

``` r
check_columns(
  df,
  columns,
  force_factor = FALSE,
  allow_multi = FALSE,
  concat_multi = FALSE,
  concat_sep = "_"
)
```

## Arguments

- df:

  A data frame

- columns:

  A character vector of column names

- force_factor:

  Whether to force the columns to be factors

- allow_multi:

  Whether to allow multiple columns

- concat_multi:

  Whether to concatenate multiple columns

- concat_sep:

  The separator to use for concatenation

## Value

A character string of the valid column
