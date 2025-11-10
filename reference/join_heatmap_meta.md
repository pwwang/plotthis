# Join the meta data to the main data frame for heatmap

Join the meta data to the main data frame for heatmap

## Usage

``` r
join_heatmap_meta(data, meta_data, by, cr_split_by, split_by, which)
```

## Arguments

- data:

  A data frame containing the main data for the heatmap.

- meta_data:

  A data frame containing the meta data to be joined.

- by:

  A character string specifying the column name in `meta_data` to join
  on. Either `rows_by` or `columns_by` should be specified in `data`.

- cr_split_by:

  A character string specifying the column name in `data` to join on.
  Either `rows_split_by` or `columns_split_by` should be specified in
  `data`.

- split_by:

  A character string specifying the column name in `data` to join on.
  Used to split the data into multiple heatmaps.

- which:

  A character string specifying whether to join on rows or columns. Can
  be either `"row"` or `"column"`.

## Value

A data frame with the meta data joined to the main data.
