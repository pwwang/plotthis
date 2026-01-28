# Process/normalize data passed to [`Heatmap()`](https://pwwang.github.io/plotthis/reference/Heatmap.md)

This function is used to process the data passed to
[`Heatmap()`](https://pwwang.github.io/plotthis/reference/Heatmap.md).

## Usage

``` r
process_heatmap_data(
  data,
  in_form,
  values_by,
  name,
  split_by,
  split_by_sep,
  rows_by,
  rows_by_sep,
  rows_name,
  rows_split_by,
  rows_split_by_sep,
  rows_split_name,
  columns_by,
  columns_by_sep,
  columns_name,
  columns_split_by,
  columns_split_by_sep,
  columns_split_name,
  pie_group_by,
  pie_group_by_sep,
  pie_name,
  rows_data,
  columns_data,
  keep_na
)
```

## Arguments

- data:

  A data frame or matrix containing the data to be plotted. Based on the
  `in_form`, the data can have the following formats:

  - `matrix`: A matrix with rows and columns directly representing the
    heatmap.

  - `long`: A data frame in long format with columns for values, rows,
    and columns.

  - `wide-rows`: A data frame in wide format with columns for heatmap
    rows and values, and a single column for heatmap columns.

  - `wide-columns`: A data frame in wide format with columns for heatmap
    columns and values, and a single column for heatmap rows.

  - `auto`: Automatically inferred from the data format. When `data` is
    a matrix, `in_form` is set to `"matrix"`. When `columns_by` has more
    than one column, `in_form` is set to `"wide-columns"`. When
    `rows_by` has more than one column, `in_form` is set to
    `"wide-rows"`. Otherwise, it is set to `"long"`.

- in_form:

  The format of the data. Can be one of `"matrix"`, `"long"`,
  `"wide-rows"`, `"wide-columns"`, or `"auto"`. Defaults to `"auto"`.

- values_by:

  A character of column name in `data` that contains the values to be
  plotted. This is required when `in_form` is `"long"`. For other
  formats, the values are pivoted into a column named by `values_by`.

- name:

  A character string to name the heatmap (will be used to rename
  `values_by`).

- split_by:

  A character of column name in `data` that contains the split
  information to split into multiple heatmaps. This is used to create a
  list of heatmaps, one for each level of the split. Defaults to `NULL`,
  meaning no split.

- split_by_sep:

  A character string to concat multiple columns in `split_by`.

- rows_by:

  A vector of column names in `data` that contains the row information.
  This is used to create the rows of the heatmap. When `in_form` is
  `"long"` or `"wide-columns"`, this is requied, and multiple columns
  can be specified, which will be concatenated by `rows_by_sep` into a
  single column.

- rows_by_sep:

  A character string to concat multiple columns in `rows_by`.

- rows_name:

  A character string to rename the column created by `rows_by`, which
  will be reflected in the name of the annotation or legend.

- rows_split_by:

  A character of column name in `data` that contains the split
  information for rows.

- rows_split_by_sep:

  A character string to concat multiple columns in `rows_split_by`.

- rows_split_name:

  A character string to rename the column created by `rows_split_by`,
  which will be reflected in the name of the annotation or legend.

- columns_by:

  A vector of column names in `data` that contains the column
  information. This is used to create the columns of the heatmap. When
  `in_form` is `"long"` or `"wide-rows"`, this is required, and multiple
  columns can be specified, which will be concatenated by
  `columns_by_sep` into a single column.

- columns_by_sep:

  A character string to concat multiple columns in `columns_by`.

- columns_name:

  A character string to rename the column created by `columns_by`, which
  will be reflected in the name of the annotation or legend.

- columns_split_by:

  A character of column name in `data` that contains the split
  information for columns.

- columns_split_by_sep:

  A character string to concat multiple columns in `columns_split_by`.

- columns_split_name:

  A character string to rename the column created by `columns_split_by`,
  which will be reflected in the name of the annotation or legend.

- pie_group_by:

  A character of column name in `data` that contains the group
  information for pie charts. This is used to create pie charts in the
  heatmap when `cell_type` is `"pie"`.

- pie_group_by_sep:

  A character string to concat multiple columns in `pie_group_by`.

- pie_name:

  A character string to rename the column created by `pie_group_by`,
  which will be reflected in the name of the annotation or legend.

- rows_data:

  A data frame containing additional data for rows, which can be used to
  add annotations to the heatmap. It will be joined to the main data by
  `rows_by` and `split_by` if `split_by` exists in `rows_data`. This is
  useful for adding additional information to the rows of the heatmap.

- columns_data:

  A data frame containing additional data for columns, which can be used
  to add annotations to the heatmap. It will be joined to the main data
  by `columns_by` and `split_by` if `split_by` exists in `columns_data`.
  This is useful for adding additional information to the columns of the
  heatmap.

- keep_na:

  Whether we should keep NA groups in rows, columns and split_by
  variables. Default is FALSE. FALSE to remove NA groups; TRUE to keep
  NA groups. A vector of column names can also be provided to specify
  which columns to keep NA groups. Note that the record will be removed
  if any of the grouping columns has NA and is not specified to keep NA.

## Value

A list containing the processed data and metadata:

- `data`: A list of data frames, one for each level of `split_by`. If no
  `split_by` is provided, the name will be `"..."`. Each data frame is
  in the long format.

- `values_by`: The name of the column containing the values to be
  plotted.

- `rows_by`: The name of the column containing the row information.

- `rows_split_by`: The name of the column containing the row split
  information.

- `columns_by`: The name of the column containing the column
  information.

- `columns_split_by`: The name of the column containing the column split
  information.

- `pie_group_by`: The name of the column containing the pie group
  information.
