# Resolve annotation aliases: .row -\> rows_by .rows.split -\> rows_split_by .col/.column -\> columns_by .col.split/.column.split -\> columns_split_by

Resolve annotation aliases: .row -\> rows_by .rows.split -\>
rows_split_by .col/.column -\> columns_by .col.split/.column.split -\>
columns_split_by

## Usage

``` r
.resolve_anno_aliases(lst, row_key, rsplit_key, col_key, csplit_key)
```

## Arguments

- lst:

  A list of annotations, which may contain aliases for rows_by,
  rows_split_by, columns_by, and columns_split_by.

- row_key:

  The actual key for rows_by in the list.

- rsplit_key:

  The actual key for rows_split_by in the list.

- col_key:

  The actual key for columns_by in the list.

- csplit_key:

  The actual key for columns_split_by in the list.

## Value

A list of annotations with aliases resolved to the actual keys.
