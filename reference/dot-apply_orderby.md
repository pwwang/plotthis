# Apply ordering to a data frame based on a specified expression

Apply ordering to a data frame based on a specified expression

## Usage

``` r
.apply_orderby(df, orderby, by, sby, sby_levels)
```

## Arguments

- df:

  A data frame to be ordered.

- orderby:

  A character string representing the expression to order by.

- by:

  A character string representing the column name to group by.

- sby:

  A character string representing the column name to group by for
  secondary ordering.

- sby_levels:

  A character vector representing the levels of the secondary grouping
  column.

## Value

A data frame ordered based on the specified expression.
