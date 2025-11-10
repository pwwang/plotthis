# Facetting a plot

Facetting a plot

## Usage

``` r
facet_plot(
  plot,
  facet_by,
  facet_scales,
  nrow,
  ncol,
  byrow,
  legend.position = "right",
  legend.direction = "vertical",
  recalc_size = TRUE,
  ...
)
```

## Arguments

- plot:

  The plot to facet or a list list(plot, height, width) if guess_size is
  TRUE

- facet_by:

  The column(s) to split data by and plot separately or facet by If
  NULL, no faceting will be done

- facet_scales:

  Whether to scale the axes of facets.

- nrow:

  The number of rows in facet_wrap

- ncol:

  The number of columns in facet_wrap

- byrow:

  Whether to fill the plots by row

- legend.position:

  The position of the legend

- legend.direction:

  The direction of the legend

- recalc_size:

  Whether to re-calculate the size of the plot

- ...:

  Additional arguments to pass to facet_wrap or facet_grid

## Value

The faceted plot. If guess_size is TRUE, attr(p, "height") and attr(p,
"width") will be set
