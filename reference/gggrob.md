# Get the grid.draw-able ggplot grob The output from ggplotGrob can not be directly used in grid.draw, the position can not be set. This function extracts the gTree from the ggplot grob.

Get the grid.draw-able ggplot grob The output from ggplotGrob can not be
directly used in grid.draw, the position can not be set. This function
extracts the gTree from the ggplot grob.

## Usage

``` r
gggrob(p, void = TRUE, nolegend = TRUE)
```

## Arguments

- p:

  A ggplot object

- void:

  If TRUE, the theme_void will be added to the ggplot object

- nolegend:

  If TRUE, the legend will be removed from the ggplot object

## Value

A gTree object
