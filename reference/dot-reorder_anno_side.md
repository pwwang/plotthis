# Reorder annotation list so split annotation (split_by) is farthest from heatmap body and name annotation (by) is closest. User annotations stay in between. For all annotation sides, ComplexHeatmap renders first-element = farthest, last-element = closest to the heatmap body.

Reorder annotation list so split annotation (split_by) is farthest from
heatmap body and name annotation (by) is closest. User annotations stay
in between. For all annotation sides, ComplexHeatmap renders
first-element = farthest, last-element = closest to the heatmap body.

## Usage

``` r
.reorder_anno_side(x, by, split_by, side)
```

## Arguments

- x:

  A list of annotations

- by:

  The name of the annotation used for row/column names

- split_by:

  The name of the annotation used for splitting rows/columns

- side:

  The annotation side ("top", "bottom", "left", "right")

## Value

A reordered list of annotations
