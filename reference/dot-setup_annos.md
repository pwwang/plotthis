# Unified annotation builder for built-in (split/name) and user-defined annotations

Unified annotation builder for built-in (split/name) and user-defined
annotations

## Usage

``` r
.setup_annos(
  which,
  names_side,
  anno_title,
  show_names,
  annotation,
  annotation_type,
  annotation_side,
  annotation_palette,
  annotation_palcolor,
  annotation_agg,
  annotation_params,
  split_by,
  splits,
  by,
  by_labels,
  flip,
  legend.direction,
  legend.position,
  data
)
```

## Arguments

- which:

  The annotation direction ("row" or "column")

- names_side:

  The side to place the row/column name annotation ("top", "bottom",
  "left", "right")

- anno_title:

  The title of the annotation (for split)

- show_names:

  A logical value indicating whether to show row/column names

- annotation:

  A list of user-defined annotations, where names are annotation names
  and values are annotation objects or parameters to build annotation
  objects

- annotation_type:

  A list of annotation types, where names are annotation names and
  values are annotation types ("simple", "label", "block", "ggcat",
  "ggseries", or "auto")

- annotation_side:

  A list of annotation sides, where names are annotation names and
  values are annotation sides ("top", "bottom", "left", "right")

- annotation_palette:

  A list of annotation palettes, where names are annotation names and
  values are palette names or color vectors

- annotation_palcolor:

  A list of annotation palette colors, where names are annotation names
  and values are color vectors to override the palette

- annotation_agg:

  A list of functions to aggregate the original data for each
  annotation, where names are annotation names and values are functions
  that take a vector of values in the cell and return an aggregated
  value

- annotation_params:

  A list of additional parameters for each annotation, where names are
  annotation names and values are lists of parameters to pass to the
  annotation constructor

- split_by:

  The name of the column used for split annotation

- splits:

  A factor vector of splits for the split annotation

- by:

  The name of the column used for name annotation

- by_labels:

  A factor vector of labels for the name annotation

- flip:

  A logical value indicating whether to flip the annotation (for
  ggseries annotations)

- legend.direction:

  The direction of the legend ("vertical" or "horizontal")

- legend.position:

  The position of the legend ("right", "left", "top", "bottom")

- data:

  A data frame used for ggcat and ggseries annotations, where each row
  corresponds to a cell in the heatmap and contains the original values
  before aggregation

## Value

A list of annotations and legends to be passed to ComplexHeatmap.
