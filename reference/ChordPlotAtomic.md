# Atomic chord plot

Atomic chord plot

## Usage

``` r
ChordPlotAtomic(
  data,
  y = NULL,
  from = NULL,
  from_sep = "_",
  to = NULL,
  to_sep = "_",
  flip = FALSE,
  links_color = c("from", "to"),
  theme = "theme_this",
  theme_args = list(),
  palette = "Paired",
  palcolor = NULL,
  alpha = 0.5,
  labels_rot = FALSE,
  title = NULL,
  subtitle = NULL,
  ...
)
```

## Arguments

- data:

  A data frame.

- y:

  A character string specifying the column name of the data frame to
  plot for the y-axis.

- from:

  A character string of the column name to plot for the source. A
  character/factor column is expected.

- from_sep:

  A character string to concatenate the columns in `from`, if multiple
  columns are provided.

- to:

  A character string of the column name to plot for the target. A
  character/factor column is expected.

- to_sep:

  A character string to concatenate the columns in `to`, if multiple
  columns are provided.

- flip:

  A logical value to flip the source and target.

- links_color:

  A character string to specify the color of the links. Either "from" or
  "to".

- theme:

  A character string or a theme class (i.e. ggplot2::theme_classic)
  specifying the theme to use. Default is "theme_this".

- theme_args:

  A list of arguments to pass to the theme function.

- palette:

  A character string specifying the palette to use. A named list or
  vector can be used to specify the palettes for different `split_by`
  values.

- palcolor:

  A character string specifying the color to use in the palette. A named
  list can be used to specify the colors for different `split_by`
  values. If some values are missing, the values from the palette will
  be used (palcolor will be NULL for those values).

- alpha:

  A numeric value specifying the transparency of the plot.

- labels_rot:

  A logical value to rotate the labels by 90 degrees.

- title:

  A character string specifying the title of the plot. A function can be
  used to generate the title based on the default title. This is useful
  when split_by is used and the title needs to be dynamic.

- subtitle:

  A character string specifying the subtitle of the plot.

- ...:

  Additional arguments.

## Value

A wrapped element of chord plot
