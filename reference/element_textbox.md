# Theme element that add a box to the text

Code grabbed from the `ggtext` package. See the original code at:
https://github.com/wilkelab/ggtext This is used to create a text box
around the text, primarily to be used in `CorPairsPlot`.

## Usage

``` r
element_textbox(
  family = NULL,
  face = NULL,
  size = NULL,
  colour = NULL,
  fill = NULL,
  box.colour = NULL,
  linetype = NULL,
  linewidth = NULL,
  hjust = NULL,
  vjust = NULL,
  halign = NULL,
  valign = NULL,
  lineheight = NULL,
  margin = NULL,
  padding = NULL,
  width = NULL,
  height = NULL,
  minwidth = NULL,
  maxwidth = NULL,
  minheight = NULL,
  maxheight = NULL,
  r = NULL,
  orientation = NULL,
  color = NULL,
  box.color = NULL,
  debug = FALSE,
  inherit.blank = FALSE
)

# S3 method for class 'element_textbox'
element_grob(
  element,
  label = "",
  x = NULL,
  y = NULL,
  family = NULL,
  face = NULL,
  colour = NULL,
  size = NULL,
  hjust = NULL,
  vjust = NULL,
  lineheight = NULL,
  margin = NULL,
  ...
)
```

## Arguments

- family:

  Font family

- face:

  Font face

- size:

  Font size (in pt)

- colour, color:

  Text color

- fill:

  Fill color of the enclosing box

- box.colour, box.color:

  Line color of the enclosing box (if different from the text color)

- linetype:

  Line type of the enclosing box (like `lty` in base R)

- linewidth:

  Line width of the enclosing box (measured in mm, just like `size` in
  [`ggplot2::element_line()`](https://ggplot2.tidyverse.org/reference/element.html)).

- hjust:

  Horizontal justification

- vjust:

  Vertical justification

- halign:

  Horizontal justification

- valign:

  Vertical justification

- lineheight:

  Line height, in multiples of the font size

- padding, margin:

  Padding and margins around the text box. See
  [`gridtext::textbox_grob()`](https://wilkelab.org/gridtext/reference/textbox_grob.html)
  for details.

- width, height:

  Unit objects specifying the width and height of the textbox, as in
  [`gridtext::textbox_grob()`](https://wilkelab.org/gridtext/reference/textbox_grob.html).

- minwidth, minheight, maxwidth, maxheight:

  Min and max values for width and height. Set to NULL to impose neither
  a minimum nor a maximum.

- r:

  Unit value specifying the corner radius of the box

- orientation:

  Orientation of the text box. See
  [`gridtext::textbox_grob()`](https://wilkelab.org/gridtext/reference/textbox_grob.html)
  for details.

- debug:

  Not implemented.

- inherit.blank:

  See
  [`ggplot2::margin()`](https://ggplot2.tidyverse.org/reference/element.html)
  for details.

- element:

  A theme element created by `element_textbox()`.

- label:

  Text to display in the textbox.

- x, y:

  Position of the textbox.

- ...:

  Other arguments passed to
  [`gridtext::textbox_grob()`](https://wilkelab.org/gridtext/reference/textbox_grob.html).

## Value

A ggplot2 theme element that can be used inside a
[`ggplot2::theme()`](https://ggplot2.tidyverse.org/reference/theme.html)
call.
