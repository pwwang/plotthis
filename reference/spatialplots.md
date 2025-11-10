# Plots for spatial elements

- `SpatImagePlot`: Plot a SpatRaster object as an image.

- `SpatMasksPlot`: Plot a SpatRaster object as masks.

- `SpatShapesPlot`: Plot a SpatVector object as shapes.

- `SpatPointsPlot`: Plot a data.frame of points with spatial
  coordinates.

## Usage

``` r
SpatImagePlot(
  data,
  ext = NULL,
  raster = NULL,
  raster_dpi = NULL,
  flip_y = TRUE,
  palette = "turbo",
  palcolor = NULL,
  palette_reverse = FALSE,
  alpha = 1,
  fill_name = NULL,
  return_layer = FALSE,
  theme = "theme_box",
  theme_args = list(),
  legend.position = ifelse(return_layer, "none", "right"),
  legend.direction = "vertical",
  title = NULL,
  subtitle = NULL,
  xlab = NULL,
  ylab = NULL,
  seed = 8525
)

SpatMasksPlot(
  data,
  ext = NULL,
  flip_y = TRUE,
  add_border = TRUE,
  border_color = "black",
  border_size = 0.5,
  border_alpha = 1,
  palette = "turbo",
  palcolor = NULL,
  palette_reverse = FALSE,
  alpha = 1,
  fill_name = NULL,
  return_layer = FALSE,
  theme = "theme_box",
  theme_args = list(),
  legend.position = "right",
  legend.direction = "vertical",
  title = NULL,
  subtitle = NULL,
  xlab = NULL,
  ylab = NULL,
  seed = 8525
)

SpatShapesPlot(
  data,
  x = NULL,
  y = NULL,
  group = NULL,
  ext = NULL,
  flip_y = TRUE,
  fill_by = NULL,
  border_color = "black",
  border_size = 0.5,
  border_alpha = 1,
  palette = NULL,
  palcolor = NULL,
  palette_reverse = FALSE,
  alpha = 1,
  fill_name = NULL,
  highlight = NULL,
  highlight_alpha = 1,
  highlight_size = 1,
  highlight_color = "black",
  highlight_stroke = 0.8,
  facet_scales = "fixed",
  facet_nrow = NULL,
  facet_ncol = NULL,
  facet_byrow = TRUE,
  return_layer = FALSE,
  theme = "theme_box",
  theme_args = list(),
  legend.position = ifelse(return_layer, "none", "right"),
  legend.direction = "vertical",
  title = NULL,
  subtitle = NULL,
  xlab = NULL,
  ylab = NULL,
  seed = 8525
)

# S3 method for class 'SpatVector'
SpatShapesPlot(
  data,
  x = NULL,
  y = NULL,
  group = NULL,
  ext = NULL,
  flip_y = TRUE,
  fill_by = NULL,
  border_color = "black",
  border_size = 0.5,
  border_alpha = 1,
  palette = NULL,
  palcolor = NULL,
  palette_reverse = FALSE,
  alpha = 1,
  fill_name = NULL,
  highlight = NULL,
  highlight_alpha = 1,
  highlight_size = 1,
  highlight_color = "black",
  highlight_stroke = 0.8,
  facet_scales = "fixed",
  facet_nrow = NULL,
  facet_ncol = NULL,
  facet_byrow = TRUE,
  return_layer = FALSE,
  theme = "theme_box",
  theme_args = list(),
  legend.position = ifelse(return_layer, "none", "right"),
  legend.direction = "vertical",
  title = NULL,
  subtitle = NULL,
  xlab = NULL,
  ylab = NULL,
  seed = 8525
)

# S3 method for class 'data.frame'
SpatShapesPlot(
  data,
  x,
  y,
  group,
  ext = NULL,
  flip_y = TRUE,
  fill_by = "grey90",
  border_color = "black",
  border_size = 0.5,
  border_alpha = 1,
  palette = NULL,
  palcolor = NULL,
  palette_reverse = FALSE,
  alpha = 1,
  fill_name = NULL,
  highlight = NULL,
  highlight_alpha = 1,
  highlight_size = 1,
  highlight_color = "black",
  highlight_stroke = 0.8,
  facet_scales = "fixed",
  facet_nrow = NULL,
  facet_ncol = NULL,
  facet_byrow = TRUE,
  return_layer = FALSE,
  theme = "theme_box",
  theme_args = list(),
  legend.position = ifelse(return_layer, "none", "right"),
  legend.direction = "vertical",
  title = NULL,
  subtitle = NULL,
  xlab = NULL,
  ylab = NULL,
  seed = 8525
)

SpatPointsPlot(
  data,
  x = NULL,
  y = NULL,
  ext = NULL,
  flip_y = TRUE,
  color_by = NULL,
  size_by = NULL,
  size = NULL,
  fill_by = NULL,
  lower_quantile = 0,
  upper_quantile = 0.99,
  lower_cutoff = NULL,
  upper_cutoff = NULL,
  palette = NULL,
  palcolor = NULL,
  palette_reverse = FALSE,
  alpha = 1,
  color_name = NULL,
  size_name = NULL,
  shape = 16,
  border_color = "black",
  border_size = 0.5,
  border_alpha = 1,
  raster = NULL,
  raster_dpi = c(512, 512),
  hex = FALSE,
  hex_linewidth = 0.5,
  hex_count = FALSE,
  hex_bins = 50,
  hex_binwidth = NULL,
  label = FALSE,
  label_size = 4,
  label_fg = "white",
  label_bg = "black",
  label_bg_r = 0.1,
  label_repel = FALSE,
  label_repulsion = 20,
  label_pt_size = 1,
  label_pt_color = "black",
  label_segment_color = "black",
  label_insitu = FALSE,
  label_pos = c("median", "mean", "max", "min", "first", "last", "center", "random"),
  highlight = NULL,
  highlight_alpha = 1,
  highlight_size = 1,
  highlight_color = "black",
  highlight_stroke = 0.8,
  graph = NULL,
  graph_x = NULL,
  graph_y = NULL,
  graph_xend = NULL,
  graph_yend = NULL,
  graph_value = NULL,
  edge_size = c(0.05, 0.5),
  edge_alpha = 0.1,
  edge_color = "grey40",
  facet_scales = "fixed",
  facet_nrow = NULL,
  facet_ncol = NULL,
  facet_byrow = TRUE,
  return_layer = FALSE,
  theme = "theme_box",
  theme_args = list(),
  legend.position = ifelse(return_layer, "none", "right"),
  legend.direction = "vertical",
  title = NULL,
  subtitle = NULL,
  xlab = NULL,
  ylab = NULL,
  seed = 8525
)
```

## Arguments

- data:

  A `SpatRaster` or `SpatVector` object from the `terra` package, or a
  data.frame for `SpatShapesPlot` or `SpatPointsPlot`.

- ext:

  A `terra`'s `SpatExtent` object or a numeric vector of length 4
  specifying the extent as `c(xmin, xmax, ymin, ymax)`. Default is NULL.

- raster:

  Whether to raster the plot. Default is NULL.

- raster_dpi:

  A numeric vector of the raster dpi. Default is c(512, 512).

- flip_y:

  Whether to flip the y-axis direction. Default is TRUE. This is useful
  for visualizing spatial data with the origin at the top left corner.

- palette:

  A character string specifying the color palette to use. For
  `SpatImagePlot`, if the data has 3 channels (RGB), it will be used as
  a color identity and this argument will be ignored.

- palcolor:

  A character string specifying the color to use in the palette. A named
  list can be used to specify the colors for different `split_by`
  values. If some values are missing, the values from the palette will
  be used (palcolor will be NULL for those values).

- palette_reverse:

  Whether to reverse the color palette. Default is FALSE.

- alpha:

  A numeric value specifying the transparency of the plot.

- fill_name:

  A character string for the fill legend title.

- return_layer:

  Whether to return the layers or the plot. Default is FALSE.

- theme:

  A character string or a theme class (i.e. ggplot2::theme_classic)
  specifying the theme to use. Default is "theme_this".

- theme_args:

  A list of arguments to pass to the theme function.

- legend.position:

  A character string specifying the position of the legend. if
  `waiver()`, for single groups, the legend will be "none", otherwise
  "right".

- legend.direction:

  A character string specifying the direction of the legend.

- title:

  A character string specifying the title of the plot. A function can be
  used to generate the title based on the default title. This is useful
  when split_by is used and the title needs to be dynamic.

- subtitle:

  A character string specifying the subtitle of the plot.

- xlab:

  A character string specifying the x-axis label.

- ylab:

  A character string specifying the y-axis label.

- seed:

  The random seed to use. Default is 8525.

- add_border:

  Whether to add a border around the masks in `SpatMasksPlot`. Default
  is TRUE.

- border_color:

  A character string of the border color. Default is "black".

- border_size:

  A numeric value of the border width. Default is 0.5.

- border_alpha:

  A numeric value of the border transparency. Default is 1.

- x:

  A character string specifying the x-axis column name for
  `SpatPointsPlot` or `SpatShapesPlot` when `data` is a data.frame. If
  `data` is a `SpatRaster` or `SpatVector`, this argument is ignored.

- y:

  A character string specifying the y-axis column name for
  `SpatPointsPlot` or `SpatShapesPlot` when `data` is a data.frame. If
  `data` is a `SpatRaster` or `SpatVector`, this argument is ignored.

- group:

  A character string specifying the grouping column for `SpatShapesPlot`
  when `data` is a data.frame.

- fill_by:

  A character string or vector specifying the column(s) to fill the
  shapes in `SpatShapesPlot`.

- highlight:

  A character vector of the row names to highlight. Default is NULL.

- highlight_alpha:

  A numeric value of the highlight transparency. Default is 1.

- highlight_size:

  A numeric value of the highlight size. Default is 1.

- highlight_color:

  A character string of the highlight color. Default is "black".

- highlight_stroke:

  A numeric value of the highlight stroke. Default is 0.8.

- facet_scales:

  Whether to scale the axes of facets. Default is "fixed" Other options
  are "free", "free_x", "free_y". See
  [`ggplot2::facet_wrap`](https://ggplot2.tidyverse.org/reference/facet_wrap.html)

- facet_nrow:

  A numeric value specifying the number of rows in the facet. When
  facet_by is a single column and facet_wrap is used.

- facet_ncol:

  A numeric value specifying the number of columns in the facet. When
  facet_by is a single column and facet_wrap is used.

- facet_byrow:

  A logical value indicating whether to fill the plots by row. Default
  is TRUE.

- color_by:

  A character string specifying the column to color the points in
  `SpatPointsPlot`.

- size_by:

  A character string specifying the column to size the points in
  `SpatPointsPlot`.

- size:

  Alias of `size_by` when size is a numeric value.

- lower_quantile, upper_quantile, lower_cutoff, upper_cutoff:

  Vector of minimum and maximum cutoff values or quantile values for
  each numeric value.

- color_name:

  A character string for the color legend title in `SpatPointsPlot`.

- size_name:

  A character string for the size legend title in `SpatPointsPlot`.

- shape:

  A numeric value or character string specifying the shape of the points
  in `SpatPointsPlot`.

- hex:

  Whether to use hex plot. Default is FALSE.

- hex_linewidth:

  A numeric value of the hex line width. Default is 0.5.

- hex_count:

  Whether to count the hex.

- hex_bins:

  A numeric value of the hex bins. Default is 50.

- hex_binwidth:

  A numeric value of the hex bin width. Default is NULL.

- label:

  Whether to show the labels of groups. Default is FALSE.

- label_size:

  A numeric value of the label size. Default is 4.

- label_fg:

  A character string of the label foreground color. Default is "white".

- label_bg:

  A character string of the label background color. Default is "black".

- label_bg_r:

  A numeric value of the background ratio of the labels. Default is 0.1.

- label_repel:

  Whether to repel the labels. Default is FALSE.

- label_repulsion:

  A numeric value of the label repulsion. Default is 20.

- label_pt_size:

  A numeric value of the label point size. Default is 1.

- label_pt_color:

  A character string of the label point color. Default is "black".

- label_segment_color:

  A character string of the label segment color. Default is "black".

- label_insitu:

  Whether to place the raw labels (group names) in the center of the
  points with the corresponding group. Default is FALSE, which uses
  numbers instead of raw labels.

- label_pos:

  A character string or a function specifying the position of the
  labels.

  - "mean": Place labels at the mean position of the points in each
    group. Same as `function(x) mean(x, na.rm = TRUE)`.

  - "center": Place labels at the center of the points in each group.
    Same as `function(x) mean(range(x, na.rm = TRUE))`.

  - "median": Place labels at the median position of the points in each
    group. Same as `function(x) median(x, na.rm = TRUE)`.

  - "first": Place labels at the first point in each group. Same as
    `function(x) x[1]`.

  - "last": Place labels at the last point in each group. Same as
    `function(x) x[length(x)]`.

  - "random": Place labels at a random point in each group. Same as
    `function(x) sample(x, 1)`.

  - "min": Place labels at the minimum position (both x and y) of the
    points in each group. Same as `function(x) min(x, na.rm = TRUE)`.

  - "max": Place labels at the maximum position (both x and y) of the
    points in each group. Same as `function(x) max(x, na.rm = TRUE)`.

- graph:

  A character string of column names or the indexes in the data for the
  graph data. Default is NULL. If "@graph" is provided, the graph data
  will be extracted from the data attribute 'graph'. The graph data
  should be an adjacency matrix (numeric matrix) with row and column
  names matching the point IDs. Or a data.frame with x, xend, y, yend
  and value columns. If so, `graph_x`, `graph_y`, `graph_xend`,
  `graph_yend`, and `graph_value` arguments can be used to specify the
  column names.

- graph_x:

  A character string of the x column name for the graph data.

- graph_y:

  A character string of the y column name for the graph data.

- graph_xend:

  A character string of the xend column name for the graph data.

- graph_yend:

  A character string of the yend column name for the graph data.

- graph_value:

  A character string of the value column name for the graph data.

- edge_size:

  A numeric vector of the edge size range. Default is c(0.05, 0.5).

- edge_alpha:

  A numeric value of the edge transparency. Default is 0.1.

- edge_color:

  A character string of the edge color. Default is "grey40".

## Examples

``` r
# \donttest{
set.seed(8525)
# --- SpatImagePlot ---
# Generate a sample SpatRaster
r <- terra::rast(
    nrows = 50, ncols = 40, vals = runif(2000),
    xmin = 0, xmax = 40, ymin = 0, ymax = 50,
    crs = ""
)
SpatImagePlot(r)

SpatImagePlot(r, raster = TRUE, raster_dpi = 20)

SpatImagePlot(r, alpha = 0.5, theme = "theme_blank",
    theme_args = list(add_coord = FALSE), fill_name = "value")

SpatImagePlot(r, ext = c(0, 10, 0, 10), flip_y = FALSE, palette = "viridis")


# --- SpatMasksPlot ---
m <- terra::rast(
   nrows = 50, ncols = 40,
   vals = sample(c(1:5, NA), 2000, replace = TRUE, prob = c(rep(0.04, 5), 0.8)),
   xmin = 0, xmax = 40, ymin = 0, ymax = 50,
   crs = ""
)
SpatMasksPlot(m, border_color = "red")

SpatMasksPlot(m, ext = c(0, 15, 0, 20), add_border = FALSE,
    palette_reverse = TRUE, fill_name = "value")
#> Warning: Raster pixels are placed at uneven horizontal intervals and will be shifted
#> ℹ Consider using `geom_tile()` instead.


# --- SpatShapesPlot ---
polygons <- data.frame(
   id = paste0("poly_", 1:10),
   cat = sample(LETTERS[1:3], 10, replace = TRUE),
   feat1 = rnorm(10),
   feat2 = rnorm(10),
   geometry = c(
       'POLYGON((64.6 75.3,66.0 70.5,66.4 70.2,67.0 69.8,72.8 70.4,64.6 75.3))',
       'POLYGON((56.7 63.0,52.3 65.6,48.0 63.2,51.2 55.7,57.1 59.2,56.7 63.0))',
       'POLYGON((9.9 16.5,9.3 15.9,8.0 13.1,11.5 7.8,17.8 11.3,9.9 16.5))',
       'POLYGON((64.9 37.2,60.3 37.4,57.6 31.7,58.9 29.3,64.0 28.1,64.9 37.2))',
       'POLYGON((30.5 49.1,22.4 46.5,22.4 43.9,30.9 41.9,31.6 42.9,30.5 49.1))',
       'POLYGON((78.3 57.8,70.5 61.6,71.6 52.7,72.2 52.5,77.4 54.5,78.3 57.8))',
       'POLYGON((41.8 23.8,41.3 25.9,41.0 26.4,36.5 28.7,35.8 28.6,41.8 23.8))',
       'POLYGON((15.7 75.9,14.2 74.4,15.7 67.5,23.0 69.8,23.4 71.7,15.7 75.9))',
       'POLYGON((80.7 37.4,75.3 31.3,77.1 28.5,82.5 28.0,83.1 28.5,80.7 37.4))',
       'POLYGON((15.5 37.8,14.4 38.6,7.3 32.6,8.3 30.9,15.1 30.2,15.5 37.8))'
   )
)

polygons <- terra::vect(polygons, crs = "EPSG:4326", geom = "geometry")

SpatShapesPlot(polygons)

SpatShapesPlot(polygons, ext = c(0, 20, 0, 20))

SpatShapesPlot(polygons, highlight = 'cat == "A"', highlight_color = "red2")

SpatShapesPlot(polygons, border_color = "red", border_size = 2)

SpatShapesPlot(polygons, fill_by = "cat", fill_name = "category")

# Let border color be determined by fill
SpatShapesPlot(polygons, fill_by = "cat", alpha = 0.6, border_color = TRUE)

SpatShapesPlot(polygons, fill_by = "feat1")

SpatShapesPlot(polygons, fill_by = c("feat1", "feat2"), palette = "RdYlBu")


# --- SpatPointsPlot ---
# create some random points in the above polygons
points <- data.frame(
  id = paste0("point_", 1:30),
  gene = sample(LETTERS[1:3], 30, replace = TRUE),
  feat1 = runif(30, 0, 100),
  feat2 = runif(30, 0, 100),
  size = runif(30, 1, 5),
  x = c(
    61.6, 14.3, 12.7, 49.6, 74.9, 58.9, 13.9, 24.7, 16.9, 15.6,
    72.4, 60.1, 75.4, 14.9, 80.3, 78.8, 16.7, 27.6, 48.9, 52.5,
    12.9, 11.8, 50.4, 25.6, 10.4, 51.9, 73.4, 26.8, 50.4, 60.0
  ),
  y = c(
    32.1, 12.8, 33.2, 59.9, 57.8, 31.9, 10.1, 46.8, 75.3, 69.0,
    60.0, 29.4, 54.2, 34.2, 35.3, 33.1, 74.7, 48.0, 63.2, 59.2,
    9.2, 15.1, 64.5, 47.1, 11.4, 60.1, 54.1, 44.5, 61.9, 30.3
  )
)

SpatPointsPlot(points)

SpatPointsPlot(points, color_by = "gene", size_by = "size", shape = 22,
  border_size = 1)

SpatPointsPlot(points, raster = TRUE, raster_dpi = 30, color_by = "feat1")

SpatPointsPlot(points, color_by = c("feat1", "feat2"), size_by = "size")

SpatPointsPlot(points, color_by = "feat1", upper_cutoff = 50)

SpatPointsPlot(points, color_by = "feat1", hex = TRUE)

SpatPointsPlot(points, color_by = "gene", label = TRUE)

SpatPointsPlot(points, color_by = "gene", highlight = 1:20,
  highlight_color = "red2", highlight_stroke = 0.8)


# --- Graph/Network functionality ---
# Create a simple adjacency matrix for demonstration
set.seed(8525)
graph_mat <- matrix(0, nrow = 30, ncol = 30)
# Add some random connections with weights
for(i in 1:30) {
  neighbors <- sample(setdiff(1:30, i), size = sample(2:5, 1))
  graph_mat[i, neighbors] <- runif(length(neighbors), 0.1, 1)
}
rownames(graph_mat) <- colnames(graph_mat) <- rownames(points)
attr(points, "graph") <- graph_mat

SpatPointsPlot(points, color_by = "gene", graph = "@graph",
  edge_color = "grey60", edge_alpha = 0.3)

SpatPointsPlot(points, color_by = "feat1", graph = graph_mat,
  edge_size = c(0.1, 1), edge_alpha = 0.5)


# --- Use the `return_layer` argument to get the ggplot layers
ext = c(0, 40, 0, 50)
ggplot2::ggplot() +
  SpatImagePlot(r, return_layer = TRUE, alpha = 0.2, ext = ext) +
  SpatShapesPlot(polygons, return_layer = TRUE, ext = ext, fill_by = "white") +
  SpatPointsPlot(points, return_layer = TRUE, ext = ext, color_by = "feat1") +
  theme_box() +
  ggplot2::coord_sf(expand = 0) +
  ggplot2::scale_y_continuous(labels = function(x) -x)


# }
```
