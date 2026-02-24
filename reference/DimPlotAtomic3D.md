# Internal helper to create a 3D dimension reduction plot using plotly

Internal helper to create a 3D dimension reduction plot using plotly

## Usage

``` r
DimPlotAtomic3D(
  data,
  dims,
  group_by = NULL,
  features = NULL,
  colorby,
  colors = NULL,
  feat_colors_value = NULL,
  label_use = NULL,
  labels_tb = NULL,
  keep_empty_group = FALSE,
  bg_color = "grey80",
  color_name = "",
  pt_size = NULL,
  pt_alpha = 1,
  show_stat = TRUE,
  title = NULL,
  subtitle = NULL,
  xlab = NULL,
  ylab = NULL,
  label = FALSE,
  label_insitu = FALSE,
  label_size = 4,
  label_fg = "white",
  label_bg = "black",
  highlight = NULL,
  highlight_color = "black",
  highlight_size = 1,
  highlight_stroke = 0.8,
  highlight_alpha = 1,
  graph = NULL,
  edge_size = c(0.05, 0.5),
  edge_alpha = 0.1,
  edge_color = "grey40",
  lineages = NULL,
  lineages_trim = c(0.01, 0.99),
  lineages_span = 0.75,
  lineages_palette = "Dark2",
  lineages_palcolor = NULL,
  palette = "Spectral",
  palcolor = NULL,
  n_sampled = NULL
)
```
