# Changelog

## Version 0.13.1

- BREAKING(LinkedHeatmap): update parameter names for link width and
  color in LinkedHeatmap and LinkedHeatmapAtomic
- BREAKING(LinkedHeatmap): replace title_gp with title_params for
  improved title customization
- feat(heatmap): consolidate annotation arguments into structured list
  params
- feat(LinkedHeatmap): enhance link width customization with constant
  value option
- feat: add y_brackets parameter for significance brackets in
  box/violin/beeswarm plots
- feat(RidgePlot): add x_min and x_max parameters for x-axis limits
- fix(heatmap): fix name annotation error when it has empty levels
- fix(heatmap): update row and column title checks to use annotation
  parameters
- fix(LinkedHeatmap): fix alignment when columnt_title provided for
  either heatmap
- fix(ViolinPlot): conditionally load ggpubr for pairwise comparisons
  (kassambara/ggpubr#751)
- docs: update plot descriptions for clarity and consistency across
  documentation

## Version 0.13.0

- feat: add split_by parameter to combine_plots for enhanced data
  composition, especially for split plots
- feat: add LinkedHeatmap and supporting functions with documentation
  and tests
- feat: uniform plotting data access from plot objects
- feat: use a fast do.call across all plotting functions
- feat(dotplot): add border customization options for dots and lollipop
  bars ([\#32](https://github.com/pwwang/plotthis/issues/32))
- feat(dotplot): update fill_cutoff parameter to accept string operators
  and improve documentation
  ([\#31](https://github.com/pwwang/plotthis/issues/31))
- feat(combine_plots): enhance split_by functionality to ensure combined
  data is accessible and correctly structured
- feat: add prepare_continuous_color_scale() utility and refactor
  DimPlot
- feat: add color scale quantile/cutoff params to DotPlot, ScatterPlot,
  BarPlot, SplitBarPlot
  ([\#30](https://github.com/pwwang/plotthis/issues/30))
- fix(chordplot): correct handling of split_by parameter when combining
  plots
- fix(heatmap): remove return_grob parameter and wrap heatmap with
  patchwork
- fix(radarplot): correct assignment of split_by variable and update
  combine_plots call
- fix(combine_plots): refine data handling in combine_plots function for
  layer-specific data integrity
- fix(utils): adjust color scale handling for edge cases in
  `prepare_continuous_color_scale`
- fix: include split_by in keep_na and keep_empty checks for
  FeatureDimPlot function
- style: use air to format all source files
- chore: add .codegraph to .gitignore to exclude codegraph files from
  version control
- chore: remove unused imports and clean up parameter documentation
  across multiple files
- docs: enhance documentation for color palette usage and plotting data
  access
- docs(Heatmap): refactor HeatmapAtomic documentation for clarity and
  consistency
- docs: enhance documentation for AreaPlot and AreaPlotAtomic functions
- docs: enhance documentation for SplitBarPlotAtomic and BarPlot
  functions
- docs: update documentation for BoxPlot, ViolinPlot, and BeeswarmPlot
  functions
- docs: enhance documentation for ChordPlot and ChordPlotAtomic
  functions
- docs: enhance RidgePlot and RidgePlotAtomic documentation and
  functionality
- docs: enhance documentation for DimPlot and FeatureDimPlot functions
- docs: enhance documentation for DotPlot and LollipopPlot functions
- docs: enhance documentation for ClustreePlot and ClustreePlotAtomic
  functions
- docs: enhance documentation for CorPairsPlotAtomic and CorPlot
- docs: update documentation for DimPlot and DimPlotAtomic parameters
- docs: update documentation for EnrichMap and EnrichNetwork functions
- docs: enhance documentation for GSEASummaryPlot and GSEAPlot functions
- docs: remove unused import for ggridges in densityplot and NAMESPACE
- docs: enhance documentation for JitterPlot and JitterPlotAtomic
- docs: update documentation for LinePlotAtomic, LinePlotGrouped, and
  LinePlotSingle
- docs: enhance ManhattanPlot and ManhattanPlotAtomic documentation
- docs: enhance NetworkAtomic documentation with detailed argument
  descriptions and architecture overview
- docs: update documentation for PieChart and PieChartAtomic functions
  with detailed descriptions and parameter explanations
- docs: update QQPlot and QQPlotAtomic documentation to include PP plot
  details and enhance parameter descriptions
- docs: enhance documentation for RadarPlotAtomic and radarplot
  functions
- docs: enhance documentation for RingPlot and RingPlotAtomic functions
  with detailed descriptions and parameter explanations
- docs: enhance documentation for RarefactionPlot and
  RarefactionPlotAtomic functions with detailed descriptions and
  parameter explanations
- docs: enhance documentation for ROCCurveAtomic and get_cutoffs_data
- docs: enhance documentation for SankeyPlot and SankeyPlotAtomic
- docs: enhance ScatterPlot and ScatterPlotAtomic documentation
- docs: improve documentation for .compute_velocity_on_grid and .flip_y
  functions with detailed descriptions and parameter explanations
- docs: enhance spatial visualization functions in terra package
- docs: enhance documentation for TrendPlot and TrendPlotAtomic
  functions with detailed descriptions and parameter explanations
- docs: update documentation for UpsetPlot and related functions
- docs: update VennDiagram and related functions for improved
  documentation and clarity
- docs: enhance documentation for VelocityPlot function with detailed
  parameter descriptions and usage examples
- docs: enhance VolcanoPlot and VolcanoPlotAtomic documentation
- docs: enhance documentation for WordCloudPlot and WordCloudPlotAtomic
  functions with detailed descriptions and usage examples

## Version 0.12.1

CRAN release: 2026-06-10

- feat: enhance check_palcolor function to handle character palcolor
  input more robustly
- feat: add position_dodge_preserve parameter to BoxViolinPlot and
  BoxViolinPlotAtomic for enhanced dodging control
- feat: add CLAUDE.md for project guidance and development workflow
  documentation
- feat(Heatmap): add block and label annotation types with unified
  building, per-annotation side control, reordering, validation, and
  split_by support
- feat(dotplot): add size_min and size_max parameters for dot size
  control ([\#29](https://github.com/pwwang/plotthis/issues/29))
- fix(.Rbuildignore): add CLAUDE.md and PDF files to ignore list
- refactor(Heatmap): move utils to a separate file
- refactor: update dplyr and tidyr functions to use all_of for column
  selection
- docs: add examples for CorPlot and DensityPlot functions
- test(DotPlot): suppress warnings when using fill_by for size
  calculation
- test(ManhattanPlot): update highlight threshold from 1e-5 to 1e-3

## Version 0.12.0

CRAN release: 2026-04-28

- chore: update .Rbuildignore and .gitignore to exclude .vscode
  directory
- refactor(Heatmap): remove deprecated annotation parameters and
  introduce alias support
- feat(Heatmap): add default dimensions for simple annotations in
  Heatmap function
- fix(Heatmap): resolve annotation name and legend overlap issue in
  ComplexHeatmap
- feat: set default figure dimensions in pkgdown and basic design
  vignette
- test: add unit tests for plot functions
- ci: add missing ‘remotes’ package to workflow dependencies
- fix(Heatmap): add validation to ensure data is not empty in
  process_heatmap_data function
- feat(Heatmap): add draw_opts parameter for additional drawing options
- feat(Heatmap): add aspect.ratio parameter for cell dimension control
- fix(Heatmap): update cell dimension calculations to use actual matrix
  dimensions
- fix(Heatmap): improve handling of annotation names to prevent overlap
  with legends
- feat(Heatmap): add alignment options for heatmap and annotation
  legends
- feat(Heatmap): add label customization options including color, size,
  and legend title
- feat(Heatmap): add support for marks on heatmap cells with
  customization options
- feat(Heatmap): add base_size parameter for scaling heatmap size
- feat(Heatmap): enhance more aliases support for row and column
  annotations
- feat(Heatmap): improve aspect ratio handling and dimension constraints
  for heatmap plots
- feat(Heatmap): add support for combined label and mark cell type
- feat(Heatmap): add octagon mark type support for heatmap cells

## Version 0.11.1

CRAN release: 2026-03-15

- feat(BoxPlot): add support for bar plots with error bars
- chore: add additional repositories link to DESCRIPTION file for ggmanh

## Version 0.11.0

- feat(palette): add tableau color palettes to palette list
- feat(dimplot): add support for 3D plots using plotly for 3-component
  dimplots
- feat(UpsetPlot): preserve group order in compmatrix (lower table)
- feat(UpsetPlot): add combmatrix_gap parameter for row spacing in
  combination matrix
- feat: implement calculate_plot_dimensions() across all plot functions
- feat: enhance heatmap dimension calculations for row and column labels
  based on visibility settings
- feat: add padding parameter to Heatmap for customizable heatmap
  spacing
- fix(UpsetPlot): nudge label position in geom_text_repel for better
  visibility
- fix(UpsetPlot): fix ytrans not working
- fix(Heatmap): handle empty string cases for row and column name
  parameters in process_heatmap_data
- fix(DimPlot): hide legend for fill and color scales for marks in
  dimension plot
- fix(DimPlot): ensure all levels are included in the legend by keeping
  empty levels for stat plots
- fix(DimPlot): disable legend for line width scale in dimension plot
  when graph is given
- fix(Box/Violin/BeeswarmPlot): update variance check to allow more
  unique values for comparison
- chore(VennDiagram): change default palette to ‘Blues’
- chore(UpsetPlot): change default palette to ‘Blues’
- chore(UpsetPlot): change segment colour of labels to NA (hiding
  segment lines)
- ci: update ggrepel package version to 0.9.5 in dependencies (0.9.6
  requires R 4.5)

## Version 0.10.1

- BREAKING(Box/ViolinPlot): update sort_x parameter to accept
  expressions for x-axis ordering
- feat(RadarPlot): add groups parameter to filter and order groups in
  the plot
- feat(Heatmap): add rows_orderby and columns_orderby to order rows and
  columns (if set, cluster_rows and cluster_columns will default to
  FALSE)
- fix: update BarPlotSingle to conditionally use geom_text for flipped
  plots and adjust height/width calculations
- fix(Box/ViolinPlo): hide legends for color, size, and alpha scales for
  highlighted points
- fix(BeeswarmPlot):fix highlight not working
- fix(JitterPlot): fix
  [`position_jitterdodge()`](https://ggplot2.tidyverse.org/reference/position_jitterdodge.html)
  requires at least one aesthetic to dodge by for ggplot2 v3
- fix(BarPlot): correct conditional assignment for `fill_by` parameter
- fix(BarPlot): fix when x has multiple columns
- fix(Heatmap): ensure ordered factors are converted to character for
  proper processing
- fix(TrendPlot): complete missing combinations for area layer to
  prevent interpolation issues
- fix(AreaPlot): complete missing combinations for x, group_by, and
  facet_by to prevent interpolation issues
- fix(RadarPlot): clarify groups parameter documentation and its
  implications on keep_empty
- fix: ensure unique values in for loops for grouping in bar, pie, ring,
  and trend plots
- fix(heatmap): ensure unique values in processed data for heatmap

## Version 0.10.0

CRAN release: 2026-02-06

### New Features

- Add `keep_na` and `keep_empty` parameters to control handling of NA
  and empty values across multiple plot functions: AreaPlot, BarPlot,
  BoxPlot, ChordPlot, DensityHistoPlot, DimPlot, DotPlot, Heatmap,
  JitterPlot, LinePlot, PieChart, RadarPlot, SpiderPlot, RingPlot,
  SankeyPlot, TrendPlot, and VelocityPlot
- Add continuous color mapping support for BarPlot and SplitBarPlot
  ([\#28](https://github.com/pwwang/plotthis/issues/28))
- Allow `palcolor` parameter to allow replacing colors by `palette`

### Bug Fixes

- fix: update color handling in various plot functions to ensure
  consistent NA value representation
- fix: enhance `palette_this` function to support custom colors and NA
  handling
- fix: add `na.value` parameter to scale functions for consistent NA
  color handling across LinePlot and BarPlotSingle
- fix: update `check_keep_na` and `check_keep_empty` functions to return
  named lists
- fix: update `fill_by` condition in BarPlotAtomic to handle missing
  values correctly
- fix: update breaks and limits parameters in scale_fill_manual for
  BarPlotSingle

### Documentation

- docs: enhance documentation for `keep_na` and `keep_empty` parameters
  across all affected functions
- docs: enhance documentation for color palette customization and NA
  handling in plots

------------------------------------------------------------------------

## Version 0.9.3

CRAN release: 2026-01-09

### New Features

- Add beeswarm plot support to BoxPlot and ViolinPlot
  ([\#26](https://github.com/pwwang/plotthis/pull/26))

### Bug Fixes

- fix(RidgePlot): drop unused levels from split_by variable
- fix(DimPlot): handle NA values in ordering for high-top and low-top
  options
- fix(docs): update link to all available color palettes in basic design
  vignette
- fix(docs): update link format for all available palettes in
  palette_this documentation
- fix(docs): remove outdated link to all available palettes in
  documentation

### Documentation

- docs: add BeeswarmPlot to the list of available plots in README

**Full Changelog:**
[0.9.1…0.9.3](https://github.com/pwwang/plotthis/compare/0.9.1...0.9.3)

**Note:** Version 0.9.2 was skipped due to CRAN submission failure.

------------------------------------------------------------------------

## Version 0.9.1

### New Features

- feat: add beeswarm plot support to BoxPlot and ViolinPlot
  ([\#26](https://github.com/pwwang/plotthis/pull/26))

### Bug Fixes

- fix(RidgePlot): drop unused levels from split_by variable
- fix(DimPlot): handle NA values in ordering for high-top and low-top
  options
  ([pwwang/scplotter#29](https://github.com/pwwang/scplotter/issues/29))
- fix(docs): update link to all available color palettes in basic design
  vignette

### Documentation

- docs: add BeeswarmPlot to the list of available plots in README

### Contributors

- @Copilot made their first contribution in
  [\#26](https://github.com/pwwang/plotthis/pull/26)

**Full Changelog:**
[0.9.0…0.9.1](https://github.com/pwwang/plotthis/compare/0.9.0...0.9.1)

------------------------------------------------------------------------

## Version 0.9.0

CRAN release: 2025-12-11

### Bug Fixes

- fix(box/violinplot): comment out error message for group_by
  comparisons
- fix(boxviolinplot): skip processing for data frames with less than 2
  rows
- fix(network): update ggplot2 version check for link_type_by support
  ([\#21](https://github.com/pwwang/plotthis/issues/21))
- fix(velocityplot): update ggplot2 version check for arrow length
  handling ([\#19](https://github.com/pwwang/plotthis/issues/19),
  [\#22](https://github.com/pwwang/plotthis/issues/22))
- chore(heatmap): add warning for unknown arguments in HeatmapAtomic
  function
- fix(boxviolinplot): make sig_labelsize relative to theme base_size

### Documentation

- docs(ROCCurve): update example to include cutoffs_at parameter for
  clarity

**Full Changelog:**
[0.8.2…0.9.0](https://github.com/pwwang/plotthis/compare/0.8.2...0.9.0)

------------------------------------------------------------------------

## Version 0.8.2

CRAN release: 2025-11-10

### Bug Fixes

- fix(prepare_fgsea_result): correct NES NA handling and add print
  statement
- fix(GSEASummaryPlot): remove debug print statement

**Full Changelog:**
[0.8.1…0.8.2](https://github.com/pwwang/plotthis/compare/0.8.1...0.8.2)

------------------------------------------------------------------------

## Version 0.8.1-rc7 (Release Candidate)

CRAN release: 2025-10-16

### New Features

- feat(jitterplot): add order_by parameter for data labeling

### Documentation

- chore(jitterplot): use simpler order_by parameter in examples

**Full Changelog:**
[0.8.1-rc6…0.8.1-rc7](https://github.com/pwwang/plotthis/compare/0.8.1-rc6...0.8.1-rc7)

------------------------------------------------------------------------

## Version 0.8.1-rc6 (Release Candidate)

CRAN release: 2025-10-16

### Bug Fixes

- fix(heatmap): preserve the column/row order if given as factor in long
  form
- fix(volcanoplot): use parameter linewidth for cutoffs instead of
  deprecated size
- fix(heatmap): optimize data grouping and processing for dot_size
- fix(heatmap): fix default dot size data for flipped plot

### Documentation

- docs(heatmap): change example of making row/column annotation thinner
  to thicker
- docs(volcanoplot): add missing documentation for label_by argument

**Full Changelog:**
[0.8.1-rc5…0.8.1-rc6](https://github.com/pwwang/plotthis/compare/0.8.1-rc5...0.8.1-rc6)

------------------------------------------------------------------------

## Version 0.8.1

CRAN release: 2025-10-16

### New Features

- feat(box/violin): add support for paired observations in Box/Violin
  plots
- feat(RadarPlot/SpiderPlot): add background color and transparency
  options and fix radial grid lines across y=0
- feat(boxviolinplot): add support for paired tests
- feat(heatmap): allow setting parameters for name annotations and set
  default width/height to 2.5mm
- feat(heatmap): enhance dot size functionality to support indexes and
  dim names
- feat(jitterplot): add order_by parameter for data labeling

### Bug Fixes

- fix(dotplot): update color handling in DotPlotAtomic function due to
  ggplot2 v4
- fix(barplot): correct label positioning and scaling in BarPlotGrouped
- fix(heatmap): prevent double printing in pkgdown with ggplot2 \>= 4
- fix(boxviolinplot): enhance pairwise comparison handling when tests
  fail
- fix(heatmap): update viewport handling for pie and boxviolin layers
- fix(heatmap): prevent double printing in pkgdown with return_grob flag
- fix(heatmap): preserve the column/row order if given as factor in long
  form
- fix(heatmap): ensure grouping retains all levels
- fix(heatmap): optimize data grouping and processing for dot_size
- fix(heatmap): fix default dot size data for flipped plot

### Documentation

- docs(heatmap): change example of making row/column annotation thinner
  to thicker
- docs(volcanoplot): add missing documentation for label_by argument
- chore(jitterplot): use simpler order_by parameter in examples

**Full Changelog:**
[0.8.0…0.8.1](https://github.com/pwwang/plotthis/compare/0.8.0...0.8.1)

------------------------------------------------------------------------

## Version 0.8.1-rc5 (Release Candidate)

CRAN release: 2025-10-16

### New Features

- feat(heatmap): allow setting parameters for name annotations and set
  default width/height to 2.5mm
- feat(heatmap): enhance dot size functionality to support indexes and
  dim names

### Bug Fixes

- fix(heatmap): update viewport handling for pie and boxviolin layers
- fix(heatmap): prevent double printing in pkgdown with return_grob flag

**Full Changelog:**
[0.8.1-rc4…0.8.1-rc5](https://github.com/pwwang/plotthis/compare/0.8.1-rc4...0.8.1-rc5)

------------------------------------------------------------------------

## Version 0.8.1-rc4 (Release Candidate)

CRAN release: 2025-10-16

### New Features

- feat(box/violin): add support for paired observations in Box/Violin
  plots
- feat(RadarPlot/SpiderPlot): add background color and transparency
  options and fix radial grid lines across y=0

### Bug Fixes

- fix(dotplot): update color handling in DotPlotAtomic function due to
  ggplot2 v4
- fix(barplot): correct label positioning and scaling in BarPlotGrouped
- fix(heatmap): prevent double printing in pkgdown with ggplot2 \>= 4
- fix(boxviolinplot): enhance pairwise comparison handling when tests
  fail

**Full Changelog:**
[0.8.0…0.8.1-rc4](https://github.com/pwwang/plotthis/compare/0.8.0...0.8.1-rc4)

------------------------------------------------------------------------

## Version 0.8.0

CRAN release: 2025-09-23

### Major Announcement

🎆 **plotthis is now compatible with ggplot2 v4!**

### New Features

- feat: add JitterPlot

### Bug Fixes

- fix(gsea): handle zero normalization in running enrichment score
  calculation
- fix(CorPlot): handle when x has zero standard variation
  ([\#23](https://github.com/pwwang/plotthis/pull/23))
- fix(Heatmap): update return_grob logic to adopt ggplot2 v4
  ([\#20](https://github.com/pwwang/plotthis/issues/20))
- fix(BarPlot): adjust label_nudge parameter for bar plot functions

### Infrastructure

- chore: introduce renv for package dependency management
- chore(dotplot): update examples to avoid testing during documentation
  build
- chore(Network): handle ggplot2 v4 compatibility for link_type_by
  - Still need
    [thomasp85/ggraph#394](https://github.com/thomasp85/ggraph/issues/394)
    to be fixed to get full support
- chore(VelocityPlot): handle ggplot2 v4 compatibility for arrow lengths
  - Still need
    [tidyverse/ggplot2#6594](https://github.com/tidyverse/ggplot2/issues/6594)
    to be fixed to get full support
