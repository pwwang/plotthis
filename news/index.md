# Changelog

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
