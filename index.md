# plotthis

`plotthis` is an R package that is built upon `ggplot2` and other
plotting packages. It provides high-level APIs and a wide range of
options to create stunning, publication-quality plots effortlessly.

## Installation

``` r
install.packages("plotthis")
# or to install the latest version:
remotes::install_github("pwwang/plotthis")
devtools::install_github("pwwang/plotthis")
```

You can also install the package using `conda`:

    $ conda install pwwang::r-plotthis

## Gallery

[`AreaPlot`](https://pwwang.github.io/plotthis/reference/AreaPlot.html)
/
[`TrendPlot`](https://pwwang.github.io/plotthis/reference/TrendPlot.html)
/
[`ROCCurve`](https://pwwang.github.io/plotthis/reference/ROCCurve.html)

![](reference/figures/area-trend.png)

[`ClustreePlot`](https://pwwang.github.io/plotthis/reference/ClustreePlot.html)
/
[`LinePlot`](https://pwwang.github.io/plotthis/reference/LinePlot.html)
/ [`Network`](https://pwwang.github.io/plotthis/reference/Network.html)

![](reference/figures/clustree-line.png)

[`Heatmap`](https://pwwang.github.io/plotthis/reference/Heatmap.html)

![](reference/figures/heatmap.png)

[CorPlot](https://pwwang.github.io/plotthis/reference/CorPlot.html) /
[CorPairsPlot](https://pwwang.github.io/plotthis/reference/CorPairsPlot.html)

![](reference/figures/cor-corpairs.png)

[`PieChart`](https://pwwang.github.io/plotthis/reference/PieChart.html)
/
[`RingPlot`](https://pwwang.github.io/plotthis/reference/RingPlot.html)
/ [`QQPlot`](https://pwwang.github.io/plotthis/reference/QQPlot.html)

![](reference/figures/pie-ring.png)

[`VolcanoPlot`](https://pwwang.github.io/plotthis/reference/VolcanoPlot.html)
/
[`WordCloudPlot`](https://pwwang.github.io/plotthis/reference/WordCloudPlot.html)

![](reference/figures/volcano-wordcloud.png)

[`BarPlot`](https://pwwang.github.io/plotthis/reference/barplot.html) /
[`SplitBarPlot`](https://pwwang.github.io/plotthis/reference/barplot.html)

![](reference/figures/bar-splitbar.png)

[`BoxPlot`](https://pwwang.github.io/plotthis/reference/boxviolinplot.html)
/
[`ViolinPlot`](https://pwwang.github.io/plotthis/reference/boxviolinplot.html)

![](reference/figures/box-violin.png)

[`ChordPlot`](https://pwwang.github.io/plotthis/reference/chordplot.html)
/
[`UpsetPlot`](https://pwwang.github.io/plotthis/reference/upsetplot1.html)
/
[`VennDiagram`](https://pwwang.github.io/plotthis/reference/venndiagram1.html)

![](reference/figures/chord-upset-venn.png)

[`DensityPlot`](https://pwwang.github.io/plotthis/reference/densityhistoplot.html)
/
[`Histogram`](https://pwwang.github.io/plotthis/reference/densityhistoplot.html)
/
[`RidgePlot`](https://pwwang.github.io/plotthis/reference/RidgePlot.html)

![](reference/figures/density-histogram.png)

[`DimPlot`](https://pwwang.github.io/plotthis/reference/dimplot.html) /
[`FeatureDimPlot`](https://pwwang.github.io/plotthis/reference/dimplot.html)
/
[`VelocityPlot`](https://pwwang.github.io/plotthis/reference/VelocityPlot.html)

![](reference/figures/dimplot.png)

[`DotPlot`](https://pwwang.github.io/plotthis/reference/dotplot.html) /
[`ScatterPlot`](https://pwwang.github.io/plotthis/reference/dotplot.html)
/
[`LollipopPlot`](https://pwwang.github.io/plotthis/reference/dotplot.html)

![](reference/figures/dot-scatter-lollipop.png)

[`EnrichMap`](https://pwwang.github.io/plotthis/reference/enrichmap1.html)
/
[`EnrichNetwork`](https://pwwang.github.io/plotthis/reference/enrichmap1.html)

![](reference/figures/enrich.png)

[`GSEASummaryPlot`](https://pwwang.github.io/plotthis/reference/gsea.html)
/ [`GSEAPlot`](https://pwwang.github.io/plotthis/reference/gsea.html)

![](reference/figures/gsea.png)

[`RadarPlot`](https://pwwang.github.io/plotthis/reference/radarplot.html)
/
[`SpiderPlot`](https://pwwang.github.io/plotthis/reference/radarplot.html)
/
[`RarefactionPlot`](https://pwwang.github.io/plotthis/reference/RarefactionPlot.html)

![](reference/figures/radar-spider.png)

[`SankeyPlot`](https://pwwang.github.io/plotthis/reference/sankeyplot.html)
/
[`AlluvialPlot`](https://pwwang.github.io/plotthis/reference/sankeyplot.html)

![](reference/figures/sankey-alluvial.png)

[`ManhattanPlot`](https://pwwang.github.io/plotthis/reference/ManhattanPlot.html)

![](reference/figures/manh.png)

[`SpatImagePlot`](https://pwwang.github.io/plotthis/reference/spatialplots.html)
/
[`SpatMasksPlot`](https://pwwang.github.io/plotthis/reference/spatialplots.html)
/
[`SpatShapesPlot`](https://pwwang.github.io/plotthis/reference/spatialplots.html)
/
[`SpatPointsPlot`](https://pwwang.github.io/plotthis/reference/spatialplots.html)

![](reference/figures/spatial.png)

## Credits

`plotthis` is greatly inspired by the
[`SCP`](https://zhanghao-njmu.github.io/SCP/index.html) package, but
with the plotting functions detached from the Seurat object or
single-cell data analysis. It is designed to be more flexible and
general-purpose, and can be used for a wide range of data types and
analysis scenarios.
