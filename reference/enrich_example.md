# An example of clusterProfiler enrichment result

An example of clusterProfiler enrichment result

## Examples

``` r
# \dontrun{
if (interactive()) {
  data(geneList, package="DOSE")
  de <- names(geneList)[abs(geneList) > 1.5]
  enrich_example <- clusterProfiler::enrichPathway(gene=de, pvalueCutoff = 0.05, readable=TRUE)
  enrich_example <- as.data.frame(enrich_example)
}
# }
```
