# An example of clusterProfiler enrichment result with multiple databases

An example of clusterProfiler enrichment result with multiple databases

## Examples

``` r
# \dontrun{
if (interactive()) {
  data(enrich_example, package="plotthis")
  enrich_example$Database <- "DB1"
  enrich_example2 <- enrich_example
  enrich_example2$Database <- "DB2"
  enrich_example2$ID <- paste0(enrich_example2$ID, "_DB2")
  enrich_example2$Description <- paste0(enrich_example2$Description, " (DB2)")
  enrich_multidb_example <- rbind(enrich_example, enrich_example2)
}
# }
```
