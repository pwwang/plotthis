# Process the enrichment results from Enrichr

Process the enrichment results from Enrichr

## Usage

``` r
prepare_enrichr_result(data, dbname = "Database", n_input = NULL)
```

## Arguments

- data:

  A data frame containing the result by Enrichr.

- dbname:

  A character string specifying the name of the database column.

- n_input:

  An integer specifying the number of input genes. Enrichr result
  doesn't ship with the number of input genes. You can either provide
  the number directly or we will infer it. See details.

## Value

A data frame that can be used in `EnrichMap`.

## Details

In order to use the `EnrichMap` and `EnrichNetwork` functions and other
visualization functions in `plotthis`, the enrichment results from
Enrichr need to be processed by the `prepare_enrichr_result` function.
The following columns are renamed:

- `Term` -\> `Description`

- `Genes` -\> `geneID` (separated replaced by `/`)

- `P.value` -\> `pvalue`

- `Adjusted.P.value` -\> `p.adjust` Additionally, GeneRatio and BgRatio
  columns are inferred. From [enrichr's
  documentation](https://maayanlab.cloud/Enrichr/help#background), the
  oddsRatio is defined as:
  `oddsRatio = (A * (D - B - C + A) / max((B - A) * (C - A), 1)`, where
  A is the overlapping genes; B is the total genes in the gene set; C
  (n_input) is the genes in input list; D is the total genes in the
  background. D is not provided by Enrichr. To infer it,
  `D = oddsRatio * max((B - A) * (C - A), 1) / A + B + C - A`.

- `Overlap = A / B` (from Enrichr)

- `GeneRatio = A / C` (from ClusterProfiler)

- `BgRatio = B / D` (from ClusterProfiler) `C (n_input)`, if not
  provided, will be inferred when `D` for all terms are equal. When
  starting inferrence, the minimum value to try will be unique genes in
  `data$Genes`/`data$geneID`.
