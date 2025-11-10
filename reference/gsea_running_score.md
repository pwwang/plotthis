# Get the running enrichment score of a gene set

Get the running enrichment score of a gene set

## Usage

``` r
gsea_running_score(genes, gene_ranks, exponent = 1, hits_only = TRUE)
```

## Arguments

- genes:

  A vector of genes

- gene_ranks:

  A numeric vector of gene ranks with names

- exponent:

  A numeric value to raise the gene ranks to

- hits_only:

  A logical value to return only the running enrichment score of the
  hits

## Value

A numeric vector of the running enrichment score
