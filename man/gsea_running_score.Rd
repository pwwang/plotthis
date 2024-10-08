% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gsea.R
\name{gsea_running_score}
\alias{gsea_running_score}
\title{Get the running enrichment score of a gene set}
\usage{
gsea_running_score(genes, gene_ranks, exponent = 1, hits_only = TRUE)
}
\arguments{
\item{genes}{A vector of genes}

\item{gene_ranks}{A numeric vector of gene ranks with names}

\item{exponent}{A numeric value to raise the gene ranks to}

\item{hits_only}{A logical value to return only the running enrichment score of the hits}
}
\value{
A numeric vector of the running enrichment score
}
\description{
Get the running enrichment score of a gene set
}
\keyword{internal}
