% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mitocarta_functions.R
\name{extract_mitocarta_data}
\alias{extract_mitocarta_data}
\title{Extract MitoCarta Data}
\source{
"https://personal.broadinstitute.org/scalvo/MitoCarta3.0"
}
\usage{
extract_mitocarta_data(species = "human", gene_id_type = "gene_symbol")
}
\arguments{
\item{species}{One of \code{mouse} or \code{human} to define whether the function should
extract the Human.MitoCarta3.0 information or the Mouse.MitoCarta3.0 information.
Defaults to \code{human}. Note that gene symbols from either data set will integrate
with other mammalian species.}

\item{gene_id_type}{One of one of \code{entrez_id}, \code{gene_symbol}, or \code{ensembl}. All
data extracted from MitoCarta will be maintained, but this option formats column
names for consistent downstream analysis.}
}
\value{
A list of dataframes containing gene-wise and pathway-wise information
from MitoCarta
}
\description{
This function extract MitoCarta3.0 data from the web. This function also formats
columns for use with other functions in the \code{MitochondrialGeneAnalyses}.
}
