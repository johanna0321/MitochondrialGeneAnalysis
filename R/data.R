#' Rat Gene Count Metadata
#'
#' A dataframe containing the rat meta data used for DESeq experiment design 
#'
#' @format ## `expected.counts.meta`
#' A data frame with 32 observations of 7 variables
#' \describe{
#'   \item{Core.ID}{Sample ID assigned by the University of Michigan Advanced Genomics
#'   Core for linking sample data to `expected.counts`}
#'   \item{Exp.ID}{Rat ID used for experimental identification}
#'   \item{Line}{High Capacity Runner (HCR) or Low Capacity Runner (LCR) rat line identifier}
#'   \item{Sex}{Female or male rat sex identifer}
#'   \item{Treatment}{Water or saline, as control treatment identifiers from their
#'   respective experiments}
#'   \item{Name}{Matches Exp.ID}
#'   \item{Exp}{Experiment number, which identifies the RNA-Sequencing batch}
#' }
#' @source University of Michigan
"expected.counts.meta"



#' Rat Expected Gene Counts (RNA-Seq)
#'
#' A dataframe containing the aligned counts from RNA-Sequencing
#'
#' @format ## `expected.counts`
#' A data frame with 32883 observations of 32 variables where column names correspond
#' to `Core.ID` from `expected.counts.meta` and row names correspond to Ensembl 
#' `gene_id` from `gene_name_conversion`
#'
#' @source https://brcf.medicine.umich.edu/cores/advanced-genomics University of Michigan Advanced Genomics Core 
"expected.counts"



#' Gene Name Conversion
#'
#' A dataframe containing rattus norvegicus gene names as ENSEMBL IDs, Entrez IDs, 
#' NCBI gene name, and gene descriptions
#'
#' @format ## `gene_name_conversion`
#' A data frame with 32883 observations of 4 variables
#' \describe{
#'   \item{gene_id}{Ensembl ID}
#'   \item{entrez_id}{Entrez ID}
#'   \item{external_gene_name}{NCBI Gene Name}
#'   \item{description}{Gene description}
#'   }
#' @source https://brcf.medicine.umich.edu/cores/advanced-genomics University of Michigan Advanced Genomics Core 
"gene_name_conversion"

