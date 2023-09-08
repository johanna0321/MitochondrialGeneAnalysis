# MitochondrialGeneAnalysis

<!-- badges: start -->
[![R-CMD-check](https://github.com/johanna0321/MitochondrialGeneAnalysis/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/johanna0321/MitochondrialGeneAnalysis/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

## Overview
  
This package contains functions that scrape MitoCarta3.0 data from the web for easy assignment 
of a pre-defined list of genes to the mitochondrial pathways identified by MitoCarta, including 
systematic checking of MitoCarta gene names synonyms. This package also provides a function to 
plot the percent of each pathway that is represented in a filtered gene list or cluster of genes. 

The functions, data, and .Rmd workflow herein are a companion to the following article:

Fleischman, J. Y., Van den Bergh, F., Collins, N. L., Bowers, M., Beard, D. A., & Burant, C. F. (2023). Higher mitochondrial oxidative capacity is the primary molecular differentiator in muscle of rats with high and low intrinsic cardiorespiratory fitness. In Molecular Metabolism (Vol. 76, p. 101793). Elsevier BV. [https://doi.org/10.1016/j.molmet.2023.101793](https://doi.org/10.1016/j.molmet.2023.101793)

## Package Updates

This package is not frequently updated. Please contact the author for help or to address bugs. 

## Installation

First, download and install R and RStudio:
  
- [R](https://mirror.las.iastate.edu/CRAN/) 
- [RStudio](https://rstudio.com/products/rstudio/download/) (free version)

Then, open RStudio and install the `devtools` package

```
install.packages("devtools")
```

Finally, install the `MitochondrialGeneAnalysis` package

```
library(devtools)
devtools::install_github("johanna0321/MitochondrialGeneAnalysis")
```

## Usage

The following are included in the package and documentation is accessible using the 
help() function or ? help operator

Data:
```
expected.counts
expected.counts.meta
gene_name_conversion
```
  
Functions: 
```
extract_mitocarta_pathways()
check_mitocarta_synonyms()
assign_mitocarta_pathways()
plot_mitocarta_pathways()
```
