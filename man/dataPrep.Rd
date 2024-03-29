% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dataPrep.R
\name{dataPrep}
\alias{dataPrep}
\title{Data standardization and phylogenetic principal component analysis}
\usage{
dataPrep(phy, dat, colnum = NULL, scale = F, method = "BM", mode = "cov")
}
\arguments{
\item{phy}{A phylogenetic tree of class phylo.}

\item{dat}{A vector or matrix containing dependent variable/s.}

\item{colnum}{The column with body size data (defaults to NULL).}

\item{scale}{A logical argument whether to centre and scale the data.}

\item{method}{Method to obtain the correlation structure: can be "BM" or "lambda"
(same as in \code{\link[phytools]{phyl.pca}}; defaults to "BM").}

\item{mode}{mode for the PCA: can be "cov" or "corr" (same as in \code{\link[phytools]{phyl.pca}}; defaults to "cov").}
}
\description{
Data standardization and phylogenetic principal component analysis.
}
\details{
This function standardizes multivariate data, performs a phylogenetic principal component analysis
using a phylogeny using utilities of R package "ape" and "phytools", and returns the results as a phyl.pca
object. The data can also be phylogenetically size-corrected via GLS regression, following which the
residuals are used for PCA.
}
\examples{
\dontrun{
dataPrep(phy=sample.tree.Tiru, dat=Col_concat, colnum=NULL, scale=F, method="BM", mode="cov")
}

}
