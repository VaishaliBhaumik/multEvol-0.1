#' Data standardization and phylogenetic principal component analysis
#'
#' @name dataPrep
#'
#' @description Data standardization and phylogenetic principal component analysis.
#'
#' @details This function standardizes multivariate data, performs a phylogenetic principal component analysis
#' using a phylogeny using utilities of R package "ape" and "phytools", and returns the results as a phyl.pca
#' object. The data can also be phylogenetically size-corrected via GLS regression, following which the
#' residuals are used for PCA.
#'
#' @param phy A phylogenetic tree of class phylo.
#' @param dat A vector or matrix containing dependent variable/s.
#' @param colnum The column with body size data (defaults to NULL).
#' @param scale A logical argument whether to centre and scale the data.
#' @param method Method to obtain the correlation structure: can be "BM" or "lambda"
#' (same as in \code{\link[phytools]{phyl.pca}}; defaults to "BM").
#' @param mode mode for the PCA: can be "cov" or "corr" (same as in \code{\link[phytools]{phyl.pca}}; defaults to "cov").
#'
#' @examples
#' \dontrun{
#' dataPrep(phy=sample.tree.Tiru, dat=Col_concat, colnum=NULL, scale=F, method="BM", mode="cov")
#' }
#'
#' @rdname dataPrep
#'
#' @export
#' @importFrom ape drop.tip
#' @importFrom phytools phyl.pca phyl.resid
#' @importFrom stats biplot
#' @importFrom graphics par

dataPrep <- function(phy, dat, colnum=NULL, scale=F, method="BM", mode="cov")
{
  '%notin%' <- Negate('%in%')
  phy <- ape::drop.tip(phy, which(phy$tip.label %notin% rownames(dat)))
  dat1<-dat
  for(i in 1:ncol(dat1)){
    pj<-dat1[,i]
    if(min(pj)==max(pj)){dat1[,i]<- rep(0, nrow(dat1))}
    }
  dat1<-dat1[, colSums(dat1 != 0) > 0]
  dat<-dat1

  pca.out <- function(x, y)
  {
    dat.pca <- phytools::phyl.pca(x, y, method, mode)
    return(dat.pca)
  }
  if(is.null(colnum))
  {
    if(missing(scale))
    {
      pca.out(phy, dat)
    }
    else
    {
      if(scale == TRUE)
      {
        dat <- scale(dat, center = T, scale = T)
        pca.out(phy, dat)
      }
      else
      {
        if(scale == FALSE)
        {
          pca.out(phy, dat)
        }
      }
    }
  }
  else
  {
    dat.resid <- phytools::phyl.resid(phy, dat[, as.numeric(colnum)], dat[, -as.numeric(colnum)])$resid
    if(missing(scale))
    {
      pca.out(phy, dat.resid)
    }
    else
    {
      if(scale == TRUE)
      {
        dat.resid <- scale(dat.resid, center = T, scale = T)
        pca.out(phy, dat.resid)
      }
      else
      {
        if(scale == FALSE)
        {
          pca.out(phy, dat.resid)
        }
      }
    }
  }
}
