#' Evolutionary model fitting
#'
#' @name pcModels
#'
#' @details This function fits six macroevolutionary models to a phylogenetic tree: Brownian motion (BM), early burst (EB),
#' Ornstein–Uhlenbeck (OU), lambda, kappa, and delta.  The function returns the estimated statistical parameters for each
#' models fitted to each PC.
#'
#' @usage pcModels(phy, dat)
#'
#' @param phy A phylogenetic tree of class phylo.
#' @param dat A vector or matrix containing dependent variable/s.
#'
#' @return A list of the estimated statistical parameters for each models fitted to each PC.
#'
#' @examples
#' \dontrun{
#' evo_model<-pcModels(sample.tree.Tiru, PCA_col)
#' }
#'
#' @rdname pcModels
#'
#' @export
#' @importFrom geiger fitContinuous

pcModels <- function(phy, dat)
{
  dat <- as.data.frame(dat)
  pc.models <- list(BM=list(), EB=list(), OU=list(), lambda=list(), kappa=list(), delta=list())
  for(i in 1:length(pc.models))
  {
    print(names(pc.models[i]))
    pc.models[[i]] <- geiger::fitContinuous(phy, dat, model = names(pc.models[i]))
  }
  return(pc.models)
}
