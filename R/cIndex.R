#' Estimating convergence among multiple species/groups.
#'
#' @name cIndex
#'
#' @description Estimating C indices (Stayton, 2015) to examine the extent of convergence in the morphospace.
#'
#' @details This function is based on the 'convevol' package (Stayton, 2015) and estimates convergence indices C1–C4
#' (all of these range from 0 to 1). Values closer to 0 indicate low convergence, and values closer to 1 indicate higher
#' convergence. This function also simulates the distributions of C indices using the Brownian motion model of evolution
#' and estimates p-values for each estimated C-index.
#'
#' @usage cIndex(x , y , z , dimn , nsim)
#'
#' @param x A dataframe containing PCA results.
#' @param y A phylogenetic tree of class phylo.
#' @param z A list of groups containing species names among which convergence indices are to be estimated.
#' @param dimn Number of PC axes to be used when calculating C indices.
#' @param nsim Number of rounds of simulation for estimating the null distribution of C indices. Defaults to 100.
#'
#' @return A list object containing estimated C indices, their p-values, the simulated null distributions, and
#' respective cutoffs.
#'
#' @examples
#' \dontrun{
#' regimes <- list(rgm1 = c("sp1", "sp2"), rgm2 = c("sp3", "sp1"))
#' cIndex(x = pc.shape, y = tree, z = regimes, dimn = 4, nsim = 100)
#' }
#'
#' @rdname cIndex
#'
#' @export
#' @importFrom stats setNames
#' @importFrom ape drop.tip
#' @importFrom convevol convratsig


cIndex <- function(x, y, z, dimn=NULL, nsim=100)
{
  emp <- stats::setNames(rep(0, 4), c("C1", "C2", "C3", "C4"))
  emp.c <- list(ObservedCs = emp, Pvals = emp, AllSimCs = matrix(0, nsim, 4), Cutoffs = emp)
  dat <- x
  '%notin%' <- Negate('%in%')
  tre <- ape::drop.tip(y, which(y$tip.label %notin% rownames(dat)))
  reg <- out <- z
  for(i in 1:length(reg)){
      if(length(reg[[i]]) > 1)
      {
        out[[i]] <- convevol::convratsig(tre, as.matrix(dat[, 1:dimn]), convtips = reg[[i]], nsim=nsim)
      }
      else
      {
        out[[i]] <- emp.c
      }
      print(names(reg)[i])
    }

  for (i in 1: length(out)){
    ak<-out[[i]][[3]]
    ak<-pmax(ak,0)
    out[[i]][[3]]<-ak
    for(j in 1: length(out[[i]][[1]])){
      if (out[[i]][[1]][j]<0){
        out[[i]][[1]][j]=0
        out[[i]][[2]][j]=1
      }
    }
  }
  return(out)
}

