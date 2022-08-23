#' Estimate rate of trait evolution
#'
#' @name rateEvol
#'
#' @description Estimate rate of trait evolution using a ridge regression method.
#'
#' @details This function based on the utilities of R package 'RRphylo' performs ridge regression and simultaneously
#' estimates rate of  multiple trait evolution along all the branches of a phylogenetic tree.Rates of evolution
#' can also be estimated for incompletely resolved (polytomic) phylogenies or phylogenies with artificially
#' introduced branches (e.g., individual branches for conspecific forms or sexes). In such cases, the
#' 'rateEvol()' function considers the introduced branches one at a time (while omitting its sister branches)
#' and calculates the rate of evolution from the node that represents species divergence. This process is repeated
#' as many times as the number of introduced branches. For rate estimation with introduced branches, the function
#' requires a user input in the form of a list  with two nested list objects. The first list object contains the tip
#' labels of introduced branches (for which the rate of evolution will be estimated in each iteration), and the second
#' contains the tip labels of introduced sister branches (that need to be omitted in each iteration).
#'
#' @usage rateEvol(traitPCA, dimn, tree, ntip, intr, tiplist)
#'
#' @param traitPCA A list containing PCA elements obtained from dataPrep implemented on different traits.
#' @param dimn Number of PC dimension to be used to estimate evolutionary rate.
#' @param tree A phylogenetic tree of class phylo.
#' @param ntip Number of tips for which tree and PCA data has been provided.
#' @param intr A logical argument, whether tips were introduced manually and rates need to be calculated for those
#' introduced tips separately (defaults to FALSE).
#' @param tiplist A list file containing two sets of tip names that will be used to separately calculate rates for
#' the introduced branches (defaults to NULL).
#'
#' @return A dataframe containing rates as coefficients of ridge regression for all the branches of the tree.
#'
#' @examples
#' \dontrun{
#' traitPCA<-list(PCA_shp, PCA_col)
#' tree<-sample.tree.Tiru
#' tiplist<-list(c("Papl_cldm","Papl_cldf", "Pare_ceym","Pare_ceyf","Pare_hipm","Pare_hipf"),
#' c("Papl_cldf","Papl_cldm", "Pare_ceyf","Pare_ceym","Pare_hipf","Pare_hipm"))
#' rateEvol(traitPCA, dimn=4, tree, ntip=29, intr=T, tiplist)
#' }
#'
#' @rdname rateEvol
#'
#' @export
#' @importFrom ape drop.tip
#' @importFrom RRphylo RRphylo

rateEvol<-function(traitPCA, dimn, tree, ntip, intr=F, tiplist=NULL){
  '%notin%' <- Negate('%in%')
  tree <- ape::drop.tip(tree, which(tree$tip.label %notin% rownames(traitPCA[[1]])))
  ratedata<-matrix(nrow=(tree$Nnode+length(tree$tip.label)), ncol=1)
  if(intr==F){
    for(i in 1:length(traitPCA)){
      print(Sys.time())
      dd<-traitPCA[[i]]
      dd1<-RRphylo::RRphylo(tree, y = dd[,1:dimn])
      dd1_1<-as.matrix(dd1$rates)
      dd1_2<-rbind(as.matrix(dd1_1[(ntip):nrow(dd1_1),]), as.matrix(dd1_1[1:(ntip-1),]))
      ratedata<-cbind(ratedata,as.data.frame(dd1_2))
    }
    ratedata<-ratedata[,-1]
    ratedata<-as.data.frame(ratedata)
    colnames(ratedata)<-names(traitPCA)
  }
  else{
    lk1<-tiplist[[1]]
    lk2<-tiplist[[2]]
    lk3<-unique(unlist(lk1))
    lk4<-traitPCA
    dd4<-matrix(nrow = (tree$Nnode*2+1), ncol = 1)
    for(i in 1:length(lk4)){
      print(Sys.time())
      dd<-lk4[[i]]
      dd1<-RRphylo::RRphylo(tree, y = dd)
      dd1_1<-as.matrix(dd1$rates)

      for(j in 1:length(lk1)){
        tip1<-unlist(lk1[[j]])
        tip2<-unlist(lk2[[j]])
        wg.tree.w1 <- ape::drop.tip(tree, tip1)
        dd2<-RRphylo::RRphylo(tree = wg.tree.w1, y = dd)
        dd3<-as.matrix(dd2$rates)
        lk5<-dd3[grep(tip2, rownames(dd3)),]
        lk6<-grep(tip2, rownames(dd1_1))
        dd1_1[lk6]<-lk5
        print(i)
        print(j)
      }
      dd1_2<-rbind(as.matrix(dd1_1[(ntip):nrow(dd1_1),]), as.matrix(dd1_1[1:(ntip-1),]))
      ratedata<- cbind(ratedata,as.data.frame(dd1_2))
    }
    ratedata<-ratedata[,-1]
    ratedata<-as.data.frame(ratedata)
    colnames(ratedata)<-names(traitPCA)
  }

  return(ratedata)
}
