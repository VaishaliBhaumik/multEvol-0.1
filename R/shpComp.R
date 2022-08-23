#' Comparative visualization of shape differences
#'
#' @name shpComp
#'
#' @description Comparative visualization of the extent of deformation between the two-dimensional contours of various
#' species/groups.
#'
#' @details This function is based on the  'Momocs' package. It estimates the differences in mean shapes of various
#' species and displays a heatmap showing the extent of deformation.
#'
#' @usage shpComp(gr1, gr2, mnshp)
#'
#' @param gr1 A vector of species names .
#' @param gr2 Another vector of species names (Can be the same as gr1).
#' @param mnshp A meanshape file obtained from \code{\link{cleqEFA}}.
#'
#' @return A multi-panel graphics showing the heatmaps of shape deformation between species/groups.
#'
#' @examples
#' \dontrun{
#' gr1<-c("Idea_mala", "Para_agle", "Para_nilg", "Tiru_limn", "Tiru_sept")
#' gr2<-c("Pare_ceym", "Pare_ceyf", "Pare_hipm", "Pare_hipf", "Papl_cldm", "Papl_cldf")
#' data("meanshape")
#' shpComp(gr1, gr2, mnshp=meanshape)
#' }
#'
#' @rdname shpComp
#'
#' @export
#' @import Momocs
#' @import grDevices
#' @importFrom graphics axis mtext par polygon rasterImage text

shpComp<-function(gr1, gr2, mnshp=NULL){

    if (length(gr1)>length(gr2)){dimn1<-gr2
    dimn2<-gr1}else{dimn1<-gr1
    dimn2<-gr2}

    '%notin%' <- Negate('%in%')

    dd2<-NULL
    dd3<-NULL
    dd2<- mnshp[-which(names(mnshp) %notin% c(dimn1))]
    dd3<- mnshp[-which(names(mnshp) %notin% c(dimn2))]

    par(mar=c(2,2,2,2), mfrow=c((length(dimn2)+2), (length(dimn1)+1)))
    for(i in 1:((((length(dimn1)+1)*(length(dimn2)+2))))){
      if(i==1){plot(x = 0:10, y = 0:10, ann = F,bty = "n",type = "n",
                    xaxt = "n", yaxt = "n")
              mtext("Names",side = 1, line = -1)
                }else if(i > 1 && i < (length(dimn1)+2)){
        plot(x = 0:10, y = 0:10, ann = F,bty = "n",type = "n",
             xaxt = "n", yaxt = "n")
                  mtext(dimn1[i-1],side = 1, line = -1)
      }
      else if(!is.na(match(i,seq(1, (length(dimn1)+1)*(length(dimn2)+1), (length(dimn1)+1))[-1]))==T){
        plot(x = 0:10, y = 0:10, ann = F,bty = "n",type = "n",
             xaxt = "n", yaxt = "n")
        mtext(dimn2[(i-1)/(length(dimn1)+1)],side = 1, line = -1)
      }
      else{
        name1<-dimn1[i-(((length(dimn1)+1)*floor((i-1)/(length(dimn1)+1)))+1)]
        name2<-dimn2[(ceiling((i-(length(dimn1)+1))/((length(dimn1)+1))))]

        if(i==(((length(dimn1)+1)*(length(dimn2)+2)))){
          col_fun = grDevices::colorRampPalette(colors = c("#a8bdf7",  "#fffc81", "#ffa196"))
          legend_image <- as.raster(matrix(col_fun(100), nrow =1))
          plot(c(0, 2), c(0, 1), type='n', axes=F, xlab='', ylab='')
          rasterImage(legend_image, 0, 0.4, 2, 0.6)
          lbsq <- seq.int(0, 1, length.out = 5)
          axis(3, at=lbsq*2, pos=0.4, labels=F, col=1, col.ticks=1, lwd = 1,tck=-.05)
          axis(1, at=lbsq*2, pos=0.6, labels=F, col=1, col.ticks=1, lwd = 1,tck=-.05)
          text(x = 0, y = 0.3, label = "0", col = "black",font = 1, cex = 1.5*4/(length(dimn1)+1))
          text(x = 1, y = 0.3, label = "0.5", col = "black",font = 1, cex = 1.5*4/(length(dimn1)+1))
          text(x = 2, y = 0.3, label = "1", col = "black",font = 1, cex = 1.5*4/(length(dimn1)+1))
          text(x = 1, y = 0.8, label = "Deformation", col = "black",font = 1, cex = 1.5*4/(length(dimn1)+1))
          }else if (i>((length(dimn1)+1)*((length(dimn2)+1)))){plot(x = 0:10, y = 0:10, ann = F,bty = "n",type = "n",xaxt = "n", yaxt = "n")}else{
            if(name1==name2){
              plot(x = 0:10, y = 0:10, ann = F,bty = "n",type = "n",
                   xaxt = "n", yaxt = "n")
              text(x = 5,y = 5, "NA", cex=1)
            }else{
        Momocs::tps_iso(dd3[[grep(name2, names(dd3))]], dd2[[grep(name1, names(dd2))]], grid =F, outline = dd3[[grep(name2, names(dd3))]], iso.nb =500, legend = F, shp = F, iso.levels = 100, cont = F, xlim=c(0.5,-0.5), ylim=c(0.5, -0.5))
		  polygon(dd3[[grep(name2, names(dd3))]], col=NULL, xlim=c(0.5,-0.5), ylim=c(0.5, -0.5),border='black',  asp=1)}
          }
         }
    }
}



