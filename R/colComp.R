#' Comparative visualization of colour pattern differences.
#'
#' @name colComp
#'
#' @description Comparative visualization of the extent of differences between homologous colour pattern
#' elements among various groups.
#'
#' @details The function  estimates differences between mean colour patterns of various species and displays
#' a heatmap showing the colour pattern variations within and between species groups. This function is based
#' on the  utilities of R package 'patternize'.
#'
#' @usage colComp(gr1, gr2, rstrstcks, ID, outline_path, bcg_col, font_col)
#'
#' @param gr1 A vector of species names.
#' @param gr2 Another vector of species names (gr2 could be similar to gr 1 or different).
#' @param rstrstcks A list object containing rasterstacks for each species as obtained from colData.
#' @param ID A raster ID containing specimen names as saved from colData.
#' @param outline_path A path containing the text file (.txt) containing outline coordinates and a binary
#' image of a cartton template on which heatmaps will be plotted.
#' @param bcg_col Background colour to be displayed in the graphics (default="black").
#' @param font_col Text colour to be displayed on the graphics (default="white").
#'
#' @return None. a multipanel graphics showing heatmaps as extent of colour pattern variations within and between species
#' groups.
#'
#' @examples
#' \dontrun{
#' gr1<-c("Idea_mala", "Para_agle", "Para_nilg", "Tiru_limn", "Tiru_sept")
#' gr2<-c("Pare_ceym", "Pare_ceyf", "Pare_hipm", "Pare_hipf", "Papl_cldm", "Papl_cldf")
#' colComp(gr1, gr2, rstrstcks=Mel_rasterstacks, ID=Mel_IDlist, outline_path="/outline/",
#' bcg_col="black", font_col="white")
#' }
#'
#' @rdname colComp
#'
#' @export
#' @importFrom patternize sumRaster plotHeat
#' @importFrom viridis plasma
#' @importFrom colorspace diverge_hcl
#' @importFrom graphics axis mtext par rasterImage text
#' @importFrom utils read.table
#' @import grDevices

colComp<-function(gr1, gr2, rstrstcks, ID, outline_path=NULL, bcg_col="black", font_col="white"){

  if (length(gr1)>length(gr2)){dimn1<-gr2
  dimn2<-gr1}else{dimn1<-gr1
  dimn2<-gr2}

  '%notin%' <- Negate('%in%')

  data1<-rstrstcks[-which(names(rstrstcks) %notin% c(dimn1))]
  ID1<-ID[-which(names(ID) %notin% c(dimn1))]
  data2<-rstrstcks[-which(names(rstrstcks) %notin% c(dimn2))]
  ID2<-ID[-which(names(ID) %notin% c(dimn2))]

  ak1<-list()
  for(i in 1:length(dimn1)){
    ID3<-unlist(ID1[[i]])
    names(ID3)<-NULL
    ak1[[i]] <- patternize::sumRaster(rList = data1[[i]], IDlist = ID3, type ='RGB' )
  }

  ak2<-list()
  for(i in 1:length(dimn2)){
    ID3<-unlist(ID2[[i]])
    names(ID3)<-NULL
    ak2[[i]] <- patternize::sumRaster(rList = data2[[i]], IDlist = ID3, type ='RGB' )
  }

  a1<-list.files(outline_path, pattern="Outline")
  target<-a1[[1]]
  out.txt<-read.table(paste0(outline_path,a1[2]) )

  colfunc1 <- viridis::plasma(100)
  colfunc2<-colorspace::diverge_hcl(100)

  par(mar=c(1.5,1.5,1.5,1.5), mfrow=c((length(dimn2)+2), (length(dimn1)+1)), bg=bcg_col)
  for(i in 1:((length(dimn1)+1)*(length(dimn2)+2))){
    if(i==1){plot(x = 0:10, y = 0:10, ann = F,bty = "n",type = "n",
                  xaxt = "n", yaxt = "n")
      text(x = 5,y = 5,"Names", col=font_col, cex=1.5)}
    else if(i > 1 && i < (length(dimn1)+2)){

      patternize::plotHeat(ak1[[i-1]], as.vector(unlist(ID1[[i-1]])), plotCartoon = T, refShape ='target',outline = out.txt,cartoonCol = bcg_col, refImage =ak1[[i-1]][[1]], crop = c(0,0,0,0),flipRaster ='x', cartoonOrder = 'under',cartoonFill =bcg_col,  colpalette = colfunc1, legend = F)
      mtext(dimn1[i-1],side = 1, line = 0.1, col = font_col)
    }
    else if(!is.na(match(i,seq(1, (length(dimn1)+1)*(length(dimn2)+1), (length(dimn1)+1))[-1]))==T){

      patternize::plotHeat(ak2[[match(i,seq(1, (length(dimn1)+1)*(length(dimn2)+1), (length(dimn1)+1))[-1])]], as.vector(unlist(ID2[[match(i,seq(1, (length(dimn1)+1)*(length(dimn2)+1), (length(dimn1)+1))[-1])]])), plotCartoon = T, cartoonCol = bcg_col,refShape ='target',outline = out.txt, refImage = ak2[[match(i,seq(1, (length(dimn1)+1)*(length(dimn2)+1), (length(dimn1)+1))[-1])]][[1]], crop = c(0,0,0,0),flipRaster ='x', cartoonOrder = 'under',cartoonFill =bcg_col,  colpalette = colfunc1,  legend = F)
      mtext(dimn2[(i-1)/(length(dimn1)+1)],side = 1, line = 0.1, col = font_col)
    }
    else{

      name1<-dimn1[i-(((length(dimn1)+1)*floor((i-1)/(length(dimn1)+1)))+1)]
      name2<-dimn2[(ceiling((i-(length(dimn1)+1))/((length(dimn1)+1))))]

      if(i==((((length(dimn1)+1)*(length(dimn2)+1)))+1)){
        col_fun = grDevices::colorRampPalette(colors = colfunc1)
        legend_image <- as.raster(matrix(col_fun(100), nrow =1))
        plot(c(0, 2), c(0, 1), type='n', axes=F, xlab='', ylab='')
        rasterImage(legend_image, 0, 0.4, 2, 0.6)
        lbsq <- seq.int(0, 1, length.out = 5)
        axis(3, at=lbsq*2, pos=0.4, labels=F, col=1, col.ticks=1, lwd = 1,tck=-.05)
        axis(1, at=lbsq*2, pos=0.6, labels=F, col=1, col.ticks=1, lwd = 1,tck=-.05)
        text(x = 0, y = 0.3, label = "0", col = font_col,font = 1, cex = 1.5*4/(length(dimn1)+1))
        text(x = 1, y = 0.3, label = "0.5", col = font_col,font = 1, cex = 1.5*4/(length(dimn1)+1))
        text(x = 2, y = 0.3, label = "1", col = font_col,font = 1, cex = 1.5*4/(length(dimn1)+1))
        text(x = 1, y = 0.8, label = "Intrataxon", col = font_col,font = 1, cex = 1.5*4/(length(dimn1)+1))
      }else if(i==((length(dimn1)+1)*(length(dimn2)+2))){
        col_fun2 = grDevices::colorRampPalette(colors = colfunc2)
        legend_image <- as.raster(matrix(col_fun2(100), nrow =1))
        plot(c(0, 2), c(0, 1), type='n', axes=F, xlab='', ylab='')
        rasterImage(legend_image, 0, 0.4, 2, 0.6)
        lbsq <- seq.int(0, 1, length.out = 5)
        axis(3, at=lbsq*2, pos=0.4, labels=F, col=1, col.ticks=1, lwd = 1,tck=-.05)
        axis(1, at=lbsq*2, pos=0.6, labels=F, col=1, col.ticks=1, lwd = 1,tck=-.05)
        text(x = 0, y = 0.3, label = "-1", col = font_col,font = 1, cex = 1.5*4/(length(dimn1)+1))
        text(x = 1, y = 0.3, label = "0", col = font_col,font = 1, cex = 1.5*4/(length(dimn1)+1))
        text(x = 2, y = 0.3, label = "1", col = font_col,font = 1, cex = 1.5*4/(length(dimn1)+1))
        text(x = 1, y = 0.8, label = "Intertaxon", col = font_col,font = 1, cex = 1.5*4/(length(dimn1)+1))
      }else if (i>((length(dimn1)+1)*(length(dimn2)+1))){
        plot(x = 0:10, y = 0:10, ann = F,bty = "n",type = "n",xaxt = "n", yaxt = "n")
        }else{
      if(name1==name2){
        plot(x = 0:10, y = 0:10, ann = F,bty = "n",type = "n",
             xaxt = "n", yaxt = "n")
        text(x = 5,y = 5, "NA", cex=1,col = font_col)
      }else {
      psro<- (ak1[[grep(name1, names(data1))]]/length(ID1[[grep(name1, names(data1))]]))-(ak2[[grep(name2, names(data2))]]/length(ID2[[grep(name2, names(data2))]]))
      patternize::plotHeat(psro, normalized=T, plotCartoon = T, refShape ='target',outline = out.txt,cartoonCol = bcg_col, refImage = data2[[grep(name2, names(data2))]][[1]], crop = c(0,0,0,0),flipRaster ='x', cartoonOrder = 'under',cartoonFill =bcg_col,  colpalette =colfunc2, legend = F )
      }
    }
  }
}
}
