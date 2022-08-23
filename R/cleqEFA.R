#' Elliptical Fourier analysis on closed contours.
#'
#' @name cleqEFA
#'
#' @description This function applies Fourier transform on outlines.
#'
#' @details This function uses utilities provided in the package "Momocs" and performs
#' Elliptical Fourier analysis on two-dimensional closed contours. This is an interactive
#' function that scales, centres, and aligns closed outlines from a list of binary images.
#'
#' @usage cleqEFA(img_path, out_path)
#'
#' @param img_path Folder path for binary image files (same as the output folder for the binImg function).
#' @param out_path Folder path for output images and final EFA descriptors.
#'
#' @return None. Images showing the contour stacks, harmonic power, results of elliptical analysis, and fitted ellipses
#' (intermediate results). The EFA descriptors and meanshapes for each species/group are saved in the output folder.
#'
#' @examples
#' \dontrun{
#' cleqEFA(img_path = "img_folder/", out_path = "output_folder/")
#' }
#'
#' @rdname cleqEFA
#'
#' @export
#' @importFrom grDevices x11 dev.print dev.copy2pdf dev.off
#' @import Momocs
#' @importFrom utils stack
#' @importFrom ggplot2 ggplot aes geom_boxplot theme_classic xlab ylab geom_vline ggsave


cleqEFA<-function(img_path=NULL, out_path=NULL){
  img_list<-list.files(img_path)
  grch <- readline(prompt = "Number of characters in group name: ")
  grch<-as.numeric(grch)
  gr<-as.vector(sapply(img_list, function(x) {substr(x, 1, grch)}))
  imgs<-Momocs::import_jpg(jpg.path = paste0(img_path,img_list))


  repeat
  {

    img_out<-Momocs::Out(imgs) # converts to the output object
    img1<- Momocs::coo_smooth(img_out, 10)
    img1<-Momocs::coo_sample(img1,min(Momocs::coo_nb(img1)))
    img1<-Momocs::coo_center(img1)
    img1<-Momocs::fgProcrustes(img1, tol = 1e-4)
    img1<-Momocs::coo_scale(img1)
    aln <- readline(prompt = "Alignment options (1=align/2=alignxax/3=aligncalliper/4=alignminradius): ")
    aln<-as.numeric(aln)
    aln1<-c("align", "alignxax", "aligncalliper", "alignminradius", "none")
    aln2<-aln1[aln]
    if(aln2==aln1[1]){img1 <-Momocs::coo_align(img1)}
    else if(aln2==aln1[2]){img1 <- Momocs::coo_alignxax(img1)}
    else if(aln2==aln1[3]){img1 <-Momocs::coo_aligncalliper(img1)}
    else if(aln2==aln1[4]){img1 <-Momocs::coo_alignminradius(img1)}
    else{print("Warning: provide correct alignment option")}
    sld <- readline(prompt = "Start point reset direction (1=down/2=left/3=up/4=right): ")
    sld<-as.numeric(sld)
    sld1<-c("down","left","up","right")
    img2<-Momocs::coo_slidedirection(img1,sld1[sld])
    print(utils::stack(img2))
    cont <- readline(prompt = "Continue (y/n): ")
    if(cont == "y")
      {
        dev.copy2pdf(out.type = "pdf", file= paste0(out_path,"Contour_stack.pdf"), width = 6, height = 6)
        dev.off()

        trgt<-readline(prompt = "Target image name (without extension): ")
        trgt<-as.character(trgt)

        pri<-as.data.frame(img2[grep(trgt,names(img2))])
        colnames(pri)<-c("x", "y")
        osc<-Momocs::coo_oscillo(as.matrix(pri), method = "efourier", nb.pts = 10)
        dev.copy2pdf(out.type = "pdf", file=paste0(out_path,"Elliptical_analysis.pdf"), width = 6, height = 6)
        dev.off()

        nb_harmonics<-readline(prompt = "Number of harmonics for calibration: ")
        nb_harmonics<-as.numeric(nb_harmonics)
        chp<-Momocs::calibrate_harmonicpower_efourier(img2, nb.h = nb_harmonics)

        tp<-chp$minh
        dk1<-chp$gg$data[1:((nb_harmonics-1)*length(img2)),2:3]
        dk1[, 1] <- factor(dk1[, 1], levels = unique(dk1[, 1]))
        harm<-hp<-NULL
        ggplot2::ggplot(dk1, ggplot2::aes(harm, hp))+ggplot2::geom_boxplot()+ggplot2::theme_classic()+
          ggplot2::xlab("Harmonics") + ggplot2::ylab("Harmonic power")+ ggplot2::geom_vline(xintercept=c(as.numeric(tp[1])), col="#B2182B")+
          ggplot2::geom_vline(xintercept=c(as.numeric(tp[2])), col="#F4A582")+ ggplot2::geom_vline(xintercept=c(as.numeric(tp[3])),
          col="#92C5DE")+ ggplot2::geom_vline(xintercept=c(as.numeric(tp[4])), col="#2166AC")+ ggplot2:: geom_text(x=as.numeric(tp[1])-0.25, y=75, label=
          "90%",col="#B2182B", angle=90)+ ggplot2:: geom_text(x=as.numeric(tp[2])-0.25, y=75, label="95%",col="#F4A582", angle=90)+ ggplot2:: geom_text(x=
          as.numeric(tp[3])-0.25, y=75, label="99%",col="#92C5DE", angle=90)+ ggplot2:: geom_text(x= as.numeric(tp[4])-0.25, y=75, label="99.9%",col=
          "#2166AC", angle=90)

        ggplot2::ggsave(paste0(out_path,"Harmonic_power.pdf"), width = 6, height = 6)



        Momocs::Ptolemy(as.data.frame(img2[grep(trgt,names(img2))]), nb.h = as.numeric(tp[3]))
        dev.copy2pdf(out.type = "pdf", file=paste0(out_path,"Ellipse_fit.pdf"), width = 6, height = 6)
        dev.off()

        nb_harmonics1<-readline(prompt = "Number of harmonics for Fourier analysis: ")
        nb_harmonics1<-as.numeric(nb_harmonics1)
        norml<-readline(prompt = "Normalize the harmonics coefficients (y/n):  ")
        norml<-as.character(norml)
        if(norml=="Y"){norml=T} else{norml=F}
        h<-Momocs::efourier(img2, nb.h = nb_harmonics1, norm=norml)
        dd1<- Momocs::MSHAPES(h,as.factor(gr), )
        saveRDS(dd1$Coe$coe, paste0(out_path, "FW_meanshape_efourier.RDS"))
        saveRDS(dd1$shp, paste0(out_path, "FW_meanshapes.RDS"))

        break
      }
  }
}
