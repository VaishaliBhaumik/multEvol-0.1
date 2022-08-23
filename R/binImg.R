#' Creating square binary images for outline analysis.
#'
#' @name binImg
#'
#' @description Transforming raw images into binary images (black foreground, white background) for the analysis of shape
#' outlines.
#'
#' @details A robust function that thresholds .jpg images of specimens photographed against a lighter background (close to
#'  white) and transforms them into a square blank-and-white image that is resized to user-defined dimensions. This
#'  transformed images are directly saved to a user-defined folder.
#'
#' @usage  binImg(img_path, thresh, dimn, out_path)
#'
#' @param img_path The path of the raw image files.
#' @param thresh The thresholding cutoff (ranges from 0 to 1). defaults to 0.9. Images with a darker foreground require
#' lower threshold values to efficiently segregate the background and foreground.
#' @param dimn Number of pixels (length and width) for image resizing. Defaults to 100.
#' @param out_path Folder path for saving the binarized images.
#'
#' @return None. Transformed and resized binary images are directly saved in the user-defined folder.
#'
#' @examples
#' \dontrun{
#' binImg(img_path = "img_folder/", thresh = 0.97, dimn = 100, out_path = "test/")
#' }
#'
#' @rdname binImg
#'
#' @export
#' @importFrom EBImage readImage channel fillHull as.Image makeBrush dilate filter2 resize writeImage

binImg <- function(img_path = NULL,
                   thresh = 0.9,
                   dimn = 100,
                   out_path = NULL){
  list_img<- list.files(img_path, pattern = ".jpg")

  for(i in 1:length(list_img)){
  x = EBImage::readImage(paste0(img_path,list_img[i]))
  x1 = EBImage::channel(x,"gray")
  uca = x1>thresh
  uca_neg = max(uca) - uca
  uca = EBImage::fillHull(uca_neg)
  y_neg = EBImage::as.Image(max(uca)-uca)
  broom = EBImage::makeBrush(size = 3, shape = 'gaussian', sigma = 5)
  img_flo = EBImage::filter2(y_neg, broom)
  uca1 = img_flo>thresh
  kern = EBImage::makeBrush(5, shape='disc')
  uca_dil = EBImage::dilate(uca1, kern)
  broom = EBImage::makeBrush(size = 3, shape = 'gaussian', sigma = 3)
  img_flo1 = EBImage::filter2(uca_dil, broom)
  uca1<-img_flo1
  wd=dim(uca1)[1]
  ht=dim(uca1)[2]

  if(wd==ht){y_res=EBImage::resize(uca1, w=dimn, h=dimn)}
  else if(wd>ht){y_res=EBImage::resize(uca1, w=dimn, h=(ht/(wd/dimn)))
  ht_res<-dim(y_res)[2]
  dif_dimn<-(dimn-ht_res)
  dif1<-round(dif_dimn/2)
  dif2<-dif_dimn-dif1
  ad1<-matrix(nrow = dimn, ncol=dif1, data = rep(1, dif1*dimn))
  ad2<-matrix(nrow = dimn, ncol=dif2, data = rep(1, dif2*dimn))
  kj<-y_res@.Data
  kj[kj < 1] <- 0
  kj1<-cbind(ad1, kj)
  kj1<-cbind(kj1, ad2)
  y_res@.Data<-kj1
  broom = EBImage::makeBrush(size = 3, shape = 'gaussian', sigma = 3)
  img_flo1 = EBImage::filter2(y_res, broom)
  y_res<-img_flo1
  }else{
  y_res=EBImage::resize(uca1, h=dimn, w=(wd/(ht/dimn)))
  wd_res<-dim(y_res)[1]
  dif_dimn<-(dimn-wd_res)
  dif1<-round(dif_dimn/2)
  dif2<-dif_dimn-dif1
  ad1<-matrix(nrow = dif1, ncol=dimn, data = rep(1, dif1*dimn))
  ad2<-matrix(nrow = dif2, ncol=dimn, data = rep(1, dif2*dimn))
  kj<-y_res@.Data
  kj[kj < 1] <- 0
  kj1<-rbind(ad1, kj)
  kj1<-rbind(kj1, ad2)
  y_res@.Data<-kj1
  broom = EBImage::makeBrush(size = 3, shape = 'gaussian', sigma = 3)
  img_flo1 = EBImage::filter2(y_res, broom)
  y_res<-img_flo1
  }
  EBImage::writeImage(y_res, files = paste0(out_path,list_img[i]), quality = 100)
}
}
