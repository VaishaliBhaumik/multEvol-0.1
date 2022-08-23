#' This function affine transforms images to create template for each group or species.
#'
#' @name affTr
#'
#' @description Affine transforms images to a template for each group/species.
#'
#' @details This function affine-transforms raw .jpg images to a template for each
#' group/species using utilities of R package "RNiftyReg". These transformed images
#' will be used further in the function colData for colour pattern extraction.
#'
#' @usage affTr(img_path,img_ID, target, out_path)
#'
#' @param img_path Folder path for raw images.
#' @param img_ID A matrix containing the names of group identifiers in the first column and
#'               the sequence number of the image in that group to be affine transformed.
#' @param target Name of the target group or species name (should be one of the names in the
#'               first column of img_ID)
#' @param out_path Folder path for template images.
#'
#' @return None. Affine-transformed images are saved in the user-defined folder.
#'
#' @examples
#' \dontrun{
#' affTr(img_path = "img_folder/", img_ID=colour_sampling,target= "Tiru_sept",
#' out_path = "transformed_folder/")
#' }
#'
#' @rdname affTr
#'
#' @export
#' @importFrom RNiftyReg niftyreg
#' @importFrom jpeg readJPEG writeJPEG

affTr <- function(img_path = NULL,
				  img_ID=NULL,
				  target = NULL,
				  out_path = NULL){

  a1 <- list.files(img_path, pattern = ".jpg")
  a2<-NULL
  for (i in 1: nrow(img_ID)){
	ak<-a1[grep(img_ID[i,1], a1)][img_ID[i,2]]
	a2<-c(a2, ak)
	}

  a3 <- a2[grep(target, a2)]
  a4 <- a2[-grep(target, a2)]
  target <- jpeg::readJPEG(paste0(img_path,a3))

  for (i in 1: length(a4)){
    source<-jpeg::readJPEG(paste0(img_path,a4[i]))
    result<-RNiftyReg::niftyreg(source, target, scope = "affine", interpolation = 0,  precision = "single", threads = 4)
    jpeg::writeJPEG(result$image, paste0(out_path, a4[i]))
	}
}
