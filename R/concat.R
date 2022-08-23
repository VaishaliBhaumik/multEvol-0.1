#' Concatenation of large colour data files
#'
#' @name concat
#'
#' @description Concatenated large data frames into a wide conjugate dataframe.
#'
#' @details This function concatenates the wide dataframes of proportional matrices of each colour range into a large
#' dataframe.
#'
#' @usage concat(file_names)
#'
#' @param file_names A list of file names containing proportional matrices for each colour as obtained
#' from function colData.
#'
#' @return a large dataframe
#'
#' @examples
#' \dontrun{
#' file_names<-list(MEL_mean, BLU_mean, RED_mean)
#' concat(file_names)
#' }
#'
#' @rdname concat
#'
#' @export

concat<-function(file_names){
  data1<-NULL
  for(i in 1:length(file_names)){
    col_data<-file_names[i]
    data1<-cbind(data1, t(as.data.frame(col_data)))
  }
  data1<-as.data.frame(data1)
  return(data1)
}
