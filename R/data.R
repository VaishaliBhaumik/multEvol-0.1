#' @title col_index
#' @description Group identifiers and the sequence number of a template image for each group.
#' @format A matrix containing the names of group identifiers in the first column and
#'         the sequence number of the image in that group to be used as the template for alignment. It is used in \code{affTr},
#'         \code{colData}.
#' @examples
#' \dontrun{
#' data(col_index)
#' summary(col_index)
#' }
"col_index"

#' @title mnshp_efa
#' @description Fourier descriptors obtained from \code{cleqEFA} and used by
#'    \code{dataPrep}.
#' @format A dataframe containing elliptical Fourier descriptors for each harmonics explaining the mean shape of each group.
#' @examples
#' \dontrun{
#' data(mnshp_efa)
#' summary(mnshp_efa)
#' }
"mnshp_efa"

#' @title mnshps
#' @description Coordinates of mean shapes returned by \code{cleqEFA} and used by
#'    \code{shpComp}.
#' @format A list of x, y coordinates of forewing mean shape of six butterfly species.
#' @examples
#' \dontrun{
#' data(mnshps)
#' summary(mnshps)
#' }
"mnshps"

#' @title MEL_IDList
#' @description List of group names as returned by \code{colData}.
#' @format A list of group names including the melanistic colour pattern extracted from
#'     dorsal forewings of six butterfly species using \code{colData}.
#' @examples
#' \dontrun{
#' data(MEL_IDList)
#' summary(MEL_IDList)
#' }
"MEL_IDList"

#' @title MEL_mean
#' @description A linearized dataframe of colour pattern data returned by \code{colData}.
#' @format A dataframe containing  linearized proportional data of the melanistic colour pattern extracted from
#'    dorsal forewings of six butterfly species using \code{colData}.
#' @examples
#' \dontrun{
#' data(MEL_mean)
#' summary(MEL_mean)
#' }
"MEL_mean"

#' @title MEL_RasterStacks
#' @description List of RasterLayers as returned by \code{colData}.
#' @format A list of RasterLayers including the melanistic color pattern extracted from
#'    dorsal forewings of  six butterfly species using \code{colData}.
#' @examples
#' \dontrun{
#' data(MEL_RasterStacks)
#' summary(MEL_RasterStacks)
#' }
"MEL_RasterStacks"

#' @title BLU_IDList
#' @description List of group names as returned by \code{colData}.
#' @format A list of group names including the blue colour pattern extracted from
#'     dorsal forewings of six butterfly species using \code{colData}.
#' @examples
#' \dontrun{
#' data(BLU_IDList)
#' summary(BLU_IDList)
#' }
"BLU_IDList"

#' @title BLU_mean
#' @description A linearized dataframe of colour pattern data returned by \code{colData}.
#' @format A dataframe containing  linearized proportional data of the blue colour pattern extracted from
#'    dorsal forewings of six butterfly species using \code{colData}.
#' @examples
#' \dontrun{
#' data(BLU_mean)
#' summary(BLU_mean)
#' }
"BLU_mean"

#' @title BLU_RasterStacks
#' @description List of RasterLayers as returned by \code{colData}.
#' @format A list of RasterLayers including the blue color pattern extracted from
#'    dorsal forewings of  six butterfly species using \code{colData}.
#' @examples
#' \dontrun{
#' data(BLU_RasterStacks)
#' summary(BLU_RasterStacks)
#' }
"BLU_RasterStacks"

#' @title RED_IDList
#' @description List of group names as returned by \code{colData}.
#' @format A list of group names including the red colour pattern extracted from
#'     dorsal forewings of six butterfly species using \code{colData}.
#' @examples
#' \dontrun{
#' data(RED_IDList)
#' summary(RED_IDList)
#' }
"RED_IDList"

#' @title RED_mean
#' @description A linearized dataframe of colour pattern data returned by \code{colData}.
#' @format A dataframe containing  linearized proportional data of the red colour pattern extracted from
#'    dorsal forewings of six butterfly species using \code{colData}.
#' @examples
#' \dontrun{
#' data(RED_mean)
#' summary(RED_mean)
#' }
"RED_mean"

#' @title RED_RasterStacks
#' @description List of RasterLayers as returned by \code{colData}.
#' @format A list of RasterLayers including the red color pattern extracted from
#'    dorsal forewings of  six butterfly species using \code{colData}.
#' @examples
#' \dontrun{
#' data(RED_RasterStacks)
#' summary(RED_RasterStacks)
#' }
"RED_RasterStacks"

#' @title col_concat
#' @description A linearized dataframe of colour pattern data returned by \code{concat}.
#' @format A concatenated linearized dataframe of colour pattern data of multiple colours extracted from dorsal
#' forewings of six butterfly species using \code{concat}.
#' @examples
#' \dontrun{
#' data(col_concat)
#' summary(col_concat)
#' }
"col_concat"

#' @title shp_pcscores
#' @description A dataframe containing PC scores.
#' @format A dataframe containing PC scores obtained from \code{\link[phytools]{phyl.pca}} on Fourier descriptors of
#' shape data returned by \code{cleqEFA}
#' @examples
#' \dontrun{
#' data(shp_pcscores)
#' summary(shp_pcscores)
#' }
"shp_pcscores"

#' @title col_pcscores
#' @description A dataframe containing PC scores.
#' @format A dataframe containing PC scores obtained from \code{\link[phytools]{phyl.pca}} on concatenated colour
#' data returned by \code{concat}.
#' @examples
#' \dontrun{
#' data(col_pcscores)
#' summary(col_pcscores)
#' }
"col_pcscores"

#' @title wg_tree
#' @description A "phylo" object.
#' @format A phylogenetic tree of class "phylo" of 29 species of butterflies from three families (Nymphalidae,
#' Pieridae and Papilionidae).
#' @examples
#' \dontrun{
#' data(wg_tree)
#' summary(wg_tree)
#' }
"wg_tree"
