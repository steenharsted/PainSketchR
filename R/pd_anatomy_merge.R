#' Merge anatomical areas into larger areas
#'
#' This assumes a pain drawing data structure of anatomical regions, where each row/pd is a single anatomical area defined by one polygon
#' In other words each pain drawing (row) should contain only a single stroke/polygon. The function collapses n pain drawings of each 1 polygon
#' into 1 pain drawing of n polygons. 
#' 
#' This is useful for creating anatomical templates -- e.g. the anterior aspect of a full body template may consist of n different anatomical areas
#' which all border each other. By collapsing these into a single pain drawing of n polygons, they can be merged into a single polygon using the
#' `pd_poly_manage_overlap` function. This new pain drawing with a single polygon (anterior aspect of the body outline) can be used to define a new 
#' anatimcal region.
#' 
#' @param pd
#'
#' @returns
#'
#' @export
#' @examples
pd_anatomy_merge <- function(pd)  {
  if(length(unique(pd$w))>1 | length(unique(pd$h))>1) {
    stop("More than one width or height value of pain drawing canvas found.")
  }

  if (pd$s |> purrr::map_int(\(t) nrow(t)) |> max() > 1) {
    stop("One or more rows contain more than one stroke/polygons.")
  }

  new_p <- pd$p |> purrr::list_rbind(names_to = 'i') |> list()
  new_s <- pd$s |> purrr::list_rbind(names_to = 'i') |> list()
  pd <- pd[1,] # Retain only row 1
  pd$id <- "anatomy_merger"
  pd$s <- new_s
  pd$p <- new_p
  return(pd)
}