#' Merge anatomical areas into larger areas
#'
#' This assumes a pain drawing data structure of anatomical regions, 
#' where each row/pdpaindrawing is a single anatomical area defined by one stroke polygon
#' In other words each pain drawing (row) should contain only a single stroke/polygon. 
#' The function collapses n pain drawings of each 1 polygon
#' into 1 pain drawing of n polygons. 
#' 
#' This is useful for creating anatomical templates -- e.g. the anterior aspect of a full body template may consist of n different anatomical areas
#' which all border each other. By collapsing these into a single pain drawing of n polygons, they can be merged into a single polygon using the
#' `pd_poly_manage_overlap` function. This new pain drawing with a single polygon (anterior aspect of the body outline) can be used to define a new 
#' anatimcal region.
#' 
#' @param pd must be a valid pain drawing data structure where each pain drawing consists of a single stroke/polygon
#' @param merge_overlaps (TRUE/FALSE) if TRUE any strokes/polygons which overlap (even by just a shared point or line) will m be merged. 
#' Otherwise the function returns all drawings collapsed into a single pain drawing with multiple strokes/polygons.
#' @returns 
#'
#' @export
#' @examples
pd_anatomy_merge <- function(pd, merge_overlaps=TRUE)  {
  if(length(unique(pd$w))>1 | length(unique(pd$h))>1) {
    stop("More than one width or height value of pain drawing canvas found.")
  }

  if (pd$s |> purrr::map_int(\(t) nrow(t)) |> max() > 1) {
    stop("One or more rows contain more than one stroke/polygons.")
  }

  # Each row in pd is a pain drawing with just one stroke/polygon
  # We collapse this into a pd data structure with just one pain drawing of
  # multiple stroke/polygons.
  new_p <- pd$p |> purrr::list_rbind(names_to = 'i') |> list()
  new_s <- pd$s |> purrr::list_rbind(names_to = 'i') |> list()
  pd <- pd[1,] # Retain only row 1
  pd$id <- "anatomy_merger"
  pd$s <- new_s
  pd$p <- new_p
  # Now merge overlapping strokes/polygons if merge_overlap is TRUE
  if (merge_overlaps) {
    pd <- pd |> mutate(p=pd_poly_manage_overlaps(p, method="union"))
    pd <- pd |> mutate(s=unique(pd$p$i))
  }
  return(pd)
}